//! # `bevy_mod_pakfile`
//!
//! Enables using `.pak` files as asset sources. Currently quite limited in functionality,
//! based on the needs of the [`seismon`](https://github.com/eira-fransham/seismon) engine.
//!
//! See the documentation of [`PakfilePlugin`] for usage.

#![deny(missing_docs)]

use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use bevy_app::Plugin;
use bevy_asset::{
    AssetApp,
    io::{
        AssetReader, AssetReaderError, AssetSource, AssetSourceId, ErasedAssetReader, PathStream,
        Reader, file::FileAssetReader,
    },
};
use bevy_log as log;
use futures::StreamExt as _;
use hashbrown::HashSet;
use pak::Pak;
use tokio::sync::SetOnce;

mod pak;

enum PakSource {
    Any(Box<dyn ErasedAssetReader>),
    #[cfg(not(target_arch = "wasm32"))]
    File(FileAssetReader),
}

impl PakSource {
    async fn load_pak(&self, file: &Path) -> Result<Pak, AssetReaderError> {
        match self {
            Self::Any(reader) => {
                let mut reader = reader.read(file).await?;

                let mut out = Vec::new();

                reader.read_to_end(&mut out).await?;

                let bytes = out.into_boxed_slice();

                Ok(Pak::from_backing(file.display(), bytes)?)
            }

            Self::File(reader) => {
                let full_path = reader.root_path().join(file);

                // Safety: The file is only ever accessed as a blob of bytes, so the potential for
                // unsoundness is low, but since it means data can change behind a
                // `&` reference it is technically unsound to open a memmap for any
                // purpose.
                //
                // TODO: Investigate the performance of copying and then unlinking the .pak in order
                // to make the data we access inaccessible to other processes.
                let mmap = unsafe { memmap2::Mmap::map(&std::fs::File::open(full_path)?)? };

                Ok(Pak::from_backing(file.display(), mmap)?)
            }
        }
    }
}

impl AssetReader for PakSource {
    fn read<'a>(
        &'a self,
        path: &'a Path,
    ) -> impl bevy_asset::io::AssetReaderFuture<Value: Reader + 'a> {
        match self {
            Self::Any(reader) => reader.read(path),
            Self::File(reader) => ErasedAssetReader::read(reader, path),
        }
    }

    fn read_meta<'a>(
        &'a self,
        path: &'a Path,
    ) -> impl bevy_asset::io::AssetReaderFuture<Value: Reader + 'a> {
        match self {
            Self::Any(reader) => reader.read_meta(path),
            Self::File(reader) => ErasedAssetReader::read_meta(reader, path),
        }
    }

    fn read_directory<'a>(
        &'a self,
        path: &'a Path,
    ) -> impl bevy_tasks::ConditionalSendFuture<Output = Result<Box<PathStream>, AssetReaderError>>
    {
        match self {
            Self::Any(reader) => reader.read_directory(path),
            Self::File(reader) => ErasedAssetReader::read_directory(reader, path),
        }
    }

    fn is_directory<'a>(
        &'a self,
        path: &'a Path,
    ) -> impl bevy_tasks::ConditionalSendFuture<Output = Result<bool, AssetReaderError>> {
        match self {
            Self::Any(reader) => reader.is_directory(path),
            Self::File(reader) => ErasedAssetReader::is_directory(reader, path),
        }
    }
}

impl From<PathBuf> for PakSource {
    #[cfg(not(target_arch = "wasm32"))]
    fn from(value: PathBuf) -> Self {
        Self::File(FileAssetReader::new(value))
    }

    #[cfg(target_arch = "wasm32")]
    fn from(value: PathBuf) -> Self {
        Self::Any(AssetSource::get_default_reader(value.to_string())())
    }
}

impl From<Box<dyn ErasedAssetReader>> for PakSource {
    fn from(value: Box<dyn ErasedAssetReader>) -> Self {
        Self::Any(value)
    }
}

type MakeSource = dyn Fn() -> PakSource + Send + Sync + 'static;

/// The core plugin to enable reading from pakfiles. Note that if you do not explicitly set a source
/// ID using [`PakfilePlugin::with_source_id`], this _must_ be added before Bevy's asset plugin.
///
/// Note that when using [`PakfilePlugin::push_reader`], the pak will be loaded into memory, but
/// with [`PakfilePlugin::push_path`], the files will be mapped using [`memmap2`](https://crates.io/crates/memmap2).
/// On platforms that do not support memory mapping, all pakfiles will be loaded into memory before
/// they can be used.
///
/// ```no_run
/// # use bevy::prelude::*;
/// # use bevy_mod_pakfile::PakfilePlugin;
///
/// let id1_path = std::env::current_dir().unwrap().join("id1");
/// let mut app = App::new();
/// app.add_plugins(PakfilePlugin::from_paths([id1_path.display()]))
///     .add_plugins(DefaultPlugins);
/// ```
pub struct PakfilePlugin {
    sources: Vec<Arc<MakeSource>>,
    source_id: AssetSourceId<'static>,
}

impl Default for PakfilePlugin {
    fn default() -> Self {
        Self::from_paths::<[&str; 0]>([])
    }
}

impl PakfilePlugin {
    /// Create a new empty [`PakfilePlugin`]. Note that if you add this to an app without adding any
    /// directories with [`PakfilePlugin::push_path`] or [`PakfilePlugin::push_reader`] then trying
    /// to read any asset will fail.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a game directory to search for pakfiles. Note that paths are evaluated in reverse order,
    /// so if you want to have a mod `foo` that depends on `id1`, you'd pass [`id1`, `foo`]. On
    /// platforms that support it, pakfiles in directories added using this method will be
    /// loaded using a memory map for maximum efficiency.
    pub fn push_path<P: Into<PathBuf>>(&mut self, path: P) -> &mut Self {
        let path = path.into();

        self.sources.push(Arc::new(move || PakSource::from(path.clone())) as Arc<MakeSource>);

        self
    }

    /// Add an arbitrary source to search for pakfiles. The returned reader must be able to list
    /// directories. If you want to use an [`AssetReader`] that is not able to do this, such as
    /// web sources, you should implement a wrapper that shims in this ability - e.g. by
    /// searching for `pak0.pak`, `pak1.pak`, etc and returning the paths that resolve.
    pub fn push_reader<MkReader>(&mut self, make_reader: MkReader) -> &mut Self
    where MkReader: Fn() -> Box<dyn ErasedAssetReader> + Send + Sync + 'static {
        self.sources.push(Arc::new(move || PakSource::from(make_reader())) as Arc<MakeSource>);

        self
    }

    /// Create a `PakfilePlugin` from a list of game paths. Paths are evaluated in reverse order, so
    /// if you want to have a mod `foo` that depends on `id1`, you'd pass [`id1`, `foo`].
    pub fn from_paths<I>(paths: I) -> Self
    where
        I: IntoIterator,
        I::Item: Into<PathBuf>, {
        Self {
            sources: paths
                .into_iter()
                .map(|path| {
                    let path = path.into();
                    Arc::new(move || PakSource::from(path.clone())) as Arc<MakeSource>
                })
                .collect(),
            source_id: AssetSourceId::Default,
        }
    }

    /// Set the source ID that assets in the underlying pakfiles will be accessed using. By default,
    /// this will register to the default asset source.
    pub fn with_source_id(&mut self, source_id: AssetSourceId<'_>) -> &mut Self {
        self.source_id = source_id.into_owned();

        self
    }
}

struct PakCollection {
    readers: SetOnce<Box<[Box<dyn ErasedAssetReader>]>>,
    dir_reader: PakSource,
}

struct VfsCollection {
    inner: Box<[Box<dyn ErasedAssetReader>]>,
}

impl PakCollection {
    async fn readers<'a>(&'a self) -> impl DoubleEndedIterator<Item = &'a dyn ErasedAssetReader> {
        // We need this for the types of the returned iterators to be the same.
        #[allow(clippy::borrowed_box)]
        let deref_box = |r: &'a Box<dyn ErasedAssetReader>| -> &'a dyn ErasedAssetReader { &**r };
        if let Some(readers) = self.readers.get() {
            readers.iter().map(deref_box)
        } else {
            let mut dir = match AssetReader::read_directory(&self.dir_reader, Path::new("")).await {
                Ok(dir) => dir,
                Err(e) => {
                    log::error!("Could not read pakfile directory: {e}");
                    let _ = self.readers.set(Box::new([]));
                    return self.readers.wait().await.iter().map(deref_box);
                }
            };

            let mut pakfiles = Vec::new();

            while let Some(file) = dir.next().await {
                let file_extension_is_pak =
                    file.extension().map(|ext| ext.eq_ignore_ascii_case("pak")).unwrap_or(false);

                if !file_extension_is_pak {
                    continue;
                }

                let pakfile = match self.dir_reader.load_pak(&file).await {
                    Ok(pakfile) => pakfile,
                    Err(e) => {
                        let file = file.display();
                        log::warn!("Could not load pakfile {file}: {e}");
                        continue;
                    }
                };

                pakfiles.push(pakfile);
            }

            pakfiles.sort_unstable_by(|a, b| a.name().cmp(b.name()));

            let pakfiles = pakfiles
                .into_iter()
                .map(|pakfile| Box::new(pakfile) as Box<dyn ErasedAssetReader>)
                .collect::<Vec<_>>();

            let _ = self.readers.set(pakfiles.into_boxed_slice());

            self.readers.wait().await.iter().map(deref_box)
        }
    }
}

// TODO: We should have a `bevy_mod_glob_reader` package that adds glob support to readers.
// TODO: This isn't specific to pakfiles, `VfsCollection` is more generic. Maybe glob and overlay
// should be part of the same package?
impl AssetReader for VfsCollection {
    async fn read<'a>(&'a self, path: &'a Path) -> Result<impl Reader + 'a, AssetReaderError> {
        let mut err = AssetReaderError::NotFound(path.to_owned());

        for reader in self.inner.iter().rev() {
            match reader.read(path).await {
                Ok(reader) => return Ok(reader),
                Err(e) => err = e,
            }
        }

        Err(err)
    }

    async fn read_meta<'a>(&'a self, path: &'a Path) -> Result<impl Reader + 'a, AssetReaderError> {
        let mut err = AssetReaderError::NotFound(path.to_owned());

        for reader in self.inner.iter().rev() {
            match reader.read_meta(path).await {
                Ok(reader) => return Ok(reader),
                Err(e) => err = e,
            }
        }

        Err(err)
    }

    async fn read_directory<'a>(
        &'a self,
        path: &'a Path,
    ) -> Result<Box<PathStream>, AssetReaderError> {
        let path = path.to_owned();

        let out = futures::future::join_all(self.inner.iter().map(|reader| async {
            match reader.read_directory(&path).await {
                Ok(paths) => paths.collect::<Vec<_>>().await,
                Err(_) => Vec::new(),
            }
        }))
        .await;

        let out = out.into_iter().flatten().collect::<HashSet<_>>();

        Ok(Box::new(futures::stream::iter(out)))
    }

    async fn is_directory<'a>(&'a self, path: &'a Path) -> Result<bool, AssetReaderError> {
        let mut err = AssetReaderError::NotFound(path.to_owned());

        for reader in self.inner.iter().rev() {
            match reader.is_directory(path).await {
                Ok(is_directory) => return Ok(is_directory),
                Err(e) => err = e,
            }
        }

        Err(err)
    }
}

impl AssetReader for PakCollection {
    async fn read<'a>(&'a self, path: &'a Path) -> Result<impl Reader + 'a, AssetReaderError> {
        for reader in self.readers().await.rev() {
            if let Ok(reader) = reader.read(path).await {
                return Ok(reader);
            }
        }

        ErasedAssetReader::read(&self.dir_reader, path).await
    }

    async fn read_meta<'a>(&'a self, path: &'a Path) -> Result<impl Reader + 'a, AssetReaderError> {
        for reader in self.readers().await.rev() {
            if let Ok(reader) = reader.read_meta(path).await {
                return Ok(reader);
            }
        }

        ErasedAssetReader::read_meta(&self.dir_reader, path).await
    }

    async fn read_directory<'a>(
        &'a self,
        path: &'a Path,
    ) -> Result<Box<PathStream>, AssetReaderError> {
        let path = path.to_owned();

        let out = futures::future::join_all(
            self.readers()
                .await
                .rev()
                .chain(std::iter::once(&self.dir_reader as &dyn ErasedAssetReader))
                .map(|reader| async {
                    match reader.read_directory(&path).await {
                        Ok(paths) => paths.collect::<Vec<_>>().await,
                        Err(_) => Vec::new(),
                    }
                }),
        )
        .await;

        let out = out.into_iter().flatten().collect::<HashSet<_>>();

        Ok(Box::new(futures::stream::iter(out)))
    }

    async fn is_directory<'a>(&'a self, path: &'a Path) -> Result<bool, AssetReaderError> {
        for reader in self.readers().await.rev() {
            if let Ok(is_dir) = reader.is_directory(path).await {
                return Ok(is_dir);
            }
        }

        ErasedAssetReader::is_directory(&self.dir_reader, path).await
    }
}

impl Plugin for PakfilePlugin {
    fn build(&self, app: &mut bevy_app::App) {
        let sources = self.sources.clone();
        app.register_asset_source(
            AssetSourceId::Default,
            AssetSource::build().with_reader(move || {
                let asset_readers = sources
                    .iter()
                    .map(|reader| {
                        Box::new(PakCollection { readers: SetOnce::new(), dir_reader: reader() })
                            as Box<dyn ErasedAssetReader + 'static>
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice();

                Box::new(VfsCollection { inner: asset_readers })
            }),
        );
    }
}
