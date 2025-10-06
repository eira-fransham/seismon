use std::{path::Path, sync::Arc};

use bevy::{
    app::Plugin,
    asset::{
        AssetApp,
        io::{
            AssetReader, AssetReaderError, AssetSource, AssetSourceId, ErasedAssetReader,
            PathStream, Reader,
        },
    },
    log,
};
use futures::StreamExt as _;
use hashbrown::HashSet;
use pak::Pak;
use tokio::sync::SetOnce;

mod pak;

const DEFAULT_BASE_NAME: &str = "id1";

type MakeSource = dyn Fn() -> Box<dyn ErasedAssetReader> + Send + Sync + 'static;
#[non_exhaustive]
pub struct PakfilePlugin {
    sources: Vec<Arc<MakeSource>>,
}

impl Default for PakfilePlugin {
    fn default() -> Self {
        Self {
            sources: vec![Arc::new(|| {
                AssetSource::get_default_reader(DEFAULT_BASE_NAME.to_string())()
            })],
        }
    }
}

impl PakfilePlugin {
    /// Create a `PakfilePlugin` from a list of game paths. Paths are evaluated in reverse order, so
    /// if you want to have a mod `foo` that depends on `id1`, you'd pass [`id1`, `foo`].
    pub fn from_paths<I>(paths: I) -> Self
    where
        I: IntoIterator,
        I::Item: ToString,
    {
        Self {
            sources: paths
                .into_iter()
                .map(|path| {
                    let path = path.to_string();
                    Arc::new(move || AssetSource::get_default_reader(path.clone())())
                        as Arc<MakeSource>
                })
                .collect(),
        }
    }
}

pub const MAX_PAKFILES: usize = 32;

struct PakCollection {
    readers: SetOnce<Box<[Box<dyn ErasedAssetReader>]>>,
    dir_reader: Box<dyn ErasedAssetReader>,
}

pub struct VfsCollection {
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
            let mut dir = match self.dir_reader.read_directory(Path::new("")).await {
                Ok(dir) => dir,
                Err(e) => {
                    log::error!("Could not read pakfile directory: {e}");
                    let _ = self.readers.set(Box::new([]));
                    return self.readers.wait().await.iter().map(deref_box);
                }
            };

            let mut pakfiles = Vec::new();

            while let Some(file) = dir.next().await {
                let file_extension_is_pak = file
                    .extension()
                    .map(|ext| ext.eq_ignore_ascii_case("pak"))
                    .unwrap_or(false);

                if !file_extension_is_pak {
                    continue;
                }

                let Ok(mut reader) = self.dir_reader.read(&file).await else {
                    continue;
                };

                let mut out = Vec::new();

                if reader.read_to_end(&mut out).await.is_err() {
                    continue;
                }

                let bytes = out.into_boxed_slice();

                let pakfile = match Pak::from_backing(file.display(), bytes) {
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

        self.dir_reader.read(path).await
    }

    async fn read_meta<'a>(&'a self, path: &'a Path) -> Result<impl Reader + 'a, AssetReaderError> {
        for reader in self.readers().await.rev() {
            if let Ok(reader) = reader.read_meta(path).await {
                return Ok(reader);
            }
        }

        self.dir_reader.read_meta(path).await
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
                .chain(std::iter::once(&*self.dir_reader))
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

        self.dir_reader.is_directory(path).await
    }
}

impl Plugin for PakfilePlugin {
    fn build(&self, app: &mut bevy::app::App) {
        let sources = self.sources.clone();
        app.register_asset_source(
            AssetSourceId::Default,
            AssetSource::build().with_reader(move || {
                let asset_readers = sources
                    .iter()
                    .map(|reader| {
                        Box::new(PakCollection {
                            readers: SetOnce::new(),
                            dir_reader: reader(),
                        }) as Box<dyn ErasedAssetReader + 'static>
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice();

                Box::new(VfsCollection {
                    inner: asset_readers,
                })
            }),
        );
    }
}
