// Copyright © 2018 Cormac O'Brien
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

// TODO: `derive(Fail)` currently triggers this, and at time of writing it is up-to-date
#![allow(non_local_definitions)]

use std::{
    borrow::Cow,
    fmt::{self, Display},
    io::{self, BufReader, Cursor, SeekFrom},
    iter,
};

use bevy::{
    asset::{
        AssetLoader, AssetPath, AsyncReadExt as _, LoadContext, LoadedAsset, RenderAssetUsages,
    },
    platform::collections::HashMap,
    prelude::*,
};
use failure::{Backtrace, Context, Fail};
use futures::{AsyncRead, AsyncSeekExt, TryFutureExt};
use futures_byteorder::{AsyncReadBytes, LittleEndian};
use serde::{Deserialize, Serialize};
use wgpu::{Extent3d, TextureDimension};

// see definition of lumpinfo_t:
// https://github.com/id-Software/Quake/blob/master/WinQuake/wad.h#L54-L63
const MAGIC: u32 = 'W' as u32 | ('A' as u32) << 8 | ('D' as u32) << 16 | ('2' as u32) << 24;

#[derive(Debug)]
pub struct WadError {
    inner: Context<WadErrorKind>,
}

impl WadError {
    pub fn kind(&self) -> WadErrorKind {
        *self.inner.get_context()
    }
}

impl From<WadErrorKind> for WadError {
    fn from(kind: WadErrorKind) -> Self {
        WadError { inner: Context::new(kind) }
    }
}

impl From<Context<WadErrorKind>> for WadError {
    fn from(inner: Context<WadErrorKind>) -> Self {
        WadError { inner }
    }
}

impl From<io::Error> for WadError {
    fn from(io_error: io::Error) -> Self {
        let kind = io_error.kind();
        match kind {
            io::ErrorKind::UnexpectedEof => io_error.context(WadErrorKind::UnexpectedEof).into(),
            _ => io_error.context(WadErrorKind::Io).into(),
        }
    }
}

impl Fail for WadError {
    fn cause(&self) -> Option<&dyn Fail> {
        self.inner.cause()
    }

    fn backtrace(&self) -> Option<&Backtrace> {
        self.inner.backtrace()
    }
}

impl Display for WadError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.inner, f)
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug, Fail)]
pub enum WadErrorKind {
    #[fail(display = "CONCHARS must be loaded with the dedicated function")]
    ConcharsUseDedicatedFunction,
    #[fail(display = "Invalid magic number")]
    InvalidMagicNumber,
    #[fail(display = "I/O error")]
    Io,
    #[fail(display = "No such file in WAD")]
    NoSuchFile,
    #[fail(display = "Failed to load QPic")]
    QPicNotLoaded,
    #[fail(display = "Unexpected end of data")]
    UnexpectedEof,
}

#[derive(Debug, Default, Reflect)]
#[non_exhaustive]
pub struct PaletteLoader {}

impl AssetLoader for PaletteLoader {
    type Asset = Palette;
    type Settings = ();
    type Error = std::io::Error;

    fn load(
        &self,
        reader: &mut dyn bevy::asset::io::Reader,
        _settings: &Self::Settings,
        _load_context: &mut bevy::asset::LoadContext,
    ) -> impl bevy::tasks::ConditionalSendFuture<Output = std::result::Result<Self::Asset, Self::Error>>
    {
        Palette::load(reader)
    }

    fn extensions(&self) -> &[&str] {
        &["lmp"]
    }
}

#[derive(Asset, Reflect)]
pub struct Palette {
    pub rgb: [[u8; 3]; 256],
}

impl Palette {
    pub async fn load(reader: &mut dyn bevy::asset::io::Reader) -> Result<Palette, std::io::Error> {
        let mut rgb = [[0u8; 3]; 256];

        reader.read_exact(rgb.as_flattened_mut()).await?;

        Ok(Palette { rgb })
    }

    // TODO: this will not render console characters correctly, as they use index 0 (black) to
    // indicate transparency.
    /// Translates a set of indices into a list of RGBA values and a list of fullbright
    /// values.
    pub fn translate(&self, indices: &[u8], transparent_color: Option<u8>) -> Vec<u8> {
        let transparent_color = transparent_color.unwrap_or(0xff);

        indices
            .iter()
            .copied()
            .flat_map(|i| {
                if i == transparent_color {
                    [0; 4]
                } else {
                    let [r, g, b] = self.rgb[i as usize];
                    [r, g, b, 0xff]
                }
            })
            .collect()
    }
}

#[derive(Asset, Reflect)]
pub struct QPic {
    width: u32,
    height: u32,
    indices: Vec<u8>,
}

#[derive(Debug, Default, Reflect)]
#[non_exhaustive]
pub struct QPicLoader {}

#[derive(Debug, Reflect, Serialize, Deserialize)]
pub struct QPicLoaderSettings {
    pub palette_path: AssetPath<'static>,
    pub size: Option<UVec2>,
    pub transparent_color: Option<u8>,
}

pub const DEFAULT_PALETTE_PATH: &str = "gfx/palette.lmp";

impl Default for QPicLoaderSettings {
    fn default() -> Self {
        Self { palette_path: DEFAULT_PALETTE_PATH.into(), size: None, transparent_color: None }
    }
}

async fn load_qpic_as_image<R>(
    reader: &mut R,
    settings: &QPicLoaderSettings,
    load_context: &mut LoadContext<'_>,
) -> Result<Image, std::io::Error>
where
    R: AsyncRead + Unpin + ?Sized,
{
    let palette: LoadedAsset<Palette> = load_context
        .loader()
        .immediate()
        .load(&settings.palette_path)
        .await
        .map_err(std::io::Error::other)?;

    let qpic = QPic::load(reader, settings.size).await?;

    let data = palette.get().translate(qpic.indices(), settings.transparent_color);

    Ok(Image::new(
        Extent3d { width: qpic.width(), height: qpic.height(), depth_or_array_layers: 1 },
        TextureDimension::D2,
        data,
        wgpu::TextureFormat::Rgba8UnormSrgb,
        RenderAssetUsages::RENDER_WORLD,
    ))
}

impl AssetLoader for QPicLoader {
    type Asset = Image;
    type Settings = QPicLoaderSettings;
    type Error = std::io::Error;

    fn load(
        &self,
        reader: &mut dyn bevy::asset::io::Reader,
        settings: &Self::Settings,
        load_context: &mut LoadContext,
    ) -> impl bevy::tasks::ConditionalSendFuture<Output = std::result::Result<Self::Asset, Self::Error>>
    {
        load_qpic_as_image(reader, settings, load_context)
    }

    fn extensions(&self) -> &[&str] {
        &["lmp"]
    }
}

impl QPic {
    pub async fn load<R>(mut reader: &mut R, size: Option<UVec2>) -> Result<QPic, std::io::Error>
    where
        R: AsyncRead + Unpin + ?Sized,
    {
        let mut reader = AsyncReadBytes::new(&mut reader);

        let [width, height] = match size {
            Some(size) => size.into(),
            None => {
                [reader.read_u32::<LittleEndian>().await?, reader.read_u32::<LittleEndian>().await?]
            }
        };

        let mut indices = Vec::new();
        (*reader).take((width * height) as u64).read_to_end(&mut indices).await?;

        indices.extend(iter::repeat_n(0xFF, (width * height) as usize - indices.len()));

        Ok(QPic { width, height, indices })
    }

    pub fn width(&self) -> u32 {
        self.width
    }

    pub fn height(&self) -> u32 {
        self.height
    }

    pub fn indices(&self) -> &[u8] {
        &self.indices
    }
}

// TODO: This can probably be made somewhat generic
#[derive(Debug, Default, Reflect)]
#[non_exhaustive]
pub struct ConcharsLoader {}

#[derive(Debug, Reflect, Serialize, Deserialize)]
pub struct ConcharsLoaderSettings {
    pub conchars_name: Cow<'static, str>,
    pub palette_path: AssetPath<'static>,
}

impl Default for ConcharsLoaderSettings {
    fn default() -> Self {
        Self { conchars_name: "CONCHARS".into(), palette_path: DEFAULT_PALETTE_PATH.into() }
    }
}

const CONCHARS_SIZE: u32 = 128;

impl AssetLoader for ConcharsLoader {
    type Asset = Image;
    type Settings = ConcharsLoaderSettings;
    type Error = failure::Error;

    fn load(
        &self,
        reader: &mut dyn bevy::asset::io::Reader,
        settings: &Self::Settings,
        load_context: &mut LoadContext,
    ) -> impl bevy::tasks::ConditionalSendFuture<Output = std::result::Result<Self::Asset, Self::Error>>
    {
        Wad::find_file(reader, &*settings.conchars_name).and_then(move |mut file| async move {
            load_qpic_as_image(
                &mut file,
                &QPicLoaderSettings {
                    palette_path: settings.palette_path.clone(),
                    size: Some(UVec2::new(CONCHARS_SIZE, CONCHARS_SIZE)),
                    transparent_color: Some(0),
                },
                load_context,
            )
            .await
            .map_err(Into::into)
        })
    }

    fn extensions(&self) -> &[&str] {
        &["wad"]
    }
}

#[derive(Asset, Reflect)]
pub struct Wad {
    files: HashMap<String, Vec<u8>>,
}

impl Wad {
    pub async fn find_file<'a>(
        root_reader: &'a mut dyn bevy::asset::io::Reader,
        name: &str,
    ) -> Result<impl AsyncRead + Unpin + 'a, failure::Error> {
        let mut root_reader = root_reader.seekable()?;
        let mut reader = AsyncReadBytes::new(&mut root_reader);

        let magic = reader.read_u32::<LittleEndian>().await?;
        if magic != MAGIC {
            return Err(WadErrorKind::InvalidMagicNumber.into());
        }

        let lump_count = reader.read_u32::<LittleEndian>().await?;
        let lumpinfo_ofs = reader.read_u32::<LittleEndian>().await?;

        root_reader.seek(SeekFrom::Start(lumpinfo_ofs as u64)).await?;

        let mut reader = AsyncReadBytes::new(&mut root_reader);

        let mut file_info = None::<(u32, u32)>;

        for _ in 0..lump_count {
            // TODO sanity check these values
            let offset = reader.read_u32::<LittleEndian>().await?;
            let _size_on_disk = reader.read_u32::<LittleEndian>().await?;
            let size = reader.read_u32::<LittleEndian>().await?;
            let _type = reader.read_u8().await?;
            let _compression = reader.read_u8().await?;
            let _pad = reader.read_u16::<LittleEndian>().await?;
            let mut name_bytes = [0u8; 16];
            reader.read_exact(&mut name_bytes).await?;
            let lump_name =
                seismon_utils::read_cstring(&mut BufReader::new(Cursor::new(name_bytes)))?;

            if file_info.is_none() && &*lump_name.to_str() == name {
                file_info = Some((offset, size))
            }
        }

        if let Some((offset, size)) = file_info {
            root_reader.seek(SeekFrom::Start(dbg!(offset as u64))).await?;
            Ok(root_reader.take(size as u64))
        } else {
            Err(failure::err_msg(format!("Could not find file {name} in the .wad")))
        }
    }
}
