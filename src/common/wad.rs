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
use futures::{AsyncRead, AsyncSeekExt};
use futures_byteorder::{AsyncReadBytes, LittleEndian};
use num::FromPrimitive as _;
use num_derive::FromPrimitive;
use seismon_utils::QString;
use serde::{Deserialize, Serialize};
use wgpu::{Extent3d, TextureDimension};

// see definition of lumpinfo_t:
// https://github.com/id-Software/Quake/blob/master/WinQuake/wad.h#L54-L63
const WAD_MAGIC: u32 = 'W' as u32 | ('A' as u32) << 8 | ('D' as u32) << 16 | ('2' as u32) << 24;

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
    #[fail(display = "Invalid filetype found in wad")]
    InvalidFiletype,
    #[fail(display = "Invalid compression kind found in wad")]
    InvalidCompression,
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
        indices
            .iter()
            .copied()
            .flat_map(|i| {
                if Some(i) == transparent_color {
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
        Self {
            palette_path: DEFAULT_PALETTE_PATH.into(),
            size: None,
            transparent_color: Some(0xff),
        }
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

#[derive(Debug, Default, Reflect)]
#[non_exhaustive]
pub struct QPicWadLoader {}

#[derive(Debug, Reflect, Serialize, Deserialize)]
pub struct QPicWadLoaderSettings {
    pub palette_path: AssetPath<'static>,
    pub transparent_color: Option<u8>,
}

impl Default for QPicWadLoaderSettings {
    fn default() -> Self {
        let default_qpic_loader_settings = QPicLoaderSettings::default();

        Self {
            palette_path: default_qpic_loader_settings.palette_path,
            transparent_color: default_qpic_loader_settings.transparent_color,
        }
    }
}

#[derive(Debug, Default, Reflect)]
#[non_exhaustive]
pub struct WadLoader {}

#[derive(Debug, Reflect, Serialize, Deserialize)]
pub struct WadLoaderSettings {
    pub conchars_name: Cow<'static, str>,
    pub palette_path: AssetPath<'static>,
}

impl Default for WadLoaderSettings {
    fn default() -> Self {
        Self { conchars_name: "CONCHARS".into(), palette_path: DEFAULT_PALETTE_PATH.into() }
    }
}

/// Handles `CONCHARS`, will not handle other miptexes and may be incorrect for mods using conchars
/// with different resolution.
const PRESUMED_MIPTEX_SIZE: u32 = 128;

impl AssetLoader for WadLoader {
    type Asset = Wad;
    type Settings = WadLoaderSettings;
    type Error = failure::Error;

    fn load(
        &self,
        reader: &mut dyn bevy::asset::io::Reader,
        settings: &Self::Settings,
        load_context: &mut LoadContext,
    ) -> impl bevy::tasks::ConditionalSendFuture<Output = std::result::Result<Self::Asset, Self::Error>>
    {
        async move {
            let mut root_reader = reader.seekable()?;
            let mut reader = AsyncReadBytes::new(&mut root_reader);

            let magic = reader.read_u32::<LittleEndian>().await?;
            if magic != WAD_MAGIC {
                return Err(WadErrorKind::InvalidMagicNumber.into());
            }

            let lump_count = reader.read_u32::<LittleEndian>().await?;
            let lumpinfo_ofs = reader.read_u32::<LittleEndian>().await?;

            root_reader.seek(SeekFrom::Start(lumpinfo_ofs as u64)).await?;

            let mut reader = AsyncReadBytes::new(&mut root_reader);

            let mut file_infos = Vec::<LumpHeader>::with_capacity(lump_count as usize);

            for _ in 0..lump_count {
                file_infos.push(LumpHeader::read(&mut reader).await?);
            }

            let mut files = HashMap::<String, WadFile>::with_capacity(lump_count as usize);

            for info in file_infos {
                let name = info.name.to_str().into_owned();

                root_reader.seek(SeekFrom::Start(info.offset as u64)).await?;
                let mut reader = root_reader.take(info.size as u64);
                let file = match info.type_ {
                    WadFiletype::QPic => {
                        let image = load_qpic_as_image(
                            &mut reader,
                            &QPicLoaderSettings {
                                palette_path: settings.palette_path.clone(),
                                ..Default::default()
                            },
                            load_context,
                        )
                        .await?;
                        let handle = load_context.add_labeled_asset(name.clone(), image);

                        WadFile::Image(handle)
                    }
                    WadFiletype::Miptex => {
                        let image = load_qpic_as_image(
                            &mut reader,
                            &QPicLoaderSettings {
                                palette_path: settings.palette_path.clone(),
                                size: Some(UVec2::new(PRESUMED_MIPTEX_SIZE, PRESUMED_MIPTEX_SIZE)),
                                // TODO: Is this correct for anything other than conchars?
                                transparent_color: Some(0x00),
                            },
                            load_context,
                        )
                        .await?;
                        let handle = load_context.add_labeled_asset(name.clone(), image);

                        WadFile::Image(handle)
                    }
                    _ => WadFile::Unknown,
                };

                files.insert(name, file);
            }

            Ok(Wad { files })
        }
    }

    fn extensions(&self) -> &[&str] {
        &["wad"]
    }
}

#[derive(Reflect, Clone, Debug)]
enum WadFile {
    Palette(Handle<Palette>),
    Image(Handle<Image>),
    // TODO: Should we store the bytes in this case? Seems like a waste of memory
    Unknown,
}

#[derive(Asset, Reflect, Debug)]
pub struct Wad {
    files: HashMap<String, WadFile>,
}

#[derive(Debug)]
#[expect(dead_code, reason = "Need to debug these fields")]
struct LumpHeader {
    offset: u32,
    size_on_disk: u32,
    size: u32,
    type_: WadFiletype,
    compression: WadCompression,
    pad: u16,
    name: QString,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, FromPrimitive)]
enum WadCompression {
    None = 0,
    LZ2 = 1,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, FromPrimitive)]
enum WadFiletype {
    None = 0,
    Label = 1,
    /// Quake has the same value for both `TYP_LUMPY` and `TYP_PALETTE`
    PaletteOrLumpy = 64,
    QTex = 65,
    QPic = 66,
    Sound = 67,
    Miptex = 68,
}

impl LumpHeader {
    pub async fn read<R>(reader: &mut AsyncReadBytes<'_, R>) -> Result<Self, failure::Error>
    where
        R: AsyncRead + Unpin,
    {
        // TODO sanity check these values
        let offset = reader.read_u32::<LittleEndian>().await?;
        let size_on_disk = reader.read_u32::<LittleEndian>().await?;
        let size = reader.read_u32::<LittleEndian>().await?;
        let type_ =
            WadFiletype::from_u8(reader.read_u8().await?).ok_or(WadErrorKind::InvalidFiletype)?;
        let compression = WadCompression::from_u8(reader.read_u8().await?)
            .ok_or(WadErrorKind::InvalidCompression)?;
        let pad = reader.read_u16::<LittleEndian>().await?;

        let mut name_bytes = [0u8; 16];
        reader.read_exact(&mut name_bytes).await?;
        let name = seismon_utils::read_cstring(&mut BufReader::new(Cursor::new(name_bytes)))?;

        Ok(Self { offset, size_on_disk, size, type_, compression, pad, name })
    }
}
