use std::{io, time::Duration};

use bevy_app::{Plugin, PostUpdate};
use bevy_asset::{Asset, AssetApp, AssetLoader, Handle, RenderAssetUsages};
use bevy_ecs::{schedule::IntoScheduleConfigs as _, world::World};
use bevy_image::Image;
use bevy_log::{debug, warn};
use bevy_math::Vec3;
use bevy_mesh::Mesh;
use bevy_mod_billboard::{BillboardMesh, BillboardTexture};
use bevy_reflect::Reflect;
use bevy_render::render_resource::{Extent3d, TextureDimension, TextureFormat};
use bevy_shape::Rectangle;
use bevy_tasks::ConditionalSendFuture;
use bevy_transform::components::Transform;
use bevy_world_serialization::WorldAsset;
use futures::AsyncReadExt as _;
use futures_byteorder::{AsyncReadBytes, LittleEndian};
use num::FromPrimitive;
use num_derive::FromPrimitive;
use qbsp::Palette;
use seismon_utils::model::SyncType;
use serde::{Deserialize, Serialize};
use snafu::Snafu;

use crate::anim::CalculatedSprSettings;

pub mod anim;

const MAGIC: u32 = u32::from_le_bytes(*b"IDSP");
const VERSION: u32 = 1;

/// Upper bound on how many frames/subframes to preallocate space for, so that a corrupt count
/// can't trigger a huge allocation before we find out that the data isn't there.
const MAX_PREALLOC: usize = 1024;

#[derive(Snafu, Debug)]
pub enum SprFileError {
    #[snafu(context(false), display("I/O error: {source}"))]
    Io { source: io::Error },
    #[snafu(display("Bad magic number for sprite (got {magic:#x}, should be {MAGIC:#x})"))]
    BadMagic { magic: u32 },
    #[snafu(display("Bad version number for sprite (got {version}, should be {VERSION})"))]
    BadVersion { version: u32 },
    #[snafu(display("Invalid sprite kind ({kind})"))]
    InvalidKind { kind: i32 },
    #[snafu(display("Invalid sync type ({sync_type})"))]
    InvalidSyncType { sync_type: i32 },
    #[snafu(display("Negative {what} ({value})"))]
    NegativeSize { what: &'static str, value: i32 },
    #[snafu(display("Frame size {width}x{height} is too large"))]
    SizeOverflow { width: i32, height: i32 },
    #[snafu(display("Invalid frame count ({count}), must be at least 1"))]
    InvalidFrameCount { count: i32 },
    #[snafu(display("Invalid subframe count ({count}) in frame {frame}"))]
    InvalidSubframeCount { frame: usize, count: i32 },
}

fn non_negative(what: &'static str, value: i32) -> Result<i32, SprFileError> {
    if value < 0 { Err(SprFileError::NegativeSize { what, value }) } else { Ok(value) }
}

#[derive(Default)]
#[non_exhaustive]
pub struct SprPlugin {}

impl Plugin for SprPlugin {
    fn build(&self, app: &mut bevy_app::App) {
        app.init_asset::<Sprite>()
            .init_asset_loader::<SpriteLoader>()
            .add_systems(PostUpdate, (anim::propagate_spr_settings, anim::update_sprs).chain());
    }
}

#[derive(Reflect, Default)]
pub struct SpriteLoader {
    pub default_palette: Palette,
}

#[derive(Deserialize, Serialize, Default)]
pub struct SpriteLoaderSettings {
    pub override_palette: Option<Palette>,
}

impl AssetLoader for SpriteLoader {
    type Asset = WorldAsset;
    type Settings = SpriteLoaderSettings;
    type Error = SprFileError;

    fn load(
        &self,
        reader: &mut dyn bevy_asset::io::Reader,
        settings: &Self::Settings,
        load_context: &mut bevy_asset::LoadContext,
    ) -> impl ConditionalSendFuture<Output = Result<Self::Asset, Self::Error>> {
        fn translate_tex(tex: &[u8], width: u32, height: u32, palette: &Palette) -> Image {
            let data = tex
                .iter()
                .flat_map(|i| match i {
                    0xff => [0; 4],
                    i => {
                        let [r, g, b] = palette.colors[*i as usize];

                        [r, g, b, 0xff]
                    }
                })
                .collect();

            Image::new(
                Extent3d { width, height, depth_or_array_layers: 1 },
                TextureDimension::D2,
                data,
                TextureFormat::Rgba8Unorm,
                RenderAssetUsages::RENDER_WORLD,
            )
        }

        async move {
            const SPRITE_HANDLE_NAME: &str = "sprite";

            let raw = load(reader).await?;

            let billboard_mesh =
                load_context.add_labeled_asset("billboard", Mesh::from(Rectangle::new(1., 1.)));

            // TODO: This is a lot of code, it'd be nice to deduplicate it with `bevy_mod_mdl`
            let processed_sprite_frames = raw
                .frames
                .into_iter()
                .enumerate()
                .filter_map(|(i, tex)| {
                    let frame = match tex {
                        SpriteFrame::Static { frame } => frame,
                        SpriteFrame::Animated { subframes, .. } => {
                            // TODO: Support animated frame groups.
                            if subframes.len() > 1 {
                                warn!(
                                    "{}: frame {i} is a group of {} subframes, which isn't \
                                     supported yet: using the first subframe",
                                    load_context.path(),
                                    subframes.len(),
                                );
                            }

                            subframes.into_iter().next()?
                        }
                    };

                    let img = load_context.add_labeled_asset(
                        format!("img{i}"),
                        translate_tex(
                            &frame.image,
                            frame.width,
                            frame.height,
                            settings.override_palette.as_ref().unwrap_or(&self.default_palette),
                        ),
                    );

                    Some((img, frame.width, frame.height))
                })
                .map(|(image, width, height)| SpriteSubframe {
                    width,
                    height,
                    image: image.clone(),
                });

            let mut world = World::new();

            let out_sprite = Sprite {
                kind: raw.kind,
                max_width: raw.max_width,
                max_height: raw.max_height,
                radius: raw.radius,
                frames: processed_sprite_frames
                    .map(|frame| SpriteFrame::Static { frame })
                    .collect(),
            };
            let out_sprite_model_handle = load_context.get_label_handle(SPRITE_HANDLE_NAME);

            // TODO: Support animated
            if let Some(SpriteFrame::Static { frame: first_frame }) = out_sprite.frames.first() {
                world.spawn((
                    CalculatedSprSettings::from(out_sprite_model_handle.clone()),
                    BillboardTexture(first_frame.image.clone()),
                    BillboardMesh(billboard_mesh.clone()),
                    Transform::from_scale(Vec3::new(
                        first_frame.width as _,
                        first_frame.height as _,
                        1.,
                    )),
                ));
            }

            load_context.add_labeled_asset(SPRITE_HANDLE_NAME, out_sprite);

            Ok(WorldAsset { world })
        }
    }

    fn extensions(&self) -> &[&str] {
        &["spr"]
    }
}

#[derive(Clone, Copy, Debug, Eq, FromPrimitive, PartialEq, Reflect)]
pub enum SpriteKind {
    ViewPlaneParallelUpright = 0,
    Upright = 1,
    ViewPlaneParallel = 2,
    Oriented = 3,
    ViewPlaneParallelOriented = 4,
}

#[derive(Debug, Clone, Reflect, Asset)]
pub struct Sprite {
    kind: SpriteKind,
    max_width: usize,
    max_height: usize,
    radius: f32,
    frames: Vec<SpriteFrame<Handle<Image>>>,
}

impl Sprite {
    pub fn min(&self) -> Vec3 {
        Vec3::new(
            -(self.max_width as f32) / 2.0,
            -(self.max_width as f32) / 2.0,
            -(self.max_height as f32) / 2.0,
        )
    }

    pub fn max(&self) -> Vec3 {
        Vec3::new(
            self.max_width as f32 / 2.0,
            self.max_width as f32 / 2.0,
            self.max_height as f32 / 2.0,
        )
    }

    pub fn radius(&self) -> f32 {
        self.radius
    }

    pub fn kind(&self) -> SpriteKind {
        self.kind
    }

    pub fn frames(&self) -> impl Iterator<Item = &SpriteFrame<Handle<Image>>> {
        self.frames.iter()
    }
}

#[derive(Debug, Clone, Reflect)]
struct RawSprite {
    kind: SpriteKind,
    max_width: usize,
    max_height: usize,
    radius: f32,
    frames: Vec<SpriteFrame>,
}

type Indices = Vec<u8>;

#[derive(Debug, Clone, Reflect)]
pub enum SpriteFrame<Image = Indices> {
    Static { frame: SpriteSubframe<Image> },
    Animated { subframes: Vec<SpriteSubframe<Image>>, durations: Vec<Duration> },
}

#[derive(Debug, Clone, Reflect)]
pub struct SpriteSubframe<Image = Indices> {
    width: u32,
    height: u32,
    image: Image,
}

impl SpriteSubframe {
    pub fn width(&self) -> u32 {
        self.width
    }

    pub fn height(&self) -> u32 {
        self.height
    }

    pub fn indexed(&self) -> &[u8] {
        &self.image
    }
}

async fn read_subframe<R>(
    reader: &mut AsyncReadBytes<'_, &mut R>,
) -> Result<SpriteSubframe, SprFileError>
where
    R: bevy_asset::io::Reader + ?Sized,
{
    let _origin_x = reader.read_i32::<LittleEndian>().await?;
    let _origin_z = reader.read_i32::<LittleEndian>().await?;

    let width = non_negative("frame width", reader.read_i32::<LittleEndian>().await?)?;
    let height = non_negative("frame height", reader.read_i32::<LittleEndian>().await?)?;

    let index_count =
        width.checked_mul(height).ok_or(SprFileError::SizeOverflow { width, height })? as usize;

    let mut indices = Vec::new();
    (&mut ***reader).take(index_count as u64).read_to_end(&mut indices).await?;
    if indices.len() != index_count {
        return Err(io::Error::from(io::ErrorKind::UnexpectedEof).into());
    }

    Ok(SpriteSubframe { width: width as u32, height: height as u32, image: indices })
}

async fn load<R>(mut reader: &mut R) -> Result<RawSprite, SprFileError>
where
    R: bevy_asset::io::Reader + ?Sized,
{
    let mut reader = AsyncReadBytes::new(&mut reader);

    let magic = reader.read_u32::<LittleEndian>().await?;
    if magic != MAGIC {
        return Err(SprFileError::BadMagic { magic });
    }

    let version = reader.read_u32::<LittleEndian>().await?;
    if version != VERSION {
        return Err(SprFileError::BadVersion { version });
    }

    let kind = reader.read_i32::<LittleEndian>().await?;
    let kind = SpriteKind::from_i32(kind).ok_or(SprFileError::InvalidKind { kind })?;

    let radius = reader.read_f32::<LittleEndian>().await?;

    let max_width = non_negative("max width", reader.read_i32::<LittleEndian>().await?)? as usize;
    let max_height = non_negative("max height", reader.read_i32::<LittleEndian>().await?)? as usize;

    let frame_count = match reader.read_i32::<LittleEndian>().await? {
        count if count < 1 => return Err(SprFileError::InvalidFrameCount { count }),
        count => count as usize,
    };

    let _beam_len = non_negative("beam length", reader.read_i32::<LittleEndian>().await?)?;

    debug!("max_width = {} max_height = {} frame_count = {}", max_width, max_height, frame_count);

    let sync_type = reader.read_i32::<LittleEndian>().await?;
    let _sync_type =
        SyncType::from_i32(sync_type).ok_or(SprFileError::InvalidSyncType { sync_type })?;

    let mut frames = Vec::with_capacity(frame_count.min(MAX_PREALLOC));

    for i in 0..frame_count {
        let frame_kind_int = reader.read_i32::<LittleEndian>().await?;

        // TODO: substitute out this magic number
        if frame_kind_int == 0 {
            frames.push(SpriteFrame::Static { frame: read_subframe(&mut reader).await? });
        } else {
            let subframe_count = match reader.read_i32::<LittleEndian>().await? {
                count if count < 0 => {
                    return Err(SprFileError::InvalidSubframeCount { frame: i, count });
                }
                count => count as usize,
            };

            let mut durations = Vec::with_capacity(subframe_count.min(MAX_PREALLOC));

            for _ in 0..subframe_count {
                durations.push(seismon_utils::duration_from_f32(
                    reader.read_f32::<LittleEndian>().await?,
                ));
            }

            let mut subframes = Vec::with_capacity(subframe_count.min(MAX_PREALLOC));

            for _ in 0..subframe_count {
                subframes.push(read_subframe(&mut reader).await?);
            }

            frames.push(SpriteFrame::Animated { durations, subframes });
        }
    }

    Ok(RawSprite { kind, max_width, max_height, radius, frames })
}

#[cfg(test)]
mod tests {
    use bevy_asset::io::VecReader;

    use super::*;

    fn header(frame_count: i32) -> Vec<u8> {
        let mut out = Vec::new();
        out.extend(MAGIC.to_le_bytes());
        out.extend(VERSION.to_le_bytes());
        out.extend(0i32.to_le_bytes()); // kind
        out.extend(1f32.to_le_bytes()); // radius
        out.extend(2i32.to_le_bytes()); // max width
        out.extend(2i32.to_le_bytes()); // max height
        out.extend(frame_count.to_le_bytes());
        out.extend(0i32.to_le_bytes()); // beam length
        out.extend(0i32.to_le_bytes()); // sync type
        out
    }

    fn subframe(out: &mut Vec<u8>, width: i32, height: i32) {
        out.extend(0i32.to_le_bytes()); // origin x
        out.extend(0i32.to_le_bytes()); // origin z
        out.extend(width.to_le_bytes());
        out.extend(height.to_le_bytes());
        out.extend((0..(width * height).max(0)).map(|i| i as u8));
    }

    fn single_frame() -> Vec<u8> {
        let mut out = header(1);
        out.extend(0i32.to_le_bytes()); // static frame
        subframe(&mut out, 2, 2);
        out
    }

    fn parse(bytes: Vec<u8>) -> Result<RawSprite, SprFileError> {
        futures::executor::block_on(load(&mut VecReader::new(bytes)))
    }

    #[test]
    fn parses_single_frame() {
        let sprite = parse(single_frame()).unwrap();
        assert_eq!(sprite.kind, SpriteKind::ViewPlaneParallelUpright);
        let [SpriteFrame::Static { frame }] = &sprite.frames[..] else { panic!() };
        assert_eq!((frame.width(), frame.height()), (2, 2));
        assert_eq!(frame.indexed(), [0, 1, 2, 3]);
    }

    #[test]
    fn parses_frame_group() {
        let mut out = header(1);
        out.extend(1i32.to_le_bytes()); // frame group
        out.extend(2i32.to_le_bytes()); // subframe count
        out.extend(0.1f32.to_le_bytes());
        out.extend(0.2f32.to_le_bytes());
        subframe(&mut out, 1, 1);
        subframe(&mut out, 1, 1);

        let sprite = parse(out).unwrap();
        let [SpriteFrame::Animated { subframes, durations }] = &sprite.frames[..] else { panic!() };
        assert_eq!(subframes.len(), 2);
        assert_eq!(durations.len(), 2);
    }

    #[test]
    fn truncated_is_error() {
        let mut bytes = single_frame();
        bytes.pop();
        assert!(matches!(parse(bytes), Err(SprFileError::Io { .. })));

        let bytes = single_frame()[..10].to_vec();
        assert!(matches!(parse(bytes), Err(SprFileError::Io { .. })));
    }

    #[test]
    fn bad_header_is_error() {
        let mut bytes = single_frame();
        bytes[0] = b'X';
        assert!(matches!(parse(bytes), Err(SprFileError::BadMagic { .. })));

        let mut bytes = single_frame();
        bytes[8..12].copy_from_slice(&99i32.to_le_bytes());
        assert!(matches!(parse(bytes), Err(SprFileError::InvalidKind { kind: 99 })));

        let mut bytes = header(0);
        bytes.extend(single_frame()[header(1).len()..].iter());
        assert!(matches!(parse(bytes), Err(SprFileError::InvalidFrameCount { count: 0 })));
    }

    #[test]
    fn bad_frame_size_is_error() {
        let mut out = header(1);
        out.extend(0i32.to_le_bytes());
        subframe(&mut out, -1, 2);
        assert!(matches!(parse(out), Err(SprFileError::NegativeSize { .. })));

        let mut out = header(1);
        out.extend(0i32.to_le_bytes());
        out.extend(0i32.to_le_bytes());
        out.extend(0i32.to_le_bytes());
        out.extend(0x10000i32.to_le_bytes());
        out.extend(0x10000i32.to_le_bytes());
        assert!(matches!(parse(out), Err(SprFileError::SizeOverflow { .. })));
    }
}
