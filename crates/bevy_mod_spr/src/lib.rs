use std::{convert::Infallible, time::Duration};

use bevy_app::{Plugin, PostUpdate};
use bevy_asset::{Asset, AssetApp, AssetLoader, Handle, RenderAssetUsages};
use bevy_ecs::{schedule::IntoScheduleConfigs as _, world::World};
use bevy_image::Image;
use bevy_log::debug;
use bevy_math::{Vec3, primitives::Rectangle};
use bevy_mesh::Mesh;
use bevy_mod_billboard::{BillboardMesh, BillboardTexture};
use bevy_reflect::Reflect;
use bevy_render::render_resource::{Extent3d, TextureDimension, TextureFormat};
use bevy_scene::Scene;
use bevy_tasks::ConditionalSendFuture;
use bevy_transform::components::Transform;
use futures_byteorder::{AsyncReadBytes, LittleEndian};
use num::FromPrimitive;
use num_derive::FromPrimitive;
use qbsp::Palette;
use seismon_utils::model::SyncType;
use serde::{Deserialize, Serialize};

use crate::anim::CalculatedSprSettings;

pub mod anim;

const MAGIC: u32 = u32::from_le_bytes(*b"IDSP");
const VERSION: u32 = 1;

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
    type Asset = Scene;
    type Settings = SpriteLoaderSettings;
    type Error = Infallible;

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

            let raw = load(reader).await;

            let billboard_mesh = load_context
                .add_labeled_asset("billboard".into(), Mesh::from(Rectangle::new(1., 1.)));

            // TODO: This is a lot of code, it'd be nice to deduplicate it with `bevy_mod_mdl`
            let processed_sprite_frames = raw
                .frames
                .into_iter()
                .enumerate()
                .filter_map(|(i, tex)| match tex {
                    SpriteFrame::Static { frame } => {
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
                    }
                    SpriteFrame::Animated { subframes, durations } if subframes.len() <= 1 => {
                        let frame = subframes.into_iter().next()?;

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
                    }
                    SpriteFrame::Animated { .. } => {
                        todo!()
                    }
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

            load_context.add_labeled_asset(SPRITE_HANDLE_NAME.into(), out_sprite);

            Ok(Scene { world })
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

async fn read_subframe<R>(reader: &mut AsyncReadBytes<'_, &mut R>) -> SpriteSubframe
where
    R: bevy_asset::io::Reader + ?Sized,
{
    let _origin_x = reader.read_i32::<LittleEndian>().await.unwrap();
    let _origin_z = reader.read_i32::<LittleEndian>().await.unwrap();

    let width = match reader.read_i32::<LittleEndian>().await.unwrap() {
        w if w < 0 => panic!("Negative frame width ({w})"),
        w => w,
    };

    let height = match reader.read_i32::<LittleEndian>().await.unwrap() {
        h if h < 0 => panic!("Negative frame height ({h})"),
        h => h,
    };

    let index_count = (width * height) as usize;
    let mut indices = Vec::with_capacity(index_count);
    for _ in 0..index_count {
        indices.push(reader.read_u8().await.unwrap());
    }

    SpriteSubframe { width: width as u32, height: height as u32, image: indices }
}

async fn load<R>(mut reader: &mut R) -> RawSprite
where
    R: bevy_asset::io::Reader + ?Sized,
{
    let mut reader = AsyncReadBytes::new(&mut reader);

    let magic = reader.read_u32::<LittleEndian>().await.unwrap();
    if magic != MAGIC {
        panic!("Bad magic number for sprite model (got {magic}, should be {MAGIC})");
    }

    let version = reader.read_u32::<LittleEndian>().await.unwrap();
    if version != VERSION {
        panic!("Bad version number for sprite model (got {version}, should be {VERSION})");
    }

    // TODO: use an enum for this
    let kind = SpriteKind::from_i32(reader.read_i32::<LittleEndian>().await.unwrap()).unwrap();

    let radius = reader.read_f32::<LittleEndian>().await.unwrap();

    let max_width = match reader.read_i32::<LittleEndian>().await.unwrap() {
        w if w < 0 => panic!("Negative max width ({w})"),
        w => w as usize,
    };

    let max_height = match reader.read_i32::<LittleEndian>().await.unwrap() {
        h if h < 0 => panic!("Negative max height ({h})"),
        h => h as usize,
    };

    let frame_count = match reader.read_i32::<LittleEndian>().await.unwrap() {
        c if c < 1 => panic!("Invalid frame count ({c}), must be at least 1"),
        c => c as usize,
    };

    let _beam_len = match reader.read_i32::<LittleEndian>().await.unwrap() {
        l if l < 0 => panic!("Negative beam length ({l})"),
        l => l as usize,
    };

    debug!("max_width = {} max_height = {} frame_count = {}", max_width, max_height, frame_count);

    let _sync_type = SyncType::from_i32(reader.read_i32::<LittleEndian>().await.unwrap()).unwrap();

    let mut frames = Vec::with_capacity(frame_count);

    for i in 0..frame_count {
        let frame_kind_int = reader.read_i32::<LittleEndian>().await.unwrap();

        // TODO: substitute out this magic number
        if frame_kind_int == 0 {
            frames.push(SpriteFrame::Static { frame: read_subframe(&mut reader).await });
        } else {
            let subframe_count = match reader.read_i32::<LittleEndian>().await.unwrap() {
                c if c < 0 => panic!("Negative subframe count ({c}) in frame {i}"),
                c => c as usize,
            };

            let mut durations = Vec::with_capacity(subframe_count);

            for _ in 0..subframe_count {
                durations.push(seismon_utils::duration_from_f32(
                    reader.read_f32::<LittleEndian>().await.unwrap(),
                ));
            }

            let mut subframes = Vec::with_capacity(subframe_count);

            for _ in 0..subframe_count {
                subframes.push(read_subframe(&mut reader).await);
            }

            frames.push(SpriteFrame::Animated { durations, subframes });
        }
    }

    RawSprite { kind, max_width, max_height, radius, frames }
}
