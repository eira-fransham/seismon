use bevy_asset::{Asset, AssetLoader, Handle, RenderAssetUsages};
use bevy_ecs::{component::Component, system::EntityCommand, world::EntityWorldMut};
use bevy_image::Image;
use bevy_materialize::{
    animation::{MaterialAnimations, NextAnimation},
    prelude::GenericMaterial,
};
use bevy_math::{UVec2, Vec3};
use bevy_mesh::Mesh;
use bevy_pbr::StandardMaterial;
use bevy_reflect::Reflect;
use bevy_render::render_resource::{Extent3d, TextureDimension, TextureFormat};
use bevy_tasks::ConditionalSendFuture;
use seismon_utils::model::ModelFlags;
use serde::{Deserialize, Serialize};
use std::{collections::HashSet, io};
use thiserror::Error;

pub use qbsp::Palette;

use crate::{
    anim::AnimMeshes,
    read::{Animation, Texture},
};

pub mod anim;
pub mod read;

#[derive(Error, Debug)]
pub enum MdlFileError {
    #[error("I/O error: {0}")]
    Io(#[from] io::Error),
    #[error("Invalid magic number: found {0}, expected {}", read::MAGIC)]
    InvalidMagicNumber(i32),
    #[error("Unrecognized version: {0}")]
    UnrecognizedVersion(i32),
    #[error("Invalid texture width: {0}")]
    InvalidTextureWidth(i32),
    #[error("Invalid texture height: {0}")]
    InvalidTextureHeight(i32),
    #[error("Invalid vertex count: {0}")]
    InvalidVertexCount(i32),
    #[error("Invalid polygon count: {0}")]
    InvalidPolygonCount(i32),
    #[error("Invalid keyframe count: {0}")]
    InvalidKeyframeCount(i32),
    #[error("Invalid model flags: {0:X?}")]
    InvalidFlags(i32),
    #[error("Invalid texture kind: {0}")]
    InvalidTextureKind(i32),
    #[error("Invalid seam flag: {0}")]
    InvalidSeamFlag(i32),
    #[error("Invalid texture coordinates: {0:?}")]
    InvalidTexcoord([i32; 2]),
    #[error("Invalid front-facing flag: {0}")]
    InvalidFrontFacing(i32),
    #[error("Keyframe name too long: {0:?}")]
    KeyframeNameTooLong([u8; 16]),
    #[error("Non-UTF-8 keyframe name: {0}")]
    NonUtf8KeyframeName(#[from] std::string::FromUtf8Error),
    #[error("Extra data found at end of file")]
    Misaligned,
    #[error("Unknown error")]
    Unknown,
}

// TODO: Use `DynamicComponent` so libraries can define their own flags.

#[derive(Component, Default, Reflect, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MdlFlagRocket;
#[derive(Component, Default, Reflect, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MdlFlagGrenade;
#[derive(Component, Default, Reflect, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MdlFlagGib;
#[derive(Component, Default, Reflect, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MdlFlagRotate;
#[derive(Component, Default, Reflect, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MdlFlagTracer;
#[derive(Component, Default, Reflect, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MdlFlagZomgib;
#[derive(Component, Default, Reflect, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MdlFlagTracer2;
#[derive(Component, Default, Reflect, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MdlFlagTracer3;

#[derive(Reflect, Hash, Clone, PartialEq, Eq)]
pub enum MdlFlag {
    Rocket(MdlFlagRocket),
    Grenade(MdlFlagGrenade),
    Gib(MdlFlagGib),
    Rotate(MdlFlagRotate),
    Tracer(MdlFlagTracer),
    Zomgib(MdlFlagZomgib),
    Tracer2(MdlFlagTracer2),
    Tracer3(MdlFlagTracer3),
}

impl EntityCommand for MdlFlag {
    fn apply(self, mut entity: EntityWorldMut) {
        match self {
            MdlFlag::Rocket(component) => entity.insert(component.clone()),
            MdlFlag::Grenade(component) => entity.insert(component.clone()),
            MdlFlag::Gib(component) => entity.insert(component.clone()),
            MdlFlag::Rotate(component) => entity.insert(component.clone()),
            MdlFlag::Tracer(component) => entity.insert(component.clone()),
            MdlFlag::Zomgib(component) => entity.insert(component.clone()),
            MdlFlag::Tracer2(component) => entity.insert(component.clone()),
            MdlFlag::Tracer3(component) => entity.insert(component.clone()),
        };
    }
}

#[derive(Asset, Reflect)]
pub struct Mdl {
    origin: Vec3,
    radius: f32,
    textures: Vec<Handle<GenericMaterial>>,
    animations: Vec<Handle<AnimMeshes>>,
    flags: HashSet<MdlFlag>,
}

#[derive(Reflect, Default)]
pub struct MdlLoader {
    pub default_palette: Palette,
}

#[derive(Deserialize, Serialize, Default)]
pub struct MdlLoaderSettings {
    pub override_palette: Option<Palette>,
}

async fn load_mdl(
    loader: &MdlLoader,
    reader: &mut dyn bevy_asset::io::Reader,
    settings: &MdlLoaderSettings,
    load_context: &mut bevy_asset::LoadContext<'_>,
) -> Result<Mdl, MdlFileError> {
    fn translate_tex(tex: &[u8], width: u32, height: u32, palette: &Palette) -> Image {
        let data = tex
            .iter()
            .flat_map(|i| {
                let [r, g, b] = palette.colors[*i as usize];

                [r, g, b, 0xff]
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

    let raw = crate::read::load(reader).await.into_result()?;

    let textures = raw
        .textures()
        .enumerate()
        .filter_map(|(i, tex)| match tex {
            Texture::Static(static_texture) => {
                let img = load_context.add_labeled_asset(
                    format!("img{i}"),
                    translate_tex(
                        static_texture.indices(),
                        raw.texture_width(),
                        raw.texture_height(),
                        settings.override_palette.as_ref().unwrap_or(&loader.default_palette),
                    ),
                );
                let mat = load_context
                    .add_labeled_asset(format!("stdmat{i}"), StandardMaterial::from(img));

                Some(load_context.add_labeled_asset(format!("tex{i}"), GenericMaterial::new(mat)))
            }
            Texture::Animated(animated_texture) if animated_texture.frames().is_empty() => None,
            Texture::Animated(animated_texture) => {
                let first_frame_name = format!("tex{i}frame0");
                let first_frame_handle =
                    load_context.get_label_handle::<GenericMaterial>(&first_frame_name);

                let frames = animated_texture
                    .frames()
                    .iter()
                    .enumerate()
                    .map(|(frame_idx, tex)| {
                        let img = load_context.add_labeled_asset(
                            format!("img{i}frame{frame_idx}"),
                            translate_tex(
                                tex.indices(),
                                raw.texture_width(),
                                raw.texture_height(),
                                settings
                                    .override_palette
                                    .as_ref()
                                    .unwrap_or(&loader.default_palette),
                            ),
                        );

                        (
                            frame_idx,
                            tex,
                            load_context.add_labeled_asset(
                                format!("stdmat{i}frame{frame_idx}"),
                                StandardMaterial::from(img),
                            ),
                        )
                    })
                    .collect::<Vec<_>>();

                let mut frames = frames.into_iter();

                let mut last_frame = frames
                    .next_back()
                    .map(|(frame_idx, frame, mat)| {
                        let mut mat = GenericMaterial::new(mat);
                        mat.set_property(
                            GenericMaterial::ANIMATION,
                            MaterialAnimations {
                                next: Some(NextAnimation {
                                    seconds: frame.duration().as_secs_f32(),
                                    material: first_frame_handle,
                                    state: Default::default(),
                                }),
                                images: None,
                            },
                        );

                        (frame_idx, mat)
                    })
                    .unwrap();

                for (frame_idx, frame, mat) in frames.rev() {
                    let (next_frame_idx, next_frame) = last_frame;

                    let next_frame_handle = load_context
                        .add_labeled_asset(format!("tex{i}frame{next_frame_idx}"), next_frame);

                    let mut mat = GenericMaterial::new(mat);
                    mat.set_property(
                        GenericMaterial::ANIMATION,
                        MaterialAnimations {
                            next: Some(NextAnimation {
                                seconds: frame.duration().as_secs_f32(),
                                material: next_frame_handle,
                                state: Default::default(),
                            }),
                            images: None,
                        },
                    );

                    last_frame = (frame_idx, mat);
                }

                let (_, first_frame) = last_frame;

                Some(load_context.add_labeled_asset(first_frame_name, first_frame))
            }
        })
        .collect();

    let animations = raw
        .animations()
        .enumerate()
        .map(|(i, anim)| match anim {
            Animation::Static(raw_mesh) => {
                let mut mesh = Mesh::new(
                    bevy_mesh::PrimitiveTopology::TriangleList,
                    RenderAssetUsages::RENDER_WORLD,
                );

                mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, raw_mesh.vertices().to_owned());
                mesh.insert_attribute(
                    Mesh::ATTRIBUTE_UV_0,
                    raw.texcoords()
                        .map(|coord| UVec2::new(coord.s(), coord.t()))
                        .collect::<Vec<_>>(),
                );

                let mesh_handle = load_context.add_labeled_asset(format!("anim{i}mesh"), mesh);

                load_context
                    .add_labeled_asset(format!("anim{i}"), AnimMeshes { frames: vec![mesh_handle] })
            }
            Animation::Animated(animated_mesh) => {
                let frames = animated_mesh
                    .frames()
                    .iter()
                    .enumerate()
                    .map(|(frame_idx, raw_mesh)| {
                        let mut mesh = Mesh::new(
                            bevy_mesh::PrimitiveTopology::TriangleList,
                            RenderAssetUsages::RENDER_WORLD,
                        );

                        mesh.insert_attribute(
                            Mesh::ATTRIBUTE_POSITION,
                            raw_mesh.vertices().to_owned(),
                        );
                        mesh.insert_attribute(
                            Mesh::ATTRIBUTE_UV_0,
                            raw.texcoords()
                                .map(|coord| UVec2::new(coord.s(), coord.t()))
                                .collect::<Vec<_>>(),
                        );

                        load_context.add_labeled_asset(format!("anim{i}frame{frame_idx}"), mesh)
                    })
                    .collect();

                // TODO: Handle durations (this is only needed server-side)
                load_context.add_labeled_asset(format!("anim{i}"), AnimMeshes { frames })
            }
        })
        .collect();

    Ok(Mdl {
        origin: raw.origin(),
        radius: raw.radius(),
        textures,
        animations,
        flags: raw
            .flags()
            .iter()
            .map(|flag| {
                if flag == ModelFlags::ROCKET {
                    MdlFlag::Rocket(Default::default())
                } else if flag == ModelFlags::GRENADE {
                    MdlFlag::Grenade(Default::default())
                } else if flag == ModelFlags::GIB {
                    MdlFlag::Gib(Default::default())
                } else if flag == ModelFlags::ROTATE {
                    MdlFlag::Rotate(Default::default())
                } else if flag == ModelFlags::TRACER {
                    MdlFlag::Tracer(Default::default())
                } else if flag == ModelFlags::ZOMGIB {
                    MdlFlag::Zomgib(Default::default())
                } else if flag == ModelFlags::TRACER2 {
                    MdlFlag::Tracer2(Default::default())
                } else if flag == ModelFlags::TRACER3 {
                    MdlFlag::Tracer3(Default::default())
                } else {
                    unreachable!("Programmer error (consider enumflags?)")
                }
            })
            .collect(),
    })
}

impl AssetLoader for MdlLoader {
    type Asset = Mdl;
    type Settings = MdlLoaderSettings;
    type Error = MdlFileError;

    fn load(
        &self,
        reader: &mut dyn bevy_asset::io::Reader,
        settings: &Self::Settings,
        load_context: &mut bevy_asset::LoadContext,
    ) -> impl ConditionalSendFuture<Output = Result<Self::Asset, Self::Error>> {
        load_mdl(self, reader, settings, load_context)
    }
}
