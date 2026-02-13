use bevy_asset::{Asset, AssetLoader, Handle};
use bevy_ecs::{component::Component, system::EntityCommand, world::EntityWorldMut};
use bevy_materialize::prelude::GenericMaterial;
use bevy_math::Vec3;
use bevy_reflect::Reflect;
use bevy_tasks::ConditionalSendFuture;
use serde::{Deserialize, Serialize};
use std::{collections::HashSet, io};
use thiserror::Error;

pub use qbsp::Palette;

use crate::anim::AssimpAnimMeshes;

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
    animations: Vec<Handle<AssimpAnimMeshes>>,
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

impl AssetLoader for MdlLoader {
    type Asset = Mdl;
    type Settings = MdlLoaderSettings;
    type Error = anyhow::Error;

    fn load(
        &self,
        _reader: &mut dyn bevy_asset::io::Reader,
        _settings: &Self::Settings,
        _load_context: &mut bevy_asset::LoadContext,
    ) -> impl ConditionalSendFuture<Output = anyhow::Result<Self::Asset>> {
        async { todo!() }
    }
}
