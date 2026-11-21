use bevy::{log::debug, math::Vec3};
use bevy_mod_mdl::MdlFileError;
use seismon_utils::model::{ModelFlags, SyncType};
use snafu::Snafu;

use crate::common::world::hull::BrushModel;

#[derive(Snafu, Debug)]
pub enum ModelError {
    #[snafu(context(false), display("MDL file error: {source}"))]
    MdlFile { source: MdlFileError },
    #[snafu(display("SPR file error"))]
    SprFile,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct AliasModel;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct SpriteModel;

#[derive(Default, Debug, Clone)]
pub struct Model<A = AliasModel, S = SpriteModel> {
    pub name: String,
    pub kind: ModelKind<A, S>,
    pub flags: ModelFlags,
}

impl<A0, S0> Model<A0, S0> {
    pub fn cast<A1, S1>(self) -> Model<A1, S1>
    where
        A0: Into<A1>,
        S0: Into<S1>,
    {
        Model { name: self.name, flags: self.flags, kind: self.kind.cast() }
    }
}

#[derive(Default, Debug, Clone)]
pub enum ModelKind<A = AliasModel, S = SpriteModel> {
    // TODO: find a more elegant way to express the null model
    #[default]
    None,
    Brush(BrushModel),
    Alias(A),
    Sprite(S),
}

impl<A0, S0> ModelKind<A0, S0> {
    pub fn cast<A1, S1>(self) -> ModelKind<A1, S1>
    where
        A0: Into<A1>,
        S0: Into<S1>,
    {
        match self {
            ModelKind::None => ModelKind::None,
            ModelKind::Brush(model) => ModelKind::Brush(model),
            ModelKind::Alias(model) => ModelKind::Alias(model.into()),
            ModelKind::Sprite(model) => ModelKind::Sprite(model.into()),
        }
    }
}

const DEFAULT_MODEL_KIND: ModelKind = ModelKind::None;

impl Default for &'_ ModelKind {
    fn default() -> Self {
        &DEFAULT_MODEL_KIND
    }
}

impl Model {
    pub fn none<Str>(path: Str) -> Model
    where
        Str: Into<String>,
    {
        Model { name: path.into(), kind: ModelKind::None, flags: ModelFlags::empty() }
    }

    /// Construct a new generic model from a brush model.
    pub fn from_brush_model<Str>(name: Str, brush_model: BrushModel) -> Model
    where
        Str: AsRef<str>,
    {
        Model {
            name: name.as_ref().into(),
            kind: ModelKind::Brush(brush_model),
            flags: ModelFlags::empty(),
        }
    }
}

impl<A> Model<A> {
    /// Return the minimum extent of this model.
    pub fn min(&self) -> Vec3 {
        debug!("Retrieving min of model {}", self.name);
        match self.kind {
            ModelKind::None => panic!("attempted to take min() of NULL model"),
            ModelKind::Brush(ref bmodel) => bmodel.min(),
            // TODO: maybe change this?
            // https://github.com/id-Software/Quake/blob/master/WinQuake/gl_model.c#L1625
            ModelKind::Sprite(_) | ModelKind::Alias(_) => Vec3::splat(-16.0),
        }
    }

    /// Return the maximum extent of this model.
    pub fn max(&self) -> Vec3 {
        debug!("Retrieving max of model {}", self.name);
        match self.kind {
            ModelKind::None => panic!("attempted to take max() of NULL model"),
            ModelKind::Brush(ref bmodel) => bmodel.max(),
            // TODO: maybe change this?
            // https://github.com/id-Software/Quake/blob/master/WinQuake/gl_model.c#L1625
            ModelKind::Sprite(_) | ModelKind::Alias(_) => Vec3::splat(16.0),
        }
    }
}

impl<A, S> Model<A, S> {
    pub fn kind(&self) -> &ModelKind<A, S> {
        &self.kind
    }

    /// Return the name of this model.
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn sync_type(&self) -> SyncType {
        match self.kind {
            ModelKind::None => panic!("Attempted to take sync_type() of NULL model"),
            ModelKind::Brush(_) => SyncType::Sync,
            // TODO: expose sync_type in Sprite and reflect it here
            ModelKind::Sprite(ref _smodel) => SyncType::Sync,
            // TODO: expose sync_type in Mdl and reflect it here
            ModelKind::Alias(ref _amodel) => SyncType::Sync,
        }
    }

    pub fn flags(&self) -> ModelFlags {
        self.flags
    }

    pub fn has_flag(&self, flag: ModelFlags) -> bool {
        self.flags.contains(flag)
    }
}
