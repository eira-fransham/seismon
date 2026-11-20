//! Values of QuakeC fields and globals.

use bevy::prelude::*;
use bevy_mod_scripting_qcvm::{QCFunction, QCString, qcvm::Type};

/// The value of a field or global.
#[derive(Debug, Clone, PartialEq)]
pub enum QcValue {
    Float(f32),
    Vector(Vec3),
    String(QCString),
    /// An entity. `world` is the worldspawn entity.
    Entity(Entity),
    Function(QCFunction),
}

impl QcValue {
    /// The zero value of type `ty`, or `None` if fields and globals can't have that type (see
    /// [`super::is_storable`]). Entities are zero when they're `world`.
    pub fn zero(ty: Type, world: Entity) -> Option<Self> {
        Some(match ty {
            Type::Float => Self::Float(0.),
            Type::Vector => Self::Vector(Vec3::ZERO),
            Type::String => Self::String(QCString::null()),
            Type::Entity => Self::Entity(world),
            Type::Function => Self::Function(QCFunction::NULL),
            Type::AnyScalar | Type::Void | Type::Field => return None,
        })
    }

    pub fn kind(&self) -> Type {
        match self {
            Self::Float(_) => Type::Float,
            Self::Vector(_) => Type::Vector,
            Self::String(_) => Type::String,
            Self::Entity(_) => Type::Entity,
            Self::Function(_) => Type::Function,
        }
    }
}
