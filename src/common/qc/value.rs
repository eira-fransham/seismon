//! Values of QuakeC fields and globals.

use bevy::prelude::*;
use bevy_mod_scripting_qcvm::{
    QCFunction, QCString, QCVmContext,
    qcvm::{self, Type},
};

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

impl QcValue {
    /// The value to pass to the VM `vm`.
    pub fn to_vm(&self, vm: &QCVmContext) -> qcvm::Value {
        match self {
            Self::Float(f) => qcvm::Value::Float(*f),
            Self::Vector(v) => qcvm::Value::Vector(v.to_array().into()),
            Self::String(s) => qcvm::Value::String(s.0.clone()),
            Self::Entity(e) => vm.entity_value(*e),
            Self::Function(f) => {
                qcvm::Value::Function(qcvm::Function::Index(qcvm::FunctionIndex(f.0)))
            }
        }
    }

    /// Converts a value from the VM `vm`, or `None` for values that fields and globals can't
    /// hold (`void` is the zero of `kind`).
    pub fn from_vm(value: qcvm::Value, kind: Type, vm: &QCVmContext) -> Option<Self> {
        Some(match value {
            qcvm::Value::Float(f) => Self::Float(f),
            qcvm::Value::Vector(v) => Self::Vector(v.to_array().into()),
            qcvm::Value::String(s) => Self::String(QCString(s)),
            qcvm::Value::Entity(e) => Self::Entity(vm.value_entity(e)),
            qcvm::Value::Function(qcvm::Function::Index(index)) => {
                Self::Function(QCFunction(index.0))
            }
            qcvm::Value::Function(qcvm::Function::Host(_)) | qcvm::Value::Field(_) => {
                return None;
            }
            qcvm::Value::Void => Self::zero(kind, vm.worldspawn)?,
        })
    }
}
