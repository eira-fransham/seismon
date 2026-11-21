//! Extension builtins that don't depend on which side (server or client) they run on: maths,
//! strings, string buffers, tokenizing, cvars, entity introspection and files.
//!
//! These are FTEQW and DarkPlaces extensions to Quake's builtins, and like in FTEQW they are
//! registered for both the server's QuakeC and CSQC (see [`ext_builtins`]). FTEQW's `pr_bgcmd.c` is
//! the reference for their behaviour.
//!
//! This VM has no integer opcodes (CRC 5927 `progs.dat`s are compiled for vanilla Quake), so
//! FTEQW's `int` values are floats: builtins that take or return `int` (`stoi`, `ftoi` etc.) use
//! the float's value.
//!
//! Per-VM state (string buffers, tokens, open files) is stored in components on the calling VM's
//! worldspawn, so VMs never see each other's state.

pub mod buffers;
pub mod cvars;
pub mod entities;
pub mod files;
pub mod functions;
pub mod math;
pub mod printf;
pub mod strings;
pub mod tokens;

use bevy::prelude::*;
use bevy_mod_scripting::bindings::{
    InteropError, NamespaceBuilder, ReflectBase, ScriptValue, ThreadWorldContainer, V,
};
use bevy_mod_scripting_qcvm::{QCEntity, QCField, QCFunction, QCString};

/// Registers the extension builtins that both SSQC and CSQC have.
pub fn ext_builtins<N: 'static>(ns: &mut NamespaceBuilder<'_, N>) {
    math::register(ns);
    strings::register(ns);
    buffers::register(ns);
    tokens::register(ns);
    cvars::register(ns);
    entities::register(ns);
    files::register(ns);
    functions::register(ns);
}

/// The names of the builtins that [`ext_builtins`] registers.
pub fn ext_builtin_names() -> Vec<String> {
    struct Names;

    let mut world = World::new();
    ext_builtins(&mut NamespaceBuilder::<Names>::new_unregistered(&mut world));
    let registry = world.resource::<bevy_mod_scripting::bindings::AppScriptFunctionRegistry>();
    let registry = registry.read();
    let mut names = registry
        .iter_type(std::any::TypeId::of::<Names>())
        .map(|(key, _)| key.name.to_string())
        .collect::<Vec<_>>();
    names.sort();
    names
}

/// A string to return to QuakeC.
pub fn qc_string(bytes: impl AsRef<[u8]>) -> V<QCString> {
    V(QCString::from_bytes(bytes.as_ref()))
}

/// An untyped (`__variant`) argument, decoded.
#[derive(Debug, Clone, PartialEq)]
pub enum AnyArg {
    Void,
    Float(f32),
    String(QCString),
    Entity(Entity),
    Function(QCFunction),
    Field(QCField),
}

impl AnyArg {
    /// Decodes an untyped argument. This needs the thread's world, so it must be called before
    /// [`with_vm_ctx`](crate::common::qc::with_vm_ctx).
    pub fn decode(value: &ScriptValue) -> Result<Self, InteropError> {
        Ok(match value {
            ScriptValue::Unit => Self::Void,
            ScriptValue::Float(f) => Self::Float(*f as f32),
            ScriptValue::Integer(i) => Self::Float(*i as f32),
            ScriptValue::Bool(b) => Self::Float(if *b { 1. } else { 0. }),
            ScriptValue::String(s) => Self::String(QCString::from(&**s)),
            ScriptValue::List(list) => match list.front() {
                Some(first) => Self::decode(first)?,
                None => Self::Void,
            },
            ScriptValue::Reference(r) => {
                if let ReflectBase::Component(entity, _) = *r.base.base_id()
                    && r.base.type_id() == std::any::TypeId::of::<QCEntity>()
                {
                    return Ok(Self::Entity(entity));
                }

                let guard = ThreadWorldContainer::try_get_context()?.world;
                let type_id = r.base.type_id();
                if type_id == std::any::TypeId::of::<QCString>() {
                    Self::String(r.downcast::<QCString>(guard)?)
                } else if type_id == std::any::TypeId::of::<QCFunction>() {
                    Self::Function(r.downcast::<QCFunction>(guard)?)
                } else if type_id == std::any::TypeId::of::<QCField>() {
                    Self::Field(r.downcast::<QCField>(guard)?)
                } else {
                    return Err(crate::common::qc::qc_error(format!(
                        "Unexpected argument {value:?}"
                    )));
                }
            }
            other => {
                return Err(crate::common::qc::qc_error(format!("Unexpected argument {other:?}")));
            }
        })
    }

    /// The argument as a number (0 for non-numbers).
    pub fn float(&self) -> f32 {
        match self {
            Self::Float(f) => *f,
            Self::Function(f) => f.0 as f32,
            Self::Field(f) => f32::from(f.0),
            _ => 0.,
        }
    }

    /// The argument as a string (empty for non-strings).
    pub fn bytes(&self) -> &[u8] {
        match self {
            Self::String(s) => s.as_bytes(),
            _ => &[],
        }
    }
}
