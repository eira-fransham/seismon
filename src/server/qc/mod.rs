//! Running QuakeC with `bevy_mod_scripting_qcvm`.
//!
//! QuakeC entities are Bevy entities with the components in [`fields`], and the globals defined by
//! `progdefs.h` are in [`globals::QcGlobals`] on worldspawn. QuakeC accesses them through
//! `bevy_mod_scripting`'s magic `get`/`set` functions, which are overridden here to use the
//! [`fields::QcTables`].

pub mod builtins;
pub mod edict;
pub mod fields;
pub mod globals;

use std::{any::TypeId, collections::VecDeque};

use bevy::prelude::*;
use bevy_mod_scripting::{
    asset::Language,
    bindings::{
        AppScriptFunctionRegistry, FunctionCallContext, InteropError, MagicFunctions, ReflectBase,
        ReflectReference, ScriptValue, WorldGuard,
    },
    core::ScriptingPlugin,
};
use bevy_mod_scripting_qcvm::{
    QCEntity, QCField, QCFunction, QCString, QCVmContext, QCWorldspawn,
    qcvm::{
        FunctionRef, MissingArguments, Type, VmOptions,
        quake1::{fields::FieldAddr, globals::GlobalAddr},
    },
};

use self::fields::{FieldError, QcTables, QcValue};
use super::error::ServerError;

pub type QCScriptingPlugin = bevy_mod_scripting_qcvm::QCScriptingPlugin<GlobalAddr, FieldAddr>;

/// The VM for the current level.
#[derive(Resource, Clone, Debug)]
pub struct LevelVm(pub QCVmContext);

/// The script attachment that the current level's VM was loaded for.
#[derive(Resource, Clone, Debug)]
pub struct LevelAttachment(pub bevy_mod_scripting::script::ScriptAttachment);

/// Calls a QuakeC function.
///
/// This is used for every call from the server into QuakeC, including from inside builtins (e.g.
/// touch functions run by `walkmove`). Calling the null function does nothing, as at `HEAD` (where
/// it ran function 0, which is just `DONE`).
pub fn call(world: &mut World, function: impl Into<FunctionRef>) -> Result<(), ServerError> {
    let function = function.into();
    if function == FunctionRef::Index(0) {
        return Ok(());
    }

    let vm = world.resource::<LevelVm>().0.clone();
    vm.call_with_world::<GlobalAddr, FieldAddr, ()>(world, function, ())?;

    Ok(())
}

/// Calls a QuakeC function given as a [`QCFunction`].
pub fn call_fn(world: &mut World, function: QCFunction) -> Result<(), ServerError> {
    call(world, FunctionRef::Index(function.0))
}

pub fn qcvm_plugin() -> QCScriptingPlugin {
    QCScriptingPlugin {
        scripting_plugin: ScriptingPlugin {
            runtime_initializers: vec![],
            context_policy: bevy_mod_scripting::core::script::ContextPolicy::per_entity(),
            language: Language::External { name: "QC".into(), one_indexed: false },
            supported_extensions: vec!["dat"],
            context_initializers: vec![],
            context_pre_handling_initializers: vec![],
            emit_responses: false,
            processing_pipeline_plugin: Default::default(),
        },
        vm_options: VmOptions {
            // `id1/progs.dat` relies on stale parameters: `ogre_smash12` calls `ai_charge()`
            // without its argument, so it gets whatever the last call left in `OFS_PARM0`.
            missing_arguments: MissingArguments::Stale,
        },
    }
}

fn field_error(error: FieldError) -> InteropError {
    match error {
        FieldError::NoSuchField { name } => InteropError::InvalidIndex {
            index: Box::new(ScriptValue::String(name.into())),
            reason: Box::new("No such QuakeC field or global".to_owned()),
        },
        other => InteropError::external_boxed(other.to_string().into()),
    }
}

/// Converts a field value to the representation `bevy_mod_scripting_qcvm` expects.
fn to_script(value: QcValue, tables: &QcTables) -> ScriptValue {
    match value {
        QcValue::Float(f) => ScriptValue::Float(f as f64),
        QcValue::Vector(v) => {
            ScriptValue::List(VecDeque::from(v.to_array().map(|f| ScriptValue::Float(f as f64))))
        }
        QcValue::String(s) => ScriptValue::Reference(ReflectReference::new_allocated(s)),
        QcValue::Entity(e) => ScriptValue::Reference(
            // Safety: `qc_entity_component` is the component ID of `QCEntity`.
            unsafe {
                ReflectReference::new_component_ref_by_id(
                    e,
                    tables.qc_entity_component,
                    TypeId::of::<QCEntity>(),
                )
            },
        ),
        QcValue::Function(f) => ScriptValue::Reference(ReflectReference::new_allocated(f)),
    }
}

/// Converts a value from `bevy_mod_scripting_qcvm` to a field of type `kind`.
fn from_script(
    value: ScriptValue,
    kind: Type,
    world: &WorldGuard,
    tables: &QcTables,
) -> Result<QcValue, InteropError> {
    let as_f32 = |value: &ScriptValue| match *value {
        ScriptValue::Float(f) => Some(f as f32),
        ScriptValue::Integer(i) => Some(i as f32),
        ScriptValue::Bool(b) => Some(if b { 1. } else { 0. }),
        _ => None,
    };

    let mismatch = |value: &ScriptValue| {
        InteropError::external_boxed(format!("Expected a QuakeC {kind}, found {value:?}").into())
    };

    if let ScriptValue::Unit = value {
        return QcValue::zero(kind, tables.worldspawn).ok_or_else(|| mismatch(&value));
    }

    Ok(match kind {
        Type::Float => QcValue::Float(as_f32(&value).ok_or_else(|| mismatch(&value))?),
        Type::Vector => {
            let ScriptValue::List(list) = &value else {
                return Err(mismatch(&value));
            };
            let [Some(x), Some(y), Some(z)] = [0, 1, 2].map(|i| list.get(i).and_then(as_f32))
            else {
                return Err(mismatch(&value));
            };
            QcValue::Vector(Vec3::new(x, y, z))
        }
        Type::String => match value {
            ScriptValue::String(s) => QcValue::String(QCString::from(&*s)),
            ScriptValue::Reference(r) => QcValue::String(r.downcast::<QCString>(world.clone())?),
            other => return Err(mismatch(&other)),
        },
        Type::Entity => match &value {
            ScriptValue::Reference(r) => match *r.base.base_id() {
                ReflectBase::Component(e, _) => QcValue::Entity(e),
                _ => return Err(mismatch(&value)),
            },
            _ => return Err(mismatch(&value)),
        },
        Type::Function => match value {
            ScriptValue::Reference(r) => {
                QcValue::Function(r.downcast::<QCFunction>(world.clone())?)
            }
            other => return Err(mismatch(&other)),
        },
        _ => return Err(mismatch(&value)),
    })
}

enum Target {
    Field(Entity),
    Global(Entity),
}

/// Finds whether `reference` is to a QuakeC entity or worldspawn's globals.
fn target(reference: &ReflectReference, tables: &QcTables) -> Option<Target> {
    match *reference.base.base_id() {
        ReflectBase::Component(entity, id) if id == tables.qc_entity_component => {
            Some(Target::Field(entity))
        }
        ReflectBase::Component(entity, id) if id == tables.qc_worldspawn_component => {
            Some(Target::Global(entity))
        }
        _ => None,
    }
}

fn key_name(key: &ScriptValue) -> Result<&str, InteropError> {
    match key {
        ScriptValue::String(name) => Ok(name),
        other => Err(InteropError::InvalidIndex {
            index: Box::new(other.clone()),
            reason: Box::new("QuakeC fields and globals are accessed by name".to_owned()),
        }),
    }
}

fn qc_get(
    ctx: FunctionCallContext,
    reference: ReflectReference,
    key: ScriptValue,
) -> Result<ScriptValue, InteropError> {
    let world = ctx.world()?;
    let tables = world.with_world_access(|w| w.get_resource::<QcTables>().cloned())?;
    let Some((tables, target)) = tables
        .and_then(|tables| Some((target(&reference, &tables)?, tables)).map(|(t, ts)| (ts, t)))
    else {
        return MagicFunctions::default_get(ctx, reference, key);
    };

    let name = key_name(&key)?;
    let value = world.with_world_access(|w| match target {
        Target::Field(entity) => tables.fields.get(w, entity, name),
        Target::Global(entity) => tables.globals.get(w, entity, name),
    })?;

    Ok(to_script(value.map_err(field_error)?, &tables))
}

fn qc_set(
    ctx: FunctionCallContext,
    reference: ReflectReference,
    key: ScriptValue,
    value: ScriptValue,
) -> Result<(), InteropError> {
    let world = ctx.world()?;
    let tables = world.with_world_access(|w| w.get_resource::<QcTables>().cloned())?;
    let Some((tables, target)) = tables
        .and_then(|tables| Some((target(&reference, &tables)?, tables)).map(|(t, ts)| (ts, t)))
    else {
        return MagicFunctions::default_set(ctx, reference, key, value);
    };

    let name = key_name(&key)?;
    let kind = match target {
        Target::Field(_) => tables.fields.kind(name),
        Target::Global(_) => tables.globals.kind(name),
    }
    .ok_or_else(|| field_error(FieldError::NoSuchField { name: name.to_owned() }))?;

    // Convert the value before taking exclusive access, as this may need to read allocations.
    let value = from_script(value, kind, &world, &tables)?;

    world
        .with_world_mut_access(|w| match target {
            Target::Field(entity) => tables.fields.set(w, entity, name, value),
            Target::Global(entity) => tables.globals.set(w, entity, name, value),
        })?
        .map_err(field_error)
}

/// Sets up `bevy_mod_scripting` to run QuakeC.
pub fn build(app: &mut App) {
    app.add_plugins(bevy_mod_scripting::core::BMSScriptingInfrastructurePlugin::default())
        .add_plugins(qcvm_plugin())
        .register_type::<QCString>()
        .register_type::<QCFunction>()
        .register_type::<QCField>()
        .register_type::<QCEntity>()
        .register_type::<QCWorldspawn>();

    let mut registry = app.world_mut().get_resource_or_init::<AppScriptFunctionRegistry>().clone();
    let mut registry = registry.write();
    registry.magic_functions = MagicFunctions { get: qc_get, set: qc_set };
    drop(registry);

    builtins::register(app.world_mut());
}

/// Formats every field of an entity, as `objerror` prints it.
pub fn format_entity(world: &World, entity: Entity) -> String {
    let Some(tables) = world.get_resource::<QcTables>() else {
        return format!("{entity}");
    };

    let mut out = String::from("{");
    for field in tables.fields.fields() {
        let Ok(value) = tables.fields.get(world, entity, &field.name) else {
            continue;
        };
        out.push_str(&format!(" {}: {},", field.name, format_value(world, &value)));
    }
    out.push_str(" }");
    out
}

/// Formats a value using QuakeC-level concepts (edict numbers, function indices).
pub fn format_value(world: &World, value: &QcValue) -> String {
    match value {
        QcValue::Float(f) => format!("{f:?}"),
        QcValue::Vector(v) => format!("[{:?},{:?},{:?}]", v.x, v.y, v.z),
        QcValue::String(s) => format!("\"{}\"", s.as_bytes().escape_ascii()),
        QcValue::Entity(e) => match world.get::<edict::Edict>(*e) {
            Some(edict) => format!("e{}", edict.0),
            // Removed entities are no longer edicts, but they're shown as the edict they were.
            None => match world.get_resource::<edict::EdictTable>().and_then(|t| t.freed_edict(*e))
            {
                Some(edict) => format!("e{edict}"),
                None => format!("{e}"),
            },
        },
        QcValue::Function(f) => format!("f{}", f.0),
    }
}
