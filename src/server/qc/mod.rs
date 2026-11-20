//! Running the server's QuakeC (`progs.dat`) with `bevy_mod_scripting_qcvm`.
//!
//! QuakeC entities are Bevy entities with the components in [`fields`], and the globals defined by
//! `progdefs.h` are in [`globals::QcGlobals`] on worldspawn. QuakeC accesses them through
//! `bevy_mod_scripting`'s magic `get`/`set` functions (see [`crate::common::qc::storage`]), which
//! use [`ServerQcStorage`] for the server's VM.

pub mod builtins;
pub mod edict;
pub mod fields;
pub mod globals;

use std::sync::Arc;

use bevy::prelude::*;
use bevy_mod_scripting::bindings::InteropError;
pub use bevy_mod_scripting_qcvm::QCScriptingPlugin;
use bevy_mod_scripting_qcvm::{
    QCFunction, QCVmConfig, QCVmContext,
    qcvm::{FunctionRef, MissingArguments, Type, VmOptions},
};
use qcvm_defs_quake1::Quake1;

pub use crate::common::qc::is_storable;
use crate::common::qc::{QcStorage, QcStorageImpl, SsqcBuiltins, add_core_plugin, storage};

use self::{
    edict::{Edict, EdictTable},
    fields::{FieldError, QcTables, QcValue},
};
use super::{ServerCtx, error::ServerError, rng::ServerRng};

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
    vm.call_with_world::<()>(world, function, ())?;

    Ok(())
}

/// Calls a QuakeC function given as a [`QCFunction`].
pub fn call_fn(world: &mut World, function: QCFunction) -> Result<(), ServerError> {
    call(world, FunctionRef::Index(function.0))
}

/// The configuration of the server's VM, for the worldspawn that `progs.dat` is attached to.
pub fn vm_config() -> QCVmConfig {
    QCVmConfig::new::<SsqcBuiltins>(Quake1).with_options(VmOptions {
        // `id1/progs.dat` relies on stale parameters: `ogre_smash12` calls `ai_charge()`
        // without its argument, so it gets whatever the last call left in `OFS_PARM0`.
        missing_arguments: MissingArguments::Stale,
    })
}

/// The storage of the server's VM: the components in [`fields`] and [`globals::QcGlobals`],
/// accessed through the [`QcTables`] and [`EdictTable`] of the current level.
pub fn server_storage() -> QcStorage {
    QcStorage(Arc::new(ServerQcStorage))
}

fn field_error(error: FieldError) -> InteropError {
    match error {
        FieldError::NoSuchField { name } => storage::no_such_field(&name),
        other => InteropError::external_boxed(other.to_string().into()),
    }
}

fn tables(world: &World) -> Result<&QcTables, InteropError> {
    world
        .get_resource::<QcTables>()
        .ok_or_else(|| InteropError::external_boxed("No level is loaded".into()))
}

fn server_error(error: ServerError) -> InteropError {
    builtins::to_interop(error)
}

/// See [`server_storage`].
pub struct ServerQcStorage;

impl QcStorageImpl for ServerQcStorage {
    fn field_kind(&self, world: &World, name: &str) -> Option<Type> {
        world.get_resource::<QcTables>()?.fields.kind(name)
    }

    fn global_kind(&self, world: &World, name: &str) -> Option<Type> {
        world.get_resource::<QcTables>()?.globals.kind(name)
    }

    fn field_name(&self, world: &World, offset: u16) -> Option<String> {
        world.get_resource::<QcTables>()?.fields.name_at(offset).map(str::to_owned)
    }

    fn get_field(
        &self,
        world: &World,
        entity: Entity,
        name: &str,
    ) -> Result<QcValue, InteropError> {
        tables(world)?.fields.get(world, entity, name).map_err(field_error)
    }

    fn set_field(
        &self,
        world: &mut World,
        entity: Entity,
        name: &str,
        value: QcValue,
    ) -> Result<(), InteropError> {
        let fields = tables(world)?.fields.clone();
        fields.set(world, entity, name, value).map_err(field_error)
    }

    fn get_global(&self, world: &World, name: &str) -> Result<QcValue, InteropError> {
        let tables = tables(world)?;
        tables.globals.get(world, tables.worldspawn, name).map_err(field_error)
    }

    fn set_global(
        &self,
        world: &mut World,
        name: &str,
        value: QcValue,
    ) -> Result<(), InteropError> {
        let tables = tables(world)?;
        let (globals, worldspawn) = (tables.globals.clone(), tables.worldspawn);
        globals.set(world, worldspawn, name, value).map_err(field_error)
    }

    fn edict(&self, world: &World, entity: Entity) -> Option<usize> {
        world.get::<Edict>(entity).map(|edict| edict.0 as usize)
    }

    fn freed_edict(&self, world: &World, entity: Entity) -> Option<usize> {
        world.get_resource::<EdictTable>()?.freed_edict(entity).map(usize::from)
    }

    fn next_edict(&self, world: &World, after: usize) -> Option<(usize, Entity)> {
        world
            .get_resource::<EdictTable>()?
            .range((std::ops::Bound::Excluded(after), std::ops::Bound::Unbounded))
            .next()
            .map(|(edict, entity)| (edict as usize, entity))
    }

    fn spawn(&self, world: &mut World) -> Result<Entity, InteropError> {
        ServerCtx::new(world).spawn_entity().map_err(server_error)
    }

    fn free(&self, world: &mut World, entity: Entity) -> Result<(), InteropError> {
        ServerCtx::new(world).free(entity).map_err(server_error)
    }

    fn random(&self, world: &mut World) -> f32 {
        world.resource_mut::<ServerRng>().random_qc()
    }

    fn format_entity(&self, world: &World, entity: Entity) -> String {
        format_entity(world, entity)
    }
}

/// Sets up `bevy_mod_scripting` to run the server's QuakeC.
pub fn build(app: &mut App) {
    add_core_plugin(app);
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
