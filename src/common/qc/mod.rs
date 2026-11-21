//! Infrastructure shared by every QuakeC VM: the server's `progs.dat` (SSQC) and the client's
//! `csprogs.dat` (CSQC).
//!
//! Each VM is a `progs.dat` attached to its own worldspawn entity, configured with a
//! [`QCVmConfig`] (its defs and builtin namespace) and a [`QcStorage`] (where its fields and
//! globals live). Entities belong to a single VM through the [`QCOwnedBy`] relationship, and VMs
//! can never access each other's entities, even when they run in the same world (as with a listen
//! server).
//!
//! Builtins that both SSQC and CSQC have and that don't depend on which side they run on are
//! registered in both VMs' namespaces by [`core_builtins::core_builtins`].
//!
//! [`QCVmConfig`]: bevy_mod_scripting_qcvm::QCVmConfig
//! [`QCOwnedBy`]: bevy_mod_scripting_qcvm::QCOwnedBy

pub mod core_builtins;
pub mod ext;
pub mod storage;
pub mod stub;
pub mod text;
mod value;

use std::{fmt, sync::Arc};

use bevy::prelude::*;
use bevy_mod_scripting::bindings::{
    AppScriptFunctionRegistry, InteropError, MagicFunctions, ReflectReference, ThreadWorldContainer,
};
use bevy_mod_scripting_qcvm::{
    QCField, QCFunction, QCScriptingPlugin, QCString, QCVmContext, current_vm, qcvm::Type,
};

pub use self::{
    storage::{QcComponentIds, QcStorage, QcStorageImpl},
    value::QcValue,
};

/// The description of cvars created by QuakeC, like FTEQW's. This is how cvars that the engine
/// created are told apart from QuakeC's.
pub const QC_CREATED_HELP: &str = "cvar created by QuakeC";

/// The VM of a worldspawn, for builtins that need the `progs.dat` itself (e.g. to look up
/// functions or field definitions). This is on every VM's worldspawn.
#[derive(Component, Clone, Debug)]
pub struct QcVm(pub QCVmContext);

/// The namespace that the server's builtins are registered in.
pub struct SsqcBuiltins;

/// The namespace that the client's CSQC builtins are registered in.
pub struct CsqcBuiltins;

/// Whether fields and globals can have type `ty`: true for floats, vectors, strings, entities
/// and functions.
pub fn is_storable(ty: Type) -> bool {
    matches!(ty, Type::Float | Type::Vector | Type::String | Type::Entity | Type::Function)
}

/// A QuakeC runtime error, e.g. from a builtin.
pub fn qc_error(msg: impl fmt::Display) -> InteropError {
    InteropError::external_boxed(msg.to_string().into())
}

/// Sets up `bevy_mod_scripting` to run QuakeC. This is added by both the server and the client,
/// so use [`add_core_plugin`] rather than adding it directly.
pub struct SeismonQcCorePlugin;

impl Plugin for SeismonQcCorePlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(bevy_mod_scripting::core::BMSScriptingInfrastructurePlugin::default())
            .add_plugins(QCScriptingPlugin::default())
            .register_type::<QCString>()
            .register_type::<QCFunction>()
            .register_type::<QCField>();

        let ids = QcComponentIds::new(app.world_mut());
        app.insert_resource(ids);

        let mut registry =
            app.world_mut().get_resource_or_init::<AppScriptFunctionRegistry>().clone();
        let mut registry = registry.write();
        registry.magic_functions = MagicFunctions { get: storage::qc_get, set: storage::qc_set };
    }
}

/// Adds [`SeismonQcCorePlugin`] if it hasn't been added yet.
pub fn add_core_plugin(app: &mut App) {
    if !app.is_plugin_added::<SeismonQcCorePlugin>() {
        app.add_plugins(SeismonQcCorePlugin);
    }
}

/// Access to the world from a builtin, through the storage of the VM that called it.
pub struct QcVmCtx<'w> {
    pub world: &'w mut World,
    /// The calling VM's worldspawn, which QuakeC calls `world`.
    pub root: Entity,
    pub storage: Arc<dyn QcStorageImpl>,
}

impl QcVmCtx<'_> {
    /// A reference to `entity` to return to QuakeC.
    pub fn entity_ref(&self, entity: Entity) -> ReflectReference {
        storage::entity_ref(self.world.resource::<QcComponentIds>(), entity)
    }

    pub fn get_global(&self, name: &str) -> Result<QcValue, InteropError> {
        self.storage.get_global(self.world, name)
    }

    pub fn set_global(&mut self, name: &str, value: QcValue) -> Result<(), InteropError> {
        self.storage.set_global(self.world, name, value)
    }

    pub fn get_field(&self, entity: Entity, name: &str) -> Result<QcValue, InteropError> {
        self.storage.get_field(self.world, entity, name)
    }

    pub fn set_field(
        &mut self,
        entity: Entity,
        name: &str,
        value: QcValue,
    ) -> Result<(), InteropError> {
        self.storage.set_field(self.world, entity, name, value)
    }

    /// The calling VM.
    pub fn vm(&self) -> Result<QCVmContext, InteropError> {
        self.world
            .get::<QcVm>(self.root)
            .map(|vm| vm.0.clone())
            .ok_or_else(|| qc_error(format!("{} isn't a QuakeC VM", self.root)))
    }

    /// The entity in the global named `name`.
    pub fn entity_global(&self, name: &str) -> Result<Entity, InteropError> {
        match self.get_global(name)? {
            QcValue::Entity(e) => Ok(e),
            other => Err(qc_error(format!("Global {name} is not an entity: {other:?}"))),
        }
    }
}

/// Runs `f` with exclusive access to the world, for the VM that is running the current builtin.
pub fn with_vm_ctx<O>(
    f: impl FnOnce(&mut QcVmCtx<'_>) -> Result<O, InteropError>,
) -> Result<O, InteropError> {
    let root = current_vm().ok_or_else(|| qc_error("No QuakeC VM is running"))?;
    let thread_ctx = ThreadWorldContainer::try_get_context()?;
    thread_ctx.world.with_world_mut_access_and_then(|world| {
        let storage = world
            .get::<QcStorage>(root)
            .ok_or_else(|| qc_error(format!("The QuakeC VM of {root} has no storage")))?
            .0
            .clone();
        f(&mut QcVmCtx { world, root, storage })
    })
}
