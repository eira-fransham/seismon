//! Where each QuakeC VM stores its fields and globals.
//!
//! Several VMs can run in the same world (the server's `progs.dat` and the client's
//! `csprogs.dat`), so fields and globals are never looked up by the type of a reference. Instead,
//! the magic `get`/`set` functions find the VM that the referenced entity belongs to (see
//! [`vm_root`]) and use the [`QcStorage`] on that VM's worldspawn.

use std::{any::Any, collections::VecDeque, fmt, sync::Arc};

use bevy::{ecs::component::ComponentId, prelude::*};
use bevy_mod_scripting::bindings::{
    FunctionCallContext, InteropError, MagicFunctions, ReflectBase, ReflectReference, ScriptValue,
    WorldGuard,
};
use bevy_mod_scripting_qcvm::{QCEntity, QCFunction, QCString, QCWorldspawn, qcvm::Type, vm_root};

use super::{QcValue, qc_error};

/// The fields, globals and entities of a QuakeC VM.
///
/// Every method is given the world, and implementations find the VM's state from it (e.g. from
/// resources for the server, or components of the VM's worldspawn). Methods that take an entity
/// are only called with entities that belong to the VM.
pub trait QcStorageImpl: Any + Send + Sync + 'static {
    /// The type of the entity field named `name`, if there is one.
    fn field_kind(&self, world: &World, name: &str) -> Option<Type>;

    /// The type of the global named `name`, if there is one.
    fn global_kind(&self, world: &World, name: &str) -> Option<Type>;

    /// The name of the entity field at `offset` (as passed to builtins such as `find`).
    fn field_name(&self, world: &World, offset: u16) -> Option<String>;

    /// Gets the entity field named `name` of `entity`.
    fn get_field(&self, world: &World, entity: Entity, name: &str)
    -> Result<QcValue, InteropError>;

    /// Sets the entity field named `name` of `entity`.
    fn set_field(
        &self,
        world: &mut World,
        entity: Entity,
        name: &str,
        value: QcValue,
    ) -> Result<(), InteropError>;

    /// Gets the global named `name`.
    fn get_global(&self, world: &World, name: &str) -> Result<QcValue, InteropError>;

    /// Sets the global named `name`.
    fn set_global(&self, world: &mut World, name: &str, value: QcValue)
    -> Result<(), InteropError>;

    /// The edict number of `entity`, if it is allocated.
    fn edict(&self, world: &World, entity: Entity) -> Option<usize>;

    /// The edict number that `entity` had before it was removed. QuakeC often keeps using
    /// entities it has just removed (e.g. with `nextent`).
    fn freed_edict(&self, world: &World, entity: Entity) -> Option<usize>;

    /// The first allocated entity with an edict number greater than `after`.
    fn next_edict(&self, world: &World, after: usize) -> Option<(usize, Entity)>;

    /// Allocates a new entity, with every field zero.
    fn spawn(&self, world: &mut World) -> Result<Entity, InteropError>;

    /// Removes `entity`.
    fn free(&self, world: &mut World, entity: Entity) -> Result<(), InteropError>;

    /// A random number in `[0, 1]`, as returned by `random`.
    fn random(&self, world: &mut World) -> f32;

    /// Formats every field of `entity`, as `objerror` prints it.
    fn format_entity(&self, world: &World, entity: Entity) -> String;
}

/// The storage of a QuakeC VM, on its worldspawn.
#[derive(Component, Clone)]
pub struct QcStorage(pub Arc<dyn QcStorageImpl>);

impl fmt::Debug for QcStorage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("QcStorage").finish_non_exhaustive()
    }
}

/// The storage of the VM that `entity` belongs to, along with that VM's worldspawn.
pub fn storage_of(world: &World, entity: Entity) -> Option<(Entity, Arc<dyn QcStorageImpl>)> {
    let root = vm_root(world, entity)?;
    let storage = world.get::<QcStorage>(root)?;
    Some((root, storage.0.clone()))
}

/// The component IDs of [`QCEntity`] and [`QCWorldspawn`], which references to entities and
/// globals are to.
#[derive(Resource, Clone, Copy, Debug)]
pub struct QcComponentIds {
    pub entity: ComponentId,
    pub worldspawn: ComponentId,
}

impl QcComponentIds {
    pub fn new(world: &mut World) -> Self {
        Self {
            entity: world.register_component::<QCEntity>(),
            worldspawn: world.register_component::<QCWorldspawn>(),
        }
    }
}

/// A reference to `entity` to pass to QuakeC.
pub fn entity_ref(ids: &QcComponentIds, entity: Entity) -> ReflectReference {
    // Safety: `ids.entity` is the component ID of `QCEntity`.
    unsafe {
        ReflectReference::new_component_ref_by_id(
            entity,
            ids.entity,
            std::any::TypeId::of::<QCEntity>(),
        )
    }
}

/// The entity that an entity argument from QuakeC refers to.
pub fn ref_entity(reference: &ReflectReference) -> Result<Entity, InteropError> {
    match *reference.base.base_id() {
        ReflectBase::Component(entity, _)
            if reference.base.type_id() == std::any::TypeId::of::<QCEntity>() =>
        {
            Ok(entity)
        }
        _ => Err(qc_error(format!("Expected an entity, found {reference:?}"))),
    }
}

/// Converts a field value to the representation `bevy_mod_scripting_qcvm` expects.
pub fn to_script(value: QcValue, ids: &QcComponentIds) -> ScriptValue {
    match value {
        QcValue::Float(f) => ScriptValue::Float(f as f64),
        QcValue::Vector(v) => {
            ScriptValue::List(VecDeque::from(v.to_array().map(|f| ScriptValue::Float(f as f64))))
        }
        QcValue::String(s) => ScriptValue::Reference(ReflectReference::new_allocated(s)),
        QcValue::Entity(e) => ScriptValue::Reference(entity_ref(ids, e)),
        QcValue::Function(f) => ScriptValue::Reference(ReflectReference::new_allocated(f)),
    }
}

/// Converts a value from `bevy_mod_scripting_qcvm` to a field of type `kind`, where `world` is the
/// VM's worldspawn (QuakeC's `world`, which entities are when they're zero).
pub fn from_script(
    value: ScriptValue,
    kind: Type,
    guard: &WorldGuard,
    world: Entity,
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
        return QcValue::zero(kind, world).ok_or_else(|| mismatch(&value));
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
            ScriptValue::Reference(r) => QcValue::String(r.downcast::<QCString>(guard.clone())?),
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
                QcValue::Function(r.downcast::<QCFunction>(guard.clone())?)
            }
            other => return Err(mismatch(&other)),
        },
        _ => return Err(mismatch(&value)),
    })
}

/// The error for accessing a field or global that doesn't exist. `bevy_mod_scripting_qcvm` treats
/// this as the address being out of range.
pub fn no_such_field(name: &str) -> InteropError {
    InteropError::InvalidIndex {
        index: Box::new(ScriptValue::String(name.to_owned().into())),
        reason: Box::new("No such QuakeC field or global".to_owned()),
    }
}

enum Target {
    Field(Entity),
    Global,
}

struct Resolved {
    target: Target,
    root: Entity,
    storage: Arc<dyn QcStorageImpl>,
    ids: QcComponentIds,
}

/// Finds whether `reference` is to a QuakeC entity or worldspawn's globals, and the storage of
/// the VM that it belongs to.
fn resolve(world: &World, reference: &ReflectReference) -> Option<Resolved> {
    let ids = *world.get_resource::<QcComponentIds>()?;
    let ReflectBase::Component(entity, id) = *reference.base.base_id() else {
        return None;
    };
    let target = if id == ids.entity {
        Target::Field(entity)
    } else if id == ids.worldspawn {
        Target::Global
    } else {
        return None;
    };
    let (root, storage) = storage_of(world, entity)?;
    Some(Resolved { target, root, storage, ids })
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

pub(super) fn qc_get(
    ctx: FunctionCallContext,
    reference: ReflectReference,
    key: ScriptValue,
) -> Result<ScriptValue, InteropError> {
    let world = ctx.world()?;
    let Some(resolved) = world.with_world_access(|w| resolve(w, &reference))? else {
        return MagicFunctions::default_get(ctx, reference, key);
    };

    let name = key_name(&key)?;
    let value = world.with_world_access(|w| match resolved.target {
        Target::Field(entity) => resolved.storage.get_field(w, entity, name),
        Target::Global => resolved.storage.get_global(w, name),
    })??;

    Ok(to_script(value, &resolved.ids))
}

pub(super) fn qc_set(
    ctx: FunctionCallContext,
    reference: ReflectReference,
    key: ScriptValue,
    value: ScriptValue,
) -> Result<(), InteropError> {
    let world = ctx.world()?;
    let Some(resolved) = world.with_world_access(|w| resolve(w, &reference))? else {
        return MagicFunctions::default_set(ctx, reference, key, value);
    };

    let name = key_name(&key)?;
    let kind = world
        .with_world_access(|w| match resolved.target {
            Target::Field(_) => resolved.storage.field_kind(w, name),
            Target::Global => resolved.storage.global_kind(w, name),
        })?
        .ok_or_else(|| no_such_field(name))?;

    // Convert the value before taking exclusive access, as this may need to read allocations.
    let value = from_script(value, kind, &world, resolved.root)?;

    world.with_world_mut_access(|w| match resolved.target {
        Target::Field(entity) => resolved.storage.set_field(w, entity, name, value),
        Target::Global => resolved.storage.set_global(w, name, value),
    })?
}
