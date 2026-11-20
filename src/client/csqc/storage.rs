//! Where CSQC's fields, globals and entities are stored.
//!
//! CSQC entities only have the components here (never the server's), and belong to the CSQC VM
//! through [`QCOwnedBy`], so the server can't see them even on a listen server.
//!
//! For now, fields and globals are stored untyped, by offset: [`CsqcGlobals`] on the CSQC
//! worldspawn and [`CsqcFields`] on each entity. Vectors are stored as three floats, so that
//! vectors and their `_x` etc. components alias like they do in Quake. Typed accessors for the
//! fields and globals that the engine needs can be added as builtins are implemented.

use std::sync::Arc;

use bevy::prelude::*;
use bevy_mod_scripting::bindings::InteropError;
use bevy_mod_scripting_qcvm::{
    QCEntity, QCFunction, QCOwnedBy, QCString, QCVmContext,
    qcvm::{self, Address as _, Type, strum::VariantArray as _},
};
use hashbrown::HashMap;
use qcvm_defs_csqc::GlobalAddr;

use crate::{
    common::qc::{
        QcStorage, QcStorageImpl, QcValue, is_storable, qc_error, storage::no_such_field,
    },
    server::qc::format_value,
};

/// The fields of a CSQC entity, by offset.
#[derive(Component, Debug, Clone)]
pub struct CsqcFields(pub Vec<QcValue>);

/// The globals of the CSQC VM that the engine accesses (those in its defs), by offset. This is
/// on the CSQC worldspawn.
#[derive(Component, Debug, Clone)]
pub struct CsqcGlobals(pub Vec<QcValue>);

/// The edict number of a CSQC entity. Worldspawn is 0.
#[derive(Component, Debug, Clone, Copy, PartialEq, Eq)]
pub struct CsqcEdict(pub usize);

/// The entities of the CSQC VM by edict number, on the CSQC worldspawn.
#[derive(Component, Debug, Default)]
pub struct CsqcEdicts {
    slots: Vec<Option<Entity>>,
    /// The edict numbers that removed entities had (see [`QcStorageImpl::freed_edict`]).
    freed: HashMap<Entity, usize>,
}

/// A table of named values stored by offset.
#[derive(Debug, Default)]
struct SlotTable {
    by_name: HashMap<String, (usize, Type)>,
    by_offset: HashMap<u16, String>,
    /// Every name, in offset order, without vector component aliases.
    names: Vec<String>,
    /// The zero value of each slot.
    zero: Vec<QcValue>,
}

impl SlotTable {
    fn new<'a>(defs: impl IntoIterator<Item = (&'a str, u16, Type)>, world: Entity) -> Self {
        let mut table = Self::default();
        let mut defs = defs.into_iter().collect::<Vec<_>>();
        // Vectors first, so that the `_x` etc. aliases of vectors don't overwrite them.
        defs.sort_by_key(|&(_, offset, kind)| (offset, kind != Type::Vector));

        for (name, offset, kind) in defs {
            if !is_storable(kind) || table.by_name.contains_key(name) {
                continue;
            }

            let slot = offset as usize;
            let len = if kind == Type::Vector { 3 } else { 1 };
            if table.zero.len() < slot + len {
                table.zero.resize(slot + len, QcValue::Float(0.));
            }

            table.by_offset.entry(offset).or_insert_with(|| name.to_owned());
            table.names.push(name.to_owned());
            table.by_name.insert(name.to_owned(), (slot, kind));

            if kind == Type::Vector {
                for (i, axis) in ["x", "y", "z"].iter().enumerate() {
                    table
                        .by_name
                        .entry(format!("{name}_{axis}"))
                        .or_insert((slot + i, Type::Float));
                }
            } else if let Some(zero) = QcValue::zero(kind, world) {
                table.zero[slot] = zero;
            }
        }

        table
    }

    fn kind(&self, name: &str) -> Option<Type> {
        self.by_name.get(name).map(|&(_, kind)| kind)
    }

    fn get(&self, values: &[QcValue], name: &str, world: Entity) -> Result<QcValue, InteropError> {
        let &(slot, kind) = self.by_name.get(name).ok_or_else(|| no_such_field(name))?;
        let float = |i: usize| match values.get(i) {
            Some(QcValue::Float(f)) => *f,
            _ => 0.,
        };

        Ok(match kind {
            Type::Vector => {
                QcValue::Vector(Vec3::new(float(slot), float(slot + 1), float(slot + 2)))
            }
            _ => match values.get(slot) {
                Some(value) if value.kind() == kind => value.clone(),
                _ => QcValue::zero(kind, world).ok_or_else(|| no_such_field(name))?,
            },
        })
    }

    fn set(&self, values: &mut [QcValue], name: &str, value: QcValue) -> Result<(), InteropError> {
        let &(slot, kind) = self.by_name.get(name).ok_or_else(|| no_such_field(name))?;
        if value.kind() != kind {
            return Err(qc_error(format!(
                "type mismatch for {name}: expected {kind}, found {}",
                value.kind()
            )));
        }

        let out_of_range = || qc_error(format!("{name} is out of range"));
        match value {
            QcValue::Vector(v) => {
                let slots = values.get_mut(slot..slot + 3).ok_or_else(out_of_range)?;
                for (slot, value) in slots.iter_mut().zip(v.to_array()) {
                    *slot = QcValue::Float(value);
                }
            }
            scalar => *values.get_mut(slot).ok_or_else(out_of_range)? = scalar,
        }

        Ok(())
    }
}

/// The [`QcStorageImpl`] of the CSQC VM.
#[derive(Debug)]
pub struct CsqcStorage {
    worldspawn: Entity,
    fields: SlotTable,
    globals: SlotTable,
}

/// Sets up the storage of the CSQC VM `vm` on its worldspawn, initialising the globals from the
/// `csprogs.dat`.
pub fn init_storage(world: &mut World, vm: &QCVmContext) {
    let worldspawn = vm.worldspawn;

    let fields = SlotTable::new(
        vm.field_defs().iter().filter_map(|def| {
            let kind = Type::try_from(def.type_).ok()?;
            Some((def.name.to_str().ok()?, def.offset, kind))
        }),
        worldspawn,
    );
    let globals = SlotTable::new(
        GlobalAddr::VARIANTS.iter().map(|addr| (addr.name(), addr.to_u16(), addr.type_())),
        worldspawn,
    );

    let mut global_values = globals.zero.clone();
    for addr in GlobalAddr::VARIANTS {
        // Vector components are initialised with the vector.
        if addr.vector_field_or_scalar().0 != *addr {
            continue;
        }
        if let Some(value) = initial_value(vm, addr.to_u16(), addr.type_(), worldspawn)
            && let Err(e) = globals.set(&mut global_values, addr.name(), value)
        {
            error!("Failed to initialise CSQC global {}: {e}", addr.name());
        }
    }

    let mut edicts = CsqcEdicts::default();
    edicts.slots.push(Some(worldspawn));

    let field_values = CsqcFields(fields.zero.clone());
    let storage = CsqcStorage { worldspawn, fields, globals };
    world.entity_mut(worldspawn).insert((
        CsqcGlobals(global_values),
        field_values,
        CsqcEdict(0),
        edicts,
        QcStorage(Arc::new(storage)),
    ));
}

/// Converts a value from the `csprogs.dat`'s initial values to a [`QcValue`].
fn initial_value(vm: &QCVmContext, offset: u16, kind: Type, world: Entity) -> Option<QcValue> {
    if !is_storable(kind) {
        return None;
    }

    let value = match kind {
        Type::Vector => vm.vm.initial_global_vector(offset).ok()?.into(),
        _ => vm.initial_global(offset).ok()?,
    };

    Some(match value {
        qcvm::Value::Float(f) => QcValue::Float(f),
        qcvm::Value::Vector(v) => QcValue::Vector(v.to_array().into()),
        qcvm::Value::String(s) => QcValue::String(QCString(s)),
        // Entities can only be initialised to world.
        qcvm::Value::Entity(_) => QcValue::Entity(world),
        qcvm::Value::Function(qcvm::Function::Index(index)) => {
            QcValue::Function(QCFunction(index.0))
        }
        qcvm::Value::Function(qcvm::Function::Host(_)) | qcvm::Value::Field(_) => return None,
        qcvm::Value::Void => QcValue::zero(kind, world)?,
    })
}

impl CsqcStorage {
    fn edicts<'w>(&self, world: &'w World) -> Option<&'w CsqcEdicts> {
        world.get::<CsqcEdicts>(self.worldspawn)
    }
}

impl QcStorageImpl for CsqcStorage {
    fn field_kind(&self, _world: &World, name: &str) -> Option<Type> {
        self.fields.kind(name)
    }

    fn global_kind(&self, _world: &World, name: &str) -> Option<Type> {
        self.globals.kind(name)
    }

    fn field_name(&self, _world: &World, offset: u16) -> Option<String> {
        self.fields.by_offset.get(&offset).cloned()
    }

    fn get_field(
        &self,
        world: &World,
        entity: Entity,
        name: &str,
    ) -> Result<QcValue, InteropError> {
        let fields = world
            .get::<CsqcFields>(entity)
            .ok_or_else(|| qc_error(format!("{entity} has no CSQC fields")))?;
        self.fields.get(&fields.0, name, self.worldspawn)
    }

    fn set_field(
        &self,
        world: &mut World,
        entity: Entity,
        name: &str,
        value: QcValue,
    ) -> Result<(), InteropError> {
        let mut fields = world
            .get_mut::<CsqcFields>(entity)
            .ok_or_else(|| qc_error(format!("{entity} has no CSQC fields")))?;
        self.fields.set(&mut fields.0, name, value)
    }

    fn get_global(&self, world: &World, name: &str) -> Result<QcValue, InteropError> {
        let globals = world
            .get::<CsqcGlobals>(self.worldspawn)
            .ok_or_else(|| qc_error("The CSQC worldspawn has no globals"))?;
        self.globals.get(&globals.0, name, self.worldspawn)
    }

    fn set_global(
        &self,
        world: &mut World,
        name: &str,
        value: QcValue,
    ) -> Result<(), InteropError> {
        let mut globals = world
            .get_mut::<CsqcGlobals>(self.worldspawn)
            .ok_or_else(|| qc_error("The CSQC worldspawn has no globals"))?;
        self.globals.set(&mut globals.0, name, value)
    }

    fn edict(&self, world: &World, entity: Entity) -> Option<usize> {
        world.get::<CsqcEdict>(entity).map(|edict| edict.0)
    }

    fn freed_edict(&self, world: &World, entity: Entity) -> Option<usize> {
        self.edicts(world)?.freed.get(&entity).copied()
    }

    fn next_edict(&self, world: &World, after: usize) -> Option<(usize, Entity)> {
        let edicts = self.edicts(world)?;
        edicts
            .slots
            .iter()
            .enumerate()
            .skip(after + 1)
            .find_map(|(edict, entity)| Some((edict, (*entity)?)))
    }

    fn spawn(&self, world: &mut World) -> Result<Entity, InteropError> {
        let edict = {
            let edicts = self.edicts(world).ok_or_else(|| qc_error("CSQC isn't loaded"))?;
            edicts
                .slots
                .iter()
                .enumerate()
                .skip(1)
                .find(|(_, entity)| entity.is_none())
                .map(|(edict, _)| edict)
                .unwrap_or(edicts.slots.len())
        };

        let entity = world
            .spawn((
                QCEntity,
                QCOwnedBy(self.worldspawn),
                CsqcEdict(edict),
                CsqcFields(self.fields.zero.clone()),
            ))
            .id();

        let mut edicts = world
            .get_mut::<CsqcEdicts>(self.worldspawn)
            .ok_or_else(|| qc_error("CSQC isn't loaded"))?;
        if edicts.slots.len() <= edict {
            edicts.slots.resize(edict + 1, None);
        }
        edicts.slots[edict] = Some(entity);
        edicts.freed.remove(&entity);

        Ok(entity)
    }

    fn free(&self, world: &mut World, entity: Entity) -> Result<(), InteropError> {
        if entity == self.worldspawn {
            return Err(qc_error("Can't remove world"));
        }
        let Some(CsqcEdict(edict)) = world.get::<CsqcEdict>(entity).copied() else {
            return Ok(());
        };

        world.despawn(entity);
        if let Some(mut edicts) = world.get_mut::<CsqcEdicts>(self.worldspawn) {
            if let Some(slot) = edicts.slots.get_mut(edict) {
                *slot = None;
            }
            edicts.freed.insert(entity, edict);
        }

        Ok(())
    }

    fn random(&self, _world: &mut World) -> f32 {
        // Like Quake's `PF_random`, this only has 15 bits of precision.
        (rand::random::<u16>() & 0x7fff) as f32 / 0x7fff as f32
    }

    fn format_entity(&self, world: &World, entity: Entity) -> String {
        let mut out = String::from("{");
        for name in &self.fields.names {
            let Ok(value) = self.get_field(world, entity, name) else {
                continue;
            };
            out.push_str(&format!(" {name}: {},", format_value(world, &value)));
        }
        out.push_str(" }");
        out
    }
}
