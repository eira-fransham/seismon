//! Snapshots of the server state for the parity harness (`tests/parity.rs`).
//!
//! The snapshot format only uses QuakeC-level concepts (field names, edict numbers, function
//! indices), so that it stays comparable across changes to how the server stores entities.

use std::fmt::Write as _;

use bevy::prelude::*;

use super::{
    LevelState,
    qc::{
        edict::EdictTable,
        fields::{PROGDEFS_FIELD_COUNT, QcTables},
        format_value,
    },
};

/// The state of every edict at one server tick.
#[derive(Debug, Clone, PartialEq)]
pub struct Snapshot {
    /// The level time.
    pub time: f32,
    /// Each allocated edict number, with its `progdefs.h` fields by name.
    pub edicts: Vec<(u16, Vec<(String, String)>)>,
}

impl Snapshot {
    /// Formats the snapshot as text, one edict per line.
    pub fn to_text(&self) -> String {
        let mut out = String::new();
        let _ = writeln!(out, "time {:?}", self.time);
        for (edict, fields) in &self.edicts {
            let _ = write!(out, "e{edict}");
            for (name, value) in fields {
                let _ = write!(out, " {name}={value}");
            }
            out.push('\n');
        }
        out
    }
}

/// Takes a snapshot of the current level, if there is one.
pub fn snapshot(world: &World) -> Option<Snapshot> {
    let level = world.get_resource::<LevelState>()?;
    let tables = world.get_resource::<QcTables>()?;
    let edicts = world.get_resource::<EdictTable>()?;

    // Set `SEISMON_PARITY_ALL_FIELDS` to include the fields defined by the progs, for debugging.
    let all_fields = std::env::var_os("SEISMON_PARITY_ALL_FIELDS").is_some();
    let mut fields = tables
        .fields
        .fields()
        .iter()
        .filter(|field| all_fields || field.offset < PROGDEFS_FIELD_COUNT)
        .collect::<Vec<_>>();
    fields.sort_by(|a, b| (a.offset, &a.name).cmp(&(b.offset, &b.name)));

    let edicts = edicts
        .iter()
        .map(|(edict, entity)| {
            let values = fields
                .iter()
                .map(|field| {
                    let value = tables
                        .fields
                        .get(world, entity, &field.name)
                        .map(|value| format_value(world, &value))
                        .unwrap_or_default();
                    (field.name.clone(), value)
                })
                .collect();
            (edict, values)
        })
        .collect();

    let mut edicts: Vec<_> = edicts;
    if all_fields && let Some(rng) = world.get_resource::<super::rng::ServerRng>() {
        // The next random number, to check that the RNG state matches.
        edicts.push((u16::MAX, vec![("rng".into(), format!("{:?}", rng.clone().random_qc()))]));
        edicts.push((u16::MAX - 1, debug_globals(world, tables)));
    }

    Some(Snapshot { time: seismon_utils::duration_to_f32(level.time), edicts })
}

/// Every named global, for debugging (see `SEISMON_PARITY_ALL_FIELDS`).
fn debug_globals(world: &World, tables: &QcTables) -> Vec<(String, String)> {
    use bevy_mod_scripting_qcvm::{
        QCFunction, QCString,
        qcvm::{Function, Value, VmScalarType, VmType},
    };

    use super::qc::{LevelVm, fields::QcValue};

    let Some(vm) = world.get_resource::<LevelVm>() else {
        return vec![];
    };
    let mut out = vec![];
    for def in vm.0.global_defs() {
        let name = def.name.to_string_lossy();
        if name == "IMMEDIATE" {
            continue;
        }
        let host = tables.globals.get(world, tables.worldspawn, &name).ok();
        let value = match (host, def.type_) {
            (Some(value), _) => value,
            (None, VmType::Vector) => {
                let v = vm.0.vm.global(def.offset).ok();
                let f = |i: u16| match vm.0.vm.global(def.offset + i) {
                    Ok(Value::Float(f)) => f,
                    _ => 0.,
                };
                let _ = v;
                QcValue::Vector(Vec3::new(f(0), f(1), f(2)))
            }
            (None, VmType::Scalar(ty)) => match (vm.0.vm.global(def.offset), ty) {
                (Ok(Value::Float(f)), VmScalarType::Float) => QcValue::Float(f),
                (Ok(Value::Void), VmScalarType::Float) => QcValue::Float(0.),
                (Ok(Value::String(s)), VmScalarType::String) => QcValue::String(QCString(s)),
                (Ok(Value::Void), VmScalarType::String) => QcValue::String(QCString::null()),
                (Ok(Value::Entity(e)), VmScalarType::Entity) => QcValue::Entity(
                    e.map(|e| Entity::from_bits(e.get())).unwrap_or(tables.worldspawn),
                ),
                (Ok(Value::Void), VmScalarType::Entity) => QcValue::Entity(tables.worldspawn),
                (Ok(Value::Function(f)), VmScalarType::Function) => {
                    QcValue::Function(QCFunction(match f {
                        Function::Index(index) => index.0,
                        Function::Host(_) => -1,
                    }))
                }
                (Ok(Value::Void), VmScalarType::Function) => QcValue::Function(QCFunction(0)),
                (
                    other,
                    VmScalarType::Float
                    | VmScalarType::String
                    | VmScalarType::Entity
                    | VmScalarType::Function,
                ) => {
                    out.push((format!("{name}@{}", def.offset), format!("{other:?}")));
                    continue;
                }
                _ => continue,
            },
        };
        out.push((format!("{name}@{}", def.offset), format_value(world, &value)));
    }
    out
}
