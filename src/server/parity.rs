//! Snapshots of the server state for the parity harness (`tests/parity.rs`).
//!
//! The snapshot format only uses QuakeC-level concepts (field names, edict numbers, function
//! indices), so that it stays comparable across changes to how the server stores entities.

use std::fmt::Write as _;

use bevy::prelude::*;

use super::{
    Session,
    progs::{EntityId, Type},
};

/// The fields defined in `progdefs.h` occupy the first 105 entity slots.
const PROGDEFS_FIELD_COUNT: u16 = 105;

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

/// Formats raw string bytes, escaping anything that isn't printable ASCII.
pub fn format_string(bytes: &[u8]) -> String {
    format!("\"{}\"", bytes.escape_ascii())
}

/// Takes a snapshot of the current level, if there is one.
pub fn snapshot(world: &World) -> Option<Snapshot> {
    let session = world.get_resource::<Session>()?;
    let level = &session.level;
    let strs = &level.string_table;

    // Field definitions from `progs.dat`, minus the `_x`/`_y`/`_z` aliases of vector fields.
    let defs = level.world.type_def().field_defs();
    let mut fields = defs
        .iter()
        .filter(|def| def.offset < PROGDEFS_FIELD_COUNT)
        .filter(|def| {
            matches!(
                def.type_,
                Type::QFloat | Type::QVector | Type::QString | Type::QEntity | Type::QFunction
            )
        })
        .filter_map(|def| {
            let name = strs.get(def.name_id)?.to_str().into_owned();
            let is_alias = def.type_ == Type::QFloat
                && ["_x", "_y", "_z"].iter().any(|suffix| {
                    name.strip_suffix(suffix).is_some_and(|base| {
                        defs.iter().any(|other| {
                            other.type_ == Type::QVector
                                && strs.get(other.name_id).is_some_and(|n| *n == *base.as_bytes())
                        })
                    })
                });
            (!is_alias).then_some((def.offset, name, def.type_))
        })
        .collect::<Vec<_>>();
    fields.sort_by(|a, b| (a.0, &a.1).cmp(&(b.0, &b.1)));

    let edicts = level
        .world
        .iter()
        .map(|id| {
            let mut ent = level.world.get(id).expect("listed entity should exist");
            let values = fields
                .iter()
                .map(|(offset, name, type_)| {
                    let offset = *offset as i16;
                    let value = match type_ {
                        Type::QFloat => format!("{:?}", ent.get_float(offset).unwrap_or_default()),
                        Type::QVector => {
                            let v = ent.get_vector(offset).unwrap_or_default();
                            format!("[{:?},{:?},{:?}]", v.x, v.y, v.z)
                        }
                        Type::QString => {
                            let id = ent.string_id(offset).unwrap_or_default();
                            format_string(
                                &strs.get(id).map(|s| s.raw.into_owned()).unwrap_or_default(),
                            )
                        }
                        Type::QEntity => {
                            let EntityId(e) = ent.entity_id(offset).unwrap_or_default();
                            // Quake has no "no entity" value: anything that isn't an edict is world.
                            format!("e{}", e.max(0))
                        }
                        Type::QFunction => {
                            format!("f{}", ent.function_id(offset).map(|f| f.0).unwrap_or_default())
                        }
                        _ => unreachable!(),
                    };
                    (name.clone(), value)
                })
                .collect();
            (id.0 as u16, values)
        })
        .collect();

    Some(Snapshot { time: seismon_utils::duration_to_f32(level.time), edicts })
}
