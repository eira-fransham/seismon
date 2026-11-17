//! QuakeC globals defined by `progdefs.h`, which are stored on worldspawn in [`QcGlobals`].
//!
//! Globals defined by the `progs.dat` itself (e.g. `newmis`) are stored in the VM.

use bevy::prelude::*;
use bevy_mod_scripting_qcvm::qcvm::strum::VariantArray as _;
use bevy_mod_scripting_qcvm::{
    QCFunction, QCString, QCVmContext,
    qcvm::{self, Address as _, quake1::globals::GlobalAddr},
};
use hashbrown::HashMap;

use super::fields::{Accessor, FieldError, FieldKind, QcValue, qc_components, type_mismatch};
use crate::server::limit_vec3;

qc_components! {
    progdefs_global_accessors;

    /// The globals defined by `progdefs.h`.
    QcGlobals {
        self_: E = "self",
        other: E = "other",
        world: E = "world",
        time: F = "time",
        frametime: F = "frametime",
        force_retouch: F = "force_retouch",
        mapname: S = "mapname",
        deathmatch: F = "deathmatch",
        coop: F = "coop",
        teamplay: F = "teamplay",
        serverflags: F = "serverflags",
        total_secrets: F = "total_secrets",
        total_monsters: F = "total_monsters",
        found_secrets: F = "found_secrets",
        killed_monsters: F = "killed_monsters",
        parm1: F = "parm1",
        parm2: F = "parm2",
        parm3: F = "parm3",
        parm4: F = "parm4",
        parm5: F = "parm5",
        parm6: F = "parm6",
        parm7: F = "parm7",
        parm8: F = "parm8",
        parm9: F = "parm9",
        parm10: F = "parm10",
        parm11: F = "parm11",
        parm12: F = "parm12",
        parm13: F = "parm13",
        parm14: F = "parm14",
        parm15: F = "parm15",
        parm16: F = "parm16",
        v_forward: V = "v_forward",
        v_up: V = "v_up",
        v_right: V = "v_right",
        trace_allsolid: F = "trace_allsolid",
        trace_startsolid: F = "trace_startsolid",
        trace_fraction: F = "trace_fraction",
        trace_endpos: V = "trace_endpos",
        trace_plane_normal: V = "trace_plane_normal",
        trace_plane_dist: F = "trace_plane_dist",
        trace_ent: E = "trace_ent",
        trace_inopen: F = "trace_inopen",
        trace_inwater: F = "trace_inwater",
        msg_entity: E = "msg_entity",
        main: Fn = "main",
        start_frame: Fn = "StartFrame",
        player_pre_think: Fn = "PlayerPreThink",
        player_post_think: Fn = "PlayerPostThink",
        client_kill: Fn = "ClientKill",
        client_connect: Fn = "ClientConnect",
        put_client_in_server: Fn = "PutClientInServer",
        client_disconnect: Fn = "ClientDisconnect",
        set_new_parms: Fn = "SetNewParms",
        set_change_parms: Fn = "SetChangeParms",
    }
}

impl QcGlobals {
    /// The spawn parameters, `parm1` to `parm16`.
    pub fn parms(&self) -> [f32; 16] {
        [
            self.parm1,
            self.parm2,
            self.parm3,
            self.parm4,
            self.parm5,
            self.parm6,
            self.parm7,
            self.parm8,
            self.parm9,
            self.parm10,
            self.parm11,
            self.parm12,
            self.parm13,
            self.parm14,
            self.parm15,
            self.parm16,
        ]
    }

    pub fn set_parms(&mut self, parms: [f32; 16]) {
        [
            self.parm1,
            self.parm2,
            self.parm3,
            self.parm4,
            self.parm5,
            self.parm6,
            self.parm7,
            self.parm8,
            self.parm9,
            self.parm10,
            self.parm11,
            self.parm12,
            self.parm13,
            self.parm14,
            self.parm15,
            self.parm16,
        ] = parms;
    }
}

/// Accessors for the globals in [`QcGlobals`], by name.
pub struct QcGlobalTable {
    by_name: HashMap<&'static str, Accessor>,
}

impl Default for QcGlobalTable {
    fn default() -> Self {
        Self { by_name: progdefs_global_accessors().into_iter().collect() }
    }
}

impl QcGlobalTable {
    fn accessor(&self, name: &str) -> Result<Accessor, FieldError> {
        self.by_name
            .get(name)
            .copied()
            .ok_or_else(|| FieldError::NoSuchField { name: name.to_owned() })
    }

    pub fn kind(&self, name: &str) -> Option<FieldKind> {
        self.by_name.get(name).map(|accessor| accessor.kind)
    }

    pub fn get(
        &self,
        world: &World,
        worldspawn: Entity,
        name: &str,
    ) -> Result<QcValue, FieldError> {
        (self.accessor(name)?.get)(world, worldspawn)
            .ok_or(FieldError::NoEntity { entity: worldspawn })
    }

    pub fn set(
        &self,
        world: &mut World,
        worldspawn: Entity,
        name: &str,
        value: QcValue,
    ) -> Result<(), FieldError> {
        (self.accessor(name)?.set)(world, worldspawn, value)
    }
}

/// Converts a value from the VM's `progs.dat` initial values to a [`QcValue`].
fn initial_value(vm: &QCVmContext, addr: GlobalAddr, world: Entity) -> Option<QcValue> {
    let kind = match addr.type_() {
        qcvm::Type::Float => FieldKind::Float,
        qcvm::Type::Vector => FieldKind::Vector,
        qcvm::Type::String => FieldKind::String,
        qcvm::Type::Entity => FieldKind::Entity,
        qcvm::Type::Function => FieldKind::Function,
        _ => return None,
    };

    let value = match kind {
        FieldKind::Vector => vm.vm.initial_global_vector(addr.to_u16()).ok()?.into(),
        _ => vm.initial_global(addr.to_u16()).ok()?,
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
        // The progs can only initialise globals to their own functions.
        qcvm::Value::Function(qcvm::Function::Host(_)) => return None,
        qcvm::Value::Void => kind.zero(world),
        // `progdefs.h` has no field-typed globals.
        qcvm::Value::Field(_) => return None,
    })
}

/// Initialises the globals on `worldspawn` from the `progs.dat`'s initial values (e.g. so that
/// `StartFrame` refers to the right function).
pub fn init_globals(
    world: &mut World,
    vm: &QCVmContext,
    worldspawn: Entity,
    table: &QcGlobalTable,
) {
    for addr in GlobalAddr::VARIANTS {
        // Vector components are initialised with the vector.
        if addr.vector_field_or_scalar().0 != *addr {
            continue;
        }

        let Some(value) = initial_value(vm, *addr, worldspawn) else {
            continue;
        };

        if let Err(e) = table.set(world, worldspawn, addr.name(), value) {
            error!("Failed to initialise global {}: {e}", addr.name());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_progdefs_global_has_an_accessor() {
        let table = QcGlobalTable::default();
        for addr in GlobalAddr::VARIANTS {
            assert!(table.kind(addr.name()).is_some(), "No accessor for {}", addr.name());
        }
        assert_eq!(table.by_name.len(), GlobalAddr::VARIANTS.len());
    }
}
