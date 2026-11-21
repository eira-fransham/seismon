//! Entity builtins (`findfloat`, `findchain`, `copyentity`, field introspection etc.), as in
//! FTEQW. They only see the calling VM's entities and fields, through its storage.

use std::ffi::CString;

use bevy::prelude::*;
use bevy_mod_scripting::bindings::{InteropError, NamespaceBuilder, V};
use bevy_mod_scripting_qcvm::{
    QCField, QCFunction, QCString, builtin_argc,
    qcvm::{Type, VmScalarType, VmType},
};

use super::{AnyArg, qc_string, strings};
use crate::common::qc::{
    QcValue, QcVmCtx, qc_error,
    storage::ref_entity,
    stub::{Any, Ent, Fld, S, string},
    text::com_parse,
    with_vm_ctx,
};

/// The edict number of `entity` (0 for `world`), including removed entities.
pub fn edict_of(ctx: &QcVmCtx<'_>, entity: Entity) -> usize {
    if entity == ctx.root {
        return 0;
    }
    ctx.storage
        .edict(ctx.world, entity)
        .or_else(|| ctx.storage.freed_edict(ctx.world, entity))
        .unwrap_or(0)
}

/// The entity with edict number `edict`, if it is in use.
pub fn entity_for_edict(ctx: &QcVmCtx<'_>, edict: usize) -> Option<Entity> {
    if edict == 0 {
        return Some(ctx.root);
    }
    match ctx.storage.next_edict(ctx.world, edict - 1) {
        Some((found, entity)) if found == edict => Some(entity),
        _ => None,
    }
}

/// The name of the field that `field` refers to.
fn field_name(ctx: &QcVmCtx<'_>, field: &Fld) -> Result<String, InteropError> {
    ctx.storage
        .field_name(ctx.world, field.0.0)
        .ok_or_else(|| qc_error(format!("Invalid field reference {}", field.0.0)))
}

/// The entities after `start` (all of them for `world`), in edict order.
fn entities_after(ctx: &QcVmCtx<'_>, start: Entity) -> Vec<Entity> {
    let mut edict = edict_of(ctx, start);
    let mut out = Vec::new();
    while let Some((next, entity)) = ctx.storage.next_edict(ctx.world, edict) {
        edict = next;
        out.push(entity);
    }
    out
}

/// Whether the field `name` of `entity` is a float with any of the bits of `flags`.
fn has_flags(ctx: &QcVmCtx<'_>, entity: Entity, name: &str, flags: f32) -> bool {
    matches!(ctx.get_field(entity, name), Ok(QcValue::Float(f)) if (f as i32) & (flags as i32) != 0)
}

/// Links every entity that `matches` into a chain through the field `chain` (`.chain` by
/// default), like `findchain`. Returns the head of the chain (`world` if there are none).
fn find_chain(
    ctx: &mut QcVmCtx<'_>,
    chain: Option<&Fld>,
    mut matches: impl FnMut(&QcVmCtx<'_>, Entity) -> bool,
) -> Result<Entity, InteropError> {
    let chain = match chain {
        Some(field) if builtin_argc() > 2 => field_name(ctx, field)?,
        _ => "chain".to_owned(),
    };

    let mut head = ctx.root;
    for entity in entities_after(ctx, ctx.root) {
        if matches(ctx, entity) {
            ctx.set_field(entity, &chain, QcValue::Entity(head))?;
            head = entity;
        }
    }
    Ok(head)
}

/// FTEQW's field type numbers (`ev_*`).
fn field_type_number(kind: VmType) -> f32 {
    match kind {
        VmType::Scalar(scalar) => f32::from(scalar as u8),
        VmType::Vector => 3.,
    }
}

/// The definition of field number `index` of the calling VM (`name`, offset and type).
fn field_def(ctx: &QcVmCtx<'_>, index: f32) -> Result<Option<(String, u16, VmType)>, InteropError> {
    let vm = ctx.vm()?;
    if index < 0. {
        return Ok(None);
    }
    Ok(vm.field_defs().get(index as usize).map(|def| {
        (def.name.to_string_lossy().into_owned(), def.offset, def.type_)
    }))
}

/// A field value as text, like FTEQW's `PR_UglyValueString`: numbers as they'd be written,
/// entities as their numbers and functions by name.
pub fn value_string(ctx: &QcVmCtx<'_>, value: &QcValue) -> Result<Vec<u8>, InteropError> {
    Ok(match value {
        QcValue::Float(f) => f.to_string().into_bytes(),
        QcValue::Vector(v) => format!("{} {} {}", v.x, v.y, v.z).into_bytes(),
        QcValue::String(s) => s.as_bytes().to_vec(),
        QcValue::Entity(e) => edict_of(ctx, *e).to_string().into_bytes(),
        QcValue::Function(f) => match ctx.vm()?.vm.functions().get_by_index(f.0) {
            Ok(def) if f.0 != 0 => def.name.to_bytes().to_vec(),
            _ => Vec::new(),
        },
    })
}

/// Parses a field value of type `kind` from text (the reverse of [`value_string`]).
pub fn parse_value(ctx: &QcVmCtx<'_>, kind: Type, s: &[u8]) -> Result<Option<QcValue>, InteropError> {
    Ok(Some(match kind {
        Type::Float => QcValue::Float(strings::atof(s)),
        Type::Vector => QcValue::Vector(strings::stov(s)),
        Type::String => QcValue::String(QCString::from_bytes(s)),
        Type::Entity => {
            let edict = strings::atof(s).max(0.) as usize;
            match entity_for_edict(ctx, edict) {
                Some(entity) => QcValue::Entity(entity),
                None => return Ok(None),
            }
        }
        Type::Function => {
            let vm = ctx.vm()?;
            let Ok(name) = CString::new(s) else {
                return Ok(None);
            };
            match vm.vm.functions().get_by_name(&name) {
                Ok(def) => QcValue::Function(QCFunction(def.index)),
                Err(_) if s.is_empty() => QcValue::Function(QCFunction::NULL),
                Err(_) => return Ok(None),
            }
        }
        Type::Field | Type::AnyScalar | Type::Void => return Ok(None),
    }))
}

/// Sets the field `name` of `entity` from text, returning whether it worked.
fn put_field_string(
    ctx: &mut QcVmCtx<'_>,
    entity: Entity,
    name: &str,
    s: &[u8],
) -> Result<bool, InteropError> {
    let Some(kind) = ctx.storage.field_kind(ctx.world, name) else {
        return Ok(false);
    };
    match parse_value(ctx, kind, s)? {
        Some(value) => {
            ctx.set_field(entity, name, value)?;
            Ok(true)
        }
        None => Ok(false),
    }
}

/// Copies every field of `from` to `to`.
pub fn copy_fields(ctx: &mut QcVmCtx<'_>, from: Entity, to: Entity) -> Result<(), InteropError> {
    let vm = ctx.vm()?;
    for def in vm.field_defs() {
        let name = def.name.to_string_lossy();
        if ctx.storage.field_kind(ctx.world, &name).is_none() {
            continue;
        }
        let value = ctx.get_field(from, &name)?;
        ctx.set_field(to, &name, value)?;
    }
    Ok(())
}

/// Parses an entity's fields in the map format (`{ "key" "value" ... }`) from `s` into `entity`,
/// like `parseentitydata` (FTEQW's `ED_ParseEdict`). Returns the offset just after the closing
/// brace, or `None` if `s` doesn't start with an entity.
pub fn parse_entity_data(
    ctx: &mut QcVmCtx<'_>,
    entity: Entity,
    s: &[u8],
) -> Result<Option<usize>, InteropError> {
    let Some(open) = com_parse(s, 0, false) else {
        return Ok(None);
    };
    if open.text != b"{" {
        return Ok(None);
    }

    let mut pos = open.end;
    loop {
        let Some(key) = com_parse(s, pos, false) else {
            return Ok(None);
        };
        if key.text == b"}" {
            return Ok(Some(key.end));
        }
        let Some(value) = com_parse(s, key.end, false) else {
            return Ok(None);
        };
        pos = value.end;

        let mut key_name = String::from_utf8_lossy(&key.text).into_owned();
        let mut value_text = value.text;
        // Keys that start with `_` are for tools, not QuakeC.
        if key_name.starts_with('_') {
            continue;
        }
        // `angle` is a shorthand for the yaw.
        if key_name == "angle" {
            key_name = "angles".to_owned();
            value_text = format!("0 {} 0", strings::atof(&value_text)).into_bytes();
        }

        if !put_field_string(ctx, entity, &key_name, &value_text)? {
            warn!("parseentitydata: can't set {key_name:?}");
        }
    }
}

pub fn register<N: 'static>(ns: &mut NamespaceBuilder<'_, N>) {
    ns.register("findfloat", |start: Ent, field: Fld, matching: Any| {
        let matching = AnyArg::decode(&matching)?;
        with_vm_ctx(|ctx| {
            let name = field_name(ctx, &field)?;
            let start = ref_entity(&start)?;
            for entity in entities_after(ctx, start) {
                let found = match (ctx.get_field(entity, &name)?, &matching) {
                    (QcValue::Entity(e), AnyArg::Entity(m)) => e == *m,
                    (QcValue::Entity(e), other) => e == ctx.root && other.float() == 0.,
                    (QcValue::Float(f), other) => f == other.float(),
                    (QcValue::Function(f), other) => f.0 as f32 == other.float(),
                    _ => false,
                };
                if found {
                    return Ok(ctx.entity_ref(entity));
                }
            }
            Ok(ctx.entity_ref(ctx.root))
        })
    })
    .register("findflags", |start: Ent, field: Fld, flags: f32| {
        with_vm_ctx(|ctx| {
            let name = field_name(ctx, &field)?;
            let start = ref_entity(&start)?;
            let found = entities_after(ctx, start)
                .into_iter()
                .find(|&entity| has_flags(ctx, entity, &name, flags))
                .unwrap_or(ctx.root);
            Ok(ctx.entity_ref(found))
        })
    })
    .register("findchain", |field: Fld, matching: S, chain: Fld| {
        with_vm_ctx(|ctx| {
            let name = field_name(ctx, &field)?;
            let matching = string(&matching);
            let head = find_chain(ctx, Some(&chain), |ctx, entity| {
                // Null strings never match, like `PF_findchain`.
                matches!(ctx.get_field(entity, &name), Ok(QcValue::String(s)) if !s.is_null() && s.as_bytes() == matching)
            })?;
            Ok(ctx.entity_ref(head))
        })
    })
    .register("findchainfloat", |field: Fld, matching: f32, chain: Fld| {
        with_vm_ctx(|ctx| {
            let name = field_name(ctx, &field)?;
            let head = find_chain(ctx, Some(&chain), |ctx, entity| match ctx.get_field(entity, &name) {
                Ok(QcValue::Float(f)) => f == matching,
                Ok(QcValue::Entity(e)) => e == ctx.root && matching == 0.,
                _ => false,
            })?;
            Ok(ctx.entity_ref(head))
        })
    })
    .register("findchainflags", |field: Fld, flags: f32, chain: Fld| {
        with_vm_ctx(|ctx| {
            let name = field_name(ctx, &field)?;
            let head =
                find_chain(ctx, Some(&chain), |ctx, entity| has_flags(ctx, entity, &name, flags))?;
            Ok(ctx.entity_ref(head))
        })
    })
    .register("edict_num", |number: f32| {
        with_vm_ctx(|ctx| {
            let entity = entity_for_edict(ctx, number.max(0.) as usize).unwrap_or(ctx.root);
            Ok(ctx.entity_ref(entity))
        })
    })
    .register("num_for_edict", |e: Ent| {
        with_vm_ctx(|ctx| Ok(edict_of(ctx, ref_entity(&e)?) as f32))
    })
    .register("wasfreed", |e: Ent| {
        with_vm_ctx(|ctx| {
            let entity = ref_entity(&e)?;
            let freed = entity != ctx.root && ctx.storage.edict(ctx.world, entity).is_none();
            Ok(if freed { 1. } else { 0. })
        })
    })
    .register("copyentity", |from: Ent, to: Ent| {
        with_vm_ctx(|ctx| {
            let from = ref_entity(&from)?;
            let to = if builtin_argc() > 1 { ref_entity(&to)? } else { ctx.storage.spawn(ctx.world)? };
            for (entity, which) in [(from, "source"), (to, "destination")] {
                if entity != ctx.root && ctx.storage.edict(ctx.world, entity).is_none() {
                    return Err(qc_error(format!("copyentity: {which} is free")));
                }
            }
            if to == ctx.root {
                return Err(qc_error("copyentity: can't copy over world"));
            }
            copy_fields(ctx, from, to)?;
            Ok(ctx.entity_ref(to))
        })
    })
    .register("numentityfields", || {
        with_vm_ctx(|ctx| Ok(ctx.vm()?.field_defs().len() as f32))
    })
    .register("entityfieldname", |index: f32| {
        with_vm_ctx(|ctx| {
            Ok(match field_def(ctx, index)? {
                Some((name, ..)) => qc_string(name),
                None => V(QCString::null()),
            })
        })
    })
    .register("entityfieldtype", |index: f32| {
        with_vm_ctx(|ctx| Ok(field_def(ctx, index)?.map_or(0., |(_, _, kind)| field_type_number(kind))))
    })
    .register("entityfieldref", |index: f32| {
        with_vm_ctx(|ctx| Ok(V(QCField(field_def(ctx, index)?.map_or(0, |(_, offset, _)| offset)))))
    })
    .register("findentityfield", |name: S| {
        with_vm_ctx(|ctx| {
            let name = string(&name);
            let vm = ctx.vm()?;
            Ok(vm
                .field_defs()
                .iter()
                .position(|def| def.name.to_bytes() == name)
                .map_or(0., |index| index as f32))
        })
    })
    .register("getentityfieldstring", |index: f32, e: Ent| {
        with_vm_ctx(|ctx| {
            let entity = ref_entity(&e)?;
            let Some((name, _, kind)) = field_def(ctx, index)? else {
                return Ok(V(QCString::null()));
            };
            if ctx.storage.field_kind(ctx.world, &name).is_none() {
                return Ok(V(QCString::null()));
            }
            let value = ctx.get_field(entity, &name)?;
            // Like FTEQW, zero vectors are empty.
            if kind == VmType::Vector && value == QcValue::Vector(Vec3::ZERO) {
                return Ok(V(QCString::null()));
            }
            Ok(qc_string(value_string(ctx, &value)?))
        })
    })
    .register("putentityfieldstring", |index: f32, e: Ent, s: S| {
        with_vm_ctx(|ctx| {
            let entity = ref_entity(&e)?;
            let Some((name, ..)) = field_def(ctx, index)? else {
                return Ok(0.);
            };
            Ok(if put_field_string(ctx, entity, &name, string(&s))? { 1. } else { 0. })
        })
    })
    .register("parseentitydata", |e: Ent, s: S, offset: f32| {
        with_vm_ctx(|ctx| {
            let entity = ref_entity(&e)?;
            let s = string(&s);
            let offset = if builtin_argc() > 2 { (offset.max(0.) as usize).min(s.len()) } else { 0 };
            Ok(match parse_entity_data(ctx, entity, &s[offset..])? {
                Some(end) => (offset + end) as f32,
                None => 0.,
            })
        })
    });
}

/// `VmScalarType`'s numbers are FTEQW's `ev_*` numbers.
const _: () = assert!(VmScalarType::Function as u8 == 6);
