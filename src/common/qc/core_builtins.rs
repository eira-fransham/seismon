//! Builtins shared by every QuakeC VM.
//!
//! These are the Quake builtins that both SSQC and CSQC have and whose behaviour doesn't depend on
//! which side they run on: maths, strings, cvars and console commands, and builtins that only work
//! with the calling VM's own globals and entities (through its [`QcStorage`]). Builtins that act
//! on the server or the client (e.g. `setorigin`, `traceline`, `sound`) are registered by each
//! side instead.
//!
//! [`QcStorage`]: super::QcStorage

use std::sync::LazyLock;

use bevy::prelude::*;
use bevy_mod_commands::{ConsoleError, Registry, RunCmd};
use bevy_mod_scripting::bindings::{InteropError, NamespaceBuilder, V};
use bevy_mod_scripting_qcvm::QCString;
use dashmap::DashSet;

use super::{
    QcValue, QcVmCtx, qc_error,
    storage::ref_entity,
    stub::{Ent, Fld, S, Vec3Arg, string, todo_builtin, var_string},
    with_vm_ctx,
};
use crate::server::math::{AngleVectors, angle_vectors, vec_to_angles};

/// The names of the builtins registered by [`core_builtins`].
pub const CORE_BUILTINS: &[&str] = &[
    "makevectors",
    "break",
    "random",
    "normalize",
    "error",
    "objerror",
    "vlen",
    "vectoyaw",
    "spawn",
    "remove",
    "find",
    "precache_file",
    "precache_file2",
    "findradius",
    "dprint",
    "ftos",
    "vtos",
    "coredump",
    "traceon",
    "traceoff",
    "eprint",
    "rint",
    "floor",
    "ceil",
    "fabs",
    "cvar",
    "cvar_set",
    "localcmd",
    "nextent",
    "vectoangles",
];

static CVARS_WARNED: LazyLock<DashSet<String>> = LazyLock::new(|| DashSet::new());

/// Registers the builtins shared by every QuakeC VM in `ns`.
pub fn core_builtins<N: 'static>(ns: &mut NamespaceBuilder<'_, N>) {
    ns.register("makevectors", |angles: Vec3Arg| {
        with_vm_ctx(|ctx| {
            let AngleVectors { forward, right, up } = angle_vectors(Vec3::from(angles));
            ctx.set_global("v_forward", QcValue::Vector(forward))?;
            ctx.set_global("v_right", QcValue::Vector(right))?;
            ctx.set_global("v_up", QcValue::Vector(up))?;
            Ok(())
        })
    })
    .register("break", || todo_builtin!("break"))
    .register("random", || with_vm_ctx(|ctx| Ok(ctx.storage.random(ctx.world))))
    .register("normalize", |v: Vec3Arg| -> [f32; 3] {
        Vec3::from(v).normalize_or_zero().to_array()
    })
    .register(
        "error",
        |a: S, b: S, c: S, d: S, e: S, f: S, g: S, h: S| -> Result<(), InteropError> {
            let msg = var_string(&[&a, &b, &c, &d, &e, &f, &g, &h]);
            with_vm_ctx(|ctx| Err(report_error(ctx, "Error", &msg)?))
        },
    )
    .register(
        "objerror",
        |a: S, b: S, c: S, d: S, e: S, f: S, g: S, h: S| -> Result<(), InteropError> {
            let msg = var_string(&[&a, &b, &c, &d, &e, &f, &g, &h]);
            with_vm_ctx(|ctx| {
                let err = report_error(ctx, "Object error", &msg)?;

                // Unlike `error`, this also removes `self`.
                let self_id = ctx.entity_global("self")?;
                if self_id != ctx.root {
                    ctx.storage.free(ctx.world, self_id)?;
                }

                Err(err)
            })
        },
    )
    .register("vlen", |v: Vec3Arg| Vec3::from(v).length())
    .register("vectoyaw", |v: Vec3Arg| vec_to_angles(v.into()).y)
    .register("spawn", || {
        with_vm_ctx(|ctx| {
            let entity = ctx.storage.spawn(ctx.world)?;
            Ok(ctx.entity_ref(entity))
        })
    })
    .register("remove", |e: Ent| with_vm_ctx(|ctx| ctx.storage.free(ctx.world, ref_entity(&e)?)))
    .register("find", |e: Ent, field: Fld, matching: S| {
        with_vm_ctx(|ctx| {
            let entity = find(ctx, ref_entity(&e)?, field.0.0, string(&matching))?;
            Ok(ctx.entity_ref(entity))
        })
    })
    // Only used in `qcc`, does nothing at runtime
    .register("precache_file", |s: S| s)
    .register("precache_file2", |s: S| s)
    .register("findradius", |origin: Vec3Arg, radius: f32| {
        with_vm_ctx(|ctx| {
            let entity = find_radius(ctx, origin.into(), radius)?;
            Ok(ctx.entity_ref(entity))
        })
    })
    .register("dprint", |a: S, b: S, c: S, d: S, e: S, f: S, g: S, h: S| {
        let string = var_string(&[&a, &b, &c, &d, &e, &f, &g, &h]);
        debug!("DPRINT: {string}");
    })
    .register("ftos", |f: f32| V(QCString::from(&*crate::server::ftos(f))))
    .register("vtos", |v: Vec3Arg| V(QCString::from(&*crate::server::vtos(v.into()))))
    .register("coredump", || todo_builtin!("coredump"))
    .register("traceon", || todo_builtin!("traceon"))
    .register("traceoff", || todo_builtin!("traceoff"))
    .register("eprint", |_e: Ent| todo_builtin!("eprint"))
    .register("rint", |f: f32| f.round())
    .register("floor", |f: f32| f.floor())
    .register("ceil", |f: f32| f.ceil())
    .register("fabs", |f: f32| f.abs())
    .register("cvar", |name: S| {
        with_vm_ctx(|ctx| {
            let name = String::from_utf8_lossy(string(&name)).into_owned();
            let f: f32 = match ctx.world.resource::<Registry>().read_cvar(&name) {
                Ok(f) => f,
                Err(e) => {
                    if !CVARS_WARNED.contains(&name) {
                        CVARS_WARNED.insert(name.clone());
                        warn!("get cvar: {}", e);
                    }
                    default()
                }
            };

            debug!("{} = {}", name, f);
            Ok(f)
        })
    })
    .register("cvar_set", |var: S, val: S| {
        with_vm_ctx(|ctx| {
            let var = String::from_utf8_lossy(string(&var));
            let val = String::from_utf8_lossy(string(&val));

            let mut registry = ctx.world.resource_mut::<Registry>();

            if let Err(e) = registry.set_cvar(&var, &val) {
                // Warn about missing cvars the first time, but other kinds of errors should
                // always be reported.
                if !matches!(e, ConsoleError::NoSuchCvar { .. }) || !CVARS_WARNED.contains(&*var) {
                    CVARS_WARNED.insert(var.clone().into_owned());
                    warn!("set cvar: {e}");
                }

                if let Err(e) = registry.insert_cvar(var.clone().into_owned(), &val) {
                    warn!("set cvar: {e}");

                    return Ok(());
                }
            }

            debug!("{} = {}", var, val);

            Ok(())
        })
    })
    .register("localcmd", |s: S| {
        with_vm_ctx(|ctx| {
            local_cmd(ctx.world, string(&s));
            Ok(())
        })
    })
    .register("nextent", |e: Ent| {
        with_vm_ctx(|ctx| {
            let e = next_ent(ctx, ref_entity(&e)?);
            Ok(ctx.entity_ref(e))
        })
    })
    .register("vectoangles", |v: Vec3Arg| vec_to_angles(v.into()).to_array());
}

/// Logs `self` for `error` and `objerror`, and returns the error to abort QuakeC with.
fn report_error(
    ctx: &mut QcVmCtx<'_>,
    err_kind: &str,
    msg: &seismon_utils::QString,
) -> Result<InteropError, InteropError> {
    let self_id = ctx.entity_global("self")?;
    if let Some(edict) = ctx.storage.edict(ctx.world, self_id) {
        error!("e{edict}: {}", ctx.storage.format_entity(ctx.world, self_id));
    }

    Ok(qc_error(format!("{err_kind}: {msg}")))
}

/// Equivalent to `PF_Find`.
fn find(
    ctx: &mut QcVmCtx<'_>,
    start: Entity,
    field: u16,
    matching: &[u8],
) -> Result<Entity, InteropError> {
    let Some(name) = ctx.storage.field_name(ctx.world, field) else {
        return Err(qc_error(format!("find: invalid field {field}")));
    };

    debug!("Finding {} for field {name}", String::from_utf8_lossy(matching));

    let mut edict = ctx.storage.edict(ctx.world, start).unwrap_or_default();
    while let Some((next, ent)) = ctx.storage.next_edict(ctx.world, edict) {
        edict = next;
        let Ok(QcValue::String(s)) = ctx.get_field(ent, &name) else {
            debug!("No string field {} on {:?}", name, ent);
            continue;
        };

        if s.as_bytes() == matching {
            return Ok(ent);
        }
    }

    Ok(ctx.root)
}

/// Equivalent to `PF_findradius`.
fn find_radius(
    ctx: &mut QcVmCtx<'_>,
    find_origin: Vec3,
    find_radius: f32,
) -> Result<Entity, InteropError> {
    // `SOLID_NOT`
    const SOLID_NOT: f32 = 0.;

    let vector = |ctx: &QcVmCtx<'_>, ent, name| match ctx.get_field(ent, name)? {
        QcValue::Vector(v) => Ok(v),
        other => Err(qc_error(format!("findradius: {name} is not a vector: {other:?}"))),
    };

    let find_radius_sqr = find_radius.powi(2);

    let mut chain_end = ctx.root;

    let mut edict = 0;
    while let Some((next, ent)) = ctx.storage.next_edict(ctx.world, edict) {
        edict = next;

        if ctx.get_field(ent, "solid")? == QcValue::Float(SOLID_NOT) {
            continue;
        }

        let (origin, mins, maxs) =
            (vector(ctx, ent, "origin")?, vector(ctx, ent, "mins")?, vector(ctx, ent, "maxs")?);
        let ent_center = origin + (mins + maxs) / 2.;
        let diff = find_origin - ent_center;
        if diff.length_squared() > find_radius_sqr {
            continue;
        }

        ctx.set_field(ent, "chain", QcValue::Entity(chain_end))?;
        chain_end = ent;
    }

    Ok(chain_end)
}

/// Equivalent to `PF_nextent`.
fn next_ent(ctx: &mut QcVmCtx<'_>, start: Entity) -> Entity {
    // QuakeC often calls `nextent` on an entity it has just removed, so fall back to the edict
    // number that the entity had before it was freed.
    let start = ctx
        .storage
        .edict(ctx.world, start)
        .or_else(|| ctx.storage.freed_edict(ctx.world, start))
        .unwrap_or(0);

    ctx.storage.next_edict(ctx.world, start).map(|(_, ent)| ent).unwrap_or(ctx.root)
}

/// Runs console commands, as `localcmd` does.
///
/// Equivalent to `PF_localcmd`.
pub fn local_cmd(world: &mut World, text: &[u8]) {
    let text = String::from_utf8_lossy(text);
    let cmds = match RunCmd::parse_many(&text) {
        Ok(cmds) => cmds,
        Err(e) => {
            warn!("localcmd: failed to parse {text:?}: {e}");
            return;
        }
    };

    for cmd in cmds {
        world.write_message(cmd.into_owned());
    }
}
