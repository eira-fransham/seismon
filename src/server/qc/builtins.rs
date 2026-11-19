//! The QuakeC builtins, as defined by Quake's `pr_cmds.c`.
//!
//! Builtins are registered by name in the `QCWorldspawn` namespace. Arguments and return values
//! are converted by `bevy_mod_scripting`: floats are `f32`, vectors are `[f32; 3]`, entities are
//! references to their `QCEntity` component, strings are `QCString` and field references are
//! `QCField`.

use std::{any::TypeId, io::Write as _, ops::Bound};

use bevy::prelude::*;
use bevy_mod_commands::{Registry, RunCmd};
use bevy_mod_scripting::bindings::{
    InteropError, NamespaceBuilder, ReflectBase, ReflectReference, ThreadWorldContainer, V,
};
use bevy_mod_scripting_qcvm::{QCEntity, QCField, QCString, QCWorldspawn};
use byteorder::{LittleEndian, WriteBytesExt as _};
use num::FromPrimitive as _;
use seismon_utils::QString;

use crate::{
    common::net::{ServerCmd, write_angle_byte},
    server::{
        MsgDest, ServerCtx, ServerVars,
        error::ServerError,
        ftos,
        math::{AngleVectors, angle_vectors, vec_to_angles},
        qc::{
            fields::{QcTables, QcValue},
            format_entity,
        },
        quake_contents, vtos,
        world::{EntityFlags, EntitySolid, phys::CollideKind},
    },
};

fn to_interop(error: ServerError) -> InteropError {
    match error {
        ServerError::Qc { source } => source,
        other => InteropError::external_boxed(Report(other).to_string().into()),
    }
}

/// Formats an error with its sources.
struct Report(ServerError);

impl std::fmt::Display for Report {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", snafu::Report::from_error(&self.0))
    }
}

/// Runs `f` with exclusive access to the world.
fn with_ctx<O>(
    f: impl FnOnce(&mut ServerCtx<'_>) -> Result<O, ServerError>,
) -> Result<O, InteropError> {
    let thread_ctx = ThreadWorldContainer::try_get_context()?;
    thread_ctx
        .world
        .with_world_mut_access_and_then(|world| f(&mut ServerCtx::new(world)).map_err(to_interop))
}

/// The entity that an entity argument refers to.
fn entity(reference: &ReflectReference) -> Result<Entity, ServerError> {
    match *reference.base.base_id() {
        ReflectBase::Component(entity, _)
            if reference.base.type_id() == TypeId::of::<QCEntity>() =>
        {
            Ok(entity)
        }
        _ => Err(ServerError::with_msg(format!("Expected an entity, found {reference:?}"))),
    }
}

/// A reference to pass an entity to QuakeC.
fn entity_ref(ctx: &ServerCtx<'_>, entity: Entity) -> ReflectReference {
    let component = ctx.world.resource::<QcTables>().qc_entity_component;
    // Safety: `qc_entity_component` is the component ID of `QCEntity`.
    unsafe {
        ReflectReference::new_component_ref_by_id(entity, component, TypeId::of::<QCEntity>())
    }
}

fn string(s: &Option<V<QCString>>) -> &[u8] {
    s.as_ref().map(|s| s.0.as_bytes()).unwrap_or_default()
}

/// Concatenates string arguments.
///
/// Equivalent to `PF_VarString`.
fn var_string(args: &[&Option<V<QCString>>]) -> QString {
    let mut out = Vec::new();
    for arg in args {
        out.extend_from_slice(string(arg));
    }
    out.into()
}

macro_rules! todo_builtin {
    ($name:literal) => {{
        const ERROR_MSG: &str = concat!("TODO: ", $name);

        if cfg!(debug_assertions) {
            panic!("{ERROR_MSG}")
        } else {
            error!("{ERROR_MSG}");
        }

        Ok::<(), InteropError>(())
    }};
}

type S = Option<V<QCString>>;
type Ent = ReflectReference;
type Vec3Arg = [f32; 3];

/// Registers the builtins with `bevy_mod_scripting`.
pub fn register(world: &mut World) {
    NamespaceBuilder::<QCWorldspawn>::new(world)
        .register("makevectors", |angles: Vec3Arg| {
            with_ctx(|ctx| {
                let AngleVectors { forward, right, up } = angle_vectors(Vec3::from(angles));
                let mut globals = ctx.globals_mut();
                globals.v_forward = forward;
                globals.v_right = right;
                globals.v_up = up;
                Ok(())
            })
        })
        .register("setorigin", |e: Ent, origin: Vec3Arg| {
            with_ctx(|ctx| ctx.set_entity_origin(entity(&e)?, origin.into()))
        })
        .register("setmodel", |e: Ent, model: S| {
            with_ctx(|ctx| {
                let model = model.map(|m| m.0).unwrap_or_default();
                ctx.set_entity_model(entity(&e)?, model)
            })
        })
        .register("setsize", |e: Ent, min: Vec3Arg, max: Vec3Arg| {
            with_ctx(|ctx| {
                ctx.phys_mut(entity(&e)?)?.set_min_max_size(min.into(), max.into());
                Ok(())
            })
        })
        .register("break", || todo_builtin!("break"))
        .register("random", || with_ctx(|ctx| Ok(ctx.rng().random_qc())))
        .register("sound", |e: Ent, channel: f32, sample: S, volume: f32, attenuation: f32| {
            with_ctx(|ctx| {
                let entity = entity(&e)?;
                let Some(sample) = sample else {
                    return Err(ServerError::with_msg("invalid sound"));
                };
                let sample_name = sample.0.to_string_lossy().into_owned();

                debug!(
                    "Playing sound on {entity}.{channel}: {sample_name} vol {volume} attn {attenuation}"
                );

                ctx.sound(entity, channel as _, &sample_name, volume, attenuation)
            })
        })
        .register("normalize", |v: Vec3Arg| -> [f32; 3] {
            Vec3::from(v).normalize_or_zero().to_array()
        })
        .register(
            "error",
            |a: S, b: S, c: S, d: S, e: S, f: S, g: S, h: S| -> Result<(), InteropError> {
                let msg = var_string(&[&a, &b, &c, &d, &e, &f, &g, &h]);
                with_ctx(|ctx| Err(qc_error(ctx, "Error", msg)))
            },
        )
        .register(
            "objerror",
            |a: S, b: S, c: S, d: S, e: S, f: S, g: S, h: S| -> Result<(), InteropError> {
                let msg = var_string(&[&a, &b, &c, &d, &e, &f, &g, &h]);
                with_ctx(|ctx| {
                    let err = qc_error(ctx, "Object error", msg);

                    // Unlike `error`, this also removes `self`.
                    let self_id = ctx.globals().self_;
                    if !ctx.is_world(self_id) {
                        ctx.free(self_id)?;
                    }

                    Err(err)
                })
            },
        )
        .register("vlen", |v: Vec3Arg| Vec3::from(v).length())
        .register("vectoyaw", |v: Vec3Arg| vec_to_angles(v.into()).y)
        .register("spawn", || {
            with_ctx(|ctx| {
                let entity = ctx.spawn_entity()?;
                Ok(entity_ref(ctx, entity))
            })
        })
        .register("remove", |e: Ent| with_ctx(|ctx| ctx.free(entity(&e)?)))
        .register("traceline", |v1: Vec3Arg, v2: Vec3Arg, kind: f32, e: Ent| {
            with_ctx(|ctx| builtin_trace_line(ctx, v1.into(), v2.into(), kind, entity(&e)?))
        })
        .register("checkclient", || {
            with_ctx(|ctx| {
                let entity = builtin_check_client(ctx)?;
                Ok(entity_ref(ctx, entity))
            })
        })
        .register("find", |e: Ent, field: V<QCField>, matching: S| {
            with_ctx(|ctx| {
                let entity = builtin_find(ctx, entity(&e)?, field.0, string(&matching))?;
                Ok(entity_ref(ctx, entity))
            })
        })
        .register("precache_sound", precache_sound)
        .register("precache_sound2", precache_sound)
        .register("precache_model", precache_model)
        .register("precache_model2", precache_model)
        // Only used in `qcc`, does nothing at runtime
        .register("precache_file", |s: S| s)
        .register("precache_file2", |s: S| s)
        .register("stuffcmd", |e: Ent, s: S| {
            with_ctx(|ctx| {
                let entity = entity(&e)?;
                let text = QString::from(string(&s).to_vec());

                let Some(mut dest) = ctx.client_message(entity) else {
                    return Err(ServerError::with_msg(format!("stuffcmd: {entity} is not a client")));
                };

                ServerCmd::StuffText { text }.serialize(&mut *dest)?;

                Ok(())
            })
        })
        .register("findradius", |origin: Vec3Arg, radius: f32| {
            with_ctx(|ctx| {
                let entity = builtin_find_radius(ctx, origin.into(), radius)?;
                Ok(entity_ref(ctx, entity))
            })
        })
        .register("bprint", |s: S, _b: S, _c: S, _d: S, _e: S, _f: S, _g: S, _h: S| {
            debug!("BPRINT: {}", String::from_utf8_lossy(string(&s)));

            // TODO: Broadcast to all clients
        })
        .register(
            "sprint",
            |e: Ent, a: S, b: S, c: S, d: S, f: S, g: S, h: S| -> Result<(), InteropError> {
                let text = var_string(&[&a, &b, &c, &d, &f, &g, &h]);
                with_ctx(|ctx| {
                    let entity = entity(&e)?;
                    let Some(mut dest) = ctx.client_message(entity) else {
                        warn!("tried to sprint to a non-client ({entity})");
                        return Ok(());
                    };

                    ServerCmd::Print { text }.serialize(&mut *dest)?;

                    Ok(())
                })
            },
        )
        .register("dprint", |a: S, b: S, c: S, d: S, e: S, f: S, g: S, h: S| {
            let string = var_string(&[&a, &b, &c, &d, &e, &f, &g, &h]);
            debug!("DPRINT: {string}");
        })
        .register("ftos", |f: f32| V(QCString::from(&*ftos(f))))
        .register("vtos", |v: Vec3Arg| V(QCString::from(&*vtos(v.into()))))
        .register("coredump", || todo_builtin!("coredump"))
        .register("traceon", || todo_builtin!("traceon"))
        .register("traceoff", || todo_builtin!("traceoff"))
        .register("eprint", |_e: Ent| todo_builtin!("eprint"))
        .register("walkmove", |yaw: f32, dist: f32| {
            with_ctx(|ctx| builtin_walk_move(ctx, yaw, dist))
        })
        // `droptofloor` is defined with 2 arguments, but every example calls it with none and
        // Quake ignores them.
        .register("droptofloor", || {
            with_ctx(|ctx| {
                let ent_id = ctx.globals().self_;
                let hit_floor = ctx.drop_entity_to_floor(ent_id)?;
                Ok(hit_floor as u8 as f32)
            })
        })
        .register("lightstyle", |style: f32, value: S| {
            with_ctx(|ctx| builtin_lightstyle(ctx, style, value.map(|v| v.0).unwrap_or_default()))
        })
        .register("rint", |f: f32| f.round())
        .register("floor", |f: f32| f.floor())
        .register("ceil", |f: f32| f.ceil())
        .register("checkbottom", |e: Ent| {
            with_ctx(|ctx| {
                let server_vars: ServerVars = ctx.registry().read_cvars()?;
                Ok(if ctx.check_bottom(entity(&e)?, &server_vars)? { 1. } else { 0. })
            })
        })
        .register("pointcontents", |pos: Vec3Arg| {
            with_ctx(|ctx| Ok(quake_contents(ctx.point_contents(pos.into()))))
        })
        .register("fabs", |f: f32| f.abs())
        // TODO: This always just emulates the `noaim` client option (or `sv_aim 1`), we should
        // implement aim properly. Without autoaim, Quake returns `v_forward`, which QuakeC sets
        // with `makevectors(self.v_angle)` before calling `aim`.
        .register("aim", |_e: Ent, _speed: f32| {
            with_ctx(|ctx| Ok(ctx.globals().v_forward.to_array()))
        })
        .register("cvar", |name: S| {
            with_ctx(|ctx| {
                let name = String::from_utf8_lossy(string(&name)).into_owned();
                let f: f32 = match ctx.registry().read_cvar(&name) {
                    Ok(f) => f,
                    Err(e) => {
                        error!("{}", e);
                        default()
                    }
                };

                debug!("{} = {}", name, f);
                Ok(f)
            })
        })
        .register("localcmd", |s: S| with_ctx(|ctx| builtin_local_cmd(ctx, string(&s))))
        .register("nextent", |e: Ent| {
            with_ctx(|ctx| {
                let e = builtin_next_ent(ctx, entity(&e)?)?;
                Ok(entity_ref(ctx, e))
            })
        })
        .register("particle", |origin: Vec3Arg, direction: Vec3Arg, color: f32, count: f32| {
            with_ctx(|ctx| {
                ServerCmd::Particle {
                    origin: origin.into(),
                    direction: direction.into(),
                    color: color as _,
                    count: count as _,
                }
                .serialize(&mut ctx.level_mut().broadcast)?;
                Ok(())
            })
        })
        .register("ChangeYaw", || {
            with_ctx(|ctx| {
                let self_id = ctx.globals().self_;
                ctx.change_yaw(self_id)
            })
        })
        .register("vectoangles", |v: Vec3Arg| vec_to_angles(v.into()).to_array())
        .register("WriteByte", |to: f32, value: f32| {
            write(to, |dest| Ok(dest.write_u8(value as i32 as u8)?))
        })
        .register("WriteChar", |to: f32, value: f32| {
            write(to, |dest| Ok(dest.write_i8(value as i32 as i8)?))
        })
        .register("WriteShort", |to: f32, value: f32| {
            write(to, |dest| Ok(dest.write_i16::<LittleEndian>(value as i32 as i16)?))
        })
        .register("WriteLong", |to: f32, value: f32| {
            write(to, |dest| Ok(dest.write_i32::<LittleEndian>(value as i32)?))
        })
        .register("WriteCoord", |to: f32, value: f32| {
            write(to, |dest| Ok(dest.write_i16::<LittleEndian>((value * 8.) as i32 as i16)?))
        })
        .register("WriteAngle", |to: f32, value: f32| {
            write(to, |dest| Ok(dest.write_u8(write_angle_byte(value))?))
        })
        .register("WriteString", |to: f32, s: S| {
            let bytes = string(&s).to_vec();
            write(to, move |dest| {
                dest.write_all(&bytes)?;
                dest.write_u8(0)?;
                Ok(())
            })
        })
        .register("WriteEntity", |to: f32, e: Ent| {
            let target = entity(&e).map_err(to_interop)?;
            with_ctx(|ctx| {
                let edict = ctx.edict(target)?;
                write_dest(ctx, to, |dest| Ok(dest.write_u16::<LittleEndian>(edict)?))
            })
        })
        .register("movetogoal", |dist: f32| with_ctx(|ctx| builtin_move_to_goal(ctx, dist)))
        .register("makestatic", |e: Ent| with_ctx(|ctx| builtin_make_static(ctx, entity(&e)?)))
        .register("changelevel", |level: S| {
            with_ctx(|ctx| {
                // Equivalent to `PF_changelevel`: only the first call in a level counts, as
                // QuakeC may call this several times before the level actually changes.
                if ctx.level().changelevel_issued {
                    return Ok(());
                }
                ctx.level_mut().changelevel_issued = true;

                let mut text = b"changelevel ".to_vec();
                text.extend_from_slice(string(&level));
                text.push(b'\n');
                builtin_local_cmd(ctx, &text)
            })
        })
        .register("cvar_set", |var: S, val: S| {
            with_ctx(|ctx| {
                let var = String::from_utf8_lossy(string(&var)).into_owned();
                let val = String::from_utf8_lossy(string(&val)).into_owned();

                if let Err(e) = ctx.world.resource_mut::<Registry>().set_cvar(&var, &val) {
                    warn!("cvar_set: failed to set {var}: {e}");
                    return Ok(());
                }
                debug!("{} = {}", var, val);

                Ok(())
            })
        })
        .register(
            "centerprint",
            |e: Ent, a: S, b: S, c: S, d: S, f: S, g: S, h: S| -> Result<(), InteropError> {
                let text = var_string(&[&a, &b, &c, &d, &f, &g, &h]);
                with_ctx(|ctx| {
                    let entity = entity(&e)?;
                    let Some(mut dest) = ctx.client_message(entity) else {
                        warn!("tried to centerprint to a non-client ({entity})");
                        return Ok(());
                    };

                    ServerCmd::CenterPrint { text }.serialize(&mut *dest)?;

                    Ok(())
                })
            },
        )
        .register("ambientsound", |origin: Vec3Arg, sample: S, volume: f32, attenuation: f32| {
            with_ctx(|ctx| {
                let sample = sample.map(|s| s.0).unwrap_or_default();
                let volume = (volume * 255.) as _;
                let attenuation = (attenuation * 64.) as _;

                let Some(sound_id) = ctx.level().sound_id(&sample) else {
                    error!("Cannot find sound {} in precache", sample.to_string_lossy());
                    return Ok(());
                };

                // Static sounds are part of the sign-on message, so clients get them when they
                // connect.
                ServerCmd::StaticSound {
                    origin: origin.into(),
                    sound_id: sound_id as _,
                    volume,
                    attenuation,
                }
                .serialize(&mut ctx.level_mut().init)?;

                Ok(())
            })
        })
        .register("setspawnparms", |e: Ent| {
            with_ctx(|ctx| {
                let parms = ctx
                    .client_spawn_params(entity(&e)?)
                    .ok_or_else(|| ServerError::with_msg("setspawnparms: entity is not a client"))?;
                ctx.globals_mut().set_parms(parms);
                Ok(())
            })
        });
}

fn precache_sound(s: S) -> Result<S, InteropError> {
    with_ctx(|ctx| {
        let name = s.as_ref().map(|s| s.0.to_string_lossy().into_owned()).unwrap_or_default();
        ctx.level_mut().sound_precache.precache(&name)?;
        Ok(())
    })?;

    Ok(s)
}

fn precache_model(s: S) -> Result<S, InteropError> {
    with_ctx(|ctx| {
        let name = s.as_ref().map(|s| s.0.clone()).unwrap_or_default();
        // TODO: disable precaching after server is active
        if ctx.level().model_id(&name).is_none() {
            ctx.level_mut().model_precache.precache(name.to_string_lossy())?;
        }
        Ok(())
    })?;

    Ok(s)
}

/// Prints a QuakeC error, along with the fields of `self`, and returns it as an error.
fn qc_error(ctx: &mut ServerCtx<'_>, err_kind: &str, msg: QString) -> ServerError {
    let self_id = ctx.globals().self_;
    if ctx.exists(self_id) {
        let edict = ctx.edict(self_id).unwrap_or_default();
        error!("e{edict}: {}", format_entity(ctx.world, self_id));
    }

    ServerError::with_msg(format!("{err_kind}: {msg}"))
}

/// Runs `f` with the destination for the `Write*` builtins.
fn write_dest(
    ctx: &mut ServerCtx<'_>,
    to: f32,
    f: impl FnOnce(&mut Vec<u8>) -> Result<(), ServerError>,
) -> Result<(), ServerError> {
    let dest: MsgDest = (to as usize).try_into()?;
    match dest {
        // TODO: `All` should be for reliable messages, but as we don't currently have an
        // unreliable/reliable       message split we just combine it with broadcast
        // (i.e. unreliable) messages.
        MsgDest::All | MsgDest::Broadcast => f(&mut ctx.level_mut().broadcast),
        MsgDest::One => {
            let client = ctx.globals().msg_entity;
            let Some(mut dest) = ctx.client_message(client) else {
                return Err(ServerError::with_msg(format!("WriteDest: {client} is not a client")));
            };
            f(&mut dest)
        }
        MsgDest::Init => f(&mut ctx.level_mut().init),
    }
}

fn write(
    to: f32,
    f: impl FnOnce(&mut Vec<u8>) -> Result<(), ServerError>,
) -> Result<(), InteropError> {
    with_ctx(|ctx| write_dest(ctx, to, f))
}

fn builtin_trace_line(
    ctx: &mut ServerCtx<'_>,
    v1: Vec3,
    v2: Vec3,
    kind: f32,
    ent: Entity,
) -> Result<(), ServerError> {
    let (trace, hit_ent) = ctx.trace_entity_move(
        ent,
        v1,
        Vec3::ZERO,
        Vec3::ZERO,
        v2,
        CollideKind::from_f32(kind).unwrap_or_default(),
    )?;

    let worldspawn = ctx.worldspawn();
    let mut globals = ctx.globals_mut();
    globals.trace_allsolid = if trace.all_solid() { 1. } else { 0. };
    globals.trace_startsolid = if trace.start_solid() { 1. } else { 0. };
    globals.trace_fraction = trace.ratio();
    globals.trace_inwater = if trace.in_water { 1. } else { 0. };
    globals.trace_inopen = if trace.in_open { 1. } else { 0. };
    globals.trace_endpos = trace.end_point();
    // Our trace planes face into the surface, but QuakeC expects them to face out of it.
    globals.trace_plane_normal = trace.plane().map(|plane| -plane.normal()).unwrap_or(Vec3::ZERO);
    globals.trace_plane_dist = trace.plane_dist().map(|dist| -dist).unwrap_or_default();
    // `trace_ent` is world if nothing was hit.
    globals.trace_ent = hit_ent.unwrap_or(worldspawn);

    Ok(())
}

fn builtin_find(
    ctx: &mut ServerCtx<'_>,
    start: Entity,
    field: QCField,
    matching: &[u8],
) -> Result<Entity, ServerError> {
    let tables = ctx.world.resource::<QcTables>().clone();
    let Some(name) = tables.fields.name_at(field.0) else {
        return Err(ServerError::with_msg(format!("find: invalid field {}", field.0)));
    };

    debug!("Finding {} for field {name}", String::from_utf8_lossy(matching));

    let start = ctx.edict(start).unwrap_or_default();
    for (_, ent) in ctx.edicts().range((Bound::Excluded(start as usize), Bound::Unbounded)) {
        let Ok(QcValue::String(s)) = tables.fields.get(ctx.world, ent, name) else {
            debug!("No string field {} on {:?}", name, ent);
            continue;
        };

        if s.as_bytes() == matching {
            return Ok(ent);
        }
    }

    Ok(ctx.worldspawn())
}

/// Appends commands to the end of the local command buffer, so they run after the current tick.
///
/// Equivalent to `PF_localcmd`.
fn builtin_local_cmd(ctx: &mut ServerCtx<'_>, text: &[u8]) -> Result<(), ServerError> {
    let text = String::from_utf8_lossy(text);
    let cmds = match RunCmd::parse_many(&text) {
        Ok(cmds) => cmds,
        Err(e) => {
            warn!("localcmd: failed to parse {text:?}: {e}");
            return Ok(());
        }
    };

    for cmd in cmds {
        ctx.world.write_message(cmd.into_owned());
    }

    Ok(())
}

/// The next entity after `start` in edict order, or world if there are no more.
///
/// Equivalent to `PF_nextent`, except that the edicts of clients that haven't connected are
/// skipped. Quake returns those (as they are never marked free), but ours have no components, so
/// QuakeC could not access any of their fields.
fn builtin_next_ent(ctx: &mut ServerCtx<'_>, start: Entity) -> Result<Entity, ServerError> {
    // QuakeC often calls `nextent` on an entity it has just removed, so fall back to the edict
    // number that the entity had before it was freed.
    let start = ctx.edict(start).ok().or_else(|| ctx.edicts().freed_edict(start)).unwrap_or(0);

    Ok(ctx
        .edicts()
        .range((Bound::Excluded(start as usize), Bound::Unbounded))
        .next()
        .map(|(_, ent)| ent)
        .unwrap_or_else(|| ctx.worldspawn()))
}

fn builtin_find_radius(
    ctx: &mut ServerCtx<'_>,
    find_origin: Vec3,
    find_radius: f32,
) -> Result<Entity, ServerError> {
    let find_radius_sqr = find_radius.powi(2);

    let mut chain_end = ctx.worldspawn();

    for (_, ent_id) in ctx.edicts().range(1..).collect::<Vec<_>>() {
        let ent = ctx.phys(ent_id)?;

        if ent.solid()? == EntitySolid::Not {
            continue;
        }

        let ent_center = ent.origin + (ent.mins + ent.maxs) / 2.;
        let diff = find_origin - ent_center;
        if diff.length_squared() > find_radius_sqr {
            continue;
        }

        ctx.ai_mut(ent_id)?.chain = chain_end;
        chain_end = ent_id;
    }

    Ok(chain_end)
}

fn builtin_lightstyle(
    ctx: &mut ServerCtx<'_>,
    style: f32,
    value: QCString,
) -> Result<(), ServerError> {
    let index = match style as i32 {
        i if i < 0 => return Err(ServerError::with_msg("negative lightstyle ID")),
        i => i as usize,
    };

    let mut level = ctx.level_mut();
    let id = u8::try_from(index)
        .map_err(|_| ServerError::with_msg(format!("lightstyle {index} out of range")))?;
    let num_styles = level.lightstyles.len();
    level.lightstyles.resize(num_styles.max(index + 1), QCString::null());

    if level.lightstyles[index] == value {
        return Ok(());
    }

    level.lightstyles[index] = value.clone();

    // Clients are sent all of the light styles when they connect, so this only needs to send
    // changes. Anything written before the level is active is cleared by `init_frame`, which
    // matches Quake only sending changes once the server is active.
    // TODO: This should be reliable (Quake sends it to each client's reliable message).
    ServerCmd::LightStyle { id, value: value.as_bytes().to_vec().into() }
        .serialize(&mut level.broadcast)?;

    Ok(())
}

fn builtin_walk_move(ctx: &mut ServerCtx<'_>, yaw: f32, dist: f32) -> Result<f32, ServerError> {
    let this_id = ctx.globals().self_;
    let ent_flags = ctx.phys(this_id)?.flags();

    if !ent_flags.intersects(EntityFlags::ON_GROUND | EntityFlags::SWIM | EntityFlags::FLY) {
        return Ok(0.);
    }

    let move_dir = Vec2::from_angle(yaw.to_radians()).extend(0.) * dist;

    let did_move = ctx.move_step(this_id, move_dir, true)?;

    // Restore after `move_step`, which can run touch functions.
    let worldspawn = ctx.worldspawn();
    let mut globals = ctx.globals_mut();
    globals.self_ = this_id;
    globals.world = worldspawn;

    Ok(if did_move { 1. } else { 0. })
}

/// Picks the next client for `checkclient` to return.
///
/// Cycles through the clients after `check`, skipping any which are dead or have `notarget`.
/// If no other client is suitable, returns `check`.
///
/// Equivalent to `PF_newcheckclient`.
fn new_check_client(ctx: &ServerCtx<'_>, check: usize) -> Result<usize, ServerError> {
    let max_clients = ctx.level().max_clients.max(1);
    let check = check.clamp(1, max_clients);

    let mut i = check;
    loop {
        i = if i == max_clients { 1 } else { i + 1 };

        if i == check {
            // Didn't find anything else.
            break;
        }

        let Some(ent_id) = ctx.edicts().get(i as u16) else {
            continue;
        };

        if ctx.player(ent_id)?.health <= 0. || ctx.phys(ent_id)?.has_flag(EntityFlags::NO_TARGET) {
            continue;
        }

        break;
    }

    Ok(i)
}

/// Returns a client that `self` might be able to see, or world.
///
/// Monsters use this to look for a player to attack. It only checks one client every 0.1s.
///
/// Equivalent to `PF_checkclient`
fn builtin_check_client(ctx: &mut ServerCtx<'_>) -> Result<Entity, ServerError> {
    const CHECK_INTERVAL: std::time::Duration = std::time::Duration::from_millis(100);

    let time = ctx.time();
    if time.saturating_sub(ctx.level().last_check_time) >= CHECK_INTERVAL {
        let check = new_check_client(ctx, ctx.level().last_check_client)?;
        let mut level = ctx.level_mut();
        level.last_check_client = check;
        level.last_check_time = time;
    }

    let check_id = ctx.edicts().get(ctx.level().last_check_client as u16);

    let visible = match check_id {
        Some(check_id) => ctx.player(check_id)?.health > 0.,
        None => false,
    };
    // TODO: Quake also returns world if `self` can't be in the client's PVS, but we don't
    // have PVS data on the server. QuakeC checks visibility itself before attacking.

    Ok(match check_id {
        Some(check_id) if visible => check_id,
        _ => ctx.worldspawn(),
    })
}

fn builtin_make_static(ctx: &mut ServerCtx<'_>, ent: Entity) -> Result<(), ServerError> {
    if !ctx.exists(ent) {
        error!("Tried to call `make_static` on a non-existant entity");
        return Ok(());
    }

    let render = ctx.render(ent)?;
    let phys = ctx.phys(ent)?;
    let (model_name, frame_id, colormap, skin_id, origin, angles) = (
        render.model.clone(),
        render.frame as _,
        render.colormap as _,
        render.skin as _,
        phys.origin,
        phys.angles,
    );

    // Even though there is a `ModelId` field, it seems like quake searches for the model in the
    // precache manually
    let model_id = ctx.level().model_index(&model_name)? as _;

    // Static entities are part of the sign-on message, so clients get them when they connect.
    ServerCmd::SpawnStatic { model_id, frame_id, colormap, skin_id, origin, angles }
        .serialize(&mut ctx.level_mut().init)?;

    ctx.free(ent)?;

    Ok(())
}

fn builtin_move_to_goal(ctx: &mut ServerCtx<'_>, dist: f32) -> Result<f32, ServerError> {
    let this_id = ctx.globals().self_;
    let goal_id = ctx.ai(this_id)?.goalentity;

    let flags = ctx.phys(this_id)?.flags();

    if !flags.intersects(EntityFlags::ON_GROUND | EntityFlags::FLY | EntityFlags::SWIM) {
        return Ok(0.);
    }

    // If the next step hits the enemy, return immediately.
    let enemy = ctx.ai(this_id)?.enemy;
    if !ctx.is_world(enemy) && ctx.close_enough(this_id, goal_id, dist)? {
        return Ok(1.);
    }

    // Try a few random directions to try to get unstuck.
    let ideal_yaw = ctx.ai(this_id)?.ideal_yaw;
    let did_move = !ctx.rng().random_ratio(1, 4) && ctx.step_direction(this_id, ideal_yaw, dist)?;

    if !did_move {
        ctx.new_chase_dir(this_id, goal_id, dist)?;
    }

    // TODO: `movetogoal` seemingly doesn't return anything in Quake should we remove this?
    Ok(if did_move { 1. } else { 0. })
}
