//! CSQC builtins that move entities and trace against the world (`setorigin`, `traceline`,
//! `walkmove`, `runstandardplayerphysics` etc.). These use the same collision and movement code as
//! the server, against the client's map and CSQC's own entities (see
//! [`physics`](crate::client::csqc::physics)).

use bevy::prelude::*;
use bevy_mod_scripting::bindings::{InteropError, NamespaceBuilder, V};
use bevy_mod_scripting_qcvm::{QCString, builtin_argc};
use num::FromPrimitive as _;
use qbsp::{BspData, data::nodes::BspNodeRef};

use crate::{
    client::csqc::{
        models,
        physics::{CsqcCollision, CsqcMoveCtx, gravity, player_vars},
    },
    common::{
        qc::{
            QcValue, QcVmCtx, qc_error,
            storage::ref_entity,
            stub::{Ent, S, Vec3Arg, string},
            with_vm_ctx,
        },
        world::{
            EntityFlags,
            model::ModelKind,
            movement::{self, MoveCtx},
            phys::{CollideKind, MoveKind, Trace},
            player::{self, PlayerCtx},
        },
    },
    server::{
        MoveCmd, accelerate, is_liquid,
        math::{AngleVectors, angle_vectors},
        quake_contents, water_move,
    },
};

/// FTEQW's `PMF_JUMP_HELD`: the jump button was held last frame, so the player doesn't jump again.
const PMF_JUMP_HELD: i32 = 1;
/// The speed that `runstandardplayerphysics` jumps at, as Quake's QuakeC does.
const JUMP_SPEED: f32 = 270.;

/// The movement context for the calling CSQC VM.
fn move_ctx<'w>(ctx: &'w mut QcVmCtx<'_>) -> Result<CsqcMoveCtx<'w>, InteropError> {
    CsqcMoveCtx::new(ctx.world, ctx.root)
}

/// The entity in the `self` global.
fn self_entity(ctx: &QcVmCtx<'_>) -> Result<Entity, InteropError> {
    ctx.entity_global("self")
}

/// Sets the `trace_*` globals from `trace`, like the server's `traceline`.
fn set_trace_globals(
    ctx: &mut QcVmCtx<'_>,
    trace: &Trace,
    hit: Option<Entity>,
) -> Result<(), InteropError> {
    let flag = |b: bool| QcValue::Float(if b { 1. } else { 0. });
    let root = ctx.root;
    let globals = [
        ("trace_allsolid", flag(trace.all_solid())),
        ("trace_startsolid", flag(trace.start_solid())),
        ("trace_fraction", QcValue::Float(trace.ratio())),
        ("trace_inwater", flag(trace.in_water)),
        ("trace_inopen", flag(trace.in_open)),
        ("trace_endpos", QcValue::Vector(trace.end_point())),
        // Our trace planes face into the surface, but QuakeC expects them to face out of it.
        (
            "trace_plane_normal",
            QcValue::Vector(trace.plane().map(|plane| -plane.normal()).unwrap_or(Vec3::ZERO)),
        ),
        ("trace_plane_dist", QcValue::Float(trace.plane_dist().map(|d| -d).unwrap_or_default())),
        ("trace_ent", QcValue::Entity(hit.unwrap_or(root))),
    ];
    for (name, value) in globals {
        ctx.set_global(name, value)?;
    }
    Ok(())
}

/// Traces a box for `traceline` and `tracebox`, ignoring `ignore` (unless it's `world`).
#[expect(clippy::too_many_arguments)]
fn trace_box(
    ctx: &mut QcVmCtx<'_>,
    start: Vec3,
    min: Vec3,
    max: Vec3,
    end: Vec3,
    kind: f32,
    ignore: Entity,
) -> Result<(), InteropError> {
    let root = ctx.root;
    let kind = CollideKind::from_f32(kind).unwrap_or_default();
    let ignore = (ignore != root).then_some(ignore);
    let (trace, hit) = move_ctx(ctx)?.trace_move(ignore, start, min, max, end, kind)?;
    set_trace_globals(ctx, &trace, hit)
}

/// The bounds of the model with index `index`, for `setmodel`.
fn model_bounds(ctx: &QcVmCtx<'_>, index: i32) -> Option<(Vec3, Vec3)> {
    if index > 0
        && let Some(CsqcCollision(collision)) = ctx.world.get::<CsqcCollision>(ctx.root)
        && let Some(model) = collision.models().get(index as usize)
        && matches!(model.kind(), ModelKind::Brush(_))
    {
        return Some((model.min(), model.max()));
    }
    models::mdl(ctx.world, ctx.root, index).and_then(|mdl| mdl.bounds())
}

/// Whether any of the leaves that the box `min..max` touches (in the world model) is in `visible`.
fn box_touches_leaves(
    bsp: &BspData,
    min: Vec3,
    max: Vec3,
    visible: &hashbrown::HashSet<usize>,
) -> bool {
    let mut stack = vec![bsp.models[0].hulls.root];
    while let Some(node) = stack.pop() {
        match node {
            BspNodeRef::Leaf(leaf) => {
                if visible.contains(&(leaf as usize)) {
                    return true;
                }
            }
            BspNodeRef::Node(index) => {
                let node = &bsp.nodes[index as usize];
                let plane = &bsp.planes[node.plane_idx as usize];
                let corners = (0..8).map(|i| {
                    Vec3::new(
                        if i & 1 == 0 { min.x } else { max.x },
                        if i & 2 == 0 { min.y } else { max.y },
                        if i & 4 == 0 { min.z } else { max.z },
                    )
                });
                let (mut front, mut back) = (false, false);
                for corner in corners {
                    if plane.point_side(corner) >= 0. {
                        front = true;
                    } else {
                        back = true;
                    }
                }
                if front {
                    stack.push(*node.front);
                }
                if back {
                    stack.push(*node.back);
                }
            }
        }
    }
    false
}

/// `checkpvs`: whether any part of the box `min..max` is potentially visible from `viewpos`.
pub fn check_pvs(bsp: &BspData, viewpos: Vec3, min: Vec3, max: Vec3) -> bool {
    let leaf = bsp.leaf_at_point(0, viewpos);
    let Some(pvs) = bsp.potentially_visible_set(leaf) else {
        // Outside the map, everything is visible.
        return true;
    };
    let visible = match pvs.into_leaf_indices() {
        Ok(leaves) => leaves.collect::<hashbrown::HashSet<_>>(),
        // Clustered visibility (Quake 2) isn't supported, so assume it's visible.
        Err(_) => return true,
    };
    box_touches_leaves(bsp, min, max, &visible)
}

/// The move command and frame length that `runstandardplayerphysics` uses, from the `input_*`
/// globals.
fn input_command(ctx: &QcVmCtx<'_>) -> (MoveCmd, Vec3, i32, f32) {
    let vector = |name: &str| match ctx.get_global(name) {
        Ok(QcValue::Vector(v)) => v,
        _ => Vec3::ZERO,
    };
    let float = |name: &str| match ctx.get_global(name) {
        Ok(QcValue::Float(f)) => f,
        _ => 0.,
    };
    let moves = vector("input_movevalues");
    let cmd = MoveCmd { forward: moves.x, side: moves.y, up: moves.z };
    let mut dt = float("input_timelength");
    if dt <= 0. {
        dt = float("frametime");
    }
    (cmd, vector("input_angles"), float("input_buttons") as i32, dt)
}

/// `runstandardplayerphysics`: moves `ent` with the input in the `input_*` globals, like the
/// server moves players (Quake's `SV_ClientThink` and `SV_WalkMove`), including jumping.
///
/// FTEQW uses QuakeWorld's player movement (`pmove`) for this, which differs in details: this
/// uses the same movement as Seismon's server, so that prediction matches it.
// TODO: This should be deduplicated with the server-side physics.
fn standard_player_physics(ctx: &mut QcVmCtx<'_>, ent: Entity) -> Result<(), InteropError> {
    let (cmd, input_angles, buttons, dt) = input_command(ctx);
    let player_vars = player_vars(ctx.world);
    let gravity = gravity(ctx.world);
    let frame_time = std::time::Duration::from_secs_f32(dt.clamp(0., 1.));

    let mut ctx = move_ctx(ctx)?;
    let hot = ctx.storage.hot().clone();
    ctx.set_vec3(ent, hot.v_angle, input_angles)?;

    let move_kind = PlayerCtx::move_kind(&ctx, ent)?;
    let swimming = player::check_water(&mut ctx, ent)?;
    let flags = ctx.entity(ent)?.flags;
    let on_ground = flags.contains(EntityFlags::ON_GROUND);

    // Jumping, which Quake's QuakeC does, but CSQC expects the engine to.
    let pmove_flags = ctx
        .world
        .get::<crate::client::csqc::storage::CsqcFields>(ent)
        .map_or(0, |f| f.float(hot.pmove_flags) as i32);
    let jump = buttons & 2 != 0;
    let mut new_pmove_flags = pmove_flags;
    if !jump {
        new_pmove_flags &= !PMF_JUMP_HELD;
    } else if on_ground && pmove_flags & PMF_JUMP_HELD == 0 && move_kind == MoveKind::Walk {
        let velocity = ctx.velocity(ent)?;
        ctx.set_velocity(ent, velocity.with_z(velocity.z + JUMP_SPEED))?;
        ctx.remove_flags(ent, EntityFlags::ON_GROUND)?;
        new_pmove_flags |= PMF_JUMP_HELD;
    }
    ctx.set_float(ent, hot.pmove_flags, new_pmove_flags as f32)?;
    let on_ground = ctx.entity(ent)?.flags.contains(EntityFlags::ON_GROUND);

    // `SV_ClientThink`'s movement.
    let water_level = ctx.water_level(ent)?;
    if water_level >= 2. && move_kind != MoveKind::NoClip {
        let velocity = water_move(
            ctx.velocity(ent)?,
            &angle_vectors(input_angles),
            &cmd,
            player_vars.max_speed,
            player_vars.ground_friction,
            player_vars.accel,
            dt,
        );
        ctx.set_velocity(ent, velocity)?;
    } else {
        let AngleVectors { forward, right, .. } = angle_vectors(Vec3::new(
            if move_kind == MoveKind::Walk { 0. } else { input_angles.x },
            input_angles.y,
            0.,
        ));
        let mut wish_vel = forward * cmd.forward + right * cmd.side;
        wish_vel.z = if move_kind == MoveKind::Walk { 0. } else { cmd.up };
        let mut wish_speed = wish_vel.length();
        if wish_speed > player_vars.max_speed {
            wish_vel *= player_vars.max_speed / wish_speed;
            wish_speed = player_vars.max_speed;
        }
        let wish_dir = wish_vel.normalize_or_zero();
        let accel_speed = player_vars.accel * dt * wish_speed;

        if matches!(move_kind, MoveKind::NoClip | MoveKind::Fly) {
            ctx.set_velocity(ent, wish_vel)?;
        } else if on_ground {
            player::apply_friction(&mut ctx, ent, frame_time, &player_vars)?;
            let velocity = accelerate(ctx.velocity(ent)?, wish_dir, wish_speed, accel_speed);
            ctx.set_velocity(ent, velocity)?;
        } else {
            let wish_speed = wish_speed.min(player_vars.max_air_speed);
            let velocity = accelerate(ctx.velocity(ent)?, wish_dir, wish_speed, accel_speed);
            ctx.set_velocity(ent, velocity)?;
        }
    }

    // The server's `SV_Physics_Client`.
    match move_kind {
        MoveKind::Walk => {
            if !swimming && !flags.contains(EntityFlags::WATER_JUMP) {
                let velocity = ctx.velocity(ent)?;
                ctx.set_velocity(ent, velocity.with_z(velocity.z - gravity * dt))?;
            }
            player::check_stuck(&mut ctx, ent)?;
            let max_step = ctx.max_step()?;
            player::walk(&mut ctx, ent, frame_time, max_step)?;
        }
        MoveKind::Fly => {
            player::fly_move(&mut ctx, ent, frame_time)?;
        }
        MoveKind::NoClip => {
            let origin = ctx.entity(ent)?.origin + ctx.velocity(ent)? * dt;
            ctx.set_origin(ent, origin)?;
        }
        // Other move types don't move the player.
        _ => {}
    }

    if PlayerCtx::exists(&ctx, ent) {
        ctx.link(ent, true)?;
    }
    Ok(())
}

/// `tracetoss`: traces the path that `ent` would take if it were thrown (with gravity), stopping
/// at the first thing it hits (DarkPlaces' `SV_Trace_Toss`).
fn trace_toss(ctx: &mut QcVmCtx<'_>, ent: Entity, ignore: Entity) -> Result<(), InteropError> {
    const STEP: f32 = 0.05;
    let gravity = gravity(ctx.world);
    let root = ctx.root;
    let move_ctx = move_ctx(ctx)?;
    let state = move_ctx.entity(ent)?;
    let mut velocity = move_ctx.velocity(ent)?;
    let entity_gravity = match move_ctx.storage.field_slot("gravity") {
        Some(slot) => ctx_float(&move_ctx, ent, slot),
        None => 0.,
    };
    let entity_gravity = if entity_gravity == 0. { 1. } else { entity_gravity };

    let mut origin = state.origin;
    let mut result = (Trace::uninitialized(origin, origin), None);
    let ignore = (ignore != root).then_some(ignore);
    // TODO: Magic number. Maybe make this a cvar?
    for _ in 0..200 {
        velocity.z -= gravity * entity_gravity * STEP;
        let end = origin + velocity * STEP;
        let (trace, hit) = move_ctx.trace_move(
            ignore,
            origin,
            state.mins,
            state.maxs,
            end,
            CollideKind::Normal,
        )?;
        origin = trace.end_point();
        let stop = trace.ratio() < 1. || trace.start_solid();
        result = (trace, hit);
        if stop {
            break;
        }
    }

    set_trace_globals(ctx, &result.0, result.1)
}

fn ctx_float(ctx: &CsqcMoveCtx<'_>, ent: Entity, slot: crate::client::csqc::storage::Slot) -> f32 {
    ctx.world
        .get::<crate::client::csqc::storage::CsqcFields>(ent)
        .map_or(0., |f| f.float(Some(slot)))
}

/// Restores `self` after movement (which can run touch functions).
fn with_self<O>(
    ctx: &mut QcVmCtx<'_>,
    f: impl FnOnce(&mut CsqcMoveCtx<'_>, Entity) -> Result<O, InteropError>,
) -> Result<O, InteropError> {
    let this = self_entity(ctx)?;
    let result = f(&mut move_ctx(ctx)?, this);
    ctx.set_global("self", QcValue::Entity(this))?;
    result
}

pub fn register<N: 'static>(ns: &mut NamespaceBuilder<'_, N>) {
    ns.register("setorigin", |e: Ent, origin: Vec3Arg| {
        with_vm_ctx(|ctx| {
            let entity = ref_entity(&e)?;
            let mut move_ctx = move_ctx(ctx)?;
            move_ctx.set_origin(entity, origin.into())?;
            move_ctx.link(entity, false)
        })
    })
    .register("setsize", |e: Ent, min: Vec3Arg, max: Vec3Arg| {
        with_vm_ctx(|ctx| {
            let entity = ref_entity(&e)?;
            let (min, max) = (Vec3::from(min), Vec3::from(max));
            if min.cmpgt(max).any() {
                return Err(qc_error("setsize: backwards mins/maxs"));
            }
            let mut move_ctx = move_ctx(ctx)?;
            let hot = move_ctx.storage.hot().clone();
            move_ctx.set_vec3(entity, hot.mins, min)?;
            move_ctx.set_vec3(entity, hot.maxs, max)?;
            move_ctx.set_vec3(entity, hot.size, max - min)?;
            move_ctx.link(entity, false)
        })
    })
    .register("setmodel", |e: Ent, name: S| {
        with_vm_ctx(|ctx| {
            let entity = ref_entity(&e)?;
            let name = String::from_utf8_lossy(string(&name)).into_owned();
            let index = models::precache_model(ctx.world, ctx.root, &name);
            ctx.set_field(entity, "model", QcValue::String(QCString::from(&*name)))?;
            ctx.set_field(entity, "modelindex", QcValue::Float(index as f32))?;
            let (min, max) = model_bounds(ctx, index).unwrap_or((Vec3::ZERO, Vec3::ZERO));
            let mut move_ctx = move_ctx(ctx)?;
            let hot = move_ctx.storage.hot().clone();
            move_ctx.set_vec3(entity, hot.mins, min)?;
            move_ctx.set_vec3(entity, hot.maxs, max)?;
            move_ctx.set_vec3(entity, hot.size, max - min)?;
            move_ctx.link(entity, false)
        })
    })
    .register("precache_model", |name: S| {
        with_vm_ctx(|ctx| {
            models::precache_model(ctx.world, ctx.root, &String::from_utf8_lossy(string(&name)));
            Ok(V(name.map_or_else(QCString::null, |s| s.0)))
        })
    })
    .register("precache_model2", |name: S| {
        with_vm_ctx(|ctx| {
            models::precache_model(ctx.world, ctx.root, &String::from_utf8_lossy(string(&name)));
            Ok(V(name.map_or_else(QCString::null, |s| s.0)))
        })
    })
    .register("traceline", |start: Vec3Arg, end: Vec3Arg, kind: f32, ignore: Ent| {
        with_vm_ctx(|ctx| {
            let ignore = ref_entity(&ignore)?;
            trace_box(ctx, start.into(), Vec3::ZERO, Vec3::ZERO, end.into(), kind, ignore)
        })
    })
    .register(
        "tracebox",
        |start: Vec3Arg, min: Vec3Arg, max: Vec3Arg, end: Vec3Arg, kind: f32, ignore: Ent| {
            with_vm_ctx(|ctx| {
                let ignore = ref_entity(&ignore)?;
                trace_box(ctx, start.into(), min.into(), max.into(), end.into(), kind, ignore)
            })
        },
    )
    .register("tracetoss", |e: Ent, ignore: Ent| {
        with_vm_ctx(|ctx| trace_toss(ctx, ref_entity(&e)?, ref_entity(&ignore)?))
    })
    .register("pointcontents", |point: Vec3Arg| {
        with_vm_ctx(|ctx| Ok(quake_contents(move_ctx(ctx)?.point_contents(point.into()))))
    })
    .register("walkmove", |yaw: f32, dist: f32, _set_trace: f32| {
        with_vm_ctx(|ctx| {
            let moved = with_self(ctx, |m, this| movement::walk_move(m, this, yaw, dist))?;
            Ok(if moved { 1. } else { 0. })
        })
    })
    .register("droptofloor", || {
        with_vm_ctx(|ctx| {
            let dropped = with_self(ctx, |m, this| movement::drop_to_floor(m, this))?;
            Ok(if dropped { 1. } else { 0. })
        })
    })
    .register("checkbottom", |e: Ent| {
        with_vm_ctx(|ctx| {
            let entity = ref_entity(&e)?;
            Ok(if movement::check_bottom(&move_ctx(ctx)?, entity)? { 1. } else { 0. })
        })
    })
    .register("ChangeYaw", || {
        with_vm_ctx(|ctx| with_self(ctx, |m, this| movement::change_yaw(m, this)))
    })
    .register("movetogoal", |dist: f32| {
        with_vm_ctx(|ctx| {
            with_self(ctx, |m, this| movement::move_to_goal(m, this, dist).map(|_| ()))
        })
    })
    .register("touchtriggers", |e: Ent, origin: Vec3Arg| {
        let argc = builtin_argc();
        with_vm_ctx(|ctx| {
            let entity = if argc > 0 { ref_entity(&e)? } else { self_entity(ctx)? };
            let mut move_ctx = move_ctx(ctx)?;
            if argc > 1 {
                move_ctx.set_origin(entity, origin.into())?;
            }
            move_ctx.link(entity, true)
        })
    })
    .register("checkpvs", |viewpos: Vec3Arg, e: Ent| {
        with_vm_ctx(|ctx| {
            let entity = ref_entity(&e)?;
            let ent = move_ctx(ctx)?.entity(entity)?;
            let Some(handle) = super::super::map_handle(ctx.world) else {
                return Ok(1.);
            };
            let Some(bsp) = ctx.world.resource::<Assets<bevy_trenchbroom::bsp::Bsp>>().get(&handle)
            else {
                return Ok(1.);
            };
            let (min, max) = (ent.origin + ent.mins, ent.origin + ent.maxs);
            Ok(if check_pvs(&bsp.data, viewpos.into(), min, max) { 1. } else { 0. })
        })
    })
    .register("runstandardplayerphysics", |e: Ent| {
        with_vm_ctx(|ctx| {
            let entity = ref_entity(&e)?;
            with_self(ctx, |_, _| Ok(()))?;
            let this = self_entity(ctx)?;
            let result = standard_player_physics(ctx, entity);
            ctx.set_global("self", QcValue::Entity(this))?;
            result
        })
    });
}

/// `is_liquid` is re-exported for the effects builtins.
#[allow(dead_code)]
fn _uses() -> fn(crate::common::world::hull::BspLeafContents) -> bool {
    is_liquid
}
