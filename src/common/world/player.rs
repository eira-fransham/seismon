//! Walking and sliding movement (Quake's `SV_FlyMove`, `SV_WalkMove` and friends), for any QuakeC
//! VM. The server uses these for players and other walking entities, and CSQC for
//! `runstandardplayerphysics`.

use std::time::Duration;

use arrayvec::ArrayVec;
use bevy::prelude::*;
use seismon_utils::duration_to_f32;

use super::{
    EntitySolid,
    hull::BspLeafContents,
    movement::MoveCtx,
    phys::{CollideKind, MoveKind, Trace},
};
use crate::{
    common::{math::CollisionResult, world::EntityFlags},
    server::{
        PlayerVars, clip_velocity, friction_scale, is_liquid, math::angle_vectors, quake_contents,
        wall_friction,
    },
};

/// Access to a QuakeC VM's walking entities, on top of [`MoveCtx`].
pub trait PlayerCtx: MoveCtx {
    fn velocity(&self, entity: Entity) -> Result<Vec3, Self::Error>;
    fn set_velocity(&mut self, entity: Entity, velocity: Vec3) -> Result<(), Self::Error>;
    fn water_level(&self, entity: Entity) -> Result<f32, Self::Error>;
    fn set_water(&mut self, entity: Entity, level: f32, kind: f32) -> Result<(), Self::Error>;
    fn view_ofs(&self, entity: Entity) -> Result<Vec3, Self::Error>;
    fn v_angle(&self, entity: Entity) -> Result<Vec3, Self::Error>;
    fn old_origin(&self, entity: Entity) -> Result<Vec3, Self::Error>;
    fn set_old_origin(&mut self, entity: Entity, origin: Vec3) -> Result<(), Self::Error>;
    fn solid(&self, entity: Entity) -> Result<EntitySolid, Self::Error>;
    fn move_kind(&self, entity: Entity) -> Result<MoveKind, Self::Error>;
    /// Whether `entity` still exists (it can be removed by touch functions).
    fn exists(&self, entity: Entity) -> bool;
    /// Whether `entity` is stuck in something where it is.
    fn is_stuck(&self, entity: Entity) -> Result<bool, Self::Error>;
    /// Runs the touch functions of `a` and `b` on each other (`SV_Impact`).
    fn impact(&mut self, a: Entity, b: Entity) -> Result<(), Self::Error>;
    /// An error for a move that hit a surface but no entity.
    fn hit_without_entity(&self, entity: Entity) -> Self::Error;
}

/// Moves an entity along its velocity for `frame_time`, sliding along anything it hits.
///
/// If the entity lands on a brush entity, it is marked as on the ground. The returned
/// metadata is the last trace which hit a wall or step, if any.
///
/// Equivalent to `SV_FlyMove`.
pub fn fly_move<C: PlayerCtx + ?Sized>(
    ctx: &mut C,
    ent_id: Entity,
    frame_time: Duration,
) -> Result<CollisionResult<Option<Trace>>, C::Error> {
    const BUMP_COUNT: usize = 4;
    const MAX_CLIP_PLANES: usize = 5;

    let mut out = CollisionResult::default();

    let primal_velocity = ctx.velocity(ent_id)?;
    let mut original_velocity = primal_velocity;
    // Normals of the planes the entity is touching. Unlike our traces (but like Quake), these
    // face out of the surface.
    let mut planes = ArrayVec::<Vec3, MAX_CLIP_PLANES>::new();

    let mut time_left: f32 = duration_to_f32(frame_time);

    for _ in 0..BUMP_COUNT {
        let ent = ctx.entity(ent_id)?;
        let velocity = ctx.velocity(ent_id)?;
        if (time_left * velocity).length_squared() < f32::EPSILON {
            break;
        }

        let origin = ent.origin;
        let min = ent.mins;
        let max = ent.maxs;

        let end = origin + time_left * velocity;

        let (trace, touched_ent_id) =
            ctx.trace(ent_id, origin, min, max, end, CollideKind::Normal)?;

        if trace.all_solid() {
            // Entity is trapped in another solid.
            ctx.set_velocity(ent_id, Vec3::ZERO)?;
            return Ok(CollisionResult { floor: true, wall_or_step: true, ..out });
        }

        if trace.ratio() > 0. {
            // If ratio > 0, we moved some distance.
            ctx.set_origin(ent_id, trace.end_point())?;
            original_velocity = ctx.velocity(ent_id)?;
            planes.clear();
        }

        let Some(plane) = trace.plane() else {
            // Travelled full trace distance.
            break;
        };

        let Some(hit) = touched_ent_id else {
            return Err(ctx.hit_without_entity(ent_id));
        };

        let normal = -plane.normal();

        if normal.z > 0.7 {
            out.floor = true;
            if ctx.solid(hit)? == EntitySolid::Bsp {
                ctx.add_flags(ent_id, EntityFlags::ON_GROUND)?;
                ctx.set_ground_entity(ent_id, hit)?;
            }
        }

        if normal.z == 0. {
            out.wall_or_step = true;
            // Saved for the player's extra wall friction.
            out.metadata = Some(trace.clone());
        }

        ctx.impact(ent_id, hit)?;
        if !ctx.exists(ent_id) {
            // Removed by the impact function.
            break;
        }

        time_left -= time_left * trace.ratio();

        if planes.try_push(normal).is_err() {
            // This shouldn't really happen.
            ctx.set_velocity(ent_id, Vec3::ZERO)?;
            return Ok(CollisionResult { floor: true, wall_or_step: true, ..out });
        }

        // Modify `original_velocity` so that it is parallel to all of the clip planes.
        let along_plane = planes.iter().enumerate().find_map(|(i, &plane_i)| {
            let new_velocity = clip_velocity(original_velocity, plane_i, 1.);
            planes
                .iter()
                .enumerate()
                .all(|(j, &plane_j)| i == j || new_velocity.dot(plane_j) >= 0.)
                .then_some(new_velocity)
        });

        let new_velocity = match along_plane {
            Some(new_velocity) => new_velocity,
            // Go along the crease.
            None if planes.len() == 2 => {
                let dir = planes[0].cross(planes[1]);
                dir * dir.dot(ctx.velocity(ent_id)?)
            }
            None => {
                ctx.set_velocity(ent_id, Vec3::ZERO)?;
                return Ok(CollisionResult {
                    floor: true,
                    wall_or_step: true,
                    dead_stop: true,
                    ..out
                });
            }
        };

        // If the new velocity is against the original velocity, stop dead to avoid tiny
        // oscillations in sloping corners.
        if new_velocity.dot(primal_velocity) <= 0. {
            ctx.set_velocity(ent_id, Vec3::ZERO)?;
            return Ok(out);
        }

        ctx.set_velocity(ent_id, new_velocity)?;
    }

    Ok(out)
}

/// Moves an entity by `move_dir` (stopping at whatever it hits), relinks it and runs the touch
/// functions of anything it hit.
///
/// Equivalent to `SV_PushEntity`.
pub fn push_entity<C: PlayerCtx + ?Sized>(
    ctx: &mut C,
    ent_id: Entity,
    move_dir: Vec3,
) -> Result<(Trace, Option<Entity>), C::Error> {
    let ent = ctx.entity(ent_id)?;
    let start = ent.origin;
    let end = start + move_dir;
    let min = ent.mins;
    let max = ent.maxs;

    let kind = match (ctx.move_kind(ent_id)?, ctx.solid(ent_id)?) {
        (MoveKind::FlyMissile, _) => CollideKind::Missile,
        (_, EntitySolid::Not | EntitySolid::Trigger) => CollideKind::NoMonsters,
        _ => CollideKind::Normal,
    };

    let (trace, hit) = ctx.trace(ent_id, start, min, max, end, kind)?;

    ctx.set_origin(ent_id, trace.end_point())?;

    ctx.link(ent_id, true)?;

    if let Some(hit) = hit {
        ctx.impact(ent_id, hit)?;
    }

    Ok((trace, hit))
}

/// Moves a walking entity, stepping up stairs and sliding along walls.
///
/// Equivalent to `SV_WalkMove`.
pub fn walk<C: PlayerCtx + ?Sized>(
    ctx: &mut C,
    ent_id: Entity,
    frame_time: Duration,
    max_step: f32,
) -> Result<(), C::Error> {
    const ORIGIN_EPSILON: f32 = 0.03125;

    let ent = ctx.entity(ent_id)?;

    let water_jump = ent.flags.contains(EntityFlags::WATER_JUMP);
    let was_on_ground = ent.flags.contains(EntityFlags::ON_GROUND);
    ctx.remove_flags(ent_id, EntityFlags::ON_GROUND)?;

    let old_origin = ent.origin;
    let old_velocity = ctx.velocity(ent_id)?;
    let water_level = ctx.water_level(ent_id)?;

    let clip = fly_move(ctx, ent_id, frame_time)?;

    if !ctx.exists(ent_id) {
        return Ok(());
    }

    // Read after moving, since if we moved into e.g. a trigger that kills the entity
    // then we don't want to do anything here.
    let move_kind = ctx.move_kind(ent_id)?;

    if
    // Move didn't block on a wall/step.
    !clip.wall_or_step ||
        // Entity is in the air, don't apply step logic.
        water_level < f32::EPSILON && !was_on_ground ||
        // Entity isn't "walk" type
        move_kind != MoveKind::Walk ||
        // TODO: `sv_nostep`
        water_jump
    {
        return Ok(());
    }

    let pre_step_origin = ctx.entity(ent_id)?.origin;
    let pre_step_velocity = ctx.velocity(ent_id)?;

    ctx.set_origin(ent_id, old_origin)?;

    let upmove = Vec3::Z * max_step;
    let downmove = Vec3::Z * (old_velocity.z * duration_to_f32(frame_time) - max_step);

    ctx.set_velocity(ent_id, old_velocity.with_z(0.))?;

    push_entity(ctx, ent_id, upmove)?;

    let mut clip = fly_move(ctx, ent_id, frame_time)?;
    // The wall hit by the stepped-up move. `SV_TryUnstick` uses its own trace, so this is
    // kept even if it replaces `clip`.
    let step_trace = clip.metadata.take();

    let post_step_origin = ctx.entity(ent_id)?.origin;
    let post_clip_move = (post_step_origin - old_origin).xy().abs();

    // Check for stuckness, possibly due to the limited precision of floats in the clipping
    // hulls: stepping up didn't make any progress.
    if clip.any_collision() && post_clip_move.to_array().iter().all(|v| *v < ORIGIN_EPSILON) {
        clip = try_unstick(ctx, ent_id, old_velocity)?;
    }

    // Extra friction based on the view angle.
    if clip.wall_or_step
        && let Some(wall) = step_trace.as_ref().and_then(Trace::plane)
    {
        let forward = angle_vectors(ctx.v_angle(ent_id)?).forward;
        // Our trace planes face into the solid, but Quake's face out of it.
        let velocity = wall_friction(ctx.velocity(ent_id)?, forward, -wall.normal());
        ctx.set_velocity(ent_id, velocity)?;
    }

    let (downtrace, down_ent) = push_entity(ctx, ent_id, downmove)?;
    if downtrace.plane().map(|plane| plane.is_floor()).unwrap_or_default() {
        let worldspawn = ctx.world();
        // This checks the moving entity rather than the ground, as Quake does. Players aren't
        // `SOLID_BSP`, so in practice `fly_move` sets the ground instead.
        if ctx.solid(ent_id)? == EntitySolid::Bsp {
            ctx.add_flags(ent_id, EntityFlags::ON_GROUND)?;
            ctx.set_ground_entity(ent_id, down_ent.unwrap_or(worldspawn))?;
        }
    } else {
        // The step down didn't end up on good ground, so use the move without the step up.
        // This happens near wall/slope combinations.
        debug!("Tried to step but could not");
        ctx.set_origin(ent_id, pre_step_origin)?;
        ctx.set_velocity(ent_id, pre_step_velocity)?;
    }

    Ok(())
}

/// Nudges a walking entity which made no progress around a little, in case it is snagged on
/// a corner. Returns the result of the first nudge which got it moving.
///
/// Equivalent to `SV_TryUnstick`.
pub fn try_unstick<C: PlayerCtx + ?Sized>(
    ctx: &mut C,
    ent_id: Entity,
    old_velocity: Vec3,
) -> Result<CollisionResult<Option<Trace>>, C::Error> {
    const NUDGES: [Vec2; 8] = [
        Vec2::new(2., 0.),
        Vec2::new(0., 2.),
        Vec2::new(-2., 0.),
        Vec2::new(0., -2.),
        Vec2::new(2., 2.),
        Vec2::new(-2., 2.),
        Vec2::new(2., -2.),
        Vec2::new(-2., -2.),
    ];

    let old_origin = ctx.entity(ent_id)?.origin;

    for nudge in NUDGES {
        push_entity(ctx, ent_id, nudge.extend(0.))?;
        if !ctx.exists(ent_id) {
            return Ok(CollisionResult::default());
        }

        ctx.set_velocity(ent_id, old_velocity.with_z(0.))?;
        let clip = fly_move(ctx, ent_id, Duration::from_millis(100))?;
        if !ctx.exists(ent_id) {
            return Ok(clip);
        }

        let moved = (ctx.entity(ent_id)?.origin - old_origin).xy().abs();
        if moved.x > 4. || moved.y > 4. {
            return Ok(clip);
        }

        // Go back to the start.
        ctx.set_origin(ent_id, old_origin)?;
    }

    // Still not moving.
    ctx.set_velocity(ent_id, Vec3::ZERO)?;
    Ok(CollisionResult { floor: true, wall_or_step: true, dead_stop: true, metadata: None })
}

/// If the entity starts the frame inside a solid, moves it back to its last good position
/// (`oldorigin`) or to a nearby free spot.
///
/// Equivalent to `SV_CheckStuck`.
pub fn check_stuck<C: PlayerCtx + ?Sized>(ctx: &mut C, ent_id: Entity) -> Result<(), C::Error> {
    if !ctx.is_stuck(ent_id)? {
        let origin = ctx.entity(ent_id)?.origin;
        ctx.set_old_origin(ent_id, origin)?;
        return Ok(());
    }

    let origin = ctx.entity(ent_id)?.origin;
    let old_origin = ctx.old_origin(ent_id)?;

    ctx.set_origin(ent_id, old_origin)?;
    if !ctx.is_stuck(ent_id)? {
        debug!("Unstuck {ent_id} by moving it back to its old origin");
        ctx.link(ent_id, true)?;
        return Ok(());
    }

    for z in 0..18 {
        for x in -1..=1 {
            for y in -1..=1 {
                let nudge = Vec3::new(x as f32, y as f32, z as f32);
                ctx.set_origin(ent_id, origin + nudge)?;
                if !ctx.is_stuck(ent_id)? {
                    debug!("Unstuck {ent_id} by nudging it {nudge}");
                    ctx.link(ent_id, true)?;
                    return Ok(());
                }
            }
        }
    }

    ctx.set_origin(ent_id, origin)?;
    debug!("{ent_id} is stuck");

    Ok(())
}

/// Updates an entity's `waterlevel` and `watertype`.
///
/// Returns whether the entity is swimming (`waterlevel > 1`).
///
/// Equivalent to `SV_CheckWater`.
pub fn check_water<C: PlayerCtx + ?Sized>(ctx: &mut C, ent_id: Entity) -> Result<bool, C::Error> {
    let ent = ctx.entity(ent_id)?;
    let origin = ent.origin;
    let min = ent.mins;
    let max = ent.maxs;
    let view_ofs = ctx.view_ofs(ent_id)?;

    let mut water_level = 0.;
    let mut water_type = quake_contents(BspLeafContents::Empty);

    // Feet
    let contents = ctx.point_contents(origin.with_z(origin.z + min.z + 1.));
    if is_liquid(contents) {
        water_type = quake_contents(contents);
        water_level = 1.;

        // Waist
        if is_liquid(ctx.point_contents(origin.with_z(origin.z + (min.z + max.z) * 0.5))) {
            water_level = 2.;

            // Eyes
            if is_liquid(ctx.point_contents(origin.with_z(origin.z + view_ofs.z))) {
                water_level = 3.;
            }
        }
    }

    ctx.set_water(ent_id, water_level, water_type)?;

    Ok(water_level > 1.)
}

/// Applies ground friction to a player.
///
/// This should only be called when the player is on the ground and not swimming.
///
/// Equivalent to `SV_UserFriction`.
pub fn apply_friction<C: PlayerCtx + ?Sized>(
    ctx: &mut C,
    ent_id: Entity,
    frame_time: Duration,
    player_vars: &PlayerVars,
) -> Result<(), C::Error> {
    /// How far ahead of the player (in the direction of movement) to look for a dropoff.
    const DROPOFF_DETECT_DIST: f32 = 16.;
    /// How far below the player's feet the ground must be for it to count as a dropoff.
    const DROPOFF_TRACE_Z: f32 = 34.;

    let ent = ctx.entity(ent_id)?;

    let velocity = ctx.velocity(ent_id)?;
    let speed = velocity.xy().length();

    if speed == 0. {
        return Ok(());
    }

    // If the leading edge is over a dropoff, increase friction.
    let origin = ent.origin;
    let min = ent.mins;
    let start =
        (origin.xy() + velocity.xy() / speed * DROPOFF_DETECT_DIST).extend(origin.z + min.z);
    let end = start.with_z(start.z - DROPOFF_TRACE_Z);

    let (trace, _) =
        ctx.trace(ent_id, start, Vec3::ZERO, Vec3::ZERO, end, CollideKind::NoMonsters)?;

    let friction = if trace.is_terminal() {
        player_vars.ground_friction * player_vars.edge_friction
    } else {
        player_vars.ground_friction
    };

    let scale =
        friction_scale(speed, friction, player_vars.stop_speed, duration_to_f32(frame_time));

    ctx.set_velocity(ent_id, velocity * scale)
}
