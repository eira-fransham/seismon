//! Monster movement (Quake's `sv_move.c`) and dropping entities to the floor.
//!
//! Each VM implements [`MoveCtx`] over its own entities and collision, so the server's monsters
//! and CSQC's entities move with the same code.

use std::ops::{Bound, RangeBounds as _};

use bevy::{prelude::*, shape::Aabb3d};
use itertools::Itertools as _;

use super::{EntityFlags, hull::BspLeafContents, phys::CollideKind, phys::Trace};

/// What movement needs to know about an entity.
#[derive(Debug, Clone, Copy)]
pub struct MoveEntity {
    pub origin: Vec3,
    pub angles: Vec3,
    pub mins: Vec3,
    pub maxs: Vec3,
    pub absmin: Vec3,
    pub absmax: Vec3,
    pub flags: EntityFlags,
}

/// Access to a QuakeC VM's entities and world, for movement.
pub trait MoveCtx {
    type Error;

    fn entity(&self, entity: Entity) -> Result<MoveEntity, Self::Error>;
    fn set_origin(&mut self, entity: Entity, origin: Vec3) -> Result<(), Self::Error>;
    /// Sets `.angles_y`.
    fn set_yaw(&mut self, entity: Entity, yaw: f32) -> Result<(), Self::Error>;
    fn add_flags(&mut self, entity: Entity, flags: EntityFlags) -> Result<(), Self::Error>;
    fn remove_flags(&mut self, entity: Entity, flags: EntityFlags) -> Result<(), Self::Error>;
    fn set_ground_entity(&mut self, entity: Entity, ground: Entity) -> Result<(), Self::Error>;
    fn ideal_yaw(&self, entity: Entity) -> Result<f32, Self::Error>;
    fn set_ideal_yaw(&mut self, entity: Entity, yaw: f32) -> Result<(), Self::Error>;
    fn yaw_speed(&self, entity: Entity) -> Result<f32, Self::Error>;
    fn enemy(&self, entity: Entity) -> Result<Entity, Self::Error>;
    fn goal_entity(&self, entity: Entity) -> Result<Entity, Self::Error>;

    /// Traces `entity` moving from `start` to `end` (see
    /// [`Collision::trace_entity_move`](super::collision::Collision::trace_entity_move)).
    fn trace(
        &self,
        entity: Entity,
        start: Vec3,
        min: Vec3,
        max: Vec3,
        end: Vec3,
        kind: CollideKind,
    ) -> Result<(Trace, Option<Entity>), Self::Error>;

    /// The contents of the world at `point`, with currents as they are.
    fn point_contents_raw(&self, point: Vec3) -> BspLeafContents;

    /// The contents of the world at `point`, with currents as water.
    fn point_contents(&self, point: Vec3) -> BspLeafContents {
        match self.point_contents_raw(point) {
            c if c as i32 >= BspLeafContents::Current0 as i32 => BspLeafContents::Water,
            c => c,
        }
    }

    /// Links `entity` into the area grid, running the touch functions of the triggers it
    /// touches if `touch_triggers`.
    fn link(&mut self, entity: Entity, touch_triggers: bool) -> Result<(), Self::Error>;

    /// The VM's worldspawn.
    fn world(&self) -> Entity;

    /// The highest step that monsters can climb (`sv_stepheight`).
    fn max_step(&self) -> Result<f32, Self::Error>;

    /// A random choice with probability `numerator / denominator`.
    fn random_ratio(&mut self, numerator: u32, denominator: u32) -> bool;

    /// A random choice with probability `p`.
    fn random_bool(&mut self, p: f64) -> bool;
}

/// How far `drop_to_floor` looks for the floor.
pub const DROP_TO_FLOOR_DIST: f32 = 256.;

use seismon_utils::angle_mod;

/// Return value of [`chase_dirs`].
///
/// The directions a monster can try to move in to chase its goal
#[derive(Debug, PartialEq)]
pub(crate) struct ChaseDirs {
    /// The direction along the x axis, if the goal is far enough away along it.
    pub x: Option<f32>,
    /// The direction along the y axis, if the goal is far enough away along it.
    pub y: Option<f32>,
    /// The diagonal direction, if the goal is far enough away along both axes.
    pub diagonal: Option<f32>,
}

/// Returns the directions to try for a monster chasing a goal `(delta_x, delta_y)` away.
///
/// Part of the implementation equivalent to `SV_NewChaseDir`.
pub(crate) fn chase_dirs(delta_x: f32, delta_y: f32) -> ChaseDirs {
    let x = if delta_x > 10. {
        Some(0.)
    } else if delta_x < -10. {
        Some(180.)
    } else {
        None
    };

    let y = if delta_y < -10. {
        Some(270.)
    } else if delta_y > 10. {
        Some(90.)
    } else {
        None
    };

    // Quake uses 215 rather than 225 here, so we do too.
    let diagonal = match (x, y) {
        (Some(0.), Some(y)) => Some(if y == 90. { 45. } else { 315. }),
        (Some(_), Some(y)) => Some(if y == 90. { 135. } else { 215. }),
        _ => None,
    };

    ChaseDirs { x, y, diagonal }
}

/// `PF_changeyaw`: turns `ent_id` towards its `ideal_yaw`, by up to its `yaw_speed`.
pub fn change_yaw<C: MoveCtx + ?Sized>(ctx: &mut C, ent_id: Entity) -> Result<(), C::Error> {
    let cur = angle_mod(ctx.entity(ent_id)?.angles.y);
    let ideal = ctx.ideal_yaw(ent_id)?;
    let speed = ctx.yaw_speed(ent_id)?;

    if (cur - ideal).abs() < f32::EPSILON {
        return Ok(());
    }

    // TODO: This is just copied directly from the Quake source, but can definitely be
    //       done more efficiently.
    let angle_delta = ideal - cur;

    let angle_delta = if ideal > cur {
        if angle_delta >= 180. { angle_delta - 360. } else { angle_delta }
    } else if angle_delta <= -180. {
        angle_delta + 360.
    } else {
        angle_delta
    };

    // Unlike `f32::clamp`, this doesn't panic if `speed` is negative or NaN.
    let angle_delta = if angle_delta > 0. {
        if angle_delta > speed { speed } else { angle_delta }
    } else if angle_delta < -speed {
        -speed
    } else {
        angle_delta
    };

    ctx.set_yaw(ent_id, angle_mod(cur + angle_delta))
}

/// Whether the bounding boxes of `a` and `b` are within `dist` of each other (`SV_CloseEnough`).
pub fn close_enough<C: MoveCtx + ?Sized>(
    ctx: &C,
    a: Entity,
    b: Entity,
    dist: f32,
) -> Result<bool, C::Error> {
    let bounds = |e: Entity| -> Result<Aabb3d, C::Error> {
        let ent = ctx.entity(e)?;
        Ok(Aabb3d { min: ent.absmin.into(), max: ent.absmax.into() })
    };
    let (a, b) = (bounds(a)?, bounds(b)?);

    Ok(a.grow(Vec3A::splat(dist)).intersects(&b))
}

/// `SV_CheckBottom`: whether `ent_id` is standing on solid ground everywhere under it (or close
/// enough to it).
pub fn check_bottom<C: MoveCtx + ?Sized>(ctx: &C, ent_id: Entity) -> Result<bool, C::Error> {
    let max_step = ctx.max_step()?;
    let ent = ctx.entity(ent_id)?;

    let origin = ent.origin;
    let min = origin + ent.mins;
    let max = origin + ent.maxs;

    let start_z = min.z - 1.;
    let mut to_check = [max.x, min.x]
        .into_iter()
        .cartesian_product([max.y, min.y])
        .map(|(x, y)| Vec3::new(x, y, start_z));

    let all_solid = to_check.all(|start| ctx.point_contents_raw(start) == BspLeafContents::Solid);

    if all_solid {
        return Ok(true);
    }

    let start_z = min.z;

    let start = Vec3::new((min.x + max.x) / 2., (min.y + max.y) / 2., start_z);
    let stop = start - Vec3::Z * max_step * 2.;

    let (trace, _) =
        ctx.trace(ent_id, start, Vec3::ZERO, Vec3::ZERO, stop, CollideKind::NoMonsters)?;

    if trace.is_terminal() {
        return Ok(false);
    }

    let mid = trace.end_point().z;
    let mut bottom = mid;

    let mut to_check = [max.x, min.x]
        .into_iter()
        .cartesian_product([max.y, min.y])
        .map(|(x, y)| Vec3::new(x, y, start_z));

    Ok(to_check.all(|start| {
        let Ok((trace, _)) = ctx.trace(
            ent_id,
            start,
            Vec3::ZERO,
            Vec3::ZERO,
            start - Vec3::Z * max_step * 2.,
            CollideKind::NoMonsters,
        ) else {
            return false;
        };

        if !trace.is_terminal() {
            bottom = bottom.max(trace.end_point().z);

            mid - trace.end_point().z <= max_step
        } else {
            false
        }
    }))
}

fn move_step_fly<C: MoveCtx + ?Sized>(
    ctx: &mut C,
    ent_id: Entity,
    move_dir: Vec3,
    relink: bool,
) -> Result<bool, C::Error> {
    let ent = ctx.entity(ent_id)?;
    let ent_flags = ent.flags;

    let min = ent.mins;
    let max = ent.maxs;

    // Try move with/without vertical motion
    for i in 0..2 {
        let old_origin = ctx.entity(ent_id)?.origin;
        let mut new_origin = old_origin + move_dir;

        let enemy_id = ctx.enemy(ent_id)?;
        let has_enemy = enemy_id != ctx.world();

        if i == 0 && has_enemy {
            // Try to stay 30-40 units above the enemy.
            match old_origin.z - ctx.entity(enemy_id)?.origin.z {
                40f32.. => new_origin.z -= 8.,
                ..30f32 => new_origin.z += 8.,
                _ => {}
            }
        }

        let (trace, _) =
            ctx.trace(ent_id, old_origin, min, max, new_origin, CollideKind::default())?;

        if trace.is_terminal() {
            if ent_flags.contains(EntityFlags::SWIM)
                && ctx.point_contents(trace.end_point()) == BspLeafContents::Empty
            {
                // Swimming monster left water
                return Ok(false);
            }

            debug!("{} -> {}", old_origin, trace.end_point());
            ctx.set_origin(ent_id, trace.end_point())?;

            if relink {
                ctx.link(ent_id, true)?;
            }

            return Ok(true);
        }

        // TODO: This can be cleaned up
        if !has_enemy {
            break;
        }
    }

    Ok(false)
}

/// `SV_movestep`: tries to move `ent_id` by `move_dir`, stepping up and down stairs.
pub fn move_step<C: MoveCtx + ?Sized>(
    ctx: &mut C,
    ent_id: Entity,
    move_dir: Vec3,
    relink: bool,
) -> Result<bool, C::Error> {
    let max_step = ctx.max_step()?;

    let ent = ctx.entity(ent_id)?;
    let ent_flags = ent.flags;

    let min = ent.mins;
    let max = ent.maxs;
    let old_origin = ent.origin;

    if ent_flags.intersects(EntityFlags::SWIM | EntityFlags::FLY) {
        return move_step_fly(ctx, ent_id, move_dir, relink);
    }

    let new_origin = old_origin + move_dir;

    let step = Vec3::Z * max_step;

    let end = new_origin - step;

    let (mut trace, mut ground_ent) =
        ctx.trace(ent_id, new_origin + step, min, max, end, CollideKind::default())?;

    if trace.all_solid() {
        debug!("Step trace failed: {ground_ent:?}");
        return Ok(false);
    } else if trace.start_solid() {
        let (temp_trace, temp_ground_ent) =
            ctx.trace(ent_id, new_origin, min, max, end, CollideKind::default())?;

        if temp_trace.all_solid() || temp_trace.start_solid() {
            debug!("Non-step trace failed");
            return Ok(false);
        }

        trace = temp_trace;
        ground_ent = temp_ground_ent;
    } else if trace.is_terminal() {
        // The entity had the ground move out from beneath it.
        return Ok(if ent_flags.contains(EntityFlags::PARTIAL_GROUND) {
            debug!("{} -> {}", old_origin, new_origin);
            ctx.set_origin(ent_id, new_origin)?;
            ctx.remove_flags(ent_id, EntityFlags::ON_GROUND)?;

            if relink {
                ctx.link(ent_id, true)?;
            }

            debug!("Entity had ground moved out from underneath");
            true
        } else {
            debug!("Entity walked off edge(?)");
            false
        });
    }

    debug!("{} -> {}", old_origin, trace.end_point());
    ctx.set_origin(ent_id, trace.end_point())?;

    if !check_bottom(ctx, ent_id)? {
        return Ok(if ctx.entity(ent_id)?.flags.contains(EntityFlags::PARTIAL_GROUND) {
            if relink {
                ctx.link(ent_id, true)?;
            }

            true
        } else {
            debug!("Resetting pos bc entity walked off edge(?)");
            ctx.set_origin(ent_id, old_origin)?;
            false
        });
    }

    let worldspawn = ctx.world();
    ctx.remove_flags(ent_id, EntityFlags::PARTIAL_GROUND)?;

    // `HEAD` set this to entity -1 if nothing was hit, which QuakeC sees as world.
    ctx.set_ground_entity(ent_id, ground_ent.unwrap_or(worldspawn))?;

    if relink {
        ctx.link(ent_id, true)?;
    }

    Ok(true)
}

/// `SV_StepDirection`: turns `ent_id` towards `yaw` and tries to move `dist` that way.
pub fn step_direction<C: MoveCtx + ?Sized>(
    ctx: &mut C,
    ent_id: Entity,
    yaw: f32,
    dist: f32,
) -> Result<bool, C::Error> {
    let old_origin = ctx.entity(ent_id)?.origin;
    ctx.set_ideal_yaw(ent_id, yaw)?;
    change_yaw(ctx, ent_id)?;

    let move_dir = Vec2::from_angle(yaw.to_radians()).extend(0.) * dist;
    let did_move = move_step(ctx, ent_id, move_dir, false)?;

    if did_move {
        // TODO: Quake doesn't do `anglemod` here, is it necessary?
        let delta = ctx.entity(ent_id)?.angles.y - ctx.ideal_yaw(ent_id)?;

        if (Bound::Excluded(45f32), Bound::Excluded(315f32)).contains(&delta) {
            debug!("Cancelling move due to angle delta");
            // Quake says that this is when the monster "didn't turn far enough" but it appears
            // to be the opposite - cancelling if it turned _too_ far.
            ctx.set_origin(ent_id, old_origin)?;
        }
    }

    ctx.link(ent_id, true)?;

    Ok(did_move)
}

/// `SV_NewChaseDir`: picks a new direction for `this_id` to chase `goal_id` in.
pub fn new_chase_dir<C: MoveCtx + ?Sized>(
    ctx: &mut C,
    this_id: Entity,
    goal_id: Entity,
    dist: f32,
) -> Result<(), C::Error> {
    let old_dir = angle_mod((ctx.ideal_yaw(this_id)? / 45.).trunc() * 45.);
    let turn_around = angle_mod(old_dir - 180.);

    let this_origin = ctx.entity(this_id)?.origin;

    let goal_origin = ctx.entity(goal_id)?.origin;

    let delta_x = goal_origin.x - this_origin.x;
    let delta_y = goal_origin.y - this_origin.y;

    let ChaseDirs { x: dir_a, y: dir_b, diagonal: turn_dir } = chase_dirs(delta_x, delta_y);

    // Try the direct route.
    if let Some(turn_dir) = turn_dir
        && (turn_dir - turn_around).abs() > f32::EPSILON
        && step_direction(ctx, this_id, turn_dir, dist)?
    {
        return Ok(());
    }

    // Try the other directions. Quake compares the deltas as integers rather than floats.
    let (dir_a, dir_b) = if ctx.random_bool(0.5) || (delta_y as i32).abs() > (delta_x as i32).abs()
    {
        (dir_b, dir_a)
    } else {
        (dir_a, dir_b)
    };

    if let Some(dir_a) = dir_a
        && (dir_a - turn_around).abs() > f32::EPSILON
        && step_direction(ctx, this_id, dir_a, dist)?
    {
        return Ok(());
    }

    if let Some(dir_b) = dir_b
        && (dir_b - turn_around).abs() > f32::EPSILON
        && step_direction(ctx, this_id, dir_b, dist)?
    {
        return Ok(());
    }

    // There is no direct path to the goal, so try the old direction.
    if step_direction(ctx, this_id, old_dir, dist)? {
        return Ok(());
    }

    const SEARCH_DIRS: [f32; 8] = [0., 45., 90., 135., 180., 225., 270., 315.];
    const SEARCH_DIRS_REV: [f32; 8] = [315., 270., 225., 180., 135., 90., 45., 0.];

    let search_dirs = if ctx.random_bool(0.5) { SEARCH_DIRS } else { SEARCH_DIRS_REV };

    for turn_dir in search_dirs {
        if (turn_dir - turn_around).abs() > f32::EPSILON
            && step_direction(ctx, this_id, turn_dir, dist)?
        {
            return Ok(());
        }
    }

    if step_direction(ctx, this_id, turn_around, dist)? {
        return Ok(());
    }

    ctx.set_ideal_yaw(this_id, old_dir)?;

    if !check_bottom(ctx, this_id)? {
        ctx.add_flags(this_id, EntityFlags::PARTIAL_GROUND)?;
    }

    Ok(())
}

/// `SV_MoveToGoal`: moves `this_id` `dist` towards its `goalentity`. Returns whether it moved.
pub fn move_to_goal<C: MoveCtx + ?Sized>(
    ctx: &mut C,
    this_id: Entity,
    dist: f32,
) -> Result<bool, C::Error> {
    let goal_id = ctx.goal_entity(this_id)?;

    let flags = ctx.entity(this_id)?.flags;

    if !flags.intersects(EntityFlags::ON_GROUND | EntityFlags::FLY | EntityFlags::SWIM) {
        return Ok(false);
    }

    // If the next step hits the enemy, return immediately.
    let enemy = ctx.enemy(this_id)?;
    if enemy != ctx.world() && close_enough(ctx, this_id, goal_id, dist)? {
        return Ok(true);
    }

    // Try a few random directions to try to get unstuck.
    let ideal_yaw = ctx.ideal_yaw(this_id)?;
    let did_move = !ctx.random_ratio(1, 4) && step_direction(ctx, this_id, ideal_yaw, dist)?;

    if !did_move {
        new_chase_dir(ctx, this_id, goal_id, dist)?;
    }

    Ok(did_move)
}

/// `PF_walkmove`: moves `this_id` `dist` in the direction `yaw`, if it's on the ground (or flies
/// or swims). Returns whether it moved.
pub fn walk_move<C: MoveCtx + ?Sized>(
    ctx: &mut C,
    this_id: Entity,
    yaw: f32,
    dist: f32,
) -> Result<bool, C::Error> {
    let ent_flags = ctx.entity(this_id)?.flags;

    if !ent_flags.intersects(EntityFlags::ON_GROUND | EntityFlags::SWIM | EntityFlags::FLY) {
        return Ok(false);
    }

    let move_dir = Vec2::from_angle(yaw.to_radians()).extend(0.) * dist;

    move_step(ctx, this_id, move_dir, true)
}

/// `PF_droptofloor`: moves `ent_id` down onto the floor, if there is one within
/// [`DROP_TO_FLOOR_DIST`].
pub fn drop_to_floor<C: MoveCtx + ?Sized>(ctx: &mut C, ent_id: Entity) -> Result<bool, C::Error> {
    debug!("Finding floor for entity {ent_id}");
    let ent = ctx.entity(ent_id)?;
    let origin = ent.origin;

    let end = Vec3::new(origin.x, origin.y, origin.z - DROP_TO_FLOOR_DIST);
    let min = ent.mins;
    let max = ent.maxs;

    let (trace, collide_entity) = ctx.trace(ent_id, origin, min, max, end, CollideKind::Normal)?;
    debug!("End position after drop: {:?}", trace.end_point());

    if trace.is_terminal() || trace.all_solid() {
        // Entity didn't hit the floor or is stuck.
        return Ok(false);
    }

    // Entity hit the floor. Update origin, relink and set ON_GROUND flag.
    ctx.set_origin(ent_id, trace.end_point())?;
    ctx.link(ent_id, false)?;
    let worldspawn = ctx.world();
    ctx.add_flags(ent_id, EntityFlags::ON_GROUND)?;
    ctx.set_ground_entity(ent_id, collide_entity.unwrap_or(worldspawn))?;

    Ok(true)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn chase_dirs_match_quake() {
        assert_eq!(chase_dirs(100., 0.), ChaseDirs { x: Some(0.), y: None, diagonal: None });
        assert_eq!(chase_dirs(-100., 0.), ChaseDirs { x: Some(180.), y: None, diagonal: None });
        // Positive y is a yaw of 90.
        assert_eq!(chase_dirs(0., 100.), ChaseDirs { x: None, y: Some(90.), diagonal: None });
        assert_eq!(chase_dirs(0., -100.), ChaseDirs { x: None, y: Some(270.), diagonal: None });
        // Within 10 units isn't far enough.
        assert_eq!(chase_dirs(10., -10.), ChaseDirs { x: None, y: None, diagonal: None });

        assert_eq!(chase_dirs(100., 100.).diagonal, Some(45.));
        assert_eq!(chase_dirs(100., -100.).diagonal, Some(315.));
        assert_eq!(chase_dirs(-100., 100.).diagonal, Some(135.));
        assert_eq!(chase_dirs(-100., -100.).diagonal, Some(215.));

        // The diagonal points the same way as the delta (apart from Quake's 215).
        for (dx, dy) in [(100., 100.), (100., -100.), (-100., 100.)] {
            let yaw = chase_dirs(dx, dy).diagonal.unwrap();
            let dir = Vec2::from_angle(yaw.to_radians());
            assert!(dir.dot(Vec2::new(dx, dy).normalize()) > 0.99, "{dx} {dy} -> {yaw}");
        }
    }
}
