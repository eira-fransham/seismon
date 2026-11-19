//! Monster movement, ported from `sv_move.c`.

use std::ops::{Bound, RangeBounds as _};

use bevy::prelude::*;
use itertools::Itertools as _;

use crate::server::{
    ServerCtx, ServerVars, angle_mod,
    error::ServerError,
    world::{EntityFlags, hull::BspLeafContents, phys::CollideKind},
};

/// Return value of [`chase_dirs`].
///
/// The directions a monster can try to move in to chase its goal
#[derive(Debug, PartialEq)]
pub(super) struct ChaseDirs {
    /// The direction along the x axis, if the goal is far enough away along it.
    x: Option<f32>,
    /// The direction along the y axis, if the goal is far enough away along it.
    y: Option<f32>,
    /// The diagonal direction, if the goal is far enough away along both axes.
    diagonal: Option<f32>,
}

/// Returns the directions to try for a monster chasing a goal `(delta_x, delta_y)` away.
///
/// Part of the implementation equivalent to `SV_NewChaseDir`.
fn chase_dirs(delta_x: f32, delta_y: f32) -> ChaseDirs {
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

impl ServerCtx<'_> {
    pub fn change_yaw(&mut self, ent_id: Entity) -> Result<(), ServerError> {
        let cur = angle_mod(self.phys(ent_id)?.angles.y);
        let ai = self.ai(ent_id)?;
        let ideal = ai.ideal_yaw;
        let speed = ai.yaw_speed;

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

        self.phys_mut(ent_id)?.angles.y = angle_mod(cur + angle_delta);

        Ok(())
    }

    pub fn close_enough(&self, a: Entity, b: Entity, dist: f32) -> Result<bool, ServerError> {
        let a = self.phys(a)?.abs_bounding_box();
        let b = self.phys(b)?.abs_bounding_box();

        Ok(a.grow(Vec3A::splat(dist)).intersects(&b))
    }

    pub fn check_bottom(
        &self,
        ent_id: Entity,
        server_vars: &ServerVars,
    ) -> Result<bool, ServerError> {
        let ent = self.phys(ent_id)?;

        let origin = ent.origin;
        let min = origin + ent.mins;
        let max = origin + ent.maxs;

        let start_z = min.z - 1.;
        let mut to_check = [max.x, min.x]
            .into_iter()
            .cartesian_product([max.y, min.y])
            .map(|(x, y)| Vec3::new(x, y, start_z));

        let all_solid =
            to_check.all(|start| self.point_contents_raw(start) == BspLeafContents::Solid);

        if all_solid {
            return Ok(true);
        }

        let start_z = min.z;

        let start = Vec3::new((min.x + max.x) / 2., (min.y + max.y) / 2., start_z);
        let stop = start - Vec3::Z * server_vars.max_step * 2.;

        let (trace, _) = self.trace_entity_move(
            ent_id,
            start,
            Vec3::ZERO,
            Vec3::ZERO,
            stop,
            CollideKind::NoMonsters,
        )?;

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
            let Ok((trace, _)) = self.trace_entity_move(
                ent_id,
                start,
                Vec3::ZERO,
                Vec3::ZERO,
                start - Vec3::Z * server_vars.max_step * 2.,
                CollideKind::NoMonsters,
            ) else {
                return false;
            };

            if !trace.is_terminal() {
                bottom = bottom.max(trace.end_point().z);

                mid - trace.end_point().z <= server_vars.max_step
            } else {
                false
            }
        }))
    }

    fn move_step_fly(
        &mut self,
        ent_id: Entity,
        move_dir: Vec3,
        relink: bool,
    ) -> Result<bool, ServerError> {
        let ent = self.phys(ent_id)?;
        let ent_flags = ent.flags();

        let min = ent.mins;
        let max = ent.maxs;

        // Try move with/without vertical motion
        for i in 0..2 {
            let old_origin = self.phys(ent_id)?.origin;
            let mut new_origin = old_origin + move_dir;

            let enemy_id = self.ai(ent_id)?.enemy;
            let has_enemy = !self.is_world(enemy_id);

            if i == 0 && has_enemy {
                // Try to stay 30-40 units above the enemy.
                match old_origin.z - self.phys(enemy_id)?.origin.z {
                    40f32.. => new_origin.z -= 8.,
                    ..30f32 => new_origin.z += 8.,
                    _ => {}
                }
            }

            let (trace, _) = self.trace_entity_move(
                ent_id,
                old_origin,
                min,
                max,
                new_origin,
                CollideKind::default(),
            )?;

            if trace.is_terminal() {
                if ent_flags.contains(EntityFlags::SWIM)
                    && self.point_contents(trace.end_point()) == BspLeafContents::Empty
                {
                    // Swimming monster left water
                    return Ok(false);
                }

                debug!("{} -> {}", old_origin, trace.end_point());
                self.phys_mut(ent_id)?.set_origin(trace.end_point());

                if relink {
                    self.link_entity(ent_id, true)?;
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

    pub fn move_step(
        &mut self,
        ent_id: Entity,
        move_dir: Vec3,
        relink: bool,
    ) -> Result<bool, ServerError> {
        let server_vars = self.server_vars()?;

        let ent = self.phys(ent_id)?;
        let ent_flags = ent.flags();

        let min = ent.mins;
        let max = ent.maxs;
        let old_origin = ent.origin;

        if ent_flags.intersects(EntityFlags::SWIM | EntityFlags::FLY) {
            return self.move_step_fly(ent_id, move_dir, relink);
        }

        let new_origin = old_origin + move_dir;

        let step = Vec3::Z * server_vars.max_step;

        let end = new_origin - step;

        let (mut trace, mut ground_ent) = self.trace_entity_move(
            ent_id,
            new_origin + step,
            min,
            max,
            end,
            CollideKind::default(),
        )?;

        if trace.all_solid() {
            debug!("Step trace failed: {ground_ent:?}");
            return Ok(false);
        } else if trace.start_solid() {
            let (temp_trace, temp_ground_ent) =
                self.trace_entity_move(ent_id, new_origin, min, max, end, CollideKind::default())?;

            if temp_trace.all_solid() || temp_trace.start_solid() {
                debug!("Non-step trace failed");
                return Ok(false);
            }

            trace = temp_trace;
            ground_ent = temp_ground_ent;
        } else if trace.is_terminal() {
            // The entity had the ground move out from beneath it.
            return Ok(if ent_flags.contains(EntityFlags::PARTIAL_GROUND) {
                let mut ent = self.phys_mut(ent_id)?;
                debug!("{} -> {}", old_origin, new_origin);
                ent.set_origin(new_origin);

                ent.remove_flags(EntityFlags::ON_GROUND);

                if relink {
                    self.link_entity(ent_id, true)?;
                }

                debug!("Entity had ground moved out from underneath");
                true
            } else {
                debug!("Entity walked off edge(?)");
                false
            });
        }

        debug!("{} -> {}", old_origin, trace.end_point());
        self.phys_mut(ent_id)?.set_origin(trace.end_point());

        if !self.check_bottom(ent_id, &server_vars)? {
            return Ok(if self.phys(ent_id)?.has_flag(EntityFlags::PARTIAL_GROUND) {
                if relink {
                    self.link_entity(ent_id, true)?;
                }

                true
            } else {
                debug!("Resetting pos bc entity walked off edge(?)");
                self.phys_mut(ent_id)?.set_origin(old_origin);
                false
            });
        }

        let worldspawn = self.worldspawn();
        let mut ent = self.phys_mut(ent_id)?;
        ent.remove_flags(EntityFlags::PARTIAL_GROUND);

        // `HEAD` set this to entity -1 if nothing was hit, which QuakeC sees as world.
        ent.groundentity = ground_ent.unwrap_or(worldspawn);

        if relink {
            self.link_entity(ent_id, true)?;
        }

        Ok(true)
    }

    pub fn step_direction(
        &mut self,
        ent_id: Entity,
        yaw: f32,
        dist: f32,
    ) -> Result<bool, ServerError> {
        let old_origin = self.phys(ent_id)?.origin;
        self.ai_mut(ent_id)?.ideal_yaw = yaw;
        self.change_yaw(ent_id)?;

        let move_dir = Vec2::from_angle(yaw.to_radians()).extend(0.) * dist;
        let did_move = self.move_step(ent_id, move_dir, false)?;

        if did_move {
            // TODO: Quake doesn't do `anglemod` here, is it necessary?
            let delta = self.phys(ent_id)?.angles.y - self.ai(ent_id)?.ideal_yaw;

            if (Bound::Excluded(45f32), Bound::Excluded(315f32)).contains(&delta) {
                debug!("Cancelling move due to angle delta");
                // Quake says that this is when the monster "didn't turn far enough" but it appears
                // to be the opposite - cancelling if it turned _too_ far.
                self.phys_mut(ent_id)?.set_origin(old_origin);
            }
        }

        self.link_entity(ent_id, true)?;

        Ok(did_move)
    }

    pub fn new_chase_dir(
        &mut self,
        this_id: Entity,
        goal_id: Entity,
        dist: f32,
    ) -> Result<(), ServerError> {
        let server_vars = self.server_vars()?;

        let old_dir = angle_mod((self.ai(this_id)?.ideal_yaw / 45.).trunc() * 45.);
        let turn_around = angle_mod(old_dir - 180.);

        let this_origin = self.phys(this_id)?.origin;

        let goal_origin = self.phys(goal_id)?.origin;

        let delta_x = goal_origin.x - this_origin.x;
        let delta_y = goal_origin.y - this_origin.y;

        let ChaseDirs { x: dir_a, y: dir_b, diagonal: turn_dir } = chase_dirs(delta_x, delta_y);

        // Try the direct route.
        if let Some(turn_dir) = turn_dir
            && (turn_dir - turn_around).abs() > f32::EPSILON
            && self.step_direction(this_id, turn_dir, dist)?
        {
            return Ok(());
        }

        // Try the other directions. Quake compares the deltas as integers rather than floats.
        let (dir_a, dir_b) =
            if self.rng().random_bool(0.5) || (delta_y as i32).abs() > (delta_x as i32).abs() {
                (dir_b, dir_a)
            } else {
                (dir_a, dir_b)
            };

        if let Some(dir_a) = dir_a
            && (dir_a - turn_around).abs() > f32::EPSILON
            && self.step_direction(this_id, dir_a, dist)?
        {
            return Ok(());
        }

        if let Some(dir_b) = dir_b
            && (dir_b - turn_around).abs() > f32::EPSILON
            && self.step_direction(this_id, dir_b, dist)?
        {
            return Ok(());
        }

        // There is no direct path to the goal, so try the old direction.
        if self.step_direction(this_id, old_dir, dist)? {
            return Ok(());
        }

        const SEARCH_DIRS: [f32; 8] = [0., 45., 90., 135., 180., 225., 270., 315.];
        const SEARCH_DIRS_REV: [f32; 8] = [315., 270., 225., 180., 135., 90., 45., 0.];

        let search_dirs = if self.rng().random_bool(0.5) { SEARCH_DIRS } else { SEARCH_DIRS_REV };

        for turn_dir in search_dirs {
            if (turn_dir - turn_around).abs() > f32::EPSILON
                && self.step_direction(this_id, turn_dir, dist)?
            {
                return Ok(());
            }
        }

        if self.step_direction(this_id, turn_around, dist)? {
            return Ok(());
        }

        self.ai_mut(this_id)?.ideal_yaw = old_dir;

        if !self.check_bottom(this_id, &server_vars)? {
            self.phys_mut(this_id)?.add_flags(EntityFlags::PARTIAL_GROUND);
        }

        Ok(())
    }
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
