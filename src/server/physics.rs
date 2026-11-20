//! Entity physics and movement, ported from `sv_phys.c`.
//!
//! Monster movement (`sv_move.c`) lives in [`super::ai`].

use std::{
    cell::RefCell,
    ops::{Bound, RangeBounds as _},
    time::Duration,
};

use arrayvec::ArrayVec;
use bevy::prelude::*;
use bevy_mod_scripting_qcvm::QCString;
use hashbrown::HashMap;
use seismon_utils::{duration_from_f32, duration_to_f32};

use crate::{
    common::{math::CollisionResult, net::ServerCmd},
    server::{
        EntCallbacks, EntPhysics, PlayerVars, ServerCtx, ServerVars, accelerate, calc_roll,
        clip_velocity,
        error::ServerError,
        friction_scale, is_liquid,
        math::{AngleVectors, angle_vectors},
        no_such_entity,
        qc::{
            self,
            fields::{QcTables, QcValue},
        },
        quake_contents,
        world::{
            EntityFlags, EntitySolid, MoveKind,
            hull::BspLeafContents,
            phys::{CollideKind, Trace},
        },
    },
};

use super::MoveCmd;

impl ServerCtx<'_> {
    /// Link an entity into the area tree.
    ///
    /// If `touch_triggers` is `true`, this will invoke the touch function of
    /// any trigger the entity is touching.
    pub fn link_entity(&mut self, ent_id: Entity, touch_triggers: bool) -> Result<(), ServerError> {
        self.link_raw(ent_id)?;

        if touch_triggers {
            self.touch_triggers(ent_id)?;
        }

        Ok(())
    }

    pub fn spawn_entity(&mut self) -> Result<Entity, ServerError> {
        let ent_id = self.alloc_uninitialized()?;

        self.link_entity(ent_id, false)?;

        Ok(ent_id)
    }

    pub fn set_entity_origin(&mut self, ent_id: Entity, origin: Vec3) -> Result<(), ServerError> {
        self.phys_mut(ent_id)?.set_origin(origin);
        self.link_entity(ent_id, false)?;

        Ok(())
    }

    /// Sets an entity's model/size.
    ///
    /// Equivalent to `PF_setmodel`.
    pub fn set_entity_model(
        &mut self,
        ent_id: Entity,
        model_name: QCString,
    ) -> Result<(), ServerError> {
        /// The server ignores the actual model data and assigns this size
        /// to all alias models (i.e. models loaded from `.mdl` files).
        const ALIAS_MODEL_HALF_SIZE: f32 = 16.;

        let model_id = self.level().model_index(&model_name).unwrap_or_else(|e| {
            warn!("{e}");
            usize::MAX
        });

        let mut render = self.render_mut(ent_id)?;
        render.model = model_name;
        render.modelindex = model_id as _;

        // Brush models (which the server has loaded) use their actual size, and the null model has
        // no size. Likewise for non-precached models.
        // TODO: Quake uses the frame size for sprites, we should actually load those.
        // TODO: Feels brittle, maybe we can clean this up.
        let no_default_collision = self.world.resource_scope(
            |world, collision: Mut<crate::server::world::LevelCollision>| {
                collision.set_entity_model(world, ent_id, model_id)
            },
        )?;
        if !no_default_collision {
            self.phys_mut(ent_id)?.set_min_max_size(
                Vec3::splat(-ALIAS_MODEL_HALF_SIZE),
                Vec3::splat(ALIAS_MODEL_HALF_SIZE),
            );
        }

        self.link_entity(ent_id, false)
    }

    /// Runs an entity's think function if it is due this frame.
    ///
    /// Like Quake (but unlike QuakeWorld), this thinks at most once per frame. Returns `false` if
    /// the entity was removed by its think function.
    ///
    /// Equivalent to `SV_RunThink`.
    // TODO: Should we implement QW's think mechanism as an option?
    pub fn think(&mut self, ent_id: Entity, frame_time: Duration) -> Result<bool, ServerError> {
        let time = self.time();
        let think_time = self.callbacks(ent_id)?.nextthink;

        if think_time <= 0. || think_time > duration_to_f32(time + frame_time) {
            // Think either already happened or isn't due yet.
            return Ok(true);
        }

        // TODO: Is this actually correct? Check quake.
        let think_time = think_time.max(duration_to_f32(time));

        // Deschedule next think.
        let mut callbacks = self.callbacks_mut(ent_id)?;
        callbacks.nextthink = 0.0;

        // Call entity's think function.
        let think = callbacks.think;
        let worldspawn = self.worldspawn();
        let mut globals = self.globals_mut();
        globals.time = think_time;
        globals.self_ = ent_id;
        globals.other = worldspawn;
        self.call(think)?;

        Ok(self.exists(ent_id))
    }

    pub fn start_frame(&mut self) -> Result<(), ServerError> {
        let worldspawn = self.worldspawn();
        let time = duration_to_f32(self.time());
        let mut globals = self.globals_mut();
        globals.self_ = worldspawn;
        globals.other = worldspawn;
        globals.world = worldspawn;
        globals.time = time;

        let start_frame = globals.start_frame;
        self.call(start_frame)?;

        Ok(())
    }

    /// Moves an entity along its velocity for `frame_time`, sliding along anything it hits.
    ///
    /// If the entity lands on a brush entity, it is marked as on the ground. The returned
    /// metadata is the last trace which hit a wall or step, if any.
    ///
    /// Equivalent to `SV_FlyMove`.
    fn physics_player_fly(
        &mut self,
        ent_id: Entity,
        frame_time: Duration,
    ) -> Result<CollisionResult<Option<Trace>>, ServerError> {
        const BUMP_COUNT: usize = 4;
        const MAX_CLIP_PLANES: usize = 5;

        let mut out = CollisionResult::default();

        let primal_velocity = self.phys(ent_id)?.velocity;
        let mut original_velocity = primal_velocity;
        // Normals of the planes the entity is touching. Unlike our traces (but like Quake), these
        // face out of the surface.
        let mut planes = ArrayVec::<Vec3, MAX_CLIP_PLANES>::new();

        let mut time_left: f32 = duration_to_f32(frame_time);

        for _ in 0..BUMP_COUNT {
            let ent = self.phys(ent_id)?;
            let velocity = ent.velocity;
            if (time_left * velocity).length_squared() < f32::EPSILON {
                break;
            }

            let origin = ent.origin;
            let min = ent.mins;
            let max = ent.maxs;

            let end = origin + time_left * velocity;

            let (trace, touched_ent_id) =
                self.trace_entity_move(ent_id, origin, min, max, end, CollideKind::Normal)?;

            if trace.all_solid() {
                // Entity is trapped in another solid.
                self.phys_mut(ent_id)?.set_velocity(Vec3::ZERO);
                return Ok(CollisionResult { floor: true, wall_or_step: true, ..out });
            }

            if trace.ratio() > 0. {
                // If ratio > 0, we moved some distance.
                let mut ent = self.phys_mut(ent_id)?;
                ent.set_origin(trace.end_point());
                original_velocity = ent.velocity;
                planes.clear();
            }

            let Some(plane) = trace.plane() else {
                // Travelled full trace distance.
                break;
            };

            let Some(hit) = touched_ent_id else {
                return Err(ServerError::with_msg(format!(
                    "fly_move: trace for {ent_id} hit a surface but no entity"
                )));
            };

            let normal = -plane.normal();

            if normal.z > 0.7 {
                out.floor = true;
                if self.phys(hit)?.solid()? == EntitySolid::Bsp {
                    let mut ent = self.phys_mut(ent_id)?;
                    ent.add_flags(EntityFlags::ON_GROUND);
                    ent.groundentity = hit;
                }
            }

            if normal.z == 0. {
                out.wall_or_step = true;
                // Saved for the player's extra wall friction.
                out.metadata = Some(trace.clone());
            }

            self.impact_entities(ent_id, hit)?;
            if !self.exists(ent_id) {
                // Removed by the impact function.
                break;
            }

            time_left -= time_left * trace.ratio();

            if planes.try_push(normal).is_err() {
                // This shouldn't really happen.
                self.phys_mut(ent_id)?.set_velocity(Vec3::ZERO);
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
                    dir * dir.dot(self.phys(ent_id)?.velocity)
                }
                None => {
                    self.phys_mut(ent_id)?.set_velocity(Vec3::ZERO);
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
                self.phys_mut(ent_id)?.set_velocity(Vec3::ZERO);
                return Ok(out);
            }

            self.phys_mut(ent_id)?.set_velocity(new_velocity);
        }

        Ok(out)
    }

    fn physics_walk(
        &mut self,
        ent_id: Entity,
        frame_time: Duration,
        server_vars: &ServerVars,
    ) -> Result<(), ServerError> {
        const ORIGIN_EPSILON: f32 = 0.03125;

        let mut ent = self.phys_mut(ent_id)?;

        let water_jump = ent.has_flag(EntityFlags::WATER_JUMP);
        let was_on_ground = ent.has_flag(EntityFlags::ON_GROUND);
        ent.remove_flags(EntityFlags::ON_GROUND);

        let old_origin = ent.origin;
        let old_velocity = ent.velocity;
        let water_level = ent.waterlevel;

        let clip = self.physics_player_fly(ent_id, frame_time)?;

        if !self.exists(ent_id) {
            return Ok(());
        }

        // Read after moving, since if we moved into e.g. a trigger that kills the entity
        // then we don't want to do anything here.
        let move_kind = self.phys(ent_id)?.move_kind()?;

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

        let mut ent = self.phys_mut(ent_id)?;

        let pre_step_origin = ent.origin;
        let pre_step_velocity = ent.velocity;

        ent.set_origin(old_origin);

        let upmove = Vec3::Z * server_vars.max_step;
        let downmove =
            Vec3::Z * (old_velocity.z * duration_to_f32(frame_time) - server_vars.max_step);

        ent.set_velocity(old_velocity.with_z(0.));

        self.push_entity(ent_id, upmove)?;

        let mut clip = self.physics_player_fly(ent_id, frame_time)?;
        // The wall hit by the stepped-up move. `SV_TryUnstick` uses its own trace, so this is
        // kept even if it replaces `clip`.
        let step_trace = clip.metadata.take();

        let post_step_origin = self.phys(ent_id)?.origin;
        let post_clip_move = (post_step_origin - old_origin).xy().abs();

        // Check for stuckness, possibly due to the limited precision of floats in the clipping
        // hulls: stepping up didn't make any progress.
        if clip.any_collision() && post_clip_move.to_array().iter().all(|v| *v < ORIGIN_EPSILON) {
            clip = self.try_unstick(ent_id, old_velocity)?;
        }

        // Extra friction based on the view angle.
        if clip.wall_or_step
            && let Some(wall) = step_trace.as_ref().and_then(Trace::plane)
        {
            let forward = angle_vectors(self.player(ent_id)?.v_angle).forward;
            let mut ent = self.phys_mut(ent_id)?;
            // Our trace planes face into the solid, but Quake's face out of it.
            let velocity = super::wall_friction(ent.velocity, forward, -wall.normal());
            ent.set_velocity(velocity);
        }

        let (downtrace, down_ent) = self.push_entity(ent_id, downmove)?;
        if downtrace.plane().map(|plane| plane.is_floor()).unwrap_or_default() {
            let worldspawn = self.worldspawn();
            let mut ent = self.phys_mut(ent_id)?;
            // This checks the moving entity rather than the ground, as Quake does. Players aren't
            // `SOLID_BSP`, so in practice `physics_player_fly` sets the ground instead.
            if ent.solid()? == EntitySolid::Bsp {
                ent.add_flags(EntityFlags::ON_GROUND);
                ent.groundentity = down_ent.unwrap_or(worldspawn);
            }
        } else {
            // The step down didn't end up on good ground, so use the move without the step up.
            // This happens near wall/slope combinations.
            debug!("Tried to step but could not");
            let mut ent = self.phys_mut(ent_id)?;

            ent.set_origin(pre_step_origin);
            ent.set_velocity(pre_step_velocity);
        }

        Ok(())
    }

    /// Nudges a walking entity which made no progress around a little, in case it is snagged on
    /// a corner. Returns the result of the first nudge which got it moving.
    ///
    /// Equivalent to `SV_TryUnstick`.
    fn try_unstick(
        &mut self,
        ent_id: Entity,
        old_velocity: Vec3,
    ) -> Result<CollisionResult<Option<Trace>>, ServerError> {
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

        let old_origin = self.phys(ent_id)?.origin;

        for nudge in NUDGES {
            self.push_entity(ent_id, nudge.extend(0.))?;
            if !self.exists(ent_id) {
                return Ok(CollisionResult::default());
            }

            self.phys_mut(ent_id)?.set_velocity(old_velocity.with_z(0.));
            let clip = self.physics_player_fly(ent_id, Duration::from_millis(100))?;
            if !self.exists(ent_id) {
                return Ok(clip);
            }

            let moved = (self.phys(ent_id)?.origin - old_origin).xy().abs();
            if moved.x > 4. || moved.y > 4. {
                return Ok(clip);
            }

            // Go back to the start.
            self.phys_mut(ent_id)?.set_origin(old_origin);
        }

        // Still not moving.
        self.phys_mut(ent_id)?.set_velocity(Vec3::ZERO);
        Ok(CollisionResult { floor: true, wall_or_step: true, dead_stop: true, metadata: None })
    }

    /// If the entity starts the frame inside a solid, moves it back to its last good position
    /// (`oldorigin`) or to a nearby free spot.
    ///
    /// Equivalent to `SV_CheckStuck`.
    fn check_stuck(&mut self, ent_id: Entity) -> Result<(), ServerError> {
        if !self.entity_is_stuck(ent_id)? {
            let mut ent = self.phys_mut(ent_id)?;
            ent.oldorigin = ent.origin;
            return Ok(());
        }

        let (origin, old_origin) = {
            let ent = self.phys(ent_id)?;
            (ent.origin, ent.oldorigin)
        };

        self.phys_mut(ent_id)?.set_origin(old_origin);
        if !self.entity_is_stuck(ent_id)? {
            debug!("Unstuck {ent_id} by moving it back to its old origin");
            self.link_entity(ent_id, true)?;
            return Ok(());
        }

        for z in 0..18 {
            for x in -1..=1 {
                for y in -1..=1 {
                    let nudge = Vec3::new(x as f32, y as f32, z as f32);
                    self.phys_mut(ent_id)?.set_origin(origin + nudge);
                    if !self.entity_is_stuck(ent_id)? {
                        debug!("Unstuck {ent_id} by nudging it {nudge}");
                        self.link_entity(ent_id, true)?;
                        return Ok(());
                    }
                }
            }
        }

        self.phys_mut(ent_id)?.set_origin(origin);
        debug!("{ent_id} is stuck");

        Ok(())
    }

    fn ent_physics(&mut self, ent_id: Entity, frame_time: Duration) -> Result<(), ServerError> {
        let server_vars = self.server_vars()?;

        match self.phys(ent_id)?.move_kind()? {
            MoveKind::Walk => self.physics_walk(ent_id, frame_time, &server_vars)?,

            MoveKind::Push => self.physics_push(ent_id, frame_time)?,
            MoveKind::NoClip => self.physics_noclip(ent_id, frame_time)?,
            MoveKind::Step => self.physics_step(ent_id, frame_time)?,
            MoveKind::Fly | MoveKind::FlyMissile | MoveKind::Bounce | MoveKind::Toss => {
                self.physics_toss(ent_id, frame_time)?
            }
            // No actual physics for this entity, but still let it think.
            MoveKind::None => {
                self.think(ent_id, frame_time)?;
            }

            MoveKind::AngleNoClip | MoveKind::AngleClip => {
                warn!("TODO: Fly physics");
                self.think(ent_id, frame_time)?;
            }
        }

        Ok(())
    }

    /// Applies gravity to the entity.
    ///
    /// The effect depends on the provided value of the `sv_gravity` cvar, the
    /// amount of time being simulated, and the entity's own `gravity` field
    /// value.
    fn apply_gravity(
        &mut self,
        ent_id: Entity,
        sv_gravity: f32,
        frame_time: Duration,
    ) -> Result<(), ServerError> {
        // Gravity set to 0 means normal gravity rather than no gravity.
        let tables = self.world.resource::<QcTables>().clone();
        let ent_gravity = match tables.fields.get(self.world, ent_id, "gravity") {
            Ok(QcValue::Float(0.)) | Err(_) => 1.0,
            Ok(QcValue::Float(gravity)) => gravity,
            Ok(_) => 1.0,
        };

        let mut ent = self.phys_mut(ent_id)?;
        let mut vel = ent.velocity;
        vel.z -= ent_gravity * sv_gravity * duration_to_f32(frame_time);
        ent.set_velocity(vel);

        Ok(())
    }

    pub fn physics_client(
        &mut self,
        player_id: Entity,
        frame_time: Duration,
    ) -> Result<(), ServerError> {
        let server_vars = self.server_vars()?;

        let time = duration_to_f32(self.time());
        let mut globals = self.globals_mut();
        let pre_think = globals.player_pre_think;
        globals.time = time;
        globals.self_ = player_id;
        self.call(pre_think)?;

        let mut player = self.phys_mut(player_id)?;

        player.limit_velocity(server_vars.max_entity_velocity);

        match player.move_kind()? {
            MoveKind::None => {
                if !self.think(player_id, frame_time)? {
                    return Ok(());
                }
            }
            MoveKind::Walk => {
                if !self.think(player_id, frame_time)? {
                    return Ok(());
                }

                if !self.check_water(player_id)?
                    && !self.phys(player_id)?.has_flag(EntityFlags::WATER_JUMP)
                {
                    self.apply_gravity(player_id, server_vars.gravity, frame_time)?;
                }

                self.check_stuck(player_id)?;
                self.physics_walk(player_id, frame_time, &server_vars)?;
            }
            // e.g. dead players.
            MoveKind::Toss | MoveKind::Bounce => {
                self.physics_toss(player_id, frame_time)?;
            }
            MoveKind::Fly => {
                if !self.think(player_id, frame_time)? {
                    return Ok(());
                }

                self.physics_player_fly(player_id, frame_time)?;
            }
            MoveKind::NoClip => {
                if !self.think(player_id, frame_time)? {
                    return Ok(());
                }

                let mut player = self.phys_mut(player_id)?;
                let origin = player.origin + duration_to_f32(frame_time) * player.velocity;
                player.set_origin(origin);
            }
            other => {
                return Err(ServerError::with_msg(format!(
                    "physics_client: bad movetype {other:?}"
                )));
            }
        }

        if !self.exists(player_id) {
            return Ok(());
        }

        self.link_entity(player_id, true)?;

        let time = duration_to_f32(self.time());
        let mut globals = self.globals_mut();
        let post_think = globals.player_post_think;
        globals.time = time;
        globals.self_ = player_id;
        self.call(post_think)?;

        Ok(())
    }

    pub fn physics(
        &mut self,
        clients: &[Entity],
        frame_time: Duration,
    ) -> Result<HashMap<Entity, ServerError>, ServerError> {
        self.globals_mut().frametime = duration_to_f32(frame_time);

        let mut ent_error_map = HashMap::<Entity, ServerError>::new();

        // Apply movement once per frame, before physics
        for &entity_id in clients {
            let cmd = self.client_cmd(entity_id);
            if let Err(e) = self.client_think(entity_id, &cmd, frame_time) {
                ent_error_map.insert(entity_id, e);
            }
        }

        self.start_frame()?;

        for &entity_id in clients {
            if let Err(e) = self.physics_client(entity_id, frame_time) {
                ent_error_map.insert(entity_id, e);
            }
        }

        let max_clients = self.world.resource::<super::Session>().max_clients();

        for (edict, ent_id) in self.edicts().iter().collect::<Vec<_>>() {
            if self.globals().force_retouch != 0.0 {
                // Force all entities to touch triggers, even if they didn't
                // move. This is required when e.g. creating new triggers, as
                // stationary entities typically do not get relinked, and so
                // will ignore new triggers even when touching them.
                //
                // TODO: this may have a subtle ordering bug. If entity 2 has
                // physics run, sets ForceRetouch and spawns entity 1, then
                // entity 1 will not have a chance to touch triggers this frame.
                // Quake solves this by using a linked list and always spawning
                // at the end so that newly spawned entities always have physics
                // run this frame.
                if let Err(e) = self.link_entity(ent_id, true) {
                    ent_error_map.insert(ent_id, e);
                    continue;
                }
            }

            if !self.exists(ent_id) {
                continue;
            }

            if edict != 0
                && edict as usize > max_clients
                && let Err(e) = self.ent_physics(ent_id, frame_time)
            {
                ent_error_map.insert(ent_id, e);
            }
        }

        // QuakeC sets this to 2 so that entities retouch for two frames.
        let force_retouch = self.globals().force_retouch;
        if force_retouch != 0.0 {
            self.globals_mut().force_retouch = force_retouch - 1.;
        }

        self.level_mut().time += frame_time;

        Ok(ent_error_map)
    }

    /// The latest movement command of the client controlling `entity`.
    fn client_cmd(&self, entity: Entity) -> MoveCmd {
        self.world
            .resource::<super::Session>()
            .persist
            .client_slots
            .active_clients()
            .find(|(_, client)| client.entity == entity)
            .map(|(_, client)| client.cmd)
            .unwrap_or_default()
    }

    /// Applies ground friction to a player.
    ///
    /// This should only be called when the player is on the ground and not swimming.
    ///
    /// Equivalent to `SV_UserFriction`.
    fn player_apply_friction(
        &mut self,
        ent_id: Entity,
        frame_time: Duration,
        player_vars: &PlayerVars,
    ) -> Result<(), ServerError> {
        /// How far ahead of the player (in the direction of movement) to look for a dropoff.
        const DROPOFF_DETECT_DIST: f32 = 16.;
        /// How far below the player's feet the ground must be for it to count as a dropoff.
        const DROPOFF_TRACE_Z: f32 = 34.;

        let ent = self.phys(ent_id)?;

        let velocity = ent.velocity;
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

        let (trace, _) = self.trace_entity_move(
            ent_id,
            start,
            Vec3::ZERO,
            Vec3::ZERO,
            end,
            CollideKind::NoMonsters,
        )?;

        let friction = if trace.is_terminal() {
            player_vars.ground_friction * player_vars.edge_friction
        } else {
            player_vars.ground_friction
        };

        let scale =
            friction_scale(speed, friction, player_vars.stop_speed, duration_to_f32(frame_time));

        self.phys_mut(ent_id)?.set_velocity(velocity * scale);

        Ok(())
    }

    /// Applies a client's latest movement command to its player entity
    ///
    /// Equivalent to `SV_ClientThink`.
    pub fn client_think(
        &mut self,
        ent_id: Entity,
        cmd: &MoveCmd,
        frame_time: Duration,
    ) -> Result<(), ServerError> {
        /// How fast the view kick (`punchangle`) decays, in degrees per second.
        const PUNCH_ANGLE_DECAY: f32 = 10.;

        let player_vars: PlayerVars = self.registry().read_cvars()?;
        let dt = duration_to_f32(frame_time);
        let time = duration_to_f32(self.time());

        let move_kind = self.phys(ent_id)?.move_kind()?;
        if move_kind == MoveKind::None {
            return Ok(());
        }

        let on_ground = self.phys(ent_id)?.has_flag(EntityFlags::ON_GROUND);

        // `DropPunchAngle`
        let mut player = self.player_mut(ent_id)?;
        let punch_angle = player.punchangle;
        let punch_len = (punch_angle.length() - PUNCH_ANGLE_DECAY * dt).max(0.);
        player.set_punchangle(punch_angle.normalize_or_zero() * punch_len);

        // Dead players can't move.
        if player.health <= 0. {
            return Ok(());
        }

        // Show the player model with 1/3 of the view pitch, and roll it when strafing.
        // Swimming uses the view angle without the view kick.
        let swim_angle = player.v_angle;
        let v_angle = swim_angle + player.punchangle;
        let fix_angle = player.fixangle != 0.;
        let move_dir = self.ai(ent_id)?.movedir;
        let teleport_time = self.player(ent_id)?.teleport_time;
        let mut ent = self.phys_mut(ent_id)?;
        let mut angles = ent.angles;
        angles.z = calc_roll(angles, ent.velocity) * 4.;
        if !fix_angle {
            angles.x = -v_angle.x / 3.;
            angles.y = v_angle.y;
        }
        ent.set_angles(angles);

        if ent.has_flag(EntityFlags::WATER_JUMP) {
            // `SV_WaterJump`
            if time > teleport_time || ent.waterlevel == 0. {
                ent.remove_flags(EntityFlags::WATER_JUMP);
                self.player_mut(ent_id)?.teleport_time = 0.;
            }

            let mut ent = self.phys_mut(ent_id)?;
            let jump_velocity = move_dir;
            let velocity = ent.velocity;
            ent.set_velocity(jump_velocity.xy().extend(velocity.z));

            return Ok(());
        }

        if ent.waterlevel >= 2. && move_kind != MoveKind::NoClip {
            let velocity = super::water_move(
                ent.velocity,
                &angle_vectors(swim_angle),
                cmd,
                player_vars.max_speed,
                player_vars.ground_friction,
                player_vars.accel,
                dt,
            );
            ent.set_velocity(velocity);
            return Ok(());
        }

        // `SV_AirMove`
        let AngleVectors { forward, right, .. } = angle_vectors(angles);

        let mut fwd_move = cmd.forward;
        // If the player is still in a teleporter, wait until they leave it
        // before allowing them to teleport again.
        if time < teleport_time && fwd_move < 0. {
            fwd_move = 0.;
        }

        let mut wish_vel = forward * fwd_move + right * cmd.side;
        wish_vel.z = if move_kind == MoveKind::Walk { 0. } else { cmd.up };

        let mut wish_speed = wish_vel.length();
        if wish_speed > player_vars.max_speed {
            wish_vel *= player_vars.max_speed / wish_speed;
            wish_speed = player_vars.max_speed;
        }
        let wish_dir = wish_vel.normalize_or_zero();

        let accel_speed = player_vars.accel * dt * wish_speed;

        if move_kind == MoveKind::NoClip {
            ent.set_velocity(wish_vel);
        } else if on_ground {
            self.player_apply_friction(ent_id, frame_time, &player_vars)?;

            let mut ent = self.phys_mut(ent_id)?;
            let velocity = accelerate(ent.velocity, wish_dir, wish_speed, accel_speed);
            ent.set_velocity(velocity);
        } else {
            // Not on the ground, so there is little effect on velocity.
            let wish_speed = wish_speed.min(player_vars.max_air_speed);
            let velocity = accelerate(ent.velocity, wish_dir, wish_speed, accel_speed);
            ent.set_velocity(velocity);
        }

        Ok(())
    }

    /// Returns the contents of the world at `point`.
    ///
    /// Currents count as water.
    ///
    /// Equivalent to `SV_PointContents`.
    pub fn point_contents(&self, point: Vec3) -> BspLeafContents {
        match self.point_contents_raw(point) {
            c if c as i32 >= BspLeafContents::Current0 as i32 => BspLeafContents::Water,
            c => c,
        }
    }

    /// Updates an entity's `waterlevel` and `watertype`.
    ///
    /// Returns whether the entity is swimming (`waterlevel > 1`).
    ///
    /// Equivalent to `SV_CheckWater`.
    fn check_water(&mut self, ent_id: Entity) -> Result<bool, ServerError> {
        let ent = self.phys(ent_id)?;
        let origin = ent.origin;
        let min = ent.mins;
        let max = ent.maxs;
        let view_ofs = self.player(ent_id)?.view_ofs;

        let mut water_level = 0.;
        let mut water_type = quake_contents(BspLeafContents::Empty);

        // Feet
        let contents = self.point_contents(origin.with_z(origin.z + min.z + 1.));
        if is_liquid(contents) {
            water_type = quake_contents(contents);
            water_level = 1.;

            // Waist
            if is_liquid(self.point_contents(origin.with_z(origin.z + (min.z + max.z) * 0.5))) {
                water_level = 2.;

                // Eyes
                if is_liquid(self.point_contents(origin.with_z(origin.z + view_ofs.z))) {
                    water_level = 3.;
                }
            }
        }

        let mut ent = self.phys_mut(ent_id)?;
        ent.waterlevel = water_level;
        ent.watertype = water_type;

        Ok(water_level > 1.)
    }

    /// Updates a non-player entity's `watertype` and plays a splash sound if it entered or left
    /// water.
    ///
    /// Equivalent to `SV_CheckWaterTransition`.
    fn check_water_transition(&mut self, ent_id: Entity) -> Result<(), ServerError> {
        const SPLASH_SOUND: &str = "misc/h2ohit1.wav";

        let origin = self.phys(ent_id)?.origin;
        let contents = self.point_contents(origin);

        let mut ent = self.phys_mut(ent_id)?;
        let old_water_type = ent.watertype;

        if old_water_type == 0. {
            // Just spawned here.
            ent.watertype = quake_contents(contents);
            ent.waterlevel = 1.;
            return Ok(());
        }

        let splash = if is_liquid(contents) {
            ent.watertype = quake_contents(contents);
            ent.waterlevel = 1.;
            old_water_type == quake_contents(BspLeafContents::Empty)
        } else {
            ent.watertype = quake_contents(BspLeafContents::Empty);
            // Quake stores the contents here rather than a water level.
            ent.waterlevel = quake_contents(contents);
            old_water_type != quake_contents(BspLeafContents::Empty)
        };

        if splash {
            self.sound(ent_id, 0, SPLASH_SOUND, 1., 1.)?;
        }

        Ok(())
    }

    pub fn physics_push(
        &mut self,
        ent_id: Entity,
        frame_time: Duration,
    ) -> Result<(), ServerError> {
        let old_local_time = duration_from_f32(self.phys(ent_id)?.ltime);
        let next_think_secs: f32 = self.callbacks(ent_id)?.nextthink;

        if next_think_secs == 0. {
            return Ok(());
        }

        let next_think = duration_from_f32(next_think_secs);

        let new_local_time = old_local_time + frame_time;

        let move_time = if next_think < new_local_time {
            // If the think is overdue, dt is zero
            next_think.saturating_sub(old_local_time)
        } else {
            frame_time
        };

        if move_time > Duration::ZERO {
            self.move_push(ent_id, move_time)?;
        }

        let new_local_time = duration_from_f32(self.phys(ent_id)?.ltime);

        if (Bound::Excluded(old_local_time), Bound::Included(new_local_time)).contains(&next_think)
        {
            // Deschedule next think
            let mut callbacks = self.callbacks_mut(ent_id)?;
            callbacks.nextthink = 0.;

            // Call entity's think function.
            let think = callbacks.think;
            let time = duration_to_f32(self.time());
            let worldspawn = self.worldspawn();
            let mut globals = self.globals_mut();
            globals.time = time;
            globals.self_ = ent_id;
            globals.other = worldspawn;
            self.call(think)?;
        }

        Ok(())
    }

    /// Moves an entity with `MOVETYPE_NOCLIP`.
    ///
    /// Equivalent to `SV_Physics_Noclip`.
    pub fn physics_noclip(
        &mut self,
        ent_id: Entity,
        frame_time: Duration,
    ) -> Result<(), ServerError> {
        if !self.think(ent_id, frame_time)? {
            // Entity was removed by its think function.
            return Ok(());
        }

        let mut ent = self.phys_mut(ent_id)?;

        let frame_time_f = duration_to_f32(frame_time);

        let angles = ent.angles;
        let angle_vel = ent.avelocity;
        ent.set_angles(angles + frame_time_f * angle_vel);

        let orig = ent.origin;
        let vel = ent.velocity;
        ent.set_origin(orig + frame_time_f * vel);

        self.link_entity(ent_id, false)
    }

    /// Runs physics for a monster.
    ///
    /// Monsters only fall when they aren't on the ground (or flying/swimming): otherwise they are
    /// moved by `walkmove`/`movetogoal`.
    ///
    /// Equivalent to `SV_Physics_Step`.
    pub fn physics_step(
        &mut self,
        ent_id: Entity,
        frame_time: Duration,
    ) -> Result<(), ServerError> {
        const LAND_SOUND: &str = "demon/dland2.wav";

        let ServerVars { gravity, max_entity_velocity: max_velocity, .. } = self.server_vars()?;

        let in_freefall = !self
            .phys(ent_id)?
            .flags()
            .intersects(EntityFlags::ON_GROUND | EntityFlags::FLY | EntityFlags::SWIM);

        if in_freefall {
            let vel = self.phys(ent_id)?.velocity;

            // If true, play an impact sound when the entity hits the ground.
            let hit_sound = vel.z < -0.1 * gravity;

            self.apply_gravity(ent_id, gravity, frame_time)?;

            self.phys_mut(ent_id)?.limit_velocity(max_velocity);

            // Move the entity and relink it.
            self.physics_player_fly(ent_id, frame_time)?;
            self.link_entity(ent_id, true)?;

            if !self.exists(ent_id) {
                return Ok(());
            }

            if self.phys(ent_id)?.has_flag(EntityFlags::ON_GROUND) && hit_sound {
                // Entity hit the ground this frame.
                self.sound(ent_id, 0, LAND_SOUND, 1., 1.)?;
            }
        }

        if !self.think(ent_id, frame_time)? {
            return Ok(());
        }

        self.check_water_transition(ent_id)
    }

    /// Runs physics for a projectile or other free-moving entity.
    ///
    /// This handles `MOVETYPE_TOSS`, `MOVETYPE_BOUNCE`, `MOVETYPE_FLY` and `MOVETYPE_FLYMISSILE`.
    ///
    /// Equivalent to `SV_Physics_Toss`.
    pub fn physics_toss(
        &mut self,
        ent_id: Entity,
        frame_time: Duration,
    ) -> Result<(), ServerError> {
        /// Bouncing entities with a lower upward speed than this after a bounce come to rest.
        const BOUNCE_STOP_SPEED: f32 = 60.;

        let ServerVars { gravity, max_entity_velocity: max_velocity, .. } = self.server_vars()?;

        if !self.think(ent_id, frame_time)? {
            return Ok(());
        }

        let mut ent = self.phys_mut(ent_id)?;

        // If on the ground, don't move.
        if ent.has_flag(EntityFlags::ON_GROUND) {
            return Ok(());
        }

        ent.limit_velocity(max_velocity);

        if !matches!(ent.move_kind()?, MoveKind::Fly | MoveKind::FlyMissile) {
            self.apply_gravity(ent_id, gravity, frame_time)?;
        }

        let dt = duration_to_f32(frame_time);

        let mut ent = self.phys_mut(ent_id)?;
        let angles = ent.angles;
        let angular_velocity = ent.avelocity;
        ent.set_angles(angles + dt * angular_velocity);

        let frame_velocity = ent.velocity * dt;

        let (trace, hit) = self.push_entity(ent_id, frame_velocity)?;

        let Some(plane) = trace.plane() else {
            // Didn't hit anything.
            return Ok(());
        };

        if !self.exists(ent_id) {
            return Ok(());
        }

        let normal = -plane.normal();

        let worldspawn = self.worldspawn();
        let mut ent = self.phys_mut(ent_id)?;
        let is_bounce = ent.move_kind()? == MoveKind::Bounce;
        let overbounce = if is_bounce { 1.5 } else { 1. };
        let velocity = clip_velocity(ent.velocity, normal, overbounce);
        ent.set_velocity(velocity);

        // Stop if on the ground.
        if normal.z > 0.7 && (velocity.z < BOUNCE_STOP_SPEED || !is_bounce) {
            ent.add_flags(EntityFlags::ON_GROUND);
            ent.groundentity = hit.unwrap_or(worldspawn);
            ent.set_velocity(Vec3::ZERO);
            ent.set_avelocity(Vec3::ZERO);
        }

        self.check_water_transition(ent_id)
    }

    fn push_entity(
        &mut self,
        ent_id: Entity,
        move_dir: Vec3,
    ) -> Result<(Trace, Option<Entity>), ServerError> {
        let ent = self.phys(ent_id)?;
        let start = ent.origin;
        let end = start + move_dir;
        let min = ent.mins;
        let max = ent.maxs;

        let kind = match (ent.move_kind()?, ent.solid()?) {
            (MoveKind::FlyMissile, _) => CollideKind::Missile,
            (_, EntitySolid::Not | EntitySolid::Trigger) => CollideKind::NoMonsters,
            _ => CollideKind::Normal,
        };

        let (trace, hit) = self.trace_entity_move(ent_id, start, min, max, end, kind)?;

        self.phys_mut(ent_id)?.set_origin(trace.end_point());

        self.link_entity(ent_id, true)?;

        if let Some(hit) = hit {
            self.impact_entities(ent_id, hit)?;
        }

        Ok((trace, hit))
    }

    /// Moves a pusher (e.g. a door or platform) and anything it pushes.
    ///
    /// If the pusher is blocked, everything is moved back and the pusher's `blocked` function is
    /// called.
    ///
    /// Equivalent to `SV_PushMove`.
    fn move_push(&mut self, pusher_id: Entity, move_time: Duration) -> Result<(), ServerError> {
        let mut pusher = self.phys_mut(pusher_id)?;

        let move_time_f32 = duration_to_f32(move_time);
        let velocity = pusher.velocity;

        let new_local_time = pusher.ltime + move_time_f32;
        pusher.ltime = new_local_time;

        if velocity == Vec3::ZERO {
            return Ok(());
        }

        let move_amt = velocity * move_time_f32;

        // The pusher's bounds at its final position.
        let push_min = pusher.absmin + move_amt;
        let push_max = pusher.absmax + move_amt;

        let pusher_origin = pusher.origin;

        // Move the pusher to its final position.
        debug!("Moving pusher by {move_amt}");
        pusher.set_origin(pusher_origin + move_amt);
        self.link_entity(pusher_id, false)?;

        // Entities which have been moved, and where they were moved from.
        let mut moved = Vec::<(Entity, Vec3)>::new();

        // See if any solid entities are inside the final position.
        for other_id in
            self.edicts().range(1..).map(|(_, e)| e).filter(|e| *e != pusher_id).collect::<Vec<_>>()
        {
            if !self.exists(other_id) {
                continue;
            }

            let other = self.phys(other_id)?;

            if matches!(other.move_kind()?, MoveKind::Push | MoveKind::None | MoveKind::NoClip) {
                continue;
            }

            // If the entity is standing on the pusher, it will definitely be moved.
            if !(other.has_flag(EntityFlags::ON_GROUND) && other.groundentity == pusher_id) {
                let other_min = other.absmin;
                let other_max = other.absmax;
                let outside =
                    (0..3).any(|i| other_min[i] >= push_max[i] || other_max[i] <= push_min[i]);

                // Skip the entity unless it's inside the pusher's final position.
                if outside || !self.entity_is_stuck(other_id)? {
                    continue;
                }
            }

            let mut other = self.phys_mut(other_id)?;

            // Remove the ground flag for non-players.
            if other.move_kind()? != MoveKind::Walk {
                other.remove_flags(EntityFlags::ON_GROUND);
            }

            let old_origin = other.origin;
            moved.push((other_id, old_origin));

            // Try moving the other entity, ignoring the pusher.
            self.phys_mut(pusher_id)?.set_solid(EntitySolid::Not);
            let push_result = self.push_entity(other_id, move_amt);
            if self.exists(pusher_id) {
                self.phys_mut(pusher_id)?.set_solid(EntitySolid::Bsp);
            }
            push_result?;

            if !self.exists(pusher_id) {
                // Removed by a touch function.
                return Ok(());
            }

            if !self.exists(other_id) {
                // Removed by a touch function.
                moved.pop();
                continue;
            }

            // If it is still inside the pusher, the pusher is blocked.
            if !self.entity_is_stuck(other_id)? {
                continue;
            }

            let mut other = self.phys_mut(other_id)?;

            if other.mins.x == other.maxs.x {
                continue;
            }

            let solid = other.solid()?;
            if solid == EntitySolid::Not || solid == EntitySolid::Trigger {
                // This is a corpse.
                let mins = Vec3 { x: 0., y: 0., z: other.mins.z };

                other.set_mins(mins);
                other.set_maxs(mins);
                continue;
            }

            other.set_origin(old_origin);
            self.link_entity(other_id, true)?;

            debug!("Resetting pusher origin");
            self.phys_mut(pusher_id)?.set_origin(pusher_origin);
            self.link_entity(pusher_id, false)?;

            let mut pusher = self.phys_mut(pusher_id)?;
            let new_local_time = pusher.ltime - move_time_f32;
            pusher.ltime = new_local_time;

            // If the pusher has a `blocked` function, call it. Otherwise, just stay in place until
            // the obstacle is gone.
            let pusher_blocked_fn = self.callbacks(pusher_id)?.blocked;

            if !pusher_blocked_fn.is_null() {
                let worldspawn = self.worldspawn();
                let mut globals = self.globals_mut();
                globals.self_ = pusher_id;
                globals.other = other_id;
                globals.world = worldspawn;
                self.call(pusher_blocked_fn)?;
            }

            // Move back any entities we already moved.
            debug!("Resetting all positions");
            for (ent, original_position) in moved {
                if !self.exists(ent) {
                    continue;
                }

                self.phys_mut(ent)?.set_origin(original_position);
                self.link_entity(ent, false)?;
            }

            return Ok(());
        }

        Ok(())
    }

    const DROP_TO_FLOOR_DIST: f32 = 256.0;

    /// Moves an entity straight down until it collides with a solid surface.
    ///
    /// Returns `true` if the entity hit the floor, `false` otherwise.
    ///
    /// ## Notes
    /// - The drop distance is limited to 256, so entities which are more than 256 units above a
    ///   solid surface will not actually hit the ground.
    pub fn drop_entity_to_floor(&mut self, ent_id: Entity) -> Result<bool, ServerError> {
        debug!("Finding floor for entity {ent_id}");
        let ent = self.phys(ent_id)?;
        let origin = ent.origin;

        let end = Vec3::new(origin.x, origin.y, origin.z - Self::DROP_TO_FLOOR_DIST);
        let min = ent.mins;
        let max = ent.maxs;

        let (trace, collide_entity) =
            self.trace_entity_move(ent_id, origin, min, max, end, CollideKind::Normal)?;
        debug!("End position after drop: {:?}", trace.end_point());

        if trace.is_terminal() || trace.all_solid() {
            // Entity didn't hit the floor or is stuck.
            return Ok(false);
        }

        // Entity hit the floor. Update origin, relink and set ON_GROUND flag.
        self.phys_mut(ent_id)?.set_origin(trace.end_point());
        self.link_entity(ent_id, false)?;
        let worldspawn = self.worldspawn();
        let mut ent = self.phys_mut(ent_id)?;
        ent.add_flags(EntityFlags::ON_GROUND);
        ent.groundentity = collide_entity.unwrap_or(worldspawn);

        Ok(true)
    }

    pub fn touch_triggers(&mut self, ent_id: Entity) -> Result<(), ServerError> {
        thread_local! {
            static TOUCHED_TRIGGERS_BUF: RefCell<Vec<Entity>> = Vec::new().into();
        }

        let Some(area) = self.world.get::<qc::fields::AreaLink>(ent_id).map(|link| link.0) else {
            return Err(no_such_entity(ent_id));
        };
        if area.is_none() {
            // Unlinked entities touch nothing.
            return Ok(());
        }

        // The buffer is taken out so that nested calls (from touch functions) can use it too.
        let mut touched = TOUCHED_TRIGGERS_BUF.with_borrow_mut(std::mem::take);
        touched.clear();

        // Start from area ID 0, like `SV_TouchLinks`
        let result = self.collision().list_touched_triggers(self.world, &mut touched, ent_id, 0);

        let result = result.and_then(|()| {
            // Save state.
            let restore_self = self.globals().self_;
            let restore_other = self.globals().other;

            // Activate the touched triggers.
            let result = touched.iter().try_for_each(|&trigger_id| {
                // An earlier touch may have freed or changed this trigger, so check it again.
                let Some(trigger_touch) =
                    self.world.get::<EntCallbacks>(trigger_id).map(|cb| cb.touch)
                else {
                    return Ok(());
                };
                let still_trigger = self
                    .world
                    .get::<EntPhysics>(trigger_id)
                    .is_some_and(|phys| phys.solid().ok() == Some(EntitySolid::Trigger));
                if trigger_touch.is_null() || !still_trigger {
                    return Ok(());
                }

                let mut globals = self.globals_mut();
                globals.self_ = trigger_id;
                globals.other = ent_id;
                self.call(trigger_touch)
            });

            // Restore state, even if a touch function failed.
            let mut globals = self.globals_mut();
            globals.self_ = restore_self;
            globals.other = restore_other;

            result
        });

        TOUCHED_TRIGGERS_BUF.with_borrow_mut(|buf| *buf = touched);

        result
    }

    /// Runs two entities' touch functions.
    pub fn impact_entities(&mut self, ent_a: Entity, ent_b: Entity) -> Result<(), ServerError> {
        let restore_self = self.globals().self_;
        let restore_other = self.globals().other;
        let worldspawn = self.worldspawn();

        let time = duration_to_f32(self.time());
        self.globals_mut().time = time;

        // Set up and run Entity A's touch function.
        let touch_a = self.callbacks(ent_a)?.touch;
        let solid_a = self.phys(ent_a)?.solid()?;
        if !touch_a.is_null() && solid_a != EntitySolid::Not {
            let mut globals = self.globals_mut();
            globals.self_ = ent_a;
            globals.other = ent_b;
            globals.world = worldspawn;
            self.call(touch_a)?;
        }

        // Set up and run Entity B's touch function.
        let touch_b = self.callbacks(ent_b)?.touch;
        let solid_b = self.phys(ent_b)?.solid()?;
        if !touch_b.is_null() && solid_b != EntitySolid::Not {
            let mut globals = self.globals_mut();
            globals.self_ = ent_b;
            globals.other = ent_a;
            globals.world = worldspawn;
            self.call(touch_b)?;
        }

        let mut globals = self.globals_mut();
        globals.self_ = restore_self;
        globals.other = restore_other;
        globals.world = worldspawn;

        Ok(())
    }

    /// Starts a sound on an entity.
    ///
    /// `volume` is in `[0, 1]` and `attenuation` is in `[0, 4)`.
    ///
    /// Equivalent to `sv_startsound`.
    pub fn sound(
        &mut self,
        entity: Entity,
        channel: i8,
        sound_name: &str,
        volume: f32,
        attenuation: f32,
    ) -> Result<(), ServerError> {
        let volume = (volume * 255.) as _;

        let Some(sound_id) = self.level().sound_precache.find(sound_name) else {
            error!("Cannot find sound {sound_name} in precache");
            return Ok(());
        };

        // Use the centre of the entity's bounds, as brush entities (doors, platforms etc.) usually
        // have their origin at the world origin.
        let ent = self.phys(entity)?;
        let position = ent.origin + 0.5 * (ent.mins + ent.maxs);
        let entity_id = self.edict(entity)?;

        ServerCmd::Sound {
            volume: Some(volume),
            attenuation: Some(attenuation),
            entity_id: entity_id as _,
            channel,
            sound_id: sound_id as _,
            position,
        }
        .serialize(&mut self.level_mut().broadcast)?;

        Ok(())
    }
}
