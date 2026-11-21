//! Monster movement, ported from `sv_move.c`. The movement itself is shared with CSQC (see
//! [`movement`]); this gives it access to the server's entities.

use bevy::prelude::*;

use crate::{
    common::world::movement::{self, MoveCtx, MoveEntity},
    server::{
        ServerCtx, ServerVars,
        error::ServerError,
        world::{EntityFlags, Trace, hull::BspLeafContents, phys::CollideKind},
    },
};

impl MoveCtx for ServerCtx<'_> {
    type Error = ServerError;

    fn entity(&self, entity: Entity) -> Result<MoveEntity, ServerError> {
        let phys = self.phys(entity)?;
        Ok(MoveEntity {
            origin: phys.origin,
            angles: phys.angles,
            mins: phys.mins,
            maxs: phys.maxs,
            absmin: phys.absmin,
            absmax: phys.absmax,
            flags: phys.flags(),
        })
    }

    fn set_origin(&mut self, entity: Entity, origin: Vec3) -> Result<(), ServerError> {
        self.phys_mut(entity)?.set_origin(origin);
        Ok(())
    }

    fn set_yaw(&mut self, entity: Entity, yaw: f32) -> Result<(), ServerError> {
        self.phys_mut(entity)?.angles.y = yaw;
        Ok(())
    }

    fn add_flags(&mut self, entity: Entity, flags: EntityFlags) -> Result<(), ServerError> {
        self.phys_mut(entity)?.add_flags(flags);
        Ok(())
    }

    fn remove_flags(&mut self, entity: Entity, flags: EntityFlags) -> Result<(), ServerError> {
        self.phys_mut(entity)?.remove_flags(flags);
        Ok(())
    }

    fn set_ground_entity(&mut self, entity: Entity, ground: Entity) -> Result<(), ServerError> {
        self.phys_mut(entity)?.groundentity = ground;
        Ok(())
    }

    fn ideal_yaw(&self, entity: Entity) -> Result<f32, ServerError> {
        Ok(self.ai(entity)?.ideal_yaw)
    }

    fn set_ideal_yaw(&mut self, entity: Entity, yaw: f32) -> Result<(), ServerError> {
        self.ai_mut(entity)?.ideal_yaw = yaw;
        Ok(())
    }

    fn yaw_speed(&self, entity: Entity) -> Result<f32, ServerError> {
        Ok(self.ai(entity)?.yaw_speed)
    }

    fn enemy(&self, entity: Entity) -> Result<Entity, ServerError> {
        Ok(self.ai(entity)?.enemy)
    }

    fn goal_entity(&self, entity: Entity) -> Result<Entity, ServerError> {
        Ok(self.ai(entity)?.goalentity)
    }

    fn trace(
        &self,
        entity: Entity,
        start: Vec3,
        min: Vec3,
        max: Vec3,
        end: Vec3,
        kind: CollideKind,
    ) -> Result<(Trace, Option<Entity>), ServerError> {
        self.trace_entity_move(entity, start, min, max, end, kind)
    }

    fn point_contents_raw(&self, point: Vec3) -> BspLeafContents {
        ServerCtx::point_contents_raw(self, point)
    }

    fn link(&mut self, entity: Entity, touch_triggers: bool) -> Result<(), ServerError> {
        self.link_entity(entity, touch_triggers)
    }

    fn world(&self) -> Entity {
        self.worldspawn()
    }

    fn max_step(&self) -> Result<f32, ServerError> {
        Ok(self.server_vars()?.max_step)
    }

    fn random_ratio(&mut self, numerator: u32, denominator: u32) -> bool {
        self.rng().random_ratio(numerator, denominator)
    }

    fn random_bool(&mut self, p: f64) -> bool {
        self.rng().random_bool(p)
    }
}

impl ServerCtx<'_> {
    pub fn change_yaw(&mut self, ent_id: Entity) -> Result<(), ServerError> {
        movement::change_yaw(self, ent_id)
    }

    pub fn close_enough(&self, a: Entity, b: Entity, dist: f32) -> Result<bool, ServerError> {
        movement::close_enough(self, a, b, dist)
    }

    /// `SV_CheckBottom`. `server_vars` is only for the callers' convenience: the step height is
    /// read from the cvars.
    pub fn check_bottom(
        &self,
        ent_id: Entity,
        _server_vars: &ServerVars,
    ) -> Result<bool, ServerError> {
        movement::check_bottom(self, ent_id)
    }

    pub fn move_step(
        &mut self,
        ent_id: Entity,
        move_dir: Vec3,
        relink: bool,
    ) -> Result<bool, ServerError> {
        movement::move_step(self, ent_id, move_dir, relink)
    }

    pub fn step_direction(
        &mut self,
        ent_id: Entity,
        yaw: f32,
        dist: f32,
    ) -> Result<bool, ServerError> {
        movement::step_direction(self, ent_id, yaw, dist)
    }

    pub fn new_chase_dir(
        &mut self,
        this_id: Entity,
        goal_id: Entity,
        dist: f32,
    ) -> Result<(), ServerError> {
        movement::new_chase_dir(self, this_id, goal_id, dist)
    }
}
