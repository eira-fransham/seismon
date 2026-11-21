// Copyright © 2018 Cormac O'Brien
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software
// and associated documentation files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or
// substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
// BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

pub mod entity;

pub use crate::common::world::{hull, model, phys};
pub use self::{
    entity::{EntityError, EntityFlags, EntitySolid},
    phys::{MoveKind, Trace, TraceEnd, TraceEndKind, TraceStart},
};

use bevy::prelude::*;

pub use crate::common::world::collision::{
    Collision as LevelCollision, CollisionEntities, CollisionEntity, CollisionError,
};
use crate::server::{
    error::ServerError,
    qc::{
        edict::{Edict, EdictTable},
        fields::{AreaLink, EntAi, EntCallbacks, EntPhysics, EntRender},
    },
};

impl From<CollisionError> for ServerError {
    fn from(error: CollisionError) -> Self {
        match error {
            CollisionError::NoSuchEntity { entity } => {
                ServerError::NoSuchEntity { entity, backtrace: snafu::Backtrace::capture() }
            }
            CollisionError::Hull { source } => source.into(),
            CollisionError::Entity { source } => source.into(),
            CollisionError::Other { message } => ServerError::with_msg(message),
        }
    }
}

/// The server's entities, for [`LevelCollision`].
pub struct ServerEnts;

impl CollisionEntities for ServerEnts {
    fn worldspawn(&self, world: &World) -> Entity {
        world.resource::<EdictTable>().worldspawn()
    }

    fn edict_entity(&self, world: &World, edict: u16) -> Option<Entity> {
        world.resource::<EdictTable>().get(edict)
    }

    fn edict(&self, world: &World, entity: Entity) -> Option<u16> {
        world.get::<Edict>(entity).map(|edict| edict.0)
    }

    fn get(&self, world: &World, entity: Entity) -> Result<CollisionEntity, CollisionError> {
        let phys =
            world.get::<EntPhysics>(entity).ok_or_else(|| CollisionError::no_such_entity(entity))?;
        Ok(CollisionEntity {
            solid: phys.solid,
            movetype: phys.movetype,
            origin: phys.origin,
            mins: phys.mins,
            maxs: phys.maxs,
            absmin: phys.absmin,
            absmax: phys.absmax,
            size: phys.size,
            flags: phys.flags(),
            owner: world.get::<EntAi>(entity).map(|ai| ai.owner),
            modelindex: world.get::<EntRender>(entity).map(|render| render.modelindex),
            has_touch: world.get::<EntCallbacks>(entity).map(|callbacks| !callbacks.touch.is_null()),
        })
    }

    fn set_abs_bounds(
        &self,
        world: &mut World,
        entity: Entity,
        absmin: Vec3,
        absmax: Vec3,
    ) -> Result<(), CollisionError> {
        let mut phys = world
            .get_mut::<EntPhysics>(entity)
            .ok_or_else(|| CollisionError::no_such_entity(entity))?;
        phys.set_absmin(absmin);
        phys.set_absmax(absmax);
        Ok(())
    }

    fn set_bounds(
        &self,
        world: &mut World,
        entity: Entity,
        mins: Vec3,
        maxs: Vec3,
    ) -> Result<(), CollisionError> {
        world
            .get_mut::<EntPhysics>(entity)
            .ok_or_else(|| CollisionError::no_such_entity(entity))?
            .set_min_max_size(mins, maxs);
        Ok(())
    }

    fn area_link(&self, world: &World, entity: Entity) -> Option<Option<usize>> {
        world.get::<AreaLink>(entity).map(|link| link.0)
    }

    fn set_area_link(
        &self,
        world: &mut World,
        entity: Entity,
        link: Option<usize>,
    ) -> Result<(), CollisionError> {
        world.get_mut::<AreaLink>(entity).ok_or_else(|| CollisionError::no_such_entity(entity))?.0 =
            link;
        Ok(())
    }
}
