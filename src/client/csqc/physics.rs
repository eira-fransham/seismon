//! Collision and movement for CSQC's entities, with the code that the server uses for its own
//! (see [`crate::common::world`]).
//!
//! CSQC has its own [`Collision`] (the client's map, and an area grid that only CSQC's entities
//! are linked into) in a [`CsqcCollision`] on its worldspawn, so traces from CSQC never see the
//! server's entities, even on a listen server.

use std::sync::Arc;

use bevy::prelude::*;
use bevy_mod_scripting::bindings::InteropError;
use bevy_mod_scripting_qcvm::{QCFunction, QCVmContext};
use bevy_trenchbroom::bsp::Bsp;

use super::storage::{CsqcEdict, CsqcEdicts, CsqcFields, CsqcStorage, csqc_storage};
use crate::{
    common::{
        qc::{QcStorageImpl as _, QcValue, qc_error},
        world::{
            EntityFlags, EntitySolid,
            collision::{Collision, CollisionEntities, CollisionEntity, CollisionError},
            hull::{BrushModel, BspLeafContents},
            model::Model,
            movement::{MoveCtx, MoveEntity},
            phys::{CollideKind, MoveKind, Trace},
            player::PlayerCtx,
        },
    },
    server::PlayerVars,
};

/// CSQC's collision: the client's map and the area grid of CSQC's entities, on the CSQC
/// worldspawn.
#[derive(Component, Debug)]
pub struct CsqcCollision(pub Collision);

/// The area grid node that a CSQC entity is linked into.
#[derive(Component, Debug, Default, Clone, Copy)]
pub struct CsqcAreaLink(pub Option<usize>);

/// The default step height, if `sv_stepheight` isn't set on the client.
const DEFAULT_MAX_STEP: f32 = 18.;

/// Builds CSQC's collision from the map `bsp`.
pub fn collision_for_bsp(bsp: &Bsp) -> Result<Collision, CollisionError> {
    let models = BrushModel::from_bsp_data(&bsp.data)?
        .into_iter()
        .enumerate()
        .map(|(i, model)| Model::from_brush_model(format!("*{i}"), model).cast())
        .collect::<Vec<_>>();
    if models.is_empty() {
        return Err(CollisionError::with_msg("The map has no models"));
    }
    Ok(Collision::new(models))
}

/// CSQC's entities, for [`Collision`].
pub struct CsqcEnts {
    pub storage: Arc<CsqcStorage>,
}

impl CollisionEntities for CsqcEnts {
    fn worldspawn(&self, _world: &World) -> Entity {
        self.storage.worldspawn()
    }

    fn edict_entity(&self, world: &World, edict: u16) -> Option<Entity> {
        world.get::<CsqcEdicts>(self.storage.worldspawn())?.get(usize::from(edict))
    }

    fn edict(&self, world: &World, entity: Entity) -> Option<u16> {
        world.get::<CsqcEdict>(entity).and_then(|edict| u16::try_from(edict.0).ok())
    }

    fn get(&self, world: &World, entity: Entity) -> Result<CollisionEntity, CollisionError> {
        let fields = world
            .get::<CsqcFields>(entity)
            .ok_or_else(|| CollisionError::no_such_entity(entity))?;
        let hot = self.storage.hot();
        let world_entity = self.storage.worldspawn();
        Ok(CollisionEntity {
            solid: fields.float(hot.solid),
            movetype: fields.float(hot.movetype),
            origin: fields.vec3(hot.origin),
            mins: fields.vec3(hot.mins),
            maxs: fields.vec3(hot.maxs),
            absmin: fields.vec3(hot.absmin),
            absmax: fields.vec3(hot.absmax),
            size: fields.vec3(hot.size),
            flags: EntityFlags::from_bits_truncate(fields.float(hot.flags) as i32 as u16),
            owner: Some(fields.entity(hot.owner, world_entity).unwrap_or(world_entity)),
            modelindex: Some(fields.float(hot.modelindex)),
            has_touch: Some(fields.function(hot.touch).is_some()),
        })
    }

    fn set_abs_bounds(
        &self,
        world: &mut World,
        entity: Entity,
        absmin: Vec3,
        absmax: Vec3,
    ) -> Result<(), CollisionError> {
        let hot = self.storage.hot().clone();
        let mut fields = world
            .get_mut::<CsqcFields>(entity)
            .ok_or_else(|| CollisionError::no_such_entity(entity))?;
        fields.set_vec3(hot.absmin, absmin);
        fields.set_vec3(hot.absmax, absmax);
        Ok(())
    }

    fn set_bounds(
        &self,
        world: &mut World,
        entity: Entity,
        mins: Vec3,
        maxs: Vec3,
    ) -> Result<(), CollisionError> {
        let hot = self.storage.hot().clone();
        let mut fields = world
            .get_mut::<CsqcFields>(entity)
            .ok_or_else(|| CollisionError::no_such_entity(entity))?;
        fields.set_vec3(hot.mins, mins);
        fields.set_vec3(hot.maxs, maxs);
        fields.set_vec3(hot.size, maxs - mins);
        Ok(())
    }

    fn area_link(&self, world: &World, entity: Entity) -> Option<Option<usize>> {
        world.get::<CsqcAreaLink>(entity).map(|link| link.0)
    }

    fn set_area_link(
        &self,
        world: &mut World,
        entity: Entity,
        link: Option<usize>,
    ) -> Result<(), CollisionError> {
        world.entity_mut(entity).insert(CsqcAreaLink(link));
        Ok(())
    }
}

fn collision_error(e: CollisionError) -> InteropError {
    qc_error(e)
}

/// Movement for CSQC's entities, for the builtins that move them.
pub struct CsqcMoveCtx<'w> {
    pub world: &'w mut World,
    pub storage: Arc<CsqcStorage>,
    pub vm: QCVmContext,
}

impl<'w> CsqcMoveCtx<'w> {
    /// The movement context of the CSQC VM with worldspawn `root`.
    pub fn new(world: &'w mut World, root: Entity) -> Result<Self, InteropError> {
        let storage = csqc_storage(world, root).ok_or_else(|| qc_error("CSQC isn't loaded"))?;
        let vm = world
            .get::<crate::common::qc::QcVm>(root)
            .ok_or_else(|| qc_error("CSQC isn't loaded"))?
            .0
            .clone();
        Ok(Self { world, storage, vm })
    }

    fn ents(&self) -> CsqcEnts {
        CsqcEnts { storage: self.storage.clone() }
    }

    fn root(&self) -> Entity {
        self.storage.worldspawn()
    }

    fn fields(&self, entity: Entity) -> Result<&CsqcFields, InteropError> {
        self.world
            .get::<CsqcFields>(entity)
            .ok_or_else(|| qc_error(format!("{entity} isn't a CSQC entity")))
    }

    fn fields_mut(&mut self, entity: Entity) -> Result<Mut<'_, CsqcFields>, InteropError> {
        self.world
            .get_mut::<CsqcFields>(entity)
            .ok_or_else(|| qc_error(format!("{entity} isn't a CSQC entity")))
    }

    /// Runs `f` with CSQC's collision, if the map has been loaded.
    pub fn with_collision<O>(
        &mut self,
        f: impl FnOnce(&mut Collision, &CsqcEnts, &mut World) -> Result<O, CollisionError>,
    ) -> Result<Option<O>, InteropError> {
        let root = self.root();
        let Some(CsqcCollision(mut collision)) =
            self.world.entity_mut(root).take::<CsqcCollision>()
        else {
            return Ok(None);
        };
        let ents = self.ents();
        let result = f(&mut collision, &ents, self.world);
        self.world.entity_mut(root).insert(CsqcCollision(collision));
        result.map(Some).map_err(collision_error)
    }

    /// Traces from `start` to `end` for `entity` (or for nothing, if `entity` is `None`).
    pub fn trace_move(
        &self,
        entity: Option<Entity>,
        start: Vec3,
        min: Vec3,
        max: Vec3,
        end: Vec3,
        kind: CollideKind,
    ) -> Result<(Trace, Option<Entity>), InteropError> {
        let Some(CsqcCollision(collision)) = self.world.get::<CsqcCollision>(self.root()) else {
            // Without a map, there's nothing to hit.
            return Ok((Trace::uninitialized(start, end), None));
        };
        collision
            .trace_move(&self.ents(), self.world, entity, start, min, max, end, kind)
            .map_err(collision_error)
    }

    /// The `self` and `other` globals, to restore after calling QuakeC.
    fn self_other(&self) -> Result<(QcValue, QcValue), InteropError> {
        Ok((
            self.storage.get_global(self.world, "self")?,
            self.storage.get_global(self.world, "other")?,
        ))
    }

    /// Calls `function` with `self` and `other` set, restoring them afterwards.
    pub fn call_with(
        &mut self,
        function: QCFunction,
        this: Entity,
        other: Entity,
    ) -> Result<(), InteropError> {
        let (old_self, old_other) = self.self_other()?;
        self.storage.set_global(self.world, "self", QcValue::Entity(this))?;
        self.storage.set_global(self.world, "other", QcValue::Entity(other))?;
        let result = self.vm.call_with_world::<()>(
            self.world,
            function.0,
            Vec::<bevy_mod_scripting_qcvm::qcvm::Value>::new(),
        );
        self.storage.set_global(self.world, "self", old_self)?;
        self.storage.set_global(self.world, "other", old_other)?;
        result
    }

    /// Runs the touch functions of the triggers that `entity` touches (`SV_TouchLinks`).
    pub fn touch_triggers(&mut self, entity: Entity) -> Result<(), InteropError> {
        let mut touched = Vec::new();
        if let Some(CsqcCollision(collision)) = self.world.get::<CsqcCollision>(self.root()) {
            collision
                .list_touched_triggers(&self.ents(), self.world, &mut touched, entity, 0)
                .map_err(collision_error)?;
        }
        let touch = self.storage.hot().touch;
        for trigger in touched {
            // Earlier touch functions can remove triggers.
            let Some(function) =
                self.world.get::<CsqcFields>(trigger).and_then(|f| f.function(touch))
            else {
                continue;
            };
            self.call_with(function, trigger, entity)?;
        }
        Ok(())
    }

    pub fn set_float(
        &mut self,
        entity: Entity,
        slot: Option<super::storage::Slot>,
        value: f32,
    ) -> Result<(), InteropError> {
        self.fields_mut(entity)?.set_float(slot, value);
        Ok(())
    }

    pub fn set_vec3(
        &mut self,
        entity: Entity,
        slot: Option<super::storage::Slot>,
        value: Vec3,
    ) -> Result<(), InteropError> {
        self.fields_mut(entity)?.set_vec3(slot, value);
        Ok(())
    }
}

impl MoveCtx for CsqcMoveCtx<'_> {
    type Error = InteropError;

    fn entity(&self, entity: Entity) -> Result<MoveEntity, InteropError> {
        let fields = self.fields(entity)?;
        let hot = self.storage.hot();
        Ok(MoveEntity {
            origin: fields.vec3(hot.origin),
            angles: fields.vec3(hot.angles),
            mins: fields.vec3(hot.mins),
            maxs: fields.vec3(hot.maxs),
            absmin: fields.vec3(hot.absmin),
            absmax: fields.vec3(hot.absmax),
            flags: EntityFlags::from_bits_truncate(fields.float(hot.flags) as i32 as u16),
        })
    }

    fn set_origin(&mut self, entity: Entity, origin: Vec3) -> Result<(), InteropError> {
        let slot = self.storage.hot().origin;
        self.set_vec3(entity, slot, origin)
    }

    fn set_yaw(&mut self, entity: Entity, yaw: f32) -> Result<(), InteropError> {
        let slot = self.storage.hot().angles;
        let mut angles = self.fields(entity)?.vec3(slot);
        angles.y = yaw;
        self.set_vec3(entity, slot, angles)
    }

    fn add_flags(&mut self, entity: Entity, flags: EntityFlags) -> Result<(), InteropError> {
        let slot = self.storage.hot().flags;
        let raw = self.fields(entity)?.float(slot) as i32;
        self.set_float(entity, slot, (raw | i32::from(flags.bits())) as f32)
    }

    fn remove_flags(&mut self, entity: Entity, flags: EntityFlags) -> Result<(), InteropError> {
        let slot = self.storage.hot().flags;
        let raw = self.fields(entity)?.float(slot) as i32;
        self.set_float(entity, slot, (raw & !i32::from(flags.bits())) as f32)
    }

    fn set_ground_entity(&mut self, entity: Entity, ground: Entity) -> Result<(), InteropError> {
        let (slot, root) = (self.storage.hot().groundentity, self.root());
        self.fields_mut(entity)?.set_entity(slot, Some(ground), root);
        Ok(())
    }

    fn ideal_yaw(&self, entity: Entity) -> Result<f32, InteropError> {
        Ok(self.fields(entity)?.float(self.storage.hot().ideal_yaw))
    }

    fn set_ideal_yaw(&mut self, entity: Entity, yaw: f32) -> Result<(), InteropError> {
        let slot = self.storage.hot().ideal_yaw;
        self.set_float(entity, slot, yaw)
    }

    fn yaw_speed(&self, entity: Entity) -> Result<f32, InteropError> {
        Ok(self.fields(entity)?.float(self.storage.hot().yaw_speed))
    }

    fn enemy(&self, entity: Entity) -> Result<Entity, InteropError> {
        let root = self.root();
        Ok(self.fields(entity)?.entity(self.storage.hot().enemy, root).unwrap_or(root))
    }

    fn goal_entity(&self, entity: Entity) -> Result<Entity, InteropError> {
        let root = self.root();
        Ok(self.fields(entity)?.entity(self.storage.hot().goalentity, root).unwrap_or(root))
    }

    fn trace(
        &self,
        entity: Entity,
        start: Vec3,
        min: Vec3,
        max: Vec3,
        end: Vec3,
        kind: CollideKind,
    ) -> Result<(Trace, Option<Entity>), InteropError> {
        self.trace_move(Some(entity), start, min, max, end, kind)
    }

    fn point_contents_raw(&self, point: Vec3) -> BspLeafContents {
        self.world
            .get::<CsqcCollision>(self.root())
            .map_or(BspLeafContents::Empty, |CsqcCollision(c)| c.contents_at_point(point))
    }

    fn link(&mut self, entity: Entity, touch_triggers: bool) -> Result<(), InteropError> {
        self.with_collision(|collision, ents, world| collision.link_entity(ents, world, entity))?;
        if touch_triggers {
            self.touch_triggers(entity)?;
        }
        Ok(())
    }

    fn world(&self) -> Entity {
        self.root()
    }

    fn max_step(&self) -> Result<f32, InteropError> {
        Ok(self
            .world
            .get_resource::<bevy_mod_commands::Registry>()
            .and_then(|r| r.read_cvar::<f32>("sv_stepheight").ok())
            .unwrap_or(DEFAULT_MAX_STEP))
    }

    fn random_ratio(&mut self, numerator: u32, denominator: u32) -> bool {
        rand::random_ratio(numerator, denominator)
    }

    fn random_bool(&mut self, p: f64) -> bool {
        rand::random_bool(p)
    }
}

impl PlayerCtx for CsqcMoveCtx<'_> {
    fn velocity(&self, entity: Entity) -> Result<Vec3, InteropError> {
        Ok(self.fields(entity)?.vec3(self.storage.hot().velocity))
    }

    fn set_velocity(&mut self, entity: Entity, velocity: Vec3) -> Result<(), InteropError> {
        let slot = self.storage.hot().velocity;
        self.set_vec3(entity, slot, velocity)
    }

    fn water_level(&self, entity: Entity) -> Result<f32, InteropError> {
        Ok(self.fields(entity)?.float(self.storage.hot().waterlevel))
    }

    fn set_water(&mut self, entity: Entity, level: f32, kind: f32) -> Result<(), InteropError> {
        let hot = self.storage.hot().clone();
        let mut fields = self.fields_mut(entity)?;
        fields.set_float(hot.waterlevel, level);
        fields.set_float(hot.watertype, kind);
        Ok(())
    }

    fn view_ofs(&self, entity: Entity) -> Result<Vec3, InteropError> {
        Ok(self.fields(entity)?.vec3(self.storage.hot().view_ofs))
    }

    fn v_angle(&self, entity: Entity) -> Result<Vec3, InteropError> {
        Ok(self.fields(entity)?.vec3(self.storage.hot().v_angle))
    }

    fn old_origin(&self, entity: Entity) -> Result<Vec3, InteropError> {
        Ok(self.fields(entity)?.vec3(self.storage.hot().oldorigin))
    }

    fn set_old_origin(&mut self, entity: Entity, origin: Vec3) -> Result<(), InteropError> {
        let slot = self.storage.hot().oldorigin;
        self.set_vec3(entity, slot, origin)
    }

    fn solid(&self, entity: Entity) -> Result<EntitySolid, InteropError> {
        let ent = self.ents().get(self.world, entity).map_err(collision_error)?;
        ent.solid().map_err(qc_error)
    }

    fn move_kind(&self, entity: Entity) -> Result<MoveKind, InteropError> {
        let ent = self.ents().get(self.world, entity).map_err(collision_error)?;
        ent.move_kind().map_err(qc_error)
    }

    fn exists(&self, entity: Entity) -> bool {
        self.world.get::<CsqcEdict>(entity).is_some()
    }

    fn is_stuck(&self, entity: Entity) -> Result<bool, InteropError> {
        let ent = self.entity(entity)?;
        let (trace, _) =
            self.trace(entity, ent.origin, ent.mins, ent.maxs, ent.origin, CollideKind::Normal)?;
        Ok(trace.start_solid())
    }

    fn impact(&mut self, a: Entity, b: Entity) -> Result<(), InteropError> {
        // `SV_Impact`: each entity touches the other, if it has a touch function and is solid.
        let touch = self.storage.hot().touch;
        for (this, other) in [(a, b), (b, a)] {
            if !self.exists(this) && this != self.root() {
                continue;
            }
            let Some(function) = self.world.get::<CsqcFields>(this).and_then(|f| f.function(touch))
            else {
                continue;
            };
            if PlayerCtx::solid(self, this)? != EntitySolid::Not {
                self.call_with(function, this, other)?;
            }
        }
        Ok(())
    }

    fn hit_without_entity(&self, entity: Entity) -> InteropError {
        qc_error(format!("fly_move: trace for {entity} hit a surface but no entity"))
    }
}

/// The player movement cvars on the client, with Quake's defaults for any that aren't set.
pub fn player_vars(world: &World) -> PlayerVars {
    let registry = world.get_resource::<bevy_mod_commands::Registry>();
    let cvar = |name: &str, default: f32| {
        registry.and_then(|r| r.read_cvar::<f32>(name).ok()).unwrap_or(default)
    };
    PlayerVars {
        max_speed: cvar("sv_maxspeed", 320.),
        max_air_speed: cvar("sv_maxairspeed", 30.),
        stop_speed: cvar("sv_stopspeed", 100.),
        accel: cvar("sv_accelerate", 10.),
        ground_friction: cvar("sv_friction", 4.),
        edge_friction: cvar("edgefriction", 2.),
    }
}

/// The gravity cvar on the client.
pub fn gravity(world: &World) -> f32 {
    world
        .get_resource::<bevy_mod_commands::Registry>()
        .and_then(|r| r.read_cvar::<f32>("sv_gravity").ok())
        .unwrap_or(800.)
}
