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

//! Collision against a level's world model and the entities linked into its area grid (Quake's
//! `world.c`).
//!
//! [`Collision`] doesn't know where entities' fields are stored: each QuakeC VM (the server and
//! CSQC) gives it access to its own entities through [`CollisionEntities`], so the same code
//! traces against the server's entities on the server and CSQC's entities on the client.

use std::collections::BTreeSet;

use arrayvec::ArrayVec;
use bevy::prelude::*;
use num::FromPrimitive as _;
use snafu::Snafu;

use super::{
    EntityError, EntityFlags, EntitySolid,
    hull::{BspCollisionHull, BspLeafContents, HullError},
    model::{Model, ModelKind},
    phys::{Collide, CollideKind, MoveKind, Trace, bounds_for_move},
};

const AREA_DEPTH: usize = 4;
const NUM_AREA_NODES: usize = 2usize.pow(AREA_DEPTH as u32 + 1) - 1;

/// An error from [`Collision`].
#[derive(Snafu, Debug)]
pub enum CollisionError {
    /// An entity which doesn't exist (e.g. because it was removed) was accessed.
    #[snafu(display("no such entity: {entity}"))]
    NoSuchEntity { entity: Entity },
    #[snafu(context(false), display("{source}"))]
    Hull { source: HullError },
    #[snafu(context(false), display("{source}"))]
    Entity { source: EntityError },
    #[snafu(display("{message}"))]
    Other { message: String },
}

impl CollisionError {
    pub fn no_such_entity(entity: Entity) -> Self {
        Self::NoSuchEntity { entity }
    }

    pub fn with_msg(message: impl Into<String>) -> Self {
        Self::Other { message: message.into() }
    }
}

/// What collision needs to know about an entity.
#[derive(Debug, Clone, Copy)]
pub struct CollisionEntity {
    /// `.solid`, which is an [`EntitySolid`].
    pub solid: f32,
    /// `.movetype`, which is a [`MoveKind`].
    pub movetype: f32,
    pub origin: Vec3,
    pub mins: Vec3,
    pub maxs: Vec3,
    pub absmin: Vec3,
    pub absmax: Vec3,
    pub size: Vec3,
    pub flags: EntityFlags,
    /// `.owner`, or `None` if the entity has no such field.
    pub owner: Option<Entity>,
    /// `.modelindex`, or `None` if the entity has no such field.
    pub modelindex: Option<f32>,
    /// Whether the entity has a `.touch` function, or `None` if it has no such field.
    pub has_touch: Option<bool>,
}

impl CollisionEntity {
    pub fn solid(&self) -> Result<EntitySolid, EntityError> {
        let solid = self.solid as i32;
        EntitySolid::from_i32(solid).ok_or_else(|| {
            EntityError::with_msg(format!("Invalid value for entity.solid ({solid})"))
        })
    }

    pub fn move_kind(&self) -> Result<MoveKind, EntityError> {
        MoveKind::from_i32(self.movetype as i32).ok_or_else(|| {
            EntityError::with_msg(format!("Invalid value for entity.move_kind ({})", self.movetype))
        })
    }
}

/// Access to the entities of a QuakeC VM, for [`Collision`]. Entities are linked into the area
/// grid by edict number, so that collision (and trigger) order is deterministic.
pub trait CollisionEntities {
    /// The VM's worldspawn.
    fn worldspawn(&self, world: &World) -> Entity;

    /// The entity with edict number `edict`, if it is in use.
    fn edict_entity(&self, world: &World, edict: u16) -> Option<Entity>;

    /// The edict number of `entity`, or `None` if it has been removed.
    fn edict(&self, world: &World, entity: Entity) -> Option<u16>;

    /// The collision fields of `entity`.
    fn get(&self, world: &World, entity: Entity) -> Result<CollisionEntity, CollisionError>;

    /// Sets `.absmin` and `.absmax`.
    fn set_abs_bounds(
        &self,
        world: &mut World,
        entity: Entity,
        absmin: Vec3,
        absmax: Vec3,
    ) -> Result<(), CollisionError>;

    /// Sets `.mins`, `.maxs` and `.size`.
    fn set_bounds(
        &self,
        world: &mut World,
        entity: Entity,
        mins: Vec3,
        maxs: Vec3,
    ) -> Result<(), CollisionError>;

    /// The area node that `entity` is linked into (`Some(None)` if it isn't linked), or `None` if
    /// it can't be linked.
    fn area_link(&self, world: &World, entity: Entity) -> Option<Option<usize>>;

    /// Sets the area node that `entity` is linked into.
    fn set_area_link(
        &self,
        world: &mut World,
        entity: Entity,
        link: Option<usize>,
    ) -> Result<(), CollisionError>;
}

#[derive(Debug)]
enum AreaNodeKind {
    Branch(AreaBranch),
    Leaf,
}

#[derive(Debug)]
struct AreaNode {
    kind: AreaNodeKind,
    /// Edict numbers of the triggers linked into this node, kept ordered so that collision and
    /// trigger order is deterministic.
    triggers: BTreeSet<u16>,
    /// Edict numbers of the solid entities linked into this node, ordered as `triggers` is.
    solids: BTreeSet<u16>,
}

impl AreaNode {
    /// Generate a breadth-first 2-D binary space partitioning tree with the given extents.
    fn generate(mins: Vec3, maxs: Vec3) -> ArrayVec<AreaNode, NUM_AREA_NODES> {
        let mut nodes: ArrayVec<AreaNode, NUM_AREA_NODES> = ArrayVec::new();

        // place internal nodes
        for i in 0..AREA_DEPTH {
            for _ in 0..2usize.pow(i as u32) {
                let len = nodes.len();
                nodes.push(AreaNode {
                    kind: AreaNodeKind::Branch(AreaBranch {
                        axis: AreaBranchAxis::X,
                        dist: 0.0,
                        front: 2 * len + 1,
                        back: 2 * len + 2,
                    }),
                    triggers: BTreeSet::new(),
                    solids: BTreeSet::new(),
                });
            }
        }

        // place leaves
        for _ in 0..2usize.pow(AREA_DEPTH as u32) {
            nodes.push(AreaNode {
                kind: AreaNodeKind::Leaf,
                triggers: BTreeSet::new(),
                solids: BTreeSet::new(),
            });
        }

        // recursively assign geometric data to the nodes
        AreaNode::setup(&mut nodes, 0, mins, maxs);

        nodes
    }

    fn setup(nodes: &mut ArrayVec<AreaNode, NUM_AREA_NODES>, index: usize, mins: Vec3, maxs: Vec3) {
        let size = maxs - mins;

        let axis = if size.x > size.y { AreaBranchAxis::X } else { AreaBranchAxis::Y };

        let dist = 0.5 * (maxs[axis as usize] + mins[axis as usize]);

        let mut front_mins = mins;
        front_mins[axis as usize] = dist;

        let mut back_maxs = maxs;
        back_maxs[axis as usize] = dist;

        let (front, back) = match nodes[index].kind {
            AreaNodeKind::Branch(ref mut b) => {
                b.axis = axis;
                b.dist = dist;
                (b.front, b.back)
            }
            AreaNodeKind::Leaf => return,
        };

        AreaNode::setup(nodes, front, front_mins, maxs);
        AreaNode::setup(nodes, back, mins, back_maxs);
    }
}

#[derive(Copy, Clone, Debug)]
enum AreaBranchAxis {
    X = 0,
    Y = 1,
}

#[derive(Debug)]
struct AreaBranch {
    axis: AreaBranchAxis,
    dist: f32,
    front: usize,
    back: usize,
}

/// The collision data for a level: the area tree (which entities are linked into) and the
/// models. Model 0 is the null model, and model 1 is the world.
#[derive(Resource, Debug)]
pub struct Collision {
    area_nodes: ArrayVec<AreaNode, NUM_AREA_NODES>,
    models: Vec<Model>,
}

impl Collision {
    pub fn new(mut brush_models: Vec<Model>) -> Collision {
        // generate area tree for world model
        let area_nodes = AreaNode::generate(brush_models[0].min(), brush_models[0].max());

        let mut models = Vec::with_capacity(brush_models.len() + 1);

        // put null model at index 0
        models.push(Model::none("").cast());

        // take ownership of all brush models
        models.append(&mut brush_models);

        Collision { area_nodes, models }
    }

    pub fn models(&self) -> &[Model] {
        &self.models
    }

    pub fn contents_at_point(&self, point: Vec3) -> BspLeafContents {
        match self.models[1].kind() {
            ModelKind::Brush(brush) => brush.hull(0).unwrap().contents_at_point(point).unwrap(),
            _ => unimplemented!("Non-brush worldmodel"),
        }
    }

    /// Lists the triggers in area node `area_id` and its children that the entity touches.
    ///
    /// To find every trigger an entity touches, start at the root (`area_id` 0): a trigger that
    /// straddles a split plane is linked at that branch node, above the entity's own node.
    ///
    /// The triggers are stored in `touched`.
    pub fn list_touched_triggers<C: CollisionEntities>(
        &self,
        ents: &C,
        world: &World,
        touched: &mut Vec<Entity>,
        ent_id: Entity,
        area_id: usize,
    ) -> Result<(), CollisionError> {
        let Ok(ent) = ents.get(world, ent_id) else {
            return Ok(());
        };

        'next_trigger: for trigger_id in
            self.area_nodes[area_id].triggers.iter().filter_map(|&e| ents.edict_entity(world, e))
        {
            if trigger_id == ent_id {
                // Don't trigger self.
                continue;
            }

            let Ok(trigger) = ents.get(world, trigger_id) else {
                continue;
            };

            let trigger_touch =
                trigger.has_touch.ok_or_else(|| CollisionError::no_such_entity(trigger_id))?;
            if !trigger_touch || trigger.solid()? != EntitySolid::Trigger {
                continue;
            }

            for i in 0..3 {
                if ent.absmin[i] > trigger.absmax[i] || ent.absmax[i] < trigger.absmin[i] {
                    // Entities are not touching.
                    continue 'next_trigger;
                }
            }

            touched.push(trigger_id);
        }

        // Recurse into the sub-areas the entity's bounding box reaches, as in `SV_TouchLinks`.
        if let AreaNodeKind::Branch(AreaBranch { axis, dist, front, back }) =
            self.area_nodes[area_id].kind
        {
            if ent.absmax[axis as usize] > dist {
                self.list_touched_triggers(ents, world, touched, ent_id, front)?;
            }
            if ent.absmin[axis as usize] < dist {
                self.list_touched_triggers(ents, world, touched, ent_id, back)?;
            }
        }

        Ok(())
    }

    pub fn unlink_entity<C: CollisionEntities>(
        &mut self,
        ents: &C,
        world: &mut World,
        e_id: Entity,
    ) -> Result<(), CollisionError> {
        // if this entity has been removed or freed, do nothing
        let (Some(edict), Some(link)) = (ents.edict(world, e_id), ents.area_link(world, e_id))
        else {
            return Ok(());
        };

        let area_id = match link {
            Some(i) => i,

            // entity not linked
            None => return Ok(()),
        };

        if self.area_nodes[area_id].triggers.remove(&edict) {
            debug!("Unlinking entity {e_id} from area triggers");
        } else if self.area_nodes[area_id].solids.remove(&edict) {
            debug!("Unlinking entity {e_id} from area solids");
        }

        ents.set_area_link(world, e_id, None)
    }

    pub fn link_entity<C: CollisionEntities>(
        &mut self,
        ents: &C,
        world: &mut World,
        e_id: Entity,
    ) -> Result<(), CollisionError> {
        // if this entity has been removed or freed, do nothing
        let Some(edict) = ents.edict(world, e_id) else {
            return Ok(());
        };

        // don't link the world entity
        if edict == 0 {
            return Ok(());
        }

        self.unlink_entity(ents, world, e_id)?;

        let (solid, abs_min, abs_max) = {
            const BOUNDS_OFFSET_MAGNITUDE: f32 = 15.;

            let ent = ents.get(world, e_id)?;

            let origin = ent.origin;
            let mins = ent.mins;
            let maxs = ent.maxs;
            debug!("origin = {:?} mins = {:?} maxs = {:?}", origin, mins, maxs);
            let mut abs_min = origin + mins;
            let mut abs_max = origin + maxs;

            let bounds_offset = if ent.flags.contains(EntityFlags::ITEM) {
                (Vec3::X + Vec3::Y) * BOUNDS_OFFSET_MAGNITUDE
            } else {
                Vec3::splat(1.)
            };

            abs_min -= bounds_offset;
            abs_max += bounds_offset;

            ents.set_abs_bounds(world, e_id, abs_min, abs_max)?;

            let solid = ent.solid()?;

            // Mark leaves containing entity for PVS.
            let model_index = ent.modelindex.ok_or_else(|| CollisionError::no_such_entity(e_id))?;
            if model_index != 0.0 {
                // TODO: SV_FindTouchedLeafs
                debug!("TODO: SV_FindTouchedLeafs");
            }

            if solid == EntitySolid::Not {
                // this entity has no touch interaction, we're done
                return Ok(());
            }

            (solid, abs_min, abs_max)
        };

        let mut node_id = 0;
        while let AreaNodeKind::Branch(b) = &self.area_nodes[node_id].kind {
            let dist = b.dist;
            debug!("abs_min = {abs_min} | abs_max = {abs_max} | dist = {dist}",);
            if abs_min[b.axis as usize] > b.dist {
                node_id = b.front;
            } else if abs_max[b.axis as usize] < b.dist {
                node_id = b.back;
            } else {
                // entity spans both sides of the plane
                break;
            }
        }

        if solid == EntitySolid::Trigger {
            debug!("Linking entity {e_id} into area {node_id} triggers");
            self.area_nodes[node_id].triggers.insert(edict);
        } else {
            debug!("Linking entity {e_id} into area {node_id} solids");
            self.area_nodes[node_id].solids.insert(edict);
        }
        ents.set_area_link(world, e_id, Some(node_id))
    }

    /// Sets an entity's size to that of the model with index `model_id`.
    ///
    /// The null model (index 0) has no size. Returns `false` (and does nothing) if the model isn't
    /// one the world has loaded, i.e. it isn't a brush model.
    pub fn set_entity_model<C: CollisionEntities>(
        &self,
        ents: &C,
        world: &mut World,
        e_id: Entity,
        model_id: usize,
    ) -> Result<bool, CollisionError> {
        let Some(model) = self.models.get(model_id) else {
            return Ok(false);
        };

        let (min, max) =
            if model_id == 0 { (Vec3::ZERO, Vec3::ZERO) } else { (model.min(), model.max()) };
        ents.set_bounds(world, e_id, min, max)?;

        Ok(true)
    }

    // TODO: handle the offset return value internally
    pub fn hull_for_entity<C: CollisionEntities>(
        &self,
        ents: &C,
        world: &World,
        e_id: Entity,
        min: Vec3,
        max: Vec3,
    ) -> Result<(BspCollisionHull, Vec3), CollisionError> {
        let ent = ents.get(world, e_id)?;
        let solid = ent.solid()?;
        debug!("Entity solid type: {:?}", solid);

        match solid {
            EntitySolid::Bsp => {
                if ent.move_kind()? != MoveKind::Push {
                    return Err(CollisionError::with_msg(format!(
                        "Brush entities must have MoveKind::Push (has {:?})",
                        ent.move_kind()
                    )));
                }

                let size = max - min;
                let model_index =
                    ent.modelindex.ok_or_else(|| CollisionError::no_such_entity(e_id))?;
                match self.models.get(model_index as usize).map(|model| model.kind()) {
                    Some(ModelKind::Brush(bmodel)) => {
                        let hull_index = if size.x < 3.0 {
                            0
                        } else if size.x <= 32.0 {
                            1
                        } else {
                            2
                        };

                        debug!("Using hull {hull_index}");
                        let hull = bmodel.hull(hull_index)?;

                        let offset = hull.mins() - min + ent.origin;

                        Ok((hull, offset))
                    }
                    _ => Err(CollisionError::with_msg(
                        "Non-brush entities may not have MoveKind::Push",
                    )),
                }
            }

            _ => {
                let hull = BspCollisionHull::for_bounds(ent.mins - max, ent.maxs - min)?;
                let offset = ent.origin;

                Ok((hull, offset))
            }
        }
    }

    pub fn entity_is_stuck<C: CollisionEntities>(
        &self,
        ents: &C,
        world: &World,
        e_id: Entity,
    ) -> Result<bool, CollisionError> {
        let ent = ents.get(world, e_id)?;

        let origin = ent.origin;
        let min = ent.mins;
        let max = ent.maxs;
        let (trace, _) = self.trace_entity_move(
            ents,
            world,
            e_id,
            origin,
            min,
            max,
            origin,
            CollideKind::Normal,
        )?;

        Ok(trace.start_solid())
    }

    /// Traces a box moving from `start` to `end` against the world and the solid entities, ignoring
    /// `e_id` (and the entities it owns or is owned by). Returns the trace and the entity hit, if any.
    // TODO: Make this a struct (which Quake does too)
    #[expect(clippy::too_many_arguments)]
    pub fn trace_entity_move<C: CollisionEntities>(
        &self,
        ents: &C,
        world: &World,
        e_id: Entity,
        start: Vec3,
        min: Vec3,
        max: Vec3,
        end: Vec3,
        kind: CollideKind,
    ) -> Result<(Trace, Option<Entity>), CollisionError> {
        self.trace_move(ents, world, Some(e_id), start, min, max, end, kind)
    }

    /// Like [`Collision::trace_entity_move`], but for a move that doesn't ignore any entity (as
    /// for `traceline` with `world` as the entity to ignore).
    #[expect(clippy::too_many_arguments)]
    pub fn trace_move<C: CollisionEntities>(
        &self,
        ents: &C,
        world: &World,
        e_id: Option<Entity>,
        start: Vec3,
        min: Vec3,
        max: Vec3,
        end: Vec3,
        kind: CollideKind,
    ) -> Result<(Trace, Option<Entity>), CollisionError> {
        debug!("start={start} min={min} max={max} end={end}",);

        let worldspawn = ents.worldspawn(world);

        let world_trace = self.trace(ents, world, start, min, max, end)?;

        debug!("End position after collision test with world hull: {:?}", world_trace.end_point());

        // If the world trace starts and ends in a solid, don't even try to trace against entities.
        if world_trace.all_solid() {
            return Ok((world_trace, Some(worldspawn)));
        }

        // if this is a rocket or a grenade, expand the monster collision box
        let (monster_min, monster_max) = match kind {
            CollideKind::Missile => (Vec3::splat(-15.0), Vec3::splat(15.0)),
            _ => (min, max),
        };

        let (move_min, move_max) = bounds_for_move(start, monster_min, monster_max, end);

        let collide = Collide {
            e_id,
            move_min,
            move_max,
            min,
            max,
            monster_min,
            monster_max,
            start,
            end,
            kind,
        };

        let (mut collide_trace, collide_ent) = self.collide(ents, world, &collide)?;

        if collide_trace.all_solid()
            || collide_trace.start_solid()
            || collide_trace.ratio() < world_trace.ratio()
        {
            // Quake seeds the entity clip with the world trace (`SV_Move`), so starting in the
            // world's solid is kept even if an entity is hit first.
            if world_trace.start_solid() {
                collide_trace.set_start_solid(true);
            }
            Ok((collide_trace, collide_ent))
        } else {
            // The trace only has an entity if it hit something, see `SV_ClipMoveToEntity`.
            let hit_world = !world_trace.is_terminal() || world_trace.start_solid();
            Ok((world_trace, hit_world.then_some(worldspawn)))
        }
    }

    pub fn collide<C: CollisionEntities>(
        &self,
        ents: &C,
        world: &World,
        collide: &Collide,
    ) -> Result<(Trace, Option<Entity>), CollisionError> {
        let mut best = (Trace::uninitialized(collide.start, collide.end), None);
        self.collide_area(ents, world, 0, collide, &mut best)?;
        Ok(best)
    }

    /// Traces `collide` against the solid entities in area node `area_id` and its children.
    ///
    /// `best` holds the closest hit found so far and the entity that was hit, and is only
    /// replaced (both parts together) when a closer hit is found, as in Quake's `SV_ClipToLinks`.
    fn collide_area<C: CollisionEntities>(
        &self,
        ents: &C,
        world: &World,
        area_id: usize,
        collide: &Collide,
        best: &mut (Trace, Option<Entity>),
    ) -> Result<(), CollisionError> {
        let area = &self.area_nodes[area_id];

        'collide_entity: for touch in area
            .solids
            .iter()
            .filter_map(|&e| ents.edict_entity(world, e))
            .filter(|e| Some(*e) != collide.e_id)
        {
            if best.0.all_solid() {
                return Ok(());
            }

            let touch_ent = ents.get(world, touch)?;

            match touch_ent.solid()? {
                // if the other entity has no collision, skip it
                EntitySolid::Not => continue,

                // triggers should not appear in the solids list
                EntitySolid::Trigger => {
                    return Err(CollisionError::with_msg(format!(
                        "Trigger in solids list ({touch})"
                    )));
                }

                // don't collide with monsters if the collide specifies not to do so
                s => {
                    if s != EntitySolid::Bsp && collide.kind == CollideKind::NoMonsters {
                        continue;
                    }
                }
            }

            // if bounding boxes never intersect, skip this entity
            {
                let abs_max = touch_ent.absmax;
                let abs_min = touch_ent.absmin;
                for i in 0..3 {
                    if collide.move_min[i] > abs_max[i] || collide.move_max[i] < abs_min[i] {
                        continue 'collide_entity;
                    }
                }
            }

            if let Some(e) = collide.e_id
                && ents.get(world, e)?.size[0] != 0.0
                && touch_ent.size[0] == 0.0
            {
                continue;
            }

            if let Some(e) = collide.e_id {
                // don't collide against owner or owned entities
                let owner = |e: Entity| -> Result<Entity, CollisionError> {
                    ents.get(world, e)?.owner.ok_or_else(|| CollisionError::no_such_entity(e))
                };
                if owner(touch)? == e || owner(e)? == touch {
                    continue;
                }
            }

            // select bounding boxes based on whether or not candidate is a monster
            let tmp_trace = if touch_ent.flags.contains(EntityFlags::MONSTER) {
                self.collide_move_with_entity(
                    ents,
                    world,
                    touch,
                    collide.start,
                    collide.monster_min,
                    collide.monster_max,
                    collide.end,
                )?
            } else {
                self.collide_move_with_entity(
                    ents,
                    world,
                    touch,
                    collide.start,
                    collide.min,
                    collide.max,
                    collide.end,
                )?
            };

            let (trace, collide_entity) = best;

            // check to see if this candidate is the closest yet and update trace if so
            if tmp_trace.all_solid() || tmp_trace.start_solid() || tmp_trace.ratio() < trace.ratio()
            {
                let start_solid = trace.start_solid() || tmp_trace.start_solid();
                *trace = tmp_trace;
                trace.set_start_solid(start_solid);
                *collide_entity = Some(touch);
            }
        }

        if let AreaNodeKind::Branch(ref b) = area.kind {
            if collide.move_max[b.axis as usize] > b.dist {
                self.collide_area(ents, world, b.front, collide, best)?;
            }

            if collide.move_min[b.axis as usize] < b.dist {
                self.collide_area(ents, world, b.back, collide, best)?;
            }
        }

        Ok(())
    }

    #[expect(clippy::too_many_arguments)]
    pub fn collide_move_with_entity<C: CollisionEntities>(
        &self,
        ents: &C,
        world: &World,
        e_id: Entity,
        start: Vec3,
        min: Vec3,
        max: Vec3,
        end: Vec3,
    ) -> Result<Trace, CollisionError> {
        let (hull, offset) = self.hull_for_entity(ents, world, e_id, min, max)?;
        debug!("hull offset for {e_id}: {offset}");

        Ok(hull.trace(start - offset, end - offset)?.adjust(offset))
    }

    /// Traces against the world model.
    pub fn trace<C: CollisionEntities>(
        &self,
        ents: &C,
        world: &World,
        start: Vec3,
        min: Vec3,
        max: Vec3,
        end: Vec3,
    ) -> Result<Trace, CollisionError> {
        let worldspawn = ents.worldspawn(world);
        self.collide_move_with_entity(ents, world, worldspawn, start, min, max, end)
    }
}
