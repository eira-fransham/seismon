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
pub mod hull;
pub mod model;
pub mod phys;

use self::phys::{Collide, CollideKind};
pub use self::{
    entity::{EntityError, EntityFlags, EntitySolid},
    phys::{MoveKind, Trace, TraceEnd, TraceEndKind, TraceStart},
};
use std::collections::BTreeSet;

use crate::server::{
    error::ServerError,
    qc::{
        edict::{Edict, EdictTable},
        fields::{AreaLink, EntCallbacks, EntPhysics, EntRender},
    },
    world::{
        hull::{BspCollisionHull, BspLeafContents},
        model::{Model, ModelKind},
    },
};

use arrayvec::ArrayVec;
use bevy::prelude::*;

const AREA_DEPTH: usize = 4;
const NUM_AREA_NODES: usize = 2usize.pow(AREA_DEPTH as u32 + 1) - 1;

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

// The areas form a quadtree-like BSP tree which alternates splitting on the X
// and Y axes.
//
//                               00                                X
//               01                              02                Y
//       03              04              05              06        X
//   07      08      09      10      11      12      13      14    Y
// 15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  Leaves
//
//  [21]      [19]      [17]      [15]
//   ||        ||        ||        ||
//   ||        ||        ||        ||
//   12===05===11        08===03===07
//   ||   ||   ||        ||   ||   ||
//   ||   ||   ||        ||   ||   ||
//  [22]  ||  [20]      [18]  ||  [16]
//        ||                  ||
//        02========00========01
//        ||                  ||
//  [29]  ||  [27]      [25]  ||  [23]
//   ||   ||   ||        ||   ||   ||
//   ||   ||   ||        ||   ||   ||
//   14===06===13        10===04===09
//   ||        ||        ||        ||
//   ||        ||        ||        ||
//  [30]      [28]      [26]      [24]
//
// The tree won't necessarily look like this, this just assumes a rectangular area with width
// between 1-2x its length.

impl AreaNode {
    /// Generate a breadth-first 2-D binary space partitioning tree with the given extents.
    pub fn generate(mins: Vec3, maxs: Vec3) -> ArrayVec<AreaNode, NUM_AREA_NODES> {
        let mut nodes: ArrayVec<AreaNode, NUM_AREA_NODES> = ArrayVec::new();

        // we generate the skeleton of the tree iteratively -- the nodes are linked but have no
        // geometric data.

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

/// The collision data for the current level: the area tree (which entities are linked into) and
/// the models.
#[derive(Resource, Debug)]
pub struct LevelCollision {
    area_nodes: ArrayVec<AreaNode, NUM_AREA_NODES>,
    models: Vec<Model>,
}

fn phys(world: &World, e: Entity) -> Result<&EntPhysics, ServerError> {
    world.get::<EntPhysics>(e).ok_or_else(|| no_such_entity(e))
}

fn no_such_entity(entity: Entity) -> ServerError {
    ServerError::NoSuchEntity { entity, backtrace: snafu::Backtrace::capture() }
}

fn edict_entity(world: &World, edict: u16) -> Option<Entity> {
    world.resource::<EdictTable>().get(edict)
}

impl LevelCollision {
    pub fn new(mut brush_models: Vec<Model>) -> LevelCollision {
        // generate area tree for world model
        let area_nodes = AreaNode::generate(brush_models[0].min(), brush_models[0].max());

        let mut models = Vec::with_capacity(brush_models.len() + 1);

        // put null model at index 0
        models.push(Model::none("").cast());

        // take ownership of all brush models
        models.append(&mut brush_models);

        LevelCollision { area_nodes, models }
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
    pub fn list_touched_triggers(
        &self,
        world: &World,
        touched: &mut Vec<Entity>,
        ent_id: Entity,
        area_id: usize,
    ) -> Result<(), ServerError> {
        let Ok(ent) = phys(world, ent_id) else {
            return Ok(());
        };

        'next_trigger: for trigger_id in
            self.area_nodes[area_id].triggers.iter().filter_map(|&e| edict_entity(world, e))
        {
            if trigger_id == ent_id {
                // Don't trigger self.
                continue;
            }

            let Ok(trigger) = phys(world, trigger_id) else {
                continue;
            };

            let trigger_touch = world
                .get::<EntCallbacks>(trigger_id)
                .ok_or_else(|| no_such_entity(trigger_id))?
                .touch;
            if trigger_touch.is_null() || trigger.solid()? != EntitySolid::Trigger {
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
                self.list_touched_triggers(world, touched, ent_id, front)?;
            }
            if ent.absmin[axis as usize] < dist {
                self.list_touched_triggers(world, touched, ent_id, back)?;
            }
        }

        Ok(())
    }

    pub fn unlink_entity(&mut self, world: &mut World, e_id: Entity) -> Result<(), ServerError> {
        // if this entity has been removed or freed, do nothing
        let (Some(edict), Some(link)) = (world.get::<Edict>(e_id), world.get::<AreaLink>(e_id))
        else {
            return Ok(());
        };
        let edict = edict.0;

        let area_id = match link.0 {
            Some(i) => i,

            // entity not linked
            None => return Ok(()),
        };

        if self.area_nodes[area_id].triggers.remove(&edict) {
            debug!("Unlinking entity {e_id} from area triggers");
        } else if self.area_nodes[area_id].solids.remove(&edict) {
            debug!("Unlinking entity {e_id} from area solids");
        }

        world.get_mut::<AreaLink>(e_id).ok_or_else(|| no_such_entity(e_id))?.0 = None;

        Ok(())
    }

    pub fn link_entity(&mut self, world: &mut World, e_id: Entity) -> Result<(), ServerError> {
        // if this entity has been removed or freed, do nothing
        let Some(&Edict(edict)) = world.get::<Edict>(e_id) else {
            return Ok(());
        };

        // don't link the world entity
        if edict == 0 {
            return Ok(());
        }

        self.unlink_entity(world, e_id)?;

        let (solid, abs_min, abs_max) = {
            const BOUNDS_OFFSET_MAGNITUDE: f32 = 15.;

            let mut ent = world.get_mut::<EntPhysics>(e_id).ok_or_else(|| no_such_entity(e_id))?;

            let origin = ent.origin;
            let mins = ent.mins;
            let maxs = ent.maxs;
            debug!("origin = {:?} mins = {:?} maxs = {:?}", origin, mins, maxs);
            let mut abs_min = origin + mins;
            let mut abs_max = origin + maxs;

            let flags = ent.flags();

            let bounds_offset = if flags.contains(EntityFlags::ITEM) {
                (Vec3::X + Vec3::Y) * BOUNDS_OFFSET_MAGNITUDE
            } else {
                Vec3::splat(1.)
            };

            abs_min -= bounds_offset;
            abs_max += bounds_offset;

            ent.set_absmin(abs_min);
            ent.set_absmax(abs_max);

            let solid = ent.solid()?;

            // Mark leaves containing entity for PVS.
            let model_index =
                world.get::<EntRender>(e_id).ok_or_else(|| no_such_entity(e_id))?.modelindex;
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
        world.get_mut::<AreaLink>(e_id).ok_or_else(|| no_such_entity(e_id))?.0 = Some(node_id);

        Ok(())
    }

    /// Sets an entity's size to that of the model with precache index `model_id`.
    ///
    /// The null model (index 0) has no size. Returns `false` (and does nothing) if the model isn't
    /// one the world has loaded, i.e. it isn't a brush model.
    pub fn set_entity_model(
        &self,
        world: &mut World,
        e_id: Entity,
        model_id: usize,
    ) -> Result<bool, ServerError> {
        let Some(model) = self.models.get(model_id) else {
            return Ok(false);
        };

        let (min, max) =
            if model_id == 0 { (Vec3::ZERO, Vec3::ZERO) } else { (model.min(), model.max()) };
        world
            .get_mut::<EntPhysics>(e_id)
            .ok_or_else(|| no_such_entity(e_id))?
            .set_min_max_size(min, max);

        Ok(true)
    }

    // TODO: handle the offset return value internally
    pub fn hull_for_entity(
        &self,
        world: &World,
        e_id: Entity,
        min: Vec3,
        max: Vec3,
    ) -> Result<(BspCollisionHull, Vec3), ServerError> {
        let ent = phys(world, e_id)?;
        let solid = ent.solid()?;
        debug!("Entity solid type: {:?}", solid);

        match solid {
            EntitySolid::Bsp => {
                if ent.move_kind()? != MoveKind::Push {
                    return Err(ServerError::with_msg(format!(
                        "Brush entities must have MoveKind::Push (has {:?})",
                        ent.move_kind()
                    )));
                }

                let size = max - min;
                let model_index =
                    world.get::<EntRender>(e_id).ok_or_else(|| no_such_entity(e_id))?.modelindex;
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

                        let offset = hull.min() - min + ent.origin;

                        Ok((hull, offset))
                    }
                    _ => {
                        Err(ServerError::with_msg("Non-brush entities may not have MoveKind::Push"))
                    }
                }
            }

            _ => {
                let hull = BspCollisionHull::for_bounds(ent.mins - max, ent.maxs - min)?;
                let offset = ent.origin;

                Ok((hull, offset))
            }
        }
    }

    pub fn entity_is_stuck(&self, world: &World, e_id: Entity) -> Result<bool, ServerError> {
        let ent = phys(world, e_id)?;

        let origin = ent.origin;
        let min = ent.mins;
        let max = ent.maxs;
        let (trace, _) =
            self.trace_entity_move(world, e_id, origin, min, max, origin, CollideKind::Normal)?;

        Ok(trace.start_solid())
    }

    /// Traces a box moving from `start` to `end` against the world and the solid entities, ignoring
    /// `e_id` (and the entities it owns or is owned by). Returns the trace and the entity hit, if any.
    // TODO: Make this a struct (which Quake does too)
    #[allow(clippy::too_many_arguments)]
    pub fn trace_entity_move(
        &self,
        world: &World,
        e_id: Entity,
        start: Vec3,
        min: Vec3,
        max: Vec3,
        end: Vec3,
        kind: CollideKind,
    ) -> Result<(Trace, Option<Entity>), ServerError> {
        debug!("start={start} min={min} max={max} end={end}",);

        let worldspawn = world.resource::<EdictTable>().worldspawn();

        debug!("Collision test: Entity {e_id} with world entity");
        let world_trace = self.trace(world, start, min, max, end)?;

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

        let (move_min, move_max) =
            self::phys::bounds_for_move(start, monster_min, monster_max, end);

        let collide = Collide {
            e_id: Some(e_id),
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

        let (collide_trace, collide_ent) = self.collide(world, &collide)?;

        if collide_trace.all_solid()
            || collide_trace.start_solid()
            || collide_trace.ratio() < world_trace.ratio()
        {
            Ok((collide_trace, collide_ent))
        } else {
            // As in Quake's `SV_ClipMoveToEntity`, the trace only has an entity if it hit
            // something.
            let hit_world = !world_trace.is_terminal() || world_trace.start_solid();
            Ok((world_trace, hit_world.then_some(worldspawn)))
        }
    }

    pub fn collide(
        &self,
        world: &World,
        collide: &Collide,
    ) -> Result<(Trace, Option<Entity>), ServerError> {
        let mut best = (Trace::uninitialized(collide.start, collide.end), None);
        self.collide_area(world, 0, collide, &mut best)?;
        Ok(best)
    }

    /// Traces `collide` against the solid entities in area node `area_id` and its children.
    ///
    /// `best` holds the closest hit found so far and the entity that was hit, and is only
    /// replaced (both parts together) when a closer hit is found, as in Quake's `SV_ClipToLinks`.
    fn collide_area(
        &self,
        world: &World,
        area_id: usize,
        collide: &Collide,
        best: &mut (Trace, Option<Entity>),
    ) -> Result<(), ServerError> {
        let area = &self.area_nodes[area_id];

        'collide_entity: for touch in area
            .solids
            .iter()
            .filter_map(|&e| edict_entity(world, e))
            .filter(|e| Some(*e) != collide.e_id)
        {
            if best.0.all_solid() {
                return Ok(());
            }

            let touch_ent = phys(world, touch)?;

            match touch_ent.solid()? {
                // if the other entity has no collision, skip it
                EntitySolid::Not => continue,

                // triggers should not appear in the solids list
                EntitySolid::Trigger => {
                    return Err(ServerError::with_msg(format!("Trigger in solids list ({touch})")));
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
                && phys(world, e)?.size[0] != 0.0
                && touch_ent.size[0] == 0.0
            {
                continue;
            }

            if let Some(e) = collide.e_id {
                // don't collide against owner or owned entities
                let owner = |e: Entity| {
                    world
                        .get::<crate::server::qc::fields::EntAi>(e)
                        .map(|ai| ai.owner)
                        .ok_or_else(|| no_such_entity(e))
                };
                if owner(touch)? == e || owner(e)? == touch {
                    continue;
                }
            }

            // select bounding boxes based on whether or not candidate is a monster
            let tmp_trace = if touch_ent.flags().contains(EntityFlags::MONSTER) {
                self.collide_move_with_entity(
                    world,
                    touch,
                    collide.start,
                    collide.monster_min,
                    collide.monster_max,
                    collide.end,
                )?
            } else {
                self.collide_move_with_entity(
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
                self.collide_area(world, b.front, collide, best)?;
            }

            if collide.move_min[b.axis as usize] < b.dist {
                self.collide_area(world, b.back, collide, best)?;
            }
        }

        Ok(())
    }

    pub fn collide_move_with_entity(
        &self,
        world: &World,
        e_id: Entity,
        start: Vec3,
        min: Vec3,
        max: Vec3,
        end: Vec3,
    ) -> Result<Trace, ServerError> {
        let (hull, offset) = self.hull_for_entity(world, e_id, min, max)?;
        debug!("hull offset for {e_id}: {offset}");
        debug!("hull contents at start: {:?}", hull.contents_at_point(start).ok());

        Ok(hull.trace(start - offset, end - offset)?.adjust(offset))
    }

    /// Traces against the world model.
    pub fn trace(
        &self,
        world: &World,
        start: Vec3,
        min: Vec3,
        max: Vec3,
        end: Vec3,
    ) -> Result<Trace, ServerError> {
        let worldspawn = world.resource::<EdictTable>().worldspawn();
        self.collide_move_with_entity(world, worldspawn, start, min, max, end)
    }
}
