//! Collision hulls for brush models, and traces through them.

use std::{collections::HashSet, sync::Arc};

use bevy::{
    log::debug,
    math::{FloatExt as _, Vec3},
};
use num::FromPrimitive as _;
use num_derive::FromPrimitive;
use qbsp::{
    BspData,
    data::nodes::{BspLeafContentFlags, BspNodeRef, BspPlaneType},
};
use snafu::prelude::*;

use crate::{
    common::math::{Hyperplane, HyperplaneSide, LinePlaneIntersect},
    common::world::phys::{Trace, TraceEnd, TraceEndBoundary, TraceEndKind},
};

/// This is 4 in the original source, but the 4th hull is never used.
const MAX_HULLS: usize = 3;

/// Bounds of the player-sized collision hull (hull 1).
const HULL_1_BOUNDS: (Vec3, Vec3) = (Vec3::new(-16., -16., -24.), Vec3::new(16., 16., 32.));
/// Bounds of the large collision hull (hull 2), used for e.g. shamblers.
const HULL_2_BOUNDS: (Vec3, Vec3) = (Vec3::new(-32., -32., -24.), Vec3::new(32., 32., 64.));

#[derive(Snafu, Debug)]
pub enum HullError {
    #[snafu(display("min bound {mins} exceeds max bound {maxs}"))]
    InvalidBounds { mins: Vec3, maxs: Vec3 },
    #[snafu(display("Invalid hull index ({index})"))]
    InvalidHullIndex { index: usize },
    #[snafu(display("BSP has no per-size collision hulls (BSP38 is not supported)"))]
    NoSizeHulls,
    #[snafu(display("Model {model} has a leaf as the root of hull {hull}"))]
    LeafRoot { model: usize, hull: usize },
    #[snafu(display("Unsupported leaf contents {contents:?}"))]
    UnsupportedLeafFlags { contents: BspLeafContentFlags },
    #[snafu(display("Invalid clip node leaf contents ({contents})"))]
    InvalidClipContents { contents: i32 },
}

/// The contents of a leaf in the BSP tree, specifying how it should look and behave.
#[derive(Default, Copy, Clone, Debug, Eq, FromPrimitive, PartialEq)]
pub enum BspLeafContents {
    /// The leaf has nothing in it. Vision is unobstructed and movement is unimpeded.
    #[default]
    Empty = 1,

    /// The leaf is solid. Physics objects will collide with its surface and may not move inside it.
    Solid = 2,

    /// The leaf is full of water. Vision is warped to simulate refraction and movement is done by
    /// swimming instead of walking.
    Water = 3,

    /// The leaf is full of acidic slime. Vision is tinted green, movement is done by swimming and
    /// entities take periodic minor damage.
    Slime = 4,

    /// The leaf is full of lava. Vision is tinted red, movement is done by swimming and entities
    /// take periodic severe damage.
    Lava = 5,

    // This doesn't appear to ever be used
    Sky = 6,

    // This is removed during map compilation
    // Origin = 7,

    // This is converted to `BspLeafContents::Solid`
    // Collide = 8,
    /// Same as `BspLeafContents::Water`, but the player is constantly pushed in the positive
    /// x-direction (east).
    Current0 = 9,

    /// Same as `BspLeafContents::Water`, but the player is constantly pushed in the positive
    /// y-direction (north).
    Current90 = 10,

    /// Same as `BspLeafContents::Water`, but the player is constantly pushed in the negative
    /// x-direction (west).
    Current180 = 11,

    /// Same as `BspLeafContents::Water`, but the player is constantly pushed in the negative
    /// y-direction (south).
    Current270 = 12,

    /// Same as `BspLeafContents::Water`, but the player is constantly pushed in the positive
    /// z-direction (up).
    CurrentUp = 13,

    /// Same as `BspLeafContents::Water`, but the player is constantly pushed in the negative
    /// z-direction (down).
    CurrentDown = 14,
}

#[derive(Debug)]
pub enum BspCollisionNodeChild {
    Node(usize),
    Contents(BspLeafContents),
}

#[derive(Debug)]
pub struct BspCollisionNode {
    plane_id: usize,
    children: [BspCollisionNodeChild; 2],
}

#[derive(Debug)]
pub struct BspCollisionHull {
    planes: Arc<[Hyperplane]>,
    nodes: Arc<[BspCollisionNode]>,
    node_id: usize,
    mins: Vec3,
    maxs: Vec3,
}

impl BspCollisionHull {
    // TODO: see if we can't make this a little less baffling
    /// Constructs a collision hull with the given minimum and maximum bounds.
    ///
    /// This generates six planes which intersect to form a rectangular prism. The interior of the
    /// prism is `BspLeafContents::Solid`; the exterior is `BspLeafContents::Empty`.
    pub fn for_bounds(mins: Vec3, maxs: Vec3) -> Result<BspCollisionHull, HullError> {
        Self::for_bounds_with_contents(mins, maxs, BspLeafContents::Solid)
    }

    /// Like [`BspCollisionHull::for_bounds`], but with the given `contents` inside the prism.
    fn for_bounds_with_contents(
        mins: Vec3,
        maxs: Vec3,
        contents: BspLeafContents,
    ) -> Result<BspCollisionHull, HullError> {
        debug!("Generating collision hull for min = {:?} max = {:?}", mins, maxs);

        if mins.x > maxs.x || mins.y > maxs.y || mins.z > maxs.z {
            return InvalidBoundsSnafu { mins, maxs }.fail();
        }

        let mut nodes = Vec::new();
        let mut planes = Vec::new();

        // front plane (positive x)
        planes.push(Hyperplane::axis_x(maxs.x));
        nodes.push(BspCollisionNode {
            plane_id: 0,
            children: [
                BspCollisionNodeChild::Contents(BspLeafContents::Empty),
                BspCollisionNodeChild::Node(1),
            ],
        });

        // back plane (negative x)
        planes.push(Hyperplane::axis_x(mins.x));
        nodes.push(BspCollisionNode {
            plane_id: 1,
            children: [
                BspCollisionNodeChild::Node(2),
                BspCollisionNodeChild::Contents(BspLeafContents::Empty),
            ],
        });

        // left plane (positive y)
        planes.push(Hyperplane::axis_y(maxs.y));
        nodes.push(BspCollisionNode {
            plane_id: 2,
            children: [
                BspCollisionNodeChild::Contents(BspLeafContents::Empty),
                BspCollisionNodeChild::Node(3),
            ],
        });

        // right plane (negative y)
        planes.push(Hyperplane::axis_y(mins.y));
        nodes.push(BspCollisionNode {
            plane_id: 3,
            children: [
                BspCollisionNodeChild::Node(4),
                BspCollisionNodeChild::Contents(BspLeafContents::Empty),
            ],
        });

        // top plane (positive z)
        planes.push(Hyperplane::axis_z(maxs.z));
        nodes.push(BspCollisionNode {
            plane_id: 4,
            children: [
                BspCollisionNodeChild::Contents(BspLeafContents::Empty),
                BspCollisionNodeChild::Node(5),
            ],
        });

        // bottom plane (negative z)
        planes.push(Hyperplane::axis_z(mins.z));
        nodes.push(BspCollisionNode {
            plane_id: 5,
            children: [
                BspCollisionNodeChild::Contents(contents),
                BspCollisionNodeChild::Contents(BspLeafContents::Empty),
            ],
        });

        Ok(BspCollisionHull {
            planes: planes.into_boxed_slice().into(),
            nodes: nodes.into_boxed_slice().into(),
            node_id: 0,
            mins,
            maxs,
        })
    }

    pub fn mins(&self) -> Vec3 {
        self.mins
    }

    pub fn maxs(&self) -> Vec3 {
        self.maxs
    }

    /// Returns the leaf contents at the given point in this hull.
    pub fn contents_at_point(&self, point: Vec3) -> Result<BspLeafContents, HullError> {
        self.contents_at_point_node(self.node_id, point)
    }

    fn contents_at_point_node(
        &self,
        node: usize,
        point: Vec3,
    ) -> Result<BspLeafContents, HullError> {
        let mut current_node = &self.nodes[node];

        loop {
            let plane = &self.planes[current_node.plane_id];

            match current_node.children[plane.point_side(point) as usize] {
                BspCollisionNodeChild::Contents(c) => return Ok(c),
                BspCollisionNodeChild::Node(n) => current_node = &self.nodes[n],
            }
        }
    }

    pub fn trace(&self, start: Vec3, end: Vec3) -> Result<Trace, HullError> {
        let mut trace = Trace::uninitialized(start, end);
        // Trace is all-solid until it enters a non-solid leaf.
        // See `recursive_trace` or Quake's `SV_ClipMoveToEntity`.
        trace.all_solid = true;

        let _ = self.recursive_trace(
            &BspCollisionNodeChild::Node(self.node_id),
            start,
            end,
            0.,
            1.,
            &mut trace,
        )?;

        Ok(trace)
    }

    fn recursive_trace(
        &self,
        node: &BspCollisionNodeChild,
        start: Vec3,
        end: Vec3,
        start_ratio: f32,
        end_ratio: f32,
        trace: &mut Trace,
    ) -> Result<bool, HullError> {
        debug!("start={:?} end={:?}", start, end);
        let (node, plane) = match node {
            // this is an internal node, keep searching for a leaf
            BspCollisionNodeChild::Node(n) => {
                let node = &self.nodes[*n];
                let plane = &self.planes[node.plane_id];

                (node, plane)
            }

            // start -> end falls entirely inside a leaf
            BspCollisionNodeChild::Contents(c) => {
                debug!("Found leaf with contents {:?}", c);
                if *c == BspLeafContents::Solid {
                    trace.start_solid = true;
                } else {
                    trace.all_solid = false;
                    if *c == BspLeafContents::Empty {
                        trace.in_open = true;
                    } else {
                        trace.in_water = true;
                    }
                }

                return Ok(true);
            }
        };

        match plane.line_segment_intersection(start, end, start_ratio, end_ratio) {
            // start -> end falls entirely on one side of the plane
            LinePlaneIntersect::NoIntersection(side) => {
                debug!("No intersection");
                self.recursive_trace(
                    &node.children[side as usize],
                    start,
                    end,
                    start_ratio,
                    end_ratio,
                    trace,
                )
            }

            // start -> end crosses the plane at one point
            LinePlaneIntersect::PointIntersection(point_intersect) => {
                let near_side = plane.point_side(start);
                let far_side = -near_side;
                let mid = point_intersect.point();
                let mid_ratio = point_intersect.ratio();
                debug!("Intersection at {:?} (ratio={})", mid, mid_ratio);

                // calculate the near subtrace
                if !self.recursive_trace(
                    &node.children[near_side as usize],
                    start,
                    mid,
                    start_ratio,
                    mid_ratio,
                    trace,
                )? {
                    return Ok(false);
                }

                let far_contents = match &node.children[far_side as usize] {
                    BspCollisionNodeChild::Contents(c) => *c,
                    BspCollisionNodeChild::Node(far_n) => {
                        self.contents_at_point_node(*far_n, mid)?
                    }
                };

                if far_contents != BspLeafContents::Solid {
                    // Continue from the crossing point: the part of the segment before `mid` is
                    // on the near side of this plane, so must not be tested against the far
                    // subtree.
                    return self.recursive_trace(
                        &node.children[far_side as usize],
                        mid,
                        end,
                        mid_ratio,
                        end_ratio,
                        trace,
                    );
                }

                if trace.all_solid {
                    return Ok(false);
                }

                let hit_plane = match far_side {
                    HyperplaneSide::Positive => plane.to_owned(),
                    HyperplaneSide::Negative => -plane.to_owned(),
                };

                // Float rounding can still leave `mid` inside the solid, so back up until it
                // isn't. Quake: "shouldn't really happen, but does occasionally".
                let mut frac = point_intersect.relative_ratio();
                let (mut mid, mut mid_ratio) = (mid, mid_ratio);
                while self.contents_at_point_node(self.node_id, mid)? == BspLeafContents::Solid {
                    frac -= 0.1;
                    if frac < 0. {
                        debug!("Trace backed up past its start");
                        break;
                    }
                    mid = start.lerp(end, frac);
                    mid_ratio = start_ratio.lerp(end_ratio, frac);
                }

                trace.end = TraceEnd {
                    point: mid,
                    kind: TraceEndKind::Boundary(TraceEndBoundary {
                        ratio: mid_ratio,
                        plane: hit_plane,
                    }),
                };

                Ok(false)
            }
        }
    }

    pub fn gen_dot_graph(&self) -> String {
        let mut dot = String::new();
        dot += "digraph hull {\n";
        dot += "    rankdir=LR\n";

        let mut rank_lists = Vec::new();
        let mut leaf_names = Vec::new();

        dot += &self.gen_dot_graph_recursive(0, &mut rank_lists, &mut leaf_names, self.node_id);

        for rank in rank_lists {
            dot += "    {rank=same;";
            for node_id in rank {
                dot += &format!("n{node_id},");
            }
            // discard trailing comma
            dot.pop().unwrap();
            dot += "}\n"
        }

        dot += "    {rank=same;";
        for leaf in leaf_names {
            dot += &format!("{leaf},");
        }
        // discard trailing comma
        dot.pop().unwrap();
        dot.pop().unwrap();
        dot += "}\n";

        dot += "}";

        dot
    }

    fn gen_dot_graph_recursive(
        &self,
        rank: usize,
        rank_lists: &mut Vec<HashSet<usize>>,
        leaf_names: &mut Vec<String>,
        node_id: usize,
    ) -> String {
        let mut result = String::new();

        if rank >= rank_lists.len() {
            rank_lists.push(HashSet::new());
        }

        rank_lists[rank].insert(node_id);

        for child in self.nodes[node_id].children.iter() {
            match child {
                BspCollisionNodeChild::Node(n) => {
                    result += &format!("    n{node_id} -> n{n}\n");
                    result += &self.gen_dot_graph_recursive(rank + 1, rank_lists, leaf_names, *n);
                }
                BspCollisionNodeChild::Contents(_) => {
                    let leaf_count = leaf_names.len();
                    let leaf_name = format!("l{leaf_count}");
                    result += &format!("    n{node_id} -> {leaf_name}\n");
                    leaf_names.push(leaf_name);
                }
            }
        }

        result
    }
}

/// The collision data shared between all models in a single BSP file.
#[derive(Debug)]
pub struct BspHulls {
    planes: Arc<[Hyperplane]>,
    /// Collision nodes for hull 0, generated from the render nodes. See `Mod_MakeHull0`.
    render_nodes: Arc<[BspCollisionNode]>,
    /// Collision nodes for hulls 1 and 2.
    clip_nodes: Arc<[BspCollisionNode]>,
}

/// The collision data for a brush model (either the world, a `*N` submodel or an external `.bsp`).
#[derive(Debug, Clone)]
pub struct BrushModel {
    hulls: Arc<BspHulls>,
    min: Vec3,
    max: Vec3,
    origin: Vec3,
    /// The root node of each hull for this model.
    head_nodes: [usize; MAX_HULLS],
}

fn plane_from_bsp(plane: &qbsp::data::nodes::BspPlane) -> Hyperplane {
    match plane.ty {
        BspPlaneType::AxialX => Hyperplane::axis_x(plane.dist),
        BspPlaneType::AxialY => Hyperplane::axis_y(plane.dist),
        BspPlaneType::AxialZ => Hyperplane::axis_z(plane.dist),
        _ => Hyperplane::new(plane.normal, plane.dist),
    }
}

fn contents_from_flags(flags: BspLeafContentFlags) -> Result<BspLeafContents, HullError> {
    type F = BspLeafContentFlags;

    // TODO: qbsp maps BSP29 Sky to SOLID, so `BspLeafContents::Sky` is never produced for hull 0.
    let contents = if flags.is_empty() {
        BspLeafContents::Empty
    } else if flags.contains(F::SOLID) {
        BspLeafContents::Solid
    } else if flags.contains(F::WATER) {
        BspLeafContents::Water
    } else if flags.contains(F::SLIME) {
        BspLeafContents::Slime
    } else if flags.contains(F::LAVA) {
        BspLeafContents::Lava
    } else if flags.contains(F::CURRENT_0) {
        BspLeafContents::Current0
    } else if flags.contains(F::CURRENT_90) {
        BspLeafContents::Current90
    } else if flags.contains(F::CURRENT_180) {
        BspLeafContents::Current180
    } else if flags.contains(F::CURRENT_270) {
        BspLeafContents::Current270
    } else if flags.contains(F::CURRENT_UP) {
        BspLeafContents::CurrentUp
    } else if flags.contains(F::CURRENT_DOWN) {
        BspLeafContents::CurrentDown
    } else {
        return UnsupportedLeafFlagsSnafu { contents: flags }.fail();
    };

    Ok(contents)
}

impl BrushModel {
    /// Builds the collision data for every model in a BSP file. Model 0 is the world model.
    pub fn from_bsp_data(data: &BspData) -> Result<Vec<BrushModel>, HullError> {
        let planes: Arc<[Hyperplane]> = data.planes.iter().map(plane_from_bsp).collect();

        // See `Mod_MakeHull0`,
        // https://github.com/id-Software/Quake/blob/master/WinQuake/gl_model.c#L1001-L1031
        //
        // This essentially duplicates the render nodes into a tree of collision nodes.
        let render_child = |node_ref: BspNodeRef| -> Result<BspCollisionNodeChild, HullError> {
            Ok(match node_ref {
                BspNodeRef::Node(n) => BspCollisionNodeChild::Node(n as usize),
                BspNodeRef::Leaf(l) => BspCollisionNodeChild::Contents(contents_from_flags(
                    data.leaves[l as usize].contents.0,
                )?),
            })
        };
        let render_nodes = data
            .nodes
            .iter()
            .map(|node| {
                Ok(BspCollisionNode {
                    plane_id: node.plane_idx as usize,
                    children: [render_child(*node.front)?, render_child(*node.back)?],
                })
            })
            .collect::<Result<Arc<[_]>, HullError>>()?;

        // For clip nodes, a leaf "index" is actually the negated leaf contents, i.e. the raw value
        // in the file is `!index`. We reverse that to get back the original contents, which
        // (unlike hull 0) preserves `Sky`.
        let clip_child = |node_ref: BspNodeRef| -> Result<BspCollisionNodeChild, HullError> {
            Ok(match node_ref {
                BspNodeRef::Node(n) => BspCollisionNodeChild::Node(n as usize),
                BspNodeRef::Leaf(l) => {
                    let contents = -!(l as i32);
                    BspCollisionNodeChild::Contents(
                        BspLeafContents::from_i32(contents)
                            .context(InvalidClipContentsSnafu { contents })?,
                    )
                }
            })
        };
        let clip_nodes = data
            .clip_nodes
            .iter()
            .map(|node| {
                Ok(BspCollisionNode {
                    plane_id: node.plane_idx as usize,
                    children: [clip_child(*node.front)?, clip_child(*node.back)?],
                })
            })
            .collect::<Result<Arc<[_]>, HullError>>()?;

        let hulls = Arc::new(BspHulls { planes, render_nodes, clip_nodes });

        data.models
            .iter()
            .enumerate()
            .map(|(i, model)| {
                let for_size = model.hulls.for_size.context(NoSizeHullsSnafu)?;

                let mut head_nodes = [0; MAX_HULLS];
                for (hull, (head_node, root)) in head_nodes
                    .iter_mut()
                    .zip([model.hulls.root, for_size.small, for_size.large])
                    .enumerate()
                {
                    *head_node = match root {
                        BspNodeRef::Node(n) => n as usize,
                        BspNodeRef::Leaf(_) => return LeafRootSnafu { model: i, hull }.fail(),
                    };
                }

                Ok(BrushModel {
                    hulls: hulls.clone(),
                    // pad the bounding box by one unit in all directions
                    min: model.bound.min - Vec3::ONE,
                    max: model.bound.max + Vec3::ONE,
                    origin: model.origin,
                    head_nodes,
                })
            })
            .collect()
    }

    /// Returns the minimum extent of this brush model.
    pub fn min(&self) -> Vec3 {
        self.min
    }

    /// Returns the maximum extent of this brush model.
    pub fn max(&self) -> Vec3 {
        self.max
    }

    /// Returns the size of this brush model.
    pub fn size(&self) -> Vec3 {
        self.max - self.min
    }

    /// Returns the origin of this brush model.
    pub fn origin(&self) -> Vec3 {
        self.origin
    }

    /// Returns the root node of each collision hull of this model.
    pub fn head_nodes(&self) -> [usize; MAX_HULLS] {
        self.head_nodes
    }

    /// Returns collision hull `index` (0 for points, 1 for player-sized and 2 for large entities).
    pub fn hull(&self, index: usize) -> Result<BspCollisionHull, HullError> {
        let (nodes, (mins, maxs)) = match index {
            0 => (&self.hulls.render_nodes, (Vec3::ZERO, Vec3::ZERO)),
            1 => (&self.hulls.clip_nodes, HULL_1_BOUNDS),
            2 => (&self.hulls.clip_nodes, HULL_2_BOUNDS),
            _ => return InvalidHullIndexSnafu { index }.fail(),
        };

        Ok(BspCollisionHull {
            planes: self.hulls.planes.clone(),
            nodes: nodes.clone(),
            node_id: self.head_nodes[index],
            mins,
            maxs,
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_hull_for_bounds() {
        let hull = BspCollisionHull::for_bounds(Vec3::ZERO, Vec3::new(1.0, 1.0, 1.0)).unwrap();

        let empty_points = vec![
            // points strictly less than hull min should be empty
            Vec3::new(-1.0, -1.0, -1.0),
            // points strictly greater than hull max should be empty
            Vec3::new(2.0, 2.0, 2.0),
            // points in front of hull should be empty
            Vec3::new(2.0, 0.5, 0.5),
            // points behind hull should be empty
            Vec3::new(-1.0, 0.5, 0.5),
            // points left of hull should be empty
            Vec3::new(0.5, 2.0, 0.5),
            // points right of hull should be empty
            Vec3::new(0.5, -1.0, 0.5),
            // points above hull should be empty
            Vec3::new(0.5, 0.5, 2.0),
            // points below hull should be empty
            Vec3::new(0.5, 0.5, -1.0),
        ];

        for point in empty_points {
            assert_eq!(hull.contents_at_point(point).unwrap(), BspLeafContents::Empty);
        }

        let solid_points = vec![
            // center of the hull should be solid
            Vec3::new(0.5, 0.5, 0.5),
            // various interior corners should be solid
            Vec3::new(0.01, 0.01, 0.01),
            Vec3::new(0.99, 0.01, 0.01),
            Vec3::new(0.01, 0.99, 0.01),
            Vec3::new(0.01, 0.01, 0.99),
            Vec3::new(0.99, 0.99, 0.01),
            Vec3::new(0.99, 0.01, 0.99),
            Vec3::new(0.01, 0.99, 0.99),
            Vec3::new(0.99, 0.99, 0.99),
        ];

        for point in solid_points {
            assert_eq!(hull.contents_at_point(point).unwrap(), BspLeafContents::Solid);
        }
    }

    fn unit_box(contents: BspLeafContents) -> BspCollisionHull {
        BspCollisionHull::for_bounds_with_contents(Vec3::splat(-1.), Vec3::splat(1.), contents)
            .unwrap()
    }

    #[test]
    fn test_trace_through_water_is_in_water() {
        let hull = unit_box(BspLeafContents::Water);
        let trace = hull.trace(Vec3::new(-10., 0., 0.), Vec3::new(10., 0., 0.)).unwrap();
        assert!(trace.in_water);
        assert!(trace.in_open);
        assert!(!trace.all_solid());
        assert!(trace.is_terminal());
    }

    #[test]
    fn test_trace_inside_solid_is_all_solid() {
        let hull = unit_box(BspLeafContents::Solid);
        let trace = hull.trace(Vec3::new(-0.5, 0., 0.), Vec3::new(0.5, 0., 0.)).unwrap();
        assert!(trace.all_solid());
        assert!(trace.start_solid());
    }

    #[test]
    fn test_trace_in_open_is_not_all_solid() {
        let hull = unit_box(BspLeafContents::Solid);
        let trace = hull.trace(Vec3::new(5., 5., 5.), Vec3::new(6., 6., 6.)).unwrap();
        assert!(!trace.all_solid());
        assert!(!trace.start_solid());
        assert!(trace.in_open);
        assert!(trace.is_terminal());
    }

    #[test]
    fn test_trace_hits_box_at_analytic_ratio() {
        let hull = unit_box(BspLeafContents::Solid);
        let trace = hull.trace(Vec3::new(-10., 0., 0.), Vec3::new(10., 0., 0.)).unwrap();
        assert!(!trace.all_solid());
        assert!(!trace.start_solid());
        // The box's near face is at x = -1, 9 units along a 20-unit trace.
        assert!((trace.ratio() - 9. / 20.).abs() < 0.01, "ratio = {}", trace.ratio());
        assert!((trace.end_point().x + 1.).abs() < 0.1, "end = {}", trace.end_point());
        // Hit planes face into the solid (cf. `Hyperplane::is_floor`).
        assert_eq!(trace.plane().unwrap().normal(), Vec3::X);
    }

    /// A hull which is solid behind a single (non-axial) plane.
    fn half_space(normal: Vec3, dist: f32) -> BspCollisionHull {
        BspCollisionHull {
            planes: vec![Hyperplane::new(normal, dist)].into(),
            nodes: vec![BspCollisionNode {
                plane_id: 0,
                children: [
                    BspCollisionNodeChild::Contents(BspLeafContents::Empty),
                    BspCollisionNodeChild::Contents(BspLeafContents::Solid),
                ],
            }]
            .into(),
            node_id: 0,
            mins: Vec3::ZERO,
            maxs: Vec3::ZERO,
        }
    }

    #[test]
    fn test_trace_never_ends_in_solid_far_from_origin() {
        // Rounding error in plane distances grows with the size of the coordinates, so the end of
        // a trace must be backed off from the plane by enough to stay out of the solid. Quake uses
        // 1/32 of a unit plus a backup loop (`SV_RecursiveHullCheck`).
        let normal = Vec3::new(0.6, 0.8, 0.1).normalize();
        let base = Vec3::new(3517.3, -3901.7, 2911.1);
        let hull = half_space(normal, base.dot(normal));

        // Deterministic xorshift, so failures are reproducible.
        let mut state = 0x2545_f491_4f6c_dd1d_u64;
        let mut rand = move || {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            (state >> 40) as f32 / (1u64 << 24) as f32
        };

        let mut failures = 0;
        for _ in 0..10_000 {
            let jitter = Vec3::new(rand(), rand(), rand()) * 64. - 32.;
            let on_plane = base + jitter - normal * jitter.dot(normal);
            // Moves of up to ~16 units, which is one 20 Hz tick at full run speed.
            let start = on_plane + normal * (rand() * 8. + 0.05);
            let end = on_plane - normal * (rand() * 8. + 0.05) + Vec3::new(rand(), rand(), 0.) * 8.;

            let trace = hull.trace(start, end).unwrap();
            assert!(!trace.start_solid(), "start {start} began in solid");
            if hull.contents_at_point(trace.end_point()).unwrap() == BspLeafContents::Solid {
                failures += 1;
            }
        }

        assert_eq!(failures, 0, "{failures} traces ended inside the solid");
    }

    #[test]
    fn test_trace_past_corner_does_not_hit() {
        // This trace crosses the planes y = -1 and y = 1 while x < -1 (i.e. outside the box),
        // then crosses x = -1 at y = 2, missing the box entirely. The far side of the x = -1
        // plane must only be tested from the crossing point onwards.
        let hull = unit_box(BspLeafContents::Solid);
        let trace = hull.trace(Vec3::new(-3., -2., 0.), Vec3::new(0., 4., 0.)).unwrap();
        assert!(!trace.start_solid());
        assert!(!trace.all_solid());
        assert!(trace.is_terminal(), "unexpected hit: {trace:?}");
    }
}
