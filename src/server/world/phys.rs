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

//! Physics and collision detection.

use crate::{
    common::math::Hyperplane,
    server::progs::EntityId,
};

use bevy::prelude::*;
use bitflags::bitflags;
use num_derive::FromPrimitive;

/// Velocity in units/second under which a *component* (not the entire
/// velocity!) is instantly reduced to zero.
///
/// This prevents objects from sliding indefinitely at low velocity.
const STOP_THRESHOLD: f32 = 0.1;

#[derive(Copy, Clone, Debug, Eq, FromPrimitive, PartialEq)]
pub enum MoveKind {
    /// Does not move.
    None = 0,
    AngleNoClip = 1,
    AngleClip = 2,
    /// Player-controlled.
    Walk = 3,
    /// Moves in discrete steps (monsters).
    Step = 4,
    Fly = 5,
    Toss = 6,
    Push = 7,
    NoClip = 8,
    FlyMissile = 9,
    Bounce = 10,
}

#[derive(Copy, Clone, Debug, Eq, FromPrimitive, PartialEq, Default)]
pub enum CollideKind {
    #[default]
    Normal = 0,
    NoMonsters = 1,
    Missile = 2,
}

#[derive(Debug)]
pub struct Collide {
    /// The ID of the entity being moved.
    pub e_id: Option<EntityId>,

    /// The minimum extent of the entire move.
    pub move_min: Vec3,

    /// The maximum extent of the entire move.
    pub move_max: Vec3,

    /// The minimum extent of the moving object.
    pub min: Vec3,

    /// The maximum extent of the moving object.
    pub max: Vec3,

    /// The minimum extent of the moving object when colliding with a monster.
    pub monster_min: Vec3,

    /// The maximum extent of the moving object when colliding with a monster.
    pub monster_max: Vec3,

    /// The start point of the move.
    pub start: Vec3,

    /// The end point of the move.
    pub end: Vec3,

    /// How this move collides with other entities.
    pub kind: CollideKind,
}

/// Represents the start of a collision trace.
#[derive(Default, Clone, Debug)]
pub struct TraceStart {
    point: Vec3,
    /// The ratio along the original trace length at which this (sub)trace
    /// begins.
    ratio: f32,
}

impl TraceStart {
    pub fn new(point: Vec3, ratio: f32) -> TraceStart {
        TraceStart { point, ratio }
    }
}

/// Represents the end of a trace which crossed between leaves.
#[derive(Clone, Debug)]
pub struct TraceEndBoundary {
    pub ratio: f32,
    pub plane: Hyperplane,
}

/// Indicates the the nature of the end of a trace.
#[derive(Default, Clone, Debug)]
pub enum TraceEndKind {
    /// This endpoint falls within a leaf.
    #[default]
    Terminal,

    /// This endpoint falls on a leaf boundary (a plane).
    Boundary(TraceEndBoundary),
}

/// Represents the end of a trace.
#[derive(Default, Clone, Debug)]
pub struct TraceEnd {
    pub point: Vec3,
    pub kind: TraceEndKind,
}

impl TraceEnd {
    pub fn terminal(point: Vec3) -> TraceEnd {
        TraceEnd {
            point,
            kind: TraceEndKind::Terminal,
        }
    }

    pub fn boundary(point: Vec3, ratio: f32, plane: Hyperplane) -> TraceEnd {
        TraceEnd {
            point,
            kind: TraceEndKind::Boundary(TraceEndBoundary { ratio, plane }),
        }
    }

    pub fn kind(&self) -> &TraceEndKind {
        &self.kind
    }
}

#[derive(Default, Clone, Debug)]
pub struct Trace {
    pub start: TraceStart,
    pub end: TraceEnd,
    pub start_solid: bool,
    pub all_solid: bool,
    pub in_open: bool,
    pub in_water: bool,
}

impl Trace {
    pub fn uninitialized(start: Vec3, end: Vec3) -> Self {
        Self {
            start: TraceStart {
                point: start,
                ratio: 0.,
            },
            end: TraceEnd {
                point: end,
                kind: TraceEndKind::Terminal,
            },
            ..Default::default()
        }
    }

    pub fn plane(&self) -> Option<&Hyperplane> {
        match &self.end.kind {
            TraceEndKind::Boundary(boundary) => Some(&boundary.plane),
            _ => None,
        }
    }

    pub fn plane_dist(&self) -> Option<f32> {
        match &self.end.kind {
            TraceEndKind::Boundary(boundary) => Some(boundary.ratio),
            _ => None,
        }
    }

    /// Adjusts the start and end points of the trace by an offset.
    pub fn adjust(self, offset: Vec3) -> Trace {
        Trace {
            start: TraceStart {
                point: self.start.point + offset,
                ratio: self.start.ratio,
            },
            end: TraceEnd {
                point: self.end.point + offset,
                kind: self.end.kind,
            },
            start_solid: self.start_solid,
            all_solid: self.all_solid,
            in_open: self.in_open,
            in_water: self.in_water,
        }
    }

    /// Returns the point at which the trace began.
    pub fn start_point(&self) -> Vec3 {
        self.start.point
    }

    /// Returns the end of this trace.
    pub fn end(&self) -> &TraceEnd {
        &self.end
    }

    /// Returns the point at which the trace ended.
    pub fn end_point(&self) -> Vec3 {
        self.end.point
    }

    /// Returns true if the entire trace is within solid leaves.
    pub fn all_solid(&self) -> bool {
        self.all_solid
    }

    /// Returns true if the trace began in a solid leaf but ended outside it.
    pub fn start_solid(&self) -> bool {
        self.start_solid
    }

    pub fn set_start_solid(&mut self, start_solid: bool) {
        self.start_solid = start_solid;
    }

    /// Returns whether the trace ended without a collision.
    pub fn is_terminal(&self) -> bool {
        matches!(self.end.kind, TraceEndKind::Terminal)
    }

    /// Returns the ratio of travelled distance to intended distance.
    ///
    /// This indicates how far along the original trajectory the trace proceeded
    /// before colliding with a different medium.
    pub fn ratio(&self) -> f32 {
        match &self.end.kind {
            TraceEndKind::Terminal => 1.0,
            TraceEndKind::Boundary(boundary) => boundary.ratio,
        }
    }
}

bitflags! {
    pub struct CollisionFlags: u32 {
        const HORIZONTAL = 1;
        const VERTICAL = 2;
        const STOPPED = 4;
    }
}

pub fn bounds_for_move(start: Vec3, min: Vec3, max: Vec3, end: Vec3) -> (Vec3, Vec3) {
    let mut box_min = Vec3::ZERO;
    let mut box_max = Vec3::ZERO;

    for i in 0..3 {
        box_min[i] = start[i].min(end[i]) + min[i] - 1.0;
        box_max[i] = start[i].max(end[i]) + max[i] + 1.0;
    }

    (box_min, box_max)
}
