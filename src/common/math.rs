// Copyright © 2018 Cormac O'Brien
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

use std::{cmp::Ordering, ops::Neg};

use bevy::prelude::*;
use num_derive::FromPrimitive;

pub const VERTEX_NORMAL_COUNT: usize = 162;
/// Precomputed vertex normals used for alias models and particle effects
pub static VERTEX_NORMALS: &[Vec3; VERTEX_NORMAL_COUNT] = &[
    Vec3::new(-0.525731, 0.000000, 0.850651),
    Vec3::new(-0.442863, 0.238856, 0.864188),
    Vec3::new(-0.295242, 0.000000, 0.955423),
    Vec3::new(-0.309017, 0.500000, 0.809017),
    Vec3::new(-0.162460, 0.262866, 0.951056),
    Vec3::new(0.000000, 0.000000, 1.000000),
    Vec3::new(0.000000, 0.850651, 0.525731),
    Vec3::new(-0.147621, 0.716567, 0.681718),
    Vec3::new(0.147621, 0.716567, 0.681718),
    Vec3::new(0.000000, 0.525731, 0.850651),
    Vec3::new(0.309017, 0.500000, 0.809017),
    Vec3::new(0.525731, 0.000000, 0.850651),
    Vec3::new(0.295242, 0.000000, 0.955423),
    Vec3::new(0.442863, 0.238856, 0.864188),
    Vec3::new(0.162460, 0.262866, 0.951056),
    Vec3::new(-0.681718, 0.147621, 0.716567),
    Vec3::new(-0.809017, 0.309017, 0.500000),
    Vec3::new(-0.587785, 0.425325, 0.688191),
    Vec3::new(-0.850651, 0.525731, 0.000000),
    Vec3::new(-0.864188, 0.442863, 0.238856),
    Vec3::new(-0.716567, 0.681718, 0.147621),
    Vec3::new(-0.688191, 0.587785, 0.425325),
    Vec3::new(-0.500000, 0.809017, 0.309017),
    Vec3::new(-0.238856, 0.864188, 0.442863),
    Vec3::new(-0.425325, 0.688191, 0.587785),
    Vec3::new(-0.716567, 0.681718, -0.147621),
    Vec3::new(-0.500000, 0.809017, -0.309017),
    Vec3::new(-0.525731, 0.850651, 0.000000),
    Vec3::new(0.000000, 0.850651, -0.525731),
    Vec3::new(-0.238856, 0.864188, -0.442863),
    Vec3::new(0.000000, 0.955423, -0.295242),
    Vec3::new(-0.262866, 0.951056, -0.162460),
    Vec3::new(0.000000, 1.000000, 0.000000),
    Vec3::new(0.000000, 0.955423, 0.295242),
    Vec3::new(-0.262866, 0.951056, 0.162460),
    Vec3::new(0.238856, 0.864188, 0.442863),
    Vec3::new(0.262866, 0.951056, 0.162460),
    Vec3::new(0.500000, 0.809017, 0.309017),
    Vec3::new(0.238856, 0.864188, -0.442863),
    Vec3::new(0.262866, 0.951056, -0.162460),
    Vec3::new(0.500000, 0.809017, -0.309017),
    Vec3::new(0.850651, 0.525731, 0.000000),
    Vec3::new(0.716567, 0.681718, 0.147621),
    Vec3::new(0.716567, 0.681718, -0.147621),
    Vec3::new(0.525731, 0.850651, 0.000000),
    Vec3::new(0.425325, 0.688191, 0.587785),
    Vec3::new(0.864188, 0.442863, 0.238856),
    Vec3::new(0.688191, 0.587785, 0.425325),
    Vec3::new(0.809017, 0.309017, 0.500000),
    Vec3::new(0.681718, 0.147621, 0.716567),
    Vec3::new(0.587785, 0.425325, 0.688191),
    Vec3::new(0.955423, 0.295242, 0.000000),
    Vec3::new(1.000000, 0.000000, 0.000000),
    Vec3::new(0.951056, 0.162460, 0.262866),
    Vec3::new(0.850651, -0.525731, 0.000000),
    Vec3::new(0.955423, -0.295242, 0.000000),
    Vec3::new(0.864188, -0.442863, 0.238856),
    Vec3::new(0.951056, -0.162460, 0.262866),
    Vec3::new(0.809017, -0.309017, 0.500000),
    Vec3::new(0.681718, -0.147621, 0.716567),
    Vec3::new(0.850651, 0.000000, 0.525731),
    Vec3::new(0.864188, 0.442863, -0.238856),
    Vec3::new(0.809017, 0.309017, -0.500000),
    Vec3::new(0.951056, 0.162460, -0.262866),
    Vec3::new(0.525731, 0.000000, -0.850651),
    Vec3::new(0.681718, 0.147621, -0.716567),
    Vec3::new(0.681718, -0.147621, -0.716567),
    Vec3::new(0.850651, 0.000000, -0.525731),
    Vec3::new(0.809017, -0.309017, -0.500000),
    Vec3::new(0.864188, -0.442863, -0.238856),
    Vec3::new(0.951056, -0.162460, -0.262866),
    Vec3::new(0.147621, 0.716567, -0.681718),
    Vec3::new(0.309017, 0.500000, -0.809017),
    Vec3::new(0.425325, 0.688191, -0.587785),
    Vec3::new(0.442863, 0.238856, -0.864188),
    Vec3::new(0.587785, 0.425325, -0.688191),
    Vec3::new(0.688191, 0.587785, -0.425325),
    Vec3::new(-0.147621, 0.716567, -0.681718),
    Vec3::new(-0.309017, 0.500000, -0.809017),
    Vec3::new(0.000000, 0.525731, -0.850651),
    Vec3::new(-0.525731, 0.000000, -0.850651),
    Vec3::new(-0.442863, 0.238856, -0.864188),
    Vec3::new(-0.295242, 0.000000, -0.955423),
    Vec3::new(-0.162460, 0.262866, -0.951056),
    Vec3::new(0.000000, 0.000000, -1.000000),
    Vec3::new(0.295242, 0.000000, -0.955423),
    Vec3::new(0.162460, 0.262866, -0.951056),
    Vec3::new(-0.442863, -0.238856, -0.864188),
    Vec3::new(-0.309017, -0.500000, -0.809017),
    Vec3::new(-0.162460, -0.262866, -0.951056),
    Vec3::new(0.000000, -0.850651, -0.525731),
    Vec3::new(-0.147621, -0.716567, -0.681718),
    Vec3::new(0.147621, -0.716567, -0.681718),
    Vec3::new(0.000000, -0.525731, -0.850651),
    Vec3::new(0.309017, -0.500000, -0.809017),
    Vec3::new(0.442863, -0.238856, -0.864188),
    Vec3::new(0.162460, -0.262866, -0.951056),
    Vec3::new(0.238856, -0.864188, -0.442863),
    Vec3::new(0.500000, -0.809017, -0.309017),
    Vec3::new(0.425325, -0.688191, -0.587785),
    Vec3::new(0.716567, -0.681718, -0.147621),
    Vec3::new(0.688191, -0.587785, -0.425325),
    Vec3::new(0.587785, -0.425325, -0.688191),
    Vec3::new(0.000000, -0.955423, -0.295242),
    Vec3::new(0.000000, -1.000000, 0.000000),
    Vec3::new(0.262866, -0.951056, -0.162460),
    Vec3::new(0.000000, -0.850651, 0.525731),
    Vec3::new(0.000000, -0.955423, 0.295242),
    Vec3::new(0.238856, -0.864188, 0.442863),
    Vec3::new(0.262866, -0.951056, 0.162460),
    Vec3::new(0.500000, -0.809017, 0.309017),
    Vec3::new(0.716567, -0.681718, 0.147621),
    Vec3::new(0.525731, -0.850651, 0.000000),
    Vec3::new(-0.238856, -0.864188, -0.442863),
    Vec3::new(-0.500000, -0.809017, -0.309017),
    Vec3::new(-0.262866, -0.951056, -0.162460),
    Vec3::new(-0.850651, -0.525731, 0.000000),
    Vec3::new(-0.716567, -0.681718, -0.147621),
    Vec3::new(-0.716567, -0.681718, 0.147621),
    Vec3::new(-0.525731, -0.850651, 0.000000),
    Vec3::new(-0.500000, -0.809017, 0.309017),
    Vec3::new(-0.238856, -0.864188, 0.442863),
    Vec3::new(-0.262866, -0.951056, 0.162460),
    Vec3::new(-0.864188, -0.442863, 0.238856),
    Vec3::new(-0.809017, -0.309017, 0.500000),
    Vec3::new(-0.688191, -0.587785, 0.425325),
    Vec3::new(-0.681718, -0.147621, 0.716567),
    Vec3::new(-0.442863, -0.238856, 0.864188),
    Vec3::new(-0.587785, -0.425325, 0.688191),
    Vec3::new(-0.309017, -0.500000, 0.809017),
    Vec3::new(-0.147621, -0.716567, 0.681718),
    Vec3::new(-0.425325, -0.688191, 0.587785),
    Vec3::new(-0.162460, -0.262866, 0.951056),
    Vec3::new(0.442863, -0.238856, 0.864188),
    Vec3::new(0.162460, -0.262866, 0.951056),
    Vec3::new(0.309017, -0.500000, 0.809017),
    Vec3::new(0.147621, -0.716567, 0.681718),
    Vec3::new(0.000000, -0.525731, 0.850651),
    Vec3::new(0.425325, -0.688191, 0.587785),
    Vec3::new(0.587785, -0.425325, 0.688191),
    Vec3::new(0.688191, -0.587785, 0.425325),
    Vec3::new(-0.955423, 0.295242, 0.000000),
    Vec3::new(-0.951056, 0.162460, 0.262866),
    Vec3::new(-1.000000, 0.000000, 0.000000),
    Vec3::new(-0.850651, 0.000000, 0.525731),
    Vec3::new(-0.955423, -0.295242, 0.000000),
    Vec3::new(-0.951056, -0.162460, 0.262866),
    Vec3::new(-0.864188, 0.442863, -0.238856),
    Vec3::new(-0.951056, 0.162460, -0.262866),
    Vec3::new(-0.809017, 0.309017, -0.500000),
    Vec3::new(-0.864188, -0.442863, -0.238856),
    Vec3::new(-0.951056, -0.162460, -0.262866),
    Vec3::new(-0.809017, -0.309017, -0.500000),
    Vec3::new(-0.681718, 0.147621, -0.716567),
    Vec3::new(-0.681718, -0.147621, -0.716567),
    Vec3::new(-0.850651, 0.000000, -0.525731),
    Vec3::new(-0.688191, 0.587785, -0.425325),
    Vec3::new(-0.587785, 0.425325, -0.688191),
    Vec3::new(-0.425325, 0.688191, -0.587785),
    Vec3::new(-0.425325, -0.688191, -0.587785),
    Vec3::new(-0.587785, -0.425325, -0.688191),
    Vec3::new(-0.688191, -0.587785, -0.425325),
];

#[derive(Clone, Copy, Debug)]
pub struct Angles {
    pub pitch: f32,
    pub roll: f32,
    pub yaw: f32,
}

impl Default for Angles {
    fn default() -> Self {
        Self::zero()
    }
}

impl Angles {
    pub fn zero() -> Angles {
        Angles {
            pitch: 0.,
            roll: 0.,
            yaw: 0.,
        }
    }

    pub fn mat3_quake(&self) -> Mat3 {
        Mat3::from_euler(
            EulerRot::XYZ,
            -self.roll.to_radians(),
            -self.pitch.to_radians(),
            self.yaw.to_radians(),
        )
    }

    pub fn mat4_quake(&self) -> Mat4 {
        Mat4::from_euler(
            EulerRot::XYZ,
            -self.roll.to_radians(),
            -self.pitch.to_radians(),
            self.yaw.to_radians(),
        )
    }

    pub fn mat3_wgpu(&self) -> Mat3 {
        Mat3::from_euler(
            EulerRot::ZXY,
            -self.roll.to_radians(),
            self.pitch.to_radians(),
            -self.yaw.to_radians(),
        )
    }

    pub fn mat4_wgpu(&self) -> Mat4 {
        Mat4::from_euler(
            EulerRot::ZXY,
            -self.roll.to_radians(),
            self.pitch.to_radians(),
            -self.yaw.to_radians(),
        )
    }
}

impl std::ops::Add for Angles {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            pitch: self.pitch + other.pitch,
            roll: self.roll + other.roll,
            yaw: self.yaw + other.yaw,
        }
    }
}

impl std::ops::Mul<f32> for Angles {
    type Output = Self;

    fn mul(self, other: f32) -> Self {
        Self {
            pitch: self.pitch * other,
            roll: self.roll * other,
            yaw: self.yaw * other,
        }
    }
}

pub fn clamp_deg(val: f32, min: f32, max: f32) -> f32 {
    assert!(min <= max);

    if val < min {
        min
    } else if val > max {
        max
    } else {
        val
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum HyperplaneSide {
    Positive = 0,
    Negative = 1,
}

impl Neg for HyperplaneSide {
    type Output = HyperplaneSide;

    fn neg(self) -> Self::Output {
        match self {
            HyperplaneSide::Positive => HyperplaneSide::Negative,
            HyperplaneSide::Negative => HyperplaneSide::Positive,
        }
    }
}

impl HyperplaneSide {
    // TODO: check this against the original game logic.
    pub fn from_dist(dist: f32) -> HyperplaneSide {
        if dist >= 0.0 {
            HyperplaneSide::Positive
        } else {
            HyperplaneSide::Negative
        }
    }
}

#[derive(Debug)]
/// The intersection of a line or segment and a plane at a point.
pub struct PointIntersection {
    // percentage of distance between start and end where crossover occurred
    ratio: f32,

    // crossover point
    point: Vec3,

    // plane crossed over
    plane: Hyperplane,
}

impl PointIntersection {
    pub fn ratio(&self) -> f32 {
        self.ratio
    }

    pub fn point(&self) -> Vec3 {
        self.point
    }

    pub fn plane(&self) -> &Hyperplane {
        &self.plane
    }
}

/// The intersection of a line or line segment with a plane.
///
/// A true mathematical representation would account for lines or segments contained entirely within
/// the plane, but here a distance of 0.0 is considered Positive. Thus, lines or segments contained
/// by the plane are considered to be `NoIntersection(Positive)`.
#[derive(Debug)]
pub enum LinePlaneIntersect {
    /// The line or line segment never intersects with the plane.
    NoIntersection(HyperplaneSide),

    /// The line or line segment intersects with the plane at precisely one point.
    PointIntersection(PointIntersection),
}

#[derive(Copy, Clone, Debug, FromPrimitive)]
pub enum Axis {
    X = 0,
    Y = 1,
    Z = 2,
}

#[derive(Clone, Debug)]
enum Alignment {
    Axis(Axis),
    Normal(Vec3),
}

#[derive(Clone, Debug)]
pub struct Hyperplane {
    alignment: Alignment,
    dist: f32,
}

impl Neg for Hyperplane {
    type Output = Self;

    fn neg(self) -> Self::Output {
        let normal = match self.alignment {
            Alignment::Axis(a) => {
                let mut n = Vec3::ZERO;
                n[a as usize] = -1.0;
                n
            }
            Alignment::Normal(n) => -n,
        };

        Hyperplane::new(normal, -self.dist)
    }
}

// see https://github.com/id-Software/Quake/blob/master/WinQuake/world.c#L525
const DISTANCE_EPSILON: f32 = 0.001;

/// Result of a move that could hit a wall.
// TODO: Can we return more info? Do we need to?
#[derive(Default)]
pub struct CollisionResult<T> {
    pub floor: bool,
    pub wall_or_step: bool,
    pub dead_stop: bool,
    pub metadata: T,
}

impl<T> CollisionResult<T> {
    pub fn any_collision(&self) -> bool {
        self.floor || self.wall_or_step || self.dead_stop
    }

    pub fn map<F, O>(self, func: F) -> CollisionResult<O>
    where
        F: FnOnce(T) -> O,
    {
        CollisionResult {
            floor: self.floor,
            wall_or_step: self.wall_or_step,
            dead_stop: self.dead_stop,
            metadata: func(self.metadata),
        }
    }
}

impl Hyperplane {
    /// Creates a new hyperplane aligned along the given normal, `dist` units away from the origin.
    ///
    /// If the given normal is equivalent to one of the axis normals, the hyperplane will be optimized
    /// to only consider that axis when performing point comparisons.
    pub fn new(normal: Vec3, dist: f32) -> Hyperplane {
        match normal {
            n if n == Vec3::X => Self::axis_x(dist),
            n if n == Vec3::Y => Self::axis_y(dist),
            n if n == Vec3::Z => Self::axis_z(dist),
            _ => Self::from_normal(normal, dist),
        }
    }

    /// Creates a new hyperplane aligned along the x-axis, `dist` units away from the origin.
    ///
    /// This hyperplane will only consider the x-axis when performing point comparisons.
    pub fn axis_x(dist: f32) -> Hyperplane {
        Hyperplane {
            alignment: Alignment::Axis(Axis::X),
            dist,
        }
    }

    /// Creates a new hyperplane aligned along the y-axis, `dist` units away from the origin.
    ///
    /// This hyperplane will only consider the y-axis when performing point comparisons.
    pub fn axis_y(dist: f32) -> Hyperplane {
        Hyperplane {
            alignment: Alignment::Axis(Axis::Y),
            dist,
        }
    }

    /// Creates a new hyperplane aligned along the z-axis, `dist` units away from the origin.
    ///
    /// This hyperplane will only consider the z-axis when performing point comparisons.
    pub fn axis_z(dist: f32) -> Hyperplane {
        Hyperplane {
            alignment: Alignment::Axis(Axis::Z),
            dist,
        }
    }

    /// Creates a new hyperplane aligned along the given normal, `dist` units away from the origin.
    ///
    /// This function will force the hyperplane alignment to be represented as a normal even if it
    /// is aligned along an axis.
    pub fn from_normal(normal: Vec3, dist: f32) -> Hyperplane {
        Hyperplane {
            alignment: Alignment::Normal(normal.normalize()),
            dist,
        }
    }

    pub fn is_floor(&self) -> bool {
        // TODO: Magic number
        self.normal().z < -0.7
    }

    /// Returns the surface normal of this plane.
    pub fn normal(&self) -> Vec3 {
        match self.alignment {
            Alignment::Axis(ax) => match ax {
                Axis::X => Vec3::X,
                Axis::Y => Vec3::Y,
                Axis::Z => Vec3::Z,
            },
            Alignment::Normal(normal) => normal,
        }
    }

    /// Calculates the shortest distance between this hyperplane and the given point.
    pub fn point_dist(&self, point: Vec3) -> f32 {
        match self.alignment {
            Alignment::Axis(a) => point[a as usize] - self.dist,
            Alignment::Normal(n) => point.dot(n) - self.dist,
        }
    }

    /// Calculates which side of this hyperplane the given point belongs to.
    ///
    /// Points with a distance of 0.0 are considered to be on the positive side.
    pub fn point_side(&self, point: Vec3) -> HyperplaneSide {
        match self.point_dist(point) {
            0f32.. => HyperplaneSide::Positive,
            _ => HyperplaneSide::Negative,
        }
    }

    /// Calculates the intersection of a line segment with this hyperplane.
    pub fn line_segment_intersection(
        &self,
        start: Vec3,
        end: Vec3,
        start_ratio: f32,
        end_ratio: f32,
    ) -> LinePlaneIntersect {
        let start_dist = self.point_dist(start);
        let end_dist = self.point_dist(end);

        debug!(
            "line_segment_intersection: alignment={:?} plane_dist={} start_dist={} end_dist={}",
            self.alignment, self.dist, start_dist, end_dist
        );

        let start_side = HyperplaneSide::from_dist(start_dist);
        let end_side = HyperplaneSide::from_dist(end_dist);

        // if both points fall on the same side of the hyperplane, there is no intersection
        if start_side == end_side {
            return LinePlaneIntersect::NoIntersection(start_side);
        }

        let start_dist = start_dist - start_dist.signum() * DISTANCE_EPSILON;

        let relative_ratio = (start_dist / (start_dist - end_dist)).clamp(0., 1.);

        // calculate how far along the segment the intersection occurred
        let absolute_ratio = start_ratio.lerp(end_ratio, relative_ratio);

        let point = start.lerp(end, relative_ratio);

        let plane = match start_side {
            HyperplaneSide::Positive => self.to_owned(),
            HyperplaneSide::Negative => -self.to_owned(),
        };

        LinePlaneIntersect::PointIntersection(PointIntersection {
            ratio: absolute_ratio,
            point,
            plane,
        })
    }

    /// Calculates a new velocity after collision with a surface.
    ///
    /// `overbounce` approximates the elasticity of the collision. A value of `1`
    /// reduces the component of `initial` antiparallel to `surface_normal` to zero,
    /// while a value of `2` reflects that component to be parallel to
    /// `surface_normal`.
    pub fn clip_velocity(&self, velocity: Vec3, overbounce: f32) -> CollisionResult<Vec3> {
        const STOP_THRESHOLD: f32 = 0.1;

        let backoff = velocity.dot(self.normal()) * overbounce;
        let out = velocity - self.normal() * backoff;
        let out = <[f32; 3]>::from(out)
            .map(|v| if v.abs() < STOP_THRESHOLD { 0. } else { v })
            .into();

        CollisionResult {
            floor: self.normal().z > 0.,
            wall_or_step: self.normal().z.abs() > f32::EPSILON,
            dead_stop: false,
            metadata: out,
        }
    }

    /// Calculates a new velocity after collision with multiple surfaces.
    pub fn clip_velocity_to_many<'a, I>(
        planes: I,
        velocity: Vec3,
        overbounce: f32,
    ) -> CollisionResult<Vec3>
    where
        I: IntoIterator,
        I::IntoIter: Iterator<Item = &'a Hyperplane> + Clone,
    {
        let mut planes = planes.into_iter();
        // Try to find a plane which produces a post-collision velocity that will
        // not cause a subsequent collision with any of the other planes.
        let new_velocity = planes.clone().enumerate().find_map(|(a, plane_a)| {
            let CollisionResult {
                metadata: new_velocity,
                ..
            } = plane_a.clip_velocity(velocity, overbounce);
            if planes
                .clone()
                .enumerate()
                .filter_map(|(b, plane_b)| Some(plane_b).filter(|_| a != b))
                .all(|plane_b| new_velocity.dot(plane_b.normal()) > 0.)
            {
                Some(new_velocity)
            } else {
                None
            }
        });

        new_velocity
            .map(|vel| CollisionResult {
                metadata: vel,
                ..Default::default()
            })
            .or_else(|| {
                let mut peekable_planes = planes.by_ref().peekable();
                // Go along the crease
                let first = peekable_planes.next()?;
                let second = peekable_planes.next()?;

                let dir = first.normal().cross(second.normal());
                let new_velocity = dir.clamp_length_min(dir.dot(velocity));

                if peekable_planes.peek().is_some() {
                    Some(Self::clip_velocity_to_many(
                        planes,
                        new_velocity,
                        overbounce,
                    ))
                } else {
                    Some(CollisionResult {
                        metadata: new_velocity,
                        ..Default::default()
                    })
                }
            })
            .unwrap_or(CollisionResult {
                floor: true,
                wall_or_step: true,
                dead_stop: true,
                metadata: Vec3::ZERO,
            })
    }
}

pub fn fov_x_to_fov_y(fov_x: f32, aspect: f32) -> Option<f32> {
    // aspect = tan(fov_x / 2) / tan(fov_y / 2)
    // tan(fov_y / 2) = tan(fov_x / 2) / aspect
    // fov_y / 2 = atan(tan(fov_x / 2) / aspect)
    // fov_y = 2 * atan(tan(fov_x / 2) / aspect)
    match fov_x {
        f if f < 0. => None,
        f if f > 360.0 => None,
        // TODO: This appears to be wrong but fixing it to be `f.to_radians()` doesn't seem to produce correct results(?)
        f => Some(f32::atan((f / 2.0).tan() / aspect) * 2.0),
    }
}

// see https://github.com/id-Software/Quake/blob/master/WinQuake/gl_rsurf.c#L1544
const COLLINEAR_EPSILON: f32 = 0.001;

/// Determines if the given points are collinear.
///
/// A set of points V is considered collinear if
/// norm(V<sub>1</sub> &minus; V<sub>0</sub>) &equals;
/// norm(V<sub>2</sub> &minus; V<sub>1</sub>) &equals;
/// .&nbsp;.&nbsp;. &equals;
/// norm(V<sub>k &minus; 1</sub> &minus; V<sub>k</sub>).
///
/// Special cases:
/// - If `vs.len() < 2`, always returns `false`.
/// - If `vs.len() == 2`, always returns `true`.
pub fn collinear(vs: &[Vec3]) -> bool {
    match vs.len() {
        l if l < 2 => false,
        2 => true,
        _ => {
            let init = (vs[1] - vs[0]).normalize();
            for i in 2..vs.len() {
                let norm = (vs[i] - vs[i - 1]).normalize();
                if (norm[0] - init[0]).abs() > COLLINEAR_EPSILON
                    || (norm[1] - init[1]).abs() > COLLINEAR_EPSILON
                    || (norm[2] - init[2]).abs() > COLLINEAR_EPSILON
                {
                    return false;
                }
            }

            true
        }
    }
}

pub fn remove_collinear(vs: Vec<Vec3>) -> Vec<Vec3> {
    assert!(vs.len() >= 3);

    let mut out = Vec::new();

    let mut vs_iter = vs.into_iter().cycle();
    let v_init = vs_iter.next().unwrap();
    let mut v1 = v_init;
    let mut v2 = vs_iter.next().unwrap();
    out.push(v1);
    for v3 in vs_iter {
        let tri = &[v1, v2, v3];

        if !collinear(tri) {
            out.push(v2);
        }

        if v3 == v_init {
            break;
        }

        v1 = v2;
        v2 = v3;
    }

    out
}

pub fn bounds<'a, I>(points: I) -> (Vec3, Vec3)
where
    I: IntoIterator<Item = &'a Vec3>,
{
    let mut min = Vec3::splat(32767.0);
    let mut max = Vec3::splat(-32768.0);
    for p in points.into_iter() {
        for c in 0..3 {
            min[c] = p[c].min(min[c]);
            max[c] = p[c].max(max[c]);
        }
    }
    (min, max)
}

pub fn vec2_extend_n(v: Vec2, n: usize, val: f32) -> Vec3 {
    let mut ar = [0.0; 3];
    for i in 0..3 {
        match i.cmp(&n) {
            Ordering::Less => ar[i] = v[i],
            Ordering::Equal => ar[i] = val,
            Ordering::Greater => ar[i] = v[i - 1],
        }
    }

    ar.into()
}

pub fn vec3_truncate_n(v: Vec3, n: usize) -> Vec2 {
    let mut ar = [0.0; 2];
    for i in 0..3 {
        match i.cmp(&n) {
            Ordering::Less => ar[i] = v[i],
            Ordering::Equal => (),
            Ordering::Greater => ar[i - 1] = v[i],
        }
    }
    ar.into()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_hyperplane_side_x() {
        let plane = Hyperplane::axis_x(1.0);
        assert_eq!(plane.point_side(Vec3::X * 2.0), HyperplaneSide::Positive);
        assert_eq!(plane.point_side(Vec3::X * -2.0), HyperplaneSide::Negative);
    }

    #[test]
    fn test_hyperplane_side_y() {
        let plane = Hyperplane::axis_y(1.0);
        assert_eq!(plane.point_side(Vec3::Y * 2.0), HyperplaneSide::Positive);
        assert_eq!(plane.point_side(Vec3::Y * -2.0), HyperplaneSide::Negative);
    }

    #[test]
    fn test_hyperplane_side_z() {
        let plane = Hyperplane::axis_z(1.0);
        assert_eq!(plane.point_side(Vec3::Z * 2.0), HyperplaneSide::Positive);
        assert_eq!(plane.point_side(Vec3::Z * -2.0), HyperplaneSide::Negative);
    }

    #[test]
    fn test_hyperplane_side_arbitrary() {
        // test 16 hyperplanes around the origin
        for x_comp in [1.0, -1.0].into_iter() {
            for y_comp in [1.0, -1.0].into_iter() {
                for z_comp in [1.0, -1.0].into_iter() {
                    for dist in [1, -1].into_iter() {
                        let base_vector = Vec3::new(x_comp, y_comp, z_comp);
                        let plane = Hyperplane::new(base_vector, dist as f32);
                        assert_eq!(
                            plane.point_side(Vec3::ZERO),
                            match dist {
                                1 => HyperplaneSide::Negative,
                                -1 => HyperplaneSide::Positive,
                                _ => unreachable!(),
                            }
                        );
                        assert_eq!(
                            plane.point_side(base_vector * 2.0 * dist as f32),
                            match dist {
                                1 => HyperplaneSide::Positive,
                                -1 => HyperplaneSide::Negative,
                                _ => unreachable!(),
                            }
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn test_hyperplane_point_dist_x() {
        let plane = Hyperplane::axis_x(1.0);
        assert_eq!(plane.point_dist(Vec3::X * 2.0), 1.0);
        assert_eq!(plane.point_dist(Vec3::ZERO), -1.0);
    }

    #[test]
    fn test_hyperplane_point_dist_y() {
        let plane = Hyperplane::axis_y(1.0);
        assert_eq!(plane.point_dist(Vec3::Y * 2.0), 1.0);
        assert_eq!(plane.point_dist(Vec3::ZERO), -1.0);
    }

    #[test]
    fn test_hyperplane_point_dist_z() {
        let plane = Hyperplane::axis_z(1.0);
        assert_eq!(plane.point_dist(Vec3::Z * 2.0), 1.0);
        assert_eq!(plane.point_dist(Vec3::ZERO), -1.0);
    }

    #[test]
    fn test_hyperplane_point_dist_x_no_axis() {
        let plane = Hyperplane::from_normal(Vec3::X, 1.0);
        assert_eq!(plane.point_dist(Vec3::X * 2.0), 1.0);
        assert_eq!(plane.point_dist(Vec3::ZERO), -1.0);
    }

    #[test]
    fn test_hyperplane_point_dist_y_no_axis() {
        let plane = Hyperplane::from_normal(Vec3::Y, 1.0);
        assert_eq!(plane.point_dist(Vec3::Y * 2.0), 1.0);
        assert_eq!(plane.point_dist(Vec3::ZERO), -1.0);
    }

    #[test]
    fn test_hyperplane_point_dist_z_no_axis() {
        let plane = Hyperplane::from_normal(Vec3::Z, 1.0);
        assert_eq!(plane.point_dist(Vec3::Z * 2.0), 1.0);
        assert_eq!(plane.point_dist(Vec3::ZERO), -1.0);
    }

    #[test]
    fn test_hyperplane_line_segment_intersection_x() {
        let plane = Hyperplane::axis_x(1.0);
        let start = Vec3::new(0.0, 0.5, 0.5);
        let end = Vec3::new(2.0, 0.5, 0.5);

        match plane.line_segment_intersection(start, end, 0., 1.) {
            LinePlaneIntersect::PointIntersection(p_i) => {
                assert_eq!(p_i.ratio(), 0.5);
                assert_eq!(p_i.point(), Vec3::new(1.0, 0.5, 0.5));
            }
            _ => panic!(),
        }
    }

    #[test]
    fn test_hyperplane_line_segment_intersection_y() {
        let plane = Hyperplane::axis_y(1.0);
        let start = Vec3::new(0.5, 0.0, 0.5);
        let end = Vec3::new(0.5, 2.0, 0.5);

        match plane.line_segment_intersection(start, end, 0., 1.) {
            LinePlaneIntersect::PointIntersection(p_i) => {
                assert_eq!(p_i.ratio(), 0.5);
                assert_eq!(p_i.point(), Vec3::new(0.5, 1.0, 0.5));
            }
            _ => panic!(),
        }
    }

    #[test]
    fn test_hyperplane_line_segment_intersection_z() {
        let plane = Hyperplane::axis_z(1.0);
        let start = Vec3::new(0.5, 0.5, 0.0);
        let end = Vec3::new(0.5, 0.5, 2.0);

        match plane.line_segment_intersection(start, end, 0., 1.) {
            LinePlaneIntersect::PointIntersection(p_i) => {
                assert_eq!(p_i.ratio(), 0.5);
                assert_eq!(p_i.point(), Vec3::new(0.5, 0.5, 1.0));
            }
            _ => panic!(),
        }
    }

    #[test]
    fn test_collinear() {
        let cases = vec![
            (vec![Vec3::X, Vec3::Y, Vec3::Z], false),
            (vec![Vec3::X, Vec3::X * 2.0, Vec3::X * 3.0], true),
            (
                vec![
                    [1400.0, 848.0, -456.0].into(),
                    [1352.0, 848.0, -456.0].into(),
                    [1272.0, 848.0, -456.0].into(),
                    [1256.0, 848.0, -456.0].into(),
                    [1208.0, 848.0, -456.0].into(),
                    [1192.0, 848.0, -456.0].into(),
                    [1176.0, 848.0, -456.0].into(),
                ],
                true,
            ),
        ];

        for (input, result) in cases.into_iter() {
            assert_eq!(collinear(&input), result);
        }
    }

    #[test]
    fn test_remove_collinear() {
        let cases = vec![
            (
                vec![
                    [1176.0, 992.0, -456.0].into(),
                    [1176.0, 928.0, -456.0].into(),
                    [1176.0, 880.0, -456.0].into(),
                    [1176.0, 864.0, -456.0].into(),
                    [1176.0, 848.0, -456.0].into(),
                    [1120.0, 848.0, -456.0].into(),
                    [1120.0, 992.0, -456.0].into(),
                ],
                vec![
                    [1176.0, 992.0, -456.0].into(),
                    [1176.0, 848.0, -456.0].into(),
                    [1120.0, 848.0, -456.0].into(),
                    [1120.0, 992.0, -456.0].into(),
                ],
            ),
            (
                vec![
                    [1400.0, 768.0, -456.0].into(),
                    [1400.0, 848.0, -456.0].into(),
                    [1352.0, 848.0, -456.0].into(),
                    [1272.0, 848.0, -456.0].into(),
                    [1256.0, 848.0, -456.0].into(),
                    [1208.0, 848.0, -456.0].into(),
                    [1192.0, 848.0, -456.0].into(),
                    [1176.0, 848.0, -456.0].into(),
                    [1120.0, 848.0, -456.0].into(),
                    [1200.0, 768.0, -456.0].into(),
                ],
                vec![
                    [1400.0, 768.0, -456.0].into(),
                    [1400.0, 848.0, -456.0].into(),
                    [1120.0, 848.0, -456.0].into(),
                    [1200.0, 768.0, -456.0].into(),
                ],
            ),
        ];

        for (input, output) in cases.into_iter() {
            assert_eq!(remove_collinear(input), output);
        }
    }
}
