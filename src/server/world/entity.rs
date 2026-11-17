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

//! Engine-level views of entity fields.

use std::{error::Error, fmt};

use crate::{
    common::net::{ButtonFlags, EntityEffects, EntityState},
    server::{
        limit_vec3,
        qc::fields::{EntPhysics, EntPlayer, EntRender},
        world::phys::MoveKind,
    },
};

use bevy::{prelude::*, shape::Aabb3d};
use bitflags::bitflags;
use num::FromPrimitive;
use num_derive::FromPrimitive;

#[derive(Debug)]
pub enum EntityError {
    Io(::std::io::Error),
    Other(String),
    NoVacantSlots,
}

impl EntityError {
    pub fn with_msg<S>(msg: S) -> Self
    where
        S: AsRef<str>,
    {
        EntityError::Other(msg.as_ref().to_owned())
    }
}

impl fmt::Display for EntityError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            EntityError::Io(ref err) => {
                write!(f, "I/O error: ")?;
                err.fmt(f)
            }
            EntityError::Other(ref msg) => write!(f, "{msg}"),
            EntityError::NoVacantSlots => write!(f, "No vacant slots"),
        }
    }
}

impl Error for EntityError {}

impl From<::std::io::Error> for EntityError {
    fn from(error: ::std::io::Error) -> Self {
        EntityError::Io(error)
    }
}

bitflags! {
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub struct EntityFlags: u16 {
        const FLY            = 0b0000000000001;
        const SWIM           = 0b0000000000010;
        const CONVEYOR       = 0b0000000000100;
        const CLIENT         = 0b0000000001000;
        const IN_WATER       = 0b0000000010000;
        const MONSTER        = 0b0000000100000;
        const GOD_MODE       = 0b0000001000000;
        const NO_TARGET      = 0b0000010000000;
        const ITEM           = 0b0000100000000;
        const ON_GROUND      = 0b0001000000000;
        const PARTIAL_GROUND = 0b0010000000000;
        const WATER_JUMP     = 0b0100000000000;
        const JUMP_RELEASED  = 0b1000000000000;
    }
}

#[derive(Debug, Copy, Clone, FromPrimitive, PartialEq, Eq)]
pub enum EntitySolid {
    Not = 0,
    Trigger = 1,
    BBox = 2,
    SlideBox = 3,
    Bsp = 4,
}

impl EntPhysics {
    pub fn solid(&self) -> Result<EntitySolid, EntityError> {
        let solid_i = self.solid as i32;
        EntitySolid::from_i32(solid_i).ok_or_else(|| {
            EntityError::with_msg(format!("Invalid value for entity.solid ({solid_i})"))
        })
    }

    pub fn set_solid(&mut self, solid: EntitySolid) {
        self.solid = solid as u32 as f32;
    }

    pub fn move_kind(&self) -> Result<MoveKind, EntityError> {
        MoveKind::from_i32(self.movetype as i32).ok_or_else(|| {
            EntityError::with_msg(format!("Invalid value for entity.move_kind ({})", self.movetype))
        })
    }

    /// Returns the engine-defined flags set on this entity.
    ///
    /// Any other bits (e.g. flags defined by a mod) are ignored.
    pub fn flags(&self) -> EntityFlags {
        EntityFlags::from_bits_truncate(self.raw_flags() as u16)
    }

    /// Returns all bits of the `flags` field, including ones not known to the engine.
    fn raw_flags(&self) -> i32 {
        self.flags as i32
    }

    pub fn has_flag(&self, flag: EntityFlags) -> bool {
        self.flags().contains(flag)
    }

    pub fn add_flags(&mut self, flags: EntityFlags) {
        // Operate on the raw bits so that flags unknown to the engine are preserved.
        self.flags = (self.raw_flags() | i32::from(flags.bits())) as f32;
    }

    pub fn remove_flags(&mut self, flags: EntityFlags) {
        self.flags = (self.raw_flags() & !i32::from(flags.bits())) as f32;
    }

    pub fn set_origin(&mut self, origin: Vec3) {
        self.origin = store_vector(origin);
    }

    pub fn set_velocity(&mut self, velocity: Vec3) {
        self.velocity = store_vector(velocity);
    }

    pub fn set_angles(&mut self, angles: Vec3) {
        self.angles = store_vector(angles);
    }

    pub fn set_avelocity(&mut self, avelocity: Vec3) {
        self.avelocity = store_vector(avelocity);
    }

    pub fn set_mins(&mut self, mins: Vec3) {
        self.mins = store_vector(mins);
    }

    pub fn set_maxs(&mut self, maxs: Vec3) {
        self.maxs = store_vector(maxs);
    }

    pub fn set_absmin(&mut self, absmin: Vec3) {
        self.absmin = store_vector(absmin);
    }

    pub fn set_absmax(&mut self, absmax: Vec3) {
        self.absmax = store_vector(absmax);
    }

    /// Set this entity's minimum and maximum bounds and calculate its size.
    pub fn set_min_max_size(&mut self, min: Vec3, max: Vec3) {
        let size = max - min;

        self.set_mins(min);
        self.set_maxs(max);
        self.size = store_vector(size);
    }

    /// Limits the entity's velocity by clamping each component (not the
    /// magnitude!) to an absolute value of `sv_maxvelocity`.
    pub fn limit_velocity(&mut self, sv_maxvelocity: f32) {
        self.set_velocity(limit_vec3(self.velocity, Some(sv_maxvelocity)));
    }

    pub fn abs_bounding_box(&self) -> Aabb3d {
        Aabb3d { min: self.absmin.into(), max: self.absmax.into() }
    }
}

impl EntPlayer {
    pub fn set_punchangle(&mut self, punchangle: Vec3) {
        self.punchangle = store_vector(punchangle);
    }

    pub fn set_v_angle(&mut self, v_angle: Vec3) {
        self.v_angle = store_vector(v_angle);
    }

    pub fn buttons(&self) -> ButtonFlags {
        let mut out = ButtonFlags::default();

        if self.button0 != 0. {
            out |= ButtonFlags::ATTACK;
        }

        if self.button1 != 0. {
            out |= ButtonFlags::USE;
        }

        if self.button2 != 0. {
            out |= ButtonFlags::JUMP;
        }

        out
    }

    pub fn set_buttons(&mut self, buttons: ButtonFlags) {
        self.button0 = if buttons.contains(ButtonFlags::ATTACK) { 1. } else { 0. };
        self.button1 = if buttons.contains(ButtonFlags::USE) { 1. } else { 0. };
        self.button2 = if buttons.contains(ButtonFlags::JUMP) { 1. } else { 0. };
    }
}

/// Vectors stored by the engine have invalid values zeroed, as Quake's `put_vector` did.
fn store_vector(v: Vec3) -> Vec3 {
    if v.to_array().iter().any(|v| !v.is_finite()) {
        error!("Tried to store NaN");
    }

    limit_vec3(v, None)
}

/// The state of an entity as sent to clients.
pub fn entity_state(render: &EntRender, phys: &EntPhysics) -> Option<EntityState> {
    Some(EntityState {
        model_id: render.modelindex as _,
        frame_id: render.frame as _,
        colormap: render.colormap as _,
        skin_id: render.skin as _,
        effects: EntityEffects::from_bits(render.effects as _)?,
        origin: phys.origin,
        angles: phys.angles,
    })
}

impl EntRender {
    /// Whether an entity is sent to clients
    ///
    /// Quake only checks that the entity has a non-zero model index and a non-empty
    /// model name.
    pub fn is_networked(&self) -> bool {
        // The null string is also empty.
        self.modelindex != 0. && !self.model.as_bytes().is_empty()
    }
}
