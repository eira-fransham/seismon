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

use std::{error::Error, fmt, ops};

use crate::{
    common::net::{ButtonFlags, EntityEffects, EntityState},
    server::{
        progs::{EntityId, FieldDef, FunctionId, StringId, StringTable, Type},
        world::phys::MoveKind,
    },
};

use arrayvec::ArrayString;
use bevy::{math::bounding::Aabb3d, prelude::*};
use bitflags::bitflags;
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use chrono::Duration;
use im::vector::{Focus, FocusMut};
use num::FromPrimitive;
use num_derive::FromPrimitive;
use parking_lot::Mutex;
use seismon_utils::duration_to_f32;
use uluru::LRUCache;

pub const MAX_ENT_LEAVES: usize = 16;

pub const STATIC_ADDRESS_COUNT: usize = 105;

#[derive(Debug)]
pub enum EntityError {
    Io(::std::io::Error),
    Address(isize),
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
            EntityError::Address(val) => write!(f, "Invalid address ({val})"),
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

/// A trait which covers addresses of typed values.
pub trait FieldAddr {
    /// The type of value referenced by this address.
    type Value;

    /// Loads the value at this address.
    fn load<A, M>(&self, ent: &mut Entity<'_, A, M>) -> Result<Self::Value, EntityError>
    where
        A: FocusIndex<Output = EntityField>;

    /// Stores a value at this address.
    fn store<A, M>(
        &self,
        ent: &mut Entity<'_, A, M>,
        value: Self::Value,
    ) -> Result<(), EntityError>
    where
        A: FocusIndexMut<Output = EntityField>;
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, FromPrimitive)]
pub enum FieldAddrFloat {
    ModelIndex = 0,
    AbsMinX = 1,
    AbsMinY = 2,
    AbsMinZ = 3,
    AbsMaxX = 4,
    AbsMaxY = 5,
    AbsMaxZ = 6,
    /// Used by mobile level geometry such as moving platforms.
    LocalTime = 7,
    /// Determines the movement behavior of an entity. The value must be a variant of `MoveKind`.
    MoveKind = 8,
    Solid = 9,
    OriginX = 10,
    OriginY = 11,
    OriginZ = 12,
    OldOriginX = 13,
    OldOriginY = 14,
    OldOriginZ = 15,
    VelocityX = 16,
    VelocityY = 17,
    VelocityZ = 18,
    AnglesX = 19,
    AnglesY = 20,
    AnglesZ = 21,
    AngularVelocityX = 22,
    AngularVelocityY = 23,
    AngularVelocityZ = 24,
    PunchAngleX = 25,
    PunchAngleY = 26,
    PunchAngleZ = 27,
    /// The index of the entity's animation frame.
    FrameId = 30,
    /// The index of the entity's skin.
    SkinId = 31,
    /// Effects flags applied to the entity. See `EntityEffects`.
    Effects = 32,
    /// Minimum extent in local coordinates, X-coordinate.
    MinsX = 33,
    /// Minimum extent in local coordinates, Y-coordinate.
    MinsY = 34,
    /// Minimum extent in local coordinates, Z-coordinate.
    MinsZ = 35,
    /// Maximum extent in local coordinates, X-coordinate.
    MaxsX = 36,
    /// Maximum extent in local coordinates, Y-coordinate.
    MaxsY = 37,
    /// Maximum extent in local coordinates, Z-coordinate.
    MaxsZ = 38,
    SizeX = 39,
    SizeY = 40,
    SizeZ = 41,
    /// The next server time at which the entity should run its think function.
    NextThink = 46,
    /// The entity's remaining health.
    Health = 48,
    /// The number of kills scored by the entity.
    Frags = 49,
    Weapon = 50,
    WeaponFrame = 52,
    /// The entity's remaining ammunition for its selected weapon.
    CurrentAmmo = 53,
    /// The entity's remaining shotgun shells.
    AmmoShells = 54,
    /// The entity's remaining shotgun shells.
    AmmoNails = 55,
    /// The entity's remaining rockets/grenades.
    AmmoRockets = 56,
    AmmoCells = 57,
    Items = 58,
    TakeDamage = 59,
    DeadFlag = 61,
    ViewOffsetX = 62,
    ViewOffsetY = 63,
    ViewOffsetZ = 64,
    Button0 = 65,
    Button1 = 66,
    Button2 = 67,
    Impulse = 68,
    FixAngle = 69,
    ViewAngleX = 70,
    ViewAngleY = 71,
    ViewAngleZ = 72,
    IdealPitch = 73,
    Flags = 76,
    Colormap = 77,
    Team = 78,
    MaxHealth = 79,
    TeleportTime = 80,
    ArmorStrength = 81,
    ArmorValue = 82,
    WaterLevel = 83,
    Contents = 84,
    IdealYaw = 85,
    YawSpeed = 86,
    SpawnFlags = 89,
    DmgTake = 92,
    DmgSave = 93,
    MoveDirectionX = 96,
    MoveDirectionY = 97,
    MoveDirectionZ = 98,
    Sounds = 100,
}

impl fmt::Display for FieldAddrFloat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ModelIndex => write!(f, "model_id"),
            Self::AbsMinX => write!(f, "abs_min_x"),
            Self::AbsMinY => write!(f, "abs_min_y"),
            Self::AbsMinZ => write!(f, "abs_min_z"),
            Self::AbsMaxX => write!(f, "abs_max_x"),
            Self::AbsMaxY => write!(f, "abs_max_y"),
            Self::AbsMaxZ => write!(f, "abs_max_z"),
            Self::LocalTime => write!(f, "local_time"),
            Self::MoveKind => write!(f, "move_kind"),
            Self::Solid => write!(f, "solid"),
            Self::OriginX => write!(f, "origin_x"),
            Self::OriginY => write!(f, "origin_y"),
            Self::OriginZ => write!(f, "origin_z"),
            Self::OldOriginX => write!(f, "old_origin_x"),
            Self::OldOriginY => write!(f, "old_origin_y"),
            Self::OldOriginZ => write!(f, "old_origin_z"),
            Self::VelocityX => write!(f, "velocity_x"),
            Self::VelocityY => write!(f, "velocity_y"),
            Self::VelocityZ => write!(f, "velocity_z"),
            Self::AnglesX => write!(f, "angles_x"),
            Self::AnglesY => write!(f, "angles_y"),
            Self::AnglesZ => write!(f, "angles_z"),
            Self::AngularVelocityX => write!(f, "angular_velocity_x"),
            Self::AngularVelocityY => write!(f, "angular_velocity_y"),
            Self::AngularVelocityZ => write!(f, "angular_velocity_z"),
            Self::PunchAngleX => write!(f, "punch_angle_x"),
            Self::PunchAngleY => write!(f, "punch_angle_y"),
            Self::PunchAngleZ => write!(f, "punch_angle_z"),
            Self::FrameId => write!(f, "frame_id"),
            Self::SkinId => write!(f, "skin_id"),
            Self::Effects => write!(f, "effects"),
            Self::MinsX => write!(f, "mins_x"),
            Self::MinsY => write!(f, "mins_y"),
            Self::MinsZ => write!(f, "mins_z"),
            Self::MaxsX => write!(f, "maxs_x"),
            Self::MaxsY => write!(f, "maxs_y"),
            Self::MaxsZ => write!(f, "maxs_z"),
            Self::SizeX => write!(f, "size_x"),
            Self::SizeY => write!(f, "size_y"),
            Self::SizeZ => write!(f, "size_z"),
            Self::NextThink => write!(f, "next_think"),
            Self::Health => write!(f, "health"),
            Self::Frags => write!(f, "frags"),
            Self::Weapon => write!(f, "weapon"),
            Self::WeaponFrame => write!(f, "weapon_frame"),
            Self::CurrentAmmo => write!(f, "current_ammo"),
            Self::AmmoShells => write!(f, "ammo_shells"),
            Self::AmmoNails => write!(f, "ammo_nails"),
            Self::AmmoRockets => write!(f, "ammo_rockets"),
            Self::AmmoCells => write!(f, "ammo_cells"),
            Self::Items => write!(f, "items"),
            Self::TakeDamage => write!(f, "take_damage"),
            Self::DeadFlag => write!(f, "dead_flag"),
            Self::ViewOffsetX => write!(f, "view_offset_x"),
            Self::ViewOffsetY => write!(f, "view_offset_y"),
            Self::ViewOffsetZ => write!(f, "view_offset_z"),
            Self::Button0 => write!(f, "button0"),
            Self::Button1 => write!(f, "button1"),
            Self::Button2 => write!(f, "button2"),
            Self::Impulse => write!(f, "impulse"),
            Self::FixAngle => write!(f, "fix_angle"),
            Self::ViewAngleX => write!(f, "view_angle_x"),
            Self::ViewAngleY => write!(f, "view_angle_y"),
            Self::ViewAngleZ => write!(f, "view_angle_z"),
            Self::IdealPitch => write!(f, "ideal_pitch"),
            Self::Flags => write!(f, "flags"),
            Self::Colormap => write!(f, "colormap"),
            Self::Team => write!(f, "team"),
            Self::MaxHealth => write!(f, "max_health"),
            Self::TeleportTime => write!(f, "teleport_time"),
            Self::ArmorStrength => write!(f, "armor_strength"),
            Self::ArmorValue => write!(f, "armor_value"),
            Self::WaterLevel => write!(f, "water_level"),
            Self::Contents => write!(f, "contents"),
            Self::IdealYaw => write!(f, "ideal_yaw"),
            Self::YawSpeed => write!(f, "yaw_speed"),
            Self::SpawnFlags => write!(f, "spawn_flags"),
            Self::DmgTake => write!(f, "dmg_take"),
            Self::DmgSave => write!(f, "dmg_save"),
            Self::MoveDirectionX => write!(f, "move_direction_x"),
            Self::MoveDirectionY => write!(f, "move_direction_y"),
            Self::MoveDirectionZ => write!(f, "move_direction_z"),
            Self::Sounds => write!(f, "sounds"),
        }
    }
}

impl FieldAddr for FieldAddrFloat {
    type Value = f32;

    fn load<A, M>(&self, ent: &mut Entity<'_, A, M>) -> Result<Self::Value, EntityError>
    where
        A: FocusIndex<Output = EntityField>,
    {
        ent.get_float(*self as i16)
    }

    fn store<A, M>(&self, ent: &mut Entity<'_, A, M>, value: Self::Value) -> Result<(), EntityError>
    where
        A: FocusIndexMut<Output = EntityField>,
    {
        ent.put_float(value, *self as i16)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, FromPrimitive)]
pub enum FieldAddrVector {
    AbsMin = 1,
    AbsMax = 4,
    Origin = 10,
    OldOrigin = 13,
    Velocity = 16,
    Angles = 19,
    AngularVelocity = 22,
    PunchAngle = 25,
    Mins = 33,
    Maxs = 36,
    Size = 39,
    ViewOffset = 62,
    ViewAngle = 70,
    MoveDirection = 96,
}

impl FieldAddr for FieldAddrVector {
    type Value = Vec3;

    fn load<A, M>(&self, ent: &mut Entity<'_, A, M>) -> Result<Self::Value, EntityError>
    where
        A: FocusIndex<Output = EntityField>,
    {
        ent.get_vector(*self as i16)
    }

    fn store<A, M>(&self, ent: &mut Entity<'_, A, M>, value: Self::Value) -> Result<(), EntityError>
    where
        A: FocusIndexMut<Output = EntityField>,
    {
        if value.to_array().iter().any(|v| !v.is_finite()) {
            error!("Tried to store NaN");
        }

        ent.put_vector(value, *self as i16)
    }
}

#[derive(Copy, Clone, Debug, FromPrimitive)]
pub enum FieldAddrStringId {
    ClassName = 28,
    ModelName = 29,
    WeaponModelName = 51,
    NetName = 74,
    Target = 90,
    TargetName = 91,
    Message = 99,
    Noise0Name = 101,
    Noise1Name = 102,
    Noise2Name = 103,
    Noise3Name = 104,
}

impl fmt::Display for FieldAddrStringId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ClassName => write!(f, "classname"),
            Self::ModelName => write!(f, "modelname"),
            Self::WeaponModelName => write!(f, "weaponmodelname"),
            Self::NetName => write!(f, "netname"),
            Self::Target => write!(f, "target"),
            Self::TargetName => write!(f, "targetname"),
            Self::Message => write!(f, "message"),
            Self::Noise0Name => write!(f, "noise0name"),
            Self::Noise1Name => write!(f, "noise1name"),
            Self::Noise2Name => write!(f, "noise2name"),
            Self::Noise3Name => write!(f, "noise3name"),
        }
    }
}

impl FieldAddr for FieldAddrStringId {
    type Value = StringId;

    fn load<A, M>(&self, ent: &mut Entity<'_, A, M>) -> Result<Self::Value, EntityError>
    where
        A: FocusIndex<Output = EntityField>,
    {
        ent.get_int(*self as i16)
            .map(|val| StringId(val.try_into().unwrap()))
    }

    fn store<A, M>(&self, ent: &mut Entity<'_, A, M>, value: Self::Value) -> Result<(), EntityError>
    where
        A: FocusIndexMut<Output = EntityField>,
    {
        ent.put_int(value.0.try_into().unwrap(), *self as i16)
    }
}

#[derive(Copy, Clone, Debug, FromPrimitive)]
pub enum FieldAddrEntityId {
    /// The entity this entity is standing on.
    Ground = 47,
    Chain = 60,
    Enemy = 75,
    Aim = 87,
    Goal = 88,
    DmgInflictor = 94,
    Owner = 95,
}

impl FieldAddr for FieldAddrEntityId {
    type Value = EntityId;

    fn load<A, M>(&self, ent: &mut Entity<'_, A, M>) -> Result<Self::Value, EntityError>
    where
        A: FocusIndex<Output = EntityField>,
    {
        ent.entity_id(*self as i16)
    }

    fn store<A, M>(&self, ent: &mut Entity<'_, A, M>, value: Self::Value) -> Result<(), EntityError>
    where
        A: FocusIndexMut<Output = EntityField>,
    {
        ent.put_entity_id(value, *self as i16)
    }
}

#[derive(Copy, Clone, Debug, FromPrimitive)]
pub enum FieldAddrFunctionId {
    Touch = 42,
    Use = 43,
    Think = 44,
    Blocked = 45,
}

impl FieldAddr for FieldAddrFunctionId {
    type Value = FunctionId;

    fn load<A, M>(&self, ent: &mut Entity<'_, A, M>) -> Result<Self::Value, EntityError>
    where
        A: FocusIndex<Output = EntityField>,
    {
        ent.function_id(*self as i16)
    }

    fn store<A, M>(&self, ent: &mut Entity<'_, A, M>, value: Self::Value) -> Result<(), EntityError>
    where
        A: FocusIndexMut<Output = EntityField>,
    {
        ent.put_function_id(value, *self as i16)
    }
}

bitflags! {
    #[derive(Debug)]
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

#[derive(Debug)]
struct FieldDefCacheEntry {
    name: ArrayString<64>,
    index: usize,
}

#[derive(Debug)]
pub struct EntityTypeDef {
    addr_count: usize,
    field_defs: Box<[FieldDef]>,

    name_cache: Mutex<LRUCache<FieldDefCacheEntry, 16>>,
}

impl EntityTypeDef {
    pub fn new(
        addr_count: usize,
        field_defs: Box<[FieldDef]>,
    ) -> Result<EntityTypeDef, EntityError> {
        if addr_count < STATIC_ADDRESS_COUNT {
            return Err(EntityError::with_msg(format!(
                "addr_count ({addr_count}) < STATIC_ADDRESS_COUNT ({STATIC_ADDRESS_COUNT})",
            )));
        }

        Ok(EntityTypeDef {
            addr_count,
            field_defs,
            name_cache: default(),
        })
    }

    pub fn addr_count(&self) -> usize {
        self.addr_count
    }

    pub fn field_defs(&self) -> &[FieldDef] {
        self.field_defs.as_ref()
    }

    /// Locate a field definition given its name.
    pub fn find<S>(&self, string_table: &StringTable, name: S) -> Option<&FieldDef>
    where
        S: AsRef<str>,
    {
        let name = name.as_ref();

        let mut name_cache = self.name_cache.lock();

        if let Some(cached) = name_cache.find(|entry| &entry.name == name) {
            return Some(&self.field_defs[cached.index]);
        }

        let name_id = string_table.find(name)?;

        let (index, def) = self
            .field_defs
            .iter()
            .enumerate()
            .find(|(_, def)| def.name_id == name_id)?;

        name_cache.insert(FieldDefCacheEntry {
            name: ArrayString::from(name).unwrap(),
            index,
        });

        Some(def)
    }
}

#[derive(Debug, FromPrimitive, PartialEq)]
pub enum EntitySolid {
    Not = 0,
    Trigger = 1,
    BBox = 2,
    SlideBox = 3,
    Bsp = 4,
}

#[derive(Default, Debug, Clone)]
pub struct EntityMeta {
    pub leaf_count: usize,
    pub leaf_ids: [usize; MAX_ENT_LEAVES],
    pub baseline: EntityState,
}

#[derive(Clone)]
pub struct Entity<'a, Addrs, Meta> {
    addrs: Addrs,
    type_def: &'a EntityTypeDef,
    meta: Meta,
}

impl<A, M> ops::Deref for Entity<'_, A, M>
where
    M: ops::Deref,
{
    type Target = <M as ops::Deref>::Target;

    fn deref(&self) -> &Self::Target {
        &self.meta
    }
}

impl<A, M> ops::DerefMut for Entity<'_, A, M>
where
    M: ops::DerefMut,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.meta
    }
}

struct EntityFormatter<'a, 'b> {
    entity: &'a EntityRef<'b>,
    string_table: &'a StringTable,
}

impl fmt::Display for EntityFormatter<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut entity = self.entity.clone();
        write!(f, "{{")?;
        for field in self.entity.type_def.field_defs() {
            if matches!(field.type_, Type::QField | Type::QVoid) {
                continue;
            }

            write!(
                f,
                " {}: {} = ",
                self.string_table.get(field.name_id).unwrap().to_str(),
                field.type_
            )?;
            match field.type_ {
                Type::QEntity => write!(f, "{},", entity.entity_id(field.offset as _).unwrap())?,
                Type::QFunction => {
                    write!(f, "{},", entity.function_id(field.offset as _).unwrap())?
                }
                Type::QFloat => write!(f, "{},", entity.get_float(field.offset as _).unwrap())?,
                Type::QString => write!(
                    f,
                    "{},",
                    self.string_table
                        .get(entity.string_id(field.offset as _).unwrap())
                        .unwrap()
                        .to_str()
                )?,
                // TODO: We probably want to skip vectors since these are handled by floats
                Type::QVector => write!(f, "{},", entity.get_vector(field.offset as _).unwrap())?,
                Type::QPointer => { /* TODO */ }
                Type::QVoid | Type::QField => unreachable!(),
            }
        }
        write!(f, "}}")?;

        Ok(())
    }
}

pub type EntityField = [u8; 4];

pub type EntityRef<'a> = Entity<'a, im::vector::Focus<'a, EntityField>, &'a EntityMeta>;
pub type EntityMut<'a> = Entity<'a, im::vector::FocusMut<'a, EntityField>, &'a mut EntityMeta>;

macro_rules! field_accessors {
    ($addr_type_param:ident; $($name:ident, $set_name:ident => $kind:ident :: $field:ident;)*) => {
        $(
            pub fn $name(&mut self) -> Result<<$kind as FieldAddr>::Value, EntityError> {
                self
                    .load($kind::$field)
            }

            pub fn $set_name(&mut self, value: <$kind as FieldAddr>::Value) -> Result<(), EntityError>
            where
                $addr_type_param: FocusIndexMut,
            {
                self
                    .store($kind::$field, value)
            }
        )*
    };
}

impl<'a, Addrs, Meta> Entity<'a, Addrs, Meta> {
    pub fn new(type_def: &'a EntityTypeDef, addrs: Addrs, meta: Meta) -> Self {
        Self {
            addrs,
            meta,
            type_def,
        }
    }
}

pub trait FocusIndex {
    type Output;

    fn index(&mut self, index: usize) -> &Self::Output;
}

pub trait FocusIndexMut: FocusIndex {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output;
}

impl<T> FocusIndex for Focus<'_, T>
where
    T: Clone,
{
    type Output = T;

    fn index(&mut self, index: usize) -> &Self::Output {
        Focus::index(self, index)
    }
}

impl<T> FocusIndex for FocusMut<'_, T>
where
    T: Clone,
{
    type Output = T;

    fn index(&mut self, index: usize) -> &Self::Output {
        FocusMut::index(self, index)
    }
}

impl<T> FocusIndexMut for FocusMut<'_, T>
where
    T: Clone,
{
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        FocusMut::index_mut(self, index)
    }
}

impl<'a> EntityRef<'a> {
    pub fn display<'this>(
        &'this self,
        string_table: &'this StringTable,
    ) -> impl fmt::Display + use<'a, 'this> {
        EntityFormatter {
            entity: self,
            string_table,
        }
    }
}

impl<Addrs, Meta> Entity<'_, Addrs, Meta>
where
    Addrs: FocusIndex<Output = EntityField>,
{
    pub fn type_check(&mut self, addr: usize, type_: Type) -> Result<(), EntityError> {
        match self
            .type_def
            .field_defs
            .iter()
            .find(|def| def.type_ != Type::QVoid && def.offset as usize == addr)
        {
            Some(d) => {
                if type_ == d.type_
                    || (type_ == Type::QFloat && d.type_ == Type::QVector)
                    || (type_ == Type::QVector && d.type_ == Type::QFloat)
                {
                    Ok(())
                } else {
                    Err(EntityError::with_msg(format!(
                        "type check failed: addr={} expected={:?} actual={:?}",
                        addr, type_, d.type_
                    )))
                }
            }
            None => Ok(()),
        }
    }

    pub fn state(&mut self) -> Option<EntityState> {
        let (model_id, frame_id, colormap, skin_id, effects, origin, angles) = (
            self.load(FieldAddrFloat::ModelIndex).ok()? as _,
            self.load(FieldAddrFloat::FrameId).ok()? as _,
            self.load(FieldAddrFloat::Colormap).ok()? as _,
            self.load(FieldAddrFloat::SkinId).ok()? as _,
            EntityEffects::from_bits(self.load(FieldAddrFloat::Effects).ok()? as _)?,
            self.origin().ok()?,
            self.angles().ok()?,
        );

        Some(EntityState {
            model_id,
            frame_id,
            colormap,
            skin_id,
            origin,
            angles,
            effects,
        })
    }

    /// Returns a reference to the memory at the given address.
    pub fn get_addr(&mut self, addr: i16) -> Result<&[u8], EntityError> {
        if addr < 0 {
            return Err(EntityError::Address(addr as isize));
        }

        let addr = addr as usize;

        if addr >= self.type_def.addr_count() {
            return Err(EntityError::Address(addr as isize));
        }

        Ok(self.addrs.index(addr))
    }

    /// Returns a mutable reference to the memory at the given address.
    pub fn get_addr_mut(&mut self, addr: i16) -> Result<&mut [u8], EntityError>
    where
        Addrs: FocusIndexMut,
    {
        if addr < 0 {
            return Err(EntityError::Address(addr as isize));
        }

        let addr = addr as usize;

        if addr >= self.type_def.addr_count() {
            return Err(EntityError::Address(addr as isize));
        }

        Ok(self.addrs.index_mut(addr))
    }

    /// Returns a copy of the memory at the given address.
    pub fn get_bytes(&mut self, addr: i16) -> Result<[u8; 4], EntityError> {
        if addr < 0 {
            return Err(EntityError::Address(addr as isize));
        }

        let addr = addr as usize;

        if addr >= self.type_def.addr_count() {
            return Err(EntityError::Address(addr as isize));
        }

        Ok(*self.addrs.index(addr))
    }

    /// Writes the provided data to the memory at the given address.
    ///
    /// This can be used to circumvent the type checker in cases where an operation is not dependent
    /// of the type of the data.
    pub fn put_bytes(&mut self, val: [u8; 4], addr: i16) -> Result<(), EntityError>
    where
        Addrs: FocusIndexMut,
    {
        if addr < 0 {
            return Err(EntityError::Address(addr as isize));
        }

        let addr = addr as usize;

        if addr >= self.type_def.addr_count() {
            return Err(EntityError::Address(addr as isize));
        }

        *self.addrs.index_mut(addr) = val;
        Ok(())
    }

    pub fn load<F>(&mut self, field: F) -> Result<F::Value, EntityError>
    where
        F: FieldAddr,
    {
        field.load(self)
    }

    pub fn store<F>(&mut self, field: F, value: F::Value) -> Result<(), EntityError>
    where
        F: FieldAddr,
        Addrs: FocusIndexMut,
    {
        field.store(self, value)
    }

    /// Loads an `i32` from the given virtual address.
    pub fn get_int(&mut self, addr: i16) -> Result<i32, EntityError> {
        Ok(self.get_addr(addr)?.read_i32::<LittleEndian>()?)
    }

    /// Loads an `i32` from the given virtual address.
    pub fn put_int(&mut self, val: i32, addr: i16) -> Result<(), EntityError>
    where
        Addrs: FocusIndexMut,
    {
        self.get_addr_mut(addr)?.write_i32::<LittleEndian>(val)?;
        Ok(())
    }

    /// Loads an `f32` from the given virtual address.
    pub fn get_float(&mut self, addr: i16) -> Result<f32, EntityError> {
        self.type_check(addr as usize, Type::QFloat)?;
        Ok(self.get_addr(addr)?.read_f32::<LittleEndian>()?)
    }

    /// Loads an `f32` from the given virtual address and converts it to a bool.
    pub fn get_bool(&mut self, addr: i16) -> Result<bool, EntityError> {
        self.type_check(addr as usize, Type::QFloat)?;
        Ok(self.get_addr(addr)?.read_f32::<LittleEndian>()? != 0.)
    }

    /// Stores an `f32` at the given virtual address.
    pub fn put_float(&mut self, val: f32, addr: i16) -> Result<(), EntityError>
    where
        Addrs: FocusIndexMut,
    {
        self.type_check(addr as usize, Type::QFloat)?;
        self.get_addr_mut(addr)?.write_f32::<LittleEndian>(val)?;
        Ok(())
    }

    /// Loads an `[f32; 3]` from the given virtual address.
    pub fn get_vector(&mut self, addr: i16) -> Result<Vec3, EntityError> {
        self.type_check(addr as usize, Type::QVector)?;

        let mut v = Vec3::ZERO;

        for i in 0..3 {
            v[i] = self.get_float(addr + i as i16)?;
        }

        Ok(v)
    }

    /// Stores an `[f32; 3]` at the given virtual address.
    pub fn put_vector(&mut self, val: Vec3, addr: i16) -> Result<(), EntityError>
    where
        Addrs: FocusIndexMut,
    {
        self.type_check(addr as usize, Type::QVector)?;
        let val = crate::server::limit_vec3(val, None);
        for i in 0..3 {
            self.put_float(val[i], addr + i as i16)?;
        }

        Ok(())
    }

    /// Loads a `StringId` from the given virtual address.
    pub fn string_id(&mut self, addr: i16) -> Result<StringId, EntityError> {
        self.type_check(addr as usize, Type::QString)?;

        Ok(StringId(self.get_int(addr)? as usize))
    }

    /// Stores a `StringId` at the given virtual address.
    pub fn put_string_id(&mut self, val: StringId, addr: i16) -> Result<(), EntityError>
    where
        Addrs: FocusIndexMut,
    {
        self.type_check(addr as usize, Type::QString)?;

        self.get_addr_mut(addr)?
            .write_i32::<LittleEndian>(val.try_into().unwrap())?;
        Ok(())
    }

    /// Loads an `EntityId` from the given virtual address.
    pub fn entity_id(&mut self, addr: i16) -> Result<EntityId, EntityError> {
        self.type_check(addr as usize, Type::QEntity)?;

        Ok(EntityId(self.get_int(addr)?))
    }

    /// Stores an `EntityId` at the given virtual address.
    pub fn put_entity_id(&mut self, val: EntityId, addr: i16) -> Result<(), EntityError>
    where
        Addrs: FocusIndexMut,
    {
        self.type_check(addr as usize, Type::QEntity)?;

        self.get_addr_mut(addr)?.write_i32::<LittleEndian>(val.0)?;
        Ok(())
    }

    /// Loads a `FunctionId` from the given virtual address.
    pub fn function_id(&mut self, addr: i16) -> Result<FunctionId, EntityError> {
        self.type_check(addr as usize, Type::QFunction)?;
        Ok(FunctionId(self.get_int(addr)? as usize))
    }

    /// Stores a `FunctionId` at the given virtual address.
    pub fn put_function_id(&mut self, val: FunctionId, addr: i16) -> Result<(), EntityError>
    where
        Addrs: FocusIndexMut,
    {
        self.type_check(addr as usize, Type::QFunction)?;
        self.get_addr_mut(addr)?
            .write_i32::<LittleEndian>(val.try_into().unwrap())?;
        Ok(())
    }

    /// Set this entity's minimum and maximum bounds and calculate its size.
    pub fn set_min_max_size<V>(&mut self, min: V, max: V) -> Result<(), EntityError>
    where
        V: Into<Vec3>,
        Addrs: FocusIndexMut,
    {
        let min = min.into();
        let max = max.into();
        let size = max - min;

        debug!("Setting entity min: {:?}", min);
        self.put_vector(min, FieldAddrVector::Mins as i16)?;

        debug!("Setting entity max: {:?}", max);
        self.put_vector(max, FieldAddrVector::Maxs as i16)?;

        debug!("Setting entity size: {:?}", size);
        self.put_vector(size, FieldAddrVector::Size as i16)?;
        Ok(())
    }

    pub fn solid(&mut self) -> Result<EntitySolid, EntityError> {
        let solid_i = self.load(FieldAddrFloat::Solid)? as i32;
        match EntitySolid::from_i32(solid_i) {
            Some(s) => Ok(s),
            None => Err(EntityError::with_msg(format!(
                "Invalid value for entity.solid ({solid_i})",
            ))),
        }
    }

    field_accessors! {
        Addrs;
        classname, set_classname => FieldAddrStringId::ClassName;
        origin, set_origin => FieldAddrVector::Origin;
        old_origin, set_old_origin => FieldAddrVector::OldOrigin;
        old_origin_x, set_old_origin_x => FieldAddrFloat::OldOriginX;
        old_origin_y, set_old_origin_y => FieldAddrFloat::OldOriginY;
        old_origin_z, set_old_origin_z => FieldAddrFloat::OldOriginZ;
        angles, set_angles => FieldAddrVector::Angles;
        angular_velocity, set_angular_velocity => FieldAddrVector::AngularVelocity;
        view_ofs, set_view_ofs => FieldAddrVector::ViewOffset;
        min, set_min => FieldAddrVector::Mins;
        max, set_max => FieldAddrVector::Maxs;
        abs_min, set_abs_min => FieldAddrVector::AbsMin;
        abs_max, set_abs_max => FieldAddrVector::AbsMax;
        model_index, set_model_index => FieldAddrFloat::ModelIndex;
        model_name, set_model_name => FieldAddrStringId::ModelName;
        local_time, set_local_time => FieldAddrFloat::LocalTime;
        ideal_pitch, set_ideal_pitch => FieldAddrFloat::IdealPitch;
        ideal_yaw, set_ideal_yaw => FieldAddrFloat::IdealYaw;
        yaw_speed, set_yaw_speed => FieldAddrFloat::YawSpeed;
        punch_angle, set_punch_angle => FieldAddrVector::PunchAngle;
        items, set_items => FieldAddrFloat::Items;
        ground, set_ground => FieldAddrEntityId::Ground;
        owner, set_owner => FieldAddrEntityId::Owner;
        enemy, set_enemy => FieldAddrEntityId::Enemy;
        chain, set_chain => FieldAddrEntityId::Chain;
        goal, set_goal => FieldAddrEntityId::Goal;
        weapon_frame, set_weapon_frame => FieldAddrFloat::WeaponFrame;
        armor, set_armor => FieldAddrFloat::ArmorStrength;
        weapon_model_name, set_weapon_model_name => FieldAddrStringId::WeaponModelName;
        active_weapon, set_active_weapon => FieldAddrFloat::Weapon;
        health, set_health => FieldAddrFloat::Health;
        ammo, set_ammo => FieldAddrFloat::CurrentAmmo;
        ammo_shells, set_ammo_shells => FieldAddrFloat::AmmoShells;
        ammo_cells, set_ammo_cells => FieldAddrFloat::AmmoCells;
        ammo_nails, set_ammo_nails => FieldAddrFloat::AmmoNails;
        ammo_rockets, set_ammo_rockets => FieldAddrFloat::AmmoRockets;
        water_level, set_water_level => FieldAddrFloat::WaterLevel;
        view_angle, set_view_angle => FieldAddrVector::ViewAngle;
        move_dir, set_move_dir => FieldAddrVector::MoveDirection;
        button_0, set_button_0 => FieldAddrFloat::Button0;
        button_1, set_button_1 => FieldAddrFloat::Button1;
        button_2, set_button_2 => FieldAddrFloat::Button2;
        impulse, set_impulse => FieldAddrFloat::Impulse;
        size, set_size => FieldAddrVector::Size;
        velocity, set_velocity => FieldAddrVector::Velocity;
        velocity_x, set_velocity_x => FieldAddrFloat::VelocityX;
        velocity_y, set_velocity_y => FieldAddrFloat::VelocityY;
        velocity_z, set_velocity_z => FieldAddrFloat::VelocityZ;
    }

    pub fn buttons(&mut self) -> Result<ButtonFlags, EntityError> {
        let mut out = ButtonFlags::default();

        if self.button_0()? != 0. {
            out |= ButtonFlags::ATTACK;
        }

        if self.button_1()? != 0. {
            out |= ButtonFlags::USE;
        }

        if self.button_2()? != 0. {
            out |= ButtonFlags::JUMP;
        }

        Ok(out)
    }

    pub fn set_buttons(&mut self, buttons: ButtonFlags) -> Result<(), EntityError>
    where
        Addrs: FocusIndexMut<Output = EntityField>,
    {
        self.set_button_0(if buttons.contains(ButtonFlags::ATTACK) {
            1.
        } else {
            0.
        })?;

        self.set_button_1(if buttons.contains(ButtonFlags::USE) {
            1.
        } else {
            0.
        })?;

        self.set_button_2(if buttons.contains(ButtonFlags::JUMP) {
            1.
        } else {
            0.
        })?;

        Ok(())
    }

    pub fn abs_bounding_box(&mut self) -> Result<Aabb3d, EntityError> {
        Ok(Aabb3d {
            min: self.abs_min()?.into(),
            max: self.abs_max()?.into(),
        })
    }

    /// Applies gravity to the entity.
    ///
    /// The effect depends on the provided value of the `sv_gravity` cvar, the
    /// amount of time being simulated, and the entity's own `gravity` field
    /// value.
    pub fn apply_gravity(
        &mut self,
        string_table: &StringTable,
        sv_gravity: f32,
        frame_time: Duration,
    ) -> Result<(), EntityError>
    where
        Addrs: FocusIndexMut,
    {
        let ent_gravity = match self.type_def.find(string_table, "gravity") {
            Some(def) => self.get_float(def.offset as i16)?,
            None => 1.0,
        };

        let mut vel = self.velocity()?;
        vel.z -= ent_gravity * sv_gravity * duration_to_f32(frame_time);
        self.store(FieldAddrVector::Velocity, vel)?;

        Ok(())
    }

    /// Limits the entity's velocity by clamping each component (not the
    /// magnitude!) to an absolute value of `sv_maxvelocity`.
    pub fn limit_velocity(&mut self, sv_maxvelocity: f32) -> Result<(), EntityError>
    where
        Addrs: FocusIndexMut,
    {
        let vel = self.velocity()?;
        self.set_velocity(crate::server::limit_vec3(vel, Some(sv_maxvelocity)))?;

        Ok(())
    }

    pub fn move_kind(&mut self) -> Result<MoveKind, EntityError> {
        let move_kind_f = self.get_float(FieldAddrFloat::MoveKind as i16)?;
        let move_kind_i = move_kind_f as i32;
        match MoveKind::from_i32(move_kind_i) {
            Some(m) => Ok(m),
            None => Err(EntityError::with_msg(format!(
                "Invalid value for entity.move_kind ({move_kind_f})"
            ))),
        }
    }

    pub fn flags(&mut self) -> Result<EntityFlags, EntityError> {
        let flags_i = self.get_float(FieldAddrFloat::Flags as i16)? as u16;
        match EntityFlags::from_bits(flags_i) {
            Some(f) => Ok(f),
            None => Err(EntityError::with_msg(format!(
                "Invalid internal flags value ({flags_i})"
            ))),
        }
    }

    pub fn has_flag(&mut self, flag: EntityFlags) -> Result<bool, EntityError> {
        Ok(self.flags()?.contains(flag))
    }

    pub fn add_flags(&mut self, flags: EntityFlags) -> Result<(), EntityError>
    where
        Addrs: FocusIndexMut,
    {
        let result = self.flags()? | (flags);
        self.put_float(result.bits() as f32, FieldAddrFloat::Flags as i16)?;
        Ok(())
    }

    pub fn remove_flags(&mut self, flags: EntityFlags) -> Result<(), EntityError>
    where
        Addrs: FocusIndexMut,
    {
        let result = self.flags()? & !flags;
        self.put_float(result.bits() as f32, FieldAddrFloat::Flags as i16)?;
        Ok(())
    }
}
