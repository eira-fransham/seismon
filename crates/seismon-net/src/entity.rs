//! Entity state, baselines and delta updates.

use std::io::{self, Read, Write};

use bevy_math::Vec3;
use bitflags::bitflags;
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};

use super::*;

bitflags! {
    #[derive(Default, Copy, Clone, Ord, Debug, Eq, PartialOrd, PartialEq)]
    pub struct EntityEffects: u8 {
        const BRIGHT_FIELD = 0b0001;
        const MUZZLE_FLASH = 0b0010;
        const BRIGHT_LIGHT = 0b0100;
        const DIM_LIGHT    = 0b1000;
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct EntityState {
    pub origin: Vec3,
    pub angles: Vec3,
    pub model_id: u8,
    pub frame_id: u8,

    // TODO: more specific types for these
    pub colormap: u8,
    pub skin_id: u8,
    pub effects: EntityEffects,
}

impl EntityState {
    pub fn uninitialized() -> EntityState {
        EntityState {
            origin: Default::default(),
            angles: Default::default(),
            model_id: 0,
            frame_id: 0,
            colormap: 0,
            skin_id: 0,
            effects: EntityEffects::empty(),
        }
    }

    pub fn spawn_baseline(&self, ent_id: u16) -> ServerCmd {
        ServerCmd::SpawnBaseline {
            ent_id,
            model_id: self.model_id as _,
            frame_id: self.frame_id as _,
            colormap: self.colormap,
            skin_id: self.skin_id as _,
            origin: self.origin,
            angles: self.angles,
        }
    }

    pub fn make_update(&self, ent_id: u16, baseline: &Self) -> EntityUpdate {
        EntityUpdate {
            ent_id,
            model_id: Some(self.model_id).filter(|v| *v != baseline.model_id),
            frame_id: Some(self.frame_id).filter(|v| *v != baseline.frame_id),
            colormap: Some(self.colormap).filter(|v| *v != baseline.colormap),
            skin_id: Some(self.skin_id).filter(|v| *v != baseline.skin_id),
            effects: Some(self.effects).filter(|v| *v != baseline.effects),
            origin_x: Some(self.origin.x).filter(|v| *v != baseline.origin.x),
            pitch: Some(self.angles.x).filter(|v| *v != baseline.angles.x),
            origin_y: Some(self.origin.y).filter(|v| *v != baseline.origin.y),
            yaw: Some(self.angles.y).filter(|v| *v != baseline.angles.y),
            origin_z: Some(self.origin.z).filter(|v| *v != baseline.origin.z),
            roll: Some(self.angles.z).filter(|v| *v != baseline.angles.z),
            // TODO: When should this be set?
            no_lerp: false,
        }
    }

    /// The inverse of [`EntityState::make_update`]: treating `self` as the baseline, returns the
    /// full entity state described by `update`. Fields missing from the update take the baseline
    /// value (not the previously-received value), matching Quake's protocol.
    pub fn apply_update(&self, update: &EntityUpdate) -> EntityState {
        EntityState {
            origin: Vec3::new(
                update.origin_x.unwrap_or(self.origin.x),
                update.origin_y.unwrap_or(self.origin.y),
                update.origin_z.unwrap_or(self.origin.z),
            ),
            angles: Vec3::new(
                update.pitch.unwrap_or(self.angles.x),
                update.yaw.unwrap_or(self.angles.y),
                update.roll.unwrap_or(self.angles.z),
            ),
            model_id: update.model_id.unwrap_or(self.model_id),
            frame_id: update.frame_id.unwrap_or(self.frame_id),
            colormap: update.colormap.unwrap_or(self.colormap),
            skin_id: update.skin_id.unwrap_or(self.skin_id),
            effects: update.effects.unwrap_or(self.effects),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct EntityUpdate {
    pub ent_id: u16,
    pub model_id: Option<u8>,
    pub frame_id: Option<u8>,
    pub colormap: Option<u8>,
    pub skin_id: Option<u8>,
    pub effects: Option<EntityEffects>,
    pub origin_x: Option<f32>,
    pub pitch: Option<f32>,
    pub origin_y: Option<f32>,
    pub yaw: Option<f32>,
    pub origin_z: Option<f32>,
    pub roll: Option<f32>,
    pub no_lerp: bool,
}

impl EntityUpdate {
    pub const fn empty(ent_id: u16) -> Self {
        Self {
            ent_id,
            model_id: None,
            frame_id: None,
            colormap: None,
            skin_id: None,
            effects: None,
            origin_x: None,
            pitch: None,
            origin_y: None,
            yaw: None,
            origin_z: None,
            roll: None,
            no_lerp: false,
        }
    }

    pub fn read<R>(reader: &mut R, update_flags: UpdateFlags) -> io::Result<Self>
    where
        R: Read,
    {
        let ent_id = if update_flags.contains(UpdateFlags::LONG_ENTITY) {
            reader.read_u16::<LittleEndian>()?
        } else {
            reader.read_u8()? as u16
        };

        let model_id =
            if update_flags.contains(UpdateFlags::MODEL) { Some(reader.read_u8()?) } else { None };

        let frame_id =
            if update_flags.contains(UpdateFlags::FRAME) { Some(reader.read_u8()?) } else { None };

        let colormap = if update_flags.contains(UpdateFlags::COLORMAP) {
            Some(reader.read_u8()?)
        } else {
            None
        };

        let skin_id =
            if update_flags.contains(UpdateFlags::SKIN) { Some(reader.read_u8()?) } else { None };

        let effects = if update_flags.contains(UpdateFlags::EFFECTS) {
            let effects_bits = reader.read_u8()?;

            Some(EntityEffects::from_bits(effects_bits).ok_or(io::ErrorKind::InvalidData)?)
        } else {
            None
        };

        let origin_x = if update_flags.contains(UpdateFlags::ORIGIN_X) {
            Some(read_coord(reader)?)
        } else {
            None
        };

        let pitch = if update_flags.contains(UpdateFlags::PITCH) {
            Some(read_angle(reader)?)
        } else {
            None
        };

        let origin_y = if update_flags.contains(UpdateFlags::ORIGIN_Y) {
            Some(read_coord(reader)?)
        } else {
            None
        };

        let yaw =
            if update_flags.contains(UpdateFlags::YAW) { Some(read_angle(reader)?) } else { None };

        let origin_z = if update_flags.contains(UpdateFlags::ORIGIN_Z) {
            Some(read_coord(reader)?)
        } else {
            None
        };

        let roll =
            if update_flags.contains(UpdateFlags::ROLL) { Some(read_angle(reader)?) } else { None };

        let no_lerp = update_flags.contains(UpdateFlags::NO_LERP);

        Ok(Self {
            ent_id,
            model_id,
            frame_id,
            colormap,
            skin_id,
            effects,
            origin_x,
            pitch,
            origin_y,
            yaw,
            origin_z,
            roll,
            no_lerp,
        })
    }

    pub fn write<W>(&self, writer: &mut W) -> io::Result<()>
    where
        W: Write,
    {
        match u8::try_from(self.ent_id) {
            Ok(byte_entity) => writer.write_u8(byte_entity)?,
            Err(_) => writer.write_u16::<LittleEndian>(self.ent_id)?,
        }

        if let Some(model_id) = self.model_id {
            writer.write_u8(model_id)?;
        }
        if let Some(frame_id) = self.frame_id {
            writer.write_u8(frame_id)?;
        }
        if let Some(colormap) = self.colormap {
            writer.write_u8(colormap)?;
        }
        if let Some(skin_id) = self.skin_id {
            writer.write_u8(skin_id)?;
        }
        if let Some(effects) = self.effects {
            writer.write_u8(effects.bits())?;
        }
        if let Some(origin_x) = self.origin_x {
            write_coord(writer, origin_x)?;
        }
        if let Some(pitch) = self.pitch {
            write_angle(writer, pitch)?;
        }
        if let Some(origin_y) = self.origin_y {
            write_coord(writer, origin_y)?;
        }
        if let Some(yaw) = self.yaw {
            write_angle(writer, yaw)?;
        }
        if let Some(origin_z) = self.origin_z {
            write_coord(writer, origin_z)?;
        }
        if let Some(roll) = self.roll {
            write_angle(writer, roll)?;
        }

        Ok(())
    }

    pub fn any(&self) -> bool {
        self.flags().any_value()
    }

    pub fn long_entity(&self) -> bool {
        u8::try_from(self.ent_id).is_err()
    }

    pub fn flags(&self) -> UpdateFlags {
        let mut out = UpdateFlags::empty();

        for (set, flag) in [
            (self.long_entity(), UpdateFlags::LONG_ENTITY),
            (self.model_id.is_some(), UpdateFlags::MODEL),
            (self.frame_id.is_some(), UpdateFlags::FRAME),
            (self.colormap.is_some(), UpdateFlags::COLORMAP),
            (self.skin_id.is_some(), UpdateFlags::SKIN),
            (self.effects.is_some(), UpdateFlags::EFFECTS),
            (self.origin_x.is_some(), UpdateFlags::ORIGIN_X),
            (self.pitch.is_some(), UpdateFlags::PITCH),
            (self.origin_y.is_some(), UpdateFlags::ORIGIN_Y),
            (self.yaw.is_some(), UpdateFlags::YAW),
            (self.origin_z.is_some(), UpdateFlags::ORIGIN_Z),
            (self.roll.is_some(), UpdateFlags::ROLL),
            (self.no_lerp, UpdateFlags::NO_LERP),
        ] {
            if set {
                out |= flag;
            }
        }

        if out.bits() & !(u8::MAX as u16) != 0 {
            out |= UpdateFlags::MORE_BITS;
        }

        out
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PlayerData {
    pub on_ground: bool,
    pub in_water: bool,
    pub view_height: Option<f32>,
    pub ideal_pitch: Option<f32>,
    pub punch_pitch: Option<f32>,
    pub velocity_x: Option<f32>,
    pub punch_yaw: Option<f32>,
    pub velocity_y: Option<f32>,
    pub punch_roll: Option<f32>,
    pub velocity_z: Option<f32>,
    pub items: ItemFlags,
    pub weapon_frame: Option<u8>,
    pub armor: Option<u8>,
    pub weapon_model_id: Option<u8>,
    pub health: i16,
    pub ammo: u8,
    pub ammo_shells: u8,
    pub ammo_nails: u8,
    pub ammo_rockets: u8,
    pub ammo_cells: u8,
    pub active_weapon: u8,
}

impl EntityUpdate {
    /// Create an `EntityState` from this update, filling in any `None` values
    /// from the specified baseline state.
    pub fn to_entity_state(&self, baseline: &EntityState) -> EntityState {
        EntityState {
            origin: Vec3::new(
                self.origin_x.unwrap_or(baseline.origin.x),
                self.origin_y.unwrap_or(baseline.origin.y),
                self.origin_z.unwrap_or(baseline.origin.z),
            ),
            angles: Vec3::new(
                self.pitch.unwrap_or(baseline.angles[0]),
                self.yaw.unwrap_or(baseline.angles[1]),
                self.roll.unwrap_or(baseline.angles[2]),
            ),
            model_id: self.model_id.unwrap_or(baseline.model_id),
            frame_id: self.frame_id.unwrap_or(baseline.frame_id),
            skin_id: self.skin_id.unwrap_or(baseline.skin_id),
            effects: self.effects.unwrap_or(baseline.effects),
            colormap: self.colormap.unwrap_or(baseline.colormap),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_entity_state_apply_update_inverts_make_update() {
        let baseline = EntityState {
            origin: Vec3::new(544.0, 288.0, 32.0),
            angles: Vec3::new(0.0, 90.0, 0.0),
            model_id: 1,
            frame_id: 0,
            colormap: 1,
            skin_id: 0,
            effects: EntityEffects::empty(),
        };
        // Shares x, pitch, roll, model, colormap and skin with the baseline.
        let state = EntityState {
            origin: Vec3::new(544.0, 1536.0, 16.0),
            angles: Vec3::new(0.0, 180.0, 0.0),
            frame_id: 12,
            effects: EntityEffects::MUZZLE_FLASH,
            ..baseline.clone()
        };

        let update = state.make_update(1, &baseline);
        assert_eq!(update.origin_x, None);
        assert_eq!(baseline.apply_update(&update), state);
    }

    #[test]
    fn test_entity_state_apply_empty_update_is_baseline() {
        let baseline = EntityState {
            origin: Vec3::new(544.0, 288.0, 32.0),
            angles: Vec3::new(10.0, 90.0, 5.0),
            model_id: 3,
            frame_id: 4,
            colormap: 2,
            skin_id: 1,
            effects: EntityEffects::DIM_LIGHT,
        };

        assert_eq!(baseline.apply_update(&EntityUpdate::empty(7)), baseline);
    }
}
