//! Bitflags and small value types shared by several messages.

use bevy_log::warn;
use bitflags::bitflags;
use num_derive::FromPrimitive;

bitflags! {
    #[derive(Copy, Clone, Ord, Debug, Eq, PartialOrd, PartialEq)]
    pub struct UpdateFlags: u16 {
        const MORE_BITS = 1 << 0;
        const ORIGIN_X = 1 << 1;
        const ORIGIN_Y = 1 << 2;
        const ORIGIN_Z = 1 << 3;
        const YAW = 1 << 4;
        const NO_LERP = 1 << 5;
        const FRAME = 1 << 6;
        const SIGNAL = 1 << 7;
        const PITCH = 1 << 8;
        const ROLL = 1 << 9;
        const MODEL = 1 << 10;
        const COLORMAP = 1 << 11;
        const SKIN = 1 << 12;
        const EFFECTS = 1 << 13;
        const LONG_ENTITY = 1 << 14;
    }
}

impl UpdateFlags {
    pub(super) fn any_value(&self) -> bool {
        !(*self
            & (Self::ORIGIN_X
                | Self::ORIGIN_Y
                | Self::ORIGIN_Z
                | Self::YAW
                | Self::NO_LERP
                | Self::FRAME
                | Self::SIGNAL
                | Self::PITCH
                | Self::ROLL
                | Self::MODEL
                | Self::COLORMAP
                | Self::SKIN
                | Self::EFFECTS
                | Self::ORIGIN_X))
            .is_empty()
    }
}

bitflags! {
    #[derive(Copy, Clone, Ord, Debug, Eq, PartialOrd, PartialEq)]
    pub struct ClientUpdateFlags: u16 {
        const VIEW_HEIGHT = 1 << 0;
        const IDEAL_PITCH = 1 << 1;
        const PUNCH_PITCH = 1 << 2;
        const PUNCH_YAW = 1 << 3;
        const PUNCH_ROLL = 1 << 4;
        const VELOCITY_X = 1 << 5;
        const VELOCITY_Y = 1 << 6;
        const VELOCITY_Z = 1 << 7;
        const _AIM_ENT = 1 << 8; // unused
        const ITEMS = 1 << 9;
        const ON_GROUND = 1 << 10;
        const IN_WATER = 1 << 11;
        const WEAPON_FRAME = 1 << 12;
        const ARMOR = 1 << 13;
        const WEAPON = 1 << 14;
    }
}

bitflags! {
    #[derive(Copy, Clone, Ord, Debug, Eq, PartialOrd, PartialEq)]
    pub struct SoundFlags: u8 {
        const VOLUME = 1 << 0;
        const ATTENUATION = 1 << 1;
        const LOOPING = 1 << 2;
    }
}

bitflags! {
    #[derive(Default, Copy, Clone, Ord, Debug, Eq, PartialOrd, PartialEq)]
    pub struct ItemFlags: u32 {
        // WEAPONS
        const SHOTGUN          = 0x00000001;
        const SUPER_SHOTGUN    = 0x00000002;
        const NAILGUN          = 0x00000004;
        const SUPER_NAILGUN    = 0x00000008;
        const GRENADE_LAUNCHER = 0x00000010;
        const ROCKET_LAUNCHER  = 0x00000020;
        const LIGHTNING        = 0x00000040;
        const SUPER_LIGHTNING  = 0x00000080;

        // AMMO
        const SHELLS           = 0x00000100;
        const NAILS            = 0x00000200;
        const ROCKETS          = 0x00000400;
        const CELLS            = 0x00000800;

        // OTHER ITEMS
        const AXE              = 0x00001000;
        const ARMOR_1          = 0x00002000;
        const ARMOR_2          = 0x00004000;
        const ARMOR_3          = 0x00008000;
        const SUPER_HEALTH     = 0x00010000;
        const KEY_1            = 0x00020000;
        const KEY_2            = 0x00040000;
        const INVISIBILITY     = 0x00080000;
        const INVULNERABILITY  = 0x00100000;
        const SUIT             = 0x00200000;
        const QUAD             = 0x00400000;

        // UNUSED
        // We include definitions for unused flags as mods use them (e.g. Rogue)
        const _UNUSED_1         = 0x00800000;
        const _UNUSED_2         = 0x01000000;
        const _UNUSED_3         = 0x02000000;
        const _UNUSED_4         = 0x04000000;
        const _UNUSED_5         = 0x08000000;

        // SIGILS
        const SIGIL_1          = 0x10000000;
        const SIGIL_2          = 0x20000000;
        const SIGIL_3          = 0x40000000;
        const SIGIL_4          = 0x80000000;
    }
}

bitflags! {
    #[derive(Default, Copy, Clone, Ord, Debug, Eq, PartialOrd, PartialEq)]
    pub struct ButtonFlags: u8 {
        const ATTACK = 0x01;
        const USE = 0x02;
        const JUMP = 0x04;
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct PlayerColor {
    top: u8,
    bottom: u8,
}

impl PlayerColor {
    pub fn new(top: u8, bottom: u8) -> PlayerColor {
        if top > 15 {
            warn!("Top color index ({}) will be truncated", top);
        }

        if bottom > 15 {
            warn!("Bottom color index ({}) will be truncated", bottom);
        }

        PlayerColor { top, bottom }
    }

    pub fn from_bits(bits: u8) -> PlayerColor {
        let top = bits >> 4;
        let bottom = bits & 0x0F;

        PlayerColor { top, bottom }
    }

    pub fn bits(&self) -> u8 {
        self.top << 4 | (self.bottom & 0x0F)
    }
}

impl ::std::convert::From<u8> for PlayerColor {
    fn from(src: u8) -> PlayerColor {
        PlayerColor { top: src << 4, bottom: src & 0x0F }
    }
}

#[derive(Default, Clone, Copy, Debug)]
pub struct ColorShift {
    pub dest_color: [u8; 3],
    pub density: f32,
    pub decay: f32,
}

#[derive(Copy, Clone, Debug, Eq, FromPrimitive, PartialEq)]
pub enum ClientStat {
    Health = 0,
    Frags = 1,
    Weapon = 2,
    Ammo = 3,
    Armor = 4,
    WeaponFrame = 5,
    Shells = 6,
    Nails = 7,
    Rockets = 8,
    Cells = 9,
    ActiveWeapon = 10,
    TotalSecrets = 11,
    TotalMonsters = 12,
    FoundSecrets = 13,
    KilledMonsters = 14,
}
