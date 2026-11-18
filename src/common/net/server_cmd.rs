//! Server-to-client messages.

use std::{
    io::{self, BufRead, Read, Write},
    mem,
};

use bevy::prelude::*;
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use num::FromPrimitive;
use num_derive::FromPrimitive;
use seismon_utils::{QAngles, QString};
use snafu::Backtrace;

use super::*;

#[derive(Default, Copy, Clone, Ord, Debug, Eq, FromPrimitive, PartialOrd, PartialEq, Hash)]
pub enum SignOnStage {
    #[default]
    Not = 0,
    Prespawn = 1,
    ClientInfo = 2,
    Begin = 3,
    Done = 4,
}

/// A trait for in-game server and client network commands.
pub trait Cmd: Sized {
    /// Returns the numeric value of this command's code.
    fn code(&self) -> u8;

    /// Reads data from the given source and constructs a command object.
    fn deserialize<R>(reader: &mut R) -> Result<Self, NetError>
    where
        R: BufRead + ReadBytesExt;

    /// Writes this command's content to the given sink.
    fn serialize<W>(&self, writer: &mut W) -> Result<(), NetError>
    where
        W: WriteBytesExt;
}

// TODO: use feature(arbitrary_enum_discriminant)
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, FromPrimitive)]
pub enum BasicServerCmdCode {
    Bad = 0,
    NoOp = 1,
    Disconnect = 2,
    UpdateStat = 3,
    Version = 4,
    SetView = 5,
    Sound = 6,
    Time = 7,
    Print = 8,
    StuffText = 9,
    SetAngle = 10,
    ServerInfo = 11,
    LightStyle = 12,
    UpdateName = 13,
    UpdateFrags = 14,
    PlayerData = 15,
    StopSound = 16,
    UpdateColors = 17,
    Particle = 18,
    Damage = 19,
    SpawnStatic = 20,
    // SpawnBinary = 21, // unused
    SpawnBaseline = 22,
    TempEntity = 23,
    SetPause = 24,
    SignOnStage = 25,
    CenterPrint = 26,
    KilledMonster = 27,
    FoundSecret = 28,
    StaticSound = 29,
    Intermission = 30,
    Finale = 31,
    CdTrack = 32,
    SellScreen = 33,
    Cutscene = 34,
}

#[derive(Debug)]
pub enum ServerCmdCode {
    Basic(BasicServerCmdCode),
    FastUpdate(UpdateFlags),
}

impl ServerCmdCode {
    pub fn read<R>(reader: &mut R) -> io::Result<Self>
    where
        R: Read,
    {
        let low_bits = reader.read_u8()?;

        if low_bits & FAST_UPDATE_FLAG != 0 {
            let low_bits = (low_bits & !FAST_UPDATE_FLAG) as u16;

            let bits = if UpdateFlags::from_bits(low_bits)
                .ok_or(io::Error::from(io::ErrorKind::InvalidData))?
                .contains(UpdateFlags::MORE_BITS)
            {
                let high_bits = reader.read_u8()? as u16;
                high_bits << 8 | low_bits
            } else {
                low_bits
            };

            let update_flags =
                UpdateFlags::from_bits(bits).ok_or(io::Error::from(io::ErrorKind::InvalidData))?;

            Ok(Self::FastUpdate(update_flags))
        } else {
            let basic_cmd = BasicServerCmdCode::from_u8(low_bits)
                .ok_or(io::Error::from(io::ErrorKind::InvalidData))?;
            Ok(Self::Basic(basic_cmd))
        }
    }

    pub fn write<W>(&self, writer: &mut W) -> io::Result<usize>
    where
        W: Write,
    {
        match self {
            Self::Basic(basic) => writer.write_u8(*basic as _).map(|()| mem::size_of::<u8>()),
            Self::FastUpdate(update) => {
                if update.contains(UpdateFlags::MORE_BITS) {
                    assert!(update.bits() & !(u8::MAX as u16) != 0);
                    writer
                        .write_u16::<LittleEndian>(update.bits() | (FAST_UPDATE_FLAG as u16))
                        .map(|()| mem::size_of::<u16>())
                } else {
                    assert!(update.bits() & !(u8::MAX as u16) == 0);
                    writer
                        .write_u8(update.bits() as u8 | FAST_UPDATE_FLAG)
                        .map(|()| mem::size_of::<u8>())
                }
            }
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, FromPrimitive, PartialEq)]
pub enum GameType {
    CoOp = 0,
    Deathmatch = 1,
}

#[derive(Message, Clone, Debug, PartialEq)]
pub enum ServerCmd {
    Bad,
    NoOp,
    Disconnect,
    UpdateStat {
        stat: ClientStat,
        value: i32,
    },
    Version {
        version: i32,
    },
    SetView {
        ent_id: u16,
    },
    Sound {
        volume: Option<u8>,
        attenuation: Option<f32>,
        entity_id: u16,
        channel: i8,
        sound_id: u8,
        position: Vec3,
    },
    Time {
        time: f32,
    },
    Print {
        text: QString,
    },
    /// Send a command to a client's console.
    StuffText {
        /// The text to write to the console.
        text: QString,
    },
    /// Set the player's angle directly.
    SetAngle {
        angles: QAngles,
    },
    ServerInfo {
        protocol_version: i32,
        max_clients: u8,
        game_type: GameType,
        message: QString,
        model_precache: Vec<String>,
        sound_precache: Vec<String>,
    },
    LightStyle {
        id: u8,
        value: QString,
    },
    UpdateName {
        player_id: u8,
        new_name: QString,
    },
    UpdateFrags {
        player_id: u8,
        new_frags: i16,
    },
    PlayerData(PlayerData),
    StopSound {
        entity_id: u16,
        channel: i8,
    },
    UpdateColors {
        player_id: u8,
        new_colors: PlayerColor,
    },
    Particle {
        origin: Vec3,
        direction: Vec3,
        count: u8,
        color: u8,
    },
    Damage {
        armor: u8,
        blood: u8,
        source: Vec3,
    },
    SpawnStatic {
        model_id: u8,
        frame_id: u8,
        colormap: u8,
        skin_id: u8,
        origin: Vec3,
        angles: Vec3,
    },
    // SpawnBinary, // unused
    SpawnBaseline {
        ent_id: u16,
        model_id: u8,
        frame_id: u8,
        colormap: u8,
        skin_id: u8,
        origin: Vec3,
        angles: Vec3,
    },
    TempEntity {
        temp_entity: TempEntity,
    },
    SetPause {
        paused: bool,
    },
    SignOnStage {
        stage: SignOnStage,
    },
    CenterPrint {
        text: QString,
    },
    KilledMonster,
    FoundSecret,
    StaticSound {
        origin: Vec3,
        sound_id: u8,
        volume: u8,
        attenuation: u8,
    },
    Intermission,
    Finale {
        text: QString,
    },
    CdTrack {
        track: u32,
        loop_: u8,
    },
    SellScreen,
    Cutscene {
        text: QString,
    },
    FastUpdate(EntityUpdate),
}

impl ServerCmd {
    /// If the command refers to an entity, return it.
    pub fn entity(&self) -> Option<u16> {
        match self {
            ServerCmd::SpawnBaseline { ent_id, .. }
            | ServerCmd::FastUpdate(EntityUpdate { ent_id, .. }) => Some(*ent_id),
            ServerCmd::Bad
            | ServerCmd::NoOp
            | ServerCmd::Disconnect
            | ServerCmd::UpdateStat { .. }
            | ServerCmd::Version { .. }
            | ServerCmd::SetView { .. }
            | ServerCmd::Sound { .. }
            | ServerCmd::Time { .. }
            | ServerCmd::Print { .. }
            | ServerCmd::StuffText { .. }
            | ServerCmd::SetAngle { .. }
            | ServerCmd::ServerInfo { .. }
            | ServerCmd::LightStyle { .. }
            | ServerCmd::UpdateName { .. }
            | ServerCmd::UpdateFrags { .. }
            | ServerCmd::PlayerData(_)
            | ServerCmd::StopSound { .. }
            | ServerCmd::UpdateColors { .. }
            | ServerCmd::Particle { .. }
            | ServerCmd::Damage { .. }
            | ServerCmd::SpawnStatic { .. }
            | ServerCmd::TempEntity { .. }
            | ServerCmd::SetPause { .. }
            | ServerCmd::SignOnStage { .. }
            | ServerCmd::CenterPrint { .. }
            | ServerCmd::KilledMonster
            | ServerCmd::FoundSecret
            | ServerCmd::StaticSound { .. }
            | ServerCmd::Intermission
            | ServerCmd::Finale { .. }
            | ServerCmd::CdTrack { .. }
            | ServerCmd::SellScreen
            | ServerCmd::Cutscene { .. } => None,
        }
    }

    pub fn code(&self) -> ServerCmdCode {
        match self {
            ServerCmd::Bad => ServerCmdCode::Basic(BasicServerCmdCode::Bad),
            ServerCmd::NoOp => ServerCmdCode::Basic(BasicServerCmdCode::NoOp),
            ServerCmd::Disconnect => ServerCmdCode::Basic(BasicServerCmdCode::Disconnect),
            ServerCmd::UpdateStat { .. } => ServerCmdCode::Basic(BasicServerCmdCode::UpdateStat),
            ServerCmd::Version { .. } => ServerCmdCode::Basic(BasicServerCmdCode::Version),
            ServerCmd::SetView { .. } => ServerCmdCode::Basic(BasicServerCmdCode::SetView),
            ServerCmd::Sound { .. } => ServerCmdCode::Basic(BasicServerCmdCode::Sound),
            ServerCmd::Time { .. } => ServerCmdCode::Basic(BasicServerCmdCode::Time),
            ServerCmd::Print { .. } => ServerCmdCode::Basic(BasicServerCmdCode::Print),
            ServerCmd::StuffText { .. } => ServerCmdCode::Basic(BasicServerCmdCode::StuffText),
            ServerCmd::SetAngle { .. } => ServerCmdCode::Basic(BasicServerCmdCode::SetAngle),
            ServerCmd::ServerInfo { .. } => ServerCmdCode::Basic(BasicServerCmdCode::ServerInfo),
            ServerCmd::LightStyle { .. } => ServerCmdCode::Basic(BasicServerCmdCode::LightStyle),
            ServerCmd::UpdateName { .. } => ServerCmdCode::Basic(BasicServerCmdCode::UpdateName),
            ServerCmd::UpdateFrags { .. } => ServerCmdCode::Basic(BasicServerCmdCode::UpdateFrags),
            ServerCmd::PlayerData(_) => ServerCmdCode::Basic(BasicServerCmdCode::PlayerData),
            ServerCmd::StopSound { .. } => ServerCmdCode::Basic(BasicServerCmdCode::StopSound),
            ServerCmd::UpdateColors { .. } => {
                ServerCmdCode::Basic(BasicServerCmdCode::UpdateColors)
            }
            ServerCmd::Particle { .. } => ServerCmdCode::Basic(BasicServerCmdCode::Particle),
            ServerCmd::Damage { .. } => ServerCmdCode::Basic(BasicServerCmdCode::Damage),
            ServerCmd::SpawnStatic { .. } => ServerCmdCode::Basic(BasicServerCmdCode::SpawnStatic),
            ServerCmd::SpawnBaseline { .. } => {
                ServerCmdCode::Basic(BasicServerCmdCode::SpawnBaseline)
            }
            ServerCmd::TempEntity { .. } => ServerCmdCode::Basic(BasicServerCmdCode::TempEntity),
            ServerCmd::SetPause { .. } => ServerCmdCode::Basic(BasicServerCmdCode::SetPause),
            ServerCmd::SignOnStage { .. } => ServerCmdCode::Basic(BasicServerCmdCode::SignOnStage),
            ServerCmd::CenterPrint { .. } => ServerCmdCode::Basic(BasicServerCmdCode::CenterPrint),
            ServerCmd::KilledMonster => ServerCmdCode::Basic(BasicServerCmdCode::KilledMonster),
            ServerCmd::FoundSecret => ServerCmdCode::Basic(BasicServerCmdCode::FoundSecret),
            ServerCmd::StaticSound { .. } => ServerCmdCode::Basic(BasicServerCmdCode::StaticSound),
            ServerCmd::Intermission => ServerCmdCode::Basic(BasicServerCmdCode::Intermission),
            ServerCmd::Finale { .. } => ServerCmdCode::Basic(BasicServerCmdCode::Finale),
            ServerCmd::CdTrack { .. } => ServerCmdCode::Basic(BasicServerCmdCode::CdTrack),
            ServerCmd::SellScreen => ServerCmdCode::Basic(BasicServerCmdCode::SellScreen),
            ServerCmd::Cutscene { .. } => ServerCmdCode::Basic(BasicServerCmdCode::Cutscene),
            ServerCmd::FastUpdate(update) => ServerCmdCode::FastUpdate(update.flags()),
        }
    }

    pub fn deserialize<R>(reader: &mut R) -> Result<Option<ServerCmd>, NetError>
    where
        R: BufRead,
    {
        // Expanded form of `has_data_left`
        if !reader.fill_buf().map(|b| !b.is_empty()).unwrap() {
            return Ok(None);
        }

        let code = ServerCmdCode::read(reader)?;

        let code = match code {
            ServerCmdCode::Basic(basic) => basic,
            ServerCmdCode::FastUpdate(update_flags) => {
                return Ok(Some(ServerCmd::FastUpdate(EntityUpdate::read(reader, update_flags)?)));
            }
        };

        let cmd = match code {
            BasicServerCmdCode::Bad => ServerCmd::Bad,
            BasicServerCmdCode::NoOp => ServerCmd::NoOp,
            BasicServerCmdCode::Disconnect => ServerCmd::Disconnect,

            BasicServerCmdCode::UpdateStat => {
                let stat_id = reader.read_u8()?;
                let stat = match ClientStat::from_u8(stat_id) {
                    Some(c) => c,
                    None => {
                        return Err(NetError::invalid_data(format!(
                            "value for ClientStat: {stat_id}"
                        )));
                    }
                };
                let value = reader.read_i32::<LittleEndian>()?;

                ServerCmd::UpdateStat { stat, value }
            }

            BasicServerCmdCode::Version => {
                let version = reader.read_i32::<LittleEndian>()?;
                ServerCmd::Version { version }
            }

            BasicServerCmdCode::SetView => {
                let ent_id = reader.read_u16::<LittleEndian>()?;
                ServerCmd::SetView { ent_id }
            }

            BasicServerCmdCode::Sound => {
                let flags_bits = reader.read_u8()?;
                let flags = match SoundFlags::from_bits(flags_bits) {
                    Some(f) => f,
                    None => {
                        return Err(NetError::invalid_data(format!("SoundFlags: {flags_bits:b}")));
                    }
                };

                let volume = match flags.contains(SoundFlags::VOLUME) {
                    true => Some(reader.read_u8()?),
                    false => None,
                };

                let attenuation = match flags.contains(SoundFlags::ATTENUATION) {
                    true => Some(reader.read_u8()? as f32 * SOUND_ATTENUATION_READ_FACTOR),
                    false => None,
                };

                let entity_channel = reader.read_i16::<LittleEndian>()?;
                let entity_id = (entity_channel >> 3) as u16;
                let channel = (entity_channel & 0b111) as i8;
                let sound_id = reader.read_u8()?;
                let position =
                    Vec3::new(read_coord(reader)?, read_coord(reader)?, read_coord(reader)?);

                ServerCmd::Sound { volume, attenuation, entity_id, channel, sound_id, position }
            }

            BasicServerCmdCode::Time => {
                let time = reader.read_f32::<LittleEndian>()?;
                ServerCmd::Time { time }
            }

            BasicServerCmdCode::Print => {
                let text = seismon_utils::read_cstring(reader)?;

                ServerCmd::Print { text }
            }

            BasicServerCmdCode::StuffText => {
                let text = seismon_utils::read_cstring(reader)?;

                ServerCmd::StuffText { text }
            }

            BasicServerCmdCode::SetAngle => {
                let angles = [read_angle(reader)?, read_angle(reader)?, read_angle(reader)?].into();

                ServerCmd::SetAngle { angles }
            }

            BasicServerCmdCode::ServerInfo => {
                let protocol_version = reader.read_i32::<LittleEndian>()?;
                let max_clients = reader.read_u8()?;
                let game_type_code = reader.read_u8()?;
                let game_type = match GameType::from_u8(game_type_code) {
                    Some(g) => g,
                    None => {
                        return Err(NetError::invalid_data(format!(
                            "Invalid game type ({game_type_code})"
                        )));
                    }
                };

                let message = seismon_utils::read_cstring(reader)?;

                let mut model_precache = Vec::new();
                loop {
                    let model_name = seismon_utils::read_cstring(reader)?.into_string();
                    if model_name.is_empty() {
                        break;
                    }
                    model_precache.push(model_name);
                }

                let mut sound_precache = Vec::new();
                loop {
                    let sound_name = seismon_utils::read_cstring(reader)?.into_string();
                    if sound_name.is_empty() {
                        break;
                    }
                    sound_precache.push(sound_name);
                }

                ServerCmd::ServerInfo {
                    protocol_version,
                    max_clients,
                    game_type,
                    message,
                    model_precache,
                    sound_precache,
                }
            }

            BasicServerCmdCode::LightStyle => {
                let id = reader.read_u8()?;
                let value = seismon_utils::read_cstring(reader)?.into_string();
                ServerCmd::LightStyle { id, value: value.into() }
            }

            BasicServerCmdCode::UpdateName => {
                let player_id = reader.read_u8()?;
                let new_name = seismon_utils::read_cstring(reader)?;
                ServerCmd::UpdateName { player_id, new_name }
            }

            BasicServerCmdCode::UpdateFrags => {
                let player_id = reader.read_u8()?;
                let new_frags = reader.read_i16::<LittleEndian>()?;

                ServerCmd::UpdateFrags { player_id, new_frags }
            }

            BasicServerCmdCode::PlayerData => {
                let flags_bits = reader.read_u16::<LittleEndian>()?;
                let flags = match ClientUpdateFlags::from_bits(flags_bits) {
                    Some(f) => f,
                    None => {
                        return Err(NetError::invalid_data(format!(
                            "client update flags: {flags_bits:b}"
                        )));
                    }
                };

                let view_height = match flags.contains(ClientUpdateFlags::VIEW_HEIGHT) {
                    true => Some(reader.read_i8()? as f32),
                    false => None,
                };

                let ideal_pitch = match flags.contains(ClientUpdateFlags::IDEAL_PITCH) {
                    true => Some(reader.read_i8()? as f32),
                    false => None,
                };

                let punch_pitch = match flags.contains(ClientUpdateFlags::PUNCH_PITCH) {
                    true => Some(reader.read_i8()? as f32),
                    false => None,
                };

                let velocity_x = match flags.contains(ClientUpdateFlags::VELOCITY_X) {
                    true => Some(reader.read_i8()? as f32 * VELOCITY_READ_FACTOR),
                    false => None,
                };

                let punch_yaw = match flags.contains(ClientUpdateFlags::PUNCH_YAW) {
                    true => Some(reader.read_i8()? as f32),
                    false => None,
                };

                let velocity_y = match flags.contains(ClientUpdateFlags::VELOCITY_Y) {
                    true => Some(reader.read_i8()? as f32 * VELOCITY_READ_FACTOR),
                    false => None,
                };

                let punch_roll = match flags.contains(ClientUpdateFlags::PUNCH_ROLL) {
                    true => Some(reader.read_i8()? as f32),
                    false => None,
                };

                let velocity_z = match flags.contains(ClientUpdateFlags::VELOCITY_Z) {
                    true => Some(reader.read_i8()? as f32 * VELOCITY_READ_FACTOR),
                    false => None,
                };

                let items_bits = reader.read_u32::<LittleEndian>()?;
                let items = match ItemFlags::from_bits(items_bits) {
                    Some(i) => i,
                    None => {
                        return Err(NetError::invalid_data(format!("ItemFlags: {items_bits:b}")));
                    }
                };

                let on_ground = flags.contains(ClientUpdateFlags::ON_GROUND);
                let in_water = flags.contains(ClientUpdateFlags::IN_WATER);

                let weapon_frame = match flags.contains(ClientUpdateFlags::WEAPON_FRAME) {
                    true => Some(reader.read_u8()?),
                    false => None,
                };

                let armor = match flags.contains(ClientUpdateFlags::ARMOR) {
                    true => Some(reader.read_u8()?),
                    false => None,
                };

                let weapon_model_id = match flags.contains(ClientUpdateFlags::WEAPON) {
                    true => Some(reader.read_u8()? as _),
                    false => None,
                };

                let health = reader.read_i16::<LittleEndian>()?;
                let ammo = reader.read_u8()?;
                let ammo_shells = reader.read_u8()?;
                let ammo_nails = reader.read_u8()?;
                let ammo_rockets = reader.read_u8()?;
                let ammo_cells = reader.read_u8()?;
                let active_weapon = reader.read_u8()?;

                ServerCmd::PlayerData(PlayerData {
                    on_ground,
                    in_water,
                    view_height,
                    ideal_pitch,
                    punch_pitch,
                    velocity_x,
                    punch_yaw,
                    velocity_y,
                    punch_roll,
                    velocity_z,
                    items,
                    weapon_frame,
                    armor,
                    weapon_model_id,
                    health,
                    ammo,
                    ammo_shells,
                    ammo_nails,
                    ammo_rockets,
                    ammo_cells,
                    active_weapon,
                })
            }

            BasicServerCmdCode::StopSound => {
                let entity_channel = reader.read_u16::<LittleEndian>()?;
                let entity_id = entity_channel >> 3;
                let channel = (entity_channel & 0b111) as i8;

                ServerCmd::StopSound { entity_id, channel }
            }

            BasicServerCmdCode::UpdateColors => {
                let player_id = reader.read_u8()?;
                let new_colors_bits = reader.read_u8()?;
                let new_colors = PlayerColor::from_bits(new_colors_bits);

                ServerCmd::UpdateColors { player_id, new_colors }
            }

            BasicServerCmdCode::Particle => {
                let origin = read_coord_vector3(reader)?;

                let mut direction = Vec3::ZERO;
                for i in 0..3 {
                    direction[i] = reader.read_i8()? as f32 * PARTICLE_DIRECTION_READ_FACTOR;
                }

                let count = reader.read_u8()?;
                let color = reader.read_u8()?;

                ServerCmd::Particle { origin, direction, count, color }
            }

            BasicServerCmdCode::Damage => {
                let armor = reader.read_u8()?;
                let blood = reader.read_u8()?;
                let source = read_coord_vector3(reader)?;

                ServerCmd::Damage { armor, blood, source }
            }

            BasicServerCmdCode::SpawnStatic => {
                let model_id = reader.read_u8()?;
                let frame_id = reader.read_u8()?;
                let colormap = reader.read_u8()?;
                let skin_id = reader.read_u8()?;

                let mut origin = Vec3::ZERO;
                let mut angles = Vec3::ZERO;
                for i in 0..3 {
                    origin[i] = read_coord(reader)?;
                    angles[i] = read_angle(reader)?;
                }

                ServerCmd::SpawnStatic { model_id, frame_id, colormap, skin_id, origin, angles }
            }

            BasicServerCmdCode::SpawnBaseline => {
                let ent_id = reader.read_u16::<LittleEndian>()?;
                let model_id = reader.read_u8()?;
                let frame_id = reader.read_u8()?;
                let colormap = reader.read_u8()?;
                let skin_id = reader.read_u8()?;

                let mut origin = Vec3::ZERO;
                let mut angles = Vec3::ZERO;
                for i in 0..3 {
                    origin[i] = read_coord(reader)?;
                    angles[i] = read_angle(reader)?;
                }

                ServerCmd::SpawnBaseline {
                    ent_id,
                    model_id,
                    frame_id,
                    colormap,
                    skin_id,
                    origin,
                    angles,
                }
            }

            BasicServerCmdCode::TempEntity => {
                let temp_entity = TempEntity::read_temp_entity(reader)?;

                ServerCmd::TempEntity { temp_entity }
            }

            BasicServerCmdCode::SetPause => {
                let paused = match reader.read_u8()? {
                    0 => false,
                    1 => true,
                    x => return Err(NetError::invalid_data(format!("setpause: {x}"))),
                };

                ServerCmd::SetPause { paused }
            }

            BasicServerCmdCode::SignOnStage => {
                let stage_num = reader.read_u8()?;
                let stage = match SignOnStage::from_u8(stage_num) {
                    Some(s) => s,
                    None => {
                        return Err(NetError::invalid_data(format!(
                            "Invalid value for sign-on stage: {stage_num}"
                        )));
                    }
                };

                ServerCmd::SignOnStage { stage }
            }

            BasicServerCmdCode::CenterPrint => {
                let text = seismon_utils::read_cstring(reader)?;

                ServerCmd::CenterPrint { text }
            }

            BasicServerCmdCode::KilledMonster => ServerCmd::KilledMonster,
            BasicServerCmdCode::FoundSecret => ServerCmd::FoundSecret,

            BasicServerCmdCode::StaticSound => {
                let origin = read_coord_vector3(reader)?;
                let sound_id = reader.read_u8()?;
                let volume = reader.read_u8()?;
                let attenuation = reader.read_u8()?;

                ServerCmd::StaticSound { origin, sound_id, volume, attenuation }
            }

            BasicServerCmdCode::Intermission => ServerCmd::Intermission,

            BasicServerCmdCode::Finale => {
                let text = seismon_utils::read_cstring(reader)?;

                ServerCmd::Finale { text }
            }

            BasicServerCmdCode::CdTrack => {
                let track = reader.read_u8()? as _;
                let loop_ = reader.read_u8()?;
                ServerCmd::CdTrack { track, loop_ }
            }

            BasicServerCmdCode::SellScreen => ServerCmd::SellScreen,

            BasicServerCmdCode::Cutscene => {
                let text = seismon_utils::read_cstring(reader)?;

                ServerCmd::Cutscene { text }
            }
        };

        Ok(Some(cmd))
    }

    pub fn serialize<W>(&self, writer: &mut W) -> Result<(), NetError>
    where
        W: Write,
    {
        self.code().write(writer)?;

        match *self {
            ServerCmd::Bad | ServerCmd::NoOp | ServerCmd::Disconnect => {}

            ServerCmd::UpdateStat { stat, value } => {
                writer.write_u8(stat as u8)?;
                writer.write_i32::<LittleEndian>(value)?;
            }

            ServerCmd::Version { version } => {
                writer.write_i32::<LittleEndian>(version)?;
            }

            ServerCmd::SetView { ent_id } => {
                writer.write_u16::<LittleEndian>(ent_id)?;
            }

            ServerCmd::Sound { volume, attenuation, entity_id, channel, sound_id, position } => {
                let mut sound_flags = SoundFlags::empty();

                if volume.is_some() {
                    sound_flags |= SoundFlags::VOLUME;
                }

                if attenuation.is_some() {
                    sound_flags |= SoundFlags::ATTENUATION;
                }

                writer.write_u8(sound_flags.bits())?;

                if let Some(v) = volume {
                    writer.write_u8(v)?;
                }

                if let Some(a) = attenuation {
                    writer.write_u8((a * SOUND_ATTENUATION_WRITE_FACTOR as f32) as u8)?;
                }

                // TODO: document this better. The entity and channel fields are combined in Sound
                // commands.
                let ent_channel = (entity_id as i16) << 3 | channel as i16 & 0b111;
                writer.write_i16::<LittleEndian>(ent_channel)?;

                writer.write_u8(sound_id)?;

                for component in 0..3 {
                    write_coord(writer, position[component])?;
                }
            }

            ServerCmd::Time { time } => writer.write_f32::<LittleEndian>(time)?,

            ServerCmd::Print { ref text } => {
                writer.write_all(&text.raw)?;
                writer.write_u8(0)?;
            }

            ServerCmd::StuffText { ref text } => {
                writer.write_all(&text.raw)?;
                writer.write_u8(0)?;
            }

            ServerCmd::SetAngle { angles } => write_angle_vector3(writer, angles)?,

            ServerCmd::ServerInfo {
                protocol_version,
                max_clients,
                game_type,
                ref message,
                ref model_precache,
                ref sound_precache,
            } => {
                writer.write_i32::<LittleEndian>(protocol_version)?;
                writer.write_u8(max_clients)?;
                writer.write_u8(game_type as u8)?;

                writer.write_all(&message.raw)?;
                writer.write_u8(0)?;

                for model_name in model_precache.iter() {
                    writer.write_all(model_name.as_bytes())?;
                    writer.write_u8(0)?;
                }
                writer.write_u8(0)?;

                for sound_name in sound_precache.iter() {
                    writer.write_all(sound_name.as_bytes())?;
                    writer.write_u8(0)?;
                }
                writer.write_u8(0)?;
            }

            ServerCmd::LightStyle { id, ref value } => {
                writer.write_u8(id)?;
                writer.write_all(value)?;
                writer.write_u8(0)?;
            }

            ServerCmd::UpdateName { player_id, ref new_name } => {
                writer.write_u8(player_id)?;
                writer.write_all(&new_name.raw)?;
                writer.write_u8(0)?;
            }

            ServerCmd::UpdateFrags { player_id, new_frags } => {
                writer.write_u8(player_id)?;
                writer.write_i16::<LittleEndian>(new_frags)?;
            }

            ServerCmd::PlayerData(PlayerData {
                on_ground,
                in_water,
                view_height,
                ideal_pitch,
                punch_pitch,
                velocity_x,
                punch_yaw,
                velocity_y,
                punch_roll,
                velocity_z,
                items,
                weapon_frame,
                armor,
                weapon_model_id,
                health,
                ammo,
                ammo_shells,
                ammo_nails,
                ammo_rockets,
                ammo_cells,
                active_weapon,
            }) => {
                let mut flags = ClientUpdateFlags::empty();
                if view_height.is_some() {
                    flags |= ClientUpdateFlags::VIEW_HEIGHT;
                }
                if ideal_pitch.is_some() {
                    flags |= ClientUpdateFlags::IDEAL_PITCH;
                }
                if punch_pitch.is_some() {
                    flags |= ClientUpdateFlags::PUNCH_PITCH;
                }
                if velocity_x.is_some() {
                    flags |= ClientUpdateFlags::VELOCITY_X;
                }
                if punch_yaw.is_some() {
                    flags |= ClientUpdateFlags::PUNCH_YAW;
                }
                if velocity_y.is_some() {
                    flags |= ClientUpdateFlags::VELOCITY_Y;
                }
                if punch_roll.is_some() {
                    flags |= ClientUpdateFlags::PUNCH_ROLL;
                }
                if velocity_z.is_some() {
                    flags |= ClientUpdateFlags::VELOCITY_Z;
                }

                // items are always sent
                flags |= ClientUpdateFlags::ITEMS;

                if on_ground {
                    flags |= ClientUpdateFlags::ON_GROUND;
                }
                if in_water {
                    flags |= ClientUpdateFlags::IN_WATER;
                }
                if weapon_frame.is_some() {
                    flags |= ClientUpdateFlags::WEAPON_FRAME;
                }
                if armor.is_some() {
                    flags |= ClientUpdateFlags::ARMOR;
                }
                if weapon_model_id.is_some() {
                    flags |= ClientUpdateFlags::WEAPON;
                }

                // write flags
                writer.write_u16::<LittleEndian>(flags.bits())?;

                if let Some(vh) = view_height {
                    writer.write_u8(vh as i32 as u8)?;
                }
                if let Some(ip) = ideal_pitch {
                    writer.write_u8(ip as i32 as u8)?;
                }
                if let Some(pp) = punch_pitch {
                    writer.write_u8(pp as i32 as u8)?;
                }
                if let Some(vx) = velocity_x {
                    writer.write_u8((vx * VELOCITY_WRITE_FACTOR) as i32 as u8)?;
                }
                if let Some(py) = punch_yaw {
                    writer.write_u8(py as i32 as u8)?;
                }
                if let Some(vy) = velocity_y {
                    writer.write_u8((vy * VELOCITY_WRITE_FACTOR) as i32 as u8)?;
                }
                if let Some(pr) = punch_roll {
                    writer.write_u8(pr as i32 as u8)?;
                }
                if let Some(vz) = velocity_z {
                    writer.write_u8((vz * VELOCITY_WRITE_FACTOR) as i32 as u8)?;
                }
                writer.write_u32::<LittleEndian>(items.bits())?;
                if let Some(wf) = weapon_frame {
                    writer.write_u8(wf)?;
                }
                if let Some(a) = armor {
                    writer.write_u8(a)?;
                }
                if let Some(w) = weapon_model_id {
                    writer.write_u8(w)?;
                }
                writer.write_i16::<LittleEndian>(health)?;
                writer.write_u8(ammo)?;
                writer.write_u8(ammo_shells)?;
                writer.write_u8(ammo_nails)?;
                writer.write_u8(ammo_rockets)?;
                writer.write_u8(ammo_cells)?;
                writer.write_u8(active_weapon)?;
            }

            ServerCmd::StopSound { entity_id, channel } => {
                let entity_channel = entity_id << 3 | channel as u16 & 0b111;
                writer.write_u16::<LittleEndian>(entity_channel)?;
            }

            ServerCmd::UpdateColors { player_id, new_colors } => {
                writer.write_u8(player_id)?;
                writer.write_u8(new_colors.bits())?;
            }

            ServerCmd::Particle { origin, direction, count, color } => {
                write_coord_vector3(writer, origin)?;

                for i in 0..3 {
                    writer.write_i8(match direction[i] * PARTICLE_DIRECTION_WRITE_FACTOR {
                        d if d > i8::MAX as f32 => i8::MAX,
                        d if d < i8::MIN as f32 => i8::MIN,
                        d => d as i8,
                    })?;
                }

                writer.write_u8(count)?;
                writer.write_u8(color)?;
            }

            ServerCmd::Damage { armor, blood, source } => {
                writer.write_u8(armor)?;
                writer.write_u8(blood)?;
                write_coord_vector3(writer, source)?;
            }

            ServerCmd::SpawnStatic { model_id, frame_id, colormap, skin_id, origin, angles } => {
                writer.write_u8(model_id)?;
                writer.write_u8(frame_id)?;
                writer.write_u8(colormap)?;
                writer.write_u8(skin_id)?;

                for i in 0..3 {
                    write_coord(writer, origin[i])?;
                    write_angle(writer, angles[i])?;
                }
            }

            ServerCmd::SpawnBaseline {
                ent_id,
                model_id,
                frame_id,
                colormap,
                skin_id,
                origin,
                angles,
            } => {
                writer.write_u16::<LittleEndian>(ent_id)?;
                writer.write_u8(model_id)?;
                writer.write_u8(frame_id)?;
                writer.write_u8(colormap)?;
                writer.write_u8(skin_id)?;

                for i in 0..3 {
                    write_coord(writer, origin[i])?;
                    write_angle(writer, angles[i])?;
                }
            }

            ServerCmd::TempEntity { ref temp_entity } => {
                temp_entity.write_temp_entity(writer)?;
            }

            ServerCmd::SetPause { paused } => {
                writer.write_u8(match paused {
                    false => 0,
                    true => 1,
                })?;
            }

            ServerCmd::SignOnStage { stage } => {
                writer.write_u8(stage as u8)?;
            }

            ServerCmd::CenterPrint { ref text } => {
                writer.write_all(&text.raw)?;
                writer.write_u8(0)?;
            }

            ServerCmd::KilledMonster | ServerCmd::FoundSecret => {}

            ServerCmd::StaticSound { origin, sound_id, volume, attenuation } => {
                write_coord_vector3(writer, origin)?;
                writer.write_u8(sound_id)?;
                writer.write_u8(volume)?;
                writer.write_u8(attenuation)?;
            }

            ServerCmd::Intermission => {}

            ServerCmd::Finale { ref text } => {
                writer.write_all(&text.raw)?;
                writer.write_u8(0)?;
            }

            ServerCmd::CdTrack { track, loop_ } => {
                writer.write_u8(track.try_into().map_err(|_| NetError::Other {
                    msg: format!("Track {track} overflowed u8"),
                    backtrace: Backtrace::capture(),
                })?)?;
                writer.write_u8(loop_)?;
            }

            ServerCmd::SellScreen => {}

            ServerCmd::Cutscene { ref text } => {
                writer.write_all(&text.raw)?;
                writer.write_u8(0)?;
            }

            ServerCmd::FastUpdate(ref update) => {
                update.write(writer)?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::io::BufReader;

    use super::*;

    #[test]
    fn test_server_cmd_update_stat_read_write_eq() {
        let src = ServerCmd::UpdateStat { stat: ClientStat::Nails, value: 64 };

        let mut packet = Vec::new();
        src.serialize(&mut packet).unwrap();
        let mut reader = BufReader::new(packet.as_slice());
        let dst = ServerCmd::deserialize(&mut reader).unwrap().unwrap();

        assert_eq!(src, dst);
    }

    #[test]
    fn test_server_cmd_spawn_baseline_stream_read_write_eq() {
        // A baseline without a model should still be serialized.
        let src = [0u8, 3, 0, 7]
            .into_iter()
            .enumerate()
            .map(|(i, model_id)| ServerCmd::SpawnBaseline {
                ent_id: i as u16 + 1,
                model_id,
                frame_id: i as u8,
                colormap: 0,
                skin_id: 1,
                origin: Vec3::new(16. * i as f32, -8., 0.5),
                angles: Vec3::new(0., 90., 0.),
            })
            .chain([ServerCmd::Version { version: 42 }])
            .collect::<Vec<_>>();

        let mut packet = Vec::new();
        for cmd in &src {
            cmd.serialize(&mut packet).unwrap();
        }

        let mut reader = BufReader::new(packet.as_slice());
        let mut dst = Vec::new();
        while let Some(cmd) = ServerCmd::deserialize(&mut reader).unwrap() {
            dst.push(cmd);
        }

        assert_eq!(src, dst);
    }

    #[test]
    fn test_server_cmd_version_read_write_eq() {
        let src = ServerCmd::Version { version: 42 };

        let mut packet = Vec::new();
        src.serialize(&mut packet).unwrap();
        let mut reader = BufReader::new(packet.as_slice());
        let dst = ServerCmd::deserialize(&mut reader).unwrap().unwrap();

        assert_eq!(src, dst);
    }

    #[test]
    fn test_server_cmd_set_view_read_write_eq() {
        let src = ServerCmd::SetView { ent_id: 17 };

        let mut packet = Vec::new();
        src.serialize(&mut packet).unwrap();
        let mut reader = BufReader::new(packet.as_slice());
        let dst = ServerCmd::deserialize(&mut reader).unwrap().unwrap();

        assert_eq!(src, dst);
    }

    #[test]
    fn test_server_cmd_time_read_write_eq() {
        let src = ServerCmd::Time { time: 23.07 };

        let mut packet = Vec::new();
        src.serialize(&mut packet).unwrap();
        let mut reader = BufReader::new(packet.as_slice());
        let dst = ServerCmd::deserialize(&mut reader).unwrap().unwrap();

        assert_eq!(src, dst);
    }

    #[test]
    fn test_server_cmd_print_read_write_eq() {
        let src = ServerCmd::Print { text: QString::from("print test") };

        let mut packet = Vec::new();
        src.serialize(&mut packet).unwrap();
        let mut reader = BufReader::new(packet.as_slice());
        let dst = ServerCmd::deserialize(&mut reader).unwrap().unwrap();

        assert_eq!(src, dst);
    }

    #[test]
    fn test_server_cmd_stuff_text_read_write_eq() {
        let src = ServerCmd::StuffText { text: QString::from("stufftext test") };

        let mut packet = Vec::new();
        src.serialize(&mut packet).unwrap();
        let mut reader = BufReader::new(packet.as_slice());
        let dst = ServerCmd::deserialize(&mut reader).unwrap().unwrap();

        assert_eq!(src, dst);
    }

    #[test]
    fn test_server_cmd_server_info_read_write_eq() {
        let src = ServerCmd::ServerInfo {
            protocol_version: 42,
            max_clients: 16,
            game_type: GameType::Deathmatch,
            message: QString::from("Test message"),
            model_precache: vec!["test1.bsp".to_string(), "test2.bsp".to_string()],
            sound_precache: vec!["test1.wav".to_string(), "test2.wav".to_string()],
        };

        let mut packet = Vec::new();
        src.serialize(&mut packet).unwrap();
        let mut reader = BufReader::new(packet.as_slice());
        let dst = ServerCmd::deserialize(&mut reader).unwrap().unwrap();

        assert_eq!(src, dst);
    }

    #[test]
    fn test_server_cmd_light_style_read_write_eq() {
        let src = ServerCmd::LightStyle {
            id: 11,
            value: QString::from("aaaaabcddeefgghjjjkaaaazzzzyxwaaaba"),
        };

        let mut packet = Vec::new();
        src.serialize(&mut packet).unwrap();
        let mut reader = BufReader::new(packet.as_slice());
        let dst = ServerCmd::deserialize(&mut reader).unwrap().unwrap();

        assert_eq!(src, dst);
    }

    #[test]
    fn test_server_cmd_update_name_read_write_eq() {
        let src = ServerCmd::UpdateName { player_id: 7, new_name: QString::from("newname") };

        let mut packet = Vec::new();
        src.serialize(&mut packet).unwrap();
        let mut reader = BufReader::new(packet.as_slice());
        let dst = ServerCmd::deserialize(&mut reader).unwrap().unwrap();

        assert_eq!(src, dst);
    }

    #[test]
    fn test_server_cmd_update_frags_read_write_eq() {
        let src = ServerCmd::UpdateFrags { player_id: 7, new_frags: 11 };

        let mut packet = Vec::new();
        src.serialize(&mut packet).unwrap();
        let mut reader = BufReader::new(packet.as_slice());
        let dst = ServerCmd::deserialize(&mut reader).unwrap().unwrap();

        assert_eq!(src, dst);
    }

    #[test]
    fn test_server_cmd_stop_sound_read_write_eq() {
        let src = ServerCmd::StopSound { entity_id: 17, channel: 3 };

        let mut packet = Vec::new();
        src.serialize(&mut packet).unwrap();
        let mut reader = BufReader::new(packet.as_slice());
        let dst = ServerCmd::deserialize(&mut reader).unwrap().unwrap();

        assert_eq!(src, dst);
    }

    #[test]
    fn test_server_cmd_update_colors_read_write_eq() {
        let src = ServerCmd::UpdateColors { player_id: 11, new_colors: PlayerColor::new(4, 13) };

        let mut packet = Vec::new();
        src.serialize(&mut packet).unwrap();
        let mut reader = BufReader::new(packet.as_slice());
        let dst = ServerCmd::deserialize(&mut reader).unwrap().unwrap();

        assert_eq!(src, dst);
    }

    #[test]
    fn test_server_cmd_set_pause_read_write_eq() {
        let src = ServerCmd::SetPause { paused: true };
        let mut packet = Vec::new();
        src.serialize(&mut packet).unwrap();
        let mut reader = BufReader::new(packet.as_slice());
        let dst = ServerCmd::deserialize(&mut reader).unwrap().unwrap();

        assert_eq!(src, dst);
    }

    #[test]
    fn test_server_cmd_sign_on_stage_read_write_eq() {
        let src = ServerCmd::SignOnStage { stage: SignOnStage::Begin };
        let mut packet = Vec::new();
        src.serialize(&mut packet).unwrap();
        let mut reader = BufReader::new(packet.as_slice());
        let dst = ServerCmd::deserialize(&mut reader).unwrap().unwrap();

        assert_eq!(src, dst);
    }

    #[test]
    fn test_server_cmd_center_print_read_write_eq() {
        let src = ServerCmd::CenterPrint { text: QString::from("Center print test") };
        let mut packet = Vec::new();
        src.serialize(&mut packet).unwrap();
        let mut reader = BufReader::new(packet.as_slice());
        let dst = ServerCmd::deserialize(&mut reader).unwrap().unwrap();

        assert_eq!(src, dst);
    }

    #[test]
    fn test_server_cmd_finale_read_write_eq() {
        let src = ServerCmd::Finale { text: QString::from("Finale test") };
        let mut packet = Vec::new();
        src.serialize(&mut packet).unwrap();
        let mut reader = BufReader::new(packet.as_slice());
        let dst = ServerCmd::deserialize(&mut reader).unwrap().unwrap();

        assert_eq!(src, dst);
    }

    #[test]
    fn test_server_cmd_cd_track_read_write_eq() {
        let src = ServerCmd::CdTrack { track: 5, loop_: 1 };
        let mut packet = Vec::new();
        src.serialize(&mut packet).unwrap();
        let mut reader = BufReader::new(packet.as_slice());
        let dst = ServerCmd::deserialize(&mut reader).unwrap().unwrap();

        assert_eq!(src, dst);
    }

    #[test]
    fn test_server_cmd_cutscene_read_write_eq() {
        let src = ServerCmd::Cutscene { text: QString::from("Cutscene test") };
        let mut packet = Vec::new();
        src.serialize(&mut packet).unwrap();
        let mut reader = BufReader::new(packet.as_slice());
        let dst = ServerCmd::deserialize(&mut reader).unwrap().unwrap();

        assert_eq!(src, dst);
    }
}
