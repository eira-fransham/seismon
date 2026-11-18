//! Protocol constants.

pub const MAX_MESSAGE: usize = 8192;
pub(super) const MAX_DATAGRAM: usize = 1024;
pub(super) const HEADER_SIZE: usize = 8;
pub(super) const MAX_PACKET: usize = HEADER_SIZE + MAX_DATAGRAM;

pub const PROTOCOL_VERSION: u8 = 15;

pub(super) const FAST_UPDATE_FLAG: u8 = 0x80;

pub(super) const VELOCITY_READ_FACTOR: f32 = 16.0;
pub(super) const VELOCITY_WRITE_FACTOR: f32 = 1.0 / VELOCITY_READ_FACTOR;

pub(super) const PARTICLE_DIRECTION_READ_FACTOR: f32 = 1.0 / 16.0;
pub(super) const PARTICLE_DIRECTION_WRITE_FACTOR: f32 = 1.0 / PARTICLE_DIRECTION_READ_FACTOR;

pub(super) const SOUND_ATTENUATION_WRITE_FACTOR: u8 = 64;
pub(super) const SOUND_ATTENUATION_READ_FACTOR: f32 = 1.0 / SOUND_ATTENUATION_WRITE_FACTOR as f32;

pub static GAME_NAME: &str = "QUAKE";
pub const MAX_CLIENTS: usize = 16;
pub const MAX_ITEMS: usize = 32;

pub const DEFAULT_VIEWHEIGHT: f32 = 22.0;
