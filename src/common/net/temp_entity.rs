//! Temporary entities (explosions, beams, etc).

use std::io::Read;

use bevy::prelude::*;
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use num::FromPrimitive;
use num_derive::FromPrimitive;

use super::*;

/// Numeric codes used to identify the type of a temporary entity.
#[derive(Debug, Eq, FromPrimitive, PartialEq)]
pub enum TempEntityCode {
    Spike = 0,
    SuperSpike = 1,
    Gunshot = 2,
    Explosion = 3,
    TarExplosion = 4,
    Lightning1 = 5,
    Lightning2 = 6,
    WizSpike = 7,
    KnightSpike = 8,
    Lightning3 = 9,
    LavaSplash = 10,
    Teleport = 11,
    ColorExplosion = 12,
    Grapple = 13,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PointEntityKind {
    Spike,
    SuperSpike,
    Gunshot,
    Explosion,
    ColorExplosion { color_start: u8, color_len: u8 },
    TarExplosion,
    WizSpike,
    KnightSpike,
    LavaSplash,
    Teleport,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BeamEntityKind {
    /// Lightning bolt
    Lightning {
        /// id of the lightning model to use. must be 1, 2, or 3.
        model_id: u8,
    },
    /// Grappling hook cable
    Grapple,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TempEntity {
    Point { kind: PointEntityKind, origin: Vec3 },
    Beam { kind: BeamEntityKind, entity_id: i16, start: Vec3, end: Vec3 },
}

impl TempEntity {
    pub fn read_temp_entity<R>(reader: &mut R) -> Result<TempEntity, NetError>
    where
        R: Read,
    {
        let code_byte = reader.read_u8()?;
        let code = match TempEntityCode::from_u8(code_byte) {
            Some(c) => c,
            None => {
                return Err(NetError::invalid_data(format!("Temp entity code {code_byte}",)));
            }
        };

        use TempEntity::*;
        use TempEntityCode as Code;

        Ok(match code {
            Code::Spike
            | Code::SuperSpike
            | Code::Gunshot
            | Code::Explosion
            | Code::TarExplosion
            | Code::WizSpike
            | Code::KnightSpike
            | Code::LavaSplash
            | Code::Teleport => Point {
                kind: match code {
                    Code::Spike => PointEntityKind::Spike,
                    Code::SuperSpike => PointEntityKind::SuperSpike,
                    Code::Gunshot => PointEntityKind::Gunshot,
                    Code::Explosion => PointEntityKind::Explosion,
                    Code::TarExplosion => PointEntityKind::TarExplosion,
                    Code::WizSpike => PointEntityKind::WizSpike,
                    Code::KnightSpike => PointEntityKind::KnightSpike,
                    Code::LavaSplash => PointEntityKind::LavaSplash,
                    Code::Teleport => PointEntityKind::Teleport,
                    _ => unreachable!(),
                },
                origin: read_coord_vector3(reader)?,
            },
            Code::ColorExplosion => {
                let origin = read_coord_vector3(reader)?;
                let color_start = reader.read_u8()?;
                let color_len = reader.read_u8()?;

                Point { origin, kind: PointEntityKind::ColorExplosion { color_start, color_len } }
            }
            Code::Lightning1 | Code::Lightning2 | Code::Lightning3 => Beam {
                kind: BeamEntityKind::Lightning {
                    model_id: match code {
                        Code::Lightning1 => 1,
                        Code::Lightning2 => 2,
                        Code::Lightning3 => 3,
                        _ => unreachable!(),
                    },
                },
                entity_id: reader.read_i16::<LittleEndian>()?,
                start: read_coord_vector3(reader)?,
                end: read_coord_vector3(reader)?,
            },
            Code::Grapple => Beam {
                kind: BeamEntityKind::Grapple,
                entity_id: reader.read_i16::<LittleEndian>()?,
                start: read_coord_vector3(reader)?,
                end: read_coord_vector3(reader)?,
            },
        })
    }

    pub fn write_temp_entity<W>(&self, writer: &mut W) -> Result<(), NetError>
    where
        W: WriteBytesExt,
    {
        use TempEntityCode as Code;

        match *self {
            TempEntity::Point { kind, origin } => {
                use PointEntityKind as Pk;
                match kind {
                    Pk::Spike
                    | Pk::SuperSpike
                    | Pk::Gunshot
                    | Pk::Explosion
                    | Pk::TarExplosion
                    | Pk::WizSpike
                    | Pk::KnightSpike
                    | Pk::LavaSplash
                    | Pk::Teleport => {
                        let code = match kind {
                            Pk::Spike => Code::Spike,
                            Pk::SuperSpike => Code::SuperSpike,
                            Pk::Gunshot => Code::Gunshot,
                            Pk::Explosion => Code::Explosion,
                            Pk::TarExplosion => Code::TarExplosion,
                            Pk::WizSpike => Code::WizSpike,
                            Pk::KnightSpike => Code::KnightSpike,
                            Pk::LavaSplash => Code::LavaSplash,
                            Pk::Teleport => Code::Teleport,
                            _ => unreachable!(),
                        };

                        // write code
                        writer.write_u8(code as u8)?;
                    }
                    PointEntityKind::ColorExplosion { color_start, color_len } => {
                        // write code and colors
                        writer.write_u8(Code::ColorExplosion as u8)?;
                        writer.write_u8(color_start)?;
                        writer.write_u8(color_len)?;
                    }
                };

                write_coord_vector3(writer, origin)?;
            }

            TempEntity::Beam { kind, entity_id, start, end } => {
                let code = match kind {
                    BeamEntityKind::Lightning { model_id } => match model_id {
                        1 => Code::Lightning1,
                        2 => Code::Lightning2,
                        3 => Code::Lightning3,
                        // TODO: error
                        _ => panic!("invalid lightning model id: {model_id}"),
                    },
                    BeamEntityKind::Grapple => Code::Grapple,
                };
                writer.write_i16::<LittleEndian>(entity_id)?;
                writer.write_u8(code as u8)?;
                write_coord_vector3(writer, start)?;
                write_coord_vector3(writer, end)?;
            }
        }

        Ok(())
    }
}
