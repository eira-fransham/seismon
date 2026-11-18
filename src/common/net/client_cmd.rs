//! Client-to-server messages.

use std::{
    io::{self, BufRead},
    time::Duration,
};

use beef::Cow;
use bevy::prelude::*;
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use num::FromPrimitive;
use num_derive::FromPrimitive;
use seismon_utils::QAngles;

use super::*;

#[derive(FromPrimitive)]
pub enum ClientCmdCode {
    Bad = 0,
    NoOp = 1,
    Disconnect = 2,
    Move = 3,
    StringCmd = 4,
}

#[derive(Debug, PartialEq)]
pub enum ClientCmd {
    Bad,
    NoOp,
    Disconnect,
    Move {
        delta_time: Duration,
        angles: QAngles,
        fwd_move: i16,
        side_move: i16,
        up_move: i16,
        button_flags: ButtonFlags,
        impulse: u8,
    },
    StringCmd {
        cmd: Cow<'static, str>,
    },
}

impl ClientCmd {
    pub fn code(&self) -> u8 {
        match *self {
            ClientCmd::Bad => ClientCmdCode::Bad as u8,
            ClientCmd::NoOp => ClientCmdCode::NoOp as u8,
            ClientCmd::Disconnect => ClientCmdCode::Disconnect as u8,
            ClientCmd::Move { .. } => ClientCmdCode::Move as u8,
            ClientCmd::StringCmd { .. } => ClientCmdCode::StringCmd as u8,
        }
    }

    pub fn deserialize<R>(reader: &mut R) -> Result<Option<ClientCmd>, NetError>
    where
        R: ReadBytesExt + BufRead,
    {
        let code_val = match reader.read_u8() {
            Ok(val) => val,
            Err(e) if e.kind() == io::ErrorKind::UnexpectedEof => return Ok(None),
            Err(e) => return Err(e.into()),
        };
        let code = match ClientCmdCode::from_u8(code_val) {
            Some(c) => c,
            None => {
                return Err(NetError::invalid_data(format!(
                    "Invalid client command code: {code_val}"
                )));
            }
        };

        let cmd = match code {
            ClientCmdCode::Bad => ClientCmd::Bad,
            ClientCmdCode::NoOp => ClientCmd::NoOp,
            ClientCmdCode::Disconnect => ClientCmd::Disconnect,
            ClientCmdCode::Move => {
                let delta_time =
                    seismon_utils::duration_from_f32(reader.read_f32::<LittleEndian>()? / 1000.);
                let angles = [read_angle(reader)?, read_angle(reader)?, read_angle(reader)?].into();
                let fwd_move = reader.read_i16::<LittleEndian>()?;
                let side_move = reader.read_i16::<LittleEndian>()?;
                let up_move = reader.read_i16::<LittleEndian>()?;
                let button_flags_val = reader.read_u8()?;
                let button_flags = match ButtonFlags::from_bits(button_flags_val) {
                    Some(bf) => bf,
                    None => {
                        error!("Invalid value for button flags: {button_flags_val:x}");
                        ButtonFlags::from_bits_truncate(button_flags_val)
                    }
                };
                let impulse = reader.read_u8()?;
                ClientCmd::Move {
                    delta_time,
                    angles,
                    fwd_move,
                    side_move,
                    up_move,
                    button_flags,
                    impulse,
                }
            }
            ClientCmdCode::StringCmd => {
                let cmd = seismon_utils::read_cstring(reader)?.into_str().into_owned().into();
                ClientCmd::StringCmd { cmd }
            }
        };

        Ok(Some(cmd))
    }

    pub fn serialize<W>(&self, writer: &mut W) -> Result<(), NetError>
    where
        W: WriteBytesExt,
    {
        writer.write_u8(self.code())?;

        match *self {
            ClientCmd::Bad => {}
            ClientCmd::NoOp => {}
            ClientCmd::Disconnect => {}
            ClientCmd::Move {
                delta_time,
                angles,
                fwd_move,
                side_move,
                up_move,
                button_flags,
                impulse,
            } => {
                // Delta time is in ms for this field.
                writer.write_f32::<LittleEndian>(
                    seismon_utils::duration_to_f32(delta_time) * 1000.,
                )?;
                write_angle_vector3(writer, angles)?;
                writer.write_i16::<LittleEndian>(fwd_move)?;
                writer.write_i16::<LittleEndian>(side_move)?;
                writer.write_i16::<LittleEndian>(up_move)?;
                writer.write_u8(button_flags.bits())?;
                writer.write_u8(impulse)?;
            }
            ClientCmd::StringCmd { ref cmd } => {
                writer.write_all(cmd.as_bytes())?;
                writer.write_u8(0)?;
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
    fn test_client_cmd_string_cmd_read_write_eq() {
        let src = ClientCmd::StringCmd { cmd: String::from("StringCmd test").into() };
        let mut packet = Vec::new();
        src.serialize(&mut packet).unwrap();
        let mut reader = BufReader::new(packet.as_slice());
        let dst = ClientCmd::deserialize(&mut reader).unwrap().unwrap();

        assert_eq!(src, dst);
    }

    #[test]
    fn test_client_cmd_move_read_write_eq() {
        let src = ClientCmd::Move {
            delta_time: Duration::from_millis(1234),
            // have to use angles that won't lose precision from write_angle
            angles: [90.0, -90.0, 0.0].into(),
            fwd_move: 27,
            side_move: 85,
            up_move: 76,
            button_flags: ButtonFlags::empty(),
            impulse: 121,
        };

        let mut packet = Vec::new();
        src.serialize(&mut packet).unwrap();
        let mut reader = BufReader::new(packet.as_slice());
        let dst = ClientCmd::deserialize(&mut reader).unwrap().unwrap();

        assert_eq!(src, dst);
    }
}
