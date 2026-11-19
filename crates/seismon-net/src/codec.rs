//! Coordinate and angle encoding.

// TODO: need to figure out an equivalence relation for read_/write_coord and read_/write_angle

use std::io::{self, Read, Write};

use bevy_math::Vec3;
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use seismon_utils::QAngles;

pub(super) fn read_coord<R>(reader: &mut R) -> io::Result<f32>
where
    R: Read,
{
    Ok(reader.read_i16::<LittleEndian>()? as f32 / 8.0)
}

pub(super) fn read_coord_vector3<R>(reader: &mut R) -> io::Result<Vec3>
where
    R: Read,
{
    Ok(Vec3::new(read_coord(reader)?, read_coord(reader)?, read_coord(reader)?))
}

pub(super) fn write_coord<W>(writer: &mut W, coord: f32) -> io::Result<()>
where
    W: Write,
{
    writer.write_i16::<LittleEndian>((coord * 8.0) as i16)?;
    Ok(())
}

pub(super) fn write_coord_vector3<W>(writer: &mut W, coords: Vec3) -> io::Result<()>
where
    W: Write,
{
    for coord in &coords.to_array()[..] {
        write_coord(writer, *coord)?;
    }

    Ok(())
}

/// Map from `0..256` to `0..360`
pub(super) fn read_angle<R>(reader: &mut R) -> io::Result<f32>
where
    R: Read,
{
    Ok(reader.read_i8()? as f32 * (360.0 / 256.0))
}

/// Converts an angle in degrees to a byte.
///
/// Equivalent to `MSG_WriteAngle`.
pub fn write_angle_byte(angle: f32) -> u8 {
    // C float-to-int wraps, but in Rust it truncates. We cast
    // to a larger int first to ensure that we wrap here.
    (((angle as i32) * 256 / 360) & 255) as u8
}

pub(super) fn write_angle<W>(writer: &mut W, angle: f32) -> io::Result<()>
where
    W: Write,
{
    writer.write_u8(write_angle_byte(angle))?;
    Ok(())
}

pub(super) fn write_angle_vector3<W>(writer: &mut W, angles: QAngles) -> io::Result<()>
where
    W: Write,
{
    for angle in &angles.to_array()[..] {
        write_angle(writer, *angle)?;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn write_angle_wraps() {
        assert_eq!(write_angle_byte(0.), 0);
        assert_eq!(write_angle_byte(90.), 64);
        assert_eq!(write_angle_byte(-90.), 192);
        assert_eq!(write_angle_byte(360.), 0);
        assert_eq!(write_angle_byte(450.), 64);
        assert_eq!(write_angle_byte(1.9), 0);
    }

    #[test]
    fn write_angle_does_not_saturate() {
        // `write_angle` used to cast with `as i8`, which saturates
        for angle in [270., 370., 540., -200., 1000., -1000.] {
            let mut buf = Vec::new();
            write_angle(&mut buf, angle).unwrap();
            assert_eq!(buf, [write_angle_byte(angle)], "angle {angle}");
        }
        // 1000 truncates to 711 byte units, which wraps to 199 (~280deg).
        assert_eq!(write_angle_byte(1000.), 199);
        // -200 truncates to -142 byte units, which wraps to 114 (~160deg).
        assert_eq!(write_angle_byte(-200.), 114);
    }
}
