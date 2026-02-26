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

use std::{
    fmt,
    io::{self, BufRead},
    mem,
    ops::{Add, Deref, Mul, Not},
    time::Duration,
};

use beef::Cow;
use bevy_math::{EulerRot, Mat3, Quat};
use bevy_reflect::Reflect;
use byteorder::{LittleEndian, ReadBytesExt};
use futures::AsyncReadExt;
use futures_byteorder::AsyncReadBytes;
use nom::AsBytes;

pub mod model;

const QUAKE_ROLL_PITCH_YAW: EulerRot = EulerRot::XYZEx;

impl From<QAngles> for Quat {
    fn from(angles: QAngles) -> Quat {
        // See https://github.com/id-Software/Quake/blob/master/WinQuake/r_alias.c#L364-L369,
        // pitch (rotation around `right`) is inverted, but yaw and roll are not.
        Quat::from_euler(
            QUAKE_ROLL_PITCH_YAW,
            angles.roll_deg.to_radians(),
            -angles.pitch_deg.to_radians(),
            angles.yaw_deg.to_radians(),
        )
    }
}

impl From<QAngles> for Mat3 {
    fn from(angles: QAngles) -> Mat3 {
        Mat3::from_quat(angles.into())
    }
}

impl From<Quat> for QAngles {
    fn from(angles: Quat) -> QAngles {
        let (roll_rad, pitch_rad, yaw_rad) = angles.to_euler(QUAKE_ROLL_PITCH_YAW);

        QAngles {
            pitch_deg: -pitch_rad.to_degrees(),
            // TODO: Unclear if this is correct, we might need to invert it.
            roll_deg: roll_rad.to_degrees(),
            yaw_deg: yaw_rad.to_degrees(),
        }
    }
}

#[derive(Default, Reflect, Copy, Clone, PartialEq, Debug)]
pub struct QAngles {
    pub pitch_deg: f32,
    pub yaw_deg: f32,
    pub roll_deg: f32,
}

impl QAngles {
    pub fn to_array(&self) -> [f32; 3] {
        [self.pitch_deg, self.yaw_deg, self.roll_deg]
    }
}

impl From<[f32; 3]> for QAngles {
    fn from([pitch_deg, yaw_deg, roll_deg]: [f32; 3]) -> Self {
        Self { pitch_deg, yaw_deg, roll_deg }
    }
}

impl Mul<f32> for QAngles {
    type Output = Self;

    fn mul(self, rhs: f32) -> Self::Output {
        Self {
            pitch_deg: self.pitch_deg * rhs,
            roll_deg: self.roll_deg * rhs,
            yaw_deg: self.yaw_deg * rhs,
        }
    }
}

impl Add for QAngles {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            pitch_deg: self.pitch_deg + rhs.pitch_deg,
            roll_deg: self.roll_deg + rhs.roll_deg,
            yaw_deg: self.yaw_deg + rhs.yaw_deg,
        }
    }
}

// TODO: Copied from Quake 1 source, can be done better.
fn angle_mod(quake_angle: f32) -> f32 {
    const PRECISION: f32 = 65536.;
    let quake_angle = quake_angle.rem_euclid(360.);

    (360f32 / PRECISION) * (quake_angle * PRECISION / 360.0).floor()
}

impl QAngles {
    /// Quantize the precision of each field to 16 bits.
    pub fn quantize(&self) -> Self {
        Self {
            pitch_deg: angle_mod(self.pitch_deg),
            roll_deg: angle_mod(self.roll_deg),
            yaw_deg: angle_mod(self.yaw_deg),
        }
    }
}

// TODO: handle this unwrap? i64 can handle ~200,000 years in microseconds
#[inline]
pub fn duration_to_f32(d: Duration) -> f32 {
    d.as_micros() as f32 / 1_000_000.0
}

#[inline]
pub fn duration_from_f32(f: f32) -> Duration {
    Duration::from_micros((f * 1_000_000.) as u64)
}

#[derive(Default, Copy, Clone, PartialEq, Eq, Hash)]
pub enum StringColor {
    #[default]
    Default,
    Red,
}

impl Not for StringColor {
    type Output = Self;
    fn not(self) -> Self::Output {
        match self {
            Self::Default => Self::Red,
            Self::Red => Self::Default,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct QStr<'a> {
    pub raw: Cow<'a, [u8]>,
}

impl fmt::Debug for QStr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.to_str())
    }
}

impl<'a> QStr<'a> {
    pub fn into_owned(self) -> QString {
        QStr { raw: self.raw.into_owned().into() }
    }
}

pub type QString = QStr<'static>;

impl Default for QStr<'_> {
    fn default() -> Self {
        Self { raw: Cow::borrowed(&[]) }
    }
}

impl<'a> From<&'a str> for QStr<'a> {
    fn from(value: &'a str) -> Self {
        value.as_bytes().into()
    }
}

impl From<String> for QString {
    fn from(value: String) -> Self {
        value.into_bytes().into()
    }
}

impl<'a> From<&'a [u8]> for QStr<'a> {
    fn from(value: &'a [u8]) -> Self {
        Self { raw: Cow::borrowed(value.as_bytes()) }
    }
}

impl From<Vec<u8>> for QString {
    fn from(value: Vec<u8>) -> Self {
        Self { raw: Cow::owned(value) }
    }
}

impl Deref for QStr<'_> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.raw
    }
}

impl AsRef<[u8]> for QStr<'_> {
    fn as_ref(&self) -> &[u8] {
        self
    }
}

impl fmt::Display for QStr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.to_str().fmt(f)
    }
}

impl<'a> QStr<'a> {
    /// Borrows this `QString` as a regular str, without any color information
    pub fn to_str(&self) -> Cow<'_, str> {
        self.reborrow().into_str()
    }

    /// Converts this `QString` into a regular str, without any color information
    pub fn into_str(self) -> Cow<'a, str> {
        let mut out = self.raw;
        for i in 0..out.len() {
            if out[i] >= 128 {
                let mut new = out.into_owned();
                new[i] %= 128;
                out = new.into();
            }
        }

        if out.is_owned() {
            String::from_utf8(out.into_owned()).unwrap().into()
        } else {
            std::str::from_utf8(out.unwrap_borrowed()).unwrap().into()
        }
    }

    pub fn into_string(self) -> String {
        self.into_str().into_owned()
    }

    pub fn reborrow(&self) -> QStr<'_> {
        QStr { raw: Cow::borrowed(&*self.raw) }
    }

    pub fn chars(&self) -> impl Iterator<Item = (char, StringColor)> + '_ {
        self.raw.iter().copied().map(|b| {
            ((b % 128) as char, if b >= 128 { StringColor::Red } else { StringColor::Default })
        })
    }

    pub fn lines(&self) -> impl Iterator<Item = QStr<'_>> + '_ {
        self.raw.chunk_by(|_, b| *b != b'\n').map(|bytes| QStr {
            raw: Cow::borrowed(bytes.strip_prefix(&b"\n"[..]).unwrap_or(bytes)),
        })
    }

    pub fn clear(&mut self) {
        if self.raw.is_owned() {
            let mut raw = mem::take(&mut self.raw).into_owned();
            raw.clear();
            self.raw = raw.into();
        } else {
            self.raw = (&[][..]).into();
        }
    }

    pub fn push_str<S: AsRef<str>>(&mut self, s: S) {
        self.push_bytes(s.as_ref().as_bytes())
    }

    pub fn push_bytes<S: AsRef<[u8]>>(&mut self, s: S) {
        let mut raw = mem::take(&mut self.raw).into_owned();
        raw.extend_from_slice(s.as_ref());
        self.raw = raw.into();
    }

    pub fn truncate(&mut self, to: usize) {
        if self.raw.is_owned() {
            let mut raw = mem::take(&mut self.raw).into_owned();
            raw.truncate(to);
            self.raw = raw.into();
        } else {
            self.raw = (&mem::take(&mut self.raw).unwrap_borrowed()[..to]).into();
        }
    }
}

/// Read a `[f32; 3]` in little-endian byte order.
pub fn read_f32_3<R>(reader: &mut R) -> Result<[f32; 3], std::io::Error>
where
    R: ReadBytesExt,
{
    let mut ar = [0.0f32; 3];
    reader.read_f32_into::<LittleEndian>(&mut ar)?;
    Ok(ar)
}

/// Read a null-terminated sequence of bytes and convert it into a `String`.
///
/// The zero byte is consumed.
///
/// ## Panics
/// - If the end of the input is reached before a zero byte is found.
pub fn read_cstring<R>(src: &mut R) -> io::Result<QString>
// QStringOwned
where
    R: BufRead,
{
    // TODO: `BufRead` would be better here
    let mut bytes: Vec<u8> = Vec::new();
    loop {
        let next_byte = src.read_u8()?;
        if next_byte == 0 {
            break;
        } else {
            bytes.push(next_byte);
        }
    }

    Ok(QStr { raw: Cow::owned(bytes) })
}

/// Read a `[f32; 3]` in little-endian byte order.
pub async fn read_f32_3_async<R>(
    reader: &mut AsyncReadBytes<'_, R>,
) -> Result<[f32; 3], std::io::Error>
where
    R: AsyncReadExt + Unpin,
{
    Ok([
        reader.read_f32::<futures_byteorder::LittleEndian>().await?,
        reader.read_f32::<futures_byteorder::LittleEndian>().await?,
        reader.read_f32::<futures_byteorder::LittleEndian>().await?,
    ])
}

/// Read a null-terminated sequence of bytes and convert it into a `String`.
///
/// The zero byte is consumed.
///
/// ## Panics
/// - If the end of the input is reached before a zero byte is found.
pub async fn read_cstring_async<R>(src: &mut AsyncReadBytes<'_, R>) -> io::Result<QString>
// QStringOwned
where
    R: AsyncReadExt + Unpin,
{
    // TODO: `BufRead` would be better here
    let mut bytes: Vec<u8> = Vec::new();
    loop {
        let next_byte = src.read_u8().await?;
        if next_byte == 0 {
            break;
        } else {
            bytes.push(next_byte);
        }
    }

    Ok(QStr { raw: Cow::owned(bytes) })
}
