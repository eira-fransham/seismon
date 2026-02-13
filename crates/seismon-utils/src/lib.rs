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
    ops::{Deref, Not},
    time::Duration,
};

use beef::Cow;
use byteorder::{LittleEndian, ReadBytesExt};
use futures::AsyncReadExt;
use futures_byteorder::AsyncReadBytes;
use nom::AsBytes;

pub mod model;

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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct QStr<'a> {
    pub raw: Cow<'a, [u8]>,
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
where R: ReadBytesExt {
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
where R: BufRead {
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
where R: AsyncReadExt + Unpin {
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
where R: AsyncReadExt + Unpin {
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
