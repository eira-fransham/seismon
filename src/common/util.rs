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
    mem::{self, size_of},
    ops::{Deref, Not},
};

use beef::Cow;
use byteorder::{LittleEndian, ReadBytesExt};
use nom::AsBytes;

/// A plain-old-data type.
pub trait Pod: 'static + Copy + Sized + Send + Sync {}
impl<T: 'static + Copy + Sized + Send + Sync> Pod for T {}

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
        QStr {
            raw: self.raw.into_owned().into(),
        }
    }
}

pub type QString = QStr<'static>;

impl Default for QStr<'_> {
    fn default() -> Self {
        Self {
            raw: Cow::borrowed(&[]),
        }
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
        Self {
            raw: Cow::borrowed(value.as_bytes()),
        }
    }
}

impl From<Vec<u8>> for QString {
    fn from(value: Vec<u8>) -> Self {
        Self {
            raw: Cow::owned(value),
        }
    }
}

impl<'a> Deref for QStr<'a> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.raw
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
        QStr {
            raw: Cow::borrowed(&*self.raw),
        }
    }

    pub fn chars(&self) -> impl Iterator<Item = (char, StringColor)> + '_ {
        self.raw.iter().copied().map(|b| {
            (
                (b % 128) as char,
                if b >= 128 {
                    StringColor::Red
                } else {
                    StringColor::Default
                },
            )
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

    Ok(QStr {
        raw: Cow::owned(bytes),
    })
}

/// # Safety
/// Identical to bytemuck, TODO just replace this with bytemuck.
pub unsafe fn any_as_bytes<T>(t: &T) -> &[u8]
where
    T: Pod,
{
    unsafe { std::slice::from_raw_parts((t as *const T) as *const u8, size_of::<T>()) }
}

/// # Safety
/// Identical to bytemuck, TODO just replace this with bytemuck.
pub unsafe fn any_slice_as_bytes<T>(t: &[T]) -> &[u8]
where
    T: Pod,
{
    unsafe { std::slice::from_raw_parts(t.as_ptr() as *const u8, mem::size_of_val(t)) }
}

/// # Safety
/// Identical to bytemuck, TODO just replace this with bytemuck.
pub unsafe fn bytes_as_any<T>(bytes: &[u8]) -> T
where
    T: Pod,
{
    assert_eq!(bytes.len(), size_of::<T>());
    unsafe { std::ptr::read_unaligned(bytes.as_ptr() as *const T) }
}

/// # Safety
/// Identical to bytemuck, TODO just replace this with bytemuck.
pub unsafe fn any_as_u32_slice<T>(t: &T) -> &[u32]
where
    T: Pod,
{
    assert!(size_of::<T>() % size_of::<u32>() == 0);
    unsafe {
        std::slice::from_raw_parts(
            (t as *const T) as *const u32,
            size_of::<T>() / size_of::<u32>(),
        )
    }
}
