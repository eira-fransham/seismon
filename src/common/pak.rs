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

//! Quake PAK archive manipulation.

use std::{
    fs,
    io::{self, Read, Seek, SeekFrom},
    ops::Range,
    path::{Path, PathBuf},
};

use bevy::{
    asset::{
        io::{AssetReader, AssetReaderError, AssetReaderFuture, PathStream, Reader, SliceReader},
        Asset, AssetLoader, LoadContext,
    },
    prelude::*,
    reflect::TypePath,
    utils::{BoxedFuture, ConditionalSendFuture},
};
use byteorder::{LittleEndian, ReadBytesExt};
use futures::AsyncReadExt as _;
use hashbrown::HashMap;
use memmap2::{Mmap, MmapOptions};
use thiserror::Error;

const PAK_MAGIC: [u8; 4] = [b'P', b'A', b'C', b'K'];
const PAK_ENTRY_SIZE: usize = 64;

#[derive(Error, Debug)]
pub enum PakError {
    #[error("I/O error: {0}")]
    Io(#[from] io::Error),
    #[error("Invalid magic number: {0:?}")]
    InvalidMagicNumber([u8; 4]),
    #[error("Invalid file table offset: {0}")]
    InvalidTableOffset(i32),
    #[error("Invalid file table size: {0}")]
    InvalidTableSize(i32),
    #[error("Invalid file offset: {0}")]
    InvalidFileOffset(i32),
    #[error("Invalid file size: {0}")]
    InvalidFileSize(i32),
    #[error("File name too long: {0}")]
    FileNameTooLong(String),
    #[error("Non-UTF-8 file name: {0}")]
    NonUtf8FileName(#[from] std::string::FromUtf8Error),
    #[error("No such file in PAK archive: {0}")]
    NoSuchFile(PathBuf),
}

#[derive(Debug, Clone)]
enum PakEntry {
    // Range in the memmap
    File(Range<usize>),
    // TODO: Maybe we don't need to allocate as many elements?
    Directory(Box<[PathBuf]>),
}

#[derive(Debug)]
enum PakBacking {
    Mmap(Mmap),
    Memory(Box<[u8]>),
}

impl From<Mmap> for PakBacking {
    fn from(value: Mmap) -> Self {
        Self::Mmap(value)
    }
}

impl From<Box<[u8]>> for PakBacking {
    fn from(value: Box<[u8]>) -> Self {
        Self::Memory(value)
    }
}

impl AsRef<[u8]> for PakBacking {
    fn as_ref(&self) -> &[u8] {
        match self {
            Self::Mmap(mmap) => mmap.as_ref(),
            Self::Memory(mem) => mem.as_ref(),
        }
    }
}

/// An open Pak archive.
#[derive(Asset, TypePath, Debug)]
pub struct Pak {
    memory: PakBacking,
    entries: HashMap<PathBuf, PakEntry>,
}

#[derive(Default)]
struct PakLoader;

impl AssetLoader for PakLoader {
    type Asset = Pak;
    type Settings = ();
    type Error = PakError;

    fn load(
        &self,
        reader: &mut dyn Reader,
        _settings: &(),
        _load_context: &mut LoadContext,
    ) -> impl ConditionalSendFuture<Output = Result<Self::Asset, Self::Error>> {
        async move {
            let mut data = Vec::new();

            reader.read_to_end(&mut data).await?;

            Pak::read(data.into_boxed_slice())
        }
    }

    fn extensions(&self) -> &[&str] {
        &["pak", "PAK"]
    }
}

impl AssetReader for Pak {
    async fn read<'a>(&'a self, path: &'a Path) -> Result<impl Reader + 'a, AssetReaderError> {
        match self.entries.get(path) {
            Some(PakEntry::File(range)) => {
                Ok(SliceReader::new(&self.memory.as_ref()[range.clone()]))
            }
            None | Some(PakEntry::Directory(..)) => {
                Err(AssetReaderError::NotFound(path.to_path_buf()))
            }
        }
    }

    async fn read_meta<'a>(&'a self, path: &'a Path) -> Result<impl Reader + 'a, AssetReaderError> {
        self.read(path).await
    }

    async fn read_directory<'a>(
        &'a self,
        path: &'a Path,
    ) -> Result<Box<PathStream>, AssetReaderError> {
        let entry = self
            .entries
            .get(path)
            .ok_or_else(|| AssetReaderError::NotFound(path.to_owned()))?;
        let dir_entries = if let PakEntry::Directory(entries) = entry {
            Some(entries)
        } else {
            None
        };
        let iter = dir_entries
            .into_iter()
            .map(AsRef::as_ref)
            .flatten()
            .cloned()
            .collect::<Vec<_>>()
            .into_iter();

        Ok(Box::new(futures::stream::iter(iter)))
    }

    async fn is_directory<'a>(&'a self, path: &'a Path) -> Result<bool, AssetReaderError> {
        match self.entries.get(path) {
            Some(PakEntry::Directory(..)) => Ok(true),
            Some(PakEntry::File(..)) => Ok(false),
            None => Err(AssetReaderError::NotFound(path.to_path_buf())),
        }
    }
}

impl Pak {
    // TODO: rename to from_path or similar
    pub fn new<P>(path: P) -> Result<Pak, PakError>
    where
        P: AsRef<Path>,
    {
        debug!("Opening {}", path.as_ref().to_str().unwrap());

        Self::read(unsafe { MmapOptions::new().map(&fs::File::open(path)?)? })
    }

    fn read<B: Into<PakBacking>>(bytes: B) -> Result<Self, PakError> {
        let bytes = bytes.into();
        let mut reader = io::Cursor::new(bytes.as_ref());

        let mut magic = [0u8; 4];
        reader.read_exact(&mut magic)?;

        if magic != PAK_MAGIC {
            Err(PakError::InvalidMagicNumber(magic))?;
        }

        // Locate the file table
        let table_offset = match reader.read_i32::<LittleEndian>()? {
            o if o <= 0 => Err(PakError::InvalidTableOffset(o))?,
            o => o as u32,
        };

        let table_size = match reader.read_i32::<LittleEndian>()? {
            s if s <= 0 || s as usize % PAK_ENTRY_SIZE != 0 => Err(PakError::InvalidTableSize(s))?,
            s => s as u32,
        };

        let mut map = HashMap::default();

        for i in 0..(table_size as usize / PAK_ENTRY_SIZE) {
            let entry_offset = table_offset as u64 + (i * PAK_ENTRY_SIZE) as u64;
            reader.seek(SeekFrom::Start(entry_offset))?;

            let mut path_bytes = [0u8; 56];
            reader.read_exact(&mut path_bytes)?;

            let file_offset = match reader.read_i32::<LittleEndian>()? {
                o if o <= 0 => Err(PakError::InvalidFileOffset(o))?,
                o => o as u32,
            };

            let file_size = match reader.read_i32::<LittleEndian>()? {
                s if s <= 0 => Err(PakError::InvalidFileSize(s))?,
                s => s as u32,
            };

            let last = path_bytes
                .iter()
                .position(|b| *b == 0)
                .ok_or(PakError::FileNameTooLong(
                    String::from_utf8_lossy(&path_bytes).into_owned(),
                ))?;
            let path = String::from_utf8(path_bytes[0..last].to_vec())?;

            map.insert(
                PathBuf::from(path),
                PakEntry::File(file_offset as usize..(file_offset + file_size) as usize),
            );
        }

        let keys = map.keys().cloned().collect::<Box<[_]>>();

        for path in &keys[..] {
            let mut path: &Path = path.as_ref();
            while let Some(parent) = path.parent() {
                map.insert(parent.to_owned(), PakEntry::Directory(Default::default()));
                path = parent;
            }
        }

        let keys = map.keys().cloned().collect::<Box<[_]>>();

        for (path, entry) in &mut map {
            if let PakEntry::Directory(inner) = entry {
                *inner = keys
                    .iter()
                    .filter(|k| path != *k && k.starts_with(path))
                    .map(|p| p.to_path_buf())
                    .collect();
            }
        }

        map.shrink_to_fit();

        Ok(Pak {
            memory: bytes,
            entries: map,
        })
    }

    /// Opens a file in the file tree for reading.
    ///
    /// # Examples
    /// ```no_run
    /// # extern crate richter;
    /// use richter::common::pak::Pak;
    ///
    /// # fn main() {
    /// let mut pak = Pak::new("pak0.pak").unwrap();
    /// let progs_dat = pak.open("progs.dat").unwrap();
    /// # }
    /// ```
    pub fn open<S>(&self, path: S) -> Result<&[u8], PakError>
    where
        S: AsRef<Path>,
    {
        let path = path.as_ref();
        self.entries
            .get(path)
            .and_then(|s| {
                if let PakEntry::File(range) = s {
                    Some(&self.memory.as_ref()[range.clone()])
                } else {
                    None
                }
            })
            .ok_or(PakError::NoSuchFile(path.to_owned()))
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Path, &[u8])> + '_ {
        self.entries.iter().filter_map(move |(path, e)| {
            if let PakEntry::File(range) = e {
                Some((path.as_ref(), &self.memory.as_ref()[range.clone()]))
            } else {
                None
            }
        })
    }
}
