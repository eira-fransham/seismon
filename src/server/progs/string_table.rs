use crate::server::progs::{ProgsError, StringId};
use dashmap::DashMap;
use seismon_utils::QStr;
use std::str;

#[derive(Debug)]
pub struct StringTable {
    /// Interned string data.
    data: Vec<u8>,

    /// Caches string lengths for faster lookup.
    lengths: DashMap<StringId, usize>,
}

impl StringTable {
    pub fn new(data: Vec<u8>) -> StringTable {
        StringTable { data, lengths: Default::default() }
    }

    pub fn id_from_i32(&self, value: i32) -> Result<StringId, ProgsError> {
        if value < 0 {
            return Err(ProgsError::with_msg("id < 0"));
        }

        let id = StringId(value as usize);

        if id.0 < self.data.len() {
            Ok(id)
        } else {
            Err(ProgsError::with_msg(format!("no string with ID {value}")))
        }
    }

    pub fn find<S>(&self, target: S) -> Option<StringId>
    where S: AsRef<str> {
        let target = target.as_ref().as_bytes();
        for (ofs, _) in target.iter().enumerate() {
            let sub = &self.data[ofs..];
            if !sub.starts_with(target) {
                continue;
            }

            // Make sure the string is NUL-terminated. Otherwise, this could
            // erroneously return the StringId of a ImString whose first
            // `target.len()` bytes were equal to `target`, but which had
            // additional bytes.
            if sub.get(target.len()) != Some(&0) {
                continue;
            }

            return Some(StringId(ofs));
        }

        None
    }

    pub fn get(&self, id: StringId) -> Option<QStr<'_>> {
        let start = id.0;

        if start >= self.data.len() {
            return None;
        }

        if let Some(len) = self.lengths.get(&id) {
            let end = start + *len;
            return Some((&self.data[start..end]).into());
        }

        match self.data[start..].iter().take(1024 * 1024).enumerate().find(|&(_i, c)| *c == 0) {
            Some((len, _)) => {
                self.lengths.insert(id, len);
                let end = start + len;
                Some((&self.data[start..end]).into())
            }
            None => panic!("string data not NUL-terminated!"),
        }
    }

    fn insert<S>(&mut self, s: S) -> StringId
    where S: AsRef<str> {
        let s = s.as_ref();

        assert!(!s.contains('\0'));

        let id = StringId(self.data.len());
        self.data.extend_from_slice(s.as_bytes());
        self.lengths.insert(id, s.len());
        id
    }

    pub fn find_or_insert<S>(&mut self, target: S) -> StringId
    where S: AsRef<str> {
        match self.find(target.as_ref()) {
            Some(id) => id,
            None => self.insert(target),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &str> + '_ {
        // TODO: Make this work properly with the refcell - since the inner data
        //       is cheaply clonable this should be relatively easy.
        self.data.split(|b| *b == 0).filter_map(|bytes| str::from_utf8(bytes).ok())
    }
}
