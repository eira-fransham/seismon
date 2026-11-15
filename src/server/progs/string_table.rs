use crate::server::progs::{ProgsError, StringId};
use hashbrown::HashMap;
use seismon_utils::QStr;
use std::str;

/// The QuakeC string table: a blob of NUL-terminated strings, addressed by byte offset.
///
/// Every string in `data` (including ones added at runtime by [`StringTable::find_or_insert`]) is
/// NUL-terminated, so any offset into `data` yields a valid string ending at the next NUL.
#[derive(Debug, Clone)]
pub struct StringTable {
    /// Interned string data.
    data: Vec<u8>,

    /// Maps each whole string to the offset it starts at, for [`StringTable::find`].
    ///
    /// Only strings starting at the beginning of the blob or directly after a NUL are indexed;
    /// suffixes of other strings are still valid IDs but cannot be found by name.
    // TODO: We should avoid re-allocating the bytes here. We can probably use a rope-like
    // structure where the internal bytes can be shared.
    index: HashMap<Box<[u8]>, StringId>,
}

impl StringTable {
    pub fn new(mut data: Vec<u8>) -> StringTable {
        if data.last() != Some(&0) {
            data.push(0);
        }

        let mut index = HashMap::new();
        let mut start = 0;
        for s in data[..data.len() - 1].split(|&b| b == 0) {
            // Keep the first occurrence if a string appears more than once.
            index.entry(s.into()).or_insert(StringId(start));
            start += s.len() + 1;
        }

        StringTable { data, index }
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

    /// Returns the ID of the string exactly equal to `target`, if it is in the table.
    pub fn find<S>(&self, target: S) -> Option<StringId>
    where
        S: AsRef<str>,
    {
        self.index.get(target.as_ref().as_bytes()).copied()
    }

    pub fn get(&self, id: StringId) -> Option<QStr<'_>> {
        let rest = self.data.get(id.0..)?;
        let len = rest.iter().position(|&c| c == 0)?;
        Some((&rest[..len]).into())
    }

    fn insert<S>(&mut self, s: S) -> StringId
    where
        S: AsRef<str>,
    {
        let s = s.as_ref().as_bytes();

        assert!(!s.contains(&0));

        let id = StringId(self.data.len());
        self.data.extend_from_slice(s);
        self.data.push(0);
        self.index.insert(s.into(), id);
        id
    }

    pub fn find_or_insert<S>(&mut self, target: S) -> StringId
    where
        S: AsRef<str>,
    {
        match self.find(target.as_ref()) {
            Some(id) => id,
            None => self.insert(target),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &str> + '_ {
        self.data[..self.data.len() - 1]
            .split(|b| *b == 0)
            .filter_map(|bytes| str::from_utf8(bytes).ok())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn table() -> StringTable {
        StringTable::new(b"\0hello\0world\0demon/dland2.wav\0".to_vec())
    }

    #[test]
    fn find_loaded_strings() {
        let t = table();
        assert_eq!(t.find(""), Some(StringId(0)));
        assert_eq!(t.find("hello"), Some(StringId(1)));
        assert_eq!(t.find("world"), Some(StringId(7)));
        assert_eq!(t.find("demon/dland2.wav"), Some(StringId(13)));
        assert_eq!(&*t.get(StringId(13)).unwrap().raw, b"demon/dland2.wav");
    }

    #[test]
    fn find_rejects_prefixes_and_long_needles() {
        let t = table();
        assert_eq!(t.find("hell"), None);
        assert_eq!(t.find("hello world, this is longer than the whole table"), None);
    }

    #[test]
    fn find_or_insert_dedups() {
        let mut t = table();
        let a = t.find_or_insert("new");
        let b = t.find_or_insert("new");
        assert_eq!(a, b);
        assert_eq!(t.find_or_insert("world"), StringId(7));
    }

    #[test]
    fn inserts_are_terminated() {
        let mut t = table();
        let a = t.find_or_insert("foo");
        let b = t.find_or_insert("bar");
        assert_eq!(&*t.get(a).unwrap().raw, b"foo");
        assert_eq!(&*t.get(b).unwrap().raw, b"bar");
        // An ID into the middle of an inserted string stops at its terminator.
        assert_eq!(&*t.get(StringId(a.0 + 1)).unwrap().raw, b"oo");
        assert_eq!(
            t.iter().collect::<Vec<_>>(),
            ["", "hello", "world", "demon/dland2.wav", "foo", "bar"]
        );
    }

    #[test]
    fn unterminated_blob() {
        let t = StringTable::new(b"\0abc".to_vec());
        assert_eq!(t.find("abc"), Some(StringId(1)));
        assert_eq!(&*t.get(StringId(1)).unwrap().raw, b"abc");
        assert!(t.get(StringId(100)).is_none());
    }
}
