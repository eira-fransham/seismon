use std::ops::Range;

use arrayvec::{ArrayString, ArrayVec};
use hashbrown::HashMap;
use snafu::Snafu;

/// Maximum permitted length of a precache path.
const MAX_PRECACHE_PATH: usize = 64;

const MAX_PRECACHE_ENTRIES: usize = 256;

#[derive(Snafu, Debug)]
pub enum PrecacheError {
    #[snafu(display("precache name (\"{name}\") too long: max length is {MAX_PRECACHE_PATH}"))]
    NameTooLong { name: String },
    #[snafu(display(
        "cannot precache \"{name}\": precache is full ({MAX_PRECACHE_ENTRIES} entries)"
    ))]
    Full { name: String },
}

/// A list of resources to be loaded before entering the game.
///
/// This is used by the server to inform clients which resources (sounds and
/// models) they should load before joining. It also serves as the canonical
/// mapping of resource IDs for a given level.
// TODO: ideally, this is parameterized by the maximum number of entries, but
// it's not currently possible to do { MAX_PRECACHE_PATH * N } where N is a
// const generic parameter. In practice both models and sounds have a maximum
// value of 256.
#[derive(Debug, Default)]
pub struct Precache {
    str_data: ArrayString<{ MAX_PRECACHE_PATH * MAX_PRECACHE_ENTRIES }>,
    items: ArrayVec<Range<usize>, MAX_PRECACHE_ENTRIES>,
    name_to_index: HashMap<String, usize>,
}

/// Offset the index in order to allow `0` to represent null.
const OFFSET: usize = 1;

impl Precache {
    /// Creates a new empty `Precache`.
    pub fn new() -> Precache {
        Self::default()
    }

    /// Retrieves an item from the precache if the item exists.
    ///
    /// `index` is 1-based, as returned by [`Precache::find`]: index 0 means "none".
    pub fn get(&self, index: usize) -> Option<&str> {
        let range = index.checked_sub(OFFSET).and_then(|i| self.items.get(i))?;
        Some(&self.str_data[range.clone()])
    }

    /// Returns the index of the target value if it exists.
    pub fn find<S>(&self, target: S) -> Option<usize>
    where
        S: AsRef<str>,
    {
        self.name_to_index.get(target.as_ref()).copied()
    }

    /// Adds an item to the precache.
    ///
    /// If the item already exists in the precache, this has no effect.
    pub fn precache<S>(&mut self, item: S) -> Result<(), PrecacheError>
    where
        S: AsRef<str>,
    {
        let item = item.as_ref();

        if item.is_empty() || item == "*0" {
            return Ok(());
        }

        if item.len() > MAX_PRECACHE_PATH {
            return Err(PrecacheError::NameTooLong { name: item.to_owned() });
        }

        if self.find(item).is_some() {
            // Already precached.
            return Ok(());
        }

        if self.items.is_full() {
            return Err(PrecacheError::Full { name: item.to_owned() });
        }

        let start = self.str_data.len();
        self.str_data.push_str(item);
        let end = self.str_data.len();

        let index = self.items.len();

        self.items.push(start..end);
        self.name_to_index.insert(item.to_owned(), index + OFFSET);

        Ok(())
    }

    /// Returns an iterator over the values in the precache.
    pub fn iter(&self) -> impl Iterator<Item = &str> {
        self.items.iter().cloned().map(move |range| &self.str_data[range])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_precache_empty() {
        let p = Precache::new();
        assert_eq!(None, p.get(0));
        assert_eq!(None, p.get(1));
    }

    #[test]
    fn test_precache_one() {
        let mut p = Precache::new();

        p.precache("hello").unwrap();
        assert_eq!(None, p.get(0));
        assert_eq!(Some("hello"), p.get(1));
        assert_eq!(Some(1), p.find("hello"));
        assert_eq!(None, p.get(2));
    }

    #[test]
    fn test_precache_several() {
        let mut p = Precache::new();

        let items = &["Quake", "is", "a", "1996", "first-person", "shooter"];

        for item in items {
            p.precache(item).unwrap();
        }

        // Pick an element in the middle
        assert_eq!(Some("first-person"), p.get(5));
        // The last element is reachable.
        assert_eq!(Some("shooter"), p.get(items.len()));
        assert_eq!(None, p.get(items.len() + 1));

        // `find` and `get` agree on indices.
        for &item in items {
            assert_eq!(Some(item), p.get(p.find(item).unwrap()));
        }

        // Check all the elements
        for (precached, &original) in p.iter().zip(items.iter()) {
            assert_eq!(precached, original);
        }
    }

    #[test]
    fn test_precache_errors() {
        let mut p = Precache::new();

        assert!(matches!(
            p.precache("x".repeat(MAX_PRECACHE_PATH + 1)),
            Err(PrecacheError::NameTooLong { .. })
        ));

        for i in 0..MAX_PRECACHE_ENTRIES {
            p.precache(format!("{i}")).unwrap();
        }

        // Even if the precache is full, we should be able to call `precache` for
        // something that's already been precached.
        p.precache("0").unwrap();
        assert!(matches!(p.precache("one-too-many"), Err(PrecacheError::Full { .. })));
    }
}
