use itertools::Itertools as _;
use std::{
    borrow::Cow,
    fmt::{self, Write as FmtWrite},
    hash::Hash,
    io::{self, BufWriter, Write},
    iter::FromIterator,
    mem,
    slice::SliceIndex,
};
use unicode_width::UnicodeWidthChar as _;

/// A modification performed on a `Buffer`. These are used for the purpose of undo/redo.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Action<'a> {
    /// Insert some text before the given location.
    Insert {
        /// The location to insert the text.
        start: usize,
        /// The text to insert (as an owned vector of UTF-32 chars).
        text: Cow<'a, [char]>,
    },
    /// Remove the given text at the given location.
    Remove {
        /// The location to remove the text.
        start: usize,
        /// The text to remove (as an owned vector of UTF-32 chars).
        text: Cow<'a, [char]>,
    },
    /// Start an "undo group", that should be undone together in a single operation.
    StartUndoGroup,
    /// End an "undo group".
    EndUndoGroup,
}

impl Action<'_> {
    fn into_owned(self) -> Action<'static> {
        match self {
            Action::Insert { start, text } => {
                Action::Insert { start, text: text.into_owned().into() }
            }
            Action::Remove { start, text } => {
                Action::Remove { start, text: text.into_owned().into() }
            }

            Action::StartUndoGroup => Action::StartUndoGroup,
            Action::EndUndoGroup => Action::EndUndoGroup,
        }
    }

    fn invert(&self) -> Action<'_> {
        match self {
            Action::Insert { start, text } => {
                Action::Remove { start: *start, text: text.as_ref().into() }
            }
            Action::Remove { start, text } => {
                Action::Insert { start: *start, text: text.as_ref().into() }
            }

            Action::StartUndoGroup => Action::EndUndoGroup,
            Action::EndUndoGroup => Action::StartUndoGroup,
        }
    }
}

impl Action<'_> {
    /// Apply this action to the given buffer.
    pub fn execute(&self, data: &mut Vec<char>) {
        match self {
            Action::Insert { start, text } => {
                let _ = data.splice(*start..*start, text.iter().copied());
            }
            Action::Remove { start, text } => {
                let _ = data.splice(*start..*start + text.len(), []);
            }
            Action::StartUndoGroup | Action::EndUndoGroup => {}
        }
    }
}

/// A buffer for text in the line editor.
///
/// It keeps track of each action performed on it for use with undo/redo. Unless otherwise
/// mentioned, all methods that manipulate the inner characters can be undone or redone using
/// `undo`/`redo`.
#[derive(Clone, Debug)]
pub struct Buffer {
    data: Vec<char>,
    actions: Actions<Action<'static>>,
}

impl Hash for Buffer {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.data.hash(state)
    }
}

impl PartialEq for Buffer {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}
impl Eq for Buffer {}

impl From<Buffer> for String {
    fn from(buf: Buffer) -> Self {
        String::from_iter(buf.data)
    }
}

impl From<String> for Buffer {
    fn from(s: String) -> Self {
        Buffer::from_iter(s.chars())
    }
}

impl<'a> From<&'a str> for Buffer {
    fn from(s: &'a str) -> Self {
        Buffer::from_iter(s.chars())
    }
}

impl fmt::Display for Buffer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for &c in &self.data {
            f.write_char(c)?;
        }
        Ok(())
    }
}

impl FromIterator<char> for Buffer {
    fn from_iter<T: IntoIterator<Item = char>>(t: T) -> Self {
        Buffer { data: t.into_iter().collect(), actions: Default::default() }
    }
}

impl Default for Buffer {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Debug)]
struct Actions<T> {
    position: usize,
    inner: Vec<T>,
}

impl<T> Default for Actions<T> {
    fn default() -> Self {
        Self { position: 0, inner: vec![] }
    }
}

impl<T> Actions<T> {
    fn clear(&mut self) {
        self.position = 0;
        self.inner.clear();
    }

    fn undo_is_empty(&self) -> bool {
        self.position == 0
    }

    fn redo_is_empty(&self) -> bool {
        self.position == self.inner.len()
    }

    fn push(&mut self, item: T) {
        self.inner.truncate(self.position);
        self.inner.push(item);
        self.position += 1;
    }

    fn undo_iter(&mut self) -> impl Iterator<Item = &T> {
        struct ActionsUndoIter<'a, T> {
            position: &'a mut usize,
            iter: std::slice::Iter<'a, T>,
        }

        impl<'a, T> Iterator for ActionsUndoIter<'a, T> {
            type Item = &'a T;

            fn next(&mut self) -> Option<Self::Item> {
                self.iter.next()
            }
        }

        impl<'a, T> Drop for ActionsUndoIter<'a, T> {
            fn drop(&mut self) {
                *self.position = self.iter.as_slice().len();
            }
        }

        let original_pos = self.position;
        ActionsUndoIter { position: &mut self.position, iter: self.inner[..original_pos].iter() }
    }

    fn redo_iter(&mut self) -> impl Iterator<Item = &T> {
        struct ActionsRedoIter<'a, T> {
            position: &'a mut usize,
            original_len: usize,
            iter: std::slice::Iter<'a, T>,
        }

        impl<'a, T> Iterator for ActionsRedoIter<'a, T> {
            type Item = &'a T;

            fn next(&mut self) -> Option<Self::Item> {
                self.iter.next()
            }
        }

        impl<'a, T> Drop for ActionsRedoIter<'a, T> {
            fn drop(&mut self) {
                *self.position = self.original_len - self.iter.as_slice().len();
            }
        }

        let original_pos = self.position;
        let original_len = self.inner.len();
        ActionsRedoIter {
            position: &mut self.position,
            original_len,
            iter: self.inner.get(original_pos..).unwrap_or(&[]).iter(),
        }
    }
}

impl Buffer {
    /// Create a new empty [`Buffer`].
    pub fn new() -> Self {
        Buffer { data: Vec::new(), actions: Default::default() }
    }

    #[cfg(test)]
    pub(crate) fn test_data_mut(&mut self) -> &mut Vec<char> {
        &mut self.data
    }

    /// Clear the actions buffer.
    pub fn clear_actions(&mut self) {
        self.actions.clear();
    }

    /// Start a group of actions that should be undone as a single operation.
    pub fn start_undo_group(&mut self) {
        self.actions.push(Action::StartUndoGroup);
    }

    /// End an undo group (see [`Self::start_undo_group`]).
    pub fn end_undo_group(&mut self) {
        self.actions.push(Action::EndUndoGroup);
    }

    /// Undo a single operation, or an undo group.
    pub fn undo(&mut self) -> bool {
        use Action::*;

        let did = !self.actions.undo_is_empty();
        let mut group_nest = 0;
        let mut group_count = 0;
        for act in self.actions.undo_iter() {
            act.invert().execute(&mut self.data);
            match act {
                EndUndoGroup => {
                    group_nest += 1;
                    group_count = 0;
                }
                StartUndoGroup => group_nest -= 1,
                // count the actions in this group so we can ignore empty groups below
                _ => group_count += 1,
            }

            // if we aren't in a group, and the last group wasn't empty
            if group_nest == 0 && group_count > 0 {
                break;
            }
        }
        did
    }

    /// Redo a single operation, or an undo group.
    pub fn redo(&mut self) -> bool {
        use Action::*;

        let did = !self.actions.redo_is_empty();
        let mut group_nest = 0;
        let mut group_count = 0;
        for act in self.actions.redo_iter() {
            act.execute(&mut self.data);
            match act {
                StartUndoGroup => {
                    group_nest += 1;
                    group_count = 0;
                }
                EndUndoGroup => group_nest -= 1,
                // count the actions in this group so we can ignore empty groups below
                _ => group_count += 1,
            }

            // if we aren't in a group, and the last group wasn't empty
            if group_nest == 0 && group_count > 0 {
                break;
            }
        }
        did
    }

    /// Undo all actions, reverting the buffer to the earliest state that we have information for.
    pub fn revert(&mut self) -> bool {
        if self.actions.undo_is_empty() {
            return false;
        }

        for act in self.actions.undo_iter() {
            act.invert().execute(&mut self.data);
        }

        true
    }

    fn push_action(&mut self, act: Action<'_>) {
        self.actions.push(act.into_owned());
    }

    /// The final WORD - split by spaces.
    pub fn last_arg(&self) -> Option<&[char]> {
        self.data.split(|&c| c == ' ').rfind(|s| !s.is_empty())
    }

    /// The number of characters currently in the buffer.
    pub fn num_chars(&self) -> usize {
        self.data.len()
    }

    /// The number of bytes this buffer will take up when encoded to UTF-8.
    pub fn num_bytes(&self) -> usize {
        self.data.iter().map(|c| c.len_utf8()).sum()
    }

    /// The character immediately before `cursor`.
    pub fn char_before(&self, cursor: usize) -> Option<char> {
        if cursor == 0 { None } else { self.data.get(cursor - 1).cloned() }
    }

    /// The character directly at `cursor`.
    pub fn char_after(&self, cursor: usize) -> Option<char> {
        self.data.get(cursor).cloned()
    }

    /// Returns the number of characters removed.
    pub fn remove(&mut self, start: usize, end: usize) -> usize {
        let s = self.data.drain(start..end).collect::<Vec<_>>();
        let num_removed = s.len();
        self.push_action(Action::Remove { start, text: s.into() });
        num_removed
    }

    /// Insert a set of characters at the given index.
    pub fn insert<'a, T: Into<Cow<'a, [char]>>>(&mut self, start: usize, text: T) {
        let text = text.into().into_owned();
        let act = Action::Insert { start, text: text.into() };
        act.execute(&mut self.data);
        self.push_action(act);
    }

    /// If `other` has more characters than `self`, extend `self` with the characters from `other`
    /// in the range `self.len()..`.
    pub fn union(&mut self, other: &Buffer) {
        let start = self.data.len();
        self.insert(start, &other.data[start..])
    }

    /// Replace the contents with the contents of `other`.
    pub fn replace(&mut self, other: &Buffer) {
        let data_len = self.data.len();
        self.remove(0, data_len);
        self.insert(0, &other.data[0..])
    }

    /// Iterate over a range of characters in the buffer.
    pub fn range<R>(&self, range: R) -> impl Iterator<Item = char>
    where
        R: SliceIndex<[char], Output = [char]>,
    {
        self.data[range].iter().cloned()
    }

    /// Get the unicode width of all lines in the buffer.
    pub fn width(&self) -> Vec<usize> {
        self.range_width(..)
    }

    /// Get the unicode width of all lines in the given range.
    pub fn range_width<R>(&self, range: R) -> Vec<usize>
    where
        R: SliceIndex<[char], Output = [char]>,
    {
        // TODO: We don't really need to collect here.
        self.range(range)
            .chunk_by(|c| *c == '\n')
            .into_iter()
            .filter_map(
                |(is_control, chars)| {
                    if is_control { None } else { Some(chars.filter_map(|c| c.width()).sum()) }
                },
            )
            .collect()
    }

    /// Get the lines in this buffer, as an iterator of slices of [`char`].
    pub fn lines(&self) -> impl Iterator<Item = &[char]> {
        self.data.split(|&c| c == '\n')
    }

    /// Get the chars in this buffer.
    pub fn chars(&self) -> impl DoubleEndedIterator<Item = char> + ExactSizeIterator {
        self.data.iter().copied()
    }

    /// Remove characters from the end to make the buffer `num` chars in length.
    pub fn truncate(&mut self, num: usize) {
        let end = self.data.len();
        self.remove(num, end);
    }

    /// Print the buffer to `out`.
    pub fn print<W>(&self, out: &mut W) -> io::Result<()>
    where
        W: Write,
    {
        let mut writer = BufWriter::new(out);

        let mut bytes = [0; mem::size_of::<char>()];

        for char in self.chars() {
            let s = char.encode_utf8(&mut bytes);
            writer.write_all(s.as_bytes())?;
        }

        Ok(())
    }

    /// Convert this buffer to a vector of UTF8-encoded bytes.
    pub fn to_bytes(&self) -> Vec<u8> {
        // NOTE: not particularly efficient. Could make a proper byte iterator with minimal
        // allocations if performance becomes an issue.
        self.to_string().into_bytes()
    }

    /// Takes other buffer, measures its length and prints this buffer from the point where
    /// the other stopped.
    /// Used to implement autosuggestions.
    pub fn print_rest<W>(&self, out: &mut W, after: usize) -> io::Result<usize>
    where
        W: Write,
    {
        let string: String = self.data.iter().skip(after).cloned().collect();
        out.write_all(string.as_bytes())?;

        Ok(string.len())
    }

    /// Check if the other buffer starts with the same content as this one.
    /// Used to implement autosuggestions.
    pub fn starts_with(&self, other: &Buffer) -> bool {
        let other_len = other.data.len();
        let self_len = self.data.len();
        if !other.data.is_empty() && self_len != other_len {
            let match_let =
                self.data.iter().zip(&other.data).take_while(|&(s, o)| *s == *o).count();
            match_let == other_len
        } else {
            false
        }
    }

    /// Check if the buffer contains pattern.
    /// Used to implement history search.
    pub fn contains(&self, pattern: &Buffer) -> bool {
        let search_term: &[char] = &pattern.data;
        if search_term.is_empty() {
            return false;
        }
        self.data.windows(search_term.len()).any(|window| window == search_term)
    }

    /// Return true if the buffer is empty.
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_insert() {
        let mut buf = Buffer::new();
        buf.insert(0, &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        assert_eq!(String::from(buf), "abcdefg");
    }

    #[test]
    fn test_truncate_empty() {
        let mut buf = Buffer::new();
        buf.truncate(0);
        assert_eq!(String::from(buf), "");
    }

    #[test]
    fn test_truncate_all() {
        let mut buf = Buffer::new();
        buf.insert(0, &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        buf.truncate(0);
        assert_eq!(String::from(buf), "");
    }

    #[test]
    fn test_truncate_end() {
        let mut buf = Buffer::new();
        buf.insert(0, &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let end = buf.num_chars();
        buf.truncate(end);
        assert_eq!(String::from(buf), "abcdefg");
    }

    #[test]
    fn test_truncate_part() {
        let mut buf = Buffer::new();
        buf.insert(0, &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        buf.truncate(3);
        assert_eq!(String::from(buf), "abc");
    }

    #[test]
    fn test_truncate_empty_undo() {
        let mut buf = Buffer::new();
        buf.truncate(0);
        buf.undo();
        assert_eq!(String::from(buf), "");
    }

    #[test]
    fn test_truncate_all_then_undo() {
        let mut buf = Buffer::new();
        buf.insert(0, &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        buf.truncate(0);
        buf.undo();
        assert_eq!(String::from(buf), "abcdefg");
    }

    #[test]
    fn test_truncate_end_then_undo() {
        let mut buf = Buffer::new();
        buf.insert(0, &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let end = buf.num_chars();
        buf.truncate(end);
        buf.undo();
        assert_eq!(String::from(buf), "abcdefg");
    }

    #[test]
    fn test_truncate_part_then_undo() {
        let mut buf = Buffer::new();
        buf.insert(0, &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        buf.truncate(3);
        buf.undo();
        assert_eq!(String::from(buf), "abcdefg");
    }

    #[test]
    fn test_undo_group() {
        let mut buf = Buffer::new();
        buf.insert(0, &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        buf.start_undo_group();
        buf.remove(0, 1);
        buf.remove(0, 1);
        buf.remove(0, 1);
        buf.end_undo_group();
        assert!(buf.undo());
        assert_eq!(String::from(buf), "abcdefg");
    }

    #[test]
    fn test_redo_group() {
        let mut buf = Buffer::new();
        buf.insert(0, &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        buf.start_undo_group();
        buf.remove(0, 1);
        buf.remove(0, 1);
        buf.remove(0, 1);
        buf.end_undo_group();
        assert!(buf.undo());
        assert!(buf.redo());
        assert_eq!(String::from(buf), "defg");
    }

    #[test]
    fn test_nested_undo_group() {
        let mut buf = Buffer::new();
        buf.insert(0, &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        buf.start_undo_group();
        buf.remove(0, 1);
        buf.start_undo_group();
        buf.remove(0, 1);
        buf.end_undo_group();
        buf.remove(0, 1);
        buf.end_undo_group();
        assert!(buf.undo());
        assert_eq!(String::from(buf), "abcdefg");
    }

    #[test]
    fn test_nested_redo_group() {
        let mut buf = Buffer::new();
        buf.insert(0, &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        buf.start_undo_group();
        buf.remove(0, 1);
        buf.start_undo_group();
        buf.remove(0, 1);
        buf.end_undo_group();
        buf.remove(0, 1);
        buf.end_undo_group();
        assert!(buf.undo());
        assert!(buf.redo());
        assert_eq!(String::from(buf), "defg");
    }

    #[test]
    fn test_starts_with() {
        let mut buf = Buffer::new();
        buf.insert(0, &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let mut buf2 = Buffer::new();
        buf2.insert(0, &['a', 'b', 'c']);
        assert!(buf.starts_with(&buf2));
    }

    #[test]
    fn test_does_not_start_with() {
        let mut buf = Buffer::new();
        buf.insert(0, &['a', 'b', 'c']);
        let mut buf2 = Buffer::new();
        buf2.insert(0, &['a', 'b', 'c']);
        assert!(!buf.starts_with(&buf2));
    }

    #[test]
    fn test_is_not_match2() {
        let mut buf = Buffer::new();
        buf.insert(0, &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let mut buf2 = Buffer::new();
        buf2.insert(0, &['x', 'y', 'z']);
        assert!(!buf.starts_with(&buf2));
    }

    #[test]
    fn test_contains() {
        let mut buf = Buffer::new();
        buf.insert(0, &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let mut buf2 = Buffer::new();
        buf2.insert(0, &['a', 'b', 'c']);
        assert!(buf.contains(&buf2));
        let mut buf2 = Buffer::new();
        buf2.insert(0, &['c', 'd', 'e']);
        assert!(buf.contains(&buf2));
        let mut buf2 = Buffer::new();
        buf2.insert(0, &['e', 'f', 'g']);
        assert!(buf.contains(&buf2));
    }

    #[test]
    fn test_does_not_contain() {
        let mut buf = Buffer::new();
        buf.insert(0, &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let mut buf2 = Buffer::new();
        buf2.insert(0, &['x', 'b', 'c']);
        assert!(!buf.contains(&buf2));
        let mut buf2 = Buffer::new();
        buf2.insert(0, &['a', 'b', 'd']);
        assert!(!buf.contains(&buf2));
    }

    #[test]
    fn test_print_rest() {
        let mut buf = Buffer::new();
        buf.insert(0, &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let mut buf2 = Buffer::new();
        buf2.insert(0, &['a', 'b', 'c']);
        let mut out: Vec<u8> = vec![];
        buf.print_rest(&mut out, buf2.data.len()).unwrap();
        assert_eq!(out.len(), 4);
    }
}
