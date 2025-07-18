use std::fmt::{self, Write as FmtWrite};
use std::io::{self, Write};
use std::iter::FromIterator;
use unicode_width::UnicodeWidthStr;

/// A modification performed on a `Buffer`. These are used for the purpose of undo/redo.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Action {
    Insert { start: usize, text: Vec<char> },
    Remove { start: usize, text: Vec<char> },
    StartGroup,
    EndGroup,
}

impl Action {
    pub fn do_on(&self, buf: &mut Buffer) {
        match *self {
            Action::Insert { start, ref text } => buf.insert_raw(start, &text[..]),
            Action::Remove { start, ref text } => {
                buf.remove_raw(start, start + text.len());
            }
            Action::StartGroup | Action::EndGroup => {}
        }
    }

    pub fn undo(&self, buf: &mut Buffer) {
        match *self {
            Action::Insert { start, ref text } => {
                buf.remove_raw(start, start + text.len());
            }
            Action::Remove { start, ref text } => buf.insert_raw(start, &text[..]),
            Action::StartGroup | Action::EndGroup => {}
        }
    }
}

/// A buffer for text in the line editor.
///
/// It keeps track of each action performed on it for use with undo/redo.
#[derive(Clone, Hash, Debug)]
pub struct Buffer {
    data: Vec<char>,
    actions: Vec<Action>,
    undone_actions: Vec<Action>,
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
        Buffer {
            data: t.into_iter().collect(),
            actions: Vec::new(),
            undone_actions: Vec::new(),
        }
    }
}

impl Default for Buffer {
    fn default() -> Self {
        Self::new()
    }
}

impl Buffer {
    pub fn new() -> Self {
        Buffer {
            data: Vec::new(),
            actions: Vec::new(),
            undone_actions: Vec::new(),
        }
    }

    pub fn clear_actions(&mut self) {
        self.actions.clear();
        self.undone_actions.clear();
    }

    pub fn start_undo_group(&mut self) {
        self.actions.push(Action::StartGroup);
    }

    pub fn end_undo_group(&mut self) {
        self.actions.push(Action::EndGroup);
    }

    pub fn undo(&mut self) -> bool {
        use Action::*;

        let did = !self.actions.is_empty();
        let mut group_nest = 0;
        let mut group_count = 0;
        while let Some(act) = self.actions.pop() {
            act.undo(self);
            self.undone_actions.push(act.clone());
            match act {
                EndGroup => {
                    group_nest += 1;
                    group_count = 0;
                }
                StartGroup => group_nest -= 1,
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

    pub fn redo(&mut self) -> bool {
        use Action::*;

        let did = !self.undone_actions.is_empty();
        let mut group_nest = 0;
        let mut group_count = 0;
        while let Some(act) = self.undone_actions.pop() {
            act.do_on(self);
            self.actions.push(act.clone());
            match act {
                StartGroup => {
                    group_nest += 1;
                    group_count = 0;
                }
                EndGroup => group_nest -= 1,
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

    pub fn revert(&mut self) -> bool {
        if self.actions.is_empty() {
            return false;
        }

        while self.undo() {}
        true
    }

    fn push_action(&mut self, act: Action) {
        self.actions.push(act);
        self.undone_actions.clear();
    }

    pub fn last_arg(&self) -> Option<&[char]> {
        self.data
            .split(|&c| c == ' ')
            .filter(|s| !s.is_empty())
            .last()
    }

    pub fn num_chars(&self) -> usize {
        self.data.len()
    }

    pub fn num_bytes(&self) -> usize {
        let s: String = self.clone().into();
        s.len()
    }

    pub fn char_before(&self, cursor: usize) -> Option<char> {
        if cursor == 0 {
            None
        } else {
            self.data.get(cursor - 1).cloned()
        }
    }

    pub fn char_after(&self, cursor: usize) -> Option<char> {
        self.data.get(cursor).cloned()
    }

    /// Returns the number of characters removed.
    pub fn remove(&mut self, start: usize, end: usize) -> usize {
        let s = self.remove_raw(start, end);
        let num_removed = s.len();
        self.push_action(Action::Remove { start, text: s });
        num_removed
    }

    pub fn insert(&mut self, start: usize, text: &[char]) {
        let act = Action::Insert {
            start,
            text: text.into(),
        };
        act.do_on(self);
        self.push_action(act);
    }

    // XXX rename, too confusing
    pub fn insert_from_buffer(&mut self, other: &Buffer) {
        let start = self.data.len();
        self.insert(start, &other.data[start..])
    }

    pub fn copy_buffer(&mut self, other: &Buffer) {
        let data_len = self.data.len();
        self.remove(0, data_len);
        self.insert(0, &other.data[0..])
    }

    pub fn range(&self, start: usize, end: usize) -> String {
        self.data[start..end].iter().cloned().collect()
    }

    pub fn range_chars(&self, start: usize, end: usize) -> Vec<char> {
        self.data[start..end].to_owned()
    }

    pub fn width(&self) -> Vec<usize> {
        self.range_width(0, self.num_chars())
    }

    pub fn range_width(&self, start: usize, end: usize) -> Vec<usize> {
        self.range(start, end)
            .split('\n')
            .map(|s| s.width())
            .collect()
    }

    pub fn lines(&self) -> Vec<String> {
        self.data
            .split(|&c| c == '\n')
            .map(|s| s.iter().cloned().collect())
            .collect()
    }

    pub fn chars(&self) -> ::std::slice::Iter<char> {
        self.data.iter()
    }

    pub fn truncate(&mut self, num: usize) {
        let end = self.data.len();
        self.remove(num, end);
    }

    pub fn print<W>(&self, out: &mut W) -> io::Result<()>
    where
        W: Write,
    {
        let string: String = self.data.iter().cloned().collect();
        out.write_all(string.as_bytes())
    }

    pub fn as_bytes(&self) -> Vec<u8> {
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

    fn remove_raw(&mut self, start: usize, end: usize) -> Vec<char> {
        self.data.drain(start..end).collect()
    }

    fn insert_raw(&mut self, start: usize, text: &[char]) {
        for (i, &c) in text.iter().enumerate() {
            self.data.insert(start + i, c)
        }
    }

    /// Check if the other buffer starts with the same content as this one.
    /// Used to implement autosuggestions.
    pub fn starts_with(&self, other: &Buffer) -> bool {
        let other_len = other.data.len();
        let self_len = self.data.len();
        if !other.data.is_empty() && self_len != other_len {
            let match_let = self
                .data
                .iter()
                .zip(&other.data)
                .take_while(|&(s, o)| *s == *o)
                .count();
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
        self.data
            .windows(search_term.len())
            .any(|window| window == search_term)
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
        assert_eq!(buf.undo(), true);
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
        assert_eq!(buf.undo(), true);
        assert_eq!(buf.redo(), true);
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
        assert_eq!(buf.undo(), true);
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
        assert_eq!(buf.undo(), true);
        assert_eq!(buf.redo(), true);
        assert_eq!(String::from(buf), "defg");
    }

    #[test]
    fn test_starts_with() {
        let mut buf = Buffer::new();
        buf.insert(0, &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let mut buf2 = Buffer::new();
        buf2.insert(0, &['a', 'b', 'c']);
        assert_eq!(buf.starts_with(&buf2), true);
    }

    #[test]
    fn test_does_not_start_with() {
        let mut buf = Buffer::new();
        buf.insert(0, &['a', 'b', 'c']);
        let mut buf2 = Buffer::new();
        buf2.insert(0, &['a', 'b', 'c']);
        assert_eq!(buf.starts_with(&buf2), false);
    }

    #[test]
    fn test_is_not_match2() {
        let mut buf = Buffer::new();
        buf.insert(0, &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let mut buf2 = Buffer::new();
        buf2.insert(0, &['x', 'y', 'z']);
        assert_eq!(buf.starts_with(&buf2), false);
    }

    #[test]
    fn test_contains() {
        let mut buf = Buffer::new();
        buf.insert(0, &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let mut buf2 = Buffer::new();
        buf2.insert(0, &['a', 'b', 'c']);
        assert_eq!(buf.contains(&buf2), true);
        let mut buf2 = Buffer::new();
        buf2.insert(0, &['c', 'd', 'e']);
        assert_eq!(buf.contains(&buf2), true);
        let mut buf2 = Buffer::new();
        buf2.insert(0, &['e', 'f', 'g']);
        assert_eq!(buf.contains(&buf2), true);
    }

    #[test]
    fn test_does_not_contain() {
        let mut buf = Buffer::new();
        buf.insert(0, &['a', 'b', 'c', 'd', 'e', 'f', 'g']);
        let mut buf2 = Buffer::new();
        buf2.insert(0, &['x', 'b', 'c']);
        assert_eq!(buf.contains(&buf2), false);
        let mut buf2 = Buffer::new();
        buf2.insert(0, &['a', 'b', 'd']);
        assert_eq!(buf.contains(&buf2), false);
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
