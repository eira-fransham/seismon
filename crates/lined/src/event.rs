//! # `event`
//!
//! Items related to editor events. See [`Event`] for the main event type.

use crate::{Editor, EditorContext, Highlighter};

/// A key press.
///
/// This mirrors `termion`'s key type, so that the editor can be driven without a real terminal.
/// With the `terminal` feature, `termion` keys convert into it.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum Key {
    /// Backspace.
    Backspace,
    /// Left arrow.
    Left,
    /// Shift Left arrow.
    ShiftLeft,
    /// Alt Left arrow.
    AltLeft,
    /// Ctrl Left arrow.
    CtrlLeft,
    /// Right arrow.
    Right,
    /// Shift Right arrow.
    ShiftRight,
    /// Alt Right arrow.
    AltRight,
    /// Ctrl Right arrow.
    CtrlRight,
    /// Up arrow.
    Up,
    /// Shift Up arrow.
    ShiftUp,
    /// Alt Up arrow.
    AltUp,
    /// Ctrl Up arrow.
    CtrlUp,
    /// Down arrow.
    Down,
    /// Shift Down arrow.
    ShiftDown,
    /// Alt Down arrow.
    AltDown,
    /// Ctrl Down arrow.
    CtrlDown,
    /// Home key.
    Home,
    /// Ctrl Home key.
    CtrlHome,
    /// End key.
    End,
    /// Ctrl End key.
    CtrlEnd,
    /// Page Up key.
    PageUp,
    /// Page Down key.
    PageDown,
    /// Backward Tab key.
    BackTab,
    /// Delete key.
    Delete,
    /// Insert key.
    Insert,
    /// Function keys (1 through 12).
    F(u8),
    /// Normal character.
    Char(char),
    /// Alt modified character.
    Alt(char),
    /// Ctrl modified character.
    Ctrl(char),
    /// Null byte.
    Null,
    /// Esc key.
    Esc,
}

#[cfg(feature = "terminal")]
impl From<termion::event::Key> for Key {
    fn from(key: termion::event::Key) -> Self {
        use termion::event::Key as K;

        match key {
            K::Backspace => Key::Backspace,
            K::Left => Key::Left,
            K::ShiftLeft => Key::ShiftLeft,
            K::AltLeft => Key::AltLeft,
            K::CtrlLeft => Key::CtrlLeft,
            K::Right => Key::Right,
            K::ShiftRight => Key::ShiftRight,
            K::AltRight => Key::AltRight,
            K::CtrlRight => Key::CtrlRight,
            K::Up => Key::Up,
            K::ShiftUp => Key::ShiftUp,
            K::AltUp => Key::AltUp,
            K::CtrlUp => Key::CtrlUp,
            K::Down => Key::Down,
            K::ShiftDown => Key::ShiftDown,
            K::AltDown => Key::AltDown,
            K::CtrlDown => Key::CtrlDown,
            K::Home => Key::Home,
            K::CtrlHome => Key::CtrlHome,
            K::End => Key::End,
            K::CtrlEnd => Key::CtrlEnd,
            K::PageUp => Key::PageUp,
            K::PageDown => Key::PageDown,
            K::BackTab => Key::BackTab,
            K::Delete => Key::Delete,
            K::Insert => Key::Insert,
            K::F(n) => Key::F(n),
            K::Char(c) => Key::Char(c),
            K::Alt(c) => Key::Alt(c),
            K::Ctrl(c) => Key::Ctrl(c),
            K::Null => Key::Null,
            K::Esc => Key::Esc,
            _ => Key::Null,
        }
    }
}

/// An editor event - carries a mutable reference to the underlying editor.
pub struct Event<'a, C: EditorContext> {
    /// The underlying editor, with the type of the highlighter erased.
    pub editor: &'a mut Editor<C, dyn Highlighter>,
    /// The event kind (see [`EventKind`]).
    pub kind: EventKind,
}

impl<'a, C: EditorContext> Event<'a, C> {
    /// Create a new [`Event`] from an editor and a kind.
    pub fn new<E>(editor: &'a mut E, kind: EventKind) -> Self
    where
        E: ?Sized + AsMut<Editor<C, dyn Highlighter>>,
    {
        Event { editor: editor.as_mut(), kind }
    }
}

/// The kind of an event, see variant docs for details.
#[derive(Debug)]
pub enum EventKind {
    /// Sent before handling a keypress.
    BeforeKey(Key),
    /// Sent after handling a keypress.
    AfterKey(Key),
    /// Sent in `Editor.complete()`, before processing the completion.
    BeforeComplete,
}
