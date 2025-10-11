//! # `event`
//!
//! Items related to editor events. See [`Event`] for the main event type.

use crate::{Editor, EditorContext, Highlighter};
use termion::event::Key;

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
    where E: ?Sized + AsMut<Editor<C, dyn Highlighter>> {
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
