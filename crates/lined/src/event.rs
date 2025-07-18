use crate::{Editor, EditorContext};
use termion::event::Key;

pub struct Event<'a, C: EditorContext> {
    pub editor: &'a mut Editor<C>,
    pub kind: EventKind,
}

impl<'a, C: EditorContext> Event<'a, C> {
    pub fn new(editor: &'a mut Editor<C>, kind: EventKind) -> Self {
        Event { editor, kind }
    }
}

#[derive(Debug)]
pub enum EventKind {
    /// Sent before handling a keypress.
    BeforeKey(Key),
    /// Sent after handling a keypress.
    AfterKey(Key),
    /// Sent in `Editor.complete()`, before processing the completion.
    BeforeComplete,
}
