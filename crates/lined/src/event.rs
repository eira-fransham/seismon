use crate::{Editor, EditorContext, Highlighter};
use termion::event::Key;

pub struct Event<'a, C: EditorContext> {
    pub editor: &'a mut Editor<C, dyn Highlighter>,
    pub kind: EventKind,
}

impl<'a, C: EditorContext> Event<'a, C> {
    pub fn new<E>(editor: &'a mut E, kind: EventKind) -> Self
    where
        E: ?Sized + AsMut<Editor<C, dyn Highlighter>>,
    {
        Event {
            editor: editor.as_mut(),
            kind,
        }
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
