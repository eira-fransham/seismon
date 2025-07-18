use crate::complete::Completer;
use crate::event::*;
use crate::Editor;
use crate::EditorContext;
use std::io::{self, ErrorKind};
use termion::event::Key;

pub trait KeyMap: Default {
    fn handle_key_core<C: EditorContext>(
        &mut self,
        key: Key,
        editor: &mut Editor<C>,
    ) -> io::Result<()>;

    fn init<C: EditorContext>(&mut self, _editor: &mut Editor<C>) -> io::Result<()> {
        Ok(())
    }

    fn handle_key<Ctx: EditorContext, C: Completer>(
        &mut self,
        mut key: Key,
        editor: &mut Editor<Ctx>,
        handler: &mut C,
    ) -> io::Result<bool> {
        let mut done = false;

        handler.on_event(Event::new(editor, EventKind::BeforeKey(key)));

        let is_empty = editor.current_buffer().is_empty();

        if key == Key::Ctrl('h') {
            // XXX: Might need to change this when remappable keybindings are added.
            key = Key::Backspace;
        }

        match key {
            Key::Ctrl('c') => {
                editor.handle_newline()?;
                return Err(io::Error::new(ErrorKind::Interrupted, "ctrl-c"));
            }
            // if the current buffer is empty, treat ctrl-d as eof
            Key::Ctrl('d') if is_empty => {
                editor.handle_newline()?;
                return Err(io::Error::new(ErrorKind::UnexpectedEof, "ctrl-d"));
            }
            Key::Char('\t') => editor.complete(handler)?,
            Key::Char('\n') => {
                done = editor.handle_newline()?;
            }
            Key::Ctrl('f') if editor.is_currently_showing_autosuggestion() => {
                editor.accept_autosuggestion()?;
            }
            Key::Ctrl('r') => {
                editor.search(false)?;
            }
            Key::Ctrl('s') => {
                editor.search(true)?;
            }
            Key::Right
                if editor.is_currently_showing_autosuggestion()
                    && editor.cursor_is_at_end_of_line() =>
            {
                editor.accept_autosuggestion()?;
            }
            _ => {
                self.handle_key_core(key, editor)?;
                editor.skip_completions_hint();
            }
        };

        handler.on_event(Event::new(editor, EventKind::AfterKey(key)));

        editor.flush()?;

        Ok(done)
    }
}

pub mod vi;
pub use vi::Vi;

pub mod emacs;
pub use emacs::Emacs;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::editor::Prompt;
    use crate::Context;
    use std::io::ErrorKind;
    use termion::event::Key::*;

    #[derive(Default)]
    struct TestKeyMap;

    impl KeyMap for TestKeyMap {
        fn handle_key_core<C: EditorContext>(
            &mut self,
            _: Key,
            _: &mut Editor<C>,
        ) -> io::Result<()> {
            Ok(())
        }
    }

    struct EmptyCompleter;

    impl Completer for EmptyCompleter {
        fn completions(&mut self, _start: &str) -> Vec<String> {
            Vec::default()
        }
    }

    #[test]
    /// when the current buffer is empty, ctrl-d generates and eof error
    fn ctrl_d_empty() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = TestKeyMap;

        let res = map.handle_key(Ctrl('d'), &mut ed, &mut EmptyCompleter);
        assert_eq!(res.is_err(), true);
        assert_eq!(res.err().unwrap().kind(), ErrorKind::UnexpectedEof);
    }

    #[test]
    /// when the current buffer is not empty, ctrl-d should be ignored
    fn ctrl_d_non_empty() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = TestKeyMap;
        ed.insert_str_after_cursor("not empty").unwrap();

        let res = map.handle_key(Ctrl('d'), &mut ed, &mut EmptyCompleter);
        assert_eq!(res.is_ok(), true);
    }

    #[test]
    /// ctrl-c should generate an error
    fn ctrl_c() {
        let mut context = Context::test();
        let mut ed = Editor::new(Prompt::from("prompt"), None, &mut context).unwrap();
        let mut map = TestKeyMap;

        let res = map.handle_key(Ctrl('c'), &mut ed, &mut EmptyCompleter);
        assert_eq!(res.is_err(), true);
        assert_eq!(res.err().unwrap().kind(), ErrorKind::Interrupted);
    }
}
