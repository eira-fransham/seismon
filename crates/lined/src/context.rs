use std::{
    fmt,
    io::{self, Stdin, Stdout},
};
use termion::{
    event::Key,
    input::{Keys, TermRead},
    raw::{IntoRawMode, RawTerminal},
};

use super::*;
use crate::editor::Prompt;
use keymap;

pub type ColorClosure = Box<dyn Fn(&str) -> String + Send + Sync + 'static>;

/// The default for `Context.word_divider_fn`.
pub fn get_buffer_words(buf: &Buffer) -> Vec<(usize, usize)> {
    let mut res = Vec::new();

    let mut word_start = None;
    let mut just_had_backslash = false;

    for (i, &c) in buf.chars().enumerate() {
        if c == '\\' {
            just_had_backslash = true;
            continue;
        }

        if let Some(start) = word_start {
            if c == ' ' && !just_had_backslash {
                res.push((start, i));
                word_start = None;
            }
        } else if c != ' ' {
            word_start = Some(i);
        }

        just_had_backslash = false;
    }

    if let Some(start) = word_start {
        res.push((start, buf.num_chars()));
    }

    res
}

/// The key bindings to use.
#[derive(Default, Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum KeyBindings {
    Vi,
    #[default]
    Emacs,
}

pub struct DefaultTty {
    stdin: Keys<Stdin>,
    stdout: RawTerminal<Stdout>,
}

impl DefaultTty {
    pub fn new() -> io::Result<Self> {
        let stdin = io::stdin().keys();
        let stdout = io::stdout().into_raw_mode()?;

        Ok(Self { stdin, stdout })
    }
}

// TODO: Maybe better to take `Stdin` out of this entirely and just have a `next_key` function
pub trait Tty: io::Write {
    fn next_key(&mut self) -> Option<io::Result<Key>>;
    fn width(&self) -> io::Result<usize>;
}

impl Tty for DefaultTty {
    fn next_key(&mut self) -> Option<io::Result<Key>> {
        self.stdin.next()
    }

    fn width(&self) -> io::Result<usize> {
        util::terminal_width()
    }
}

impl io::Write for DefaultTty {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.stdout.write(buf)
    }

    fn write_vectored(&mut self, bufs: &[io::IoSlice<'_>]) -> io::Result<usize> {
        self.stdout.write_vectored(bufs)
    }

    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        self.stdout.write_all(buf)
    }

    fn write_fmt(&mut self, fmt: fmt::Arguments<'_>) -> io::Result<()> {
        self.stdout.write_fmt(fmt)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.stdout.flush()
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Context<T = DefaultTty, F = Box<dyn Fn(&Buffer) -> Vec<(usize, usize)>>> {
    pub history: History,
    pub word_divider_fn: F,
    pub key_bindings: KeyBindings,
    pub terminal: T,
    pub buffer: String,
}

pub trait EditorContext: fmt::Write {
    type Terminal: Tty;
    type WordDividerIter: ExactSizeIterator<Item = (usize, usize)> + Clone;

    /// Get an immutable reference to the context history
    fn history(&self) -> &History;
    /// Get a mutable reference to the context history
    fn history_mut(&mut self) -> &mut History;
    /// Get the word divider points in the specified buffer
    fn word_divider(&self, buf: &Buffer) -> Self::WordDividerIter;
    /// Get an immutable reference to the output terminal
    fn terminal(&self) -> &Self::Terminal;
    /// Get a mutable reference to the output terminal
    fn terminal_mut(&mut self) -> &mut Self::Terminal;
    /// Get the key binding type (Vi or Emacs)
    fn key_bindings(&self) -> KeyBindings;

    /// Creates an `Editor` and feeds it keypresses from stdin until the line is entered.
    /// The output is stdout.
    /// The returned line has the newline removed.
    /// Before returning, will revert all changes to the history buffers.
    fn read_line<C: Completer>(
        &mut self,
        prompt: Prompt,
        f: Option<ColorClosure>,
        handler: &mut C,
    ) -> io::Result<String>
    where
        Self: Sized,
    {
        self.read_line_with_init_buffer(prompt, handler, f, Buffer::new())
    }

    /// Same as `Context.read_line()`, but passes the provided initial buffer to the editor.
    ///
    /// ```no_run
    /// use liner::{Context, Completer, Prompt, EditorContext};
    ///
    /// struct EmptyCompleter;
    ///
    /// impl Completer for EmptyCompleter {
    ///     fn completions(&mut self, _start: &str) -> Vec<String> {
    ///         Vec::new()
    ///     }
    /// }
    ///
    /// let mut context = Context::new().unwrap();
    /// let line =
    ///     context.read_line_with_init_buffer(Prompt::from("[prompt]$ "),
    ///                                        &mut EmptyCompleter,
    ///                                        Some(Box::new(|s| String::from(s))),
    ///                                        "some initial buffer");
    /// ```
    fn read_line_with_init_buffer<B: Into<Buffer>, C: Completer>(
        self,
        prompt: Prompt,
        handler: &mut C,
        f: Option<ColorClosure>,
        buffer: B,
    ) -> io::Result<String>
    where
        Self: Sized,
    {
        let keybindings = self.key_bindings();

        let ed = Editor::new_with_init_buffer(prompt, f, self, buffer)?;

        match keybindings {
            KeyBindings::Emacs => Self::handle_keys(keymap::Emacs::new(), ed, handler),
            KeyBindings::Vi => Self::handle_keys(keymap::Vi::new(), ed, handler),
        }

        // TODO: Why is this commented?
        //self.revert_all_history();
    }

    fn handle_keys<'a, M: KeyMap, C: Completer>(
        mut keymap: M,
        mut ed: Editor<Self>,
        handler: &mut C,
    ) -> io::Result<String>
    where
        Self: Sized,
    {
        keymap.init(&mut ed)?;
        loop {
            let key = ed.context_mut().terminal_mut().next_key();

            if let Some(key) = key {
                if keymap.handle_key(key?, &mut ed, handler)? {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(ed.into())
    }

    fn revert_all_history(&mut self) {
        for buf in &mut self.history_mut().buffers {
            buf.revert();
        }
    }
}

impl<C: EditorContext> EditorContext for &'_ mut C {
    type Terminal = C::Terminal;
    type WordDividerIter = C::WordDividerIter;

    fn history(&self) -> &History {
        (&**self).history()
    }

    fn history_mut(&mut self) -> &mut History {
        (&mut **self).history_mut()
    }

    fn word_divider(&self, buf: &Buffer) -> Self::WordDividerIter {
        (&**self).word_divider(buf)
    }

    fn terminal(&self) -> &Self::Terminal {
        (&**self).terminal()
    }

    fn terminal_mut(&mut self) -> &mut Self::Terminal {
        (&mut **self).terminal_mut()
    }

    fn key_bindings(&self) -> KeyBindings {
        (&**self).key_bindings()
    }
}

impl<T, F> fmt::Write for Context<T, F> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.buffer.push_str(s);

        Ok(())
    }
}

impl<T: Tty, F: Fn(&Buffer) -> Vec<(usize, usize)>> EditorContext for Context<T, F> {
    type Terminal = T;
    type WordDividerIter = <Vec<(usize, usize)> as IntoIterator>::IntoIter;

    fn history(&self) -> &History {
        &self.history
    }

    fn history_mut(&mut self) -> &mut History {
        &mut self.history
    }

    fn word_divider(&self, buf: &Buffer) -> Self::WordDividerIter {
        (self.word_divider_fn)(buf).into_iter()
    }

    fn terminal(&self) -> &Self::Terminal {
        &self.terminal
    }

    fn terminal_mut(&mut self) -> &mut Self::Terminal {
        &mut self.terminal
    }

    fn key_bindings(&self) -> KeyBindings {
        self.key_bindings
    }
}

impl<T: Default> Default for Context<T> {
    fn default() -> Self {
        Self::with_terminal(T::default())
    }
}

impl Context {
    pub fn new() -> io::Result<Self> {
        Ok(Self::with_terminal(DefaultTty::new()?))
    }
}

impl<T> Context<T> {
    pub fn with_terminal(terminal: T) -> Self {
        Context {
            history: History::new(),
            word_divider_fn: Box::new(get_buffer_words),
            key_bindings: KeyBindings::Emacs,
            buffer: String::with_capacity(512),
            terminal,
        }
    }
}

#[cfg(test)]
impl Context<crate::test::TestTty> {
    pub(crate) fn test() -> Self {
        Self::default()
    }
}

impl<T> From<T> for Context<T> {
    fn from(value: T) -> Self {
        Self::with_terminal(value)
    }
}
