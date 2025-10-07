use std::{
    borrow::Cow,
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

pub trait Highlighter: Send + Sync + 'static {
    fn highlight<'a>(&self, input: &'a str) -> Cow<'a, str>;
}

pub trait WordDivider {
    type DividerIter: ExactSizeIterator<Item = (usize, usize)> + Clone;

    fn divide(&self, buf: &Buffer) -> Self::DividerIter;
}

pub struct NoHighlighting;

impl Highlighter for NoHighlighting {
    fn highlight<'a>(&self, input: &'a str) -> Cow<'a, str> {
        input.into()
    }
}

/// The default for `Context.word_divider`.
pub struct DefaultWordDivider;

impl WordDivider for DefaultWordDivider {
    type DividerIter = <Vec<(usize, usize)> as IntoIterator>::IntoIter;

    fn divide(&self, buf: &Buffer) -> Self::DividerIter {
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

        res.into_iter()
    }
}

/// The style of key bindings to use.
#[derive(Default, Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum KeyBindings {
    /// Vi/Vim-style keybindings.
    // TODO: List out all keybindings in documentation.
    Vi,
    /// Emacs-style keybindings (default).
    // TODO: List out all keybindings in documentation.
    #[default]
    Emacs,
}

/// [`Tty`] implementation that reads input from stdin and writes output to stdout.
pub struct DefaultTty {
    stdin: Keys<Stdin>,
    stdout: RawTerminal<Stdout>,
}

impl DefaultTty {
    /// Create a new [`DefaultTty`] from the environment's stdin and stdout.
    pub fn new() -> io::Result<Self> {
        let stdin = io::stdin().keys();
        let stdout = io::stdout().into_raw_mode()?;

        Ok(Self { stdin, stdout })
    }
}

/// A trait to represent TTYs. [`DefaultTty`] implements this for STDIN/STDOUT, and for standard
/// `redox_liner`-like functionality you should use that.
// TODO: Maybe better to take `Stdin` out of this entirely and just have a `next_key` function
pub trait Tty: io::Write {
    /// Read the next key from the underlying TTY. For [`DefaultTty`], this is stdin, but for
    /// an embedded terminal this might read keys from another input library.
    fn next_key(&mut self) -> Option<io::Result<Key>>;
    /// Get the width of the TTY, used for line breaking and formatting.
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

/// The context for a `lined` session. Contains the history, configurable word dividers, keybindings,
/// the underlying TTY, and the current line being edited.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Context<T = DefaultTty, W = DefaultWordDivider> {
    /// The history of previous commands (see [`History`]).
    pub history: History,
    /// The function that is used to divide words, for splitting lines at the terminal width.
    pub word_divider: W,
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
    fn read_line<C>(&mut self, prompt: Prompt, handler: &mut C) -> io::Result<String>
    where
        C: Completer,
        Self: Sized,
    {
        self.read_line_with_highlighter(prompt, handler, NoHighlighting)
    }

    /// Same as [`Context::read_line`], but with a custom highlighter.
    fn read_line_with_highlighter<C, H>(
        &mut self,
        prompt: Prompt,
        handler: &mut C,
        highlighter: H,
    ) -> io::Result<String>
    where
        C: Completer,
        Self: Sized,
        H: Highlighter,
    {
        self.read_line_with_init_buffer_and_highlighter(prompt, handler, highlighter, Buffer::new())
    }

    /// Same as [`Context::read_line_with_highlighter`], but passes the provided initial buffer to the editor.
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
    fn read_line_with_init_buffer_and_highlighter<B, C, H>(
        self,
        prompt: Prompt,
        handler: &mut C,
        highlighter: H,
        buffer: B,
    ) -> io::Result<String>
    where
        B: Into<Buffer>,
        C: Completer,
        Self: Sized,
        H: Highlighter,
    {
        let keybindings = self.key_bindings();

        let ed = Editor::new(prompt, self)?
            .with_init_buffer(buffer)
            .with_highlighter(highlighter);

        match keybindings {
            KeyBindings::Emacs => Self::handle_keys(keymap::Emacs::new(), ed, handler),
            KeyBindings::Vi => Self::handle_keys(keymap::Vi::new(), ed, handler),
        }
    }

    fn handle_keys<M, C, H>(
        mut keymap: M,
        mut ed: Editor<Self, H>,
        handler: &mut C,
    ) -> io::Result<String>
    where
        Self: Sized,
        M: KeyMap,
        C: Completer,
        H: Highlighter,
    {
        keymap.init(ed.as_dyn_mut())?;
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
        (**self).history()
    }

    fn history_mut(&mut self) -> &mut History {
        (**self).history_mut()
    }

    fn word_divider(&self, buf: &Buffer) -> Self::WordDividerIter {
        (**self).word_divider(buf)
    }

    fn terminal(&self) -> &Self::Terminal {
        (**self).terminal()
    }

    fn terminal_mut(&mut self) -> &mut Self::Terminal {
        (**self).terminal_mut()
    }

    fn key_bindings(&self) -> KeyBindings {
        (**self).key_bindings()
    }
}

impl<T, F> fmt::Write for Context<T, F> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.buffer.push_str(s);

        Ok(())
    }
}

impl<T, W> EditorContext for Context<T, W>
where
    W: WordDivider,
    T: Tty,
{
    type Terminal = T;
    type WordDividerIter = W::DividerIter;

    fn history(&self) -> &History {
        &self.history
    }

    fn history_mut(&mut self) -> &mut History {
        &mut self.history
    }

    fn word_divider(&self, buf: &Buffer) -> Self::WordDividerIter {
        self.word_divider.divide(buf)
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
        Self::from_terminal(T::default())
    }
}

impl Context {
    pub fn new() -> io::Result<Self> {
        Ok(Self::from_terminal(DefaultTty::new()?))
    }
}

impl<T> Context<T> {
    pub fn from_terminal(terminal: T) -> Self {
        Context {
            history: History::new(),
            word_divider: DefaultWordDivider,
            key_bindings: KeyBindings::Emacs,
            buffer: String::with_capacity(512),
            terminal,
        }
    }
}

impl<T, W> Context<T, W> {
    pub fn with_word_divider<NewW>(self, word_divider: NewW) -> Context<T, NewW> {
        Context {
            history: self.history,
            word_divider,
            key_bindings: self.key_bindings,
            buffer: self.buffer,
            terminal: self.terminal,
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
        Self::from_terminal(value)
    }
}
