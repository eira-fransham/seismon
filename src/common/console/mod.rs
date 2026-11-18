use std::{fmt, io, marker::PhantomData, mem, time::Duration};

use bevy::{asset::AssetLoader, prelude::*};
use bevy_mod_commands::{
    CommandsPlugin, OutputType, PendingCommands, RegisterCmdExt as _, RunCmd, clap::Parser,
};
use lined::{
    DefaultWordDivider, Editor, EditorContext, Emacs, Key, KeyBindings, KeyMap as _, Prompt, Tty,
    WordDivider,
};
use seismon_utils::QString;

/// Adds [`CommandsPlugin`] (if it hasn't been added already) plus the Seismon-specific
/// `stuffcmds` command.
#[derive(Default)]
pub struct SeismonConsoleCorePlugin;

impl SeismonConsoleCorePlugin {
    pub fn new() -> Self {
        Self
    }
}

/// Commands passed on the command line, to be run by `stuffcmds`.
#[derive(Resource, Default)]
pub struct StuffCmds(pub Vec<RunCmd<'static>>);

impl Plugin for SeismonConsoleCorePlugin {
    fn build(&self, app: &mut App) {
        #[derive(Parser)]
        #[command(name = "stuffcmds", about = "Run the commands from the CLI input arguments")]
        struct StuffCmdsArgs;

        app.add_plugins(CommandsPlugin {
            capture_out: true,
            capture_unhandled: true,
            register_builtins: true,
        });

        app.init_resource::<StuffCmds>().command(
            |In(StuffCmdsArgs),
             mut stuffcmds: ResMut<StuffCmds>,
             mut pending_commands: ResMut<PendingCommands>| {
                for cmd in stuffcmds.0.drain(..).rev() {
                    pending_commands.0.push_front(cmd);
                }
            },
        );
    }
}

#[derive(Asset, Reflect)]
pub struct TextAsset {
    pub text: String,
}

#[derive(Default, Reflect)]
#[non_exhaustive]
pub struct TextLoader {}

impl AssetLoader for TextLoader {
    type Asset = TextAsset;
    type Error = io::Error;
    type Settings = ();

    fn load(
        &self,
        reader: &mut dyn bevy::asset::io::Reader,
        _: &Self::Settings,
        _: &mut bevy::asset::LoadContext,
    ) -> impl bevy::tasks::ConditionalSendFuture<Output = std::result::Result<Self::Asset, Self::Error>>
    {
        async move {
            let mut bytes = Vec::new();
            reader.read_to_end(&mut bytes).await?;
            Ok(TextAsset { text: String::from_utf8(bytes).map_err(io::Error::other)? })
        }
    }

    fn extensions(&self) -> &[&str] {
        &["cfg", "rc"]
    }
}

/// The line of text currently being edited in the console.
#[derive(Default)]
pub struct ConsoleInputContext {
    pub input_buf: String,
    pub history: lined::History,
    pub key_bindings: KeyBindings,
    pub commands: Vec<RunCmd<'static>>,
    pub terminal: ConsoleInputTerminal,
    pub cmd_buf: String,
}

/// The editor's output terminal. The console draws its input line from the editor's buffer
/// directly, so the terminal escape codes the editor writes here are discarded.
#[derive(Default)]
pub struct ConsoleInputTerminal;

impl io::Write for ConsoleInputTerminal {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl Tty for ConsoleInputTerminal {
    fn width(&self) -> io::Result<usize> {
        Ok(80) // TODO: Make this actually read the console width
    }
}

impl fmt::Write for ConsoleInputContext {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.cmd_buf.push_str(s);

        Ok(())
    }
}

impl EditorContext for ConsoleInputContext {
    type Terminal = ConsoleInputTerminal;
    type WordDividerIter = <DefaultWordDivider as WordDivider>::DividerIter;

    fn history(&self) -> &lined::History {
        &self.history
    }

    fn history_mut(&mut self) -> &mut lined::History {
        &mut self.history
    }

    fn word_divider(&self, buf: &lined::Buffer) -> Self::WordDividerIter {
        DefaultWordDivider.divide(buf)
    }

    fn terminal(&self) -> &Self::Terminal {
        &self.terminal
    }

    fn terminal_mut(&mut self) -> &mut Self::Terminal {
        &mut self.terminal
    }

    fn key_bindings(&self) -> lined::KeyBindings {
        self.key_bindings
    }
}

pub fn to_terminal_key<'a>(
    key: &'a bevy::input::keyboard::Key,
    button_state: &bevy::input::ButtonInput<KeyCode>,
) -> impl Iterator<Item = Key> + 'a {
    use bevy::input::keyboard::Key::*;
    use itertools::Either;

    let make_char =
        if button_state.pressed(KeyCode::AltLeft) || button_state.pressed(KeyCode::AltRight) {
            Key::Alt
        } else if button_state.pressed(KeyCode::ControlLeft)
            || button_state.pressed(KeyCode::ControlRight)
        {
            Key::Ctrl
        } else {
            Key::Char
        };

    match key {
        Character(c) => Either::Left(c.chars().map(make_char)),
        Backspace => Either::Right(Some(Key::Backspace).into_iter()),
        Delete => Either::Right(Some(Key::Delete).into_iter()),
        Enter => Either::Right(Some(Key::Char('\n')).into_iter()),
        Tab => Either::Right(Some(Key::Char('\t')).into_iter()),
        Space => Either::Right(Some(Key::Char(' ')).into_iter()),
        ArrowUp => Either::Right(Some(Key::Up).into_iter()),
        ArrowDown => Either::Right(Some(Key::Down).into_iter()),
        ArrowLeft => Either::Right(Some(Key::Left).into_iter()),
        ArrowRight => Either::Right(Some(Key::Right).into_iter()),
        End => Either::Right(Some(Key::End).into_iter()),
        Home => Either::Right(Some(Key::Home).into_iter()),
        Fn | FnLock | ScrollLock | Symbol | SymbolLock | Meta | Hyper | Shift | Control | Alt
        | Super | Escape | CapsLock | Dead(_) => Either::Right(None.into_iter()),

        // TODO
        _ => Either::Right(None.into_iter()),
    }
}

// TODO: This can be a tree for much better completions but we don't have enough commands to make it
// necessary right now       The `lined` interface allocates a lot anyway, so micro-optimisation
// isn't necessary here.
struct IterCompleter<'a, I> {
    iter: I,
    _marker: PhantomData<&'a ()>,
}

impl<'iter, I, Item> lined::Completer for IterCompleter<'iter, I>
where
    Item: ?Sized + 'iter,
    I: Iterator<Item = &'iter Item> + Clone + 'iter,
    &'iter Item: Into<std::borrow::Cow<'iter, str>>,
{
    fn completions<'a>(
        &'a mut self,
        start: &'a str,
    ) -> impl Iterator<Item = std::borrow::Cow<'a, str>> + 'a {
        self.iter.clone().filter_map(move |candidate: &'iter Item| {
            let candidate = candidate.into();
            if candidate.starts_with(start) { Some(candidate) } else { None }
        })
    }
}

#[derive(Resource)]
pub struct ConsoleInput {
    editor: Option<Editor<ConsoleInputContext>>,
    keymap: Emacs,
}

impl Default for ConsoleInput {
    fn default() -> Self {
        Self::new(default()).unwrap()
    }
}

impl ConsoleInput {
    pub const PROMPT: &'static str = "] ";

    /// Constructs a new `ConsoleInput`.
    ///
    /// Initializes the text content to be empty and places the cursor at position 0.
    pub fn new(history: lined::History) -> io::Result<ConsoleInput> {
        let mut keymap = Emacs::new();

        let editor = match Editor::new(
            Prompt::from(Self::PROMPT.to_owned()),
            ConsoleInputContext { history, ..default() },
        ) {
            Ok(mut editor) => {
                keymap.init(&mut editor)?;
                Some(editor)
            }
            Err(e) => {
                error!("{e}");
                None
            }
        };

        Ok(ConsoleInput { editor, keymap })
    }

    /// Send characters to the inner editor
    pub fn update<'a, I, C, Item>(
        &'a mut self,
        keys: I,
        candidates: C,
    ) -> impl Iterator<Item = io::Result<String>> + 'a
    where
        I: IntoIterator<Item = Key>,
        I::IntoIter: 'a,
        C: Iterator<Item = &'a Item> + Clone + 'a,
        Item: ?Sized + 'a,
        &'a Item: Into<std::borrow::Cow<'a, str>>,
    {
        let mut completer = IterCompleter { iter: candidates, _marker: PhantomData };
        keys.into_iter().filter_map(move |key| {
            let editor = self.editor.as_mut()?;

            // TODO: Completion
            match self.keymap.handle_key(key, editor, &mut completer) {
                Ok(true) => {
                    let out = editor.take_exec_buffer();

                    if let Err(e) = editor.move_to_end_of_history() {
                        warn!(target: "console", "{e}");
                    }

                    Some(
                        editor
                            .context_mut()
                            .history
                            .push(out.clone().into())
                            .and_then(|()| editor.move_cursor_to_start_of_line())
                            .map(|()| out),
                    )
                }
                Ok(false) => None,
                Err(e) => Some(Err(e)),
            }
        })
    }

    /// Returns the text currently being edited
    pub fn get_text(&self) -> impl Iterator<Item = char> + '_ {
        Self::PROMPT.chars().chain(self.editor.iter().flat_map(|e| e.current_buffer().chars()))
    }
}

#[derive(Debug, Clone)]
pub struct ConsoleText {
    pub output_type: OutputType,
    pub text: QString,
}

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct Timestamp {
    pub(crate) timestamp: i64,
    /// Extra tag to ensure that messages with the same timestamp are still emitted in the order
    /// they were added
    generation: u16,
}

impl Timestamp {
    pub fn new(timestamp: i64, generation: u16) -> Self {
        Self { timestamp, generation }
    }
}

#[derive(Resource, Default, Debug)]
pub struct ConsoleOutput {
    generation: u16,
    center_print: Option<(Timestamp, QString)>,
    buffer_ty: OutputType,
    buffer: QString,
    last_timestamp: i64,
    unwritten_chunks: Vec<(Timestamp, ConsoleText)>,
}

pub(crate) fn millis_i64(duration: Duration) -> i64 {
    duration.as_millis().try_into().expect("Elapsed overflowed i64")
}

impl ConsoleOutput {
    pub fn print<S: AsRef<[u8]>>(&mut self, s: S, timestamp: &Time<Real>) {
        self.push(s, millis_i64(timestamp.elapsed()), OutputType::Console);
    }

    pub fn print_alert<S: AsRef<[u8]>>(&mut self, s: S, timestamp: &Time<Real>) {
        self.push(s, millis_i64(timestamp.elapsed()), OutputType::Alert);
    }

    pub fn println<S: AsRef<[u8]>>(&mut self, s: S, timestamp: &Time<Real>) {
        self.push_line(s, millis_i64(timestamp.elapsed()), OutputType::Console);
    }

    pub fn println_alert<S: AsRef<[u8]>>(&mut self, s: S, timestamp: &Time<Real>) {
        self.push_line(s, millis_i64(timestamp.elapsed()), OutputType::Alert);
    }

    pub fn new() -> ConsoleOutput {
        ConsoleOutput::default()
    }

    fn push<S: AsRef<[u8]>>(&mut self, chars: S, timestamp: i64, ty: OutputType) {
        let chars = chars.as_ref();

        if chars.is_empty() {
            return;
        }

        self.last_timestamp = timestamp;

        // TODO: set maximum capacity and pop_back when we reach it
        if ty != self.buffer_ty {
            self.flush();
        }

        self.buffer_ty = ty;
        self.buffer.push_bytes(chars);

        self.try_flush();
    }

    fn try_flush(&mut self) {
        if let Some(last_newline) = self.buffer.raw.iter().rposition(|v| *v == b'\n') {
            let (to_flush, rest) = self.buffer.split_at(last_newline + 1);
            let new_buf = rest.to_owned();
            self.buffer.truncate(to_flush.len());
            let generation = self.generation();
            self.unwritten_chunks.push((
                Timestamp::new(self.last_timestamp, generation),
                ConsoleText {
                    text: mem::replace(&mut self.buffer, new_buf.into()),
                    output_type: self.buffer_ty,
                },
            ));
        }
    }

    fn flush(&mut self) {
        let generation = self.generation();
        self.unwritten_chunks.push((
            Timestamp::new(self.last_timestamp, generation),
            ConsoleText { text: mem::take(&mut self.buffer), output_type: self.buffer_ty },
        ));
    }

    fn push_line<S: AsRef<[u8]>>(&mut self, chars: S, timestamp: i64, ty: OutputType) {
        if let Ok(vals) = str::from_utf8(chars.as_ref()) {
            match ty {
                OutputType::Console => debug!(target: "console", "{vals}"),
                OutputType::Alert => warn!(target: "console", "{vals}"),
            }
        }
        self.push(chars, timestamp, ty);
        self.push("\n", timestamp, ty);
    }

    fn generation(&mut self) -> u16 {
        let out = self.generation;
        self.generation = self.generation.wrapping_add(1);
        out
    }

    pub fn set_center_print<S: Into<QString>>(&mut self, print: S, timestamp: &Time<Real>) {
        let generation = self.generation();
        self.center_print =
            Some((Timestamp::new(millis_i64(timestamp.elapsed()), generation), print.into()));
    }

    pub(crate) fn drain_center_print(&mut self) -> Option<(Timestamp, QString)> {
        self.center_print.take()
    }

    pub(crate) fn drain_unwritten(
        &mut self,
    ) -> impl ExactSizeIterator<Item = (Timestamp, ConsoleText)> + '_ {
        self.unwritten_chunks.drain(..)
    }
}
