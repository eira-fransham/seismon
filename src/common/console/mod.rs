use std::{collections::BTreeMap, fmt, io, marker::PhantomData, mem, ops::Range, time::Duration};

use bevy::{asset::AssetLoader, prelude::*};
use bevy_mod_commands::{
    CommandOutput, CommandsPlugin, CommandsSystems, OutputType, PendingCommands,
    RegisterCmdExt as _, Registry, RunCmd, UnhandledCmd, clap::Parser,
};
use lined::{
    DefaultWordDivider, Editor, EditorContext, Emacs, Key, KeyBindings, KeyMap as _, Prompt, Tty,
    WordDivider,
};
use seismon_utils::{QStr, QString};

use crate::client::{Disconnect, NewConnection, input::InputFocus};

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

pub struct SeismonConsoleRenderPlugin;

impl Plugin for SeismonConsoleRenderPlugin {
    fn build(&self, app: &mut App) {
        warn!(target: "console", "Console history unimplemented for now");

        fn resize_small(mut console_ui: Query<&mut Node, With<ConsoleUi>>) {
            for mut style in &mut console_ui {
                style.height = Val::Percent(30.);
            }
        }

        fn resize_large(mut console_ui: Query<&mut Node, With<ConsoleUi>>) {
            for mut style in &mut console_ui {
                style.height = Val::Percent(100.);
            }
        }

        app.add_plugins(SeismonConsoleCorePlugin::new())
            .add_message::<UnhandledCmd>()
            .add_message::<CommandOutput>()
            .init_resource::<ConsoleOutput>()
            .insert_resource(ConsoleInput::new(Default::default()).unwrap())
            .init_resource::<RenderConsoleOutput>()
            .init_resource::<RenderConsoleInput>()
            .init_resource::<ConsoleAlertSettings>()
            .add_systems(
                Startup,
                (systems::startup::init_alert_output, systems::startup::init_console),
            )
            .add_systems(
                PreUpdate,
                (
                    resize_large.run_if(on_message::<Disconnect>),
                    resize_small.run_if(on_message::<NewConnection>),
                ),
            )
            .add_systems(
                Update,
                (
                    systems::update_render_console,
                    systems::write_alert,
                    (systems::write_console_out, systems::write_center_print)
                        .run_if(resource_changed::<RenderConsoleOutput>),
                    systems::write_console_in.run_if(resource_changed::<RenderConsoleInput>),
                    systems::update_console_visibility.run_if(state_changed::<InputFocus>),
                ),
            )
            .add_systems(
                PostUpdate,
                (systems::write_command_output, systems::send_unhandled_commands_to_server)
                    .after(CommandsSystems),
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

#[derive(Default)]
pub struct ConsoleInputTerminal {
    pub stdout: Vec<u8>,
}

impl io::Write for ConsoleInputTerminal {
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

impl Tty for ConsoleInputTerminal {
    fn next_key(&mut self) -> Option<std::io::Result<lined::Key>> {
        unreachable!("TODO: Remove `next_key` from `lined::Tty`")
    }

    fn width(&self) -> std::io::Result<usize> {
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
    type WordDividerIter = <lined::Context as EditorContext>::WordDividerIter;

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

#[derive(Resource, Default)]
pub struct RenderConsoleInput {
    pub cur_text: String,
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

        Ok(ConsoleInput { editor, keymap: Emacs::new() })
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
struct Timestamp {
    timestamp: i64,
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

#[derive(Resource, Default)]
pub struct RenderConsoleOutput {
    text_chunks: BTreeMap<Timestamp, ConsoleText>,
    center_print: (Timestamp, QString),
}

fn millis_i64(duration: Duration) -> i64 {
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

    fn drain_center_print(&mut self) -> Option<(Timestamp, QString)> {
        self.center_print.take()
    }

    fn drain_unwritten(&mut self) -> impl ExactSizeIterator<Item = (Timestamp, ConsoleText)> + '_ {
        self.unwritten_chunks.drain(..)
    }
}

impl RenderConsoleOutput {
    pub fn text(&self) -> impl Iterator<Item = (i64, &ConsoleText)> + '_ {
        self.text_chunks.iter().map(|(Timestamp { timestamp: k, .. }, v)| (*k, v))
    }

    pub fn center_print(&self, since: Duration) -> Option<QStr<'_>> {
        if self.center_print.0.timestamp >= millis_i64(since) {
            Some(self.center_print.1.reborrow())
        } else {
            None
        }
    }

    /// Return an iterator over lines that have been printed in the last
    /// `interval` of time.
    ///
    /// The iterator yields the oldest results first.
    ///
    /// `max_candidates` specifies the maximum number of lines to consider,
    /// while `max_results` specifies the maximum number of lines that should
    /// be returned.
    pub fn recent(&self, since: i64) -> impl DoubleEndedIterator<Item = (i64, &ConsoleText)> + '_ {
        self.text_chunks
            .range(Timestamp::new(since, 0)..)
            .map(|(Timestamp { timestamp: k, .. }, v)| (*k, v))
    }
}

#[derive(Component, Default)]
struct AlertOutput {
    // Semantically this is an inclusive range, but `RangeInclusive` does not implement `Default`
    timestamp_range: Range<Option<i64>>,
}

#[derive(Resource)]
pub struct ConsoleAlertSettings {
    timeout: Duration,
    max_lines: usize,
}

impl Default for ConsoleAlertSettings {
    fn default() -> Self {
        Self { timeout: Duration::from_secs(3), max_lines: 10 }
    }
}

#[derive(Component)]
struct ConsoleUi;

#[derive(Component)]
struct ConsoleTextOutputUi;

#[derive(Component)]
struct ConsoleTextCenterPrintUi;

#[derive(Component)]
struct ConsoleTextInputUi;

mod systems {
    use crate::{
        client::text::AtlasText,
        common::net::{ClientCmd, ClientMessage, MessageKind},
    };

    use super::*;

    pub mod startup {
        use crate::client::text::{AtlasText, Conchars};

        use super::*;

        pub fn init_alert_output(conchars: Res<Conchars>, mut commands: Commands) {
            const GLYPH_SIZE_MULTIPLIER: f32 = 1.5;

            commands.spawn((
                Node {
                    position_type: PositionType::Absolute,
                    width: Val::Percent(100.),
                    top: Val::Percent(30.),
                    justify_content: JustifyContent::Center,
                    flex_direction: FlexDirection::Column,
                    ..default()
                },
                GlobalZIndex(1),
                AtlasText {
                    text: "".into(),
                    image: conchars.image.clone().into(),
                    layout: conchars.layout.clone(),
                    glyph_size: conchars.glyph_size.map(|s| s * GLYPH_SIZE_MULTIPLIER).into(),
                    line_padding: UiRect { top: Val::Px(4.), ..default() },
                    justify: JustifyContent::Center,
                },
                ConsoleTextCenterPrintUi,
            ));

            let [half_w, half_h] = conchars.glyph_size.map(|s| s / 2.);

            commands.spawn((
                Node {
                    left: half_w,
                    top: half_h,
                    flex_direction: FlexDirection::Column,
                    ..default()
                },
                AtlasText {
                    text: "".into(),
                    image: conchars.image.clone().into(),
                    layout: conchars.layout.clone(),
                    line_padding: UiRect { top: Val::Px(4.), ..default() },
                    glyph_size: conchars.glyph_size.into(),
                    justify: JustifyContent::Center,
                },
                AlertOutput::default(),
            ));
        }

        pub fn init_console(
            conchars: Res<Conchars>,
            mut commands: Commands,
            assets: Res<AssetServer>,
        ) {
            let image = assets.load("gfx/conback.lmp");

            commands.spawn((
                Node {
                    position_type: PositionType::Absolute,
                    width: Val::Percent(100.),
                    height: Val::Percent(30.),
                    overflow: Overflow::clip(),
                    flex_direction: FlexDirection::Column,
                    justify_content: JustifyContent::End,
                    ..default()
                },
                Visibility::Hidden,
                GlobalZIndex(2),
                ConsoleUi,
                children![
                    (
                        ImageNode { image, ..default() },
                        Node {
                            position_type: PositionType::Absolute,
                            width: Val::Vw(100.),
                            ..default()
                        },
                        ZIndex(-1),
                    ),
                    (
                        Node {
                            flex_direction: FlexDirection::Column,
                            flex_wrap: FlexWrap::NoWrap,
                            justify_content: JustifyContent::End,
                            ..default()
                        },
                        children![
                            (
                                Node { flex_direction: FlexDirection::Column, ..default() },
                                AtlasText {
                                    text: "".into(),
                                    image: conchars.image.clone().into(),
                                    layout: conchars.layout.clone(),
                                    glyph_size: conchars.glyph_size.into(),
                                    line_padding: UiRect { top: Val::Px(4.), ..default() },
                                    justify: JustifyContent::FlexStart,
                                },
                                ConsoleTextOutputUi,
                            ),
                            (
                                Node { flex_direction: FlexDirection::Column, ..default() },
                                AtlasText {
                                    text: "] ".into(),
                                    image: conchars.image.clone().into(),
                                    layout: conchars.layout.clone(),
                                    glyph_size: conchars.glyph_size.into(),
                                    line_padding: UiRect { top: Val::Px(4.), ..default() },
                                    justify: JustifyContent::FlexStart,
                                },
                                ConsoleTextInputUi,
                            )
                        ]
                    )
                ],
            ));
        }
    }

    pub fn update_console_visibility(
        mut consoles: Query<&mut Visibility, With<ConsoleUi>>,
        focus: Res<State<InputFocus>>,
    ) {
        for mut vis in consoles.iter_mut() {
            match focus.get() {
                InputFocus::Console => {
                    *vis = Visibility::Visible;
                }
                InputFocus::Game | InputFocus::Menu => {
                    *vis = Visibility::Hidden;
                }
            }
        }
    }

    pub fn send_unhandled_commands_to_server(
        mut unhandled: MessageReader<UnhandledCmd>,
        mut to_server: MessageWriter<ClientMessage>,
        mut console_out: ResMut<ConsoleOutput>,
        time: Res<Time<Real>>,
    ) {
        to_server.write_batch(unhandled.read().map(|UnhandledCmd(e)| {
            // TODO: Receive cmd output from server(?)
            console_out.println(format!("Sending \"{}\" to server...", e.0.name), &time);

            let mut bytes = vec![];
            let _ = ClientCmd::StringCmd { cmd: e.to_string().into() }.serialize(&mut bytes);

            ClientMessage { client_id: 0, packet: bytes.into(), kind: MessageKind::Reliable }
        }));
    }

    pub fn write_command_output(
        mut outputs: MessageReader<CommandOutput>,
        mut console_out: ResMut<ConsoleOutput>,
        time: Res<Time<Real>>,
    ) {
        for CommandOutput { output, output_ty } in outputs.read() {
            match output_ty {
                OutputType::Console => console_out.println(output.as_bytes(), &time),
                OutputType::Alert => console_out.println_alert(output.as_bytes(), &time),
            }
        }
    }

    pub fn update_render_console(
        mut console_out: ResMut<ConsoleOutput>,
        mut render_out: ResMut<RenderConsoleOutput>,
        console_in: Res<ConsoleInput>,
        mut render_in: ResMut<RenderConsoleInput>,
        time: Res<Time<Real>>,
        registry: Res<Registry>,
    ) {
        if let Some(center) = console_out.drain_center_print() {
            render_out.center_print = center;
        }

        let center_time = registry.read_cvar::<f32>("scr_centertime").unwrap_or(2.);
        if !render_out.center_print.1.is_empty()
            && millis_i64(time.elapsed())
                > (render_out.center_print.0.timestamp + (center_time * 1000.) as i64)
        {
            render_out.center_print.1.clear();
        }

        let new_text = console_out.drain_unwritten();
        if new_text.len() > 0 {
            render_out.text_chunks.extend(new_text);
        }

        if !itertools::equal(render_in.cur_text.chars(), console_in.get_text()) {
            render_in.cur_text.clear();
            render_in.cur_text.extend(console_in.get_text());
        }
    }

    pub fn write_console_out(
        console_out: Res<RenderConsoleOutput>,
        mut out_ui: Query<&mut AtlasText, With<ConsoleTextOutputUi>>,
    ) {
        for mut text in out_ui.iter_mut() {
            // TODO: Write only extra lines
            if !text.text.is_empty() {
                text.text.clear();
            }

            for line in console_out.text_chunks.values() {
                text.text.push_bytes(&*line.text);
            }
        }
    }

    pub fn write_center_print(
        console_out: Res<RenderConsoleOutput>,
        mut center_ui: Query<&mut AtlasText, With<ConsoleTextCenterPrintUi>>,
    ) {
        for mut text in center_ui.iter_mut() {
            // TODO: Write only extra lines
            if !text.text.is_empty() {
                text.text.clear();
            }

            if !console_out.center_print.1.is_empty() {
                text.text.push_bytes(&*console_out.center_print.1.raw);
            }
        }
    }

    pub fn write_console_in(
        console_in: Res<RenderConsoleInput>,
        mut in_ui: Query<&mut AtlasText, With<ConsoleTextInputUi>>,
    ) {
        for mut text in in_ui.iter_mut() {
            if console_in.cur_text.as_bytes() == &*text.text.raw {
                continue;
            }

            // TODO: Write only extra lines
            text.text.clear();
            text.text.push_str(&console_in.cur_text);
        }
    }

    pub fn write_alert(
        mut alert: Query<(&mut AtlasText, &mut AlertOutput)>,
        settings: Res<ConsoleAlertSettings>,
        time: Res<Time<Real>>,
        console_out: Res<RenderConsoleOutput>,
    ) {
        let elapsed = millis_i64(time.elapsed());
        let timeout = millis_i64(settings.timeout);

        let since = elapsed - timeout;
        let mut lines = console_out
            .recent(since)
            .filter(|(_, line)| line.output_type == OutputType::Alert)
            .map(|(ts, line)| (ts, &line.text));

        let first = lines.next();
        let first_timestamp = first.map(|(ts, _)| ts);

        let last = lines.next_back();
        let last_timestamp = last.map(|(ts, _)| ts);

        let mut lines = Some(lines);

        let mut cached_lines = Vec::new();

        for (mut text, mut alert) in alert.iter_mut() {
            if alert.timestamp_range.start == first_timestamp
                && alert.timestamp_range.end == last_timestamp
            {
                continue;
            }

            // We do this here in order to avoid reading the iterator and allocating a vec if no alerts need to be updated.
            if let Some(lines) = lines.take() {
                cached_lines.reserve(settings.max_lines);
                // Lines to emit in reverse order. `- 2` because we already popped two items (first and last).
                cached_lines.extend(
                    last.into_iter()
                        .chain(lines.rev().take(settings.max_lines.saturating_sub(2)))
                        .chain(first),
                );
            }

            alert.timestamp_range = first_timestamp..last_timestamp;

            text.text.clear();

            // Take the last N lines and then emit them in the order that they
            // were returned from `recent` - so we need to `.rev` twice.
            for (_, line) in cached_lines.iter().rev() {
                text.text.push_bytes(&*line.raw);
            }
        }
    }
}
