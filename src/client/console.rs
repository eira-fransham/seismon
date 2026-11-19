//! Rendering and client-side behaviour of the console.
//!
//! The console core (command execution, line editing, output buffering) lives in
//! [`crate::common::console`].

use std::{collections::BTreeMap, ops::Range, time::Duration};

use bevy::prelude::*;
use bevy_mod_commands::{CommandOutput, CommandsSystems, OutputType, Registry, UnhandledCmd};
use seismon_utils::{QStr, QString};

use crate::{
    client::{Disconnect, NewConnection, input::InputFocus},
    common::console::{
        ConsoleInput, ConsoleOutput, ConsoleText, SeismonConsoleCorePlugin, Timestamp, millis_i64,
    },
};

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

#[derive(Resource, Default)]
pub struct RenderConsoleInput {
    pub cur_text: String,
}

#[derive(Resource, Default)]
pub struct RenderConsoleOutput {
    text_chunks: BTreeMap<Timestamp, ConsoleText>,
    center_print: (Timestamp, QString),
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
