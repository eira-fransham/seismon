use std::{marker::PhantomData, ops::ControlFlow};

use bevy::{asset::LoadState, prelude::*};
use clap::Parser;

use crate::{
    client::NewConnection,
    common::{
        console::{
            AliasInfo, CommandIncomplete, ExecResult, PendingCommands, RegisterCmdExt as _,
            Registry, RunCmd, TextAsset,
        },
        net::{ClientCmd, ClientMessage, MessageKind, ServerMessage, SignOnStage},
    },
    server::Session,
};

use super::{
    Connection, ConnectionStage, ConnectionTarget, DemoQueue, connect,
    input::InputFocus,
    sound::{MixerMessage, MusicSource},
};

pub fn register_commands(app: &mut App) {
    app.action("forward")
        // TODO: Maybe have a way to specify that an input is pressed by default? We can emit a
        // `+mlook` command       but it'd be nice to be able to automatically reset the
        // triggers to their defaults, respecting       the fact that mlook acts
        // differently.
        .action("mlook")
        .action("moveleft")
        .action("back")
        .action("moveright")
        .action("jump")
        .action("lookup")
        .action("left")
        .action("lookdown")
        .action("right")
        .action("attack")
        .action("use");

    app.cvar("mousedelta", "(0 0)", "Mouse delta amount for this frame.");

    #[derive(Parser)]
    #[command(name = "toggleconsole", about = "Open or close the console")]
    struct ToggleConsole;

    // set up overlay/ui toggles
    app.command(
        |In(ToggleConsole),
         conn: Option<Res<Connection>>,
         focus: Res<State<InputFocus>>,
         mut next_focus: ResMut<NextState<InputFocus>>| {
            if conn.is_some() {
                match focus.get() {
                    InputFocus::Menu | InputFocus::Game => next_focus.set(InputFocus::Console),
                    InputFocus::Console => next_focus.set(InputFocus::Game),
                }
            } else {
                match focus.get() {
                    InputFocus::Console => next_focus.set(InputFocus::Menu),
                    InputFocus::Menu => next_focus.set(InputFocus::Console),
                    InputFocus::Game => {
                        unreachable!("Game focus is invalid when we are disconnected")
                    }
                }
            }

            ""
        },
    );

    // TODO: Support server pause
    #[derive(Parser)]
    #[command(name = "pausedemo", about = "Pause/unpause demo playback")]
    struct PauseDemo;

    // set up overlay/ui toggles
    app.command(|In(PauseDemo), mut time: ResMut<Time<Virtual>>| {
        if time.is_paused() {
            time.unpause();
        } else {
            time.pause();
        }

        ExecResult::default()
    });

    #[derive(Parser)]
    #[command(name = "togglemenu", about = "Open or close the menu")]
    struct ToggleMenu;

    app.command(
        |In(ToggleMenu),
         conn: Option<Res<Connection>>,
         focus: Res<State<InputFocus>>,
         mut next_focus: ResMut<NextState<InputFocus>>| {
            if conn.is_some() {
                match focus.get() {
                    InputFocus::Game => next_focus.set(InputFocus::Menu),
                    InputFocus::Console => next_focus.set(InputFocus::Menu),
                    InputFocus::Menu => next_focus.set(InputFocus::Game),
                }
            } else {
                match focus.get() {
                    InputFocus::Console => next_focus.set(InputFocus::Menu),
                    InputFocus::Menu => next_focus.set(InputFocus::Console),
                    InputFocus::Game => {
                        unreachable!("Game focus is invalid when we are disconnected")
                    }
                }
            }
            ExecResult::default()
        },
    );

    #[derive(Parser)]
    #[command(name = "connect", about = "Connect to a remote server")]
    struct Connect {
        remote: String,
    }

    // set up connection console commands
    app.command(|In(Connect { remote }), mut commands: Commands| match connect(&remote) {
        Ok((new_conn, new_state)) => {
            commands.write_message(NewConnection::Server(Some(new_conn), new_state));
            ExecResult::default()
        }
        Err(e) => e.to_string().into(),
    });

    #[derive(Parser)]
    #[command(name = "reconnect", about = "Reconnect to the current server")]
    struct Reconnect;

    app.command(
        |In(Reconnect),
         mut conn: Option<ResMut<Connection>>,
         mut next_focus: ResMut<NextState<InputFocus>>| {
            if let Some(conn) = conn.as_deref_mut()
                && let ConnectionTarget::Server { stage, .. } = &mut conn.target
            {
                // TODO: is this all that's needed to reconnect to a server?
                *stage = ConnectionStage::SignOn(SignOnStage::Prespawn);
                next_focus.set(InputFocus::Game);
                ExecResult::default()
            } else {
                "not connected".into()
            }
        },
    );

    #[derive(Parser)]
    #[command(name = "reconnect", about = "Disconnect from the current server")]
    struct Disconnect;

    app.command(|In(Disconnect), mut commands: Commands, conn: Option<Res<Connection>>| {
        if conn.is_some() {
            commands.write_message(crate::client::Disconnect);
            ExecResult::default()
        } else {
            "not connected".into()
        }
    });

    #[derive(Parser)]
    #[command(name = "playdemo", about = "Play a specific demo")]
    struct PlayDemo {
        demo: String,
    }

    // TODO: Deduplicate this and `StartDemos`
    app.command(
        |In(PlayDemo { demo }),
         mut commands: Commands,
         assets: Res<AssetServer>,
         mut time: ResMut<Time<Virtual>>| {
            // TODO: Commands should be async and this should use `wait_for_asset`
            *time = Time::default();

            // TODO: This should look for demos in root or `/demos`
            commands.write_message(NewConnection::Demo(assets.load(format!("{demo}.dem"))));

            ExecResult::default()
        },
    );

    #[derive(Parser)]
    #[command(name = "startdemos", about = "Play a specific demo")]
    struct StartDemos {
        demos: Vec<String>,
    }

    app.command(
        |In(StartDemos { demos }),
         mut commands: Commands,
         assets: Res<AssetServer>,
         mut time: ResMut<Time<Virtual>>,
         mut demo_queue: ResMut<DemoQueue>,
         server: Option<Res<Session>>| {
            if demos.is_empty() {
                demo_queue.reset();
            } else {
                *demo_queue = DemoQueue::new(demos);
            }

            *time = Time::default();

            // Only actually start playing the demos if we aren't already running a server
            // (this appears to be Quake's expected behaviour?)
            if server.is_none()
                && let Some(demo) = demo_queue.next_demo()
            {
                // TODO: This should look for demos in root or `/demos`
                commands.write_message(NewConnection::Demo(assets.load(format!("{demo}.dem"))));
            }

            ExecResult::default()
        },
    );

    #[derive(Parser)]
    #[command(name = "music", about = "Play a named music track")]
    struct Music {
        #[arg(value_name = "TRACKNAME")]
        track: String,
    }

    app.command(|In(Music { track }), mut events: MessageWriter<MixerMessage>| {
        events.write(MixerMessage::StartMusic(Some(MusicSource::Named(track))));
        ExecResult::default()
    });

    // TODO: Make these subcommands of `music`, with aliases
    #[derive(Parser)]
    #[command(name = "music_stop", about = "Stop the current music track")]
    struct MusicStop;

    app.command(|In(MusicStop), mut events: MessageWriter<MixerMessage>| {
        events.write(MixerMessage::StopMusic);
        ExecResult::default()
    });

    // TODO: Make these subcommands of `music`, with aliases
    #[derive(Parser)]
    #[command(name = "music_pause", about = "Pause playback of the current music track")]
    struct MusicPause;

    app.command(|In(MusicPause), mut events: MessageWriter<MixerMessage>| {
        events.write(MixerMessage::PauseMusic);
        ExecResult::default()
    });

    // TODO: Make these subcommands of `music`, with aliases
    #[derive(Parser)]
    #[command(name = "music_resume", about = "Resume playback of the current music track")]
    struct MusicResume;

    app.command(|In(MusicResume), mut events: MessageWriter<MixerMessage>| {
        events.write(MixerMessage::StartMusic(None));
        ExecResult::default()
    });

    #[derive(Parser)]
    #[command(name = "echo", about = "Echo to the console")]
    struct Echo {
        args: Vec<String>,
    }

    app.command(|In(Echo { args })| -> ExecResult { args.join(" ").trim().to_owned().into() });

    #[derive(Parser)]
    #[command(name = "alias", about = "Make an alias command")]
    struct Alias {
        alias_name: Option<String>,
        commands: Option<String>,
    }

    app.command(|In(Alias { alias_name, commands }), mut registry: ResMut<Registry>| {
        match (alias_name, commands) {
            (None, None) => {
                let aliases = registry.aliases();

                // TODO: There's probably a better way to do this
                let mut count = 0;
                let alias_text = aliases.flat_map(|AliasInfo { name, target, help: _ }| {
                    count += 1;
                    ["    ", name, ": ", target, "\n"]
                });
                let mut out = String::new();
                for text in alias_text {
                    out.push_str(text);
                }
                out.push_str(&count.to_string());
                out.push_str("alias command(s)");

                out.into()
            }

            (Some(from), Some(to)) => {
                registry.alias(from, to);

                ExecResult::default()
            }

            // TODO: Alias info for just one alias
            _ => String::new().into(),
        }
    });

    #[derive(Parser)]
    #[command(name = "find", about = "Find a command by name")]
    struct Find {
        pattern: String,
    }

    app.command(move |In(Find { pattern }), cmds: Res<Registry>| -> ExecResult {
        // Take every item starting with the target.
        let it = cmds
            .all_names()
            .skip_while(|item| !item.starts_with(&pattern))
            .take_while(|item| item.starts_with(&pattern))
            .collect::<Vec<_>>()
            .join("\n");

        it.into()
    });

    #[derive(Parser)]
    #[command(name = "exec", about = "Execute a cfg file")]
    struct Exec {
        #[arg(required = true)]
        cfgs: Vec<String>,
    }

    app.command(move |In(Exec { cfgs }), world: &mut World| {
        fn continuation(
            // Have to double-box because we can't downcast `Box<dyn Any>` to `Box<[..]>`
            // Handles are in reverse order to allow `pop`
            In(mut cfg_handles): In<Box<Vec<Handle<TextAsset>>>>,
            asset_server: Res<AssetServer>,
            assets: Res<Assets<TextAsset>>,
            mut pending_commands: ResMut<PendingCommands>,
        ) -> ControlFlow<ExecResult, Box<Vec<Handle<TextAsset>>>> {
            while let Some(cfg) = cfg_handles.pop() {
                match asset_server.load_state(&cfg) {
                    LoadState::Loaded => {}
                    LoadState::Loading => {
                        cfg_handles.push(cfg);

                        return ControlFlow::Continue(cfg_handles);
                    }
                    LoadState::NotLoaded => {
                        return ControlFlow::Break(
                            "Failed to exec file: load not initiated correctly".into(),
                        );
                    }
                    LoadState::Failed(asset_load_error) => {
                        return ControlFlow::Break(
                            format!("Failed to exec file: {asset_load_error}").into(),
                        );
                    }
                }

                let script_file =
                    assets.get(&cfg).expect("Asset marked loaded but it was not found");

                match RunCmd::parse_many(&script_file.text) {
                    Ok(commands) => {
                        for cmd in commands.into_iter().rev() {
                            pending_commands.0.push_front(cmd.into_owned());
                        }
                    }
                    Err(e) => {
                        return ControlFlow::Break(format!("Couldn't parse file: {e}").into());
                    }
                }
            }

            ControlFlow::Break("".into())
        }

        let asset_server = world.resource::<AssetServer>();
        // Reversed order so we can `pop` in the continuation
        let cfg_handles = cfgs.iter().rev().map(|cfg| asset_server.load(cfg)).collect::<Vec<_>>();

        CommandIncomplete {
            context: Box::new(cfg_handles),
            system: continuation,
            marker: PhantomData,
        }
    });

    // #[derive(Parser)]
    // #[command(name = "bf", about = "Flash the screen")]
    // struct Bf;

    // app.command(
    //     move |In(Bf), conn: Option<ResMut<Connection>>| -> ExecResult {
    //         const BF_DECAY: f32 = 100.;

    //         if let Some(mut conn) = conn {
    //             conn.client_state.color_shifts[ColorShiftCode::Bonus as usize] = ColorShift {
    //                 dest_color: [215, 186, 69],
    //                 density: 0.5,
    //                 decay: BF_DECAY,
    //             };
    //         }
    //         default()
    //     },
    // );

    // #[derive(Parser, Debug, Copy, Clone)]
    // #[command(name = "v_cshift", about = "Colored form of `bf`")]
    // struct CShift {
    //     r: f32,
    //     g: f32,
    //     b: f32,
    //     #[arg(default_value_t = 1.)]
    //     density: f32,
    //     #[arg(default_value_t = 0.)]
    //     decay: f32,
    // }

    // app.command(
    //     move |In(CShift {
    //               r,
    //               g,
    //               b,
    //               density,
    //               decay,
    //           }),
    //           conn: Option<ResMut<Connection>>|
    //           -> ExecResult {
    //         if let Some(mut conn) = conn {
    //             conn.client_state.color_shifts[ColorShiftCode::Custom as usize] = ColorShift {
    //                 dest_color: [r, g, b].map(|v| v.clamp(0., u8::MAX as _) as u8),
    //                 density: density / u8::MAX as f32,
    //                 decay,
    //             };
    //         }
    //         default()
    //     },
    // );

    #[derive(Parser)]
    #[command(name = "name", about = "Set the player name")]
    struct Name {
        new_name: String,
    }

    app.command(move |In(Name { new_name }), mut registry: ResMut<Registry>| -> ExecResult {
        match registry.set_cvar_raw("_cl_name", new_name.into()) {
            Ok(_) => default(),
            Err(e) => format!("Error: {e}").into(),
        }
    });

    #[derive(Parser)]
    #[command(name = "map", about = "Load and start a new map")]
    struct Map {
        map_name: String,
    }

    // TODO: Can probably do this better but it's also possibly fine because it's a system
    fn cmd_map(
        In(Map { map_name }): In<Map>,
        mut commands: Commands,
        mut client_events: ResMut<Messages<ClientMessage>>,
        mut server_events: ResMut<Messages<ServerMessage>>,
    ) -> ExecResult {
        client_events.clear();
        server_events.clear();

        let mut bytes = vec![];
        ClientCmd::StringCmd { cmd: format!("map {map_name}").into() }
            .serialize(&mut bytes)
            .unwrap();

        client_events.write(ClientMessage {
            client_id: 0,
            packet: bytes.into(),
            kind: MessageKind::Reliable,
        });

        // TODO: This should not be handled here, server and client should be decoupled
        commands.write_message(NewConnection::Server(
            None,
            ConnectionStage::SignOn(SignOnStage::Prespawn),
        ));

        Default::default()
    }

    app.command(cmd_map);
}
