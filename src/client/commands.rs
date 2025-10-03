use std::io::Read as _;

use bevy::prelude::*;
use clap::Parser;

use crate::{
    common::{
        console::{AliasInfo, ExecResult, RegisterCmdExt as _, Registry, RunCmd},
        net::{ColorShift, QSocket, SignOnStage},
        vfs::Vfs,
    },
    server::Session,
};

use super::{
    ColorShiftCode, Connection, ConnectionKind, ConnectionState, DemoQueue, connect,
    demo::DemoServer,
    input::InputFocus,
    sound::{MixerEvent, MusicSource},
    state::ClientState,
};

pub fn register_commands(app: &mut App) {
    app.action("forward")
        // TODO: Maybe have a way to specify that an input is pressed by default? We can emit a `+mlook` command
        //       but it'd be nice to be able to automatically reset the triggers to their defaults, respecting
        //       the fact that mlook acts differently.
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
        |In(ToggleConsole), conn: Option<Res<Connection>>, mut focus: ResMut<InputFocus>| {
            if conn.is_some() {
                match &*focus {
                    InputFocus::Menu | InputFocus::Game => *focus = InputFocus::Console,
                    InputFocus::Console => *focus = InputFocus::Game,
                }
            } else {
                match &*focus {
                    InputFocus::Console => *focus = InputFocus::Menu,
                    InputFocus::Menu => *focus = InputFocus::Console,
                    InputFocus::Game => {
                        unreachable!("Game focus is invalid when we are disconnected")
                    }
                }
            }

            default()
        },
    );

    #[derive(Parser)]
    #[command(name = "togglemenu", about = "Open or close the menu")]
    struct ToggleMenu;

    app.command(
        |In(ToggleMenu), conn: Option<Res<Connection>>, mut focus: ResMut<InputFocus>| {
            if conn.is_some() {
                match &*focus {
                    InputFocus::Game => *focus = InputFocus::Menu,
                    InputFocus::Console => *focus = InputFocus::Menu,
                    InputFocus::Menu => *focus = InputFocus::Game,
                }
            } else {
                match &*focus {
                    InputFocus::Console => *focus = InputFocus::Menu,
                    InputFocus::Menu => *focus = InputFocus::Console,
                    InputFocus::Game => {
                        unreachable!("Game focus is invalid when we are disconnected")
                    }
                }
            }
            default()
        },
    );

    #[derive(Parser)]
    #[command(name = "connect", about = "Connect to a remote server")]
    struct Connect {
        remote: String,
    }

    // set up connection console commands
    app.command(
        |In(Connect { remote }), mut commands: Commands, mut focus: ResMut<InputFocus>| {
            match connect(&remote) {
                Ok((new_conn, new_state)) => {
                    *focus = InputFocus::Game;
                    commands.insert_resource(new_conn);
                    commands.insert_resource(Connection::new_server());
                    commands.insert_resource(new_state);
                    default()
                }
                Err(e) => e.to_string().into(),
            }
        },
    );

    #[derive(Parser)]
    #[command(name = "reconnect", about = "Reconnect to the current server")]
    struct Reconnect;

    app.command(
        |In(Reconnect),
         conn: Option<Res<Connection>>,
         mut conn_state: ResMut<ConnectionState>,
         mut focus: ResMut<InputFocus>| {
            if conn.is_some() {
                // TODO: clear client state
                *conn_state = ConnectionState::SignOn(SignOnStage::Prespawn);
                *focus = InputFocus::Game;
                default()
            } else {
                // TODO: log message, e.g. "can't reconnect while disconnected"
                "not connected".into()
            }
        },
    );

    #[derive(Parser)]
    #[command(name = "reconnect", about = "Disconnect from the current server")]
    struct Disconnect;

    app.command(
        |In(Disconnect),
         mut commands: Commands,
         conn: Option<Res<Connection>>,
         mut focus: ResMut<InputFocus>| {
            if conn.is_some() {
                commands.remove_resource::<Connection>();
                commands.remove_resource::<QSocket>();
                *focus = InputFocus::Console;
                default()
            } else {
                "not connected".into()
            }
        },
    );

    #[derive(Parser)]
    #[command(name = "playdemo", about = "Play a specific demo")]
    struct PlayDemo {
        demo: String,
    }

    app.command(
        |In(PlayDemo { demo }),
         mut commands: Commands,
         vfs: Res<Vfs>,
         mut focus: ResMut<InputFocus>,
         mut conn_state: ResMut<ConnectionState>| {
            let (new_conn, new_state) = {
                let mut demo_file = match vfs.open(format!("{demo}.dem")) {
                    Ok(f) => f,
                    Err(e) => {
                        return e.to_string().into();
                    }
                };

                match DemoServer::new(&mut demo_file) {
                    Ok(d) => (
                        Connection {
                            kind: ConnectionKind::Demo(d),
                            state: ClientState::new(),
                        },
                        ConnectionState::SignOn(SignOnStage::Prespawn),
                    ),
                    Err(e) => {
                        return e.to_string().into();
                    }
                }
            };

            *focus = InputFocus::Game;

            commands.insert_resource(new_conn);
            *conn_state = new_state;

            default()
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
         vfs: Res<Vfs>,
         mut demo_queue: ResMut<DemoQueue>,
         mut focus: ResMut<InputFocus>,
         mut conn_state: ResMut<ConnectionState>,
         server: Option<Res<Session>>| {
            if !demos.is_empty() {
                *demo_queue = DemoQueue::new(demos);
            } else {
                demo_queue.reset();
            }

            // Only actually start playing the demos if we aren't already running a server
            // (this appears to be Quake's expected behaviour?)
            if server.is_none() {
                let (new_conn, new_state) = match demo_queue.next_demo() {
                    Some(demo) => {
                        let mut demo_file = match vfs
                            .open(format!("{demo}.dem"))
                            .or_else(|_| vfs.open(format!("demos/{demo}.dem")))
                        {
                            Ok(f) => f,
                            Err(e) => {
                                // log the error, dump the demo queue and disconnect
                                return e.to_string().into();
                            }
                        };

                        match DemoServer::new(&mut demo_file) {
                            Ok(d) => (
                                Connection {
                                    kind: ConnectionKind::Demo(d),
                                    state: ClientState::new(),
                                },
                                ConnectionState::SignOn(SignOnStage::Prespawn),
                            ),
                            Err(e) => {
                                return e.to_string().into();
                            }
                        }
                    }
                    None => return "".into(),
                };

                commands.insert_resource(new_conn);
                *conn_state = new_state;
                *focus = InputFocus::Game;
            }

            default()
        },
    );

    #[derive(Parser)]
    #[command(name = "music", about = "Play a named music track")]
    struct Music {
        #[arg(value_name = "TRACKNAME")]
        track: String,
    }

    app.command(|In(Music { track }), mut events: EventWriter<MixerEvent>| {
        events.write(MixerEvent::StartMusic(Some(MusicSource::Named(track))));
        default()
    });

    // TODO: Make these subcommands of `music`, with aliases
    #[derive(Parser)]
    #[command(name = "music_stop", about = "Stop the current music track")]
    struct MusicStop;

    app.command(|In(MusicStop), mut events: EventWriter<MixerEvent>| {
        events.write(MixerEvent::StopMusic);
        default()
    });

    // TODO: Make these subcommands of `music`, with aliases
    #[derive(Parser)]
    #[command(
        name = "music_pause",
        about = "Pause playback of the current music track"
    )]
    struct MusicPause;

    app.command(|In(MusicPause), mut events: EventWriter<MixerEvent>| {
        events.write(MixerEvent::PauseMusic);
        default()
    });

    // TODO: Make these subcommands of `music`, with aliases
    #[derive(Parser)]
    #[command(
        name = "music_resume",
        about = "Resume playback of the current music track"
    )]
    struct MusicResume;

    app.command(|In(MusicResume), mut events: EventWriter<MixerEvent>| {
        events.write(MixerEvent::StartMusic(None));
        default()
    });

    #[derive(Parser)]
    #[command(name = "echo", about = "Echo to the console")]
    struct Echo {
        args: Vec<String>,
    }

    app.command(|In(Echo { args })| args.join(" ").trim().to_owned().into());

    #[derive(Parser)]
    #[command(name = "alias", about = "Make an alias command")]
    struct Alias {
        alias_name: Option<String>,
        commands: Option<String>,
    }

    app.command(
        |In(Alias {
             alias_name,
             commands,
         }),
         mut registry: ResMut<Registry>| {
            match (alias_name, commands) {
                (None, None) => {
                    let aliases = registry.aliases();

                    // TODO: There's probably a better way to do this
                    let mut count = 0;
                    let alias_text = aliases.flat_map(
                        |AliasInfo {
                             name,
                             target,
                             help: _,
                         }| {
                            count += 1;
                            ["    ", name, ": ", target, "\n"]
                        },
                    );
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

                    default()
                }

                // TODO: Alias info for just one alias
                _ => String::new().into(),
            }
        },
    );

    #[derive(Parser)]
    #[command(name = "find", about = "Find a command by name")]
    struct Find {
        pattern: String,
    }

    app.command(move |In(Find { pattern }), cmds: Res<Registry>| {
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

    app.command(move |In(Exec { cfgs }), vfs: Res<Vfs>| {
        let mut script = String::new();

        for cfg in &cfgs {
            let mut script_file = match vfs.open(cfg) {
                Ok(s) => s,
                Err(e) => {
                    return ExecResult {
                        output: format!("Couldn't exec {cfg}: {e}").into(),
                        ..default()
                    };
                }
            };

            // TODO: Error handling
            script_file.read_to_string(&mut script).unwrap();
            script.push('\n');
        }

        let script = match RunCmd::parse_many(&script) {
            Ok(commands) => commands,
            Err(e) => {
                return ExecResult {
                    output: format!("Couldn't exec: {e}").into(),
                    ..default()
                };
            }
        };

        let extra_commands = Box::new(
            script
                .into_iter()
                .map(RunCmd::into_owned)
                .collect::<Vec<_>>()
                .into_iter(),
        );

        ExecResult {
            extra_commands,
            ..default()
        }
    });

    #[derive(Parser)]
    #[command(name = "bf", about = "Flash the screen")]
    struct Bf;

    app.command(
        move |In(Bf), conn: Option<ResMut<Connection>>| -> ExecResult {
            const BF_DECAY: f32 = 100.;

            if let Some(mut conn) = conn {
                conn.state.color_shifts[ColorShiftCode::Bonus as usize] = ColorShift {
                    dest_color: [215, 186, 69],
                    density: 0.5,
                    decay: BF_DECAY,
                };
            }
            default()
        },
    );

    #[derive(Parser, Debug, Copy, Clone)]
    #[command(name = "v_cshift", about = "Colored form of `bf`")]
    struct CShift {
        r: f32,
        g: f32,
        b: f32,
        #[arg(default_value_t = 1.)]
        density: f32,
        #[arg(default_value_t = 0.)]
        decay: f32,
    }

    app.command(
        move |In(CShift {
                  r,
                  g,
                  b,
                  density,
                  decay,
              }),
              conn: Option<ResMut<Connection>>|
              -> ExecResult {
            if let Some(mut conn) = conn {
                conn.state.color_shifts[ColorShiftCode::Custom as usize] = ColorShift {
                    dest_color: [r, g, b].map(|v| v.clamp(0., u8::MAX as _) as u8),
                    density: density / u8::MAX as f32,
                    decay,
                };
            }
            default()
        },
    );

    #[derive(Parser)]
    #[command(name = "name", about = "Set the player name")]
    struct Name {
        new_name: String,
    }

    app.command(
        move |In(Name { new_name }), mut registry: ResMut<Registry>| -> ExecResult {
            match registry.set_cvar_raw("_cl_name", new_name.into()) {
                Ok(_) => default(),
                Err(e) => format!("Error: {e}").into(),
            }
        },
    );
}
