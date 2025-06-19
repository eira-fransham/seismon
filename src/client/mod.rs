// Copyright © 2020 Cormac O'Brien
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

pub mod commands;
mod cvars;
pub mod demo;
pub mod entity;
pub mod input;
pub mod menu;
pub mod render;
pub mod sound;
pub mod state;
pub mod trace;
pub mod view;

use self::{
    input::SeismonInputPlugin,
    menu::{MenuBodyView, MenuBuilder, MenuView},
    render::{RenderResolution, SeismonRenderPlugin},
    sound::{MixerEvent, SeismonSoundPlugin},
};

use std::{iter, mem, net::ToSocketAddrs, ops::Range, path::PathBuf};

use crate::{
    client::{
        demo::{DemoServer, DemoServerError},
        entity::{ClientEntity, MAX_STATIC_ENTITIES},
        sound::{MusicPlayer, StartSound, StartStaticSound, StopSound},
        state::{ClientState, PlayerInfo},
        trace::{TraceEntity, TraceFrame},
        view::{IdleVars, KickVars, MouseVars, RollVars},
    },
    common::{
        self,
        console::{ConsoleError, ConsoleOutput, RunCmd, SeismonConsolePlugin},
        engine,
        model::{Model, ModelError},
        net::{
            self,
            connect::{ConnectSocket, Request, Response, CONNECT_PROTOCOL_VERSION},
            BlockingMode, ClientCmd, ClientMessage, ClientStat, EntityEffects, EntityState,
            GameType, NetError, PlayerColor, QSocket, ServerCmd, ServerMessage, SignOnStage,
        },
        util::QString,
        vfs::{Vfs, VfsError},
    },
};
use cgmath::{Deg, Vector3};

use bevy::{
    asset::AssetServer,
    ecs::{
        event::{EventCursor, EventWriter},
        system::{Res, ResMut, Resource},
    },
    prelude::*,
    render::extract_resource::ExtractResource,
    time::{Time, Virtual},
    window::PrimaryWindow,
};
use chrono::Duration;
use input::InputFocus;
use menu::Menu;
use num_derive::FromPrimitive;
use serde::Deserialize;
use sound::SoundError;
use thiserror::Error;
use view::BobVars;

// connections are tried 3 times, see
// https://github.com/id-Software/Quake/blob/master/WinQuake/net_dgrm.c#L1248
const MAX_CONNECT_ATTEMPTS: usize = 3;
const MAX_STATS: usize = 32;

const DEFAULT_SOUND_PACKET_VOLUME: u8 = 255;
const DEFAULT_SOUND_PACKET_ATTENUATION: f32 = 1.0;

const CONSOLE_DIVIDER: &str = "\
\n\n\
\x1D\x1E\x1E\x1E\x1E\x1E\x1E\x1E\
\x1E\x1E\x1E\x1E\x1E\x1E\x1E\x1E\
\x1E\x1E\x1E\x1E\x1E\x1E\x1E\x1E\
\x1E\x1E\x1E\x1E\x1E\x1E\x1E\x1F\
\n\n";

#[derive(Default)]
pub struct SeismonClientPlugin<
    F = Box<dyn Fn(MenuBuilder) -> Result<Menu, failure::Error> + Send + Sync + 'static>,
> {
    pub base_dir: Option<PathBuf>,
    pub game: Option<String>,
    pub main_menu: F,
}

fn build_default(builder: MenuBuilder) -> Result<Menu, failure::Error> {
    Ok(builder.build(MenuView {
        draw_plaque: true,
        title_path: "gfx/ttl_main.lmp".into(),
        body: MenuBodyView::Predefined {
            path: "gfx/mainmenu.lmp".into(),
        },
    }))
}

impl SeismonClientPlugin {
    pub fn new() -> Self {
        Self {
            base_dir: None,
            game: None,
            main_menu: Box::new(build_default),
        }
    }
}

#[derive(Clone, Resource, ExtractResource)]
pub struct SeismonGameSettings {
    pub base_dir: PathBuf,
    pub game: Option<String>,
}

impl<F> Plugin for SeismonClientPlugin<F>
where
    F: Fn(MenuBuilder) -> Result<Menu, failure::Error> + Clone + Send + Sync + 'static,
{
    fn build(&self, app: &mut bevy::prelude::App) {
        if let Ok(menu) = (self.main_menu)(MenuBuilder::new(app.world_mut())) {
            app.insert_resource(menu);
        }

        let app = app
            .insert_resource(SeismonGameSettings {
                base_dir: self
                    .base_dir
                    .clone()
                    .unwrap_or_else(|| common::default_base_dir()),
                game: self.game.clone(),
            })
            .init_resource::<Vfs>()
            .init_resource::<MusicPlayer>()
            .init_resource::<DemoQueue>()
            .add_event::<Impulse>()
            .add_event::<ClientMessage>()
            .add_event::<ServerMessage>()
            // TODO: Use bevy's state system
            .insert_resource(ConnectionState::SignOn(SignOnStage::Not))
            .add_systems(
                Main,
                (
                    systems::set_resolution.run_if(any_with_component::<PrimaryWindow>),
                    systems::handle_input.pipe(|In(res)| {
                        // TODO: Error handling
                        if let Err(e) = res {
                            error!("Error handling input: {}", e);
                        }
                    }),
                    systems::frame.pipe(|In(res)| {
                        // TODO: Error handling
                        if let Err(e) = res {
                            error!("Error handling frame: {}", e);
                        }
                    }),
                    systems::process_network_messages
                        .pipe(|In(res)| {
                            // TODO: Error handling
                            if let Err(e) = res {
                                error!("Error handling frame: {}", e);
                            }
                        })
                        .run_if(resource_exists::<QSocket>),
                ),
            )
            .add_plugins(SeismonConsolePlugin)
            .add_plugins(SeismonRenderPlugin)
            .add_plugins(SeismonSoundPlugin)
            .add_plugins(SeismonInputPlugin);

        cvars::register_cvars(app);
        commands::register_commands(app);
    }

    fn finish(&self, app: &mut bevy::prelude::App) {
        app.init_resource::<RenderResolution>();
    }
}

#[derive(Error, Debug)]
pub enum ClientError {
    #[error("Connection rejected: {0}")]
    ConnectionRejected(String),
    #[error("Couldn't read cvar value: {0}")]
    Cvar(ConsoleError),
    #[error("Server sent an invalid port number ({0})")]
    InvalidConnectPort(i32),
    #[error("Server sent an invalid connect response")]
    InvalidConnectResponse,
    #[error("Invalid server address")]
    InvalidServerAddress,
    #[error("No response from server")]
    NoResponse,
    #[error("Unrecognized protocol: {0}")]
    UnrecognizedProtocol(i32),
    #[error("Client is not connected")]
    NotConnected,
    #[error("Client has already signed on")]
    AlreadySignedOn,
    #[error("No client with ID {0}")]
    NoSuchClient(usize),
    #[error("No player with ID {0}")]
    NoSuchPlayer(usize),
    #[error("No entity with ID {0}")]
    NoSuchEntity(usize),
    #[error("Null entity access")]
    NullEntity,
    #[error("Entity already exists: {0}")]
    EntityExists(usize),
    #[error("Invalid view entity: {0}")]
    InvalidViewEntity(usize),
    #[error("Too many static entities")]
    TooManyStaticEntities,
    #[error("No such lightmap animation: {0}")]
    NoSuchLightmapAnimation(usize),
    // TODO: wrap PlayError
    #[error("Failed to open audio output stream")]
    OutputStream,
    #[error("Demo server error: {0}")]
    DemoServer(#[from] DemoServerError),
    #[error("Model error: {0}")]
    Model(#[from] ModelError),
    #[error("Network error: {0}")]
    Network(#[from] NetError),
    #[error("Failed to load sound: {0}")]
    Sound(#[from] SoundError),
    #[error("Virtual filesystem error: {0}")]
    Vfs(#[from] VfsError),
}

impl From<ConsoleError> for ClientError {
    fn from(value: ConsoleError) -> Self {
        Self::Cvar(value)
    }
}

#[derive(Deserialize, Copy, Clone, Debug)]
pub struct MoveVars {
    #[serde(rename(deserialize = "cl_anglespeedkey"))]
    cl_anglespeedkey: f32,
    #[serde(rename(deserialize = "cl_pitchspeed"))]
    cl_pitchspeed: f32,
    #[serde(rename(deserialize = "cl_yawspeed"))]
    cl_yawspeed: f32,
    #[serde(rename(deserialize = "cl_sidespeed"))]
    cl_sidespeed: f32,
    #[serde(rename(deserialize = "cl_upspeed"))]
    cl_upspeed: f32,
    #[serde(rename(deserialize = "cl_forwardspeed"))]
    cl_forwardspeed: f32,
    #[serde(rename(deserialize = "cl_backspeed"))]
    cl_backspeed: f32,
    #[serde(rename(deserialize = "cl_movespeedkey"))]
    cl_movespeedkey: f32,
}

#[derive(Debug, FromPrimitive)]
enum ColorShiftCode {
    Contents = 0,
    Damage = 1,
    Bonus = 2,
    Powerup = 3,
}

struct ServerInfo {
    _max_clients: u8,
    _game_type: GameType,
}

#[derive(Clone, Debug)]
pub enum IntermissionKind {
    Intermission,
    Finale { text: QString },
    Cutscene { text: QString },
}

/// Indicates to the client what should be done with the current connection.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum ConnectionStatus {
    /// Maintain the connection.
    Maintain,

    /// Disconnect from the server or demo server.
    Disconnect,

    /// Play the next demo in the demo queue.
    NextDemo,
}

#[derive(Clone, Debug)]
pub struct ConnectedState {
    pub model_precache: im::Vector<Model>,
    pub worldmodel_id: usize,
}

/// Indicates the state of an active connection.
#[derive(Resource, Debug, ExtractResource, Clone)]
pub enum ConnectionState {
    /// The client is in the sign-on process.
    SignOn(SignOnStage),

    /// The client is fully connected.
    Connected(ConnectedState),
}

/// Possible targets that a client can be connected to.
enum ConnectionKind {
    /// A regular Quake server.
    Server {
        /// The socket used to communicate with the server.
        reader: EventCursor<ServerMessage>,

        /// The client's packet composition buffer.
        compose: Vec<u8>,
    },

    /// A demo server.
    Demo(DemoServer),
}

struct ServerUpdate {
    message: Vec<u8>,
    angles: Option<Vector3<Deg<f32>>>,
    track_override: Option<u32>,
}

impl Default for ServerUpdate {
    fn default() -> Self {
        Self {
            message: (&[][..]).into(),
            angles: None,
            track_override: None,
        }
    }
}

impl ConnectionKind {
    fn recv(
        &mut self,
        events: &Events<ServerMessage>,
    ) -> Result<Option<ServerUpdate>, ClientError> {
        match self {
            Self::Server { reader, .. } => {
                let mut out = Vec::new();
                for ServerMessage { client_id, packet } in reader.read(events) {
                    // TODO: Actually use correct client id
                    if *client_id == 0 {
                        out.extend(packet);
                    }
                }

                Ok(Some(ServerUpdate {
                    message: out,
                    ..default()
                }))
            }
            Self::Demo(demo_srv) => {
                let track_override = demo_srv.track_override();
                let msg_view = match demo_srv.next() {
                    Some(v) => v,
                    None => {
                        // if there are no commands left in the demo, play
                        // the next demo if there is one
                        return Ok(None);
                    }
                };

                let mut view_angles = msg_view.view_angles();
                // invert entity angles to get the camera direction right.
                // yaw is already inverted.
                view_angles.z = -view_angles.z;

                // TODO: we shouldn't have to copy the message here
                Ok(Some(ServerUpdate {
                    message: msg_view.message().into(),
                    angles: Some(view_angles),
                    track_override,
                }))
            }
        }
    }

    fn is_demo(&self) -> bool {
        match self {
            Self::Demo(_) => true,
            Self::Server { .. } => false,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ClientVars<'a> {
    pub name: &'a str,
    pub color: u8,
}

/// A connection to a game server of some kind.
///
/// The exact nature of the connected server is specified by [`ConnectionKind`].
#[derive(Resource)]
pub struct Connection {
    state: ClientState,
    kind: ConnectionKind,
}

impl Connection {
    pub fn new_server() -> Self {
        Self {
            state: default(),
            kind: ConnectionKind::Server {
                reader: default(),
                compose: default(),
            },
        }
    }
}

impl Connection {
    pub fn view_entity_id(&self) -> usize {
        self.state.view_entity_id()
    }

    pub fn trace<'a, I>(&self, entity_ids: I) -> Result<TraceFrame, ClientError>
    where
        I: IntoIterator<Item = &'a usize>,
    {
        let mut trace = TraceFrame {
            msg_times_ms: [
                self.state.msg_times[0].num_milliseconds(),
                self.state.msg_times[1].num_milliseconds(),
            ],
            time_ms: self.state.time.num_milliseconds(),
            lerp_factor: self.state.lerp_factor,
            entities: default(),
        };

        for id in entity_ids.into_iter() {
            let ent = &self.state.entities[*id];

            let msg_origins = [ent.msg_origins[0].into(), ent.msg_origins[1].into()];
            let msg_angles_deg = [
                [
                    ent.msg_angles[0][0].0,
                    ent.msg_angles[0][1].0,
                    ent.msg_angles[0][2].0,
                ],
                [
                    ent.msg_angles[1][0].0,
                    ent.msg_angles[1][1].0,
                    ent.msg_angles[1][2].0,
                ],
            ];

            trace.entities.insert(
                *id as u32,
                TraceEntity {
                    msg_origins,
                    msg_angles_deg,
                    origin: ent.origin.into(),
                },
            );
        }

        Ok(trace)
    }

    fn handle_signon(
        &mut self,
        client_vars: &ClientVars,
        mut state: Mut<ConnectionState>,
        new_stage: SignOnStage,
    ) -> Result<(), ClientError> {
        use SignOnStage::*;

        let new_conn_state = match &*state {
            // TODO: validate stage transition
            ConnectionState::SignOn(_) => {
                if let ConnectionKind::Server {
                    ref mut compose, ..
                } = self.kind
                {
                    match new_stage {
                        Not => (), // TODO this is an error (invalid value)
                        Prespawn => {
                            ClientCmd::StringCmd {
                                cmd: String::from("prespawn"),
                            }
                            .serialize(compose)?;
                        }
                        ClientInfo => {
                            // TODO: fill in client info here
                            ClientCmd::StringCmd {
                                cmd: format!("name \"{}\"\n", &client_vars.name),
                            }
                            .serialize(compose)?;
                            ClientCmd::StringCmd {
                                cmd: format!(
                                    "color {} {}",
                                    client_vars.color >> 4,
                                    client_vars.color & ((1 << 4) - 1)
                                ),
                            }
                            .serialize(compose)?;
                            // TODO: need default spawn parameters?
                            ClientCmd::StringCmd {
                                cmd: format!("spawn {}", ""),
                            }
                            .serialize(compose)?;
                        }
                        Begin => {
                            ClientCmd::StringCmd {
                                cmd: String::from("begin"),
                            }
                            .serialize(compose)?;
                        }
                        Done => {
                            debug!("SignOn complete");
                            // TODO: end load screen
                            self.state.start_time = self.state.time;
                        }
                    }
                }
                match new_stage {
                    // TODO proper error
                    Not => panic!("SignOnStage::Not in handle_signon"),
                    // still signing on, advance to the new stage
                    Prespawn | ClientInfo | Begin => ConnectionState::SignOn(new_stage),

                    // finished signing on, build world renderer
                    Done => ConnectionState::Connected(ConnectedState {
                        model_precache: self.state.models().clone(),
                        worldmodel_id: self.state.worldmodel_id,
                    }),
                }
            }

            // ignore spurious sign-on messages
            ConnectionState::Connected { .. } => return Ok(()),
        };

        *state = new_conn_state;

        Ok(())
    }

    fn parse_server_msg(
        &mut self,
        mut state: Mut<ConnectionState>,
        time: Time,
        vfs: &Vfs,
        asset_server: &AssetServer,
        server_events: &Events<ServerMessage>,
        mixer_events: &mut EventWriter<MixerEvent>,
        console_commands: &mut EventWriter<RunCmd<'static>>,
        mut console_output: Mut<ConsoleOutput>,
        kick_vars: KickVars,
        client_vars: ClientVars,
    ) -> Result<ConnectionStatus, ClientError> {
        use ConnectionStatus::*;

        let time = Duration::from_std(time.elapsed()).unwrap();

        if self.state.time < self.state.msg_times[0] {
            return Ok(Maintain);
        }

        let Some(ServerUpdate {
            message,
            angles: demo_view_angles,
            track_override,
        }) = self.kind.recv(server_events)?
        else {
            return if self.kind.is_demo() {
                Ok(NextDemo)
            } else {
                Ok(Maintain)
            };
        };

        // no data available at this time
        if message.is_empty() {
            return Ok(Maintain);
        }

        let reader = &mut message.as_slice();

        loop {
            let cmd = match ServerCmd::deserialize(reader) {
                Err(e) => {
                    error!("{}", e);
                    break;
                }
                Ok(Some(cmd)) => cmd,
                Ok(None) => break,
            };

            match cmd {
                ServerCmd::Bad => {
                    warn!("Invalid command from server")
                }

                ServerCmd::NoOp => {}

                ServerCmd::CdTrack { track, .. } => {
                    mixer_events.send(MixerEvent::StartMusic(Some(sound::MusicSource::TrackId(
                        match track_override {
                            Some(t) => t as usize,
                            None => track as usize,
                        },
                    ))));
                }

                ServerCmd::CenterPrint { text } => {
                    console_output.set_center_print(text, time);
                }

                ServerCmd::PlayerData(player_data) => self.state.update_player(player_data),

                ServerCmd::Cutscene { text } => {
                    self.state.intermission = Some(IntermissionKind::Cutscene { text });
                    self.state.completion_time = Some(self.state.time);
                }

                ServerCmd::Damage {
                    armor,
                    blood,
                    source,
                } => self.state.handle_damage(armor, blood, source, kick_vars),

                ServerCmd::Disconnect => {
                    return Ok(match self.kind {
                        ConnectionKind::Demo(_) => NextDemo,
                        ConnectionKind::Server { .. } => Disconnect,
                    });
                }

                ServerCmd::FastUpdate(ent_update) => {
                    // first update signals the last sign-on stage
                    self.handle_signon(&client_vars, state.reborrow(), SignOnStage::Done)?;

                    let ent_id = ent_update.ent_id as usize;
                    self.state.update_entity(ent_id, ent_update)?;

                    // patch view angles in demos
                    if let Some(angles) = demo_view_angles {
                        if ent_id == self.state.view_entity_id() {
                            self.state.update_view_angles(angles);
                        }
                    }
                }

                ServerCmd::Finale { text } => {
                    self.state.intermission = Some(IntermissionKind::Finale { text });
                    self.state.completion_time = Some(self.state.time);
                }

                ServerCmd::FoundSecret => self.state.stats[ClientStat::FoundSecrets as usize] += 1,
                ServerCmd::Intermission => {
                    self.state.intermission = Some(IntermissionKind::Intermission);
                    self.state.completion_time = Some(self.state.time);
                }
                ServerCmd::KilledMonster => {
                    self.state.stats[ClientStat::KilledMonsters as usize] += 1
                }

                ServerCmd::LightStyle { id, value } => {
                    trace!("Inserting light style {} with value {}", id, &value);
                    self.state.light_styles[id as usize] = value.to_str().into_owned();
                }

                ServerCmd::Particle {
                    origin,
                    direction,
                    count,
                    color,
                } => {
                    match count {
                        // if count is 255, this is an explosion
                        255 => self
                            .state
                            .particles
                            .create_explosion(self.state.time, origin),

                        // otherwise it's an impact
                        _ => self.state.particles.create_projectile_impact(
                            self.state.time,
                            origin,
                            direction,
                            color,
                            count as usize,
                        ),
                    }
                }

                ServerCmd::Print { text } => console_output.print_alert(text.raw, time),

                ServerCmd::ServerInfo {
                    protocol_version,
                    max_clients,
                    game_type,
                    message,
                    model_precache,
                    sound_precache,
                } => {
                    // check protocol version
                    if protocol_version != net::PROTOCOL_VERSION as i32 {
                        Err(ClientError::UnrecognizedProtocol(protocol_version))?;
                    }

                    console_output.println_alert(CONSOLE_DIVIDER, time);
                    console_output.println_alert(message.raw, time);
                    console_output.println_alert(CONSOLE_DIVIDER, time);

                    let _server_info = ServerInfo {
                        _max_clients: max_clients,
                        _game_type: game_type,
                    };

                    self.state = ClientState::from_server_info(
                        vfs,
                        asset_server,
                        max_clients,
                        model_precache,
                        sound_precache,
                    )?;
                }

                ServerCmd::SetAngle { angles } => self.state.set_view_angles(angles),

                ServerCmd::SetView { ent_id } => {
                    if ent_id > 0 {
                        self.state.set_view_entity(ent_id as usize)?;
                    } else if ent_id < 0 {
                        Err(ClientError::InvalidViewEntity(ent_id as usize))?;
                    }
                }

                ServerCmd::SignOnStage { stage } => {
                    self.handle_signon(&client_vars, state.reborrow(), stage)?;
                }

                ServerCmd::Sound {
                    volume,
                    attenuation,
                    entity_id,
                    channel,
                    sound_id,
                    position,
                } => {
                    trace!(
                        "starting sound with id {} on entity {} channel {}",
                        sound_id,
                        entity_id,
                        channel
                    );

                    if entity_id as usize >= self.state.entities.len() {
                        error!(
                            "server tried to start sound on nonexistent entity {}",
                            entity_id
                        );
                        break;
                    }

                    let volume = volume.unwrap_or(DEFAULT_SOUND_PACKET_VOLUME);
                    let attenuation = attenuation.unwrap_or(DEFAULT_SOUND_PACKET_ATTENUATION);
                    // TODO: apply volume, attenuation, spatialization
                    mixer_events.send(MixerEvent::StartSound(StartSound {
                        src: self.state.sounds[sound_id as usize].clone(),
                        ent_id: Some(entity_id as usize),
                        ent_channel: channel,
                        volume: volume as f32 / 255.0,
                        attenuation,
                        origin: position.into(),
                    }));
                }

                ServerCmd::SpawnBaseline {
                    ent_id,
                    model_id,
                    frame_id,
                    colormap,
                    skin_id,
                    origin,
                    angles,
                } => {
                    if ent_id == 0 {
                        match &mut *state {
                            ConnectionState::Connected(state) => {
                                state.worldmodel_id = model_id as _
                            }
                            _ => {}
                        }

                        self.state.worldmodel_id = model_id as _;
                    } else {
                        self.state.spawn_entities(
                            ent_id as usize,
                            EntityState {
                                model_id: model_id as usize,
                                frame_id: frame_id as usize,
                                colormap,
                                skin_id: skin_id as usize,
                                origin,
                                angles,
                                effects: EntityEffects::empty(),
                            },
                        )?;
                    }
                }

                ServerCmd::SpawnStatic {
                    model_id,
                    frame_id,
                    colormap,
                    skin_id,
                    origin,
                    angles,
                } => {
                    if self.state.static_entities.len() >= MAX_STATIC_ENTITIES {
                        Err(ClientError::TooManyStaticEntities)?;
                    }
                    let id = self.state.static_entities.len();
                    self.state
                        .static_entities
                        .push_back(ClientEntity::from_baseline(
                            id,
                            EntityState {
                                origin,
                                angles,
                                model_id: model_id as usize,
                                frame_id: frame_id as usize,
                                colormap,
                                skin_id: skin_id as usize,
                                effects: EntityEffects::empty(),
                            },
                        ));
                }

                ServerCmd::SpawnStaticSound {
                    origin,
                    sound_id,
                    volume,
                    attenuation,
                } => {
                    if let Some(sound) = self.state.sounds.get(sound_id as usize) {
                        mixer_events.send(MixerEvent::StartStaticSound(StartStaticSound {
                            src: sound.clone(),
                            origin,
                            volume: volume as f32 / 255.0,
                            attenuation: attenuation as f32 / 64.0,
                        }));
                    }
                }

                ServerCmd::TempEntity { temp_entity } => {
                    self.state.spawn_temp_entity(mixer_events, &temp_entity);
                }

                ServerCmd::StuffText { text } => match text.to_str().parse() {
                    Ok(parsed) => {
                        console_commands.send(parsed);
                    }
                    Err(err) => console_output.println(format!("{}", err), self.state.msg_times[0]),
                },

                ServerCmd::Time { time } => {
                    self.state.msg_times[1] = self.state.msg_times[0];
                    self.state.msg_times[0] = engine::duration_from_f32(time);
                }

                ServerCmd::UpdateColors {
                    player_id,
                    new_colors,
                } => {
                    let player_id = player_id as usize;
                    self.state.check_player_id(player_id)?;

                    match self.state.player_info[player_id] {
                        Some(ref mut info) => {
                            trace!(
                                "Player {} (ID {}) colors: {:?} -> {:?}",
                                info.name,
                                player_id,
                                info.colors,
                                new_colors,
                            );
                            info.colors = new_colors;
                        }

                        None => {
                            error!(
                                "Attempted to set colors on nonexistent player with ID {}",
                                player_id
                            );
                        }
                    }
                }

                ServerCmd::UpdateFrags {
                    player_id,
                    new_frags,
                } => {
                    let player_id = player_id as usize;
                    self.state.check_player_id(player_id)?;

                    match self.state.player_info[player_id] {
                        Some(ref mut info) => {
                            trace!(
                                "Player {} (ID {}) frags: {} -> {}",
                                &info.name,
                                player_id,
                                info.frags,
                                new_frags
                            );
                            info.frags = new_frags as i32;
                        }
                        None => {
                            error!(
                                "Attempted to set frags on nonexistent player with ID {}",
                                player_id
                            );
                        }
                    }
                }

                ServerCmd::UpdateName {
                    player_id,
                    new_name,
                } => {
                    let player_id = player_id as usize;
                    self.state.check_player_id(player_id)?;

                    if let Some(ref mut info) = self.state.player_info[player_id] {
                        // if this player is already connected, it's a name change
                        debug!("Player {} has changed name to {}", &info.name, &new_name);
                        info.name = new_name.into_string().into();
                    } else {
                        // if this player is not connected, it's a join
                        debug!("Player {} with ID {} has joined", &new_name, player_id);
                        self.state.player_info[player_id] = Some(PlayerInfo {
                            name: new_name.into_string().into(),
                            colors: PlayerColor::new(0, 0),
                            frags: 0,
                        });
                    }
                }

                ServerCmd::UpdateStat { stat, value } => {
                    debug!(
                        "{:?}: {} -> {}",
                        stat, self.state.stats[stat as usize], value
                    );
                    self.state.stats[stat as usize] = value;
                }

                ServerCmd::Version { version } => {
                    if version != net::PROTOCOL_VERSION as i32 {
                        // TODO: handle with an error
                        error!(
                            "Incompatible server version: server's is {}, client's is {}",
                            version,
                            net::PROTOCOL_VERSION,
                        );
                        panic!("bad version number");
                    }
                }

                ServerCmd::SetPause { .. } => {}

                ServerCmd::StopSound { entity_id, channel } => {
                    mixer_events.send(MixerEvent::StopSound(StopSound {
                        ent_id: Some(entity_id as _),
                        ent_channel: channel,
                    }));
                }
                ServerCmd::SellScreen => todo!(),
            }
        }

        Ok(Maintain)
    }

    fn frame(
        &mut self,
        mut state: Mut<ConnectionState>,
        time: Time,
        vfs: &Vfs,
        asset_server: &AssetServer,
        from_server: &Events<ServerMessage>,
        to_server: &mut EventWriter<ClientMessage>,
        mixer_events: &mut EventWriter<MixerEvent>,
        console_commands: &mut EventWriter<RunCmd<'static>>,
        mut console: Mut<ConsoleOutput>,
        idle_vars: IdleVars,
        kick_vars: KickVars,
        roll_vars: RollVars,
        bob_vars: BobVars,
        client_vars: ClientVars,
        cl_nolerp: bool,
        sv_gravity: f32,
    ) -> Result<ConnectionStatus, ClientError> {
        let frame_time = Duration::from_std(time.delta()).unwrap();
        debug!("frame time: {}ms", frame_time.num_milliseconds());

        // do this _before_ parsing server messages so that we know when to
        // request the next message from the demo server.
        self.state.advance_time(frame_time);
        match self.parse_server_msg(
            state.reborrow(),
            time,
            vfs,
            asset_server,
            from_server,
            mixer_events,
            console_commands,
            console.reborrow(),
            kick_vars,
            client_vars,
        )? {
            ConnectionStatus::Maintain => {}
            // if Disconnect or NextDemo, delegate up the chain
            s => return Ok(s),
        };

        self.state.update_interp_ratio(cl_nolerp);

        // interpolate entity data and spawn particle effects, lights
        self.state.update_entities()?;

        // update temp entities (lightning, etc.)
        self.state.update_temp_entities()?;

        // remove expired lights
        self.state.lights.update(self.state.time);

        // apply particle physics and remove expired particles
        self.state
            .particles
            .update(self.state.time, frame_time, sv_gravity);

        if let ConnectionKind::Server { compose, .. } = &mut self.kind {
            // respond to the server
            if !compose.is_empty() {
                to_server.send(ClientMessage {
                    packet: mem::take(compose),
                    ..default()
                });
            }
        }

        // these all require the player entity to have spawned
        // TODO: Need to improve this code - maybe split it out into surrounding function?
        if let ConnectionState::Connected(_) = &*state {
            // update view
            self.state.calc_final_view(
                idle_vars,
                kick_vars,
                roll_vars,
                if self.state.intermission().is_none() {
                    bob_vars
                } else {
                    default()
                },
            );

            // update camera color shifts for new position/effects
            self.state.update_color_shifts(frame_time)?;
        }

        Ok(ConnectionStatus::Maintain)
    }
}

#[derive(Resource, ExtractResource, Clone)]
pub struct DemoQueue {
    values: Vec<String>,
    indices: iter::Peekable<iter::Cycle<Range<usize>>>,
}

impl Default for DemoQueue {
    fn default() -> Self {
        Self::new(default())
    }
}

impl DemoQueue {
    pub fn new(inner: Vec<String>) -> Self {
        let range = (0..inner.len()).cycle().peekable();
        Self {
            values: inner,
            indices: range,
        }
    }

    pub fn next(&mut self) -> Option<&str> {
        self.indices.next().map(|i| &self.values[i][..])
    }

    pub fn index(&mut self) -> Option<usize> {
        self.indices.peek().copied()
    }

    pub fn reset(&mut self) {
        self.indices = (0..self.values.len()).cycle().peekable();
    }
}

fn connect<A>(server_addrs: A) -> Result<(QSocket, ConnectionState), ClientError>
where
    A: ToSocketAddrs,
{
    let mut con_sock = ConnectSocket::bind("0.0.0.0:0")?;
    let server_addr = match server_addrs.to_socket_addrs() {
        Ok(ref mut a) => a.next().ok_or(ClientError::InvalidServerAddress),
        Err(_) => Err(ClientError::InvalidServerAddress),
    }?;

    let mut response = None;

    for attempt in 0..MAX_CONNECT_ATTEMPTS {
        println!(
            "Connecting...(attempt {} of {})",
            attempt + 1,
            MAX_CONNECT_ATTEMPTS
        );
        con_sock.send_request(
            Request::connect(net::GAME_NAME, CONNECT_PROTOCOL_VERSION),
            server_addr,
        )?;

        // TODO: get rid of magic constant (2.5 seconds wait time for response)
        match con_sock.recv_response(Some(Duration::try_milliseconds(2500).unwrap())) {
            Err(err) => {
                match err {
                    // if the message is invalid, log it but don't quit
                    // TODO: this should probably disconnect
                    err @ NetError::InvalidData { .. } => error!("{}", err),

                    // other errors are fatal
                    e => return Err(e.into()),
                }
            }

            Ok(opt) => {
                if let Some((resp, remote)) = opt {
                    // if this response came from the right server, we're done
                    if remote == server_addr {
                        response = Some(resp);
                        break;
                    }
                }
            }
        }
    }

    let port = match response.ok_or(ClientError::NoResponse)? {
        Response::Accept(accept) => {
            // validate port number
            if accept.port < 0 || accept.port >= std::u16::MAX as i32 {
                Err(ClientError::InvalidConnectPort(accept.port))?;
            }

            debug!("Connection accepted on port {}", accept.port);
            accept.port as u16
        }

        // our request was rejected.
        Response::Reject(reject) => Err(ClientError::ConnectionRejected(
            reject.message.into_string(),
        ))?,

        // the server sent back a response that doesn't make sense here (i.e. something other
        // than an Accept or Reject).
        _ => Err(ClientError::InvalidConnectResponse)?,
    };

    let mut new_addr = server_addr;
    new_addr.set_port(port);

    // we're done with the connection socket, so turn it into a QSocket with the new address
    let qsock = con_sock.into_qsocket(new_addr);

    Ok((qsock, ConnectionState::SignOn(SignOnStage::Prespawn)))
}

#[derive(Event)]
pub struct Impulse(pub u8);

mod systems {
    use common::net::MessageKind;
    use serde::Deserialize;

    use self::common::console::Registry;

    use super::*;

    pub fn handle_input(
        // mut console: ResMut<Console>,
        registry: ResMut<Registry>,
        conn_state: Option<Res<ConnectionState>>,
        mut conn: Option<ResMut<Connection>>,
        frame_time: Res<Time<Virtual>>,
        mut client_events: EventWriter<ClientMessage>,
        mut impulses: EventReader<Impulse>,
    ) -> Result<(), ClientError> {
        match conn_state.as_deref() {
            None | Some(ConnectionState::SignOn(_)) => return Ok(()),
            _ => {}
        }

        // TODO: Error handling
        let move_vars: MoveVars = registry.read_cvars().unwrap();
        let mouse_vars: MouseVars = registry.read_cvars().unwrap();

        // TODO: Unclear fromm the bevy documentation if this drops all other events for the frame,
        //       but in this case it's almost certainly fine
        let impulse = impulses.read().next().map(|i| i.0);

        match conn.as_deref_mut() {
            Some(Connection {
                ref mut state,
                kind: ConnectionKind::Server { .. },
                ..
            }) => {
                let move_cmd = state.handle_input(
                    &*registry,
                    Duration::from_std(frame_time.delta()).unwrap(),
                    move_vars,
                    mouse_vars,
                    impulse,
                );
                let mut msg = Vec::new();
                move_cmd.serialize(&mut msg)?;
                client_events.send(ClientMessage {
                    client_id: 0,
                    packet: msg,
                    kind: MessageKind::Unreliable,
                });

                // TODO: Refresh input (e.g. mouse movement)
            }

            _ => (),
        }

        Ok(())
    }

    #[derive(Deserialize)]
    struct NetworkVars {
        #[serde(rename(deserialize = "cl_nolerp"))]
        disable_lerp: f32,
        #[serde(rename(deserialize = "sv_gravity"))]
        gravity: f32,
    }

    pub fn frame(
        mut commands: Commands,
        cvars: Res<Registry>,
        vfs: Res<Vfs>,
        time: Res<Time<Virtual>>,
        asset_server: Res<AssetServer>,
        mut mixer_events: EventWriter<MixerEvent>,
        from_server: Res<Events<ServerMessage>>,
        mut to_server: EventWriter<ClientMessage>,
        mut console: ResMut<ConsoleOutput>,
        mut console_commands: EventWriter<RunCmd<'static>>,
        mut demo_queue: ResMut<DemoQueue>,
        mut focus: ResMut<InputFocus>,
        mut conn: Option<ResMut<Connection>>,
        mut conn_state: ResMut<ConnectionState>,
    ) -> Result<(), ClientError> {
        let NetworkVars {
            disable_lerp,
            gravity,
        } = cvars.read_cvars().map_err(|c| ClientError::Cvar(c))?;
        let idle_vars: IdleVars = cvars.read_cvars().map_err(|c| ClientError::Cvar(c))?;
        let kick_vars: KickVars = cvars.read_cvars().map_err(|c| ClientError::Cvar(c))?;
        let roll_vars: RollVars = cvars.read_cvars().map_err(|c| ClientError::Cvar(c))?;
        let bob_vars: BobVars = cvars.read_cvars().map_err(|c| ClientError::Cvar(c))?;
        // `serde_lexpr` doesn't allow us to configure deserialising strings and doesn't recognise symbols
        // as valid strings, so we need to use `.value().as_name()` and can't use `read_cvars`.
        let client_vars: ClientVars = ClientVars {
            name: cvars
                .get_cvar("_cl_name")
                .ok_or(ClientError::Cvar(ConsoleError::CvarParseInvalid {
                    backtrace: snafu::Backtrace::capture(),
                }))?
                .value()
                .as_name()
                .unwrap_or("player"),
            color: cvars.read_cvar("_cl_color")?,
        };

        let status = match conn.as_deref_mut() {
            Some(ref mut conn) => conn.frame(
                conn_state.reborrow(),
                if cvars.read_cvar::<u8>("sv_paused").unwrap() != 0 {
                    default()
                } else {
                    time.as_generic()
                },
                &*vfs,
                &*asset_server,
                &*from_server,
                &mut to_server,
                &mut mixer_events,
                &mut console_commands,
                console.reborrow(),
                idle_vars,
                kick_vars,
                roll_vars,
                bob_vars,
                client_vars,
                disable_lerp != 0.,
                gravity,
            )?,
            None => ConnectionStatus::Disconnect,
        };

        use ConnectionStatus::*;
        match status {
            Maintain => (),
            _ => {
                let time = Duration::from_std(time.elapsed()).unwrap();
                let new_conn = match status {
                    // if client is already disconnected, this is a no-op
                    Disconnect => None,

                    // get the next demo from the queue
                    NextDemo => loop {
                        match demo_queue.next() {
                            Some(demo) => {
                                // TODO: Extract this to a separate function so we don't duplicate the logic to find the demos in different places
                                let mut demo_file = match vfs
                                    .open(format!("{}.dem", demo))
                                    .or_else(|_| vfs.open(format!("demos/{}.dem", demo)))
                                {
                                    Ok(f) => Some(f),
                                    Err(e) => {
                                        // log the error, dump the demo queue and disconnect
                                        console.println(format!("{}", e), time);

                                        match demo_queue.index() {
                                            Some(0) => break None,
                                            _ => continue,
                                        }
                                    }
                                };

                                break demo_file.as_mut().and_then(|df| {
                                    match DemoServer::new(df) {
                                        Ok(d) => Some(Connection {
                                            kind: ConnectionKind::Demo(d),
                                            state: ClientState::new(),
                                        }),
                                        Err(e) => {
                                            console.println(format!("{}", e), time);
                                            demo_queue.reset();
                                            None
                                        }
                                    }
                                });
                            }

                            // if there are no more demos in the queue, disconnect
                            None => break None,
                        }
                    },

                    // covered in first match
                    Maintain => unreachable!(),
                };

                // don't allow game focus when disconnected
                if new_conn.is_none() {
                    *focus = InputFocus::Console;
                }

                match (conn, new_conn) {
                    (Some(mut conn), Some(new_conn)) => {
                        *conn = new_conn;
                        *conn_state = ConnectionState::SignOn(SignOnStage::Prespawn);
                    }
                    (None, Some(new_conn)) => {
                        commands.insert_resource(new_conn);
                        *conn_state = ConnectionState::SignOn(SignOnStage::Prespawn);
                    }
                    (Some(_), None) => {
                        commands.remove_resource::<Connection>();
                        *conn_state = ConnectionState::SignOn(SignOnStage::Not);
                    }
                    (None, None) => {}
                }
            }
        }

        Ok(())
    }

    pub fn set_resolution(
        window: Query<&Window, With<PrimaryWindow>>,
        mut target_resource: ResMut<RenderResolution>,
    ) {
        let res = &window.single().resolution;
        let res = RenderResolution(res.width() as _, res.height() as _);
        if *target_resource != res {
            *target_resource = res;
        }
    }

    pub fn process_network_messages(
        state: Res<ConnectionState>,
        mut qsock: ResMut<QSocket>,
        mut server_events: EventWriter<ServerMessage>,
        mut client_events: EventReader<ClientMessage>,
    ) -> Result<(), NetError> {
        let blocking_mode = match &*state {
            // if we're in the game, don't block waiting for messages
            ConnectionState::Connected(_) => BlockingMode::NonBlocking,

            // otherwise, give the server some time to respond
            // TODO: might make sense to make this a future or something
            ConnectionState::SignOn(_) => BlockingMode::Timeout(Duration::try_seconds(5).unwrap()),
        };

        server_events.send(ServerMessage {
            client_id: 0,
            packet: qsock.recv_msg(blocking_mode)?,
        });

        for event in client_events.read() {
            match event.kind {
                MessageKind::Unreliable => qsock.send_msg_unreliable(&event.packet)?,
                MessageKind::Reliable => qsock.begin_send_msg(&event.packet)?,
            }
        }

        Ok(())
    }
}
