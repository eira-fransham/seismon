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
pub mod sound;
pub mod state;
pub mod trace;
pub mod view;

use self::{
    input::SeismonInputPlugin,
    menu::{MenuBodyView, MenuBuilder, MenuView},
    sound::{MixerEvent, SeismonSoundPlugin},
};

use std::{iter, net::ToSocketAddrs, ops::Range, path::PathBuf};

use crate::{
    client::{
        demo::{DemoServer, DemoServerError},
        entity::ClientEntity,
        sound::{MusicPlayer, StartSound, StartStaticSound, StopSound},
        state::ClientState,
        view::{KickVars, MouseVars},
    },
    common::{
        self,
        console::{ConsoleError, ConsoleOutput, RunCmd, SeismonConsolePlugin},
        net::{
            self, BlockingMode, ClientCmd, ClientMessage, GameType, NetError, QSocket, ServerCmd,
            ServerMessage, SignOnStage,
            connect::{CONNECT_PROTOCOL_VERSION, ConnectSocket, Request, Response},
        },
        vfs::{Vfs, VfsError},
    },
};

use beef::Cow;
use bevy::{
    asset::AssetServer,
    ecs::{
        event::{EventCursor, EventWriter},
        system::{Res, ResMut},
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
use seismon_utils::QString;
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
        body: MenuBodyView::Predefined { path: "gfx/mainmenu.lmp".into() },
    }))
}

impl SeismonClientPlugin {
    pub fn new() -> Self {
        Self { base_dir: None, game: None, main_menu: Box::new(build_default) }
    }
}

#[derive(Clone, Resource, ExtractResource)]
pub struct SeismonGameSettings {
    pub base_dir: PathBuf,
    pub game: Option<String>,
}

impl<F> Plugin for SeismonClientPlugin<F>
where F: Fn(MenuBuilder) -> Result<Menu, failure::Error> + Clone + Send + Sync + 'static
{
    fn build(&self, app: &mut bevy::prelude::App) {
        if let Ok(menu) = (self.main_menu)(MenuBuilder::new(app.world_mut())) {
            app.insert_resource(menu);
        }

        let app = app
            .insert_resource(SeismonGameSettings {
                base_dir: self.base_dir.clone().unwrap_or_else(common::default_base_dir),
                game: self.game.clone(),
            })
            .init_resource::<Vfs>()
            .init_resource::<MusicPlayer>()
            .init_resource::<DemoQueue>()
            .add_event::<ServerMessage>()
            .add_event::<ClientMessage>()
            .add_event::<Impulse>()
            .add_systems(
                Main,
                (
                    systems::lock_cursor,
                    systems::send_input_to_server,
                    systems::frame::parse_server_msg
                        .pipe(systems::frame::advance_frame)
                        .pipe(systems::frame::end_frame)
                        .pipe(|In(res)| {
                            // TODO: Error handling
                            if let Err(e) = res {
                                error!("Error handling input: {}", e);
                            }
                        })
                        // TODO: Use bevy's state system
                        .run_if(
                            resource_exists::<Connection>
                                .and(|conn: Res<Connection>| conn.is_connected()),
                        ),
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
            .add_plugins(SeismonSoundPlugin)
            .add_plugins(SeismonInputPlugin);

        cvars::register_cvars(app);
        commands::register_commands(app);
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
    #[error("Network error: {0}")]
    Network(#[from] NetError),
    #[error("Failed to load sound: {0}")]
    Sound(#[from] SoundError),
    #[error("Virtual filesystem error: {0}")]
    Vfs(#[from] VfsError),
    #[error("Sound decoding error: {0}")]
    DecodeSound(#[from] symphonium::error::LoadError),
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
    // For `v_cshift`
    Custom = 4,
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

/// Indicates the state of an active connection.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ConnectionStage {
    /// The client is in the sign-on process.
    SignOn(SignOnStage),

    /// The client is fully connected.
    Connected,
}

#[derive(Event, Copy, Clone, Default)]
pub struct Connected(pub bool);

/// Possible targets that a client can be connected to.
enum ConnectionTarget {
    /// A regular Quake server.
    Server {
        /// The socket used to communicate with the server.
        reader: EventCursor<ServerMessage>,

        /// The client's packet composition buffer.
        compose: Vec<u8>,

        /// How far we are through the sign-on process.
        stage: ConnectionStage,
    },

    /// A demo server.
    Demo(DemoServer),
}

impl ConnectionTarget {
    pub fn stage(&self) -> ConnectionStage {
        match self {
            ConnectionTarget::Server { stage, .. } => *stage,
            ConnectionTarget::Demo(_) => ConnectionStage::Connected,
        }
    }
}

#[derive(Debug)]
struct ServerUpdate<'a> {
    message: Cow<'a, [u8]>,
    angles: Option<Vec3>,
    track_override: Option<u32>,
}

impl ServerUpdate<'_> {
    pub fn into_owned(self) -> ServerUpdate<'static> {
        ServerUpdate {
            message: self.message.into_owned().into(),
            angles: self.angles,
            track_override: self.track_override,
        }
    }
}

impl Default for ServerUpdate<'_> {
    fn default() -> Self {
        Self { message: (&[][..]).into(), angles: None, track_override: None }
    }
}

impl ConnectionTarget {
    fn recv(
        &mut self,
        events: &Events<ServerMessage>,
    ) -> Result<Option<ServerUpdate<'_>>, ClientError> {
        match self {
            Self::Server { reader, .. } => {
                let mut out = Vec::new();
                for ServerMessage { client_id, packet } in reader.read(events) {
                    // TODO: Actually use correct client id
                    if *client_id == 0 {
                        out.extend(packet.iter().copied());
                    }
                }

                Ok(Some(ServerUpdate { message: out.into(), ..default() }))
            }
            Self::Demo(demo_srv) => {
                let track_override = demo_srv.track_override();
                let msg_view = match demo_srv.next_msg() {
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
    client_state: ClientState,
    target: ConnectionTarget,
    last_msg_time: Time,
}

impl Connection {
    pub fn new_server(connection_state: ConnectionStage) -> Self {
        Self {
            client_state: default(),
            target: ConnectionTarget::Server {
                reader: default(),
                compose: default(),
                stage: connection_state,
            },
            last_msg_time: Time::default(),
        }
    }
}

impl Connection {
    pub fn is_connected(&self) -> bool {
        !matches!(self.target, ConnectionTarget::Server { stage: ConnectionStage::SignOn(_), .. })
    }

    fn handle_signon(
        &mut self,
        new_stage: SignOnStage,
        mut commands: Commands<'_, '_>,
        client_vars: &ClientVars,
    ) -> Result<(), ClientError> {
        use SignOnStage as S;

        let ConnectionTarget::Server { compose, stage: stage @ ConnectionStage::SignOn(_), .. } =
            &mut self.target
        else {
            return Ok(());
        };

        match new_stage {
            S::Not => (), // TODO this is an error (invalid value)
            S::Prespawn => {
                ClientCmd::StringCmd { cmd: "prespawn".into() }.serialize(compose)?;
            }
            S::ClientInfo => {
                // TODO: fill in client info here
                ClientCmd::StringCmd { cmd: format!("name \"{}\"\n", &client_vars.name).into() }
                    .serialize(compose)?;
                ClientCmd::StringCmd {
                    cmd: format!(
                        "color {} {}",
                        client_vars.color >> 4,
                        client_vars.color & ((1 << 4) - 1)
                    )
                    .into(),
                }
                .serialize(compose)?;
                ClientCmd::StringCmd {
                    // TODO: need default spawn parameters?
                    cmd: "spawn".into(),
                }
                .serialize(compose)?;
            }
            S::Begin => {
                ClientCmd::StringCmd { cmd: "begin".into() }.serialize(compose)?;
            }
            S::Done => {
                debug!("SignOn complete");
            }
        }

        *stage = match new_stage {
            // TODO proper error
            S::Not => panic!("SignOnStage::Not in handle_signon"),
            // still signing on, advance to the new stage
            S::Prespawn | S::ClientInfo | S::Begin => ConnectionStage::SignOn(new_stage),

            // finished signing on, build world renderer
            S::Done => {
                if let Some(model) = self.client_state.models.get(0) {
                    commands.spawn(SceneRoot(model.clone()));
                } else {
                    todo!("No worldmodel in precache")
                }

                ConnectionStage::Connected
            }
        };

        Ok(())
    }
}

#[derive(Resource, Clone)]
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
        Self { values: inner, indices: range }
    }

    pub fn next_demo(&mut self) -> Option<&str> {
        self.indices.next().map(|i| &self.values[i][..])
    }

    pub fn index(&mut self) -> Option<usize> {
        self.indices.peek().copied()
    }

    pub fn reset(&mut self) {
        self.indices = (0..self.values.len()).cycle().peekable();
    }
}

fn connect<A>(server_addrs: A) -> Result<(QSocket, ConnectionStage), ClientError>
where A: ToSocketAddrs {
    let mut con_sock = ConnectSocket::bind("0.0.0.0:0")?;
    let server_addr = match server_addrs.to_socket_addrs() {
        Ok(ref mut a) => a.next().ok_or(ClientError::InvalidServerAddress),
        Err(_) => Err(ClientError::InvalidServerAddress),
    }?;

    let mut response = None;

    for attempt in 0..MAX_CONNECT_ATTEMPTS {
        println!("Connecting...(attempt {} of {})", attempt + 1, MAX_CONNECT_ATTEMPTS);
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
            if accept.port < 0 || accept.port >= u16::MAX as i32 {
                Err(ClientError::InvalidConnectPort(accept.port))?;
            }

            debug!("Connection accepted on port {}", accept.port);
            accept.port as u16
        }

        // our request was rejected.
        Response::Reject(reject) => {
            Err(ClientError::ConnectionRejected(reject.message.into_string()))?
        }

        // the server sent back a response that doesn't make sense here (i.e. something other
        // than an Accept or Reject).
        _ => Err(ClientError::InvalidConnectResponse)?,
    };

    let mut new_addr = server_addr;
    new_addr.set_port(port);

    // we're done with the connection socket, so turn it into a QSocket with the new address
    let qsock = con_sock.into_qsocket(new_addr);

    Ok((qsock, ConnectionStage::SignOn(SignOnStage::Prespawn)))
}

#[derive(Event)]
pub struct Impulse(pub u8);

mod systems {

    use bevy::window::CursorGrabMode;
    use common::net::MessageKind;
    use serde::Deserialize;

    use crate::common::net::ButtonFlags;

    use self::common::console::Registry;

    use super::*;

    pub fn lock_cursor(
        mut q_windows: Query<&mut Window, With<PrimaryWindow>>,
        registry: Res<Registry>,
        focus: Res<InputFocus>,
    ) {
        let mut primary_window = q_windows.single_mut().unwrap();

        if *focus == InputFocus::Game && registry.is_pressed("mlook") {
            primary_window.cursor_options.grab_mode = CursorGrabMode::Locked;
            primary_window.cursor_options.visible = false;
        } else {
            primary_window.cursor_options.grab_mode = CursorGrabMode::None;
            primary_window.cursor_options.visible = true;
        }
    }

    pub fn send_input_to_server(
        // mut console: ResMut<Console>,
        registry: ResMut<Registry>,
        mut conn: ResMut<Connection>,
        frame_time: Res<Time<Virtual>>,
        mut client_events: EventWriter<ClientMessage>,
        mut impulses: EventReader<Impulse>,
    ) -> Result<(), ClientError> {
        fn handle_input(
            registry: &Registry,
            frame_time: Duration,
            move_vars: MoveVars,
            // mouse_vars: MouseVars,
            impulse: Option<u8>,
        ) -> Option<ClientCmd> {
            let mlook = registry.is_pressed("mlook");

            let mut move_left = registry.is_pressed("moveleft");
            let mut move_right = registry.is_pressed("moveright");
            if registry.is_pressed("strafe") {
                move_left |= registry.is_pressed("left");
                move_right |= registry.is_pressed("right");
            }

            let mut sidemove =
                move_vars.cl_sidespeed * (move_right as i32 - move_left as i32) as f32;

            let mut upmove = move_vars.cl_upspeed
                * (registry.is_pressed("moveup") as i32 - registry.is_pressed("movedown") as i32)
                    as f32;

            let mut forwardmove = 0.0;
            if !registry.is_pressed("klook") {
                forwardmove +=
                    move_vars.cl_forwardspeed * registry.is_pressed("forward") as i32 as f32;
                forwardmove -= move_vars.cl_backspeed * registry.is_pressed("back") as i32 as f32;
            }

            if registry.is_pressed("speed") {
                sidemove *= move_vars.cl_movespeedkey;
                upmove *= move_vars.cl_movespeedkey;
                forwardmove *= move_vars.cl_movespeedkey;
            }

            let mut button_flags = ButtonFlags::empty();

            if registry.is_pressed("attack") {
                button_flags |= ButtonFlags::ATTACK;
            }

            if registry.is_pressed("jump") {
                button_flags |= ButtonFlags::JUMP;
            }

            if registry.is_pressed("use") {
                button_flags |= ButtonFlags::USE;
            }

            if !mlook {
                // TODO: IN_Move (mouse / joystick / gamepad)
            }

            let delta_time = frame_time;

            Some(ClientCmd::Move {
                delta_time,
                angles: Vec3::ZERO,
                fwd_move: forwardmove as i16,
                side_move: sidemove as i16,
                up_move: upmove as i16,
                button_flags,
                // TODO: Is `impulse 0` correct?
                impulse: impulse.unwrap_or_default(),
            })
        }

        let Connection {
            client_state: state,
            target: ConnectionTarget::Server { stage: ConnectionStage::Connected, .. },
            ..
        } = &mut *conn
        else {
            return Ok(());
        };

        // TODO: Error handling
        let move_vars: MoveVars = registry.read_cvars().unwrap();
        let mouse_vars: MouseVars = registry.read_cvars().unwrap();

        // TODO: Unclear fromm the bevy documentation if this drops all other events for the frame,
        //       but in this case it's almost certainly fine
        let impulse = impulses.read().next().map(|i| i.0);

        let Some(move_cmd) = handle_input(
            &registry,
            Duration::from_std(frame_time.delta()).unwrap(),
            move_vars,
            // mouse_vars,
            impulse,
        ) else {
            return Ok(());
        };
        let mut msg = Vec::new();
        move_cmd.serialize(&mut msg)?;
        client_events.write(ClientMessage {
            client_id: 0,
            packet: msg.into(),
            kind: MessageKind::Unreliable,
        });

        // TODO: Refresh input (e.g. mouse movement)

        Ok(())
    }

    #[derive(Deserialize)]
    struct NetworkVars {
        #[serde(rename(deserialize = "cl_nolerp"))]
        disable_lerp: f32,
        #[serde(rename(deserialize = "sv_gravity"))]
        gravity: f32,
    }

    pub fn process_network_messages(
        state: Res<Connection>,
        mut qsock: ResMut<QSocket>,
        mut server_events: EventWriter<ServerMessage>,
        mut client_events: EventReader<ClientMessage>,
    ) -> Result<(), NetError> {
        let blocking_mode = match &state.target {
            // if we're in the game, don't block waiting for messages
            ConnectionTarget::Server { stage: ConnectionStage::Connected, .. } => {
                BlockingMode::NonBlocking
            }

            // otherwise, give the server some time to respond
            // TODO: might make sense to make this a future or something
            ConnectionTarget::Server { stage: ConnectionStage::SignOn(_), .. } => {
                BlockingMode::Timeout(Duration::try_seconds(5).unwrap())
            }

            _ => return Ok(()),
        };

        server_events
            .write(ServerMessage { client_id: 0, packet: qsock.recv_msg(blocking_mode)?.into() });

        for event in client_events.read() {
            match event.kind {
                MessageKind::Unreliable => qsock.send_msg_unreliable(&event.packet)?,
                MessageKind::Reliable => qsock.begin_send_msg(&event.packet)?,
            }
        }

        Ok(())
    }

    pub fn send_connected_event(
        conn: Option<Res<Connection>>,
        mut connected: EventWriter<Connected>,
        mut old_stage: Local<Option<ConnectionStage>>,
    ) {
        let Some(conn) = conn.as_ref() else {
            *old_stage = None;
            return;
        };

        if !conn.is_changed() {
            return;
        }

        let new_stage = conn.target.stage();
        let old_stage = old_stage.replace(new_stage);

        if old_stage != Some(ConnectionStage::Connected) {
            connected.write(Connected(new_stage == ConnectionStage::Connected));
        }
    }

    mod frame {
        use super::*;

        pub enum FrameSystem {}

        pub fn parse_server_msg(
            commands: Commands<'_, '_>,
            mut conn: ResMut<Connection>,
            time: Res<Time<Virtual>>,
            asset_server: &AssetServer,
            server_events: &Events<ServerMessage>,
            mixer_events: &mut EventWriter<MixerEvent>,
            console_commands: &mut EventWriter<RunCmd<'static>>,
            mut console_output: Mut<ConsoleOutput>,
            kick_vars: KickVars,
            client_vars: ClientVars,
        ) -> Result<ConnectionStatus, ClientError> {
            use ConnectionStatus as S;

            if time.elapsed() < conn.last_msg_time.elapsed() {
                return Ok(S::Maintain);
            }

            let Some(server_update) = conn.target.recv(server_events)? else {
                return if conn.target.is_demo() { Ok(S::NextDemo) } else { Ok(S::Maintain) };
            };

            let ServerUpdate { message, angles: demo_view_angles, track_override } =
                server_update.into_owned();

            // no data available at this time
            if message.is_empty() {
                return Ok(S::Maintain);
            }

            let reader = &mut &message[..];

            loop {
                let cmd = match ServerCmd::deserialize(reader) {
                    Err(e) => {
                        error!("{}", e);
                        break;
                    }
                    Ok(Some(cmd)) => cmd,
                    Ok(None) => break,
                };

                if !matches!(
                    &cmd,
                    ServerCmd::FastUpdate(..) | ServerCmd::Time { .. } | ServerCmd::PlayerData(..)
                ) {
                    debug!("CMD: {cmd:?}");
                } else {
                    trace!("CMD: {cmd:?}");
                }

                match cmd {
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

                        console_output.println_alert(CONSOLE_DIVIDER, &time);
                        console_output.println_alert(message.raw, &time);
                        console_output.println_alert(CONSOLE_DIVIDER, &time);

                        let _server_info =
                            ServerInfo { _max_clients: max_clients, _game_type: game_type };
                    }

                    ServerCmd::Bad => {
                        warn!("Invalid command from server")
                    }

                    ServerCmd::NoOp => {}

                    ServerCmd::CdTrack { track, .. } => {
                        mixer_events.write(MixerEvent::StartMusic(Some(
                            sound::MusicSource::TrackId(match track_override {
                                Some(t) => t as usize,
                                None => track as usize,
                            }),
                        )));
                    }

                    ServerCmd::CenterPrint { text } => {
                        console_output.set_center_print(text, &time);
                    }

                    ServerCmd::PlayerData(player_data) => {
                        // conn.client_state.update_player(player_data);
                        warn!("TODO: player data")
                    }

                    ServerCmd::Cutscene { text } => {
                        warn!("TODO: cutscene")
                    }

                    ServerCmd::Damage { armor, blood, source } => {
                        conn.client_state.handle_damage(armor, blood, source, kick_vars)
                    }

                    ServerCmd::Disconnect => {
                        return Ok(match &conn.target {
                            ConnectionTarget::Demo(_) => S::NextDemo,
                            ConnectionTarget::Server { .. } => S::Disconnect,
                        });
                    }

                    ServerCmd::FastUpdate(ent_update) => {
                        // first update signals the last sign-on stage
                        conn.handle_signon(SignOnStage::Done, commands.reborrow(), &client_vars)?;

                        conn.client_state.update_entity(commands.reborrow(), &ent_update)?;

                        // patch view angles in demos
                        // if let Some(angles) = demo_view_angles
                        //     && conn.client_state.view_entity_id() == Some(ent_update.ent_id as
                        // usize) {
                        //     conn.client_state.update_view_angles(angles);
                        // }
                    }

                    ServerCmd::Finale { .. } => warn!("TODO: intermission"),
                    ServerCmd::FoundSecret => warn!("TODO: found secret"),
                    ServerCmd::Intermission => {
                        warn!("TODO: intermission");
                    }
                    ServerCmd::KilledMonster => {
                        warn!("TODO: killed monster")
                    }

                    ServerCmd::LightStyle { .. } => {
                        warn!("TODO: lightstyle")
                    }

                    ServerCmd::Particle { .. } => {
                        warn!("TODO: particle")
                    }

                    ServerCmd::Print { text } => console_output.print_alert(text.raw, &time),

                    ServerCmd::SetAngle { .. } => warn!("TODO: set angle"),

                    ServerCmd::SetView { .. } => {
                        // TODO: Spawn camera as child of entity here
                        warn!("TODO: set view");
                    }

                    ServerCmd::SignOnStage { stage } => {
                        conn.handle_signon(stage, commands.reborrow(), &client_vars)?;
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
                            sound_id, entity_id, channel
                        );

                        if !conn
                            .client_state
                            .server_entity_to_client_entity
                            .contains_key(&entity_id)
                        {
                            error!(
                                "server tried to start sound on nonexistent entity {}",
                                entity_id
                            );
                            continue;
                        }

                        let volume = volume.unwrap_or(DEFAULT_SOUND_PACKET_VOLUME);
                        let attenuation = attenuation.unwrap_or(DEFAULT_SOUND_PACKET_ATTENUATION);
                        if let Some(sound) = conn.client_state.sounds.get(sound_id as usize) {
                            mixer_events.write(MixerEvent::StartSound(StartSound {
                                src: sound.clone(),
                                ent_id: Some(entity_id as usize),
                                ent_channel: channel,
                                volume: volume as f32 / 255.0,
                                attenuation,
                                origin: position.into(),
                            }));
                        }
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
                        let entity = commands
                            .spawn((
                                SceneRoot(conn.client_state.models[model_id as usize].clone()),
                                Transform::from_xyz(origin.x, origin.y, origin.z).with_rotation(
                                    Quat::from_euler(EulerRot::YZX, angles.x, angles.y, angles.z),
                                ),
                                // TODO: Handle the other fields
                            ))
                            .id();

                        conn.client_state.server_entity_to_client_entity.insert(ent_id, entity);
                    }

                    ServerCmd::SpawnStatic {
                        model_id,
                        frame_id,
                        colormap,
                        skin_id,
                        origin,
                        angles,
                    } => {
                        commands.spawn((
                            SceneRoot(conn.client_state.models[model_id as usize].clone()),
                            Transform::from_xyz(origin.x, origin.y, origin.z).with_rotation(
                                Quat::from_euler(EulerRot::YZX, angles.x, angles.y, angles.z),
                            ),
                            // TODO: Handle the other fields
                        ));
                    }

                    ServerCmd::SpawnStaticSound { origin, sound_id, volume, attenuation } => {
                        if let Some(sound) = conn.client_state.sounds.get(sound_id as usize) {
                            mixer_events.write(MixerEvent::StartStaticSound(StartStaticSound {
                                src: sound.clone(),
                                origin,
                                volume: volume as f32 / 255.0,
                                attenuation: attenuation as f32 / 64.0,
                            }));
                        }
                    }

                    ServerCmd::TempEntity { temp_entity } => {
                        error!("TODO: temp entity");
                    }

                    ServerCmd::StuffText { text } => match text.to_str().parse() {
                        Ok(parsed) => {
                            console_commands.write(parsed);
                        }
                        Err(err) => console_output.println(format!("{err}"), &time),
                    },

                    ServerCmd::Time { time } => {
                        conn.last_msg_time = Time::from_seconds(time as f64).as_generic();
                    }

                    ServerCmd::UpdateColors { player_id, new_colors } => {
                        // let player_id = player_id as usize;
                        // conn.client_state.check_player_id(player_id)?;

                        // match conn.client_state.player_info[player_id] {
                        //     Some(ref mut info) => {
                        //         trace!(
                        //             "Player {} (ID {}) colors: {:?} -> {:?}",
                        //             info.name, player_id, info.colors, new_colors,
                        //         );
                        //         info.colors = new_colors;
                        //     }

                        //     None => {
                        //         error!(
                        //             "Attempted to set colors on nonexistent player with ID {}",
                        //             player_id
                        //         );
                        //     }
                        // }
                    }

                    ServerCmd::UpdateFrags { player_id, new_frags } => {
                        // let player_id = player_id as usize;
                        // conn.client_state.check_player_id(player_id)?;

                        // match conn.client_state.player_info[player_id] {
                        //     Some(ref mut info) => {
                        //         trace!(
                        //             "Player {} (ID {}) frags: {} -> {}",
                        //             &info.name, player_id, info.frags, new_frags
                        //         );
                        //         info.frags = new_frags as i32;
                        //     }
                        //     None => {
                        //         error!(
                        //             "Attempted to set frags on nonexistent player with ID {}",
                        //             player_id
                        //         );
                        //     }
                        // }
                    }

                    ServerCmd::UpdateName { player_id, new_name } => {
                        // let player_id = player_id as usize;
                        // conn.client_state.check_player_id(player_id)?;

                        // if let Some(ref mut info) = conn.client_state.player_info[player_id] {
                        //     // if this player is already connected, it's a name change
                        //     debug!("Player {} has changed name to {}", &info.name, &new_name);
                        //     info.name = new_name.into_string().into();
                        // } else {
                        //     // if this player is not connected, it's a join
                        //     debug!("Player {} with ID {} has joined", &new_name, player_id);
                        //     conn.client_state.player_info[player_id] = Some(PlayerInfo {
                        //         name: new_name.into_string().into(),
                        //         colors: PlayerColor::new(0, 0),
                        //         frags: 0,
                        //     });
                        // }
                    }

                    ServerCmd::UpdateStat { stat, value } => {
                        // debug!(
                        //     "{:?}: {} -> {}",
                        //     stat, conn.client_state.stats[stat as usize], value
                        // );
                        // conn.client_state.stats[stat as usize] = value;
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
                        mixer_events.write(MixerEvent::StopSound(StopSound {
                            ent_id: Some(entity_id as _),
                            ent_channel: channel,
                        }));
                    }
                    ServerCmd::SellScreen => todo!(),
                }
            }

            Ok(S::Maintain)
        }

        pub fn advance_frame(
            In(status): In<Result<ConnectionStatus, ClientError>>,
            mut conn: Mut<Connection>,
            to_server: &mut EventWriter<ClientMessage>,
        ) -> Result<ConnectionStatus, ClientError> {
            match status? {
                ConnectionStatus::Maintain => {}
                // if Disconnect or NextDemo, delegate up the chain
                s => return Ok(s),
            }

            if let ConnectionTarget::Server { compose, .. } = &mut conn.target {
                // respond to the server
                if !compose.is_empty() {
                    to_server
                        .write(ClientMessage { packet: compose.drain(..).collect(), ..default() });
                }
            }

            // these all require the player entity to have spawned
            // TODO: Need to improve this code - maybe split it out into surrounding function?
            // if let ConnectionStage::Connected = todo!() {
            //     // update view
            //     // conn.client_state.calc_final_view(
            //     //     idle_vars,
            //     //     kick_vars,
            //     //     roll_vars,
            //     //     if conn.client_state.intermission().is_none() {
            //     //         bob_vars
            //     //     } else {
            //     //         default()
            //     //     },
            //     // );

            //     // // update camera color shifts for new position/effects
            //     // conn.client_state.update_color_shifts(frame_time)?;
            // }

            Ok(ConnectionStatus::Maintain)
        }

        #[allow(clippy::too_many_arguments)]
        pub fn end_frame(
            // TODO: This can almost certainly be simplified.
            In(status): In<Result<ConnectionStatus, ClientError>>,
            mut commands: Commands,
            vfs: Res<Vfs>,
            time: Res<Time<Virtual>>,
            mut console: ResMut<ConsoleOutput>,
            mut demo_queue: ResMut<DemoQueue>,
            mut focus: ResMut<InputFocus>,
        ) -> Result<(), ClientError> {
            use ConnectionStatus::*;
            let new_conn = match status? {
                Maintain => return Ok(()),
                // if client is already disconnected, this is a no-op
                Disconnect => None,

                // get the next demo from the queue
                NextDemo => loop {
                    match demo_queue.next_demo() {
                        Some(demo) => {
                            // TODO: Extract this to a separate function so we don't duplicate the
                            // logic to find the demos in different places
                            let mut demo_file = match vfs
                                .open(format!("{demo}.dem"))
                                .or_else(|_| vfs.open(format!("demos/{demo}.dem")))
                            {
                                Ok(f) => Some(f),
                                Err(e) => {
                                    // log the error, dump the demo queue and disconnect
                                    console.println(format!("{e}"), &time);

                                    match demo_queue.index() {
                                        Some(0) => break None,
                                        _ => continue,
                                    }
                                }
                            };

                            break demo_file.as_mut().and_then(|df| match DemoServer::new(df) {
                                Ok(d) => Some(Connection {
                                    target: ConnectionTarget::Demo(d),
                                    client_state: ClientState::new(),
                                    last_msg_time: Time::default(),
                                }),
                                Err(e) => {
                                    console.println(format!("{e}"), &time);
                                    demo_queue.reset();
                                    None
                                }
                            });
                        }

                        // if there are no more demos in the queue, disconnect
                        None => break None,
                    }
                },
            };

            // don't allow game focus when disconnected
            if new_conn.is_none() {}

            if let Some(new_conn) = new_conn {
                commands.insert_resource(new_conn);
            } else {
                commands.remove_resource::<Connection>();
                *focus = InputFocus::Console;
            }

            Ok(())
        }
    }
}
