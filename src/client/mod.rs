pub mod commands;
mod cvars;
pub mod demo;
pub mod entity;
mod hud;
pub mod input;
mod interpolation;
mod inventory;
pub mod menu;
pub mod sound;
pub mod state;
pub mod trace;
pub mod view;

use self::{
    input::SeismonInputPlugin,
    menu::{MenuBodyView, MenuBuilder, MenuView},
    sound::{MixerMessage, SeismonSoundPlugin},
};

use std::{iter, net::ToSocketAddrs, ops::Range, path::PathBuf, time::Duration};

use crate::{
    client::{
        demo::{Demo, DemoError, DemoLoader, DemoServer},
        entity::ClientEntity,
        interpolation::InterpolateApp,
        inventory::InventoryPlugin,
        sound::{MusicPlayer, StartSound, StartStaticSound, StopSound},
        state::ClientState,
        view::KickVars,
    },
    common::{
        self,
        console::{ConsoleError, ConsoleOutput, RunCmd, SeismonClientConsolePlugin},
        net::{
            self, BlockingMode, ClientCmd, ClientMessage, EntityState, EntityUpdate, NetError,
            QSocket, ServerCmd, ServerMessage, SignOnStage,
            connect::{CONNECT_PROTOCOL_VERSION, ConnectSocket, Request, Response},
        },
        vfs::{Vfs, VfsError},
        wad::{Palette, PaletteLoader, QPicLoader, Wad, WadLoader},
    },
};

use beef::Cow;
use bevy::{
    asset::{AssetServer, LoadState},
    ecs::{
        entity_disabling::Disabled,
        message::MessageCursor,
        system::{Res, ResMut},
    },
    prelude::*,
    time::{Time, Virtual},
    window::PrimaryWindow,
};
use bevy_trenchbroom::{TrenchBroomPlugins, config::WriteTrenchBroomConfigOnStartPlugin};
use input::InputFocus;
use menu::Menu;
use seismon_utils::{QAngles, QString};
use serde::Deserialize;
use sound::SoundError;
use thiserror::Error;

// connections are tried 3 times, see
// https://github.com/id-Software/Quake/blob/master/WinQuake/net_dgrm.c#L1248
const MAX_CONNECT_ATTEMPTS: usize = 3;
// TODO
// const MAX_STATS: usize = 32;

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

#[derive(Clone, Resource)]
pub struct SeismonGameSettings {
    pub base_dir: PathBuf,
    pub game: Option<String>,
    pub camera_template: Entity,
    default_camera_template: Entity,
}

const DEFAULT_AMBIENT_LIGHT: f32 = 120.;

/// The default set of components for the camera.
pub fn default_camera() -> impl Bundle {
    (
        Camera3d::default(),
        Projection::Perspective(PerspectiveProjection { fov: 90_f32.to_radians(), ..default() }),
        Transform::from_translation(Vec3::new(0.0, 0.0, 22.0)).with_rotation(Quat::from_euler(
            EulerRot::XYZ,
            std::f32::consts::FRAC_PI_2,
            -std::f32::consts::FRAC_PI_2,
            0.,
        )),
        Visibility::Visible,
        AmbientLight {
            color: Color::WHITE,
            brightness: DEFAULT_AMBIENT_LIGHT,
            affects_lightmapped_meshes: false,
        },
        Disabled,
    )
}

#[derive(States, Default, Debug, Clone, PartialEq, Eq, Hash, Reflect)]
enum ClientGameState {
    #[default]
    Disconnected,
    WaitForServerInfo,
    Loading,
    InGame,
}

impl<F> Plugin for SeismonClientPlugin<F>
where
    F: Fn(MenuBuilder) -> Result<Menu, failure::Error> + Clone + Send + Sync + 'static,
{
    fn build(&self, app: &mut bevy::prelude::App) {
        if let Ok(menu) = (self.main_menu)(MenuBuilder::new(app.world_mut())) {
            app.insert_resource(menu);
        }

        let camera_template = app.world_mut().spawn(default_camera()).id();

        let app = app
            .init_asset::<Palette>()
            .init_asset::<Wad>()
            .init_asset_loader::<QPicLoader>()
            .init_asset_loader::<PaletteLoader>()
            .init_asset_loader::<WadLoader>()
            .interpolate_component::<Transform, Virtual>()
            .insert_resource(SeismonGameSettings {
                base_dir: self.base_dir.clone().unwrap_or_else(common::default_base_dir),
                game: self.game.clone(),
                camera_template,
                default_camera_template: camera_template,
            })
            .init_state::<ClientGameState>()
            .init_resource::<Vfs>()
            .init_resource::<MusicPlayer>()
            .init_resource::<DemoQueue>()
            .init_asset::<Demo>()
            .init_asset_loader::<DemoLoader>()
            .add_message::<ServerCmd>()
            .add_message::<Disconnect>()
            .add_message::<NewConnection>()
            .add_message::<ServerMessage>()
            .add_message::<ClientMessage>()
            .add_message::<Impulse>()
            .add_systems(
                PreUpdate,
                (systems::disconnect.run_if(resource_exists::<Connection>), systems::connect)
                    .chain()
                    .run_if(on_message::<Disconnect>.or(on_message::<NewConnection>)),
            )
            .add_systems(
                PreUpdate,
                systems::wait_for_load.run_if(in_state(ClientGameState::Loading)),
            )
            .add_systems(
                PreUpdate,
                systems::process_network_messages
                    .pipe(|In(res)| {
                        // TODO: Error handling
                        if let Err(e) = res {
                            error!("Error handling frame: {}", e);
                        }
                    })
                    .run_if(resource_exists::<QSocket>),
            )
            .add_systems(
                Update,
                (
                    systems::frame::parse_server_message
                        .pipe(systems::frame::execute_server_msg)
                        .pipe(systems::frame::advance_frame)
                        .pipe(systems::frame::end_frame)
                        .pipe(|In(res)| {
                            if let Err(e) = res {
                                error!("Error handling frame: {e}");
                            }
                        }),
                    systems::send_input_to_server.pipe(|In(res)| {
                        if let Err(e) = res {
                            error!("Error handling input: {e}");
                        }
                    }),
                )
                    .run_if(
                        resource_exists::<Connection>.and(
                            in_state(ClientGameState::WaitForServerInfo)
                                .or(in_state(ClientGameState::InGame)),
                        ),
                    ),
            )
            .add_systems(
                Update,
                systems::lock_cursor.run_if(
                    state_changed::<InputFocus>.or(resource_changed::<common::console::Registry>),
                ),
            )
            .add_plugins(SeismonClientConsolePlugin)
            .add_plugins(SeismonSoundPlugin)
            .add_plugins(SeismonInputPlugin)
            .add_plugins(InventoryPlugin::default())
            .add_plugins(
                TrenchBroomPlugins(Default::default())
                    .build()
                    .disable::<WriteTrenchBroomConfigOnStartPlugin>(),
            );

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
    DemoServer(#[from] DemoError),
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
    // TODO
    // #[serde(rename(deserialize = "cl_anglespeedkey"))]
    // cl_anglespeedkey: f32,
    // #[serde(rename(deserialize = "cl_pitchspeed"))]
    // cl_pitchspeed: f32,
    // #[serde(rename(deserialize = "cl_yawspeed"))]
    // cl_yawspeed: f32,
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

// TODO
// #[derive(Debug, FromPrimitive)]
// enum ColorShiftCode {
//     Contents = 0,
//     Damage = 1,
//     Bonus = 2,
//     Powerup = 3,
//     // For `v_cshift`
//     Custom = 4,
// }

#[derive(Clone, Debug)]
pub enum IntermissionKind {
    Intermission,
    Finale { text: QString },
    Cutscene { text: QString },
}

/// Indicates to the client what should be done with the current connection.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum ConnectionStatus {
    /// Early exit, skip the rest of the logic for this frame
    Break,

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

/// Possible targets that a client can be connected to.
///
/// > TODO: These should be two different resources so we can switch the systems based on
/// > which exists.
enum ConnectionTarget {
    /// A regular Quake server.
    Server {
        /// The socket used to communicate with the server.
        reader: MessageCursor<ServerMessage>,

        /// The client's packet composition buffer.
        compose: Vec<u8>,

        /// How far we are through the sign-on process.
        stage: ConnectionStage,
    },

    /// A demo server.
    Demo(DemoServer),
}

#[derive(Debug, Clone)]
struct ServerUpdate<T = ServerCmd> {
    message: T,
    angles: Option<QAngles>,
    track_override: Option<u32>,
}

impl ServerUpdate {
    /// Override angles for view entity and CD track, for demos.
    fn to_cmd(self, view_id: Option<u16>) -> ServerCmd {
        match self.message {
            ServerCmd::CdTrack { track, loop_ } => {
                ServerCmd::CdTrack { track: self.track_override.unwrap_or(track), loop_ }
            }
            ServerCmd::SetAngle { angles } => {
                ServerCmd::SetAngle { angles: self.angles.unwrap_or(angles) }
            }
            ServerCmd::FastUpdate(entity_update) => {
                if Some(entity_update.ent_id) == view_id
                    && let Some(angles) = self.angles
                {
                    ServerCmd::FastUpdate(EntityUpdate {
                        pitch: Some(angles.pitch_deg),
                        roll: Some(angles.roll_deg),
                        yaw: Some(angles.yaw_deg),
                        no_lerp: false,
                        ..entity_update
                    })
                } else {
                    ServerCmd::FastUpdate(entity_update)
                }
            }
            other => other,
        }
    }
}

impl ServerUpdate<Cow<'_, [u8]>> {
    pub fn into_owned(self) -> ServerUpdate<Vec<u8>> {
        ServerUpdate {
            message: self.message.into_owned(),
            angles: self.angles,
            track_override: self.track_override,
        }
    }
}

impl Default for ServerUpdate<Cow<'_, [u8]>> {
    fn default() -> Self {
        Self { message: (&[][..]).into(), angles: None, track_override: None }
    }
}

impl ConnectionTarget {
    // TODO: `Result<Result<..>>` is ugly here.
    fn recv<'a>(
        &mut self,
        assets: &AssetServer,
        demos: &'a Assets<Demo>,
        events: &Messages<ServerMessage>,
    ) -> Result<Result<ServerUpdate<Cow<'a, [u8]>>, ConnectionStatus>, ClientError> {
        match self {
            Self::Server { reader, .. } => {
                let mut out = Vec::new();
                for ServerMessage { client_id, packet } in reader.read(events) {
                    // TODO: Actually use correct client id
                    if *client_id == 0 {
                        out.extend(packet.iter().copied());
                    }
                }

                Ok(Ok(ServerUpdate { message: out.into(), ..default() }))
            }
            Self::Demo(demo_srv) => {
                match assets.load_state(demo_srv.demo()) {
                    LoadState::Loaded => {}
                    LoadState::Loading => return Ok(Err(ConnectionStatus::Break)),
                    LoadState::NotLoaded => return Ok(Err(ConnectionStatus::NextDemo)),
                    LoadState::Failed(err) => {
                        return Err(ClientError::DemoServer(DemoError::Load(err)));
                    }
                };

                let msg_view = match demo_srv.next_msg(demos) {
                    Some(v) => v,
                    None => {
                        // if there are no commands left in the demo, play
                        // the next demo if there is one
                        return Ok(Err(ConnectionStatus::NextDemo));
                    }
                };

                Ok(Ok(ServerUpdate {
                    message: msg_view.message.into(),
                    angles: Some(msg_view.view_angles),
                    track_override: msg_view.track_override,
                }))
            }
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
    client_state: Option<ClientState>,
    target: ConnectionTarget,
    last_msg_time: Duration,
    connected: bool,
}

#[derive(Message)]
pub enum NewConnection {
    Demo(Handle<Demo>),
    Server(Option<QSocket>, ConnectionStage),
}

impl NewConnection {
    fn socket(&self) -> Option<&QSocket> {
        match self {
            NewConnection::Server(Some(qsocket), _) => Some(qsocket),
            _ => None,
        }
    }
}

#[derive(Message)]
pub struct Disconnect;

impl From<&NewConnection> for Connection {
    fn from(value: &NewConnection) -> Self {
        match value {
            NewConnection::Server(_, connection_state) => Connection {
                target: ConnectionTarget::Server {
                    reader: default(),
                    compose: default(),
                    stage: *connection_state,
                },
                client_state: default(),
                last_msg_time: Duration::ZERO,
                connected: false,
            },
            NewConnection::Demo(demo) => Connection {
                target: ConnectionTarget::Demo(demo.clone().into()),
                client_state: default(),
                last_msg_time: Duration::ZERO,
                connected: false,
            },
        }
    }
}

impl Connection {
    pub fn is_connected(&self) -> bool {
        self.connected
    }

    fn handle_signon(
        &mut self,
        new_stage: SignOnStage,
        client_vars: &ClientVars,
    ) -> Result<(), ClientError> {
        use SignOnStage as S;

        if new_stage == S::Done {
            self.connected = true;
        }

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
            S::Done => ConnectionStage::Connected,
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
        println!("Connecting...(attempt {} of {})", attempt + 1, MAX_CONNECT_ATTEMPTS);
        con_sock.send_request(
            Request::connect(net::GAME_NAME, CONNECT_PROTOCOL_VERSION),
            server_addr,
        )?;

        // TODO: get rid of magic constant (2.5 seconds wait time for response)
        match con_sock.recv_response(Some(Duration::from_millis(2500))) {
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

#[derive(Message)]
pub struct Impulse(pub u8);

mod systems {
    use bevy::window::{CursorGrabMode, CursorOptions};
    use bevy_trenchbroom::bsp::Bsp;
    use common::net::MessageKind;

    use crate::{client::state::Worldspawn, common::net::ButtonFlags};

    use self::common::console::Registry;

    use super::*;

    pub fn lock_cursor(
        mut cursor_options: Single<&mut CursorOptions, (With<Window>, With<PrimaryWindow>)>,
        registry: Res<Registry>,
        focus: Res<State<InputFocus>>,
    ) {
        if *focus == InputFocus::Game && registry.is_pressed("mlook") {
            cursor_options.grab_mode = CursorGrabMode::Locked;
            cursor_options.visible = false;
        } else {
            cursor_options.grab_mode = CursorGrabMode::None;
            cursor_options.visible = true;
        }
    }

    pub fn send_input_to_server(
        // mut console: ResMut<Console>,
        registry: ResMut<Registry>,
        mut conn: ResMut<Connection>,
        frame_time: Res<Time<Virtual>>,
        mut client_events: MessageWriter<ClientMessage>,
        mut impulses: MessageReader<Impulse>,
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

            Some(ClientCmd::Move {
                delta_time: frame_time,
                // TODO: Look
                angles: Default::default(),
                fwd_move: forwardmove as i16,
                side_move: sidemove as i16,
                up_move: upmove as i16,
                button_flags,
                // TODO: Is `impulse 0` correct?
                impulse: impulse.unwrap_or_default(),
            })
        }

        let Connection {
            client_state: _,
            target: ConnectionTarget::Server { stage: ConnectionStage::Connected, .. },
            ..
        } = &mut *conn
        else {
            return Ok(());
        };

        // TODO: Error handling
        let move_vars: MoveVars = registry.read_cvars().unwrap();

        // TODO: Unclear fromm the bevy documentation if this drops all other events for the frame,
        //       but in this case it's almost certainly fine
        let impulse = impulses.read().next().map(|i| i.0);

        let Some(move_cmd) = handle_input(&registry, frame_time.delta(), move_vars, impulse) else {
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

    pub fn process_network_messages(
        state: Res<Connection>,
        mut qsock: ResMut<QSocket>,
        mut server_events: MessageWriter<ServerMessage>,
        mut client_events: MessageReader<ClientMessage>,
    ) -> Result<(), NetError> {
        let blocking_mode = match &state.target {
            // if we're in the game, don't block waiting for messages
            ConnectionTarget::Server { stage: ConnectionStage::Connected, .. } => {
                BlockingMode::NonBlocking
            }

            // otherwise, give the server some time to respond
            // TODO: might make sense to make this a future or something
            ConnectionTarget::Server { stage: ConnectionStage::SignOn(_), .. } => {
                BlockingMode::Timeout(Duration::from_secs(5))
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

    // pub fn send_connected_event(
    //     conn: Option<Res<Connection>>,
    //     mut connected: MessageWriter<Connected>,
    //     mut old_stage: Local<Option<ConnectionStage>>,
    // ) {
    //     let Some(conn) = conn.as_ref() else {
    //         *old_stage = None;
    //         return;
    //     };

    //     if !conn.is_changed() {
    //         return;
    //     }

    //     let new_stage = conn.target.stage();
    //     let old_stage = old_stage.replace(new_stage);

    //     if old_stage != Some(ConnectionStage::Connected) {
    //         connected.write(Connected(new_stage == ConnectionStage::Connected));
    //     }
    // }

    pub fn wait_for_load(
        mut conn: ResMut<Connection>,
        bsp_assets: Res<Assets<Bsp>>,
        mut time: ResMut<Time<Virtual>>,
        worldspawn: Query<&Worldspawn>,
        mut next_state: ResMut<NextState<ClientGameState>>,
        mut commands: Commands,
    ) {
        let Some(client_state) = &mut conn.client_state else {
            next_state.set(ClientGameState::Disconnected);
            return;
        };

        let Ok(worldspawn) = worldspawn.get(client_state.worldspawn) else {
            next_state.set(ClientGameState::Disconnected);
            return;
        };

        if let Some(bsp) = bsp_assets.get(&worldspawn.bsp) {
            commands.entity(client_state.worldspawn).insert(bsp.transform.clone());
            client_state.populate_precache(bsp);

            // TODO: The server should be able to communicate this to the client, but the
            // default is 20fps
            let max_delta = Duration::from_millis(1000 / 20);
            *time = Time::from_max_delta(max_delta);
            time.advance_to(conn.last_msg_time.saturating_sub(max_delta));
            next_state.set(ClientGameState::InGame);
        }
    }

    pub fn disconnect(
        mut commands: Commands,
        conn: Res<Connection>,
        mut next_focus: ResMut<NextState<InputFocus>>,
        mut next_state: ResMut<NextState<ClientGameState>>,
    ) {
        if let Some(state) = &conn.client_state
            && let Ok(mut worldspawn) = commands.get_entity(state.worldspawn)
        {
            worldspawn.try_despawn();
        }

        commands.remove_resource::<Connection>();
        commands.remove_resource::<QSocket>();
        // TODO: We can do this generically rather than manually setting it each time
        next_state.set(ClientGameState::Disconnected);
        // don't allow game focus when disconnected
        next_focus.set(InputFocus::Console);
    }

    pub fn connect(
        mut commands: Commands,
        mut next_focus: ResMut<NextState<InputFocus>>,
        mut next_state: ResMut<NextState<ClientGameState>>,
        mut connections: MessageReader<NewConnection>,
    ) {
        let Some(new_connection) = connections.read().last() else {
            return;
        };

        if let Some(_) = new_connection.socket() {
            todo!("Need to remove connection from the command and put it here");
        }

        commands.insert_resource(Connection::from(new_connection));
        // TODO: We can do this generically rather than manually setting it each time
        next_state.set(ClientGameState::WaitForServerInfo);
        // don't allow game focus when disconnected
        next_focus.set(InputFocus::Game);
    }

    pub mod frame {
        use bevy::ecs::entity_disabling::Disabled;

        use crate::client::{hud::build_hud, state::PrecacheModel};

        use super::*;

        #[derive(Component, Reflect)]
        #[reflect(Component)]
        #[relationship(relationship_target = ViewEntities)]
        pub struct ViewFor(pub Entity);

        #[derive(Component, Reflect)]
        #[reflect(Component)]
        #[relationship_target(relationship = ViewFor, linked_spawn)]
        pub struct ViewEntities(Vec<Entity>);

        pub fn parse_server_message(
            mut conn: ResMut<Connection>,
            mut time: ResMut<Time<Virtual>>,
            mut server_cmds: MessageWriter<ServerCmd>,
            server_events: Res<Messages<ServerMessage>>,
            asset_server: Res<AssetServer>,
            demos: Res<Assets<Demo>>,
        ) -> Result<ConnectionStatus, ClientError> {
            if time.elapsed() < conn.last_msg_time {
                return Ok(ConnectionStatus::Maintain);
            } else if conn.last_msg_time > time.elapsed() + time.max_delta() {
                *time = Time::from_max_delta(time.max_delta());
                time.advance_to(conn.last_msg_time);
            }

            // TODO: Shuld "wait for demo load" logic be in its own system?
            let server_update = match conn.target.recv(&*asset_server, &*demos, &server_events)? {
                Ok(update) => update,
                Err(status) => return Ok(status),
            };

            let ServerUpdate { message, angles, track_override } = server_update.into_owned();

            // no data available at this time
            if message.is_empty() {
                return Ok(ConnectionStatus::Maintain);
            }

            let reader = &mut &message[..];

            let cmd_iter = std::iter::from_fn(|| match ServerCmd::deserialize(reader) {
                Err(e) => {
                    error!("{}", e);
                    None
                }
                Ok(Some(cmd)) => Some(
                    ServerUpdate { message: cmd, angles, track_override }
                        .to_cmd(conn.client_state.as_ref().and_then(|s| s.view)),
                ),
                Ok(None) => None,
            });
            server_cmds.write_batch(cmd_iter);

            Ok(ConnectionStatus::Maintain)
        }

        #[expect(clippy::too_many_arguments)]
        pub fn execute_server_msg(
            In(status): In<Result<ConnectionStatus, ClientError>>,
            mut commands: Commands,
            mut next_state: ResMut<NextState<ClientGameState>>,
            mut conn: ResMut<Connection>,
            settings: Res<SeismonGameSettings>,
            time: Res<Time<Virtual>>,
            asset_server: Res<AssetServer>,
            registry: Res<Registry>,
            mut pending_messages: Local<Vec<ServerCmd>>,
            mut server_cmds: MessageReader<ServerCmd>,
            mut mixer_events: MessageWriter<MixerMessage>,
            mut console_commands: MessageWriter<RunCmd<'static>>,
            mut console_output: ResMut<ConsoleOutput>,
            view_entities: Query<&ViewEntities>,
            transforms: Query<&Transform>,
        ) -> Result<ConnectionStatus, ClientError> {
            use ConnectionStatus as S;

            match status? {
                S::Maintain => {}
                s => return Ok(s),
            }

            let kick_vars: KickVars = registry.read_cvars()?;
            // `serde_lexpr` doesn't allow us to configure deserialising strings and doesn't
            // recognise symbols as valid strings, so we need to use
            // `.value().as_name()` and can't use `read_cvars`.
            // TODO: Use `nu` (https://github.com/eira-fransham/seismon/issues/44)
            let client_vars: ClientVars = ClientVars {
                name: registry
                    .get_cvar("_cl_name")
                    .ok_or(ClientError::Cvar(ConsoleError::CvarParseInvalid {
                        backtrace: snafu::Backtrace::capture(),
                    }))?
                    .value()
                    .as_name()
                    .unwrap_or("player"),
                color: registry.read_cvar("_cl_color")?,
            };

            macro_rules! msg_todo {
                ($cmd_name:expr) => {{ debug!("TODO: {}", $cmd_name) }};
            }

            let mut server_cmds = server_cmds.read().cloned();

            let status = {
                let mut pending_message_iter = pending_messages.drain(..);

                loop {
                    let Some(cmd) = pending_message_iter.next().or_else(|| server_cmds.next())
                    else {
                        break S::Maintain;
                    };

                    if !matches!(
                        cmd,
                        ServerCmd::FastUpdate(..)
                            | ServerCmd::Time { .. }
                            | ServerCmd::PlayerData(..)
                    ) {
                        debug!("CMD: {cmd:?}");
                    } else {
                        trace!("CMD: {cmd:?}");
                    }

                    if let Some(ent_id) = cmd.entity()
                        && let Some(state) = &mut conn.client_state
                    {
                        state.mark_entity_alive(ent_id);
                        // TODO: This seems wrong.
                        if let Some(entity) = state.server_entity_to_client_entity.get(&ent_id) {
                            commands.entity(*entity).remove_recursive::<Children, Disabled>();
                        }
                    }

                    match cmd {
                        ServerCmd::ServerInfo {
                            protocol_version,
                            max_clients: _,
                            game_type: _,
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

                            if let Some(state) = conn.client_state.as_ref() {
                                commands.entity(state.worldspawn).despawn();
                            }

                            let mut new_worldspawn = commands.spawn(Visibility::Visible);

                            conn.client_state = Some(ClientState::from_server_info(
                                &mut new_worldspawn,
                                &asset_server,
                                model_precache,
                                sound_precache,
                            )?);

                            next_state.set(ClientGameState::Loading);
                            break S::Break;
                        }

                        ServerCmd::Bad => {
                            warn!("Invalid command from server")
                        }

                        ServerCmd::NoOp => {}

                        ServerCmd::CdTrack { track, .. } => {
                            mixer_events.write(MixerMessage::StartMusic(Some(
                                sound::MusicSource::TrackId(track as usize),
                            )));
                        }

                        ServerCmd::CenterPrint { text } => {
                            console_output.set_center_print(text, &time);
                        }

                        ServerCmd::PlayerData(player_data) => {
                            commands
                                .run_system_cached_with(ClientState::update_player, player_data);
                        }

                        ServerCmd::Cutscene { text: _ } => {
                            msg_todo!("cutscene")
                        }

                        ServerCmd::Damage { armor, blood, source } => {
                            if let Some(state) = conn.client_state.as_mut() {
                                state.handle_damage(armor, blood, source, kick_vars)
                            }
                        }

                        ServerCmd::Disconnect => {
                            return Ok(match &conn.target {
                                ConnectionTarget::Demo(_) => S::NextDemo,
                                ConnectionTarget::Server { .. } => S::Disconnect,
                            });
                        }

                        ServerCmd::FastUpdate(ent_update) => {
                            // first update signals the last sign-on stage
                            conn.handle_signon(SignOnStage::Done, &client_vars)?;

                            commands.run_system_cached_with(
                                ClientState::update_entity,
                                (ent_update, conn.last_msg_time.as_secs_f64()),
                            );
                        }

                        ServerCmd::Finale { .. } => msg_todo!("intermission"),
                        ServerCmd::FoundSecret => msg_todo!("found secret"),
                        ServerCmd::Intermission => {
                            msg_todo!("intermission");
                        }
                        ServerCmd::KilledMonster => {
                            msg_todo!("killed monster")
                        }

                        ServerCmd::LightStyle { .. } => {
                            msg_todo!("lightstyle")
                        }

                        ServerCmd::Particle { .. } => {
                            msg_todo!("particle")
                        }

                        ServerCmd::Print { text } => console_output.print_alert(text.raw, &time),

                        ServerCmd::SetAngle { angles } => {
                            if let Some(state) = conn.client_state.as_mut()
                                && let Some(view) = state.view.take()
                            {
                                commands.run_system_cached_with(
                                    ClientState::update_entity,
                                    (
                                        EntityUpdate::new_angles(view, angles),
                                        conn.last_msg_time.as_secs_f64(),
                                    ),
                                );
                            }
                        }

                        ServerCmd::SetView { ent_id } => {
                            if let Some(state) = conn.client_state.as_mut() {
                                if let Some(prev_view) = state.view.take()
                                    && let Some(prev_view) =
                                        state.server_entity_to_client_entity.get(&prev_view)
                                    && let Ok(view_entities) = view_entities.get(*prev_view)
                                {
                                    for view_ent in view_entities.iter() {
                                        commands.entity(view_ent).despawn();
                                    }
                                }

                                state.view = Some(ent_id);

                                let mut view_entity = match state
                                    .server_entity_to_client_entity
                                    .get(&ent_id)
                                {
                                    Some(ent) => commands.entity(*ent),
                                    None => {
                                        warn!(
                                            "Server tried to set view on non-existent entity {ent_id}"
                                        );
                                        let entity = commands.spawn((
                                            Transform::default(),
                                            ChildOf(state.worldspawn),
                                        ));
                                        state
                                            .server_entity_to_client_entity
                                            .insert(ent_id, entity.id());
                                        entity
                                    }
                                };

                                // Viewing entity should be invisible (e.g. do not show player model)
                                view_entity.insert((Name::new("player"), Visibility::Hidden));

                                let view_entity = view_entity.id();

                                let mut camera_entity =
                                    match commands.get_entity(settings.camera_template) {
                                        Ok(ent) => ent,
                                        Err(_) => {
                                            if let Ok(ent) = commands
                                                .get_entity(settings.default_camera_template)
                                            {
                                                ent
                                            } else {
                                                warn!("No camera template");
                                                continue;
                                            }
                                        }
                                    };

                                camera_entity
                                    .clone_and_spawn_with_opt_out(|builder| {
                                        builder.deny::<Disabled>();
                                    })
                                    .insert((ViewFor(view_entity), ChildOf(view_entity)));

                                commands.spawn((
                                    ViewFor(view_entity),
                                    build_hud(view_entity, &*asset_server),
                                ));
                            }
                        }

                        ServerCmd::SignOnStage { stage } => {
                            conn.handle_signon(stage, &client_vars)?;
                        }

                        ServerCmd::Sound {
                            volume,
                            attenuation,
                            entity_id,
                            channel,
                            sound_id,
                            position,
                        } => {
                            if let Some(state) = conn.client_state.as_ref() {
                                trace!(
                                    "starting sound with id {} on entity {} channel {}",
                                    sound_id, entity_id, channel
                                );

                                if !state.server_entity_to_client_entity.contains_key(&entity_id) {
                                    error!(
                                        "server tried to start sound on nonexistent entity {}",
                                        entity_id
                                    );
                                    continue;
                                }

                                let entity = state
                                    .server_entity_to_client_entity
                                    .get(&entity_id)
                                    .cloned()
                                    .unwrap_or(state.worldspawn);
                                let entity_origin = transforms
                                    .get(entity)
                                    .map(|trans| trans.translation)
                                    .unwrap_or_default();

                                let origin = position - entity_origin;
                                let volume = volume.unwrap_or(DEFAULT_SOUND_PACKET_VOLUME);
                                let attenuation =
                                    attenuation.unwrap_or(DEFAULT_SOUND_PACKET_ATTENUATION);
                                if let Some(sound) = state.sounds.get(sound_id as usize) {
                                    mixer_events.write(MixerMessage::StartSound(StartSound {
                                        entity,
                                        src: sound.clone(),
                                        ent_channel: channel,
                                        volume: volume as f32 / 255.0,
                                        attenuation,
                                        origin,
                                    }));
                                }
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
                            let msg_time = conn.last_msg_time.as_secs_f64();
                            if let Some(state) = conn.client_state.as_mut() {
                                state.spawn_entities(
                                    commands.reborrow(),
                                    ent_id,
                                    EntityState {
                                        origin,
                                        angles,
                                        model_id: model_id.into(),

                                        // TODO
                                        frame_id: frame_id.into(),
                                        colormap,
                                        skin_id: skin_id.into(),
                                        effects: Default::default(),
                                    },
                                    msg_time,
                                );
                            }
                        }

                        ServerCmd::SpawnStatic { model_id, origin, angles, .. } => {
                            if let Some(state) = conn.client_state.as_mut() {
                                let mut ent = commands.spawn((
                                    Transform::from_translation(origin).with_rotation(
                                        Quat::from_euler(
                                            EulerRot::XYZ,
                                            angles.x,
                                            angles.y,
                                            angles.z,
                                        ),
                                    ),
                                    // TODO: Handle the other fields
                                    Visibility::Inherited,
                                    ChildOf(state.worldspawn),
                                ));

                                if let Some(PrecacheModel::Loaded(model)) =
                                    state.models.get(model_id as usize).cloned()
                                {
                                    ent.insert(SceneRoot(model));
                                }
                            }
                        }

                        ServerCmd::StaticSound { origin, sound_id, volume, attenuation } => {
                            if let Some(state) = conn.client_state.as_ref()
                                && let Some(sound) = state.sounds.get(sound_id as usize)
                            {
                                mixer_events.write(MixerMessage::StartStaticSound(
                                    StartStaticSound {
                                        world: state.worldspawn,
                                        src: sound.clone(),
                                        origin,
                                        volume: volume as f32 / 255.,
                                        attenuation: attenuation as f32 / 64.,
                                    },
                                ));
                            }
                        }

                        ServerCmd::TempEntity { temp_entity: _ } => {
                            msg_todo!("temp entity");
                        }

                        ServerCmd::StuffText { text } => match text.to_str().parse() {
                            Ok(parsed) => {
                                console_commands.write(parsed);
                            }
                            Err(err) => console_output.println(format!("{err}"), &time),
                        },

                        ServerCmd::Time { time } => {
                            let last_msg_time = conn.last_msg_time;
                            if conn.is_connected()
                                && let Some(state) = &mut conn.client_state
                            {
                                if !last_msg_time.is_zero() {
                                    for entity in state.dead_entities() {
                                        commands
                                            .entity(entity)
                                            .insert_recursive::<Children>(Disabled);
                                    }
                                }
                            }

                            conn.last_msg_time = Duration::from_secs_f32(time);
                        }

                        ServerCmd::UpdateColors { player_id: _, new_colors: _ } => {
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

                        ServerCmd::UpdateFrags { player_id: _, new_frags: _ } => {
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

                        ServerCmd::UpdateName { player_id: _, new_name: _ } => {
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

                        ServerCmd::UpdateStat { stat: _, value: _ } => {
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
                            if let Some(state) = conn.client_state.as_ref() {
                                mixer_events.write(MixerMessage::StopSound(StopSound {
                                    entity: state
                                        .server_entity_to_client_entity
                                        .get(&entity_id)
                                        .cloned()
                                        .unwrap_or(state.worldspawn),
                                    ent_channel: channel,
                                }));
                            }
                        }
                        ServerCmd::SellScreen => todo!(),
                    }
                }
            };

            pending_messages.extend(server_cmds);

            Ok(status)
        }

        pub fn advance_frame(
            In(status): In<Result<ConnectionStatus, ClientError>>,
            mut conn: ResMut<Connection>,
            mut to_server: MessageWriter<ClientMessage>,
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
            asset_server: Res<AssetServer>,
            mut demo_queue: ResMut<DemoQueue>,
        ) -> Result<(), ClientError> {
            use ConnectionStatus as S;
            let new_conn = match status? {
                S::Break | S::Maintain => return Ok(()),
                // if client is already disconnected, this is a no-op
                S::Disconnect => None,

                // get the next demo from the queue
                S::NextDemo => demo_queue
                    .next_demo()
                    .map(|demo| NewConnection::Demo(asset_server.load(format!("{demo}.dem")))),
            };

            if let Some(new_conn) = new_conn {
                commands.write_message(new_conn);
            } else {
                commands.write_message(Disconnect);
            }

            Ok(())
        }
    }
}
