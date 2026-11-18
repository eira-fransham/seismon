pub mod commands;
mod cvars;
pub mod demo;
pub mod entity;
mod hud;
pub mod input;
mod interpolation;
mod inventory;
mod loading_screen;
pub mod menu;
mod post_process;
pub mod sound;
pub mod state;
pub mod text;
pub mod trace;
mod view;

use self::{
    input::SeismonInputPlugin,
    menu::{MenuBodyView, MenuBuilder, MenuView},
    sound::{MixerMessage, SeismonSoundPlugin},
};

use std::{
    iter,
    net::ToSocketAddrs,
    ops::Range,
    path::{Path, PathBuf},
    time::Duration,
};

use crate::{
    client::{
        demo::{Demo, DemoError, DemoLoader, DemoServer},
        entity::ClientEntity,
        hud::{HudIntermission, HudPlugin, IntermissionState, LevelStats},
        interpolation::InterpolateApp,
        inventory::InventoryPlugin,
        post_process::ColorShiftPlugin,
        sound::{MusicPlayer, StartSound, StartStaticSound, StopSound},
        state::ClientState,
        text::Conchars,
    },
    common::{
        self, Template,
        console::{ConsoleOutput, SeismonConsoleRenderPlugin, TextAsset, TextLoader},
        net::{
            self, BlockingMode, ClientCmd, ClientMessage, EntityState, EntityUpdate, NetError,
            QSocket, ServerCmd, ServerMessage, SignOnStage,
            connect::{CONNECT_PROTOCOL_VERSION, ConnectSocket, Request, Response},
        },
        wad::{Palette, PaletteLoader, QPicLoader, Wad, WadLoader},
    },
};

use beef::Cow;
use bevy::{
    asset::{AssetServer, LoadState},
    camera::visibility::{Layer, RenderLayers},
    ecs::{
        message::MessageCursor,
        system::{Res, ResMut},
    },
    prelude::*,
    time::{Time, Virtual},
    window::PrimaryWindow,
};
use bevy_mod_billboard::plugin::BillboardPlugin;
use bevy_mod_commands::{ConsoleError, RunCmd};
use bevy_trenchbroom::{TrenchBroomPlugins, config::WriteTrenchBroomConfigOnStartPlugin};
use input::InputFocus;
use menu::Menu;
use seismon_utils::{QAngles, QString};
use serde::Deserialize;
use snafu::Snafu;
use sound::SoundError;
use wgpu::BlendState;

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
    F = Box<dyn Fn(MenuBuilder) -> anyhow::Result<Menu> + Send + Sync + 'static>,
> {
    pub base_dir: Option<PathBuf>,
    pub game: Option<String>,
    pub main_menu: F,
}

fn build_default(builder: MenuBuilder) -> anyhow::Result<Menu> {
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

#[derive(Default, Clone)]
pub struct CameraTemplates {
    /// The camera used to render the world
    pub world: Option<Entity>,
    /// The camera used to render the viewmodel and HUD
    pub overlay: Option<Entity>,
}

#[derive(Clone, Resource)]
pub struct SeismonGameSettings {
    pub base_dir: PathBuf,
    pub game: Option<String>,
    pub camera_templates: CameraTemplates,
    default_world_camera_template: Entity,
    default_overlay_camera_template: Entity,
}

/// The default set of components for the camera used to render the world.
pub fn default_world_camera() -> impl Bundle {
    (
        Name::new("World Camera"),
        Camera3d::default(),
        Projection::Perspective(PerspectiveProjection { fov: 90_f32.to_radians(), ..default() }),
        Transform::from_translation(Vec3::new(0.0, 0.0, 22.0)).with_rotation(Quat::from_euler(
            EulerRot::XYZ,
            std::f32::consts::FRAC_PI_2,
            -std::f32::consts::FRAC_PI_2,
            0.,
        )),
        Visibility::Visible,
        Template,
    )
}

pub const OVERLAY_RENDER_LAYER: Layer = 1;

/// The default set of components for the camera used to render the viewmodel and UI.
pub fn default_overlay_camera() -> impl Bundle {
    (
        Name::new("Overlay Camera"),
        Camera3d::default(),
        Camera2d,
        Camera {
            order: 1,
            clear_color: ClearColorConfig::Custom(Color::srgba(0., 0., 0., 0.)),
            output_mode: bevy::camera::CameraOutputMode::Write {
                blend_state: Some(BlendState::ALPHA_BLENDING),
                clear_color: ClearColorConfig::None,
            },
            ..Default::default()
        },
        IsDefaultUiCamera,
        RenderLayers::layer(OVERLAY_RENDER_LAYER),
        // Approximated from Source engine default FoV
        Projection::Perspective(PerspectiveProjection { fov: 54_f32.to_radians(), ..default() }),
        Visibility::Visible,
        Template,
    )
}

#[derive(States, Default, Debug, Clone, PartialEq, Eq, Hash, Reflect)]
enum ClientGameState {
    #[default]
    Disconnected,
    WaitForServer,
    Loading,
    InGame,
}

impl<F> Plugin for SeismonClientPlugin<F>
where
    F: Fn(MenuBuilder) -> anyhow::Result<Menu> + Clone + Send + Sync + 'static,
{
    fn build(&self, app: &mut bevy::prelude::App) {
        if let Ok(menu) = (self.main_menu)(MenuBuilder::new(app.world_mut())) {
            app.insert_resource(menu);
        }

        let base_dir = self.base_dir.clone().unwrap_or_else(common::default_base_dir);

        // The binary is expected to have checked this already (see `check_game_dirs`), but don't
        // kill the process from inside a plugin if it hasn't.
        if let Err(e) = check_game_dirs(&base_dir, self.game.as_deref()) {
            warn!("{e}");
        }

        let default_world_camera_template = app.world_mut().spawn(default_world_camera()).id();
        let default_overlay_camera_template = app.world_mut().spawn(default_overlay_camera()).id();

        app.register_disabling_component::<Template>();

        let app = app
            .init_asset::<Palette>()
            .init_asset::<Wad>()
            .init_asset::<TextAsset>()
            .init_asset_loader::<QPicLoader>()
            .init_asset_loader::<PaletteLoader>()
            .init_asset_loader::<WadLoader>()
            // TODO: We need to do this here to prevent the server and client from both trying to register it,
            // but realistically we need a better solution. The client is the main focus for now, so we'll put
            // it here, but eventually we'll need to fix it. We should be able to run the client without the
            // server and vice versa. Maybe something like `SeismonCorePlugin` which gets added to the app
            // before `SeismonClientPlugin`?
            .init_asset_loader::<TextLoader>()
            .interpolate_component::<Transform, Virtual>()
            .insert_resource(SeismonGameSettings {
                base_dir,
                game: self.game.clone(),
                camera_templates: Default::default(),
                default_world_camera_template,
                default_overlay_camera_template,
            })
            .init_state::<ClientGameState>()
            .init_state::<SignOn>()
            .init_resource::<Conchars>()
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
            // TODO: Extract to HUD plugin
            .add_observer(observers::apply_render_layers_to_children)
            .add_systems(Startup, self::loading_screen::init_loading_screen)
            .add_systems(
                Last,
                self::loading_screen::update_loading_screen_visible
                    .run_if(state_changed::<ClientGameState>),
            )
            .add_systems(
                PreUpdate,
                (systems::disconnect.run_if(resource_exists::<Connection>), systems::connect)
                    .chain()
                    .run_if(on_message::<Disconnect>.or_else(on_message::<NewConnection>)),
            )
            .add_systems(
                PreUpdate,
                systems::wait_for_load.run_if(in_state(ClientGameState::Loading)),
            )
            .add_systems(
                PreUpdate,
                systems::process_network_messages
                    .pipe(|In(res), mut commands: Commands| {
                        if let Err(e) = res {
                            error!("Error handling frame: {}", e);
                            commands.write_message(Disconnect);
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
                        .pipe(|In(res), mut commands: Commands| {
                            if let Err(e) = res {
                                error!("Error handling frame: {e}");
                                commands.write_message(Disconnect);
                            }
                        }),
                    systems::send_input_to_server.pipe(|In(res)| {
                        if let Err(e) = res {
                            error!("Error handling input: {e}");
                        }
                    }),
                )
                    .run_if(
                        resource_exists::<Connection>.and_then(
                            in_state(ClientGameState::WaitForServer)
                                .or_else(in_state(ClientGameState::InGame)),
                        ),
                    ),
            )
            .add_systems(
                Update,
                systems::lock_cursor.run_if(
                    state_changed::<InputFocus>
                        .or_else(resource_changed::<bevy_mod_commands::Registry>),
                ),
            )
            .add_systems(PostUpdate, text::systems::update_atlas_text)
            .add_systems(
                PostUpdate,
                systems::apply_local_view_angles
                    .run_if(resource_exists::<Connection>)
                    .before(TransformSystems::Propagate),
            )
            .add_systems(
                PostUpdate,
                view::apply_view_effects
                    .after(systems::apply_local_view_angles)
                    .before(TransformSystems::Propagate),
            )
            .add_systems(
                PostUpdate,
                systems::handle_signon
                    .pipe(|In(res)| {
                        if let Err(e) = res {
                            error!("Error handling signon: {e}");
                        }
                    })
                    .run_if(resource_exists::<Connection>.and_then(state_changed::<SignOn>)),
            )
            .add_systems(
                PostUpdate,
                systems::enter_game.run_if(
                    state_changed::<ClientGameState>.and_then(in_state(ClientGameState::InGame)),
                ),
            )
            .add_plugins(HudPlugin::default())
            .add_plugins(SeismonConsoleRenderPlugin)
            .add_plugins(SeismonSoundPlugin)
            .add_plugins(SeismonInputPlugin)
            .add_plugins(InventoryPlugin::default())
            .add_plugins(
                TrenchBroomPlugins(Default::default())
                    .build()
                    .disable::<WriteTrenchBroomConfigOnStartPlugin>(),
            )
            .add_plugins(BillboardPlugin);

        // `cfg!(..)` instead of `#[cfg(..)]` so that it's not marked unused.
        if cfg!(feature = "color-shift") {
            app.add_plugins(ColorShiftPlugin::default());
        }

        cvars::register_cvars(app);
        commands::register_commands(app);
    }
}

/// A game directory needed by [`SeismonClientPlugin`] is missing.
#[derive(Snafu, Debug)]
#[snafu(display(
    "`{game}/` directory does not exist in `{}`! Use the `--base-dir` option with the name of the \
     directory which contains `{game}/`.",
    base_dir.display()
))]
pub struct MissingGameDirError {
    base_dir: PathBuf,
    game: String,
}

/// Checks that `base_dir` contains `id1/` and, if given, `game/`.
pub fn check_game_dirs(base_dir: &Path, game: Option<&str>) -> Result<(), MissingGameDirError> {
    for game in iter::once("id1").chain(game) {
        if !base_dir.join(game).is_dir() {
            return Err(MissingGameDirError {
                base_dir: base_dir.to_owned(),
                game: game.to_owned(),
            });
        }
    }

    Ok(())
}

#[derive(Snafu, Debug)]
pub enum ClientError {
    #[snafu(display("Connection rejected: {message}"))]
    ConnectionRejected { message: String },
    #[snafu(context(false), display("Couldn't read cvar value: {source}"))]
    Cvar { source: ConsoleError },
    #[snafu(display("Server sent an invalid port number ({port})"))]
    InvalidConnectPort { port: i32 },
    #[snafu(display("Server sent an invalid connect response"))]
    InvalidConnectResponse,
    #[snafu(display("Invalid server address"))]
    InvalidServerAddress,
    #[snafu(display("No response from server"))]
    NoResponse,
    #[snafu(display("Unrecognized protocol: {version}"))]
    UnrecognizedProtocol { version: i32 },
    #[snafu(display("Client is not connected"))]
    NotConnected,
    #[snafu(display("Client has already signed on"))]
    AlreadySignedOn,
    #[snafu(display("No client with ID {id}"))]
    NoSuchClient { id: usize },
    #[snafu(display("No player with ID {id}"))]
    NoSuchPlayer { id: usize },
    #[snafu(display("No entity with ID {id}"))]
    NoSuchEntity { id: usize },
    #[snafu(display("Null entity access"))]
    NullEntity,
    #[snafu(display("Entity already exists: {id}"))]
    EntityExists { id: usize },
    #[snafu(display("Invalid view entity: {id}"))]
    InvalidViewEntity { id: usize },
    #[snafu(display("Too many static entities"))]
    TooManyStaticEntities,
    #[snafu(display("No such lightmap animation: {id}"))]
    NoSuchLightmapAnimation { id: usize },
    // TODO: wrap PlayError
    #[snafu(display("Failed to open audio output stream"))]
    OutputStream,
    #[snafu(context(false), display("Demo server error: {source}"))]
    DemoServer { source: DemoError },
    #[snafu(context(false), display("Network error: {source}"))]
    Network { source: NetError },
    #[snafu(context(false), display("Failed to load sound: {source}"))]
    Sound { source: SoundError },
}

#[derive(Deserialize, Copy, Clone, Debug)]
pub struct MoveVars {
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
    fn into_cmd(self, view_id: Option<u16>) -> ServerCmd {
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

// TODO: `Result<Result<..>>` is ugly here.
type RecvResult<'a> = Result<Result<ServerUpdate<Cow<'a, [u8]>>, ConnectionStatus>, ClientError>;

impl ConnectionTarget {
    fn recv<'a>(
        &mut self,
        assets: &AssetServer,
        demos: &'a Assets<Demo>,
        events: &Messages<ServerMessage>,
    ) -> RecvResult<'a> {
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
                    LoadState::Loading => return Ok(Err(ConnectionStatus::Maintain)),
                    LoadState::NotLoaded => return Ok(Err(ConnectionStatus::NextDemo)),
                    LoadState::Failed(err) => {
                        return Err(DemoError::from(err).into());
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
    Server(Option<Box<QSocket>>, ConnectionStage),
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
                ClientCmd::StringCmd { cmd: format!("name \"{}\"\n", client_vars.name).into() }
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
        Err(_) => InvalidServerAddressSnafu.fail(),
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
                InvalidConnectPortSnafu { port: accept.port }.fail()?;
            }

            debug!("Connection accepted on port {}", accept.port);
            accept.port as u16
        }

        // our request was rejected.
        Response::Reject(reject) => {
            ConnectionRejectedSnafu { message: reject.message.into_string() }.fail()?
        }

        // the server sent back a response that doesn't make sense here (i.e. something other
        // than an Accept or Reject).
        _ => InvalidConnectResponseSnafu.fail()?,
    };

    let mut new_addr = server_addr;
    new_addr.set_port(port);

    // we're done with the connection socket, so turn it into a QSocket with the new address
    let qsock = con_sock.into_qsocket(new_addr);

    Ok((qsock, ConnectionStage::SignOn(SignOnStage::Prespawn)))
}

#[derive(Message)]
pub struct Impulse(pub u8);

#[derive(States, Clone, Default, PartialEq, Eq, Hash, Debug)]
pub struct SignOn(pub SignOnStage);

mod observers {
    use bevy::{
        camera::visibility::RenderLayers,
        ecs::{
            hierarchy::Children,
            observer::On,
            query::Without,
            system::{Commands, Query},
        },
        transform::components::Transform,
        world_serialization::WorldInstanceReady,
    };

    // Somewhat hacky, but applies the `RenderLayers` of a parent entity to its children from a scene
    pub fn apply_render_layers_to_children(
        trigger: On<WorldInstanceReady>,
        mut commands: Commands,
        children: Query<&Children>,
        transforms: Query<&Transform, Without<RenderLayers>>,
        query: Query<&RenderLayers>,
    ) {
        let Ok(render_layers) = query.get(trigger.entity) else {
            return;
        };
        children.iter_descendants(trigger.entity).for_each(|entity| {
            if transforms.contains(entity) {
                commands.entity(entity).insert(render_layers.clone());
            }
        });
    }
}

mod systems {
    use bevy::window::{CursorGrabMode, CursorOptions};
    use bevy_trenchbroom::bsp::Bsp;
    use common::net::MessageKind;

    use crate::{
        client::{state::Worldspawn, view::MouseVars},
        common::net::ButtonFlags,
    };

    use bevy_mod_commands::Registry;

    use super::*;

    pub fn enter_game(
        mut visibility: Query<&mut Visibility>,
        mut time: ResMut<Time<Virtual>>,
        conn: Res<Connection>,
    ) {
        let Some(state) = &conn.client_state else {
            return;
        };

        let max_delta = time.max_delta();
        *time = Time::from_max_delta(max_delta);
        time.advance_to(conn.last_msg_time.saturating_sub(max_delta));

        #[cfg(feature = "dev-tools")]
        time.pause();

        *visibility.get_mut(state.worldspawn).unwrap() = Visibility::Visible;
    }

    pub fn handle_signon(
        mut commands: Commands,
        stage: Res<State<SignOn>>,
        mut conn: ResMut<Connection>,
        registry: Res<Registry>,
    ) -> Result<(), ClientError> {
        // `SignOn` starts as `Not`, and this runs the first time a connection exists. We haven't
        // received a sign-on stage from the server yet, so there's nothing to do.
        if stage.0 == SignOnStage::Not {
            return Ok(());
        }

        // `serde_lexpr` doesn't allow us to configure deserialising strings and doesn't
        // recognise symbols as valid strings, so we need to use
        // `.value().as_name()` and can't use `read_cvars`.
        // TODO: Use `nu` (https://github.com/eira-fransham/seismon/issues/44)
        let client_vars: ClientVars = ClientVars {
            name: registry
                .get_cvar("_cl_name")
                .ok_or_else(|| ConsoleError::CvarParseInvalid {
                    backtrace: snafu::Backtrace::capture(),
                })?
                .value()
                .as_name()
                .unwrap_or("player"),
            color: registry.read_cvar("_cl_color")?,
        };

        if stage.0 == SignOnStage::Done {
            commands.set_state_if_different(ClientGameState::InGame);
        }

        conn.handle_signon(stage.0, &client_vars)?;

        Ok(())
    }

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
        registry: ResMut<Registry>,
        mut conn: ResMut<Connection>,
        frame_time: Res<Time<Virtual>>,
        mut client_events: MessageWriter<ClientMessage>,
        mut impulses: MessageReader<Impulse>,
    ) -> Result<(), ClientError> {
        fn handle_input(
            registry: &Registry,
            frame_time: Duration,
            look_angles: &mut QAngles,
            move_vars: MoveVars,
            mouse_vars: MouseVars,
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
                    move_vars.cl_forwardspeed * f32::from(registry.is_pressed("forward"));
                forwardmove -= move_vars.cl_backspeed * f32::from(registry.is_pressed("back"));
            }

            if registry.is_pressed("speed") {
                sidemove *= move_vars.cl_movespeedkey;
                upmove *= move_vars.cl_movespeedkey;
                forwardmove *= move_vars.cl_movespeedkey;
            }

            let mut button_flags = ButtonFlags::empty();

            for (action, flag) in [
                ("attack", ButtonFlags::ATTACK),
                ("jump", ButtonFlags::JUMP),
                ("use", ButtonFlags::USE),
            ] {
                if registry.is_pressed(action) {
                    button_flags |= flag;
                }
            }

            if mlook {
                let pitch_factor = mouse_vars.pitch_factor * mouse_vars.sensitivity;
                let yaw_factor = mouse_vars.yaw_factor * mouse_vars.sensitivity;
                match registry.read_cvar::<Vec2>("mousedelta") {
                    Ok(mouse_move) => {
                        let mouse_move = mouse_move * Vec2::new(yaw_factor, pitch_factor);
                        look_angles.yaw_deg -= mouse_move.x;
                        look_angles.pitch_deg -= mouse_move.y;

                        look_angles.yaw_deg %= 360.;
                        look_angles.pitch_deg = look_angles.pitch_deg.clamp(-90., 90.);
                    }
                    Err(e) => {
                        error!("Could not read mouse delta: {e}");
                    }
                }
            }

            Some(ClientCmd::Move {
                delta_time: frame_time,
                angles: *look_angles,
                fwd_move: forwardmove as i16,
                side_move: sidemove as i16,
                up_move: upmove as i16,
                button_flags,
                // TODO: Is `impulse 0` correct?
                impulse: impulse.unwrap_or_default(),
            })
        }

        let Connection {
            client_state,
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

        let mut dummy_look_angles = QAngles::default();

        let look_angles =
            client_state.as_mut().map(|s| &mut s.look_angles).unwrap_or(&mut dummy_look_angles);

        let Some(move_cmd) = handle_input(
            &registry,
            frame_time.delta(),
            look_angles,
            move_vars,
            mouse_vars,
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

    /// Writes the client's own look angles to the view entity's `Transform`, bypassing
    /// interpolation. This only applies when connected to a server (including listen
    /// servers), as view angles should be interpolated for demos.
    pub fn apply_local_view_angles(conn: Res<Connection>, mut transforms: Query<&mut Transform>) {
        let Connection {
            client_state: Some(state),
            target: ConnectionTarget::Server { stage: ConnectionStage::Connected, .. },
            ..
        } = &*conn
        else {
            return;
        };

        let Some(&view) = state.view.and_then(|id| state.server_entity_to_client_entity.get(&id))
        else {
            return;
        };

        if let Ok(mut transform) = transforms.get_mut(view) {
            transform.rotation = state.look_angles.into();
        }
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

    pub fn wait_for_load(
        mut commands: Commands,
        mut conn: ResMut<Connection>,
        bsp_assets: Res<Assets<Bsp>>,
        worldspawn: Query<&Worldspawn>,
    ) {
        let Some(client_state) = &mut conn.client_state else {
            commands.set_state_if_different(ClientGameState::Disconnected);
            return;
        };

        let Ok(worldspawn) = worldspawn.get(client_state.worldspawn) else {
            commands.set_state_if_different(ClientGameState::Disconnected);
            return;
        };

        if let Some(bsp) = bsp_assets.get(&worldspawn.bsp) {
            commands.entity(client_state.worldspawn).insert(bsp.transform);
            client_state.populate_precache(bsp);

            commands.set_state_if_different(ClientGameState::WaitForServer);
        }
    }

    pub fn disconnect(mut commands: Commands, conn: Res<Connection>) {
        if let Some(state) = &conn.client_state
            && let Ok(mut worldspawn) = commands.get_entity(state.worldspawn)
        {
            worldspawn.try_despawn();
        }

        commands.remove_resource::<Connection>();
        commands.remove_resource::<QSocket>();
        commands.set_state_if_different(ClientGameState::Disconnected);

        // don't allow game focus when disconnected
        commands.set_state_if_different(InputFocus::Console);
    }

    pub fn connect(mut commands: Commands, mut connections: MessageReader<NewConnection>) {
        // If we request multiple connections, we only care about the last one.
        let Some(new_connection) = connections.read().last() else {
            return;
        };

        if new_connection.socket().is_some() {
            todo!("Need to remove connection from the command and put it here");
        }

        // TODO: The server should be able to communicate this to the client, but the
        // default is 20fps
        commands.insert_resource(Time::from_max_delta(Duration::from_millis(1000 / 20)));
        commands.insert_resource(Connection::from(new_connection));
        commands.set_state_if_different(ClientGameState::WaitForServer);
        commands.set_state_if_different(InputFocus::Game);
    }

    pub mod frame {
        use std::collections::VecDeque;

        use bevy::ecs::entity_disabling::Disabled;

        use crate::client::{hud::MakeHud, state::PrecacheModel};

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
            time: ResMut<Time<Virtual>>,
            mut server_cmds: MessageWriter<ServerCmd>,
            server_events: Res<Messages<ServerMessage>>,
            asset_server: Res<AssetServer>,
            demos: Res<Assets<Demo>>,
            state: Res<State<ClientGameState>>,
        ) -> Result<ConnectionStatus, ClientError> {
            // Fast-forward until we get to the point where we're actually in-game
            if *state.get() == ClientGameState::InGame && time.elapsed() < conn.last_msg_time {
                return Ok(ConnectionStatus::Maintain);
            }

            // TODO: Shuld "wait for demo load" logic be in its own system?
            let server_update = match conn.target.recv(&asset_server, &demos, &server_events)? {
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
                        .into_cmd(conn.client_state.as_ref().and_then(|s| s.view)),
                ),
                Ok(None) => None,
            });
            server_cmds.write_batch(cmd_iter);

            Ok(ConnectionStatus::Maintain)
        }

        // TODO: Should we split server messages into separate pieces? Many (e.g. `StartSound`, `StuffText`)
        // don't actually affect one another and it'd be nice to make the argument list smaller.
        #[expect(clippy::too_many_arguments)]
        pub fn execute_server_msg(
            In(status): In<Result<ConnectionStatus, ClientError>>,
            mut pending_messages: Local<VecDeque<ServerCmd>>,
            mut commands: Commands,
            mut conn: ResMut<Connection>,
            mut server_cmds: MessageReader<ServerCmd>,
            mut console_output: ResMut<ConsoleOutput>,
            settings: Res<SeismonGameSettings>,
            wall_time: Res<Time<Real>>,
            asset_server: Res<AssetServer>,
            transforms: Query<&Transform>,
            mut level_stats: ResMut<LevelStats>,
            mut intermission: ResMut<HudIntermission>,
        ) -> Result<ConnectionStatus, ClientError> {
            use ConnectionStatus as S;

            match status? {
                S::Maintain => {}
                s => return Ok(s),
            }

            macro_rules! msg_todo {
                ($cmd_name:expr) => {{ debug!("TODO: {}", $cmd_name) }};
            }

            let mut server_cmds = server_cmds.read().cloned();

            let status = loop {
                let Some(cmd) = pending_messages.pop_front().or_else(|| server_cmds.next()) else {
                    break S::Maintain;
                };

                if !matches!(
                    cmd,
                    ServerCmd::FastUpdate(..) | ServerCmd::Time { .. } | ServerCmd::PlayerData(..)
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
                            UnrecognizedProtocolSnafu { version: protocol_version }.fail()?;
                        }

                        // New level: reset the level stats (`cl.levelname` is the server's
                        // message) and leave any intermission.
                        *level_stats = LevelStats { level_name: message.clone(), ..default() };
                        intermission.0 = None;

                        if !message.raw.is_empty() {
                            console_output.println_alert(CONSOLE_DIVIDER, &wall_time);
                            console_output.println_alert(message.raw, &wall_time);
                            console_output.println_alert(CONSOLE_DIVIDER, &wall_time);
                        }

                        if let Some(state) = conn.client_state.as_ref() {
                            commands.entity(state.worldspawn).despawn();
                        }

                        let mut new_worldspawn = commands.spawn(Visibility::Hidden);

                        conn.client_state = Some(ClientState::from_server_info(
                            &mut new_worldspawn,
                            &asset_server,
                            model_precache,
                            sound_precache,
                        )?);

                        commands.set_state_if_different(ClientGameState::Loading);
                        break S::Maintain;
                    }

                    ServerCmd::Bad => {
                        warn!("Invalid command from server")
                    }

                    ServerCmd::NoOp => {}

                    ServerCmd::CdTrack { track, .. } => {
                        commands.write_message(MixerMessage::StartMusic(Some(
                            sound::MusicSource::TrackId(track as usize),
                        )));
                    }

                    ServerCmd::CenterPrint { text } => {
                        console_output.set_center_print(text, &wall_time);
                    }

                    ServerCmd::PlayerData(player_data) => {
                        commands.run_system_cached_with(ClientState::update_player, player_data);
                    }

                    ServerCmd::Cutscene { text } => {
                        // TODO: Typewriter reveal of the text (`SCR_DrawCenterString` in finale
                        // mode)
                        console_output.set_center_print(text.clone(), &wall_time);
                        intermission.0 = Some(IntermissionState {
                            kind: IntermissionKind::Cutscene { text },
                            completed_secs: conn.last_msg_time.as_secs_f32(),
                        });
                    }

                    ServerCmd::Damage { armor, blood, source } => {
                        commands
                            .run_system_cached_with(view::handle_damage, (armor, blood, source));
                    }

                    ServerCmd::Disconnect => {
                        return Ok(match &conn.target {
                            ConnectionTarget::Demo(_) => S::NextDemo,
                            ConnectionTarget::Server { .. } => S::Disconnect,
                        });
                    }

                    ServerCmd::FastUpdate(ent_update) => {
                        // first update signals the last sign-on stage
                        commands.set_state_if_different(SignOn(SignOnStage::Done));

                        commands.run_system_cached_with(
                            ClientState::update_entity,
                            (ent_update, conn.last_msg_time.as_secs_f64()),
                        );
                    }

                    ServerCmd::Finale { text } => {
                        // TODO: Typewriter reveal of the text (`SCR_DrawCenterString` in finale
                        // mode)
                        console_output.set_center_print(text.clone(), &wall_time);
                        intermission.0 = Some(IntermissionState {
                            kind: IntermissionKind::Finale { text },
                            completed_secs: conn.last_msg_time.as_secs_f32(),
                        });
                    }
                    ServerCmd::FoundSecret => level_stats.secrets += 1,
                    ServerCmd::Intermission => {
                        intermission.0 = Some(IntermissionState {
                            kind: IntermissionKind::Intermission,
                            completed_secs: conn.last_msg_time.as_secs_f32(),
                        });
                    }
                    ServerCmd::KilledMonster => level_stats.monsters += 1,

                    ServerCmd::LightStyle { .. } => {
                        msg_todo!("lightstyle")
                    }

                    ServerCmd::Particle { .. } => {
                        msg_todo!("particle")
                    }

                    ServerCmd::Print { text } => console_output.print_alert(text.raw, &wall_time),

                    ServerCmd::SetAngle { angles } => {
                        let is_demo = matches!(conn.target, ConnectionTarget::Demo(_));

                        // Applied to the view entity's `Transform` by `apply_local_view_angles`.
                        if let Some(state) = conn.client_state.as_mut() {
                            // TODO: This feels a bit hacky, but ensures that view angles are interpolated in demos
                            if is_demo && state.view.is_some() {
                                commands.run_system_cached_with(
                                    ClientState::set_view_angles,
                                    (angles, conn.last_msg_time.as_secs_f64()),
                                );
                            } else {
                                state.look_angles = angles;
                            }
                        }
                    }

                    ServerCmd::SetView { ent_id } => {
                        if let Some(state) = conn.client_state.as_mut() {
                            if let Some(prev_view) = state.view.take()
                                && let Some(prev_view) =
                                    state.server_entity_to_client_entity.get(&prev_view)
                            {
                                commands.entity(*prev_view).despawn_related::<ViewEntities>();
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
                                    let entity = commands
                                        .spawn((Transform::default(), ChildOf(state.worldspawn)));
                                    state
                                        .server_entity_to_client_entity
                                        .insert(ent_id, entity.id());
                                    entity
                                }
                            };

                            // Viewing entity should be invisible (e.g. do not show player model)
                            view_entity
                                .insert(Visibility::Hidden)
                                .insert_if_new(view::ViewEffects::default());

                            let view_entity = view_entity.id();

                            let mut world_camera_entity = match settings
                                .camera_templates
                                .world
                                .and_then(|cam| commands.get_entity(cam).ok())
                            {
                                Some(ent) => ent,
                                None => {
                                    if let Ok(ent) =
                                        commands.get_entity(settings.default_world_camera_template)
                                    {
                                        ent
                                    } else {
                                        warn!("No world camera template");
                                        continue;
                                    }
                                }
                            };

                            world_camera_entity
                                .clone_and_spawn_with_opt_out(|builder| {
                                    builder.deny::<Template>();
                                })
                                .insert((ViewFor(view_entity), ChildOf(view_entity)))
                                .queue(view::capture_view_base);

                            let mut overlay_camera_entity = match settings
                                .camera_templates
                                .overlay
                                .and_then(|cam| commands.get_entity(cam).ok())
                            {
                                Some(ent) => ent,
                                None => {
                                    if let Ok(ent) = commands
                                        .get_entity(settings.default_overlay_camera_template)
                                    {
                                        ent
                                    } else {
                                        warn!("No overlay camera template");
                                        continue;
                                    }
                                }
                            };

                            overlay_camera_entity
                                .clone_and_spawn_with_opt_out(|builder| {
                                    builder.deny::<Template>();
                                })
                                .insert((ViewFor(view_entity), ChildOf(state.worldspawn)));

                            commands.queue(MakeHud {
                                owner: view_entity,
                                components: ViewFor(view_entity),
                            });
                        }
                    }

                    ServerCmd::SignOnStage { stage } => {
                        commands.set_state_if_different(SignOn(stage));
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

                            let entity = state
                                .server_entity_to_client_entity
                                .get(&entity_id)
                                .cloned()
                                .unwrap_or_else(|| {
                                    warn!(
                                        "server tried to start sound on nonexistent entity {}",
                                        entity_id
                                    );

                                    state.worldspawn
                                });
                            let entity_origin = transforms
                                .get(entity)
                                .map(|trans| trans.translation)
                                .unwrap_or_default();

                            let origin = position - entity_origin;
                            let volume = volume.unwrap_or(DEFAULT_SOUND_PACKET_VOLUME);
                            let attenuation =
                                attenuation.unwrap_or(DEFAULT_SOUND_PACKET_ATTENUATION);
                            if let Some(sound) = state.sounds.get(sound_id as usize) {
                                commands.write_message(MixerMessage::StartSound(StartSound {
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

                    ServerCmd::StaticSound { origin, sound_id, volume, attenuation } => {
                        if let Some(state) = conn.client_state.as_ref()
                            && let Some(sound) = state.sounds.get(sound_id as usize)
                        {
                            commands.write_message(MixerMessage::StartStaticSound(
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
                                    model_id,
                                    frame_id,
                                    colormap,
                                    skin_id,
                                    // TODO
                                    effects: Default::default(),
                                },
                                msg_time,
                            );
                        }
                    }

                    ServerCmd::SpawnStatic { model_id, origin, angles, .. } => {
                        if let Some(state) = conn.client_state.as_mut() {
                            let mut ent = commands.spawn((
                                // Same conversion as dynamic entities: `angles` is a Quake
                                // pitch/yaw/roll vector in degrees.
                                Transform::from_translation(origin)
                                    .with_rotation(QAngles::from(angles.to_array()).into()),
                                // TODO: Handle the other fields
                                Visibility::Inherited,
                                ChildOf(state.worldspawn),
                            ));

                            if let Some(PrecacheModel::Loaded(model)) =
                                state.models.get(model_id as usize).cloned()
                            {
                                ent.insert(WorldAssetRoot(model));
                            }
                        }
                    }

                    ServerCmd::TempEntity { temp_entity: _ } => {
                        msg_todo!("temp entity");
                    }

                    ServerCmd::StuffText { text } => {
                        match text.to_str().parse::<RunCmd<'static>>() {
                            Ok(parsed) => {
                                commands.write_message(parsed);
                            }
                            Err(err) => console_output.println(format!("{err}"), &wall_time),
                        }
                    }

                    ServerCmd::Time { time } => {
                        let last_msg_time = conn.last_msg_time;
                        if conn.is_connected()
                            && let Some(state) = &mut conn.client_state
                            && !last_msg_time.is_zero()
                        {
                            for entity in state.drain_dead_entities() {
                                commands.entity(entity).insert_recursive::<Children>(Disabled);
                            }
                        }

                        conn.last_msg_time = Duration::from_secs_f32(time);
                    }

                    ServerCmd::UpdateColors { player_id: _, new_colors: _ } => {
                        msg_todo!("update colors");
                    }

                    ServerCmd::UpdateFrags { player_id: _, new_frags: _ } => {
                        msg_todo!("update frags");
                    }
                    ServerCmd::UpdateName { player_id: _, new_name: _ } => {
                        msg_todo!("update name");
                    }

                    ServerCmd::UpdateStat { stat, value } => {
                        use net::ClientStat as Stat;

                        match stat {
                            Stat::TotalSecrets => level_stats.total_secrets = value,
                            Stat::TotalMonsters => level_stats.total_monsters = value,
                            Stat::FoundSecrets => level_stats.secrets = value,
                            Stat::KilledMonsters => level_stats.monsters = value,
                            // The rest are sent in `PlayerData` by Quake servers.
                            stat => msg_todo!(format!("Update stat {stat:?}")),
                        }
                    }

                    ServerCmd::Version { version } => {
                        if version != net::PROTOCOL_VERSION as i32 {
                            return UnrecognizedProtocolSnafu { version }.fail();
                        }
                    }

                    ServerCmd::SetPause { .. } => {}

                    ServerCmd::StopSound { entity_id, channel } => {
                        if let Some(state) = conn.client_state.as_ref() {
                            commands.write_message(MixerMessage::StopSound(StopSound {
                                entity: state
                                    .server_entity_to_client_entity
                                    .get(&entity_id)
                                    .cloned()
                                    .unwrap_or(state.worldspawn),
                                ent_channel: channel,
                            }));
                        }
                    }
                    ServerCmd::SellScreen => {
                        msg_todo!("Display upsell screen");
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

            Ok(ConnectionStatus::Maintain)
        }

        pub fn end_frame(
            // TODO: This can almost certainly be simplified.
            In(status): In<Result<ConnectionStatus, ClientError>>,
            mut commands: Commands,
            asset_server: Res<AssetServer>,
            mut demo_queue: ResMut<DemoQueue>,
        ) -> Result<(), ClientError> {
            use ConnectionStatus as S;
            let new_conn = match status? {
                S::Maintain => return Ok(()),
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
