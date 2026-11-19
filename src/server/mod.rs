// Copyright © 2018 Cormac O'Brien.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software
// and associated documentation files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or
// substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
// BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

mod ai;
mod commands;
mod cvars;
pub mod error;
pub mod math;
#[doc(hidden)]
pub mod parity;
mod physics;
pub mod precache;
pub mod qc;
mod rng;
mod spawn;
pub mod world;

use std::time::Duration;

use bevy_mod_commands::{Registry, RunCmd};
use bevy_mod_scripting::asset::ScriptAsset;
use bevy_mod_scripting_qcvm::{QCFunction, QCString};

use crate::{
    common::{
        console::SeismonConsoleCorePlugin,
        net::{ClientCmd, ClientMessage, EntityState, ServerCmd, ServerMessage},
    },
    server::{
        error::ServerError,
        math::{AngleVectors, angle_vectors},
        qc::{
            edict::{Edict, EdictTable},
            fields::{
                EntAi, EntCallbacks, EntInfo, EntPhysics, EntPlayer, EntRender, QcTables,
                qc_entity_bundle,
            },
            globals::QcGlobals,
        },
        world::{
            LevelCollision,
            hull::BspLeafContents,
            phys::{CollideKind, Trace},
        },
    },
};

use self::{precache::Precache, rng::ServerRng};

use anyhow::bail;
use arrayvec::ArrayVec;
use bevy::prelude::*;
use bevy_trenchbroom::bsp::Bsp;
use hashbrown::HashMap;
use seismon_utils::{QString, duration_to_f32};
use serde::Deserialize;
use snafu::{Backtrace, Report};

use crate::common::net::SignOnStage;

/// The destination of a message in the `Write*` family of QuakeC fns
#[derive(Default, Copy, Clone, PartialEq, Eq)]
enum MsgDest {
    /// Unreliable messages sent to all clients
    #[default]
    Broadcast = 0,
    /// Reliable messages sent to a specific clients (specified by `msg_entity`).
    One = 1,
    /// Reliable messages sent to all clients.
    All = 2,
    /// Write to init string - this is sent on sign-on to all clients.
    Init = 3,
}

impl TryFrom<usize> for MsgDest {
    type Error = ServerError;

    fn try_from(value: usize) -> std::result::Result<Self, Self::Error> {
        const MSG_BROADCAST: usize = MsgDest::Broadcast as usize;
        const MSG_ONE: usize = MsgDest::One as usize;
        const MSG_ALL: usize = MsgDest::All as usize;
        const MSG_INIT: usize = MsgDest::Init as usize;

        match value {
            MSG_BROADCAST => Ok(MsgDest::Broadcast),
            MSG_ONE => Ok(MsgDest::One),
            MSG_ALL => Ok(MsgDest::All),
            MSG_INIT => Ok(MsgDest::Init),
            _ => Err(ServerError::with_msg(format!("Invalid message dest: {value}"))),
        }
    }
}

/// Tick rate for the server.
///
/// > TODO: Make this a cvar
const TICK_RATE: f32 = 0.05;

#[derive(Default, Copy, Clone)]
pub struct SeismonServerPlugin;

/// Notify the local client that the server has initiated a level load.
#[derive(Message, Debug, Clone, Copy)]
pub struct LocalLevelStarted;

impl Plugin for SeismonServerPlugin {
    fn build(&self, app: &mut App) {
        // The console is shared with the client if both are running in the same app.
        if !app.is_plugin_added::<SeismonConsoleCorePlugin>() {
            app.add_plugins(SeismonConsoleCorePlugin::new());
        }

        assert!(
            app.world().contains_resource::<Assets<Bsp>>(),
            // TODO: We can probably just re-register the asset or even simply re-add
            // `TrenchBroomPlugins`.
            "`TrenchBroomPlugins` should be added before `SeismonServerPlugin`",
        );

        qc::build(app);

        app.add_systems(FixedPreUpdate, systems::recv_client_messages)
            .add_systems(
                Update,
                systems::advance_pending_level.run_if(resource_exists::<PendingLevel>),
            )
            .add_systems(
                FixedUpdate,
                (systems::server_spawn, systems::server_update)
                    .chain()
                    .run_if(resource_exists::<Session>),
            )
            .add_message::<ClientMessage>()
            .add_message::<ServerMessage>()
            .add_message::<LocalLevelStarted>()
            .insert_resource(Time::<Fixed>::from_seconds(TICK_RATE as _));

        commands::register_commands(app);
        cvars::register_cvars(app);
    }
}

const NUM_SPAWN_PARAMS: usize = 16;

#[derive(Debug)]
pub struct Client {
    name: QString,
    // TODO: Implement `color`.
    #[expect(dead_code)]
    color: u8,
    state: ClientSlotState,
    spawn_params: [f32; NUM_SPAWN_PARAMS],
}

impl Default for Client {
    fn default() -> Self {
        Self {
            name: "player".into(),
            color: 0,
            state: ClientSlotState::Connecting,
            spawn_params: [0.; NUM_SPAWN_PARAMS],
        }
    }
}

impl Client {
    pub fn entity(&self) -> Option<Entity> {
        match &self.state {
            ClientSlotState::Active(active) => Some(active.entity),
            _ => None,
        }
    }
}

#[derive(Debug)]
/// The state of a client's connection to the server.
pub enum ClientSlotState {
    /// The client is still connecting.
    Connecting,

    /// The client is active.
    Active(ClientActive),
}

type BitSet = hi_sparse_bitset::BitSet<hi_sparse_bitset::config::_256bit>;

#[derive(Debug)]
pub struct ClientActive {
    /// If true, client may execute any command.
    ///
    /// > TODO: Implement client commands properly.
    _privileged: bool,

    /// The entity controlled by this client.
    entity: Entity,

    /// The set of entities for which we've sent a baseline. Boxed so we don't blow up
    /// the size of `ClientSlotState` (level 0 is stored inline).
    sent_baseline: Box<BitSet>,

    /// The most-recent movement command received from this client (see [`LevelState::client_think`]).
    cmd: MoveCmd,
}

/// The movement part of a client's most recent move command
///
/// Equivalent to `usercmd_t`.
#[derive(Debug, Default, Copy, Clone, PartialEq)]
pub struct MoveCmd {
    pub forward: f32,
    pub side: f32,
    pub up: f32,
}

/// A fixed-size pool of client connections.
pub struct ClientSlots {
    /// Occupied slots are `Some`.
    slots: Vec<Option<Client>>,
}

impl ClientSlots {
    /// Creates a new pool which supports at most `limit` clients.
    pub fn new(limit: usize) -> ClientSlots {
        let mut slots = Vec::with_capacity(limit);
        slots.resize_with(limit, || None);

        ClientSlots { slots }
    }

    pub fn connected_clients(&self) -> impl Iterator<Item = usize> + '_ {
        self.slots.iter().enumerate().filter(|(_, v)| v.is_some()).map(|(i, _)| i)
    }

    pub fn active_clients(&self) -> impl Iterator<Item = (usize, &ClientActive)> + '_ {
        self.connected_clients().filter_map(|i| {
            if let Some(ClientSlotState::Active(active)) = self.get(i).map(|c| &c.state) {
                Some((i, active))
            } else {
                None
            }
        })
    }

    pub fn active_clients_mut(&mut self) -> impl Iterator<Item = (usize, &mut ClientActive)> + '_ {
        self.slots.iter_mut().enumerate().filter_map(|(i, client)| {
            if let Some(ClientSlotState::Active(active)) = client.as_mut().map(|c| &mut c.state) {
                Some((i, active))
            } else {
                None
            }
        })
    }

    /// Returns a reference to the client in a slot.
    ///
    /// If the slot is unoccupied, or if `id` is greater than `self.limit()`,
    /// returns `None`.
    pub fn get(&self, id: usize) -> Option<&Client> {
        self.slots.get(id)?.as_ref()
    }

    /// Returns a reference to the client in a slot.
    ///
    /// If the slot is unoccupied, or if `id` is greater than `self.limit()`,
    /// returns `None`.
    pub fn get_mut(&mut self, id: usize) -> Option<&mut Client> {
        self.slots.get_mut(id)?.as_mut()
    }

    /// Returns the maximum number of simultaneous clients.
    pub fn limit(&self) -> usize {
        self.slots.len()
    }

    /// Finds an available connection slot for a new client, and returns the slot.
    pub fn find_available(&mut self) -> Option<usize> {
        let slot = self.slots.iter().position(Option::is_none)?;
        self.slots[slot] = Some(Client::default());
        Some(slot)
    }

    /// Occupies `slot` with a new client, returning it. Returns `None` if `slot` is out of range.
    pub fn occupy(&mut self, slot: usize) -> Option<&mut Client> {
        let entry = self.slots.get_mut(slot)?;
        *entry = Some(Client::default());
        entry.as_mut()
    }
}

/// Server state that persists between levels.
pub struct SessionPersistent {
    client_slots: ClientSlots,
    /// The `serverflags` global, saved between levels (e.g. the episode runes).
    ///
    /// This is a float rather than a bitflags type, as QuakeC can store arbitrary values in it.
    serverflags: f32,
}

impl SessionPersistent {
    pub fn new(max_clients: usize) -> SessionPersistent {
        SessionPersistent { client_slots: ClientSlots::new(max_clients), serverflags: 0. }
    }

    pub fn client(&self, slot: usize) -> Option<&Client> {
        self.client_slots.get(slot)
    }

    pub fn client_mut(&mut self, slot: usize) -> Option<&mut Client> {
        self.client_slots.get_mut(slot)
    }
}

/// The state of a server.
pub enum SessionState {
    /// The server is loading.
    ///
    /// Certain operations, such as precaching, are only permitted while the
    /// server is loading a level.
    Loading,

    /// The server is active (in-game).
    Active,
}

/// A level which is waiting for its assets to load before it can be spawned.
///
/// See `systems::advance_pending_level`.
#[derive(Resource)]
pub struct PendingLevel {
    /// The path of the map, e.g. `maps/e1m1.bsp`.
    map_name: String,
    bsp: Handle<Bsp>,
    progs: Handle<ScriptAsset>,
    /// The worldspawn entity, once the VM has been attached to it.
    worldspawn: Option<Entity>,
    /// The state carried over from the previous level (for `changelevel` and `restart`), or `None`
    /// to start a fresh session (for `map`).
    persist: Option<SessionPersistent>,
}

/// A server instance.
#[derive(Resource)]
pub struct Session {
    persist: SessionPersistent,
    state: SessionState,
}

impl Session {
    /// Returns the maximum number of clients allowed on the server.
    pub fn max_clients(&self) -> usize {
        self.persist.client_slots.limit()
    }

    pub fn client(&self, slot: usize) -> Option<&Client> {
        self.persist.client(slot)
    }

    pub fn client_mut(&mut self, slot: usize) -> Option<&mut Client> {
        self.persist.client_mut(slot)
    }

    pub fn loading(&self) -> bool {
        matches!(&self.state, SessionState::Loading)
    }

    /// The entities of the active clients, in slot order.
    pub fn active_client_entities(&self) -> Vec<Entity> {
        self.persist.client_slots.active_clients().map(|(_, client)| client.entity).collect()
    }
}

/// Used for `entity_gravity`. See https://github.com/id-Software/Quake/blob/master/QW/server/sv_phys.c#L946
fn default_entity_gravity() -> f32 {
    1.
}

pub fn limit_vec3(vel: Vec3, max: Option<f32>) -> Vec3 {
    vel.to_array()
        .map(|v| {
            if v.is_finite() {
                if let Some(max) = max { v.clamp(-max, max) } else { v }
            } else {
                error!("Found invalid");
                0.
            }
        })
        .into()
}

/// The magical bhop-enabling acceleration function.
///
/// Quake splits this into aerial/grounded variants but the only difference is
/// limiting the maximum desired speed.
///
/// Equivalent to `SV_Accelerate`/`SV_AirAccelerate`
pub fn accelerate(vel: Vec3, wish_dir: Vec3, wish_speed: f32, accel_speed: f32) -> Vec3 {
    let current_speed = vel.dot(wish_dir);
    let add_speed = wish_speed - current_speed;
    if add_speed <= 0. {
        return vel;
    }

    vel + accel_speed.min(add_speed) * wish_dir
}

/// Slows a player who is running into a wall, depending on how directly they are facing it.
///
/// `forward` is the player's view direction and `wall_normal` faces out of the wall. The
/// component of the velocity into the wall is removed, and the rest of the horizontal velocity is
/// scaled down the more directly the player faces the wall. Vertical velocity is unchanged.
///
/// Equivalent to `SV_WallFriction`.
pub fn wall_friction(velocity: Vec3, forward: Vec3, wall_normal: Vec3) -> Vec3 {
    let d = wall_normal.dot(forward) + 0.5;
    if d >= 0. {
        return velocity;
    }

    let side = velocity - wall_normal * wall_normal.dot(velocity);
    (side.xy() * (1. + d)).extend(velocity.z)
}

/// Calculates a swimming player's new velocity.
///
/// Swimming follows the full view direction (`view`, from `v_angle`), and `cmd.up` moves straight
/// up or down. With no input at all the player slowly sinks.
///
/// Equivalent to `SV_WaterMove`.
pub fn water_move(
    velocity: Vec3,
    view: &AngleVectors,
    cmd: &MoveCmd,
    max_speed: f32,
    friction: f32,
    accel: f32,
    dt: f32,
) -> Vec3 {
    /// How fast a player with no input tries to sink.
    const DRIFT_SPEED: f32 = 60.;
    /// Swimming is slower than walking by this factor.
    const SWIM_SPEED_SCALE: f32 = 0.7;

    let mut wish_vel = view.forward * cmd.forward + view.right * cmd.side;
    if cmd.forward == 0. && cmd.side == 0. && cmd.up == 0. {
        wish_vel.z -= DRIFT_SPEED;
    } else {
        wish_vel.z += cmd.up;
    }

    let mut wish_speed = wish_vel.length();
    if wish_speed > max_speed {
        wish_vel *= max_speed / wish_speed;
        wish_speed = max_speed;
    }
    wish_speed *= SWIM_SPEED_SCALE;

    // Water friction, which unlike ground friction is in 3D and has no stop speed.
    let speed = velocity.length();
    let (mut velocity, new_speed) = if speed != 0. {
        let new_speed = (speed - dt * speed * friction).max(0.);
        (velocity * (new_speed / speed), new_speed)
    } else {
        (velocity, 0.)
    };

    // Water acceleration. Unlike `accelerate`, this compares against the whole speed rather than
    // the speed in the wished-for direction.
    if wish_speed == 0. {
        return velocity;
    }

    let add_speed = wish_speed - new_speed;
    if add_speed <= 0. {
        return velocity;
    }

    let accel_speed = (accel * wish_speed * dt).min(add_speed);
    velocity += accel_speed * wish_vel.normalize_or_zero();

    velocity
}

/// Calculates how far to roll the player model during a strafe.
///
/// Equivalent to `V_CalcRoll`
fn calc_roll(angles: Vec3, velocity: Vec3) -> f32 {
    /// Equivalent to `cl_rollangle`
    // TODO: Make this a cvar
    const ROLL_ANGLE: f32 = 2.;
    /// Equivalent to `cl_rollspeed`
    // TODO: Make this a cvar
    const ROLL_SPEED: f32 = 200.;

    let side = velocity.dot(angle_vectors(angles).right);
    let sign = if side < 0. { -1. } else { 1. };
    let side = side.abs();
    let side = if side < ROLL_SPEED { side * ROLL_ANGLE / ROLL_SPEED } else { ROLL_ANGLE };

    side * sign
}

/// Nulls out the component of `velocity` pointing into a surface, scaled by
/// `overbounce`.
///
/// Equivalent to `ClipVelocity`
fn clip_velocity(velocity: Vec3, normal: Vec3, overbounce: f32) -> Vec3 {
    const STOP_EPSILON: f32 = 0.1;

    let backoff = velocity.dot(normal) * overbounce;
    (velocity - normal * backoff)
        .to_array()
        .map(|v| if v.abs() < STOP_EPSILON { 0. } else { v })
        .into()
}

/// Converts leaf contents to the values QuakeC expects.
const fn quake_contents(contents: BspLeafContents) -> f32 {
    -(contents as i32 as f32)
}

/// Returns whether `contents` counts as being in liquid.
///
/// **NOTE**: Sky is counted as a liquid, a quirk inherited from Quake.
fn is_liquid(contents: BspLeafContents) -> bool {
    contents as i32 >= BspLeafContents::Water as i32
}

#[derive(Copy, Clone, PartialEq, Deserialize)]
pub struct ServerVars {
    #[serde(rename(deserialize = "sv_gravity"))]
    gravity: f32,
    #[serde(skip, default = "default_entity_gravity")]
    entity_gravity: f32,
    #[serde(rename(deserialize = "sv_maxvelocity"))]
    max_entity_velocity: f32,
    #[serde(rename(deserialize = "sv_stepheight"))]
    max_step: f32,
}

#[derive(Copy, Clone, PartialEq, Deserialize)]
pub struct PlayerVars {
    #[serde(rename(deserialize = "sv_maxspeed"))]
    max_speed: f32,
    #[serde(rename(deserialize = "sv_maxairspeed"))]
    max_air_speed: f32,
    #[serde(rename(deserialize = "sv_stopspeed"))]
    stop_speed: f32,
    #[serde(rename(deserialize = "sv_accelerate"))]
    accel: f32,
    #[serde(rename(deserialize = "sv_friction"))]
    ground_friction: f32,
    /// Multiplier for `ground_friction` when the player is about to walk off an edge.
    #[serde(rename(deserialize = "edgefriction"))]
    edge_friction: f32,
}

/// Returns the factor to scale a player's velocity by after applying `friction` for `dt` seconds
/// at horizontal speed `speed`, as in Quake's `SV_UserFriction`.
///
/// Below `stop_speed`, friction acts as if the player were moving at `stop_speed`, so that slow
/// players come to a stop quickly.
fn friction_scale(speed: f32, friction: f32, stop_speed: f32, dt: f32) -> f32 {
    if speed <= 0. {
        return 1.;
    }

    let control = speed.max(stop_speed);
    (speed - dt * control * friction).max(0.) / speed
}

use seismon_utils::angle_mod;

/// Formats a float in the format expected by QuakeC.
///
/// Whole numbers are printed as integers, anything else with one decimal place.
///
/// Equivalent to `PF_ftos`.
fn ftos(f: f32) -> String {
    // TODO: Check the formatting docs to see if there's a single format invocation
    // that does this.
    if f == (f as i32) as f32 { format!("{}", f as i32) } else { format!("{f:5.1}") }
}

/// Formats a vector in the format expected by QuakeC.
///
/// Equivalent to `PF_vtos`.
fn vtos(v: Vec3) -> String {
    format!("'{:5.1} {:5.1} {:5.1}'", v.x, v.y, v.z)
}

/// Server-side level state, other than the entities.
#[derive(Resource, Debug)]
pub struct LevelState {
    /// The path of the map, e.g. `maps/e1m1.bsp`.
    map_path: String,

    sound_precache: Precache,
    model_precache: Precache,
    /// One entry per lightstyle, sized by [`max_lightstyles`] when the level is created.
    lightstyles: Vec<QCString>,

    /// Amount of time the current level has been active.
    time: Duration,

    /// Reliable messages sent to one client.
    frame_client_messages: HashMap<Entity, Vec<u8>>,

    /// Unreliable messages sent to all clients, for this frame only.
    broadcast: Vec<u8>,

    /// Persistent bytestring to append to the message sent to clients on sign-on.
    init: Vec<u8>,

    /// The maximum number of clients. Client entities are `1..=max_clients`.
    max_clients: usize,

    /// The client returned by `checkclient`
    ///
    /// Equivalent to `sv.lastcheck`.
    last_check_client: usize,

    /// When `last_check_client` was last changed
    ///
    /// Equivalent to `sv.lastchecktime`.
    last_check_time: Duration,

    /// Whether QuakeC has already called `changelevel`.
    ///
    /// Subsequent calls to `changelevel` are ignored until the level
    /// actually changes.
    pub changelevel_issued: bool,
}

impl LevelState {
    pub fn new(map_path: &str, max_clients: usize, models: &[world::model::Model]) -> LevelState {
        let sound_precache = Precache::new();
        let mut model_precache = Precache::new();

        if let Err(e) = model_precache.precache(map_path) {
            error!("{e}");
        }

        for model in models.iter() {
            // "*0" is the null model
            if model.name() != "*0"
                && let Err(e) = model_precache.precache(model.name())
            {
                error!("{e}");
            }
        }

        LevelState {
            map_path: map_path.to_owned(),
            sound_precache,
            model_precache,
            frame_client_messages: default(),
            lightstyles: Vec::new(),
            time: Duration::from_secs(1),
            broadcast: default(),
            init: default(),
            max_clients,
            last_check_client: 0,
            last_check_time: Duration::ZERO,
            changelevel_issued: false,
        }
    }

    pub fn sound_id(&self, name: &QCString) -> Option<usize> {
        self.sound_precache.find(name.to_string_lossy())
    }

    pub fn model_id(&self, name: &QCString) -> Option<usize> {
        self.model_precache.find(name.to_string_lossy())
    }

    /// Clears the messages broadcast during the previous frame.
    ///
    /// Messages to individual clients (`frame_client_messages`) are reliable, so they are kept
    /// until they are sent rather than cleared here.
    pub fn init_frame(&mut self) {
        self.broadcast.clear();
    }

    /// Returns the precache index of a model.
    ///
    /// The empty model name is always index 0.
    ///
    /// Equivalent to `SV_ModelIndex`.
    fn model_index(&self, model_name: &QCString) -> Result<usize, ServerError> {
        if model_name.as_bytes().is_empty() {
            return Ok(0);
        }

        self.model_precache.find(model_name.to_string_lossy()).ok_or_else(|| {
            ServerError::with_msg(format!("no precache: {}", model_name.to_string_lossy()))
        })
    }
}

/// Access to the server state for code that may run QuakeC.
///
/// This holds `&mut World` rather than borrows of the individual components and resources, as
/// QuakeC can access anything. Borrows of components and resources must not be held across calls to
/// QuakeC.
pub struct ServerCtx<'w> {
    pub world: &'w mut World,
}

fn no_such_entity(entity: Entity) -> ServerError {
    ServerError::NoSuchEntity { entity, backtrace: Backtrace::capture() }
}

macro_rules! component_accessors {
    ($($get:ident, $get_mut:ident => $component:ty;)*) => {
        $(
            pub fn $get(&self, entity: Entity) -> Result<&$component, ServerError> {
                self.world.get::<$component>(entity).ok_or_else(|| no_such_entity(entity))
            }

            pub fn $get_mut(&mut self, entity: Entity) -> Result<Mut<'_, $component>, ServerError> {
                self.world.get_mut::<$component>(entity).ok_or_else(|| no_such_entity(entity))
            }
        )*
    };
}

impl<'w> ServerCtx<'w> {
    pub fn new(world: &'w mut World) -> Self {
        Self { world }
    }

    component_accessors! {
        phys, phys_mut => EntPhysics;
        render, render_mut => EntRender;
        callbacks, callbacks_mut => EntCallbacks;
        info, info_mut => EntInfo;
        player, player_mut => EntPlayer;
        ai, ai_mut => EntAi;
    }

    pub fn level(&self) -> &LevelState {
        self.world.resource::<LevelState>()
    }

    pub fn level_mut(&mut self) -> Mut<'_, LevelState> {
        self.world.resource_mut::<LevelState>()
    }

    pub fn registry(&self) -> &Registry {
        self.world.resource::<Registry>()
    }

    pub fn rng(&mut self) -> Mut<'_, ServerRng> {
        self.world.resource_mut::<ServerRng>()
    }

    pub fn edicts(&self) -> &EdictTable {
        self.world.resource::<EdictTable>()
    }

    pub fn collision(&self) -> &LevelCollision {
        self.world.resource::<LevelCollision>()
    }

    pub fn worldspawn(&self) -> Entity {
        self.edicts().worldspawn()
    }

    pub fn globals(&self) -> &QcGlobals {
        let worldspawn = self.worldspawn();
        self.world.get::<QcGlobals>(worldspawn).expect("Worldspawn should have the globals")
    }

    pub fn globals_mut(&mut self) -> Mut<'_, QcGlobals> {
        let worldspawn = self.worldspawn();
        self.world.get_mut::<QcGlobals>(worldspawn).expect("Worldspawn should have the globals")
    }

    pub fn time(&self) -> Duration {
        self.level().time
    }

    /// Whether `entity` is an allocated QuakeC entity.
    pub fn exists(&self, entity: Entity) -> bool {
        self.world.get::<Edict>(entity).is_some()
    }

    /// The saved spawn parameters of the client controlling `entity`, if it is a client.
    pub fn client_spawn_params(&self, entity: Entity) -> Option<[f32; NUM_SPAWN_PARAMS]> {
        let slots = &self.world.get_resource::<Session>()?.persist.client_slots;
        slots
            .connected_clients()
            .filter_map(|slot| slots.get(slot))
            .find(|client| client.entity() == Some(entity))
            .map(|client| client.spawn_params)
    }

    pub fn edict(&self, entity: Entity) -> Result<u16, ServerError> {
        self.world.get::<Edict>(entity).map(|edict| edict.0).ok_or_else(|| no_such_entity(entity))
    }

    /// Whether `entity` is world.
    pub fn is_world(&self, entity: Entity) -> bool {
        entity == self.worldspawn()
    }

    /// Calls a QuakeC function.
    pub fn call(&mut self, function: QCFunction) -> Result<(), ServerError> {
        qc::call_fn(self.world, function)
    }

    pub fn server_vars(&self) -> Result<ServerVars, ServerError> {
        Ok(self.registry().read_cvars()?)
    }

    pub fn point_contents_raw(&self, point: Vec3) -> BspLeafContents {
        self.collision().contents_at_point(point)
    }

    pub fn trace_entity_move(
        &self,
        entity: Entity,
        start: Vec3,
        min: Vec3,
        max: Vec3,
        end: Vec3,
        kind: CollideKind,
    ) -> Result<(Trace, Option<Entity>), ServerError> {
        self.collision().trace_entity_move(self.world, entity, start, min, max, end, kind)
    }

    pub fn entity_is_stuck(&self, entity: Entity) -> Result<bool, ServerError> {
        self.collision().entity_is_stuck(self.world, entity)
    }

    /// Links an entity into the area tree, without touching triggers.
    pub fn link_raw(&mut self, entity: Entity) -> Result<(), ServerError> {
        self.world.resource_scope(|world, mut collision: Mut<LevelCollision>| {
            collision.link_entity(world, entity)
        })
    }

    pub fn unlink(&mut self, entity: Entity) -> Result<(), ServerError> {
        self.world.resource_scope(|world, mut collision: Mut<LevelCollision>| {
            collision.unlink_entity(world, entity)
        })
    }

    /// Allocates a new entity in the lowest free edict slot, with every field zero.
    pub fn alloc_uninitialized(&mut self) -> Result<Entity, ServerError> {
        let edict = self.world.resource_mut::<EdictTable>().vacant_slot();
        Ok(self.spawn_in_slot(edict))
    }

    /// Allocates the entity for a client, in one of the reserved slots.
    pub fn alloc_uninitialized_reserved(&mut self) -> Result<Entity, ServerError> {
        let edict = self.edicts().reserved_slot().ok_or(world::EntityError::NoVacantSlots)?;
        Ok(self.spawn_in_slot(edict))
    }

    fn spawn_in_slot(&mut self, edict: u16) -> Entity {
        let worldspawn = self.worldspawn();
        let entity = self.world.spawn(qc_entity_bundle(worldspawn, edict)).id();
        assert_ne!(entity.to_bits(), 0, "Entities with all-zero bits can't be passed to QuakeC");
        self.world.resource_mut::<EdictTable>().occupy(edict, entity);
        entity
    }

    /// Unlinks an entity from the world and removes it.
    pub fn free(&mut self, entity: Entity) -> Result<(), ServerError> {
        let Some(&Edict(edict)) = self.world.get::<Edict>(entity) else {
            return Ok(());
        };

        self.unlink(entity)?;
        self.world.resource_mut::<EdictTable>().free(edict);
        self.world.despawn(entity);
        Ok(())
    }

    /// Returns whether `entity` is a client entity, i.e. in `1..=max_clients`.
    fn is_client_entity(&self, entity: Entity) -> bool {
        self.edict(entity).is_ok_and(|edict| (1..=self.level().max_clients as u16).contains(&edict))
    }

    /// Returns the buffer of reliable messages for the client controlling `entity`.
    ///
    /// Returns `None` if `entity` isn't a client entity.
    fn client_message(&mut self, entity: Entity) -> Option<Mut<'_, Vec<u8>>> {
        if !self.is_client_entity(entity) {
            return None;
        }

        Some(
            self.level_mut()
                .map_unchanged(|level| level.frame_client_messages.entry(entity).or_default()),
        )
    }

    pub fn entity_state(&self, entity: Entity) -> Option<EntityState> {
        world::entity::entity_state(self.render(entity).ok()?, self.phys(entity).ok()?)
    }

    pub fn is_networked(&self, entity: Entity) -> bool {
        self.render(entity).is_ok_and(|render| render.is_networked())
    }

    /// Writes the messages describing the client's own entity: `SetAngle` (if QuakeC requested
    /// it with `fixangle`) and `PlayerData`.
    fn write_client_data(
        &mut self,
        ent_id: Entity,
        packet: &mut Vec<u8>,
    ) -> Result<(), ServerError> {
        if self.player(ent_id)?.fixangle != 0. {
            ServerCmd::SetAngle { angles: self.phys(ent_id)?.angles.to_array().into() }
                .serialize(packet)?;
            self.player_mut(ent_id)?.fixangle = 0.;
        }

        let player = self.player(ent_id)?;
        let phys = self.phys(ent_id)?;

        let view_ofs = player.view_ofs;
        let velocity = phys.velocity;
        let ideal_pitch = player.idealpitch;
        let punch_angle = player.punchangle;
        let items = player.items;
        let weapon_frame = player.weaponframe;
        let on_ground = phys.has_flag(world::EntityFlags::ON_GROUND);
        let water_level = phys.waterlevel;
        let armor = player.armorvalue;
        let health = player.health;
        let ammo = player.currentammo;
        let ammo_shells = player.ammo_shells;
        let ammo_nails = player.ammo_nails;
        let ammo_rockets = player.ammo_rockets;
        let ammo_cells = player.ammo_cells;
        let active_weapon = player.weapon;
        let weapon_name = &player.weaponmodel;
        let weapon_model_id = if weapon_name.is_null() {
            None
        } else {
            self.level().model_id(weapon_name).map(|m| m as _)
        };

        use crate::common::net::{ItemFlags, PlayerData};

        ServerCmd::PlayerData(PlayerData {
            on_ground,
            in_water: water_level >= 2.,
            view_height: Some(view_ofs.z),
            ideal_pitch: Some(ideal_pitch),
            punch_pitch: Some(punch_angle.x),
            velocity_x: Some(velocity.x),
            punch_yaw: Some(punch_angle.y),
            velocity_y: Some(velocity.y),
            punch_roll: Some(punch_angle.z),
            velocity_z: Some(velocity.z),
            items: ItemFlags::from_bits_truncate(items as _),
            weapon_frame: Some(weapon_frame as _),
            armor: Some(armor as _),
            weapon_model_id,
            // Truncating, like vanilla's `MSG_WriteShort`.
            health: health as i16,
            ammo: ammo as _,
            ammo_shells: ammo_shells as _,
            ammo_nails: ammo_nails as _,
            ammo_rockets: ammo_rockets as _,
            ammo_cells: ammo_cells as _,
            active_weapon: active_weapon as _,
        })
        .serialize(packet)?;

        Ok(())
    }
}

/// Client commands, which run QuakeC.
impl ServerCtx<'_> {
    fn session_mut(&mut self) -> Mut<'_, Session> {
        self.world.resource_mut::<Session>()
    }

    pub fn clientcmd_prespawn(&mut self, slot: usize) -> anyhow::Result<()> {
        // A client that is already in its slot is reconnecting after a level change, and keeps
        // the spawn parms saved by `SetChangeParms`. Only run that function again on fresh
        // connections.
        if self.session_mut().persist.client(slot).is_some() {
            return Ok(());
        }

        if self.session_mut().persist.client_slots.occupy(slot).is_none() {
            bail!("No such client slot {slot}");
        }

        let set_new_parms = self.globals().set_new_parms;
        self.call(set_new_parms)?;

        let parms = self.globals().parms();
        if let Some(client) = self.session_mut().persist.client_slots.get_mut(slot) {
            client.spawn_params = parms;
        }

        Ok(())
    }

    /// Saves the state that carries over to the next level. Equivalent to `SV_SaveSpawnparms`.
    ///
    /// Copies `serverflags` into the session, and runs `SetChangeParms` for each active client to
    /// save its spawn parms.
    pub fn save_spawn_parms(&mut self) -> Result<(), ServerError> {
        let serverflags = self.globals().serverflags;
        self.session_mut().persist.serverflags = serverflags;

        // Collect these first, so that no borrow of `Session` is held while QuakeC runs.
        let clients = self
            .world
            .resource::<Session>()
            .persist
            .client_slots
            .active_clients()
            .map(|(slot, active)| (slot, active.entity))
            .collect::<Vec<_>>();

        let worldspawn = self.worldspawn();
        let time = duration_to_f32(self.time());
        for (slot, entity) in clients {
            let mut globals = self.globals_mut();
            globals.self_ = entity;
            globals.world = worldspawn;
            globals.time = time;
            let set_change_parms = globals.set_change_parms;
            self.call(set_change_parms)?;

            let parms = self.globals().parms();
            if let Some(client) = self.session_mut().persist.client_mut(slot) {
                client.spawn_params = parms;
            }
        }

        Ok(())
    }

    pub fn clientcmd_name(&mut self, slot: usize, name: QString) -> anyhow::Result<()> {
        if self.session_mut().persist.client_mut(slot).is_none() {
            bail!("No such client {}", slot);
        }

        ServerCmd::UpdateName { player_id: slot as _, new_name: name.clone() }
            .serialize(&mut self.level_mut().broadcast)?;

        if let Some(client) = self.session_mut().persist.client_mut(slot) {
            client.name = name;
        }

        Ok(())
    }

    // TODO: Spawn parameters
    pub fn clientcmd_spawn(&mut self, slot: usize) -> anyhow::Result<()> {
        if self.world.resource::<Session>().client(slot).is_none() {
            bail!("No such client {}", slot);
        }

        // TODO: Actually run spawn routines

        Ok(())
    }

    pub fn clientcmd_begin(&mut self, slot: usize) -> anyhow::Result<()> {
        let client_entity = self.alloc_uninitialized_reserved()?;

        let spawn_params = {
            let mut session = self.session_mut();
            let Some(client) = session.persist.client_mut(slot) else {
                bail!("No such client {}", slot);
            };

            // TODO: All players are currently privileged
            client.state = ClientSlotState::Active(ClientActive {
                _privileged: true,
                entity: client_entity,
                sent_baseline: Default::default(),
                cmd: MoveCmd::default(),
            });

            client.spawn_params
        };

        let worldspawn = self.worldspawn();
        let time = duration_to_f32(self.time());

        let mut globals = self.globals_mut();
        globals.set_parms(spawn_params);
        globals.self_ = client_entity;
        globals.world = worldspawn;
        globals.time = time;
        let client_connect = globals.client_connect;
        self.call(client_connect)?;

        let mut globals = self.globals_mut();
        globals.self_ = client_entity;
        globals.world = worldspawn;
        globals.time = time;
        let put_client_in_server = globals.put_client_in_server;
        self.call(put_client_in_server)?;

        Ok(())
    }

    /// Handles one string command (e.g. `prespawn`, `begin`) sent by client `client_id`,
    /// writing any replies to `out_packet`.
    ///
    /// Malformed commands are logged and ignored, rather than treated as errors.
    pub fn handle_string_cmd(
        &mut self,
        client_id: usize,
        name: &str,
        args: &[String],
        out_packet: &mut Vec<u8>,
    ) -> anyhow::Result<()> {
        // Like Quake, ignore any extra arguments.
        let warn_extra_args = |expected: usize| {
            if args.len() > expected {
                warn!("client {client_id}: ignoring extra arguments to `{name}`: {args:?}");
            }
        };

        match name {
            "prespawn" => {
                warn_extra_args(0);

                self.clientcmd_prespawn(client_id)?;

                ServerCmd::SignOnStage { stage: SignOnStage::ClientInfo }.serialize(out_packet)?;
            }
            "name" => {
                warn_extra_args(1);

                let Some(new_name) = args.first() else {
                    warn!("client {client_id}: `name` requires an argument");
                    return Ok(());
                };

                self.clientcmd_name(client_id, new_name.to_owned().into())?;
            }
            "color" => {
                if args.len() != 2 {
                    warn!("client {client_id}: `color` requires 2 arguments, got {args:?}");
                    return Ok(());
                }

                warn!("TODO: Set color");
            }
            "spawn" => {
                self.clientcmd_spawn(client_id)?;

                ServerCmd::SignOnStage { stage: SignOnStage::Begin }.serialize(out_packet)?;
            }
            "begin" => {
                warn_extra_args(0);

                self.clientcmd_begin(client_id)?;

                let Some(client_ent) =
                    self.world.resource::<Session>().client(client_id).and_then(|c| c.entity())
                else {
                    bail!("client {client_id} has no entity after `begin`");
                };

                ServerCmd::SetView { ent_id: self.edict(client_ent)? as _ }
                    .serialize(out_packet)?;
                ServerCmd::SignOnStage { stage: SignOnStage::Done }.serialize(out_packet)?;
            }
            _ => {
                // Don't forward to the console: the console and the server are shared in a
                // listen server, so commands that the console doesn't handle would be sent
                // straight back here.
                warn!("Unhandled client command: {name}");
            }
        }

        Ok(())
    }

    /// Records a `ClientCmd::Move` from client `client_id`.
    ///
    /// The view angles, buttons and impulse are written to the player entity straight away, but
    /// the movement itself is stored and applied once per server frame by
    /// [`ServerCtx::client_think`]. Moves from clients which aren't active yet are ignored.
    ///
    /// Equivalent to `SV_ReadClientMove`
    pub fn handle_move(&mut self, client_id: usize, cmd: &ClientCmd) -> Result<(), ServerError> {
        let &ClientCmd::Move {
            // Client delta time appears to be unused in Quake.
            delta_time: _,
            angles,
            fwd_move,
            side_move,
            up_move,
            button_flags,
            impulse,
        } = cmd
        else {
            return Err(ServerError::with_msg(format!("not a move command: {cmd:?}")));
        };

        let entity = {
            let mut session = self.session_mut();
            let Some(Client { state: ClientSlotState::Active(active), .. }) =
                session.persist.client_mut(client_id)
            else {
                return Ok(());
            };

            active.cmd =
                MoveCmd { forward: fwd_move as f32, side: side_move as f32, up: up_move as f32 };
            active.entity
        };

        let mut player = self.player_mut(entity)?;

        player.set_v_angle(angles.to_array().into());
        player.set_buttons(button_flags);
        // Empty impulse doesn't overwrite the existing one within a frame (this appears to be Quake's behaviour)
        if impulse != 0 {
            player.impulse = impulse as _;
        }

        Ok(())
    }
}

pub mod systems {
    use std::sync::Arc;

    use bevy::{asset::LoadState, ecs::message::MessageCursor};
    use bevy_mod_commands::CmdName;
    use bevy_mod_scripting::{
        core::{
            commands::{AttachScript, DetachScript},
            script::ScriptContexts,
        },
        script::ScriptAttachment,
    };
    use bevy_mod_scripting_qcvm::{QCEntity, QCWorldspawn};

    use crate::{
        common::net::{self, ClientCmd, ClientMessage, GameType, ServerMessage, SignOnStage},
        server::{
            qc::{
                LevelAttachment, LevelVm, QCScriptingPlugin,
                fields::QcFieldTable,
                globals::{QcGlobalTable, init_globals},
            },
            world::{EntitySolid, MoveKind, hull::BrushModel, model::Model},
        },
    };

    use super::*;

    pub fn recv_client_messages(
        world: &mut World,
        mut cursor: Local<MessageCursor<ClientMessage>>,
    ) {
        let messages = cursor
            .read(world.resource::<Messages<ClientMessage>>())
            .map(|msg| (msg.client_id, msg.packet.clone()))
            .collect::<Vec<_>>();

        let mut out_packet = Vec::new();
        for (client_id, packet) in messages {
            let mut packet = &packet[..];
            loop {
                // TODO: Should this be handled by the registry too?
                match ClientCmd::deserialize(&mut packet) {
                    Ok(Some(cmd)) => match cmd {
                        ClientCmd::StringCmd { cmd } => {
                            let Ok(cmds) = RunCmd::parse_many(&cmd) else {
                                continue;
                            };
                            for RunCmd(CmdName { name, trigger }, args) in cmds {
                                if trigger.is_some() {
                                    error!(
                                        "TODO: Action in `ClientCmd` - currently we only handle network-related cmds"
                                    );
                                    continue;
                                }

                                if !world.contains_resource::<Session>() {
                                    if !matches!(
                                        &*name,
                                        "prespawn" | "name" | "color" | "spawn" | "begin"
                                    ) {
                                        warn!("Unhandled client command: {name}");
                                    }
                                    continue;
                                }

                                // Client input must never crash the server: log and carry on.
                                if let Err(e) = ServerCtx::new(world).handle_string_cmd(
                                    client_id,
                                    &name,
                                    &args,
                                    &mut out_packet,
                                ) {
                                    warn!("client {client_id}: `{name}` failed: {e:#}");
                                }
                            }
                        }
                        cmd @ ClientCmd::Move { .. } => {
                            if !world.contains_resource::<Session>() {
                                continue;
                            }

                            if let Err(e) = ServerCtx::new(world).handle_move(client_id, &cmd) {
                                warn!("client {client_id}: move failed: {e}");
                            }
                        }
                        other => {
                            warn!("TODO: Unimplemented command {:?}", other);
                        }
                    },
                    Ok(None) => break,
                    Err(e) => {
                        error!("{}", e);
                        break;
                    }
                };
            }
        }

        if !out_packet.is_empty() {
            // TODO: Should not hard-code client id 0: replies should go to the client that sent
            //       the command (needs multi-client support).
            world.write_message(ServerMessage { client_id: 0, packet: out_packet.into() });
        }
    }

    pub fn advance_pending_level(world: &mut World) {
        if let Err(e) = try_advance_pending_level(world) {
            error!("Failed loading level: {e:#}");
            world.remove_resource::<PendingLevel>();
            world.write_message(RunCmd::from("startdemos"));
        }
    }

    /// Removes everything from the previous level.
    fn despawn_level(world: &mut World) {
        if let Some(LevelAttachment(attachment)) = world.remove_resource::<LevelAttachment>() {
            DetachScript::<QCScriptingPlugin>::new(attachment).apply(world);
        }

        let entities =
            world.query_filtered::<Entity, With<QCEntity>>().iter(world).collect::<Vec<_>>();
        for entity in entities.into_iter().chain(worldspawns_left(world)) {
            world.despawn(entity);
        }

        world.remove_resource::<LevelVm>();
        world.remove_resource::<QcTables>();
        world.remove_resource::<EdictTable>();
        world.remove_resource::<LevelState>();
        world.remove_resource::<LevelCollision>();
        world.remove_resource::<ServerRng>();
    }

    fn worldspawns_left(world: &mut World) -> Vec<Entity> {
        world.query_filtered::<Entity, With<QCWorldspawn>>().iter(world).collect()
    }

    fn try_advance_pending_level(world: &mut World) -> anyhow::Result<()> {
        let pending = world.resource::<PendingLevel>();
        let asset_server = world.resource::<AssetServer>();
        for (name, state) in [
            (pending.map_name.as_str(), asset_server.load_state(&pending.bsp)),
            ("progs.dat", asset_server.load_state(&pending.progs)),
        ] {
            if let LoadState::Failed(e) = state {
                bail!("Failed loading {name}: {e}");
            }
        }

        if !world.resource::<Assets<Bsp>>().contains(&pending.bsp)
            || !world.resource::<Assets<ScriptAsset>>().contains(&pending.progs)
        {
            return Ok(());
        }

        let progs = pending.progs.clone();

        // Attach the VM to a new worldspawn. Each worldspawn gets its own context, so each level
        // gets fresh globals, as Quake does.
        let worldspawn = match pending.worldspawn {
            Some(worldspawn) => worldspawn,
            None => {
                despawn_level(world);

                let worldspawn = world.spawn(QCWorldspawn).id();
                assert_ne!(worldspawn.to_bits(), 0);
                AttachScript::<QCScriptingPlugin>::new(ScriptAttachment::EntityScript(
                    worldspawn,
                    progs.clone(),
                ))
                .apply(world);
                world.resource_mut::<PendingLevel>().worldspawn = Some(worldspawn);
                worldspawn
            }
        };

        let attachment = ScriptAttachment::EntityScript(worldspawn, progs);
        let Some(context) = world
            .resource::<ScriptContexts<QCScriptingPlugin>>()
            .read()
            .get_context(&attachment)
            .and_then(|context| context.as_loaded().cloned())
        else {
            // Not loaded yet.
            return Ok(());
        };
        let vm = context.lock().clone();
        // Belt-and-braces: the context should be new anyway.
        vm.vm.reset_globals();

        let mut pending = world.remove_resource::<PendingLevel>().expect("Checked above");
        let bsp = world.resource::<Assets<Bsp>>().get(&pending.bsp).expect("Checked above");

        let models = BrushModel::from_bsp_data(&bsp.data)?
            .into_iter()
            .enumerate()
            .map(|(i, model)| Model::from_brush_model(format!("*{i}"), model).cast())
            .collect::<Vec<_>>();
        let entmap = String::from_utf8_lossy(&bsp.data.entities).into_owned();

        // TODO: Make `max_clients` a cvar
        const MAX_CLIENTS: usize = 8;

        let seed = world.resource::<Registry>().read_cvar::<u64>("sv_seed").unwrap_or_default();

        world.insert_resource(LevelVm(vm.clone()));
        world.insert_resource(LevelAttachment(attachment));
        world.insert_resource(ServerRng::new(seed));
        world.insert_resource(EdictTable::new(worldspawn, MAX_CLIENTS));
        world
            .entity_mut(worldspawn)
            .insert((qc_entity_bundle(worldspawn, 0), QcGlobals::new(worldspawn)));

        let mut persist =
            pending.persist.take().unwrap_or_else(|| SessionPersistent::new(MAX_CLIENTS));
        // Carried-over clients have to sign on again: their entities belonged to the old level.
        for slot in persist.client_slots.connected_clients().collect::<Vec<_>>() {
            if let Some(client) = persist.client_mut(slot) {
                client.state = ClientSlotState::Connecting;
            }
        }

        let globals = QcGlobalTable::default();
        init_globals(world, &vm, worldspawn, &globals);
        let qc_entity_component = world.register_component::<QCEntity>();
        let qc_worldspawn_component = world.register_component::<QCWorldspawn>();
        world.insert_resource(QcTables {
            fields: Arc::new(QcFieldTable::new(vm.field_defs(), worldspawn)),
            globals: Arc::new(globals),
            worldspawn,
            qc_entity_component,
            qc_worldspawn_component,
        });
        {
            // Quake sets either `coop` or `deathmatch`, not both.
            let registry = world.resource::<Registry>();
            let coop = registry.read_cvar::<f32>("coop").unwrap_or_default();
            let deathmatch = if coop != 0. {
                0.
            } else {
                registry.read_cvar::<f32>("deathmatch").unwrap_or_default()
            };
            let mapname = std::path::Path::new(&pending.map_name)
                .file_stem()
                .map(|stem| stem.to_string_lossy().into_owned())
                .unwrap_or_default();

            let mut globals = world.get_mut::<QcGlobals>(worldspawn).expect("Inserted above");
            globals.frametime = TICK_RATE;
            globals.serverflags = persist.serverflags;
            globals.mapname = QCString::from(mapname.as_str());
            globals.coop = coop;
            globals.deathmatch = deathmatch;
        }

        world.insert_resource(LevelState::new(&pending.map_name, MAX_CLIENTS, &models));

        // generate world entity
        let world_model_name = QCString::from(models[0].name());
        world.insert_resource(LevelCollision::new(models));
        {
            let mut ctx = ServerCtx::new(world);
            let mut render = ctx.render_mut(worldspawn)?;
            render.model = world_model_name;
            render.modelindex = 1.;
            let mut phys = ctx.phys_mut(worldspawn)?;
            phys.set_solid(EntitySolid::Bsp);
            phys.movetype = MoveKind::Push as u32 as f32;
        }

        ServerCtx::new(world).spawn_map_entities(&entmap)?;

        world.insert_resource(Session { persist, state: SessionState::Loading });

        Ok(())
    }

    pub fn server_spawn(world: &mut World) {
        if let Err(e) = try_server_spawn(world) {
            error!("Failed spawning server: {}", Report::from_error(e));
            world.remove_resource::<Session>();
            world.write_message(RunCmd::from("startdemos"));
        }
    }

    fn try_server_spawn(world: &mut World) -> Result<(), ServerError> {
        if !world.resource::<Session>().loading() {
            return Ok(());
        }

        let delta = world.resource::<Time<Fixed>>().delta();

        // In `sv_init.c` there is a comment saying to run physics twice before starting the server
        // properly to "allow everything to settle".
        for _ in 0..2 {
            let clients = world.resource::<Session>().active_client_entities();
            for (ent, err) in ServerCtx::new(world).physics(&clients, delta)? {
                error!("Failed running frame for {ent}: {}", Report::from_error(err));
            }
        }

        world.resource_mut::<Session>().state = SessionState::Active;

        let coop = world.resource::<Registry>().read_cvar::<f32>("coop").unwrap_or_default();
        let game_type = if coop != 0. { GameType::CoOp } else { GameType::Deathmatch };

        // Set the message sent to the client from QuakeC
        let level_title = {
            let ctx = ServerCtx::new(world);
            ctx.info(ctx.worldspawn())?.message.as_bytes().to_vec().into()
        };

        let session = world.resource::<Session>();
        let level = world.resource::<LevelState>();

        let mut packet = Vec::new();

        ServerCmd::ServerInfo {
            protocol_version: net::PROTOCOL_VERSION as _,
            max_clients: session.max_clients() as _,
            game_type,
            message: level_title,
            model_precache: level.model_precache.iter().map(ToOwned::to_owned).collect(),
            sound_precache: level.sound_precache.iter().map(ToOwned::to_owned).collect(),
        }
        .serialize(&mut packet)?;

        // Style IDs are a `u8` on the wire, so styles past 255 can never be sent.
        for (id, style) in (0..=u8::MAX).zip(level.lightstyles.iter()) {
            if !style.as_bytes().is_empty() {
                ServerCmd::LightStyle { id, value: style.as_bytes().to_vec().into() }
                    .serialize(&mut packet)?;
            }
        }

        ServerCmd::SignOnStage { stage: SignOnStage::Prespawn }.serialize(&mut packet)?;

        packet.extend_from_slice(&level.init);

        world.write_message(ServerMessage { client_id: 0, packet: packet.into() });

        Ok(())
    }

    pub fn server_update(world: &mut World, mut unsent_entity_cache: Local<Vec<(u16, Entity)>>) {
        {
            let session = world.resource::<Session>();
            if session.loading()
                || world.resource::<Registry>().read_cvar::<u8>("sv_paused").unwrap() != 0
                || session.persist.client_slots.active_clients().count() == 0
            {
                return;
            }
        }

        world.resource_mut::<LevelState>().init_frame();

        let send_diff = match world.resource::<Session>().state {
            SessionState::Active => {
                let clients = world.resource::<Session>().active_client_entities();
                let delta = world.resource::<Time<Fixed>>().delta();
                match ServerCtx::new(world).physics(&clients, delta) {
                    Ok(map) => {
                        for (ent, err) in map {
                            error!("Failed running frame for {ent}: {}", Report::from_error(err));
                        }

                        true
                    }
                    Err(e) => {
                        error!("Failed running frame: {}", Report::from_error(e));
                        false
                    }
                }
            }
            _ => false,
        };

        if !send_diff {
            return;
        }

        // Slots which were freed or reused since the last update need a fresh baseline.
        let recycled = world.resource_mut::<EdictTable>().take_recycled().collect::<Vec<_>>();
        {
            let mut session = world.resource_mut::<Session>();
            for edict in recycled {
                for (_, client) in session.persist.client_slots.active_clients_mut() {
                    client.sent_baseline.remove(edict as usize);
                }
            }
        }

        // TODO: Stop hardcoding `8` for max players
        let client_ids = world
            .resource::<Session>()
            .persist
            .client_slots
            .active_clients()
            .map(|(id, _)| id)
            .collect::<ArrayVec<_, 8>>();

        for client_id in client_ids {
            let mut packet = Vec::new();

            let (client_entity, mut sent_baseline) = {
                let mut session = world.resource_mut::<Session>();
                let Some(Client { state: ClientSlotState::Active(client), .. }) =
                    session.persist.client_mut(client_id)
                else {
                    continue;
                };
                (client.entity, std::mem::take(&mut client.sent_baseline))
            };

            let mut ctx = ServerCtx::new(world);

            // Reliable messages are kept until they are sent.
            if let Some(messages) = ctx.level_mut().frame_client_messages.remove(&client_entity) {
                packet.extend_from_slice(&messages);
            }

            ServerCmd::Time { time: duration_to_f32(ctx.time()) }.serialize(&mut packet).unwrap();

            unsent_entity_cache.extend(
                ctx.edicts().iter().filter(|(edict, _)| !sent_baseline.contains(*edict as usize)),
            );

            // Only mark a baseline as sent once it has actually been written, so entities
            // which aren't networked yet (no model) get one when they become
            // networked.
            for (edict, entity) in unsent_entity_cache.drain(..) {
                if ctx.is_networked(entity)
                    && let Some(state) = ctx.entity_state(entity)
                {
                    state.spawn_baseline(edict as _).serialize(&mut packet).unwrap();
                    // TODO: This is leftover from when we had `new_entities`, we need to
                    // reimplement it properly since this will break if we have multiple clients.
                    if let Some(mut baseline) = ctx.world.get_mut::<qc::fields::Baseline>(entity) {
                        baseline.0 = state;
                    }
                    sent_baseline.insert(edict as usize);
                }
            }

            // Skip world entity
            for (edict, entity) in ctx.edicts().range(1..).collect::<Vec<_>>() {
                // Never send an update for an entity the client has no baseline for.
                if !sent_baseline.contains(edict as usize) {
                    continue;
                }

                // Like `SV_WriteEntitiesToClient`, the client's own entity is always sent, even
                // without a model (e.g. during intermission, where QuakeC sets `modelindex = 0`
                // and moves the player to the intermission spot in the same frame).
                if !ctx.is_networked(entity) && entity != client_entity {
                    continue;
                };

                let Some(state) = ctx.entity_state(entity) else {
                    continue;
                };

                let Some(baseline) = ctx.world.get::<qc::fields::Baseline>(entity) else {
                    continue;
                };

                let update = state.make_update(edict as _, &baseline.0);

                // We send a `FastUpdate` even if the update is empty, as the client
                // will delete entities if it doesn't receive an update.
                ServerCmd::FastUpdate(update).serialize(&mut packet).unwrap();
            }

            if ctx.exists(client_entity)
                && let Err(e) = ctx.write_client_data(client_entity, &mut packet)
            {
                warn!("Failed to write client data for {client_entity}: {e}");
            }

            // We add broadcast packets at the end to ensure that entities can spawn before
            // broadcasted events related to those entities
            packet.extend_from_slice(&ctx.level().broadcast);

            if let Some(Client { state: ClientSlotState::Active(client), .. }) =
                world.resource_mut::<Session>().persist.client_mut(client_id)
            {
                client.sent_baseline = sent_baseline;
            }

            world.write_message(ServerMessage { client_id, packet: packet.into() });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::server::TICK_RATE;

    fn assert_approx_eq(a: f32, b: f32) {
        const SLOP_FACTOR: f32 = 1e-5;

        assert!((a - b).abs() < SLOP_FACTOR, "{a} != {b}");
    }

    #[test]
    fn friction_scale_matches_quake() {
        // Above stop speed, `control == speed`: 320 - 0.05 * 320 * 4 = 256.
        assert_approx_eq(super::friction_scale(320., 4., 100., TICK_RATE), 256. / 320.);

        // With edgefriction 2: 320 - 0.05 * 320 * 8 = 192.
        assert_approx_eq(super::friction_scale(320., 8., 100., TICK_RATE), 192. / 320.);

        // Below stop speed, `control == stop_speed`: 50 - 0.05 * 100 * 4 = 30.
        assert_approx_eq(super::friction_scale(50., 4., 100., TICK_RATE), 30. / 50.);

        // Friction never reverses the direction of movement.
        assert_approx_eq(super::friction_scale(10., 4., 100., TICK_RATE), 0.);

        // Stationary (should have friction scale of 1).
        assert_approx_eq(super::friction_scale(0., 4., 100., TICK_RATE), 1.);
    }

    fn assert_approx_eq_v3(a: Vec3, b: Vec3) {
        assert!(a.abs_diff_eq(b, 1e-3), "{a} != {b}");
    }

    #[test]
    fn wall_friction_matches_quake() {
        // The wall faces +x, so the player is on its +x side.
        let normal = Vec3::X;
        let velocity = Vec3::new(-100., 200., -50.);

        // Facing straight into the wall: d = -1 + 0.5. The into-wall part is removed and the
        // horizontal speed along the wall is halved, but falling speed is unchanged.
        let into_wall = super::wall_friction(velocity, -Vec3::X, normal);
        assert_approx_eq_v3(into_wall, Vec3::new(0., 100., -50.));

        // Facing mostly along the wall (d = -0.4 + 0.5 >= 0), so there is no extra friction.
        let along = Vec3::new(-0.4, 0.84f32.sqrt(), 0.);
        assert_eq!(super::wall_friction(velocity, along, normal), velocity);
    }

    #[test]
    fn water_move_matches_quake() {
        const MAX_SPEED: f32 = 320.;
        const FRICTION: f32 = 4.;
        const ACCEL: f32 = 10.;

        // Looking straight up.
        let view = AngleVectors { forward: Vec3::Z, right: Vec3::X, up: -Vec3::Y };
        let swim = |velocity, forward, up| {
            let cmd = MoveCmd { forward, side: 0., up };
            super::water_move(velocity, &view, &cmd, MAX_SPEED, FRICTION, ACCEL, TICK_RATE)
        };

        // With no input, sink: wishspeed = 60 * 0.7 = 42, so accelerate by 10 * 42 * 0.05 = 21.
        assert_approx_eq_v3(swim(Vec3::ZERO, 0., 0.), Vec3::new(0., 0., -21.));

        // Swimming up with `upmove`: wishspeed = 200 * 0.7 = 140, accelerating by 70.
        assert_approx_eq_v3(swim(Vec3::ZERO, 0., 200.), Vec3::new(0., 0., 70.));

        // Swimming forward follows the view, which here is straight up.
        assert_approx_eq_v3(swim(Vec3::ZERO, 200., 0.), Vec3::new(0., 0., 70.));

        // Friction is applied to the whole speed, and the result (256) is already above the
        // wished-for speed (42), so there is no acceleration: 320 - 0.05 * 320 * 4 = 256.
        assert_approx_eq_v3(swim(Vec3::X * 320., 0., 0.), Vec3::X * 256.);

        // Holding forward faster than `sv_maxspeed` settles at 0.7 * `sv_maxspeed`.
        let mut velocity = Vec3::ZERO;
        for _ in 0..200 {
            velocity = swim(velocity, 400., 0.);
        }
        assert_approx_eq_v3(velocity, Vec3::Z * 0.7 * MAX_SPEED);
    }

    #[test]
    fn accelerate_matches_quake() {
        const DT: f32 = TICK_RATE;

        // On the ground, accelerate towards 320 by at most `sv_accelerate * dt * wishspeed` (160).
        let vel = super::accelerate(Vec3::ZERO, Vec3::X, 320., 10. * DT * 320.);
        assert_eq!(vel, Vec3::new(160., 0., 0.));
        let vel = super::accelerate(vel, Vec3::X, 320., 10. * DT * 320.);
        assert_eq!(vel, Vec3::new(320., 0., 0.));

        // Already at the wish speed: no change.
        assert_eq!(super::accelerate(vel, Vec3::X, 320., 160.), vel);

        // In the air, the wish speed is capped at 30, but the acceleration uses the full wish speed.
        let vel = super::accelerate(Vec3::new(0., 300., 0.), Vec3::X, 30., 10. * DT * 320.);
        assert_eq!(vel, Vec3::new(30., 300., 0.));
    }

    #[test]
    fn ftos_vtos_match_quake() {
        assert_eq!(super::ftos(5.), "5");
        assert_eq!(super::ftos(-12.), "-12");
        assert_eq!(super::ftos(0.), "0");
        assert_eq!(super::ftos(5.5), "  5.5");
        assert_eq!(super::ftos(-0.25), " -0.2");
        assert_eq!(super::vtos(Vec3::new(1., -2.5, 100.)), "'  1.0  -2.5 100.0'");
    }
}
