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

mod builtins;
mod commands;
mod cvars;
#[doc(hidden)]
pub mod parity;
pub mod precache;
pub mod progs;
mod rng;
pub mod world;

use std::{
    cell::RefCell,
    fmt::{self},
    io::Write,
    ops::{Bound, RangeBounds as _},
    time::Duration,
};

use bevy_mod_commands::{Registry, RunCmd};

use crate::{
    common::{
        console::SeismonConsoleCorePlugin,
        math::CollisionResult,
        net::{ClientCmd, ClientMessage, EntityState, ServerCmd, ServerMessage},
        parse,
    },
    server::{
        progs::{
            GlobalAddrFunction,
            functions::{BuiltinFunctionId, FunctionKind},
        },
        world::{
            FieldAddrVector, MoveKind,
            hull::{BrushModel, BspLeafContents},
            model::Model,
        },
    },
};

use self::{
    precache::Precache,
    progs::{
        EntityFieldAddr, EntityId, ExecutionContext, FieldAddr, FunctionId, GlobalAddrEntity,
        GlobalAddrFloat, GlobalAddrVector, Globals, Opcode, ProgsAsset, ProgsError, ProgsLoader,
        StringId, StringTable,
        globals::{
            AngleVectors, GLOBAL_ADDR_ARG_0, GLOBAL_ADDR_ARG_1, GLOBAL_ADDR_ARG_2,
            GLOBAL_ADDR_ARG_3, GLOBAL_ADDR_ARG_4, GLOBAL_ADDR_RETURN, angle_vectors,
        },
    },
    rng::ServerRng,
    world::{
        EntityFlags, EntitySolid, FieldAddrFloat, FieldAddrFunctionId, FieldAddrStringId, World,
        phys::{CollideKind, Trace},
    },
};

use anyhow::bail;
use arrayvec::ArrayVec;
use bevy::prelude::*;
use bevy_trenchbroom::bsp::Bsp;
use bitflags::bitflags;
use bitvec::vec::BitVec;
use byteorder::{LittleEndian, WriteBytesExt as _};
use hashbrown::HashMap;
use itertools::Itertools;
use num::FromPrimitive;
use seismon_utils::{QStr, QString, duration_from_f32, duration_to_f32};
use serde::Deserialize;
use snafu::{Backtrace, Report};

use crate::client::{ConnectionStage, NewConnection};
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
    type Error = ProgsError;

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
            _ => Err(ProgsError::with_msg(format!("Invalid message dest: {value}"))),
        }
    }
}

const MAX_LIGHTSTYLES: usize = 256;
/// Tick rate for the server.
///
/// > TODO: Make this a cvar
const TICK_RATE: f32 = 0.05;

#[derive(Default, Copy, Clone)]
pub struct SeismonServerPlugin;

/// Enable running a listen server that the local client can connect to directly,
/// e.g. for single-player games.
#[derive(Default, Copy, Clone)]
pub struct SeismonListenServerPlugin;

/// Notify the local client that the server has initiated a level load.
#[derive(Message, Debug, Clone, Copy)]
pub struct LocalLevelStarted;

impl Plugin for SeismonListenServerPlugin {
    fn build(&self, app: &mut App) {
        /// Connect the local client to the level that the local server just started loading.
        fn connect_to_local_server(
            mut client_messages: ResMut<Messages<ClientMessage>>,
            mut server_messages: ResMut<Messages<ServerMessage>>,
            mut new_connection: MessageWriter<NewConnection>,
        ) {
            // Drop any messages that were sent to or from the previous level.
            client_messages.clear();
            server_messages.clear();

            new_connection
                .write(NewConnection::Server(None, ConnectionStage::SignOn(SignOnStage::Prespawn)));
        }

        fn shutdown_on_remote_connect(
            mut commands: Commands,
            mut new_connections: MessageReader<NewConnection>,
        ) {
            // If we connect to a remote server, i.e. `NewConnection::Server` with a `QSocket`,
            // shut down the listen server session.
            if new_connections.read().any(|conn| matches!(conn, NewConnection::Server(Some(_), _)))
            {
                commands.remove_resource::<Session>();
                commands.remove_resource::<PendingLevel>();
            }
        }

        app.add_plugins(SeismonServerPlugin).add_systems(
            Last,
            (
                connect_to_local_server.run_if(on_message::<LocalLevelStarted>),
                shutdown_on_remote_connect.run_if(on_message::<NewConnection>),
            ),
        );
    }
}

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

        app.init_asset::<ProgsAsset>()
            .init_asset_loader::<ProgsLoader>()
            .add_systems(FixedPreUpdate, systems::recv_client_messages)
            .add_systems(
                Update,
                systems::advance_pending_level
                    .pipe(|In(res), mut commands: Commands| {
                        if let Err(e) = res {
                            error!("Failed loading level: {e:#}");
                            commands.remove_resource::<PendingLevel>();
                            commands.write_message(RunCmd::from("startdemos"));
                        }
                    })
                    .run_if(resource_exists::<PendingLevel>),
            )
            .add_systems(
                FixedUpdate,
                (
                    systems::server_update,
                    systems::server_spawn.pipe(
                        |In(res),
                         mut commands: Commands,
                         mut runcmd: MessageWriter<RunCmd<'static>>| {
                            if let Err(e) = res {
                                error!("Failed spawning server: {}", Report::from_error(e));
                                commands.remove_resource::<Session>();
                                runcmd.write("startdemos".into());
                            }
                        },
                    ),
                )
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

const SPAWN_PARAM_ADDRS: [GlobalAddrFloat; NUM_SPAWN_PARAMS] = [
    GlobalAddrFloat::Arg0,
    GlobalAddrFloat::Arg1,
    GlobalAddrFloat::Arg2,
    GlobalAddrFloat::Arg3,
    GlobalAddrFloat::Arg4,
    GlobalAddrFloat::Arg5,
    GlobalAddrFloat::Arg6,
    GlobalAddrFloat::Arg7,
    GlobalAddrFloat::Arg8,
    GlobalAddrFloat::Arg9,
    GlobalAddrFloat::Arg10,
    GlobalAddrFloat::Arg11,
    GlobalAddrFloat::Arg12,
    GlobalAddrFloat::Arg13,
    GlobalAddrFloat::Arg14,
    GlobalAddrFloat::Arg15,
];

#[derive(Debug)]
pub struct Client {
    name: QString,
    color: u8,
    state: ClientState,
    spawn_params: [f32; NUM_SPAWN_PARAMS],
}

impl Default for Client {
    fn default() -> Self {
        Self {
            name: "player".into(),
            color: 0,
            state: ClientState::Connecting,
            spawn_params: [0.; NUM_SPAWN_PARAMS],
        }
    }
}

impl Client {
    pub fn entity(&self) -> Option<EntityId> {
        match &self.state {
            ClientState::Active(active) => Some(active.entity_id),
            _ => None,
        }
    }
}

#[derive(Debug)]
/// The state of a client's connection to the server.
pub enum ClientState {
    /// The client is still connecting.
    Connecting,

    /// The client is active.
    Active(ClientActive),
}

#[derive(Debug)]
pub struct ClientActive {
    /// If true, client may execute any command.
    ///
    /// > TODO: Implement client commands properly.
    _privileged: bool,

    /// ID of the entity controlled by this client.
    entity_id: EntityId,

    sent_baseline: BitVec,

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

bitflags! {
    pub struct SessionFlags: i32 {
        const EPISODE_1 =      0x0001;
        const EPISODE_2 =      0x0002;
        const EPISODE_3 =      0x0004;
        const EPISODE_4 =      0x0008;
        const NEW_UNIT =       0x0010;
        const NEW_EPISODE =    0x0020;
        const CROSS_TRIGGERS = 0xFF00;
    }
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
            if let Some(ClientState::Active(active)) = self.get(i).map(|c| &c.state) {
                Some((i, active))
            } else {
                None
            }
        })
    }

    pub fn active_clients_mut(&mut self) -> impl Iterator<Item = (usize, &mut ClientActive)> + '_ {
        self.slots.iter_mut().enumerate().filter_map(|(i, client)| {
            if let Some(ClientState::Active(active)) = client.as_mut().map(|c| &mut c.state) {
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

    /// Finds an available connection slot for a new client.
    pub fn find_available(&mut self) -> Option<&mut Client> {
        let slot = self.slots.iter_mut().find(|s| s.is_none())?;
        Some(slot.insert(Client::default()))
    }
}

/// Server state that persists between levels.
pub struct SessionPersistent {
    client_slots: ClientSlots,
    // TODO: Implement this
    _flags: SessionFlags,
}

impl SessionPersistent {
    pub fn new(max_clients: usize) -> SessionPersistent {
        SessionPersistent {
            client_slots: ClientSlots::new(max_clients),
            _flags: SessionFlags::empty(),
        }
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
    progs: Handle<ProgsAsset>,
}

/// A server instance.
#[derive(Resource)]
pub struct Session {
    persist: SessionPersistent,
    state: SessionState,
    level: LevelState,
}

impl Session {
    pub fn new(
        bsp_name: String,
        max_clients: usize,
        registry: Mut<Registry>,
        progs: ProgsAsset,
        models: Vec<Model>,
        entmap: String,
    ) -> Session {
        Session {
            persist: SessionPersistent::new(max_clients),
            state: SessionState::Loading,
            level: LevelState::new(bsp_name, max_clients, progs, models, entmap, registry),
        }
    }

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

    pub fn clientcmd_prespawn(
        &mut self,
        // TODO: Handle client slot
        _slot: usize,
        mut registry: Mut<Registry>,
    ) -> anyhow::Result<()> {
        let Some(client) = self.persist.client_slots.find_available() else {
            // TODO: Error handling
            return Ok(());
        };

        let set_new_args =
            self.level.globals.get_function_id(GlobalAddrFunction::SetNewArgs as i16)?;
        self.level.execute_program(set_new_args, registry.reborrow())?;

        for (i, arg_addr) in SPAWN_PARAM_ADDRS.iter().enumerate() {
            client.spawn_params[i] = self.level.globals.load(*arg_addr)?;
        }

        Ok(())
    }

    pub fn clientcmd_name(&mut self, slot: usize, name: QString) -> anyhow::Result<()> {
        let Some(client) = self.persist.client_mut(slot) else {
            bail!("No such client {}", slot);
        };

        ServerCmd::UpdateName { player_id: slot as _, new_name: name.clone() }
            .serialize(&mut self.level.broadcast)?;

        client.name = name;

        Ok(())
    }

    pub fn clientcmd_color(&mut self, slot: usize, color: u8) -> anyhow::Result<()> {
        let Some(client) = self.client_mut(slot) else {
            bail!("No such client {}", slot);
        };

        client.color = color;

        Ok(())
    }

    // TODO: Spawn parameters
    pub fn clientcmd_spawn(&mut self, slot: usize) -> anyhow::Result<()> {
        let Some(_client) = self.client(slot) else {
            bail!("No such client {}", slot);
        };

        // TODO: Actually run spawn routines

        Ok(())
    }

    pub fn clientcmd_begin(
        &mut self,
        slot: usize,
        mut registry: Mut<Registry>,
    ) -> anyhow::Result<()> {
        let client_entity = self.level.world.alloc_uninitialized_reserved()?;

        let Some(client) = self.persist.client_mut(slot) else {
            bail!("No such client {}", slot);
        };

        // TODO: All players are currently privileged
        client.state = ClientState::Active(ClientActive {
            _privileged: true,
            entity_id: client_entity,
            sent_baseline: BitVec::repeat(false, world::MAX_ENTITIES),
            cmd: MoveCmd::default(),
        });

        for (i, arg_addr) in SPAWN_PARAM_ADDRS.iter().enumerate() {
            self.level.globals.store(*arg_addr, client.spawn_params[i])?;
        }

        self.level.globals.store(GlobalAddrEntity::Self_, client_entity)?;
        self.level.globals.store(GlobalAddrEntity::World, EntityId(0))?;
        self.level.globals.store(GlobalAddrFloat::Time, duration_to_f32(self.level.time))?;
        let client_connect =
            self.level.globals.get_function_id(GlobalAddrFunction::ClientConnect as i16)?;
        self.level.execute_program(client_connect, registry.reborrow())?;

        self.level.globals.store(GlobalAddrEntity::Self_, client_entity)?;
        self.level.globals.store(GlobalAddrEntity::World, EntityId(0))?;
        self.level.globals.store(GlobalAddrFloat::Time, duration_to_f32(self.level.time))?;
        let put_client_in_server =
            self.level.globals.get_function_id(GlobalAddrFunction::PutClientInServer as i16)?;
        self.level.execute_program(put_client_in_server, registry.reborrow())?;

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
        mut registry: Mut<Registry>,
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

                self.clientcmd_prespawn(client_id, registry.reborrow())?;

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

                self.clientcmd_begin(client_id, registry.reborrow())?;

                let Some(client_ent) = self.client(client_id).and_then(|c| c.entity()) else {
                    bail!("client {client_id} has no entity after `begin`");
                };

                ServerCmd::SetView { ent_id: client_ent.0 as _ }.serialize(out_packet)?;
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
    /// [`LevelState::client_think`]. Moves from clients which aren't active yet are ignored.
    ///
    /// Equivalent to `SV_ReadClientMove`
    pub fn handle_move(&mut self, client_id: usize, cmd: &ClientCmd) -> progs::Result<()> {
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
            return Err(ProgsError::with_msg(format!("not a move command: {cmd:?}")));
        };

        let Some(Client { state: ClientState::Active(active), .. }) =
            self.persist.client_mut(client_id)
        else {
            return Ok(());
        };

        active.cmd =
            MoveCmd { forward: fwd_move as f32, side: side_move as f32, up: up_move as f32 };

        let mut entity = self.level.world.get_mut(active.entity_id)?;

        entity.set_view_angle(angles.to_array().into())?;
        entity.set_buttons(button_flags)?;
        // Empty impulse doesn't overwrite the existing one within a frame (this appears to be Quake's behaviour)
        if impulse != 0 {
            entity.set_impulse(impulse as _)?;
        }

        Ok(())
    }

    pub fn precache_sound(&mut self, name_id: StringId) -> progs::Result<()> {
        if let SessionState::Loading = self.state {
            self.level.precache_sound(name_id)
        } else {
            Err(ProgsError::with_msg("Sounds cannot be precached after loading"))
        }
    }

    pub fn precache_model(&mut self, name_id: StringId) -> progs::Result<()> {
        if let SessionState::Loading = self.state {
            self.level.precache_model(name_id)
        } else {
            Err(ProgsError::with_msg("Models cannot be precached after loading"))
        }
    }

    pub fn loading(&self) -> bool {
        matches!(&self.state, SessionState::Loading)
    }

    fn level(&self) -> &LevelState {
        &self.level
    }

    fn level_mut(&mut self) -> &mut LevelState {
        &mut self.level
    }

    pub fn sound_id(&self, name_id: StringId) -> Option<usize> {
        self.level().sound_id(name_id)
    }

    pub fn model_id(&self, name_id: StringId) -> Option<usize> {
        self.level().model_id(name_id)
    }

    pub fn set_lightstyle(&mut self, index: usize, val: StringId) {
        self.level_mut().set_lightstyle(index, val);
    }

    /// Returns the amount of time the current level has been active.
    pub fn time(&self) -> Option<Duration> {
        match self.state {
            SessionState::Loading => None,
            SessionState::Active => Some(self.level.time),
        }
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
    // TODO: Used by `SV_WaterMove`, which isn't implemented yet.
    #[allow(dead_code)]
    #[serde(rename(deserialize = "sv_wateraccelerate"))]
    water_accel: f32,
    #[serde(rename(deserialize = "sv_friction"))]
    ground_friction: f32,
    /// Multiplier for `ground_friction` when the player is about to walk off an edge.
    #[serde(rename(deserialize = "edgefriction"))]
    edge_friction: f32,
    // TODO: Used by `SV_WaterMove`, which isn't implemented yet.
    #[allow(dead_code)]
    #[serde(rename(deserialize = "sv_waterfriction"))]
    water_friction: f32,
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

/// Server-side level state.
#[derive(Debug)]
pub struct LevelState {
    string_table: StringTable,
    sound_precache: Precache,
    model_precache: Precache,
    lightstyles: [StringId; MAX_LIGHTSTYLES],

    /// Amount of time the current level has been active.
    time: Duration,

    /// QuakeC bytecode execution context.
    ///
    /// This includes the program counter, call stack, and local variables.
    cx: ExecutionContext,

    /// Global values for QuakeC bytecode.
    globals: Globals,

    /// The state of the game world.
    ///
    /// This contains the entities and world geometry.
    world: World,

    // -- TODO: Next 3 fields should be extracted to a separate struct for borrowck purposes --
    /// Reliable messages sent to one client.
    frame_client_messages: HashMap<EntityId, Vec<u8>>,

    /// Unreliable messages sent to all clients, for this frame only.
    broadcast: Vec<u8>,

    /// Persistent bytestring to append to the message sent to clients on sign-on.
    init: Vec<u8>,

    /// The maximum number of clients. Client entities are `1..=max_clients`.
    max_clients: usize,

    /// The client returned by `checkclient`, as in Quake's `sv.lastcheck`.
    last_check_client: usize,

    /// When `last_check_client` was last changed, as in Quake's `sv.lastchecktime`.
    last_check_time: Duration,

    /// Source of all game-affecting randomness, seeded from `sv_seed`.
    rng: ServerRng,
}

// TODO: Copied from Quake 1 source, can be done better.
fn angle_mod(quake_angle: f32) -> f32 {
    const PRECISION: f32 = 65536.;
    let quake_angle = quake_angle.rem_euclid(360.);

    (360f32 / PRECISION) * (quake_angle * PRECISION / 360.0).floor()
}

/// Return value of [`chase_dirs`].
///
/// The directions a monster can try to move in to chase its goal
#[derive(Debug, PartialEq)]
struct ChaseDirs {
    /// The direction along the x axis, if the goal is far enough away along it.
    x: Option<f32>,
    /// The direction along the y axis, if the goal is far enough away along it.
    y: Option<f32>,
    /// The diagonal direction, if the goal is far enough away along both axes.
    diagonal: Option<f32>,
}

/// Returns the directions to try for a monster chasing a goal `(delta_x, delta_y)` away.
///
/// Part of the implementation equivalent to `SV_NewChaseDir`.
fn chase_dirs(delta_x: f32, delta_y: f32) -> ChaseDirs {
    let x = if delta_x > 10. {
        Some(0.)
    } else if delta_x < -10. {
        Some(180.)
    } else {
        None
    };

    let y = if delta_y < -10. {
        Some(270.)
    } else if delta_y > 10. {
        Some(90.)
    } else {
        None
    };

    // Quake uses 215 rather than 225 here, so we do too.
    let diagonal = match (x, y) {
        (Some(0.), Some(y)) => Some(if y == 90. { 45. } else { 315. }),
        (Some(_), Some(y)) => Some(if y == 90. { 135. } else { 215. }),
        _ => None,
    };

    ChaseDirs { x, y, diagonal }
}

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

/// Converts an angle in degrees to a byte, as in Quake's `MSG_WriteAngle`.
///
/// Like Quake, this truncates the angle to an integer first and wraps around.
fn write_angle_byte(angle: f32) -> u8 {
    (((angle as i32) * 256 / 360) & 255) as u8
}

impl LevelState {
    pub fn new(
        map_path: String,
        max_clients: usize,
        progs: ProgsAsset,
        models: Vec<Model>,
        entmap: String,
        mut registry: Mut<Registry>,
    ) -> LevelState {
        let ProgsAsset { cx, mut globals, entity_def, mut string_table } = progs;

        let seed = registry.read_cvar::<u64>("sv_seed").unwrap_or_default();

        globals.store(GlobalAddrFloat::FrameTime, TICK_RATE).unwrap();

        let sound_precache = Precache::new();
        let mut model_precache = Precache::new();

        if let Err(e) = model_precache.precache(&map_path) {
            error!("{e}");
        }

        for model in models.iter() {
            let model_name = string_table.find_or_insert(model.name());
            let name = string_table.get(model_name).unwrap();
            // "*0" is the null model
            if &*name.raw != b"*0"
                && let Err(e) = model_precache.precache(name.to_str())
            {
                error!("{e}");
            }
        }

        let world = World::new(models, entity_def, &mut string_table).unwrap();
        let entity_list = match parse::entities(&entmap) {
            Ok(ents) => ents,
            Err(err) => {
                panic!("{err}");
            }
        };

        let mut level = LevelState {
            string_table,
            sound_precache,
            model_precache,
            frame_client_messages: default(),
            lightstyles: [StringId(0); MAX_LIGHTSTYLES],
            time: Duration::from_secs(1),
            cx,
            globals,
            world,

            broadcast: default(),
            init: default(),
            max_clients,
            last_check_client: 0,
            last_check_time: Duration::ZERO,
            rng: ServerRng::new(seed),
        };

        for entity in entity_list {
            if let Err(e) = level.spawn_entity_from_map(entity, registry.reborrow()) {
                error!("Failed spawning entity: {e}");
            }
        }

        level
    }

    pub fn string(&self, string_id: StringId) -> Option<QStr<'_>> {
        self.string_table.get(string_id)
    }

    pub fn precache_sound(&mut self, name_id: StringId) -> progs::Result<()> {
        let name = self.string_table.get(name_id).ok_or_else(|| {
            ProgsError::with_msg(format!("precache_sound: no string with ID {name_id}"))
        })?;
        self.sound_precache.precache(name.to_str())?;
        Ok(())
    }

    pub fn precache_model(&mut self, name_id: StringId) -> progs::Result<()> {
        let name = self.string_table.get(name_id).ok_or_else(|| {
            ProgsError::with_msg(format!("precache_model: no string with ID {name_id}"))
        })?;
        self.model_precache.precache(name.to_str())?;
        Ok(())
    }

    pub fn sound_id(&self, name_id: StringId) -> Option<usize> {
        self.sound_precache.find(self.string_table.get(name_id).unwrap().to_str())
    }

    pub fn model_id(&self, name_id: StringId) -> Option<usize> {
        self.model_precache.find(self.string_table.get(name_id).unwrap().to_str())
    }

    pub fn set_lightstyle(&mut self, index: usize, val: StringId) {
        self.lightstyles[index] = val;
    }

    /// Writes the messages describing the client's own entity: `SetAngle` (if QuakeC requested
    /// it with `fixangle`) and `PlayerData`.
    fn write_client_data(&mut self, ent_id: EntityId, packet: &mut Vec<u8>) -> progs::Result<()> {
        let mut entity = self.world.get_mut(ent_id)?;

        if entity.get_bool(FieldAddrFloat::FixAngle as i16)? {
            ServerCmd::SetAngle { angles: entity.angles()?.to_array().into() }.serialize(packet)?;
            entity.put_float(0., FieldAddrFloat::FixAngle as i16)?;
        }

        let view_ofs = entity.view_ofs()?;
        let velocity = entity.velocity()?;
        let ideal_pitch = entity.ideal_pitch()?;
        let punch_angle = entity.punch_angle()?;
        let items = entity.items()?;
        let weapon_frame = entity.weapon_frame()?;
        let on_ground = entity.has_flag(EntityFlags::ON_GROUND)?;
        let water_level = entity.water_level()?;
        let armor = entity.armor()?;
        let health = entity.health()?;
        let ammo = entity.ammo()?;
        let ammo_shells = entity.ammo_shells()?;
        let ammo_nails = entity.ammo_nails()?;
        let ammo_rockets = entity.ammo_rockets()?;
        let ammo_cells = entity.ammo_cells()?;
        let active_weapon = entity.active_weapon()?;
        let weapon_name = entity.weapon_model_name()?;
        let weapon_model_id =
            if weapon_name.is_none() { None } else { self.model_id(weapon_name).map(|m| m as _) };

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
            health: health as _,
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

    pub fn entity_state(&self, id: EntityId) -> Option<EntityState> {
        self.world.get(id).ok()?.state()
    }

    pub fn is_networked(&self, id: EntityId) -> bool {
        self.world.get(id).is_ok_and(|mut e| e.is_networked())
    }

    /// Clears the messages broadcast during the previous frame.
    ///
    /// Messages to individual clients (`frame_client_messages`) are reliable, so they are kept
    /// until they are sent rather than cleared here.
    pub fn init_frame(&mut self) {
        self.broadcast.clear();
    }

    /// Returns whether `ent_id` is a client entity, i.e. in `1..=max_clients`.
    fn is_client_entity(&self, ent_id: EntityId) -> bool {
        (1..=self.max_clients as i32).contains(&ent_id.0)
    }

    /// Returns the buffer of reliable messages for the client controlling `ent_id`.
    ///
    /// Returns `None` if `ent_id` isn't a client entity.
    fn client_message(&mut self, ent_id: EntityId) -> Option<&mut Vec<u8>> {
        if !self.is_client_entity(ent_id) {
            return None;
        }

        Some(self.frame_client_messages.entry(ent_id).or_default())
    }

    fn write_dest(&mut self) -> progs::Result<impl Write> {
        let dest: MsgDest =
            (self.globals.get_float(GLOBAL_ADDR_ARG_0 as i16)? as usize).try_into()?;
        match dest {
            // TODO: `All` should be for reliable messages, but as we don't currently have an
            // unreliable/reliable       message split we just combine it with broadcast
            // (i.e. unreliable) messages.
            MsgDest::All | MsgDest::Broadcast => Ok(&mut self.broadcast),
            MsgDest::One => {
                let client = self.globals.load(GlobalAddrEntity::MsgEntity)?;
                self.client_message(client).ok_or_else(|| {
                    ProgsError::with_msg(format!("WriteDest: {client} is not a client"))
                })
            }
            MsgDest::Init => Ok(&mut self.init),
        }
    }

    /// Concatenates the strings `first_arg` up to `last_arg`.
    ///
    /// Equivalent to `PF_VarString`.
    fn var_string(&self, first_arg: usize, last_arg: usize) -> progs::Result<QString> {
        let mut out = Vec::new();
        // In Quake, `last_arg` is called `pr_argc`, but that's misleading since
        // it's not a count.
        for i in first_arg..last_arg.max(first_arg + 1) {
            let s_id = self.globals.string_id((GLOBAL_ADDR_ARG_0 + 3 * i) as i16)?;
            if let Some(s) = self.string_table.get(s_id) {
                out.extend_from_slice(&s.raw);
            }
        }

        Ok(out.into())
    }

    /// Runs a builtin function which was called with `argc` arguments.
    fn enter_builtin(
        &mut self,
        builtin_id: BuiltinFunctionId,
        argc: usize,
        mut registry: Mut<Registry>,
    ) -> progs::Result<()> {
        use BuiltinFunctionId::*;

        macro_rules! todo_builtin {
            ($id:ident) => {{
                const ERROR_MSG: &str = concat!("TODO: ", stringify!($id));

                self.cx.print_backtrace(&self.string_table, false);
                if cfg!(debug_assertions) {
                    panic!("{ERROR_MSG}")
                } else {
                    error!("{ERROR_MSG}");
                }
            }};
        }

        match builtin_id {
            Aim => self.builtin_aim()?,
            AmbientSound => self.builtin_ambient_sound()?,
            BPrint => self.builtin_bprint()?,
            Break => todo_builtin!(Break),
            Ceil => self.globals.builtin_ceil()?,
            CenterPrint => self.builtin_center_print(argc)?,
            ChangeLevel => self.builtin_change_level()?,
            ChangeYaw => self.builtin_change_yaw()?,
            CheckBottom => self.builtin_check_bottom(&registry)?,
            CheckClient => self.builtin_check_client()?,
            CoreDump => todo_builtin!(CoreDump),
            Cvar => self.builtin_cvar(&registry)?,
            CvarSet => self.builtin_cvar_set(registry.reborrow())?,
            DPrint => self.builtin_dprint(argc)?,
            DropToFloor => self.builtin_drop_to_floor(registry.reborrow())?,
            EPrint => todo_builtin!(EPrint),
            Error => self.builtin_error(argc)?,
            FAbs => self.globals.builtin_f_abs()?,
            FindRadius => self.builtin_find_radius()?,
            Find => self.builtin_find()?,
            Floor => self.globals.builtin_floor()?,
            FToS => self.builtin_ftos()?,
            LightStyle => self.builtin_light_style()?,
            LocalCmd => todo_builtin!(LocalCmd),
            MakeStatic => self.builtin_make_static()?,
            MakeVectors => self.globals.make_vectors()?,
            MoveToGoal => self.builtin_move_to_goal(registry.reborrow())?,
            NextEnt => todo_builtin!(NextEnt),
            Normalize => self.builtin_normalize()?,
            ObjError => self.builtin_obj_error(argc)?,
            Particle => self.builtin_particle()?,
            PointContents => self.builtin_point_contents()?,
            // Only used in `qcc`, does nothing at runtime
            PrecacheFile => {}
            // Only used in `qcc`, does nothing at runtime
            PrecacheFile2 => {}
            // PrecacheModel2/PrecacheSound2 only differ for `qcc`, not at runtime
            PrecacheModel | PrecacheModel2 => self.builtin_precache_model()?,
            PrecacheSound | PrecacheSound2 => self.builtin_precache_sound()?,
            Random => self.globals.builtin_random(&mut self.rng)?,
            Remove => self.builtin_remove()?,
            RInt => self.globals.builtin_r_int()?,
            SetModel => self.builtin_set_model(registry.reborrow())?,
            SetOrigin => self.builtin_set_origin(registry.reborrow())?,
            SetSize => self.builtin_set_size()?,
            SetSpawnArgs => todo_builtin!(SetSpawnArgs),
            Sound => self.builtin_sound()?,
            Spawn => self.builtin_spawn(registry.reborrow())?,
            SPrint => self.builtin_sprint(argc)?,
            StuffCmd => self.builtin_stuffcmd()?,
            TraceLine => self.builtin_trace_line()?,
            TraceOff => todo_builtin!(TraceOff),
            TraceOn => todo_builtin!(TraceOn),
            VecToAngles => self.globals.builtin_vec_to_angles()?,
            VecToYaw => self.globals.builtin_vec_to_yaw()?,
            VLen => self.globals.builtin_v_len()?,
            VToS => self.builtin_vtos()?,
            WalkMove => self.builtin_walk_move(registry.reborrow())?,
            WriteAngle => self.builtin_write_angle()?,
            WriteByte => self.builtin_write_byte()?,
            WriteChar => self.builtin_write_char()?,
            WriteCoord => self.builtin_write_coord()?,
            WriteEntity => self.builtin_write_entity()?,
            WriteLong => self.builtin_write_long()?,
            WriteShort => self.builtin_write_short()?,
            WriteString => self.builtin_write_string()?,
        }

        Ok(())
    }

    /// Execute a QuakeC function in the VM.
    ///
    /// If execution fails, the call stack is unwound back to where it was when this was called,
    /// restoring the locals of every function that was entered.
    pub fn execute_program(&mut self, f: FunctionId, registry: Mut<Registry>) -> progs::Result<()> {
        let exit_depth = self.cx.call_stack_depth();

        self.cx.enter_function(&self.string_table, &mut self.globals, f)?;

        let result = self.run_until_depth(exit_depth, registry);

        if result.is_err()
            && let Err(e) = self.cx.unwind_to(&self.string_table, &mut self.globals, exit_depth)
        {
            error!("Failed to unwind QuakeC call stack: {e}");
        }

        result
    }

    /// Runs the VM until the call stack returns to `exit_depth`.
    fn run_until_depth(
        &mut self,
        exit_depth: usize,
        mut registry: Mut<Registry>,
    ) -> progs::Result<()> {
        use Opcode::*;

        let mut runaway = progs::MAX_OPS_SINGLE_CALL;

        while self.cx.call_stack_depth() != exit_depth {
            runaway -= 1;

            if runaway == 0 {
                self.cx.print_backtrace(&self.string_table, true);
                return Err(ProgsError::RunawayLoop { backtrace: Backtrace::capture() });
            }

            let statement = self.cx.load_statement()?;
            let op = statement.opcode;
            let a = statement.arg1;
            let b = statement.arg2;
            let c = statement.arg3;

            debug!("{:<12} {:>5} {:>5} {:>5}", op.to_string(), a, b, c);

            match op {
                // Control flow ================================================
                If => {
                    let cond = self.globals.get_int(a)? != 0;
                    debug!("{op}: cond == {cond}");

                    if cond {
                        self.cx.jump_relative(b);
                        continue;
                    }
                }

                IfNot => {
                    let cond = self.globals.get_int(a)? != 0;
                    debug!("{op}: cond != {cond}");

                    if !cond {
                        self.cx.jump_relative(b);
                        continue;
                    }
                }

                Goto => {
                    self.cx.jump_relative(a);
                    continue;
                }

                Call0 | Call1 | Call2 | Call3 | Call4 | Call5 | Call6 | Call7 | Call8 => {
                    let f_to_call = self.globals.get_function_id(a)?;

                    if f_to_call.0 == 0 {
                        return Err(ProgsError::with_msg("NULL function"));
                    }

                    let Ok(def) = self.cx.function_def(f_to_call) else {
                        return Err(ProgsError::with_msg("NULL function"));
                    };

                    let name_id = def.name_id;

                    debug!(
                        "Calling function {} ({:?})",
                        self.string_table.get(name_id).unwrap(),
                        f_to_call
                    );

                    let called_with_args = op as usize - Call0 as usize;
                    if def.argc != called_with_args {
                        /// Seemingly `droptofloor` is defined with 2 args in the quakec defs
                        /// but every example I can find calls it with 0 args and the
                        /// implementation ignores any extra args. To prevent spamming the
                        /// console with warnings, we ignore arg count mismatches for this
                        /// function.
                        const HACK_IGNORE_MISMATCH: &[&[u8]] = &[b"droptofloor"];

                        let func_name = self.string_table.get(name_id).unwrap();
                        if !HACK_IGNORE_MISMATCH.contains(&&*func_name) {
                            self.cx.print_backtrace(&self.string_table, false);
                            warn!(
                                "Arg count mismatch calling {}: expected {}, found {}",
                                func_name, def.argc, called_with_args,
                            );
                        }
                    }

                    if let FunctionKind::BuiltIn(b) = def.kind {
                        self.enter_builtin(b, called_with_args, registry.reborrow())?;
                        debug!(
                            "Returning from built-in function {}",
                            self.string_table.get(name_id).unwrap()
                        );
                    } else {
                        self.cx.enter_function(&self.string_table, &mut self.globals, f_to_call)?;
                        continue;
                    }
                }

                Done | Return => {
                    self.op_return(a, b, c)?;

                    // If this program was run from inside a builtin (e.g. a touch callback),
                    // stepping here would break the caller by stepping over the call
                    // statement and skipping the following instruction.
                    if self.cx.call_stack_depth() == exit_depth {
                        break;
                    }
                }

                MulF => self.globals.op_mul_f(a, b, c)?,
                MulV => self.globals.op_mul_v(a, b, c)?,
                MulFV => self.globals.op_mul_fv(a, b, c)?,
                MulVF => self.globals.op_mul_vf(a, b, c)?,
                Div => self.globals.op_div(a, b, c)?,
                AddF => self.globals.op_add_f(a, b, c)?,
                AddV => self.globals.op_add_v(a, b, c)?,
                SubF => self.globals.op_sub_f(a, b, c)?,
                SubV => self.globals.op_sub_v(a, b, c)?,
                EqF => self.globals.op_eq_f(a, b, c)?,
                EqV => self.globals.op_eq_v(a, b, c)?,
                EqS => self.globals.op_eq_s(&self.string_table, a, b, c)?,
                EqEnt => self.globals.op_eq_ent(a, b, c)?,
                EqFnc => self.globals.op_eq_fnc(a, b, c)?,
                NeF => self.globals.op_ne_f(a, b, c)?,
                NeV => self.globals.op_ne_v(a, b, c)?,
                NeS => self.globals.op_ne_s(&self.string_table, a, b, c)?,
                NeEnt => self.globals.op_ne_ent(a, b, c)?,
                NeFnc => self.globals.op_ne_fnc(a, b, c)?,
                Le => self.globals.op_le(a, b, c)?,
                Ge => self.globals.op_ge(a, b, c)?,
                Lt => self.globals.op_lt(a, b, c)?,
                Gt => self.globals.op_gt(a, b, c)?,
                LoadF => self.op_load_f(a, b, c)?,
                LoadV => self.op_load_v(a, b, c)?,
                LoadS => self.op_load_s(a, b, c)?,
                LoadEnt => self.op_load_ent(a, b, c)?,
                LoadFld => self.op_load_fld(a, b, c)?,
                LoadFnc => self.op_load_fnc(a, b, c)?,
                Address => self.op_address(a, b, c)?,
                StoreF => self.globals.op_store_f(a, b, c)?,
                StoreV => self.globals.op_store_v(a, b, c)?,
                StoreS => self.globals.op_store_s(a, b, c)?,
                StoreEnt => self.globals.op_store_ent(a, b, c)?,
                StoreFld => self.globals.op_store_fld(a, b, c)?,
                StoreFnc => self.globals.op_store_fnc(a, b, c)?,
                StorePF => self.op_storep_f(a, b, c)?,
                StorePV => self.op_storep_v(a, b, c)?,
                StorePS => self.op_storep_s(a, b, c)?,
                StorePEnt => self.op_storep_ent(a, b, c)?,
                StorePFld => self.op_storep_fld(a, b, c)?,
                StorePFnc => self.op_storep_fnc(a, b, c)?,
                NotF => self.globals.op_not_f(a, b, c)?,
                NotV => self.globals.op_not_v(a, b, c)?,
                NotS => self.globals.op_not_s(a, b, c)?,
                NotEnt => self.globals.op_not_ent(a, b, c)?,
                NotFnc => self.globals.op_not_fnc(a, b, c)?,
                And => self.globals.op_and(a, b, c)?,
                Or => self.globals.op_or(a, b, c)?,
                BitAnd => self.globals.op_bit_and(a, b, c)?,
                BitOr => self.globals.op_bit_or(a, b, c)?,

                State => self.op_state(a, b, c)?,
            }

            // Increment program counter.
            self.cx.jump_relative(1);
        }

        Ok(())
    }

    pub fn execute_program_by_name<S>(
        &mut self,
        name: S,
        registry: Mut<Registry>,
    ) -> progs::Result<()>
    where
        S: AsRef<str>,
    {
        let name = name.as_ref();
        let func_id = self.cx.find_function_by_name(&self.string_table, name)?;
        self.execute_program(func_id, registry)?;

        Ok(())
    }

    /// Link an entity into the `World`.
    ///
    /// If `touch_triggers` is `true`, this will invoke the touch function of
    /// any trigger the entity is touching.
    pub fn link_entity(
        &mut self,
        ent_id: EntityId,
        touch_triggers: bool,
        registry: Mut<Registry>,
    ) -> progs::Result<()> {
        self.world.link_entity(ent_id)?;

        if touch_triggers {
            self.touch_triggers(ent_id, registry)?;
        }

        Ok(())
    }

    pub fn spawn_entity(&mut self, registry: Mut<Registry>) -> progs::Result<EntityId> {
        let ent_id = self.world.alloc_uninitialized()?;

        self.link_entity(ent_id, false, registry)?;

        Ok(ent_id)
    }

    pub fn spawn_entity_from_map<'a, I: IntoIterator<Item = (&'a str, &'a str)>>(
        &mut self,
        map: I,
        mut registry: Mut<Registry>,
    ) -> progs::Result<EntityId> {
        let ent_id = self.world.alloc_from_map(&mut self.string_table, map)?;

        let classname = self
            .string_table
            .get(self.world.get(ent_id)?.classname()?)
            .ok_or_else(|| ProgsError::with_msg("No classname for entity!"))?
            .to_str()
            .into_owned();

        // set `self` before calling spawn function
        self.globals.store(GlobalAddrEntity::Self_, ent_id)?;
        self.globals.store(GlobalAddrEntity::World, EntityId(0))?;
        self.execute_program_by_name(&classname, registry.reborrow())?;

        Ok(ent_id)
    }

    pub fn set_entity_origin(
        &mut self,
        ent_id: EntityId,
        origin: Vec3,
        registry: Mut<Registry>,
    ) -> progs::Result<()> {
        self.world.get_mut(ent_id)?.store(FieldAddrVector::Origin, origin)?;
        self.link_entity(ent_id, false, registry)?;

        Ok(())
    }

    /// Returns the precache index of a model.
    ///
    /// The empty model name is always index 0.
    ///
    /// Equivalent to `SV_ModelIndex`.
    fn model_index(&self, model_name_id: StringId) -> progs::Result<usize> {
        let model_name = self
            .string_table
            .get(model_name_id)
            .ok_or_else(|| ProgsError::with_msg(format!("invalid model name {model_name_id}")))?;

        if model_name.is_empty() {
            return Ok(0);
        }

        self.model_precache
            .find(model_name.to_str())
            .ok_or_else(|| ProgsError::with_msg(format!("no precache: {model_name}")))
    }

    /// Sets an entity's model/size.
    ///
    /// Equivalent to `PF_setmodel`.
    pub fn set_entity_model(
        &mut self,
        ent_id: EntityId,
        model_name_id: StringId,
        registry: Mut<Registry>,
    ) -> progs::Result<()> {
        /// The server ignores the actual model data and assigns this size
        /// to all alias models (i.e. models loaded from `.mdl` files).
        const ALIAS_MODEL_HALF_SIZE: f32 = 16.;

        let model_id = self.model_index(model_name_id)?;

        let mut ent = self.world.get_mut(ent_id)?;
        ent.set_model_name(model_name_id)?;
        ent.set_model_index(model_id as _)?;

        // Brush models (which the server has loaded) use their actual size, and the null model has
        // no size.
        // TODO: Quake uses the frame size for sprites, we should actually load those.
        if !self.world.set_entity_model(ent_id, model_id)? {
            self.world.set_entity_size(
                ent_id,
                Vec3::splat(-ALIAS_MODEL_HALF_SIZE),
                Vec3::splat(ALIAS_MODEL_HALF_SIZE),
            )?;
        }

        self.link_entity(ent_id, false, registry)
    }

    /// Runs an entity's think function if it is due this frame.
    ///
    /// Like Quake (but unlike QuakeWorld), this thinks at most once per frame. Returns `false` if
    /// the entity was removed by its think function.
    ///
    /// Equivalent to `SV_RunThink`.
    // TODO: Should we implement QW's think mechanism as an option?
    pub fn think(
        &mut self,
        ent_id: EntityId,
        frame_time: Duration,
        registry: Mut<Registry>,
    ) -> progs::Result<bool> {
        let mut ent = self.world.get_mut(ent_id)?;
        let think_time = ent.load(FieldAddrFloat::NextThink)?;

        if think_time <= 0. || think_time > duration_to_f32(self.time + frame_time) {
            // Think either already happened or isn't due yet.
            return Ok(true);
        }

        // TODO: Is this actually correct? Check quake.
        let think_time = think_time.max(duration_to_f32(self.time));

        // Deschedule next think.
        ent.store(FieldAddrFloat::NextThink, 0.0)?;

        // Call entity's think function.
        let think = ent.load(FieldAddrFunctionId::Think)?;
        self.globals.store(GlobalAddrFloat::Time, think_time)?;
        self.globals.store(GlobalAddrEntity::Self_, ent_id)?;
        self.globals.store(GlobalAddrEntity::Other, EntityId(0))?;
        self.execute_program(think, registry)?;

        Ok(self.world.exists(ent_id))
    }

    pub fn start_frame(&mut self, mut registry: Mut<Registry>) -> progs::Result<()> {
        self.cx.reset();

        self.globals.store(GlobalAddrEntity::Self_, EntityId(0))?;
        self.globals.store(GlobalAddrEntity::Other, EntityId(0))?;
        self.globals.store(GlobalAddrEntity::World, EntityId(0))?;
        self.globals.store(GlobalAddrFloat::Time, duration_to_f32(self.time))?;

        let start_frame = self.globals.get_function_id(GlobalAddrFunction::StartFrame as i16)?;
        self.execute_program(start_frame, registry.reborrow())?;

        Ok(())
    }

    /// Moves an entity along its velocity for `frame_time`, sliding along anything it hits.
    ///
    /// If the entity lands on a brush entity, it is marked as on the ground. The returned
    /// metadata is the last trace which hit a wall or step, if any.
    ///
    /// Equivalent to `SV_FlyMove`.
    fn physics_player_fly(
        &mut self,
        ent_id: EntityId,
        frame_time: Duration,
        mut registry: Mut<Registry>,
    ) -> progs::Result<CollisionResult<Option<Trace>>> {
        const BUMP_COUNT: usize = 4;
        const MAX_CLIP_PLANES: usize = 5;

        let mut out = CollisionResult::default();

        let primal_velocity = self.world.get(ent_id)?.velocity()?;
        let mut original_velocity = primal_velocity;
        // Normals of the planes the entity is touching. Unlike our traces (but like Quake), these
        // face out of the surface.
        let mut planes = ArrayVec::<Vec3, MAX_CLIP_PLANES>::new();

        let mut time_left: f32 = duration_to_f32(frame_time);

        for _ in 0..BUMP_COUNT {
            let mut ent = self.world.get(ent_id)?;
            let velocity = ent.velocity()?;
            if (time_left * velocity).length_squared() < f32::EPSILON {
                break;
            }

            let origin = ent.origin()?;
            let min = ent.min()?;
            let max = ent.max()?;

            let end = origin + time_left * velocity;

            let (trace, touched_ent_id) = self.world.trace_entity_move(
                ent_id,
                origin,
                min,
                max,
                end,
                CollideKind::Normal,
                |_| true,
            )?;

            if trace.all_solid() {
                // Entity is trapped in another solid.
                self.world.get_mut(ent_id)?.set_velocity(Vec3::ZERO)?;
                return Ok(CollisionResult { floor: true, wall_or_step: true, ..out });
            }

            if trace.ratio() > 0. {
                // If ratio > 0, we moved some distance.
                let mut ent = self.world.get_mut(ent_id)?;
                ent.set_origin(trace.end_point())?;
                original_velocity = ent.velocity()?;
                planes.clear();
            }

            let Some(plane) = trace.plane() else {
                // Travelled full trace distance.
                break;
            };

            let Some(hit) = touched_ent_id else {
                return Err(ProgsError::with_msg(format!(
                    "fly_move: trace for {ent_id} hit a surface but no entity"
                )));
            };

            let normal = -plane.normal();

            if normal.z > 0.7 {
                out.floor = true;
                if self.world.get(hit)?.solid()? == EntitySolid::Bsp {
                    let mut ent = self.world.get_mut(ent_id)?;
                    ent.add_flags(EntityFlags::ON_GROUND)?;
                    ent.set_ground(hit)?;
                }
            }

            if normal.z == 0. {
                out.wall_or_step = true;
                // Saved for the player's extra wall friction.
                out.metadata = Some(trace.clone());
            }

            self.impact_entities(ent_id, hit, registry.reborrow())?;
            if !self.world.exists(ent_id) {
                // Removed by the impact function.
                break;
            }

            time_left -= time_left * trace.ratio();

            if planes.try_push(normal).is_err() {
                // This shouldn't really happen.
                self.world.get_mut(ent_id)?.set_velocity(Vec3::ZERO)?;
                return Ok(CollisionResult { floor: true, wall_or_step: true, ..out });
            }

            // Modify `original_velocity` so that it is parallel to all of the clip planes.
            let along_plane = planes.iter().enumerate().find_map(|(i, &plane_i)| {
                let new_velocity = clip_velocity(original_velocity, plane_i, 1.);
                planes
                    .iter()
                    .enumerate()
                    .all(|(j, &plane_j)| i == j || new_velocity.dot(plane_j) >= 0.)
                    .then_some(new_velocity)
            });

            let new_velocity = match along_plane {
                Some(new_velocity) => new_velocity,
                // Go along the crease.
                None if planes.len() == 2 => {
                    let dir = planes[0].cross(planes[1]);
                    dir * dir.dot(self.world.get(ent_id)?.velocity()?)
                }
                None => {
                    self.world.get_mut(ent_id)?.set_velocity(Vec3::ZERO)?;
                    return Ok(CollisionResult {
                        floor: true,
                        wall_or_step: true,
                        dead_stop: true,
                        ..out
                    });
                }
            };

            // If the new velocity is against the original velocity, stop dead to avoid tiny
            // oscillations in sloping corners.
            if new_velocity.dot(primal_velocity) <= 0. {
                self.world.get_mut(ent_id)?.set_velocity(Vec3::ZERO)?;
                return Ok(out);
            }

            self.world.get_mut(ent_id)?.set_velocity(new_velocity)?;
        }

        Ok(out)
    }

    fn physics_walk(
        &mut self,
        ent_id: EntityId,
        frame_time: Duration,
        server_vars: &ServerVars,
        mut registry: Mut<Registry>,
    ) -> progs::Result<()> {
        const ORIGIN_EPSILON: f32 = 0.03125;

        let mut ent = self.world.get_mut(ent_id)?;

        let water_jump = ent.has_flag(EntityFlags::WATER_JUMP)?;
        let was_on_ground = ent.has_flag(EntityFlags::ON_GROUND)?;
        ent.remove_flags(EntityFlags::ON_GROUND)?;

        let old_origin = ent.origin()?;
        let old_velocity = ent.velocity()?;
        let water_level = ent.water_level()?;

        let clip = self.physics_player_fly(ent_id, frame_time, registry.reborrow())?;

        if !self.world.exists(ent_id) {
            return Ok(());
        }

        // Read after moving, since if we moved into e.g. a trigger that kills the entity
        // then we don't want to do anything here.
        let move_kind = self.world.get(ent_id)?.move_kind()?;

        if
        // Move didn't block on a wall/step.
        !clip.wall_or_step ||
            // Entity is in the air, don't apply step logic.
            water_level < f32::EPSILON && !was_on_ground ||
            // Entity isn't "walk" type
            move_kind != MoveKind::Walk ||
            // TODO: `sv_nostep`
            water_jump
        {
            return Ok(());
        }

        let mut ent = self.world.get_mut(ent_id)?;

        let pre_step_origin = ent.origin()?;
        let pre_step_velocity = ent.velocity()?;

        ent.set_origin(old_origin)?;

        let upmove = Vec3::Z * server_vars.max_step;
        let downmove =
            Vec3::Z * (old_velocity.z * duration_to_f32(frame_time) - server_vars.max_step);

        ent.set_velocity(old_velocity.with_z(0.))?;

        self.push_entity(ent_id, upmove, registry.reborrow())?;

        let clip = self.physics_player_fly(ent_id, frame_time, registry.reborrow())?;

        let mut ent = self.world.get(ent_id)?;
        let post_step_origin = ent.origin()?;
        let post_clip_move = (post_step_origin - old_origin).xy().abs();

        if clip.any_collision() && post_clip_move.to_array().iter().all(|v| *v < ORIGIN_EPSILON) {
            error!("TODO: SV_TryUnstick");
        }

        if clip.wall_or_step {
            error!("TODO: SV_WallFriction");
        }

        let (downtrace, down_ent) = self.push_entity(ent_id, downmove, registry.reborrow())?;
        if downtrace.plane().map(|plane| plane.is_floor()).unwrap_or_default() {
            let mut ent = self.world.get_mut(ent_id)?;
            // This checks the moving entity rather than the ground, as Quake does. Players aren't
            // `SOLID_BSP`, so in practice `physics_player_fly` sets the ground instead.
            if ent.solid()? == EntitySolid::Bsp {
                ent.add_flags(EntityFlags::ON_GROUND)?;
                ent.set_ground(down_ent.unwrap_or(EntityId(0)))?;
            }
        } else {
            // The step down didn't end up on good ground, so use the move without the step up.
            // This happens near wall/slope combinations.
            debug!("Tried to step but could not");
            let mut ent = self.world.get_mut(ent_id)?;

            ent.set_origin(pre_step_origin)?;
            ent.set_velocity(pre_step_velocity)?;
        }

        Ok(())
    }

    fn ent_physics(
        &mut self,
        ent_id: EntityId,
        frame_time: Duration,
        mut registry: Mut<Registry>,
    ) -> progs::Result<()> {
        let server_vars: ServerVars = registry.read_cvars()?;

        let mut ent = self.world.get(ent_id)?;
        match ent.move_kind()? {
            MoveKind::Walk => {
                self.physics_walk(ent_id, frame_time, &server_vars, registry.reborrow())?
            }

            MoveKind::Push => self.physics_push(ent_id, frame_time, registry.reborrow())?,
            MoveKind::NoClip => self.physics_noclip(ent_id, frame_time, registry.reborrow())?,
            MoveKind::Step => self.physics_step(ent_id, frame_time, registry.reborrow())?,
            MoveKind::Fly | MoveKind::FlyMissile | MoveKind::Bounce | MoveKind::Toss => {
                self.physics_toss(ent_id, frame_time, registry.reborrow())?
            }
            // No actual physics for this entity, but still let it think.
            MoveKind::None => {
                self.think(ent_id, frame_time, registry.reborrow())?;
            }

            MoveKind::AngleNoClip | MoveKind::AngleClip => {
                warn!("TODO: Fly physics");
                self.think(ent_id, frame_time, registry.reborrow())?;
            }
        }

        Ok(())
    }

    pub fn physics_client(
        &mut self,
        player_id: EntityId,
        frame_time: Duration,
        mut registry: Mut<Registry>,
    ) -> progs::Result<()> {
        let server_vars: ServerVars = registry.read_cvars()?;

        let pre_think = self.globals.load(GlobalAddrFunction::PlayerPreThink)?;
        self.globals.store(GlobalAddrFloat::Time, duration_to_f32(self.time))?;
        self.globals.store(GlobalAddrEntity::Self_, player_id)?;
        self.execute_program(pre_think, registry.reborrow())?;

        let mut player = self.world.get_mut(player_id)?;

        player.limit_velocity(server_vars.max_entity_velocity)?;

        match player.move_kind()? {
            MoveKind::None => {
                if !self.think(player_id, frame_time, registry.reborrow())? {
                    return Ok(());
                }
            }
            MoveKind::Walk => {
                if !self.think(player_id, frame_time, registry.reborrow())? {
                    return Ok(());
                }

                if !self.check_water(player_id)?
                    && !self.world.get(player_id)?.has_flag(EntityFlags::WATER_JUMP)?
                {
                    self.world.get_mut(player_id)?.apply_gravity(
                        &self.string_table,
                        server_vars.gravity,
                        frame_time,
                    )?;
                }

                // TODO: SV_CheckStuck
                self.physics_walk(player_id, frame_time, &server_vars, registry.reborrow())?;
            }
            // e.g. dead players.
            MoveKind::Toss | MoveKind::Bounce => {
                self.physics_toss(player_id, frame_time, registry.reborrow())?;
            }
            MoveKind::Fly => {
                if !self.think(player_id, frame_time, registry.reborrow())? {
                    return Ok(());
                }

                self.physics_player_fly(player_id, frame_time, registry.reborrow())?;
            }
            MoveKind::NoClip => {
                if !self.think(player_id, frame_time, registry.reborrow())? {
                    return Ok(());
                }

                let mut player = self.world.get_mut(player_id)?;
                let origin = player.origin()? + duration_to_f32(frame_time) * player.velocity()?;
                player.set_origin(origin)?;
            }
            other => {
                return Err(ProgsError::with_msg(format!(
                    "physics_client: bad movetype {other:?}"
                )));
            }
        }

        if !self.world.exists(player_id) {
            return Ok(());
        }

        self.link_entity(player_id, true, registry.reborrow())?;

        let post_think = self.globals.load(GlobalAddrFunction::PlayerPostThink)?;
        self.globals.store(GlobalAddrFloat::Time, duration_to_f32(self.time))?;
        self.globals.store(GlobalAddrEntity::Self_, player_id)?;
        self.execute_program(post_think, registry)?;

        Ok(())
    }

    pub fn physics(
        &mut self,
        clients: &ClientSlots,
        frame_time: Duration,
        mut registry: Mut<Registry>,
    ) -> progs::Result<HashMap<EntityId, ProgsError>> {
        self.globals.store(GlobalAddrFloat::FrameTime, duration_to_f32(frame_time))?;

        let mut ent_error_map = HashMap::<EntityId, ProgsError>::new();

        // As in Quake's `SV_RunClients`, apply each client's latest movement once per frame,
        // before running physics.
        for (_, ClientActive { entity_id, cmd, .. }) in clients.active_clients() {
            if let Err(e) = self.client_think(*entity_id, cmd, frame_time, &registry) {
                ent_error_map.insert(*entity_id, e);
            }
        }

        self.start_frame(registry.reborrow())?;

        for (_, ClientActive { entity_id, .. }) in clients.active_clients() {
            if let Err(e) = self.physics_client(*entity_id, frame_time, registry.reborrow()) {
                ent_error_map.insert(*entity_id, e);
            }
        }

        for ent_id in self.world.iter().collect::<Vec<_>>() {
            if self.globals.load(GlobalAddrFloat::ForceRetouch)? != 0.0 {
                // Force all entities to touch triggers, even if they didn't
                // move. This is required when e.g. creating new triggers, as
                // stationary entities typically do not get relinked, and so
                // will ignore new triggers even when touching them.
                //
                // TODO: this may have a subtle ordering bug. If entity 2 has
                // physics run, sets ForceRetouch and spawns entity 1, then
                // entity 1 will not have a chance to touch triggers this frame.
                // Quake solves this by using a linked list and always spawning
                // at the end so that newly spawned entities always have physics
                // run this frame.
                if let Err(e) = self.link_entity(ent_id, true, registry.reborrow()) {
                    ent_error_map.insert(ent_id, e);
                    continue;
                }
            }

            if !self.world.exists(ent_id) {
                continue;
            }

            let max_clients = clients.limit();
            if !ent_id.is_none()
                && ent_id.0 as usize > max_clients
                && let Err(e) = self.ent_physics(ent_id, frame_time, registry.reborrow())
            {
                ent_error_map.insert(ent_id, e);
            }
        }

        // QuakeC sets this to 2 so that entities retouch for two frames.
        let force_retouch = self.globals.load(GlobalAddrFloat::ForceRetouch)?;
        if force_retouch != 0.0 {
            self.globals.store(GlobalAddrFloat::ForceRetouch, force_retouch - 1.)?;
        }

        self.time += frame_time;

        Ok(ent_error_map)
    }

    /// Applies ground friction to a player, as in Quake's `SV_UserFriction`.
    ///
    /// This should only be called when the player is on the ground and not swimming.
    fn player_apply_friction(
        &mut self,
        ent_id: EntityId,
        frame_time: Duration,
        player_vars: &PlayerVars,
    ) -> progs::Result<()> {
        /// How far ahead of the player (in the direction of movement) to look for a dropoff.
        const DROPOFF_DETECT_DIST: f32 = 16.;
        /// How far below the player's feet the ground must be for it to count as a dropoff.
        const DROPOFF_TRACE_Z: f32 = 34.;

        let mut ent = self.world.get(ent_id)?;

        let velocity = ent.velocity()?;
        let speed = velocity.xy().length();

        if speed == 0. {
            return Ok(());
        }

        // If the leading edge is over a dropoff, increase friction.
        let origin = ent.origin()?;
        let min = ent.min()?;
        let start =
            (origin.xy() + velocity.xy() / speed * DROPOFF_DETECT_DIST).extend(origin.z + min.z);
        let end = start.with_z(start.z - DROPOFF_TRACE_Z);

        let (trace, _) = self.world.trace_entity_move(
            ent_id,
            start,
            Vec3::ZERO,
            Vec3::ZERO,
            end,
            CollideKind::NoMonsters,
            |_| true,
        )?;

        let friction = if trace.is_terminal() {
            player_vars.ground_friction * player_vars.edge_friction
        } else {
            player_vars.ground_friction
        };

        let scale =
            friction_scale(speed, friction, player_vars.stop_speed, duration_to_f32(frame_time));

        self.world.get_mut(ent_id)?.set_velocity(velocity * scale)?;

        Ok(())
    }

    /// Applies a client's latest movement command to its player entity, as in Quake's
    /// `SV_ClientThink` (and `SV_AirMove`).
    pub fn client_think(
        &mut self,
        ent_id: EntityId,
        cmd: &MoveCmd,
        frame_time: Duration,
        registry: &Registry,
    ) -> progs::Result<()> {
        /// How fast the view kick (`punchangle`) decays, in degrees per second.
        const PUNCH_ANGLE_DECAY: f32 = 10.;

        let player_vars: PlayerVars = registry.read_cvars()?;
        let dt = duration_to_f32(frame_time);
        let time = duration_to_f32(self.time);

        let mut ent = self.world.get_mut(ent_id)?;

        let move_kind = ent.move_kind()?;
        if move_kind == MoveKind::None {
            return Ok(());
        }

        let on_ground = ent.has_flag(EntityFlags::ON_GROUND)?;

        // `DropPunchAngle`
        let punch_angle = ent.punch_angle()?;
        let punch_len = (punch_angle.length() - PUNCH_ANGLE_DECAY * dt).max(0.);
        ent.set_punch_angle(punch_angle.normalize_or_zero() * punch_len)?;

        // Dead players can't move.
        if ent.health()? <= 0. {
            return Ok(());
        }

        // Show the player model with 1/3 of the view pitch, and roll it when strafing.
        let v_angle = ent.view_angle()? + ent.punch_angle()?;
        let mut angles = ent.angles()?;
        angles.z = calc_roll(angles, ent.velocity()?) * 4.;
        if !ent.get_bool(FieldAddrFloat::FixAngle as i16)? {
            angles.x = -v_angle.x / 3.;
            angles.y = v_angle.y;
        }
        ent.set_angles(angles)?;

        if ent.has_flag(EntityFlags::WATER_JUMP)? {
            // `SV_WaterJump`
            if time > ent.load(FieldAddrFloat::TeleportTime)? || ent.water_level()? == 0. {
                ent.remove_flags(EntityFlags::WATER_JUMP)?;
                ent.store(FieldAddrFloat::TeleportTime, 0.)?;
            }

            let jump_velocity = ent.move_dir()?;
            let velocity = ent.velocity()?;
            ent.set_velocity(jump_velocity.xy().extend(velocity.z))?;

            return Ok(());
        }

        if ent.water_level()? >= 2. && move_kind != MoveKind::NoClip {
            warn_once!("TODO: SV_WaterMove");
            return Ok(());
        }

        // `SV_AirMove`
        let AngleVectors { forward, right, .. } = angle_vectors(angles);

        let mut fwd_move = cmd.forward;
        // If the player is still in a teleporter, wait until they leave it
        // before allowing them to teleport again.
        if time < ent.load(FieldAddrFloat::TeleportTime)? && fwd_move < 0. {
            fwd_move = 0.;
        }

        let mut wish_vel = forward * fwd_move + right * cmd.side;
        wish_vel.z = if move_kind == MoveKind::Walk { 0. } else { cmd.up };

        let mut wish_speed = wish_vel.length();
        if wish_speed > player_vars.max_speed {
            wish_vel *= player_vars.max_speed / wish_speed;
            wish_speed = player_vars.max_speed;
        }
        let wish_dir = wish_vel.normalize_or_zero();

        let accel_speed = player_vars.accel * dt * wish_speed;

        if move_kind == MoveKind::NoClip {
            ent.set_velocity(wish_vel)?;
        } else if on_ground {
            self.player_apply_friction(ent_id, frame_time, &player_vars)?;

            let mut ent = self.world.get_mut(ent_id)?;
            let velocity = accelerate(ent.velocity()?, wish_dir, wish_speed, accel_speed);
            ent.set_velocity(velocity)?;
        } else {
            // Not on the ground, so there is little effect on velocity.
            let wish_speed = wish_speed.min(player_vars.max_air_speed);
            let velocity = accelerate(ent.velocity()?, wish_dir, wish_speed, accel_speed);
            ent.set_velocity(velocity)?;
        }

        Ok(())
    }

    /// Returns the contents of the world at `point`.
    ///
    /// Currents count as water.
    ///
    /// Equivalent to `SV_PointContents`.
    fn point_contents(&self, point: Vec3) -> BspLeafContents {
        match self.world.contents_at_point(point) {
            c if c as i32 >= BspLeafContents::Current0 as i32 => BspLeafContents::Water,
            c => c,
        }
    }

    /// Updates an entity's `waterlevel` and `watertype`.
    ///
    /// Returns whether the entity is swimming (`waterlevel > 1`).
    ///
    /// Equivalent to `SV_CheckWater`.
    fn check_water(&mut self, ent_id: EntityId) -> progs::Result<bool> {
        let mut ent = self.world.get(ent_id)?;
        let origin = ent.origin()?;
        let min = ent.min()?;
        let max = ent.max()?;
        let view_ofs = ent.view_ofs()?;

        let mut water_level = 0.;
        let mut water_type = quake_contents(BspLeafContents::Empty);

        // Feet
        let contents = self.point_contents(origin.with_z(origin.z + min.z + 1.));
        if is_liquid(contents) {
            water_type = quake_contents(contents);
            water_level = 1.;

            // Waist
            if is_liquid(self.point_contents(origin.with_z(origin.z + (min.z + max.z) * 0.5))) {
                water_level = 2.;

                // Eyes
                if is_liquid(self.point_contents(origin.with_z(origin.z + view_ofs.z))) {
                    water_level = 3.;
                }
            }
        }

        let mut ent = self.world.get_mut(ent_id)?;
        ent.set_water_level(water_level)?;
        ent.store(FieldAddrFloat::Contents, water_type)?;

        Ok(water_level > 1.)
    }

    /// Updates a non-player entity's `watertype` and plays a splash sound if it entered or left
    /// water.
    ///
    /// Equivalent to `SV_CheckWaterTransition`.
    fn check_water_transition(&mut self, ent_id: EntityId) -> progs::Result<()> {
        const SPLASH_SOUND: &str = "misc/h2ohit1.wav";

        let origin = self.world.get(ent_id)?.origin()?;
        let contents = self.point_contents(origin);

        let mut ent = self.world.get_mut(ent_id)?;
        let old_water_type = ent.load(FieldAddrFloat::Contents)?;

        if old_water_type == 0. {
            // Just spawned here.
            ent.store(FieldAddrFloat::Contents, quake_contents(contents))?;
            ent.set_water_level(1.)?;
            return Ok(());
        }

        let splash = if is_liquid(contents) {
            ent.store(FieldAddrFloat::Contents, quake_contents(contents))?;
            ent.set_water_level(1.)?;
            old_water_type == quake_contents(BspLeafContents::Empty)
        } else {
            ent.store(FieldAddrFloat::Contents, quake_contents(BspLeafContents::Empty))?;
            // Quake stores the contents here rather than a water level.
            ent.set_water_level(quake_contents(contents))?;
            old_water_type != quake_contents(BspLeafContents::Empty)
        };

        if splash {
            self.sound(ent_id, 0, SPLASH_SOUND, 1., 1.)?;
        }

        Ok(())
    }

    pub fn physics_push(
        &mut self,
        ent_id: EntityId,
        frame_time: Duration,
        mut registry: Mut<Registry>,
    ) -> progs::Result<()> {
        let mut ent = self.world.get_mut(ent_id)?;

        let old_local_time = duration_from_f32(ent.load(FieldAddrFloat::LocalTime)?);
        let next_think_secs: f32 = ent.load(FieldAddrFloat::NextThink)?;

        if next_think_secs == 0. {
            return Ok(());
        }

        let next_think = duration_from_f32(next_think_secs);

        let new_local_time = old_local_time + frame_time;

        let move_time = if next_think < new_local_time {
            // As in Quake's `SV_Physics_Pusher`, clamp to zero if the think is overdue.
            next_think.saturating_sub(old_local_time)
        } else {
            frame_time
        };

        if move_time > Duration::ZERO {
            self.move_push(ent_id, move_time, registry.reborrow())?;
        }

        let mut ent = self.world.get_mut(ent_id)?;
        let new_local_time = duration_from_f32(ent.local_time()?);

        if (Bound::Excluded(old_local_time), Bound::Included(new_local_time)).contains(&next_think)
        {
            // Deschedule next think
            ent.store(FieldAddrFloat::NextThink, 0.)?;

            // Call entity's think function.
            let think = ent.load(FieldAddrFunctionId::Think)?;
            self.globals.store(GlobalAddrFloat::Time, duration_to_f32(self.time))?;
            self.globals.store(GlobalAddrEntity::Self_, ent_id)?;
            self.globals.store(GlobalAddrEntity::Other, EntityId(0))?;
            self.execute_program(think, registry)?;
        }

        Ok(())
    }

    /// Moves an entity with `MOVETYPE_NOCLIP`, as in Quake's `SV_Physics_Noclip`.
    pub fn physics_noclip(
        &mut self,
        ent_id: EntityId,
        frame_time: Duration,
        mut registry: Mut<Registry>,
    ) -> progs::Result<()> {
        if !self.think(ent_id, frame_time, registry.reborrow())? {
            // Entity was removed by its think function.
            return Ok(());
        }

        let mut ent = self.world.get_mut(ent_id)?;

        let frame_time_f = duration_to_f32(frame_time);

        let angles: Vec3 = ent.load(FieldAddrVector::Angles)?;
        let angle_vel: Vec3 = ent.load(FieldAddrVector::AngularVelocity)?;
        ent.store(FieldAddrVector::Angles, angles + frame_time_f * angle_vel)?;

        let orig: Vec3 = ent.load(FieldAddrVector::Origin)?;
        let vel: Vec3 = ent.load(FieldAddrVector::Velocity)?;
        ent.store(FieldAddrVector::Origin, orig + frame_time_f * vel)?;

        self.link_entity(ent_id, false, registry)
    }

    /// Runs physics for a monster.
    ///
    /// Monsters only fall when they aren't on the ground (or flying/swimming): otherwise they are
    /// moved by `walkmove`/`movetogoal`.
    ///
    /// Equivalent to `SV_Physics_Step`.
    pub fn physics_step(
        &mut self,
        ent_id: EntityId,
        frame_time: Duration,
        mut registry: Mut<Registry>,
    ) -> progs::Result<()> {
        const LAND_SOUND: &str = "demon/dland2.wav";

        let ServerVars { gravity, max_entity_velocity: max_velocity, .. } =
            registry.read_cvars()?;

        let in_freefall = !self
            .world
            .get(ent_id)?
            .flags()?
            .intersects(EntityFlags::ON_GROUND | EntityFlags::FLY | EntityFlags::SWIM);

        if in_freefall {
            let vel = self.world.get(ent_id)?.velocity()?;

            // If true, play an impact sound when the entity hits the ground.
            let hit_sound = vel.z < -0.1 * gravity;

            self.world.get_mut(ent_id)?.apply_gravity(&self.string_table, gravity, frame_time)?;

            self.world.get_mut(ent_id)?.limit_velocity(max_velocity)?;

            // Move the entity and relink it.
            self.physics_player_fly(ent_id, frame_time, registry.reborrow())?;
            self.link_entity(ent_id, true, registry.reborrow())?;

            if !self.world.exists(ent_id) {
                return Ok(());
            }

            if self.world.get(ent_id)?.has_flag(EntityFlags::ON_GROUND)? && hit_sound {
                // Entity hit the ground this frame.
                self.sound(ent_id, 0, LAND_SOUND, 1., 1.)?;
            }
        }

        if !self.think(ent_id, frame_time, registry)? {
            return Ok(());
        }

        self.check_water_transition(ent_id)
    }

    /// Runs physics for a projectile or other free-moving entity.
    ///
    /// This handles `MOVETYPE_TOSS`, `MOVETYPE_BOUNCE`, `MOVETYPE_FLY` and `MOVETYPE_FLYMISSILE`.
    ///
    /// Equivalent to `SV_Physics_Toss`.
    pub fn physics_toss(
        &mut self,
        ent_id: EntityId,
        frame_time: Duration,
        mut registry: Mut<Registry>,
    ) -> progs::Result<()> {
        /// Bouncing entities with a lower upward speed than this after a bounce come to rest.
        const BOUNCE_STOP_SPEED: f32 = 60.;

        let ServerVars { gravity, max_entity_velocity: max_velocity, .. } =
            registry.read_cvars()?;

        if !self.think(ent_id, frame_time, registry.reborrow())? {
            return Ok(());
        }

        let mut ent = self.world.get_mut(ent_id)?;

        // If on the ground, don't move.
        if ent.has_flag(EntityFlags::ON_GROUND)? {
            return Ok(());
        }

        ent.limit_velocity(max_velocity)?;

        if !matches!(ent.move_kind()?, MoveKind::Fly | MoveKind::FlyMissile) {
            ent.apply_gravity(&self.string_table, gravity, frame_time)?;
        }

        let dt = duration_to_f32(frame_time);

        let angles = ent.angles()?;
        let angular_velocity = ent.angular_velocity()?;
        ent.set_angles(angles + dt * angular_velocity)?;

        let frame_velocity = ent.velocity()? * dt;

        let (trace, hit) = self.push_entity(ent_id, frame_velocity, registry)?;

        let Some(plane) = trace.plane() else {
            // Didn't hit anything.
            return Ok(());
        };

        if !self.world.exists(ent_id) {
            return Ok(());
        }

        let normal = -plane.normal();

        let mut ent = self.world.get_mut(ent_id)?;
        let is_bounce = ent.move_kind()? == MoveKind::Bounce;
        let overbounce = if is_bounce { 1.5 } else { 1. };
        let velocity = clip_velocity(ent.velocity()?, normal, overbounce);
        ent.set_velocity(velocity)?;

        // Stop if on the ground.
        if normal.z > 0.7 && (velocity.z < BOUNCE_STOP_SPEED || !is_bounce) {
            ent.add_flags(EntityFlags::ON_GROUND)?;
            ent.set_ground(hit.unwrap_or(EntityId(0)))?;
            ent.set_velocity(Vec3::ZERO)?;
            ent.set_angular_velocity(Vec3::ZERO)?;
        }

        self.check_water_transition(ent_id)
    }

    fn push_entity(
        &mut self,
        ent_id: EntityId,
        move_dir: Vec3,
        mut registry: Mut<Registry>,
    ) -> progs::Result<(Trace, Option<EntityId>)> {
        let mut ent = self.world.get(ent_id)?;
        let start = ent.origin()?;
        let end = start + move_dir;
        let min = ent.min()?;
        let max = ent.max()?;

        let kind = match (ent.move_kind()?, ent.solid()?) {
            (MoveKind::FlyMissile, _) => CollideKind::Missile,
            (_, EntitySolid::Not | EntitySolid::Trigger) => CollideKind::NoMonsters,
            _ => CollideKind::Normal,
        };

        let (trace, hit) =
            self.world.trace_entity_move(ent_id, start, min, max, end, kind, |_| true)?;

        let mut ent = self.world.get_mut(ent_id)?;

        ent.set_origin(trace.end_point())?;

        self.link_entity(ent_id, true, registry.reborrow())?;

        if let Some(hit) = hit {
            self.impact_entities(ent_id, hit, registry)?;
        }

        Ok((trace, hit))
    }

    /// Moves a pusher (e.g. a door or platform) and anything it pushes.
    ///
    /// If the pusher is blocked, everything is moved back and the pusher's `blocked` function is
    /// called.
    ///
    /// Equivalent to `SV_PushMove`.
    fn move_push(
        &mut self,
        pusher_id: EntityId,
        move_time: Duration,
        mut registry: Mut<Registry>,
    ) -> progs::Result<()> {
        let mut pusher = self.world.get_mut(pusher_id)?;

        let move_time_f32 = duration_to_f32(move_time);
        let velocity = pusher.velocity()?;

        let new_local_time = pusher.local_time()? + move_time_f32;
        pusher.set_local_time(new_local_time)?;

        if velocity == Vec3::ZERO {
            return Ok(());
        }

        let move_amt = velocity * move_time_f32;

        // The pusher's bounds at its final position.
        let push_min = pusher.abs_min()? + move_amt;
        let push_max = pusher.abs_max()? + move_amt;

        let pusher_origin = pusher.origin()?;

        // Move the pusher to its final position.
        debug!("Moving pusher by {move_amt}");
        pusher.set_origin(pusher_origin + move_amt)?;
        self.link_entity(pusher_id, false, registry.reborrow())?;

        // Entities which have been moved, and where they were moved from.
        let mut moved = Vec::<(EntityId, Vec3)>::new();

        // See if any solid entities are inside the final position.
        // TODO: Don't need to collect here
        for other_id in self.world.range(1..).filter(|e| *e != pusher_id).collect::<Vec<_>>() {
            if !self.world.exists(other_id) {
                continue;
            }

            let mut other = self.world.get(other_id)?;

            if matches!(other.move_kind()?, MoveKind::Push | MoveKind::None | MoveKind::NoClip) {
                continue;
            }

            // If the entity is standing on the pusher, it will definitely be moved.
            if !(other.has_flag(EntityFlags::ON_GROUND)? && other.ground()? == pusher_id) {
                let other_min = other.abs_min()?;
                let other_max = other.abs_max()?;
                let outside =
                    (0..3).any(|i| other_min[i] >= push_max[i] || other_max[i] <= push_min[i]);

                // Skip the entity unless it's inside the pusher's final position.
                if outside || !self.world.entity_is_stuck(other_id, |_| true)? {
                    continue;
                }
            }

            let mut other = self.world.get_mut(other_id)?;

            // Remove the ground flag for non-players.
            if other.move_kind()? != MoveKind::Walk {
                other.remove_flags(EntityFlags::ON_GROUND)?;
            }

            let old_origin = other.origin()?;
            moved.push((other_id, old_origin));

            // Try moving the other entity, ignoring the pusher.
            self.world
                .get_mut(pusher_id)?
                .store(FieldAddrFloat::Solid, EntitySolid::Not as u32 as f32)?;
            let push_result = self.push_entity(other_id, move_amt, registry.reborrow());
            if self.world.exists(pusher_id) {
                self.world
                    .get_mut(pusher_id)?
                    .store(FieldAddrFloat::Solid, EntitySolid::Bsp as u32 as f32)?;
            }
            push_result?;

            if !self.world.exists(pusher_id) {
                // Removed by a touch function.
                return Ok(());
            }

            if !self.world.exists(other_id) {
                // Removed by a touch function.
                moved.pop();
                continue;
            }

            // If it is still inside the pusher, the pusher is blocked.
            if !self.world.entity_is_stuck(other_id, |_| true)? {
                continue;
            }

            let mut other = self.world.get_mut(other_id)?;

            if other.min()?.x == other.max()?.x {
                continue;
            }

            let solid = other.solid()?;
            if solid == EntitySolid::Not || solid == EntitySolid::Trigger {
                // This is a corpse.
                let mins = Vec3 { x: 0., y: 0., z: other.min()?.z };

                other.set_min(mins)?;
                other.set_max(mins)?;
                continue;
            }

            other.set_origin(old_origin)?;
            self.link_entity(other_id, true, registry.reborrow())?;

            debug!("Resetting pusher origin");
            self.world.get_mut(pusher_id)?.set_origin(pusher_origin)?;
            self.link_entity(pusher_id, false, registry.reborrow())?;

            let mut pusher = self.world.get_mut(pusher_id)?;
            let new_local_time = pusher.local_time()? - move_time_f32;
            pusher.set_local_time(new_local_time)?;

            // If the pusher has a `blocked` function, call it. Otherwise, just stay in place until
            // the obstacle is gone.
            let pusher_blocked_fn = pusher.load(FieldAddrFunctionId::Blocked)?;

            if !pusher_blocked_fn.is_none() {
                self.globals.store(GlobalAddrEntity::Self_, pusher_id)?;
                self.globals.store(GlobalAddrEntity::Other, other_id)?;
                self.globals.store(GlobalAddrEntity::World, EntityId(0))?;
                self.execute_program(pusher_blocked_fn, registry.reborrow())?;
            }

            // Move back any entities we already moved.
            debug!("Resetting all positions");
            for (ent, original_position) in moved {
                if !self.world.exists(ent) {
                    continue;
                }

                self.world.get_mut(ent)?.set_origin(original_position)?;
                self.link_entity(ent, false, registry.reborrow())?;
            }

            return Ok(());
        }

        Ok(())
    }

    const DROP_TO_FLOOR_DIST: f32 = 256.0;

    /// Moves an entity straight down until it collides with a solid surface.
    ///
    /// Returns `true` if the entity hit the floor, `false` otherwise.
    ///
    /// ## Notes
    /// - The drop distance is limited to 256, so entities which are more than 256 units above a
    ///   solid surface will not actually hit the ground.
    pub fn drop_entity_to_floor(
        &mut self,
        ent_id: EntityId,
        registry: Mut<Registry>,
    ) -> progs::Result<bool> {
        debug!("Finding floor for entity with ID {}", ent_id.0);
        let mut ent = self.world.get(ent_id)?;
        let origin = ent.origin()?;

        let end = Vec3::new(origin.x, origin.y, origin.z - Self::DROP_TO_FLOOR_DIST);
        let min = ent.min()?;
        let max = ent.max()?;

        let (trace, collide_entity) = self.world.trace_entity_move(
            ent_id,
            origin,
            min,
            max,
            end,
            CollideKind::Normal,
            |_| true,
        )?;
        debug!("End position after drop: {:?}", trace.end_point());

        if trace.is_terminal() || trace.all_solid() {
            // Entity didn't hit the floor or is stuck.
            return Ok(false);
        }

        // Entity hit the floor. Update origin, relink and set ON_GROUND flag.
        self.world.get_mut(ent_id)?.set_origin(trace.end_point())?;
        self.link_entity(ent_id, false, registry)?;
        let mut ent = self.world.get_mut(ent_id)?;
        ent.add_flags(EntityFlags::ON_GROUND)?;
        ent.set_ground(collide_entity.unwrap_or(EntityId(0)))?;

        Ok(true)
    }

    pub fn touch_triggers(
        &mut self,
        ent_id: EntityId,
        mut registry: Mut<Registry>,
    ) -> progs::Result<()> {
        thread_local! {
            static TOUCHED_TRIGGERS_BUF: RefCell<Vec<EntityId>> = Vec::new().into();
        }

        let Some(area) = self.world.entity_area(ent_id)? else {
            return Ok(());
        };

        TOUCHED_TRIGGERS_BUF.with_borrow_mut(|touched| -> progs::Result<()> {
            touched.clear();

            self.world.list_touched_triggers(touched, ent_id, area)?;

            // Save state.
            let restore_self = self.globals.load(GlobalAddrEntity::Self_)?;
            let restore_other = self.globals.load(GlobalAddrEntity::Other)?;

            // Activate the touched triggers.
            for trigger_id in &*touched {
                let trigger_touch =
                    self.world.get(*trigger_id)?.load(FieldAddrFunctionId::Touch)?;

                self.globals.store(GlobalAddrEntity::Self_, *trigger_id)?;
                self.globals.store(GlobalAddrEntity::Other, ent_id)?;
                self.execute_program(trigger_touch, registry.reborrow())?;
            }

            // Restore state.
            self.globals.store(GlobalAddrEntity::Self_, restore_self)?;
            self.globals.store(GlobalAddrEntity::Other, restore_other)?;

            Ok(())
        })?;

        Ok(())
    }

    /// Runs two entities' touch functions.
    pub fn impact_entities(
        &mut self,
        ent_a: EntityId,
        ent_b: EntityId,
        mut registry: Mut<Registry>,
    ) -> progs::Result<()> {
        let restore_self = self.globals.load(GlobalAddrEntity::Self_)?;
        let restore_other = self.globals.load(GlobalAddrEntity::Other)?;

        self.globals.store(GlobalAddrFloat::Time, duration_to_f32(self.time))?;

        // Set up and run Entity A's touch function.
        let touch_a = self.world.get(ent_a)?.load(FieldAddrFunctionId::Touch)?;
        let solid_a = self.world.get(ent_a)?.solid()?;
        if touch_a.0 != 0 && solid_a != EntitySolid::Not {
            self.globals.store(GlobalAddrEntity::Self_, ent_a)?;
            self.globals.store(GlobalAddrEntity::Other, ent_b)?;
            self.globals.store(GlobalAddrEntity::World, EntityId(0))?;
            self.execute_program(touch_a, registry.reborrow())?;
        }

        // Set up and run Entity B's touch function.
        let touch_b = self.world.get(ent_b)?.load(FieldAddrFunctionId::Touch)?;
        let solid_b = self.world.get(ent_b)?.solid()?;
        if touch_b.0 != 0 && solid_b != EntitySolid::Not {
            self.globals.store(GlobalAddrEntity::Self_, ent_b)?;
            self.globals.store(GlobalAddrEntity::Other, ent_a)?;
            self.globals.store(GlobalAddrEntity::World, EntityId(0))?;
            self.execute_program(touch_b, registry.reborrow())?;
        }

        self.globals.store(GlobalAddrEntity::Self_, restore_self)?;
        self.globals.store(GlobalAddrEntity::Other, restore_other)?;
        self.globals.store(GlobalAddrEntity::World, EntityId(0))?;

        Ok(())
    }

    /// Starts a sound on an entity.
    ///
    /// `volume` is in `[0, 1]` and `attenuation` is in `[0, 4)`.
    ///
    /// Equivalent to `sv_startsound`.
    pub fn sound(
        &mut self,
        entity: EntityId,
        channel: i8,
        sound_name: &str,
        volume: f32,
        attenuation: f32,
    ) -> progs::Result<()> {
        let volume = (volume * 255.) as _;

        let Some(sound_id) = self.sound_precache.find(sound_name) else {
            error!("Cannot find sound {sound_name} in precache");
            return Ok(());
        };

        // Use the centre of the entity's bounds, as brush entities (doors, platforms etc.) usually
        // have their origin at the world origin.
        let mut ent = self.world.get(entity)?;
        let position = ent.origin()? + 0.5 * (ent.min()? + ent.max()?);

        ServerCmd::Sound {
            volume: Some(volume),
            attenuation: Some(attenuation),
            entity_id: entity.0 as _,
            channel,
            sound_id: sound_id as _,
            position,
        }
        .serialize(&mut self.broadcast)?;

        Ok(())
    }

    // QuakeC instructions ====================================================

    pub fn op_return(&mut self, a: i16, b: i16, c: i16) -> progs::Result<()> {
        let val1 = self.globals.get_bytes(a)?;
        let val2 = self.globals.get_bytes(b)?;
        let val3 = self.globals.get_bytes(c)?;

        self.globals.put_bytes(val1, GLOBAL_ADDR_RETURN as i16)?;
        self.globals.put_bytes(val2, GLOBAL_ADDR_RETURN as i16 + 1)?;
        self.globals.put_bytes(val3, GLOBAL_ADDR_RETURN as i16 + 2)?;

        debug!(
            "Returning from quakec function {}",
            self.string_table
                .get(self.cx.function_def(self.cx.current_function()).unwrap().name_id)
                .unwrap()
        );

        self.cx.leave_function(&self.string_table, &mut self.globals)?;

        Ok(())
    }

    // LOAD_F: load float field from entity
    pub fn op_load_f(&mut self, e_ofs: i16, e_f: i16, dest_ofs: i16) -> progs::Result<()> {
        let ent_id = self.globals.get_entity_id(e_ofs)?;

        let fld_ofs = self.globals.get_field_addr(e_f)?;

        let f = self.world.get(ent_id)?.get_float(fld_ofs.0 as i16)?;
        if let Some(field) = FieldAddrFloat::from_usize(fld_ofs.0) {
            debug!("{:?}.{:?} = {}", ent_id, field, f);
        }
        self.globals.put_float(f, dest_ofs)?;

        Ok(())
    }

    // LOAD_V: load vector field from entity
    pub fn op_load_v(
        &mut self,
        ent_id_addr: i16,
        ent_vector_addr: i16,
        dest_addr: i16,
    ) -> progs::Result<()> {
        let ent_id = self.globals.get_entity_id(ent_id_addr)?;
        let ent_vector = self.globals.get_field_addr(ent_vector_addr)?;
        let v = self.world.get(ent_id)?.get_vector(ent_vector.0 as i16)?;
        self.globals.put_vector(v, dest_addr)?;

        Ok(())
    }

    pub fn op_load_s(
        &mut self,
        ent_id_addr: i16,
        ent_string_id_addr: i16,
        dest_addr: i16,
    ) -> progs::Result<()> {
        let ent_id = self.globals.get_entity_id(ent_id_addr)?;
        let ent_string_id = self.globals.get_field_addr(ent_string_id_addr)?;
        let s = self.world.get(ent_id)?.string_id(ent_string_id.0 as i16)?;
        self.globals.put_string_id(s, dest_addr)?;

        Ok(())
    }

    pub fn op_load_ent(
        &mut self,
        ent_id_addr: i16,
        ent_entity_id_addr: i16,
        dest_addr: i16,
    ) -> progs::Result<()> {
        let ent_id = self.globals.get_entity_id(ent_id_addr)?;
        let ent_entity_id = self.globals.get_field_addr(ent_entity_id_addr)?;
        let e = self.world.get(ent_id)?.entity_id(ent_entity_id.0 as i16)?;
        self.globals.put_entity_id(e, dest_addr)?;

        Ok(())
    }

    pub fn op_load_fnc(
        &mut self,
        ent_id_addr: i16,
        ent_function_id_addr: i16,
        dest_addr: i16,
    ) -> progs::Result<()> {
        let ent_id = self.globals.get_entity_id(ent_id_addr)?;
        let fnc_function_id = self.globals.get_field_addr(ent_function_id_addr)?;
        let f = self.world.get(ent_id)?.function_id(fnc_function_id.0 as i16)?;
        self.globals.put_function_id(f, dest_addr)?;

        Ok(())
    }

    // LOAD_FLD: load field-offset field from entity
    pub fn op_load_fld(
        &mut self,
        ent_id_addr: i16,
        ent_fld_addr_addr: i16,
        dest_addr: i16,
    ) -> progs::Result<()> {
        let ent_id = self.globals.get_entity_id(ent_id_addr)?;
        let ent_fld_addr = self.globals.get_field_addr(ent_fld_addr_addr)?;
        let fld = self.world.get(ent_id)?.get_int(ent_fld_addr.0 as i16)?;
        let fld = usize::try_from(fld).map_err(|_| {
            ProgsError::with_msg(format!("load_fld: negative field address ({fld})"))
        })?;
        self.globals.put_field_addr(FieldAddr(fld), dest_addr)?;

        Ok(())
    }

    pub fn op_address(
        &mut self,
        ent_id_addr: i16,
        fld_addr_addr: i16,
        dest_addr: i16,
    ) -> progs::Result<()> {
        let ent_id = self.globals.get_entity_id(ent_id_addr)?;
        let fld_addr = self.globals.get_field_addr(fld_addr_addr)?;
        self.globals.put_entity_field(
            self.world
                .ent_fld_addr_to_i32(EntityFieldAddr { entity_id: ent_id, field_addr: fld_addr }),
            dest_addr,
        )?;

        Ok(())
    }

    pub fn op_storep_f(
        &mut self,
        src_float_addr: i16,
        dst_ent_fld_addr: i16,
        unused: i16,
    ) -> progs::Result<()> {
        if unused != 0 {
            return Err(ProgsError::with_msg("storep_f: nonzero arg3"));
        }

        let f = self.globals.get_float(src_float_addr)?;
        let ent_fld_addr =
            self.world.ent_fld_addr_from_i32(self.globals.get_entity_field(dst_ent_fld_addr)?)?;

        self.world
            .get_mut(ent_fld_addr.entity_id)?
            .put_float(f, ent_fld_addr.field_addr.0 as i16)?;

        Ok(())
    }

    pub fn op_storep_v(
        &mut self,
        src_vector_addr: i16,
        dst_ent_fld_addr: i16,
        unused: i16,
    ) -> progs::Result<()> {
        if unused != 0 {
            return Err(ProgsError::with_msg("storep_v: nonzero arg3"));
        }

        let v = self.globals.get_vector(src_vector_addr)?;
        let ent_fld_addr =
            self.world.ent_fld_addr_from_i32(self.globals.get_entity_field(dst_ent_fld_addr)?)?;
        self.world
            .get_mut(ent_fld_addr.entity_id)?
            .put_vector(v, ent_fld_addr.field_addr.0 as i16)?;

        Ok(())
    }

    pub fn op_storep_s(
        &mut self,
        src_string_id_addr: i16,
        dst_ent_fld_addr: i16,
        unused: i16,
    ) -> progs::Result<()> {
        if unused != 0 {
            return Err(ProgsError::with_msg("storep_s: nonzero arg3"));
        }

        let s = self.globals.string_id(src_string_id_addr)?;
        let ent_fld_addr =
            self.world.ent_fld_addr_from_i32(self.globals.get_entity_field(dst_ent_fld_addr)?)?;
        self.world
            .get_mut(ent_fld_addr.entity_id)?
            .put_string_id(s, ent_fld_addr.field_addr.0 as i16)?;

        Ok(())
    }

    pub fn op_storep_ent(
        &mut self,
        src_entity_id_addr: i16,
        dst_ent_fld_addr: i16,
        unused: i16,
    ) -> progs::Result<()> {
        if unused != 0 {
            return Err(ProgsError::with_msg("storep_ent: nonzero arg3"));
        }

        let e = self.globals.get_entity_id(src_entity_id_addr)?;
        let ent_fld_addr =
            self.world.ent_fld_addr_from_i32(self.globals.get_entity_field(dst_ent_fld_addr)?)?;
        self.world
            .get_mut(ent_fld_addr.entity_id)?
            .put_entity_id(e, ent_fld_addr.field_addr.0 as i16)?;

        Ok(())
    }

    pub fn op_storep_fnc(
        &mut self,
        src_function_id_addr: i16,
        dst_ent_fld_addr: i16,
        unused: i16,
    ) -> progs::Result<()> {
        if unused != 0 {
            return Err(ProgsError::with_msg(format!("storep_fnc: nonzero arg3 ({unused})")));
        }

        let f = self.globals.get_function_id(src_function_id_addr)?;
        let ent_fld_addr =
            self.world.ent_fld_addr_from_i32(self.globals.get_entity_field(dst_ent_fld_addr)?)?;
        self.world
            .get_mut(ent_fld_addr.entity_id)?
            .put_function_id(f, ent_fld_addr.field_addr.0 as i16)?;

        Ok(())
    }

    pub fn op_storep_fld(
        &mut self,
        src_field_addr_addr: i16,
        dst_ent_fld_addr: i16,
        unused: i16,
    ) -> progs::Result<()> {
        if unused != 0 {
            return Err(ProgsError::with_msg(format!("storep_fld: nonzero arg3 ({unused})")));
        }

        let fld = self.globals.get_field_addr(src_field_addr_addr)?;
        let fld = i32::try_from(fld.0)
            .map_err(|_| ProgsError::with_msg("storep_fld: field address out of range"))?;
        let ent_fld_addr =
            self.world.ent_fld_addr_from_i32(self.globals.get_entity_field(dst_ent_fld_addr)?)?;
        self.world
            .get_mut(ent_fld_addr.entity_id)?
            .put_int(fld, ent_fld_addr.field_addr.0 as i16)?;

        Ok(())
    }

    pub fn op_state(
        &mut self,
        frame_id_addr: i16,
        think_function_addr: i16,
        unused_c: i16,
    ) -> progs::Result<()> {
        if unused_c != 0 {
            return Err(ProgsError::with_msg(format!("state: nonzero arg3 ({unused_c})")));
        }

        let self_id = self.globals.get_entity_id(GlobalAddrEntity::Self_ as i16)?;
        let mut self_ent = self.world.get_mut(self_id)?;
        let next_think_time = self.globals.get_float(GlobalAddrFloat::Time as i16)? + 0.1;

        self_ent.put_float(next_think_time, FieldAddrFloat::NextThink as i16)?;

        let frame_id = self.globals.get_float(frame_id_addr)?;
        self_ent.put_float(frame_id, FieldAddrFloat::FrameId as i16)?;

        let think_func = self.globals.get_function_id(think_function_addr)?;
        self_ent.put_function_id(think_func, FieldAddrFunctionId::Think as _)?;

        Ok(())
    }

    // QuakeC built-in functions ==============================================

    /// Prints a QuakeC error, along with the fields of `self`, and returns it as an error.
    fn qc_error(&mut self, err_kind: impl fmt::Display, argc: usize) -> progs::Result<ProgsError> {
        let msg = self.var_string(0, argc)?;
        let function_name = self
            .cx
            .function_def(self.cx.current_function())
            .ok()
            .and_then(|def| self.string_table.get(def.name_id))
            .map(|name| name.into_owned())
            .unwrap_or_default();

        self.cx.print_backtrace(&self.string_table, false);

        let self_id = self.globals.load(GlobalAddrEntity::Self_)?;
        if let Ok(ent) = self.world.get(self_id) {
            error!("{self_id}: {}", ent.display(&self.string_table));
        }

        Ok(ProgsError::with_msg(format!("{err_kind} in {function_name}: {msg}")))
    }

    /// QuakeC `error`, as in Quake's `PF_error`.
    pub fn builtin_error(&mut self, argc: usize) -> progs::Result<()> {
        Err(self.qc_error("Error", argc)?)
    }

    /// QuakeC `objerror`, as in Quake's `PF_objerror`. Unlike `error`, this also removes `self`.
    pub fn builtin_obj_error(&mut self, argc: usize) -> progs::Result<()> {
        let err = self.qc_error("Object error", argc)?;

        let self_id = self.globals.load(GlobalAddrEntity::Self_)?;
        if !self_id.is_none() {
            self.world.free(self_id)?;
        }

        Err(err)
    }

    pub fn builtin_set_origin(&mut self, registry: Mut<Registry>) -> progs::Result<()> {
        let e_id = self.globals.get_entity_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let origin = self.globals.get_vector(GLOBAL_ADDR_ARG_1 as i16)?;
        self.set_entity_origin(e_id, origin, registry)?;

        Ok(())
    }

    pub fn builtin_set_model(&mut self, registry: Mut<Registry>) -> progs::Result<()> {
        let ent_id = self.globals.get_entity_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let model_name_id = self.globals.string_id(GLOBAL_ADDR_ARG_1 as i16)?;
        self.set_entity_model(ent_id, model_name_id, registry)?;

        Ok(())
    }

    pub fn builtin_set_size(&mut self) -> progs::Result<()> {
        let e_id = self.globals.get_entity_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let mins = self.globals.get_vector(GLOBAL_ADDR_ARG_1 as i16)?;
        let maxs = self.globals.get_vector(GLOBAL_ADDR_ARG_2 as i16)?;
        self.world.set_entity_size(e_id, mins, maxs)?;

        Ok(())
    }

    pub fn builtin_spawn(&mut self, registry: Mut<Registry>) -> progs::Result<()> {
        let ent_id = self.spawn_entity(registry)?;
        self.globals.put_entity_id(ent_id, GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    pub fn builtin_remove(&mut self) -> progs::Result<()> {
        let ent_id = self.globals.get_entity_id(GLOBAL_ADDR_ARG_0 as i16)?;
        self.world.free(ent_id)?;

        Ok(())
    }

    pub fn builtin_precache_sound(&mut self) -> progs::Result<()> {
        let s_id = self.globals.string_id(GLOBAL_ADDR_ARG_0 as i16)?;
        self.precache_sound(s_id)?;
        self.globals.put_string_id(s_id, GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    pub fn builtin_precache_model(&mut self) -> progs::Result<()> {
        // TODO: disable precaching after server is active
        let s_id = self.globals.string_id(GLOBAL_ADDR_ARG_0 as i16)?;
        if self.model_id(s_id).is_none() {
            self.precache_model(s_id)?;
        }

        self.globals.put_string_id(s_id, GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    pub fn builtin_stuffcmd(&mut self) -> progs::Result<()> {
        let ent_id = self.globals.get_entity_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let s_id = self.globals.string_id(GLOBAL_ADDR_ARG_1 as i16)?;
        let text = self.string_table.get(s_id).unwrap().into_owned();

        let Some(dest) = self.client_message(ent_id) else {
            return Err(ProgsError::with_msg(format!("stuffcmd: {ent_id} is not a client")));
        };

        ServerCmd::StuffText { text }.serialize(dest)?;

        Ok(())
    }

    pub fn builtin_find_radius(&mut self) -> progs::Result<()> {
        let find_origin = self.globals.get_vector(GLOBAL_ADDR_ARG_0 as i16)?;
        let find_radius = self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)?;
        let find_radius_sqr = find_radius.powi(2);

        let mut chain_end = EntityId::NONE;

        // TODO: Collect is unnecessary here, we can do better.
        for ent_id in self.world.iter().filter(|e_id| !e_id.is_none()).collect::<Vec<_>>() {
            let mut ent = self.world.get_mut(ent_id)?;

            if ent.solid()? == EntitySolid::Not {
                continue;
            }

            let ent_center = ent.origin()? + (ent.min()? + ent.max()?) / 2.;
            let diff = find_origin - ent_center;
            if diff.length_squared() > find_radius_sqr {
                continue;
            }

            ent.set_chain(chain_end)?;
            chain_end = ent_id;
        }

        self.globals.put_entity_id(chain_end, GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    pub fn builtin_bprint(&mut self) -> progs::Result<()> {
        let strs = &self.string_table;
        let s_id = self.globals.string_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let string = strs.get(s_id).unwrap();
        debug!("BPRINT: {string}");

        // TODO: Broadcast to all clients

        Ok(())
    }

    /// Sends a message to a single client.
    ///
    /// Equivalent to `PF_sprint`.
    pub fn builtin_sprint(&mut self, argc: usize) -> progs::Result<()> {
        let ent_id = self.globals.get_entity_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let text = self.var_string(1, argc)?;

        let Some(dest) = self.client_message(ent_id) else {
            warn!("tried to sprint to a non-client ({ent_id})");
            return Ok(());
        };

        ServerCmd::Print { text }.serialize(dest)?;

        Ok(())
    }

    pub fn builtin_dprint(&mut self, argc: usize) -> progs::Result<()> {
        let string = self.var_string(0, argc)?;
        debug!("DPRINT: {string}");

        Ok(())
    }

    pub fn builtin_ftos(&mut self) -> progs::Result<()> {
        let f = self.globals.get_float(GLOBAL_ADDR_ARG_0 as i16)?;
        // TODO: This leaks data in the string table! We should have a separate temp string area.
        let out = self.string_table.find_or_insert(ftos(f));

        self.globals.put_string_id(out, GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    pub fn builtin_vtos(&mut self) -> progs::Result<()> {
        let vec = self.globals.get_vector(GLOBAL_ADDR_ARG_0 as i16)?;
        // TODO: This leaks data in the string table! We should have a separate temp string area.
        let out = self.string_table.find_or_insert(vtos(vec));

        self.globals.put_string_id(out, GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    pub fn builtin_drop_to_floor(&mut self, registry: Mut<Registry>) -> progs::Result<()> {
        let ent_id = self.globals.get_entity_id(GlobalAddrEntity::Self_ as i16)?;
        let hit_floor = self.drop_entity_to_floor(ent_id, registry)?;
        self.globals.put_float(hit_floor as u8 as f32, GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    pub fn builtin_light_style(&mut self) -> progs::Result<()> {
        let index = match self.globals.get_float(GLOBAL_ADDR_ARG_0 as i16)? as i32 {
            i if i < 0 => return Err(ProgsError::with_msg("negative lightstyle ID")),
            i => i as usize,
        };
        if index >= MAX_LIGHTSTYLES {
            return Err(ProgsError::with_msg(format!("lightstyle {index} out of range")));
        }

        let val = self.globals.string_id(GLOBAL_ADDR_ARG_1 as i16)?;
        self.set_lightstyle(index, val);

        // Clients are sent all of the light styles when they connect, so this only needs to send
        // changes. Anything written before the level is active is cleared by `init_frame`, which
        // matches Quake only sending changes once the server is active.
        // TODO: This should be reliable (Quake sends it to each client's reliable message).
        let value = self.string_table.get(val).unwrap_or_default().into_owned();
        ServerCmd::LightStyle { id: index as _, value }.serialize(&mut self.broadcast)?;

        Ok(())
    }

    pub fn builtin_check_bottom(&mut self, registry: &Registry) -> progs::Result<()> {
        let server_vars = registry.read_cvars::<ServerVars>()?;
        let ent_id = self.globals.get_entity_id(GLOBAL_ADDR_ARG_0 as _)?;

        self.globals.put_float(
            if self.check_bottom(ent_id, &server_vars)? { 1. } else { 0. },
            GLOBAL_ADDR_RETURN as _,
        )?;
        Ok(())
    }

    pub fn builtin_point_contents(&mut self) -> progs::Result<()> {
        let pos = self.globals.get_vector(GLOBAL_ADDR_ARG_0 as _)?;

        let contents = self.point_contents(pos);
        self.globals.put_float(quake_contents(contents), GLOBAL_ADDR_RETURN as _)?;

        Ok(())
    }

    pub fn builtin_aim(&mut self) -> progs::Result<()> {
        // TODO: This always just emulates the `noaim` client option (or `sv_aim 1`), we should
        // implement aim properly. Without autoaim, Quake returns `v_forward`, which QuakeC sets
        // with `makevectors(self.v_angle)` before calling `aim`.
        let forward = self.globals.load(GlobalAddrVector::VForward)?;
        self.globals.put_vector(forward, GLOBAL_ADDR_RETURN as _)?;

        Ok(())
    }

    pub fn builtin_cvar(&mut self, registry: &Registry) -> progs::Result<()> {
        let s_id = self.globals.string_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let strs = &self.string_table;
        let s = strs.get(s_id).unwrap();
        let f = match registry.read_cvar(s.to_str()) {
            Ok(f) => f,
            Err(e) => {
                error!("{}", e);
                default()
            }
        };

        debug!("{} = {}", s, f);
        self.globals.put_float(f, GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    fn change_yaw(&mut self, ent_id: EntityId) -> progs::Result<()> {
        let mut ent = self.world.get_mut(ent_id)?;

        let cur = angle_mod(ent.load(FieldAddrFloat::AnglesY)?);
        let ideal = ent.ideal_yaw()?;
        let speed = ent.yaw_speed()?;

        if (cur - ideal).abs() < f32::EPSILON {
            return Ok(());
        }

        // TODO: This is just copied directly from the Quake source, but can definitely be
        //       done more efficiently.
        let angle_delta = ideal - cur;

        let angle_delta = if ideal > cur {
            if angle_delta >= 180. { angle_delta - 360. } else { angle_delta }
        } else if angle_delta <= -180. {
            angle_delta + 360.
        } else {
            angle_delta
        };

        // Unlike `f32::clamp`, this doesn't panic if `speed` is negative or NaN.
        let angle_delta = if angle_delta > 0. {
            if angle_delta > speed { speed } else { angle_delta }
        } else if angle_delta < -speed {
            -speed
        } else {
            angle_delta
        };

        ent.store(FieldAddrFloat::AnglesY, angle_mod(cur + angle_delta))?;

        Ok(())
    }

    pub fn builtin_particle(&mut self) -> progs::Result<()> {
        let origin = self.globals.get_vector(GLOBAL_ADDR_ARG_0 as _)?;
        let direction = self.globals.get_vector(GLOBAL_ADDR_ARG_1 as _)?;
        let color = self.globals.get_float(GLOBAL_ADDR_ARG_2 as _)? as _;
        let count = self.globals.get_float(GLOBAL_ADDR_ARG_3 as _)? as _;
        ServerCmd::Particle { origin, direction, color, count }.serialize(&mut self.broadcast)?;

        Ok(())
    }

    pub fn builtin_change_yaw(&mut self) -> progs::Result<()> {
        self.change_yaw(self.globals.load(GlobalAddrEntity::Self_)?)
    }

    pub fn builtin_cvar_set(&mut self, mut registry: Mut<Registry>) -> progs::Result<()> {
        let strs = &self.string_table;

        let var_id = self.globals.string_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let var = strs.get(var_id).unwrap();
        let val_id = self.globals.string_id(GLOBAL_ADDR_ARG_1 as i16)?;
        let val = strs.get(val_id).unwrap();

        if let Err(e) = registry.set_cvar(var.to_str(), val.to_str()) {
            warn!("cvar_set: failed to set {var}: {e}");
            return Ok(());
        }
        debug!("{} = {}", var, val);

        Ok(())
    }

    pub fn builtin_ambient_sound(&mut self) -> progs::Result<()> {
        let origin = self.globals.get_vector(GLOBAL_ADDR_ARG_0 as i16)?;
        let sample = self.globals.string_id(GLOBAL_ADDR_ARG_1 as i16)?;
        let volume = (self.globals.get_float(GLOBAL_ADDR_ARG_2 as i16)? * 255.) as _;
        let attenuation = (self.globals.get_float(GLOBAL_ADDR_ARG_3 as i16)? * 64.) as _;

        let Some(sound_id) = self.sound_id(sample) else {
            error!("Cannot find sound {} in precache", self.string_table.get(sample).unwrap());
            return Ok(());
        };

        // Static sounds are part of the sign-on message, so clients get them when they connect.
        ServerCmd::StaticSound { origin, sound_id: sound_id as _, volume, attenuation }
            .serialize(&mut self.init)?;

        Ok(())
    }

    pub fn builtin_find(&mut self) -> progs::Result<()> {
        let entity = self.globals.get_entity_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let field = self.globals.get_field_addr(GLOBAL_ADDR_ARG_1 as i16)?;
        let match_str = self.globals.string_id(GLOBAL_ADDR_ARG_2 as i16)?;

        let Some(match_str) = self.string_table.get(match_str) else {
            return Err(ProgsError::with_msg("Failed to find match string".to_owned()));
        };

        debug!("Finding {match_str} for field {field:?}");

        let field = i16::try_from(field.0)
            .map_err(|_| ProgsError::with_msg(format!("find: invalid field {field:?}")))?;

        for ent in self.world.range((Bound::Excluded(entity.0.max(0) as usize), Bound::Unbounded)) {
            let Ok(s_id) = self.world.get(ent)?.string_id(field) else {
                debug!("No string field {} on {:?}", field, ent);
                continue;
            };

            let Some(s) = self.string_table.get(s_id) else {
                continue;
            };

            if s == match_str {
                self.globals.put_entity_id(ent, GLOBAL_ADDR_RETURN as i16)?;
                return Ok(());
            }
        }

        self.globals.put_entity_id(EntityId(0), GLOBAL_ADDR_RETURN as i16)?;
        Ok(())
    }

    pub fn builtin_walk_move(&mut self, registry: Mut<Registry>) -> progs::Result<()> {
        let yaw = self.globals.get_float(GLOBAL_ADDR_ARG_0 as i16)?;
        let dist = self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)?;

        let this_id = self.globals.load(GlobalAddrEntity::Self_)?;
        let mut this = self.world.get(this_id)?;
        let ent_flags = this.flags()?;

        if !ent_flags.intersects(EntityFlags::ON_GROUND | EntityFlags::SWIM | EntityFlags::FLY) {
            self.globals.put_float(0., GLOBAL_ADDR_RETURN as i16)?;
            return Ok(());
        }

        let move_dir = Vec2::from_angle(yaw.to_radians()).extend(0.) * dist;

        let func = self.cx.current_function();

        let did_move = self.move_step(this_id, move_dir, true, registry)?;
        self.globals.put_float(if did_move { 1. } else { 0. }, GLOBAL_ADDR_RETURN as i16)?;

        // Restore after `move_step`.
        self.cx.set_current_function(func);
        self.globals.store(GlobalAddrEntity::Self_, this_id)?;
        self.globals.store(GlobalAddrEntity::World, EntityId(0))?;

        Ok(())
    }

    /// Picks the next client for `checkclient` to return.
    ///
    /// Cycles through the clients after `check`, skipping any which are dead or have `notarget`.
    /// If no other client is suitable, returns `check`.
    ///
    /// Equivalent to `PF_newcheckclient`.
    fn new_check_client(&mut self, check: usize) -> progs::Result<usize> {
        let max_clients = self.max_clients.max(1);
        let check = check.clamp(1, max_clients);

        let mut i = check;
        loop {
            i = if i == max_clients { 1 } else { i + 1 };

            if i == check {
                // Didn't find anything else.
                break;
            }

            let ent_id = EntityId(i as i32);
            if !self.world.exists(ent_id) {
                continue;
            }

            let mut ent = self.world.get(ent_id)?;
            if ent.health()? <= 0. || ent.has_flag(EntityFlags::NO_TARGET)? {
                continue;
            }

            break;
        }

        Ok(i)
    }

    /// Returns a client that `self` might be able to see, or world.
    ///
    /// Monsters use this to look for a player to attack. It only checks one client every 0.1s.
    ///
    /// Equivalent to `PF_checkclient`
    pub fn builtin_check_client(&mut self) -> progs::Result<()> {
        const CHECK_INTERVAL: Duration = Duration::from_millis(100);

        if self.time.saturating_sub(self.last_check_time) >= CHECK_INTERVAL {
            self.last_check_client = self.new_check_client(self.last_check_client)?;
            self.last_check_time = self.time;
        }

        let check_id = EntityId(self.last_check_client as i32);

        let visible = self.world.exists(check_id) && self.world.get(check_id)?.health()? > 0.;
        // TODO: Quake also returns world if `self` can't be in the client's PVS, but we don't
        // have PVS data on the server. QuakeC checks visibility itself before attacking.

        self.globals.put_entity_id(
            if visible { check_id } else { EntityId(0) },
            GLOBAL_ADDR_RETURN as i16,
        )?;

        Ok(())
    }

    pub fn builtin_make_static(&mut self) -> progs::Result<()> {
        let ent = self.globals.get_entity_id(GLOBAL_ADDR_ARG_0 as i16)?;

        let Ok(mut entity) = self.world.get(ent) else {
            error!("Tried to call `make_static` on a non-existant entity");
            return Ok(());
        };

        let (model_name, frame_id, colormap, skin_id, origin, angles) = (
            entity.string_id(FieldAddrStringId::ModelName as i16)?,
            entity.get_float(FieldAddrFloat::FrameId as i16)? as _,
            entity.get_float(FieldAddrFloat::Colormap as i16)? as _,
            entity.get_float(FieldAddrFloat::SkinId as i16)? as _,
            entity.get_vector(FieldAddrVector::Origin as i16)?,
            entity.get_vector(FieldAddrVector::Angles as i16)?,
        );

        // Even though there is a `ModelId` field, it seems like quake searches for the model in the
        // precache manually
        let model_id = self.model_index(model_name)? as _;

        // Static entities are part of the sign-on message, so clients get them when they connect.
        ServerCmd::SpawnStatic { model_id, frame_id, colormap, skin_id, origin, angles }
            .serialize(&mut self.init)?;

        self.world.free(ent)?;

        Ok(())
    }

    pub fn builtin_sound(&mut self) -> progs::Result<()> {
        let entity = self.globals.get_entity_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let channel = self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)? as _;
        let sample = self.globals.string_id(GLOBAL_ADDR_ARG_2 as i16)?;
        let volume = self.globals.get_float(GLOBAL_ADDR_ARG_3 as i16)?;
        let attenuation = self.globals.get_float(GLOBAL_ADDR_ARG_4 as i16)?;

        let Some(sample_name) = self.string(sample).map(|s| s.to_str().into_owned()) else {
            return Err(ProgsError::with_msg(format!("invalid sound: {sample}")));
        };

        debug!(
            "Playing sound on {entity}.{channel}: {sample_name} vol {volume} attn {attenuation}"
        );

        self.sound(entity, channel, &sample_name, volume, attenuation)?;

        Ok(())
    }

    pub fn builtin_write_byte(&mut self) -> progs::Result<()> {
        let val = self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)? as i32 as u8;
        let mut dest = self.write_dest()?;
        dest.write_u8(val)?;

        Ok(())
    }

    pub fn builtin_write_char(&mut self) -> progs::Result<()> {
        let val = self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)? as i32 as i8;
        let mut dest = self.write_dest()?;
        dest.write_i8(val)?;
        Ok(())
    }

    pub fn builtin_write_short(&mut self) -> progs::Result<()> {
        let val = self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)? as i32 as i16;
        let mut dest = self.write_dest()?;
        dest.write_i16::<LittleEndian>(val)?;
        Ok(())
    }

    pub fn builtin_write_long(&mut self) -> progs::Result<()> {
        let val = self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)? as i32;
        let mut dest = self.write_dest()?;
        dest.write_i32::<LittleEndian>(val)?;
        Ok(())
    }

    pub fn builtin_write_coord(&mut self) -> progs::Result<()> {
        let val = (self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)? * 8.) as i32 as i16;
        let mut dest = self.write_dest()?;
        dest.write_i16::<LittleEndian>(val)?;
        Ok(())
    }

    pub fn builtin_write_angle(&mut self) -> progs::Result<()> {
        let val = write_angle_byte(self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)?);
        let mut dest = self.write_dest()?;
        dest.write_u8(val)?;
        Ok(())
    }

    pub fn builtin_write_string(&mut self) -> progs::Result<()> {
        let val = self.globals.string_id(GLOBAL_ADDR_ARG_1 as i16)?;
        // TODO: `into_owned` not necessary here, `broadcast`/`frame_client_messages` should be
        // extracted into a single field.
        let string = self.string_table.get(val).unwrap_or_default().into_owned();
        let mut dest = self.write_dest()?;
        dest.write_all(&string.raw)?;
        dest.write_u8(0)?;
        Ok(())
    }

    pub fn builtin_write_entity(&mut self) -> progs::Result<()> {
        let val = self.globals.get_entity_id(GLOBAL_ADDR_ARG_1 as i16)?;
        let mut dest = self.write_dest()?;
        dest.write_u16::<LittleEndian>(val.0 as u16)?;
        Ok(())
    }

    fn close_enough(&self, a: EntityId, b: EntityId, dist: f32) -> progs::Result<bool> {
        let a = self.world.get(a)?.abs_bounding_box()?;
        let b = self.world.get(b)?.abs_bounding_box()?;

        Ok(a.grow(Vec3A::splat(dist)).intersects(&b))
    }

    fn check_bottom(&self, ent_id: EntityId, server_vars: &ServerVars) -> progs::Result<bool> {
        let mut ent = self.world.get(ent_id)?;

        let origin = ent.origin()?;
        let min = origin + ent.min()?;
        let max = origin + ent.max()?;

        let start_z = min.z - 1.;
        let mut to_check = [max.x, min.x]
            .into_iter()
            .cartesian_product([max.y, min.y])
            .map(|(x, y)| Vec3::new(x, y, start_z));

        let all_solid =
            to_check.all(|start| self.world.contents_at_point(start) == BspLeafContents::Solid);

        if all_solid {
            return Ok(true);
        }

        let start_z = min.z;

        let start = Vec3::new((min.x + max.x) / 2., (min.y + max.y) / 2., start_z);
        let stop = start - Vec3::Z * server_vars.max_step * 2.;

        let (trace, _) = self.world.trace_entity_move(
            ent_id,
            start,
            Vec3::ZERO,
            Vec3::ZERO,
            stop,
            CollideKind::NoMonsters,
            |_| true,
        )?;

        if trace.is_terminal() {
            return Ok(false);
        }

        let mid = trace.end_point().z;
        let mut bottom = mid;

        let mut to_check = [max.x, min.x]
            .into_iter()
            .cartesian_product([max.y, min.y])
            .map(|(x, y)| Vec3::new(x, y, start_z));

        Ok(to_check.all(|start| {
            let Ok((trace, _)) = self.world.trace_entity_move(
                ent_id,
                start,
                Vec3::ZERO,
                Vec3::ZERO,
                start - Vec3::Z * server_vars.max_step * 2.,
                CollideKind::NoMonsters,
                |_| true,
            ) else {
                return false;
            };

            if !trace.is_terminal() {
                bottom = bottom.max(trace.end_point().z);

                mid - trace.end_point().z <= server_vars.max_step
            } else {
                false
            }
        }))
    }

    fn move_step_fly(
        &mut self,
        ent_id: EntityId,
        move_dir: Vec3,
        relink: bool,
        registry: Mut<Registry>,
    ) -> progs::Result<bool> {
        let mut ent = self.world.get(ent_id)?;
        let ent_flags = ent.flags()?;

        let min = ent.min()?;
        let max = ent.max()?;

        // Try move with/without vertical motion
        for i in 0..2 {
            let mut ent = self.world.get(ent_id)?;
            let old_origin = ent.origin()?;
            let mut new_origin = old_origin + move_dir;

            let enemy_id = ent.enemy()?;

            if i == 0 && !enemy_id.is_none() {
                // Try to stay 30-40 units above the enemy.
                match old_origin.z - self.world.get(enemy_id)?.origin()?.z {
                    40f32.. => new_origin.z -= 8.,
                    ..30f32 => new_origin.z += 8.,
                    _ => {}
                }
            }

            let (trace, _) = self.world.trace_entity_move(
                ent_id,
                old_origin,
                min,
                max,
                new_origin,
                CollideKind::default(),
                |_| true,
            )?;

            if trace.is_terminal() {
                if ent_flags.contains(EntityFlags::SWIM)
                    && self.point_contents(trace.end_point()) == BspLeafContents::Empty
                {
                    // Swimming monster left water
                    return Ok(false);
                }

                debug!("{} -> {}", old_origin, trace.end_point());
                self.world.get_mut(ent_id)?.set_origin(trace.end_point())?;

                if relink {
                    self.link_entity(ent_id, true, registry)?;
                }

                return Ok(true);
            }

            // TODO: This can be cleaned up
            if enemy_id.is_none() {
                break;
            }
        }

        Ok(false)
    }

    fn move_step(
        &mut self,
        ent_id: EntityId,
        move_dir: Vec3,
        relink: bool,
        registry: Mut<Registry>,
    ) -> progs::Result<bool> {
        let server_vars = registry.read_cvars::<ServerVars>()?;

        let mut ent = self.world.get(ent_id)?;
        let ent_flags = ent.flags()?;

        let min = ent.min()?;
        let max = ent.max()?;
        let old_origin = ent.origin()?;

        if ent_flags.intersects(EntityFlags::SWIM | EntityFlags::FLY) {
            return self.move_step_fly(ent_id, move_dir, relink, registry);
        }

        let new_origin = old_origin + move_dir;

        let step = Vec3::Z * server_vars.max_step;

        let end = new_origin - step;

        let (mut trace, mut ground_ent) = self.world.trace_entity_move(
            ent_id,
            new_origin + step,
            min,
            max,
            end,
            CollideKind::default(),
            |_| true,
        )?;

        if trace.all_solid() {
            debug!("Step trace failed: {ground_ent:?}");
            return Ok(false);
        } else if trace.start_solid() {
            let (temp_trace, temp_ground_ent) = self.world.trace_entity_move(
                ent_id,
                new_origin,
                min,
                max,
                end,
                CollideKind::default(),
                |_| true,
            )?;

            if temp_trace.all_solid() || temp_trace.start_solid() {
                debug!("Non-step trace failed");
                return Ok(false);
            }

            trace = temp_trace;
            ground_ent = temp_ground_ent;
        } else if trace.is_terminal() {
            // The entity had the ground move out from beneath it.
            return Ok(if ent_flags.contains(EntityFlags::PARTIAL_GROUND) {
                let mut ent = self.world.get_mut(ent_id)?;
                debug!("{} -> {}", old_origin, new_origin);
                ent.set_origin(new_origin)?;

                ent.remove_flags(EntityFlags::ON_GROUND)?;

                if relink {
                    self.link_entity(ent_id, true, registry)?;
                }

                debug!("Entity had ground moved out from underneath");
                true
            } else {
                debug!("Entity walked off edge(?)");
                false
            });
        }

        let mut ent = self.world.get_mut(ent_id)?;
        debug!("{} -> {}", old_origin, trace.end_point());
        ent.set_origin(trace.end_point())?;

        if !self.check_bottom(ent_id, &server_vars)? {
            let mut ent = self.world.get_mut(ent_id)?;
            return Ok(if ent.has_flag(EntityFlags::PARTIAL_GROUND)? {
                if relink {
                    self.link_entity(ent_id, true, registry)?;
                }

                true
            } else {
                debug!("Resetting pos bc entity walked off edge(?)");
                ent.set_origin(old_origin)?;
                false
            });
        }

        let mut ent = self.world.get_mut(ent_id)?;
        ent.remove_flags(EntityFlags::PARTIAL_GROUND)?;

        ent.set_ground(ground_ent.unwrap_or(EntityId(-1)))?;

        if relink {
            self.link_entity(ent_id, true, registry)?;
        }

        Ok(true)
    }

    fn step_direction(
        &mut self,
        ent_id: EntityId,
        yaw: f32,
        dist: f32,
        mut registry: Mut<Registry>,
    ) -> progs::Result<bool> {
        let mut ent = self.world.get_mut(ent_id)?;
        let old_origin = ent.origin()?;
        ent.set_ideal_yaw(yaw)?;
        self.change_yaw(ent_id)?;

        let move_dir = Vec2::from_angle(yaw.to_radians()).extend(0.) * dist;
        let did_move = self.move_step(ent_id, move_dir, false, registry.reborrow())?;

        if did_move {
            let mut ent = self.world.get_mut(ent_id)?;
            // TODO: Quake doesn't do `anglemod` here, is it necessary?
            let delta = ent.angles()?.y - ent.ideal_yaw()?;

            if (Bound::Excluded(45f32), Bound::Excluded(315f32)).contains(&delta) {
                debug!("Cancelling move due to angle delta");
                // Quake says that this is when the monster "didn't turn far enough" but it appears
                // to be the opposite - cancelling if it turned _too_ far.
                ent.set_origin(old_origin)?;
            }
        }

        self.link_entity(ent_id, true, registry)?;

        Ok(did_move)
    }

    fn new_chase_dir(
        &mut self,
        this_id: EntityId,
        goal_id: EntityId,
        dist: f32,
        mut registry: Mut<Registry>,
    ) -> progs::Result<()> {
        let server_vars: ServerVars = registry.read_cvars()?;

        let mut this = self.world.get(this_id)?;
        let old_dir = angle_mod((this.ideal_yaw()? / 45.).trunc() * 45.);
        let turn_around = angle_mod(old_dir - 180.);

        let this_origin = this.origin()?;

        let mut goal = self.world.get(goal_id)?;

        let goal_origin = goal.origin()?;

        let delta_x = goal_origin.x - this_origin.x;
        let delta_y = goal_origin.y - this_origin.y;

        let ChaseDirs { x: dir_a, y: dir_b, diagonal: turn_dir } = chase_dirs(delta_x, delta_y);

        // Try the direct route.
        if let Some(turn_dir) = turn_dir
            && (turn_dir - turn_around).abs() > f32::EPSILON
            && self.step_direction(this_id, turn_dir, dist, registry.reborrow())?
        {
            return Ok(());
        }

        // Try the other directions. Quake compares the deltas as integers rather than floats.
        let (dir_a, dir_b) =
            if self.rng.random_bool(0.5) || (delta_y as i32).abs() > (delta_x as i32).abs() {
                (dir_b, dir_a)
            } else {
                (dir_a, dir_b)
            };

        if let Some(dir_a) = dir_a
            && (dir_a - turn_around).abs() > f32::EPSILON
            && self.step_direction(this_id, dir_a, dist, registry.reborrow())?
        {
            return Ok(());
        }

        if let Some(dir_b) = dir_b
            && (dir_b - turn_around).abs() > f32::EPSILON
            && self.step_direction(this_id, dir_b, dist, registry.reborrow())?
        {
            return Ok(());
        }

        // There is no direct path to the goal, so try the old direction.
        if self.step_direction(this_id, old_dir, dist, registry.reborrow())? {
            return Ok(());
        }

        const SEARCH_DIRS: [f32; 8] = [0., 45., 90., 135., 180., 225., 270., 315.];
        const SEARCH_DIRS_REV: [f32; 8] = [315., 270., 225., 180., 135., 90., 45., 0.];

        let search_dirs = if self.rng.random_bool(0.5) { SEARCH_DIRS } else { SEARCH_DIRS_REV };

        for turn_dir in search_dirs {
            if (turn_dir - turn_around).abs() > f32::EPSILON
                && self.step_direction(this_id, turn_dir, dist, registry.reborrow())?
            {
                return Ok(());
            }
        }

        if self.step_direction(this_id, turn_around, dist, registry.reborrow())? {
            return Ok(());
        }

        self.world.get_mut(this_id)?.set_ideal_yaw(old_dir)?;

        if !self.check_bottom(this_id, &server_vars)? {
            self.world.get_mut(this_id)?.add_flags(EntityFlags::PARTIAL_GROUND)?;
        }

        Ok(())
    }

    pub fn builtin_move_to_goal(&mut self, mut registry: Mut<Registry>) -> progs::Result<()> {
        let dist = self.globals.get_float(GLOBAL_ADDR_ARG_0 as i16)?;
        let this_id = self.globals.load(GlobalAddrEntity::Self_)?;
        let mut this = self.world.get(this_id)?;
        let goal_id = this.goal()?;

        let flags = this.flags()?;

        if !flags.intersects(EntityFlags::ON_GROUND | EntityFlags::FLY | EntityFlags::SWIM) {
            self.globals.put_float(0., GLOBAL_ADDR_RETURN as i16)?;
            return Ok(());
        }

        // If the next step hits the enemy, return immediately.
        if !this.enemy()?.is_none() && self.close_enough(this_id, goal_id, dist)? {
            self.globals.put_float(1., GLOBAL_ADDR_RETURN as i16)?;
            return Ok(());
        }

        // Try a few random directions to try to get unstuck.
        let ideal_yaw = this.ideal_yaw()?;
        let did_move = !self.rng.random_ratio(1, 4)
            && self.step_direction(this_id, ideal_yaw, dist, registry.reborrow())?;

        if !did_move {
            self.new_chase_dir(this_id, goal_id, dist, registry.reborrow())?;
        }

        // TODO: `movetogoal` seemingly doesn't return anything in Quake should we remove this?
        self.globals.put_float(if did_move { 1. } else { 0. }, GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    /// Prints a message in the middle of a single client's screen.
    ///
    /// Equivalent to `PF_centerprint`.
    pub fn builtin_center_print(&mut self, argc: usize) -> progs::Result<()> {
        let ent_id = self.globals.get_entity_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let text = self.var_string(1, argc)?;

        let Some(dest) = self.client_message(ent_id) else {
            warn!("tried to centerprint to a non-client ({ent_id})");
            return Ok(());
        };

        ServerCmd::CenterPrint { text }.serialize(dest)?;

        Ok(())
    }

    pub fn builtin_change_level(&mut self) -> progs::Result<()> {
        // TODO: Do this properly!
        error!("TODO: ChangeLevel");
        let level_id = self.globals.string_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let level = self.string_table.get(level_id).unwrap();
        ServerCmd::StuffText { text: format!("map {level}\n").into() }
            .serialize(&mut self.broadcast)?;

        Ok(())
    }

    pub fn builtin_trace_line(&mut self) -> progs::Result<()> {
        let v1 = self.globals.get_vector(GLOBAL_ADDR_ARG_0 as i16)?;
        let v2 = self.globals.get_vector(GLOBAL_ADDR_ARG_1 as i16)?;
        let kind = self.globals.get_float(GLOBAL_ADDR_ARG_2 as i16)?;
        let ent = self.globals.get_entity_id(GLOBAL_ADDR_ARG_3 as i16)?;

        let (trace, hit_ent) = self.world.trace_entity_move(
            ent,
            v1,
            Vec3::ZERO,
            Vec3::ZERO,
            v2,
            CollideKind::from_f32(kind).unwrap_or_default(),
            |_| true,
        )?;

        self.globals.put_float(
            if trace.all_solid() { 1. } else { 0. },
            GlobalAddrFloat::TraceAllSolid as _,
        )?;
        self.globals.put_float(
            if trace.start_solid() { 1. } else { 0. },
            GlobalAddrFloat::TraceStartSolid as _,
        )?;
        self.globals.put_float(trace.ratio(), GlobalAddrFloat::TraceFraction as _)?;
        self.globals
            .put_float(if trace.in_water { 1. } else { 0. }, GlobalAddrFloat::TraceInWater as _)?;
        self.globals
            .put_float(if trace.in_open { 1. } else { 0. }, GlobalAddrFloat::TraceInOpen as _)?;
        self.globals.put_vector(trace.end_point(), GlobalAddrVector::TraceEndPos as _)?;
        // Our trace planes face into the surface, but QuakeC expects them to face out of it.
        self.globals.put_vector(
            trace.plane().map(|plane| -plane.normal()).unwrap_or(Vec3::ZERO),
            GlobalAddrVector::TracePlaneNormal as _,
        )?;
        self.globals.put_float(
            trace.plane_dist().map(|dist| -dist).unwrap_or_default(),
            GlobalAddrFloat::TracePlaneDist as _,
        )?;
        // `trace_ent` is world (entity 0) if nothing was hit.
        self.globals
            .put_entity_id(hit_ent.unwrap_or(EntityId(0)), GlobalAddrEntity::TraceEntity as _)?;

        Ok(())
    }

    pub fn builtin_normalize(&mut self) -> progs::Result<()> {
        let vec = self.globals.get_vector(GLOBAL_ADDR_ARG_0 as i16)?;
        let normalized = vec.normalize_or_zero();

        self.globals.put_vector(normalized, GLOBAL_ADDR_RETURN as _)?;
        Ok(())
    }
}

pub mod systems {
    use bevy::asset::LoadState;

    use bevy_mod_commands::CmdName;

    use crate::common::net::{
        self, ClientCmd, ClientMessage, GameType, ServerMessage, SignOnStage,
    };

    use super::*;

    pub fn recv_client_messages(
        mut server: Option<ResMut<Session>>,
        mut client_msgs: MessageReader<ClientMessage>,
        mut server_messages: MessageWriter<ServerMessage>,
        mut registry: ResMut<Registry>,
    ) {
        let mut out_packet = Vec::new();
        for ClientMessage { client_id, packet, kind: _ } in client_msgs.read() {
            let mut packet = &packet[..];
            let client_id = *client_id;
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

                                let Some(server) = &mut server else {
                                    if !matches!(
                                        &*name,
                                        "prespawn" | "name" | "color" | "spawn" | "begin"
                                    ) {
                                        warn!("Unhandled client command: {name}");
                                    }
                                    continue;
                                };

                                // Client input must never crash the server: log and carry on.
                                if let Err(e) = server.handle_string_cmd(
                                    client_id,
                                    &name,
                                    &args,
                                    &mut out_packet,
                                    registry.reborrow(),
                                ) {
                                    warn!("client {client_id}: `{name}` failed: {e:#}");
                                }
                            }
                        }
                        cmd @ ClientCmd::Move { .. } => {
                            let Some(server) = server.as_deref_mut() else {
                                continue;
                            };

                            if let Err(e) = server.handle_move(client_id, &cmd) {
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
            server_messages.write(ServerMessage { client_id: 0, packet: out_packet.into() });
        }
    }

    pub fn advance_pending_level(
        mut commands: Commands,
        pending: Res<PendingLevel>,
        asset_server: Res<AssetServer>,
        bsps: Res<Assets<Bsp>>,
        progs_assets: Res<Assets<ProgsAsset>>,
        mut registry: ResMut<Registry>,
    ) -> anyhow::Result<()> {
        for (name, state) in [
            (pending.map_name.as_str(), asset_server.load_state(&pending.bsp)),
            ("progs.dat", asset_server.load_state(&pending.progs)),
        ] {
            if let LoadState::Failed(e) = state {
                bail!("Failed loading {name}: {e}");
            }
        }

        let (Some(bsp), Some(progs)) = (bsps.get(&pending.bsp), progs_assets.get(&pending.progs))
        else {
            return Ok(());
        };

        // TODO: We can probably do better than deep-cloning the progs here.
        // The only mutable part of the loaded progs is the globals, everything
        // else can be shared.
        let progs = progs.clone();
        let models = BrushModel::from_bsp_data(&bsp.data)?
            .into_iter()
            .enumerate()
            .map(|(i, model)| Model::from_brush_model(format!("*{i}"), model).cast())
            .collect();
        let entmap = String::from_utf8_lossy(&bsp.data.entities).into_owned();

        // TODO: Make `max_clients` a cvar
        // TODO: When we start the server, `precache_model` and friends will be called
        // for all the relevant submodels. The level load has not fully finished until
        // those precached models are also loaded.
        commands.insert_resource(Session::new(
            pending.map_name.clone(),
            8,
            registry.reborrow(),
            progs,
            models,
            entmap,
        ));
        commands.remove_resource::<PendingLevel>();

        Ok(())
    }

    pub fn server_spawn(
        mut server: ResMut<Session>,
        mut registry: ResMut<Registry>,
        mut server_messages: MessageWriter<ServerMessage>,
        time: Res<Time<Fixed>>,
    ) -> progs::Result<()> {
        if !server.loading() {
            return Ok(());
        }

        // In `sv_init.c` there is a comment saying to run physics twice before starting the server
        // properly to "allow everything to settle".
        for _ in 0..2 {
            let server = &mut *server;
            server.level.physics(
                &server.persist.client_slots,
                time.delta(),
                registry.reborrow(),
            )?;
        }

        server.state = SessionState::Active;

        let coop = registry.read_cvar::<f32>("coop").unwrap_or_default();
        let game_type = if coop != 0. { GameType::CoOp } else { GameType::Deathmatch };

        let mut packet = Vec::new();
        ServerCmd::ServerInfo {
            protocol_version: net::PROTOCOL_VERSION as _,
            max_clients: server.max_clients() as _,
            game_type,
            message: "Seismon server".into(),
            model_precache: server.level.model_precache.iter().map(ToOwned::to_owned).collect(),
            sound_precache: server.level.sound_precache.iter().map(ToOwned::to_owned).collect(),
        }
        .serialize(&mut packet)?;

        for (id, style) in server.level.lightstyles.iter().enumerate() {
            let value = server.level.string_table.get(*style).unwrap_or_default();
            if !value.is_empty() {
                ServerCmd::LightStyle { id: id as _, value: value.into_owned() }
                    .serialize(&mut packet)?;
            }
        }

        ServerCmd::SignOnStage { stage: SignOnStage::Prespawn }.serialize(&mut packet)?;

        packet.extend_from_slice(&server.level.init);

        server_messages.write(ServerMessage { client_id: 0, packet: packet.into() });

        Ok(())
    }

    pub fn server_update(
        mut server: ResMut<Session>,
        time: Res<Time<Fixed>>,
        mut server_messages: MessageWriter<ServerMessage>,
        mut registry: ResMut<Registry>,
        mut unsent_entity_cache: Local<Vec<EntityId>>,
    ) {
        if server.loading()
            || registry.read_cvar::<u8>("sv_paused").unwrap() != 0
            || server.persist.client_slots.active_clients().count() == 0
        {
            return;
        }

        server.level.init_frame();

        let send_diff = match &mut *server {
            Session { persist, state: SessionState::Active, level } => {
                match level.physics(&persist.client_slots, time.delta(), registry.reborrow()) {
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

        if send_diff {
            let Session { persist, level, .. } = &mut *server;

            // Slots which were freed or reused since the last update need a fresh baseline.
            for entity_id in level.world.take_recycled() {
                let index = usize::try_from(entity_id.0).expect("Invalid entity ID");
                for (_, client) in persist.client_slots.active_clients_mut() {
                    client.sent_baseline.set(index, false);
                }
            }

            // TODO: Stop hardcoding `8` for max players
            for (client_id, client) in
                persist.client_slots.active_clients_mut().collect::<ArrayVec<_, 8>>()
            {
                let mut packet = Vec::new();

                // Reliable messages are kept until they are sent.
                if let Some(messages) = level.frame_client_messages.remove(&client.entity_id) {
                    packet.extend_from_slice(&messages);
                }

                ServerCmd::Time { time: seismon_utils::duration_to_f32(level.time) }
                    .serialize(&mut packet)
                    .unwrap();

                unsent_entity_cache.extend(level.world.iter().filter(|e_id| {
                    !client.sent_baseline[usize::try_from(e_id.0).expect("Invalid entity ID")]
                }));

                // Only mark a baseline as sent once it has actually been written, so entities
                // which aren't networked yet (no model, trigger, etc.) get one when they become
                // networked.
                for entity_id in unsent_entity_cache.drain(..) {
                    if level.is_networked(entity_id)
                        && let Some(state) = level.entity_state(entity_id)
                    {
                        state.spawn_baseline(entity_id.0 as _).serialize(&mut packet).unwrap();
                        // TODO: This is leftover from when we had `new_entities`, we need to
                        // reimplement it properly since this will break if we have multiple clients.
                        level.world.get_mut(entity_id).unwrap().baseline = state;
                        client.sent_baseline.set(entity_id.0 as usize, true);
                    }
                }

                // Skip world entity
                for ent in level.world.range(1..) {
                    // Never send an update for an entity the client has no baseline for.
                    if !client.sent_baseline[ent.0 as usize] {
                        continue;
                    }

                    if !level.is_networked(ent) {
                        continue;
                    };

                    let Ok(mut entity) = level.world.get(ent) else {
                        continue;
                    };

                    let Some(state) = entity.state() else {
                        continue;
                    };

                    let update = state.make_update(ent.0 as _, &entity.baseline);

                    // We send a `FastUpdate` even if the update is empty, as the client
                    // will delete entities if it doesn't receive an update.
                    ServerCmd::FastUpdate(update).serialize(&mut packet).unwrap();
                }

                if level.world.exists(client.entity_id)
                    && let Err(e) = level.write_client_data(client.entity_id, &mut packet)
                {
                    warn!("Failed to write client data for {}: {e}", client.entity_id);
                }

                // We add broadcast packets at the end to ensure that entities can spawn before
                // broadcasted events related to those entities
                packet.extend_from_slice(&level.broadcast);

                server_messages.write(ServerMessage { client_id, packet: packet.into() });
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::server::{
        TICK_RATE,
        progs::{
            Functions,
            functions::{FunctionDef, MAX_ARGS, Statement},
        },
        world::{
            EntityTypeDef,
            entity::STATIC_ADDRESS_COUNT,
            model::{ModelKind, SpriteModel},
        },
    };
    use seismon_utils::model::ModelFlags;

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

    #[test]
    fn write_angle_wraps_like_quake() {
        assert_eq!(super::write_angle_byte(0.), 0);
        assert_eq!(super::write_angle_byte(90.), 64);
        assert_eq!(super::write_angle_byte(-90.), 192);
        assert_eq!(super::write_angle_byte(360.), 0);
        assert_eq!(super::write_angle_byte(450.), 64);
        // Truncated to an integer before scaling.
        assert_eq!(super::write_angle_byte(1.9), 0);
    }

    #[test]
    fn chase_dirs_match_quake() {
        use super::{ChaseDirs, chase_dirs};

        assert_eq!(chase_dirs(100., 0.), ChaseDirs { x: Some(0.), y: None, diagonal: None });
        assert_eq!(chase_dirs(-100., 0.), ChaseDirs { x: Some(180.), y: None, diagonal: None });
        // Positive y is a yaw of 90.
        assert_eq!(chase_dirs(0., 100.), ChaseDirs { x: None, y: Some(90.), diagonal: None });
        assert_eq!(chase_dirs(0., -100.), ChaseDirs { x: None, y: Some(270.), diagonal: None });
        // Within 10 units isn't far enough.
        assert_eq!(chase_dirs(10., -10.), ChaseDirs { x: None, y: None, diagonal: None });

        assert_eq!(chase_dirs(100., 100.).diagonal, Some(45.));
        assert_eq!(chase_dirs(100., -100.).diagonal, Some(315.));
        assert_eq!(chase_dirs(-100., 100.).diagonal, Some(135.));
        assert_eq!(chase_dirs(-100., -100.).diagonal, Some(215.));

        // The diagonal points the same way as the delta (apart from Quake's 215).
        for (dx, dy) in [(100., 100.), (100., -100.), (-100., 100.)] {
            let yaw = chase_dirs(dx, dy).diagonal.unwrap();
            let dir = Vec2::from_angle(yaw.to_radians());
            assert!(dir.dot(Vec2::new(dx, dy).normalize()) > 0.99, "{dx} {dy} -> {yaw}");
        }
    }

    /// Mock builder for a `LevelState` with the given QuakeC functions and statements, but with
    /// an otherwise empty world.
    fn level_with_program(defs: Vec<FunctionDef>, statements: Vec<Statement>) -> LevelState {
        let mut string_table = StringTable::new(vec![0]);
        let world_model = Model {
            name: "*1".into(),
            kind: ModelKind::Sprite(SpriteModel),
            flags: ModelFlags::empty(),
        };
        let type_def = EntityTypeDef::new(STATIC_ADDRESS_COUNT, Box::new([])).unwrap();
        let world = World::new(vec![world_model], type_def, &mut string_table).unwrap();
        let functions =
            Functions { defs: defs.into_boxed_slice(), statements: statements.into_boxed_slice() };

        LevelState {
            string_table,
            sound_precache: Precache::new(),
            model_precache: Precache::new(),
            lightstyles: [StringId(0); MAX_LIGHTSTYLES],
            time: Duration::ZERO,
            cx: ExecutionContext::create(functions),
            globals: Globals::new(Vec::new(), vec![[0; 4]; 128].into_boxed_slice()),
            world,
            frame_client_messages: default(),
            broadcast: default(),
            init: default(),
            max_clients: 1,
            last_check_client: 0,
            last_check_time: Duration::ZERO,
            rng: ServerRng::new(0),
        }
    }

    fn quakec_def(entry: usize) -> FunctionDef {
        FunctionDef {
            kind: FunctionKind::QuakeC(entry),
            arg_start: 100,
            locals: 0,
            name_id: StringId(0),
            srcfile_id: StringId(0),
            argc: 0,
            argsz: [0; MAX_ARGS],
        }
    }

    #[test]
    fn nested_program_preserves_caller_pc() {
        // Regression test for an issue on `e1m1` where enemies would constantly cause overflow
        // errors.
        //
        // QuakeC run from inside a builtin (e.g. a touch function run by `movetogoal`) must leave
        // the calling function's program counter on its call statement. The caller steps past it
        // itself, so if the nested run also stepped, the statement after the call would be skipped.
        // If the skipped statement was `return`, execution ran on into the next function and
        // execution would overflow.

        let ret = Statement { opcode: Opcode::Return, arg1: 0, arg2: 0, arg3: 0 };
        let mut level =
            level_with_program(vec![quakec_def(0), quakec_def(5), quakec_def(10)], vec![ret; 11]);
        let mut ecs = bevy::ecs::world::World::new();
        ecs.init_resource::<Registry>();
        let mut registry = ecs.resource_mut::<Registry>();

        // Stand in for a caller that is in the middle of calling a builtin at statement 5.
        let strs = level.string_table.clone();
        level.cx.enter_function(&strs, &mut level.globals, FunctionId(1)).unwrap();
        assert_eq!(level.cx.pc(), 5);

        // The builtin runs QuakeC, as touch callbacks do.
        level.execute_program(FunctionId(2), registry.reborrow()).unwrap();

        assert_eq!(level.cx.call_stack_depth(), 1);
        assert_eq!(level.cx.current_function(), FunctionId(1));
        assert_eq!(level.cx.pc(), 5);
    }
}
