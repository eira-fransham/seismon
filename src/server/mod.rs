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
pub mod precache;
pub mod progs;
pub mod world;

use std::{
    cell::RefCell,
    ffi::CStr,
    fmt::{self},
    io::{Read, Write},
    ops::{Bound, RangeBounds as _},
    time::Duration,
};

use crate::{
    common::{
        bsp::BspLeafContents,
        console::{Registry, RunCmd, SeismonConsoleCorePlugin},
        math::{CollisionResult, Hyperplane},
        model::Model,
        net::{
            ClientMessage, EntityState, InMemoryMessagingClient, InMemoryMessagingServer,
            ServerCmd, ServerMessage, in_memory_messaging,
        },
        parse,
        vfs::Vfs,
    },
    server::{
        progs::{
            GlobalAddrFunction,
            functions::{BuiltinFunctionId, FunctionKind},
        },
        world::{FieldAddrEntityId, FieldAddrVector, MoveKind},
    },
};

use self::{
    precache::Precache,
    progs::{
        EntityFieldAddr, EntityId, ExecutionContext, FunctionId, GlobalAddrEntity, GlobalAddrFloat,
        GlobalAddrVector, Globals, LoadProgs, Opcode, ProgsError, StringId, StringTable,
        globals::{
            GLOBAL_ADDR_ARG_0, GLOBAL_ADDR_ARG_1, GLOBAL_ADDR_ARG_2, GLOBAL_ADDR_ARG_3,
            GLOBAL_ADDR_ARG_4, GLOBAL_ADDR_RETURN,
        },
    },
    world::{
        EntityFlags, EntitySolid, FieldAddrFloat, FieldAddrFunctionId, FieldAddrStringId, World,
        phys::{CollideKind, Trace, TraceEndKind},
    },
};

use arrayvec::ArrayVec;
use bevy::{
    app::{AppLabel, FixedMain, MainSchedulePlugin},
    math::bounding::{Aabb3d, BoundingVolume as _, IntersectsVolume as _},
    prelude::*,
    time::TimePlugin,
};
use bitflags::bitflags;
use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt as _};
use failure::bail;
use hashbrown::{HashMap, HashSet};
use itertools::Itertools;
use num::FromPrimitive;
use seismon_utils::{QStr, QString, duration_from_f32, duration_to_f32};
use serde::Deserialize;
use snafu::{Backtrace, Report};

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

#[derive(Default, Copy, Clone)]
pub struct SeismonListenServerPlugin;

impl Plugin for SeismonListenServerPlugin {
    fn build(&self, app: &mut App) {
        use bevy::ecs::schedule::ScheduleLabel as _;

        pub fn server_send_to_client(
            mut sender: ResMut<InMemoryMessagingServer>,
            mut server_events: MessageReader<ServerMessage>,
        ) {
            for event in server_events.read().cloned() {
                sender.send(event);
            }
        }

        pub fn server_recv_from_client(
            mut receiver: ResMut<InMemoryMessagingServer>,
            mut client_events: MessageWriter<ClientMessage>,
        ) {
            client_events.write_batch(receiver.recv());
        }

        pub fn client_send_to_server(
            mut sender: ResMut<InMemoryMessagingClient>,
            mut client_events: MessageReader<ClientMessage>,
        ) {
            for event in client_events.read().cloned() {
                sender.send(event);
            }
        }

        pub fn client_recv_from_server(
            mut receiver: ResMut<InMemoryMessagingClient>,
            mut server_events: MessageWriter<ServerMessage>,
        ) {
            server_events.write_batch(receiver.recv());
        }

        #[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, AppLabel)]
        struct ServerApp;

        let (client_messaging, server_messaging) = in_memory_messaging();
        app.add_message::<ServerMessage>()
            .add_message::<ClientMessage>()
            .insert_resource(client_messaging)
            .add_systems(First, client_recv_from_server)
            .add_systems(Last, client_send_to_server);

        let asset_server = app.world().resource::<AssetServer>().clone();
        let app_type_registry = app.world().resource::<AppTypeRegistry>().clone();
        let vfs = app.world().resource::<Vfs>().clone();

        let mut server_sub_app = SubApp::new();
        server_sub_app
            .add_plugins(MainSchedulePlugin)
            .insert_resource(app_type_registry)
            .insert_resource(asset_server)
            .insert_resource(vfs)
            .insert_resource(server_messaging)
            .add_message::<ServerMessage>()
            .add_message::<ClientMessage>()
            .add_systems(FixedFirst, server_recv_from_client)
            .add_systems(FixedLast, server_send_to_client)
            .add_plugins(SeismonServerPlugin)
            .add_plugins(TimePlugin);

        server_sub_app.update_schedule = Some(FixedMain.intern());

        server_sub_app.set_extract(|client_world, server_world| {
            server_world.resource_mut::<Messages<ClientMessage>>().write_batch(
                client_world
                    .resource_mut::<Messages<ClientMessage>>()
                    .iter_current_update_messages()
                    .cloned(),
            );
            client_world.resource_mut::<Messages<ServerMessage>>().write_batch(
                server_world
                    .resource_mut::<Messages<ServerMessage>>()
                    .iter_current_update_messages()
                    .cloned(),
            );
        });

        app.insert_sub_app(ServerApp, server_sub_app);
    }
}

impl Plugin for SeismonServerPlugin {
    fn build(&self, app: &mut App) {
        // TODO: Should we share consoles between client and server? Seems like it'd be better to
        // keep them separate.
        app.init_resource::<Vfs>()
            .add_systems(PreUpdate, systems::recv_client_messages)
            .add_systems(
                FixedMain,
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
            .insert_resource(Time::<Fixed>::from_seconds(TICK_RATE as _))
            .add_plugins(SeismonConsoleCorePlugin);

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
        vfs: &Vfs,
        progs: LoadProgs,
        models: Vec<Model>,
        entmap: String,
    ) -> Session {
        Session {
            persist: SessionPersistent::new(max_clients),
            state: SessionState::Loading,
            level: LevelState::new(bsp_name, progs, models, entmap, registry, vfs),
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
        vfs: &Vfs,
    ) -> Result<(), failure::Error> {
        let Some(client) = self.persist.client_slots.find_available() else {
            // TODO: Error handling
            return Ok(());
        };

        let set_new_args =
            self.level.globals.get_function_id(GlobalAddrFunction::SetNewArgs as i16)?;
        self.level.execute_program(set_new_args, registry.reborrow(), vfs)?;

        for (i, arg_addr) in SPAWN_PARAM_ADDRS.iter().enumerate() {
            client.spawn_params[i] = self.level.globals.load(*arg_addr)?;
        }

        Ok(())
    }

    pub fn clientcmd_name(&mut self, slot: usize, name: QString) -> Result<(), failure::Error> {
        let Some(client) = self.persist.client_mut(slot) else {
            bail!("No such client {}", slot);
        };

        ServerCmd::UpdateName { player_id: slot as _, new_name: name.clone() }
            .serialize(&mut self.level.broadcast)?;

        client.name = name;

        Ok(())
    }

    pub fn clientcmd_color(&mut self, slot: usize, color: u8) -> Result<(), failure::Error> {
        let Some(client) = self.client_mut(slot) else {
            bail!("No such client {}", slot);
        };

        client.color = color;

        Ok(())
    }

    // TODO: Spawn parameters
    pub fn clientcmd_spawn(&mut self, slot: usize) -> Result<(), failure::Error> {
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
        vfs: &Vfs,
    ) -> Result<(), failure::Error> {
        let client_entity = self.level.world.alloc_uninitialized_reserved()?;

        let Some(client) = self.persist.client_mut(slot) else {
            bail!("No such client {}", slot);
        };

        // TODO: All players are currently privileged
        client.state =
            ClientState::Active(ClientActive { _privileged: true, entity_id: client_entity });

        for (i, arg_addr) in SPAWN_PARAM_ADDRS.iter().enumerate() {
            self.level.globals.store(*arg_addr, client.spawn_params[i])?;
        }

        self.level.globals.store(GlobalAddrEntity::Self_, client_entity)?;
        self.level.globals.store(GlobalAddrEntity::World, EntityId(0))?;
        self.level.globals.store(GlobalAddrFloat::Time, duration_to_f32(self.level.time))?;
        let client_connect =
            self.level.globals.get_function_id(GlobalAddrFunction::ClientConnect as i16)?;
        self.level.execute_program(client_connect, registry.reborrow(), vfs)?;

        self.level.globals.store(GlobalAddrEntity::Self_, client_entity)?;
        self.level.globals.store(GlobalAddrEntity::World, EntityId(0))?;
        self.level.globals.store(GlobalAddrFloat::Time, duration_to_f32(self.level.time))?;
        let put_client_in_server =
            self.level.globals.get_function_id(GlobalAddrFunction::PutClientInServer as i16)?;
        self.level.execute_program(put_client_in_server, registry.reborrow(), vfs)?;

        Ok(())
    }

    pub fn precache_sound(&mut self, name_id: StringId) {
        if let SessionState::Loading = self.state {
            self.level.precache_sound(name_id);
        } else {
            panic!("Sounds cannot be precached after loading");
        }
    }

    pub fn precache_model(&mut self, name_id: StringId) {
        if let SessionState::Loading = self.state {
            self.level.precache_model(name_id);
        } else {
            panic!("Models cannot be precached after loading");
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
/// Quake splits this into `accelerate` and `airaccelerate` but the only difference is
/// limiting the maximum desired speed.
pub fn player_accelerate(move_amt: Vec3, vel: Vec3, accel: f32) -> Vec3 {
    let desired_dir = move_amt.normalize_or_zero();
    let current_speed = desired_dir.dot(vel);
    let desired_speed = move_amt.length();
    let max_accel = (desired_speed - current_speed).max(0.);
    vel + (accel * desired_speed).min(max_accel) * desired_dir
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
    ground_accel: f32,
    #[serde(rename(deserialize = "sv_airaccelerate"))]
    air_accel: f32,
    #[serde(rename(deserialize = "sv_wateraccelerate"))]
    water_accel: f32,
    #[serde(rename(deserialize = "sv_friction"))]
    ground_friction: f32,
    #[serde(rename(deserialize = "sv_waterfriction"))]
    water_friction: f32,
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

    new_entities: HashSet<EntityId>,

    // -- TODO: Next 3 fields should be extracted to a separate struct for borrowck purposes --
    /// Reliable messages sent to one client.
    frame_client_messages: HashMap<EntityId, Vec<u8>>,

    /// Unreliable messages sent to all clients, for this frame only.
    broadcast: Vec<u8>,

    /// Persistent bytestring to append to the message sent to clients on sign-on.
    init: Vec<u8>,
}

// TODO: Copied from Quake 1 source, can be done better.
fn angle_mod(quake_angle: f32) -> f32 {
    const PRECISION: f32 = 65536.;
    let quake_angle = quake_angle.rem_euclid(360.);

    (360f32 / PRECISION) * (quake_angle * PRECISION / 360.0).floor()
}

// TODO: This can definitely be done better, but for now we directly take the implementation from
// Quake.
fn angle_vectors(angles: Vec3, movement: Vec3) -> Vec3 {
    let angle = angles.x.to_radians();
    let sy = angle.sin();
    let cy = angle.cos();
    let angle = angles.y.to_radians();
    let sp = angle.sin();
    let cp = angle.cos();
    let angle = angles.z.to_radians();
    let sr = angle.sin();
    let cr = angle.cos();

    movement.x * Vec3::new(cp * cy, cp * sy, -sp)
        + movement.y * Vec3::new(-sr * sp * cy + cr * sy, -(sr * sp * sy + cr * cy), -sr * cp)
        + movement.z * Vec3::new(cr * sp * cy + sr * sy, cr * sp * sy + -sr * cy, cr * cp)
}

const DEBUG_DUMP: bool = false;

impl LevelState {
    pub fn new(
        map_path: String,
        progs: LoadProgs,
        models: Vec<Model>,
        entmap: String,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> LevelState {
        let LoadProgs { cx, mut globals, entity_def, mut string_table } = progs;

        globals.store(GlobalAddrFloat::FrameTime, TICK_RATE).unwrap();

        let sound_precache = Precache::new();
        let mut model_precache = Precache::new();

        model_precache.precache(&map_path);

        for model in models.iter() {
            let model_name = string_table.find_or_insert(model.name());
            let name = string_table.get(model_name).unwrap();
            // "*0" is the null model
            if &*name.raw != b"*0" {
                model_precache.precache(name.to_str());
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
            new_entities: default(),
            cx,
            globals,
            world,

            broadcast: default(),
            init: default(),
        };

        if DEBUG_DUMP {
            let debug_dump =
                std::fs::read(dbg!(concat!(env!("CARGO_MANIFEST_DIR"), "/debug-edicts-dump.bin")))
                    .unwrap();

            let mut reader = std::io::Cursor::new(&debug_dump);
            let mut big_buf = Vec::new();

            loop {
                #[repr(C)]
                #[derive(Debug)]
                #[allow(non_snake_case)]
                struct PrGlobals {
                    pad: [i32; 28],
                    this: i32,
                    other: i32,
                    world: i32,
                    time: f32,
                    frametime: f32,
                    force_retouch: f32,
                    mapname: i32,
                    deathmatch: f32,
                    coop: f32,
                    teamplay: f32,
                    serverflags: f32,
                    total_secrets: f32,
                    total_monsters: f32,
                    found_secrets: f32,
                    killed_monsters: f32,
                    parm1: f32,
                    parm2: f32,
                    parm3: f32,
                    parm4: f32,
                    parm5: f32,
                    parm6: f32,
                    parm7: f32,
                    parm8: f32,
                    parm9: f32,
                    parm10: f32,
                    parm11: f32,
                    parm12: f32,
                    parm13: f32,
                    parm14: f32,
                    parm15: f32,
                    parm16: f32,
                    v_forward: [f32; 3],
                    v_up: [f32; 3],
                    v_right: [f32; 3],
                    trace_allsolid: f32,
                    trace_startsolid: f32,
                    trace_fraction: f32,
                    trace_endpos: [f32; 3],
                    trace_plane_normal: [f32; 3],
                    trace_plane_dist: f32,
                    trace_ent: i32,
                    trace_inopen: f32,
                    trace_inwater: f32,
                    msg_entity: i32,
                    main: i32,

                    StartFrame: i32,
                    PlayerPreThink: i32,
                    PlayerPostThink: i32,
                    ClientKill: i32,
                    ClientConnect: i32,
                    PutClientInServer: i32,
                    ClientDisconnect: i32,
                    SetNewParms: i32,
                    SetChangeParms: i32,
                }

                const BUILTIN: &CStr = c"BUILTIN";
                const BUILTIN_LEN: usize = BUILTIN.to_bytes_with_nul().len();
                const PRESTATE: &CStr = c"PRESTATE";
                const PRESTATE_LEN: usize = PRESTATE.to_bytes_with_nul().len();
                const POSTSTATE: &CStr = c"POSTSTATE";
                const POSTSTATE_LEN: usize = POSTSTATE.to_bytes_with_nul().len();
                const PREGLOBALS: &CStr = c"PREGLOBALS";
                const PREGLOBALS_LEN: usize = PREGLOBALS.to_bytes_with_nul().len();
                const POSTGLOBALS: &CStr = c"POSTGLOBALS";
                const POSTGLOBALS_LEN: usize = POSTGLOBALS.to_bytes_with_nul().len();
                const DONE: &CStr = c"DONE";
                const DONE_LEN: usize = DONE.to_bytes_with_nul().len();

                let mut buf = [0; POSTGLOBALS_LEN];
                reader.read_exact(&mut buf[..BUILTIN_LEN]).unwrap();
                assert_eq!(CStr::from_bytes_with_nul(&buf[..BUILTIN_LEN]).unwrap(), BUILTIN);
                let _builtin_idx = dbg!(
                    BuiltinFunctionId::from_i32(reader.read_i32::<LittleEndian>().unwrap())
                        .unwrap()
                );

                reader.read_exact(&mut buf[..PRESTATE_LEN]).unwrap();
                assert_eq!(CStr::from_bytes_with_nul(&buf[..PRESTATE_LEN]).unwrap(), PRESTATE);

                let num_bytes: u64 =
                    dbg!(reader.read_i32::<LittleEndian>().unwrap().try_into().unwrap());
                big_buf.clear();
                let reader_bytes =
                    reader.by_ref().take(num_bytes).read_to_end(&mut big_buf).unwrap();
                assert_eq!(reader_bytes, num_bytes as usize);
                assert_eq!(big_buf.len(), num_bytes as usize);

                reader.read_exact(&mut buf[..PREGLOBALS_LEN]).unwrap();
                assert_eq!(CStr::from_bytes_with_nul(&buf[..PREGLOBALS_LEN]).unwrap(), PREGLOBALS);
                let num_bytes: u64 =
                    dbg!(reader.read_i32::<LittleEndian>().unwrap().try_into().unwrap());
                big_buf.clear();
                let reader_bytes =
                    reader.by_ref().take(num_bytes).read_to_end(&mut big_buf).unwrap();
                assert_eq!(reader_bytes, num_bytes as usize);
                assert_eq!(big_buf.len(), num_bytes as usize);
                assert_eq!(big_buf.len(), std::mem::size_of::<PrGlobals>());
                println!("{:#?}", unsafe { &*big_buf.as_ptr().cast::<PrGlobals>() });

                reader.read_exact(&mut buf[..POSTSTATE_LEN]).unwrap();
                assert_eq!(CStr::from_bytes_with_nul(&buf[..POSTSTATE_LEN]).unwrap(), POSTSTATE);

                let num_bytes: u64 =
                    dbg!(reader.read_i32::<LittleEndian>().unwrap().try_into().unwrap());
                big_buf.clear();
                let reader_bytes =
                    reader.by_ref().take(num_bytes).read_to_end(&mut big_buf).unwrap();
                assert_eq!(reader_bytes, num_bytes as usize);
                assert_eq!(big_buf.len(), num_bytes as usize);

                reader.read_exact(&mut buf[..POSTGLOBALS_LEN]).unwrap();
                assert_eq!(
                    CStr::from_bytes_with_nul(&buf[..POSTGLOBALS_LEN]).unwrap(),
                    POSTGLOBALS
                );

                let num_bytes: u64 =
                    dbg!(reader.read_i32::<LittleEndian>().unwrap().try_into().unwrap());
                big_buf.clear();
                let reader_bytes =
                    reader.by_ref().take(num_bytes).read_to_end(&mut big_buf).unwrap();
                assert_eq!(reader_bytes, num_bytes as usize);
                assert_eq!(big_buf.len(), num_bytes as usize);
                assert_eq!(big_buf.len(), std::mem::size_of::<PrGlobals>());
                println!("{:#?}", unsafe { &*big_buf.as_ptr().cast::<PrGlobals>() });

                reader.read_exact(&mut buf[..DONE_LEN]).unwrap();
                assert_eq!(CStr::from_bytes_with_nul(&buf[..DONE_LEN]).unwrap(), DONE);
            }
        }

        for entity in entity_list {
            if let Err(e) = level.spawn_entity_from_map(entity, registry.reborrow(), vfs) {
                error!("Failed spawning entity: {e}");
            }
        }

        level
    }

    pub fn string(&self, string_id: StringId) -> Option<QStr<'_>> {
        self.string_table.get(string_id)
    }

    pub fn precache_sound(&mut self, name_id: StringId) {
        self.sound_precache.precache(self.string_table.get(name_id).unwrap().to_str());
    }

    pub fn precache_model(&mut self, name_id: StringId) {
        self.model_precache.precache(self.string_table.get(name_id).unwrap().to_str())
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

    pub fn entity_state(&self, id: EntityId) -> Option<EntityState> {
        self.world.get(id).ok()?.state()
    }

    pub fn is_networked(&self, id: EntityId) -> bool {
        self.world
            .get(id)
            .ok()
            .and_then(|mut ent| ent.solid().ok())
            // TODO: Is this the right way to hide triggers client-side?
            .map(|s| s != EntitySolid::Trigger)
            .unwrap_or_default()
    }

    pub fn init_frame(&mut self) {
        // TODO: Only clear if the message was sent.
        for (_, message) in &mut self.frame_client_messages {
            message.clear();
        }
        self.broadcast.clear();
        self.new_entities.clear();
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
                // TODO: We should insert the client ID when the client joins and reject if it is
                // not found here.
                Ok(self.frame_client_messages.entry(client).or_default())
            }
            MsgDest::Init => Ok(&mut self.init),
        }
    }

    fn enter_builtin(
        &mut self,
        builtin_id: BuiltinFunctionId,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
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
            CenterPrint => self.builtin_center_print()?,
            ChangeLevel => self.builtin_change_level()?,
            ChangeYaw => self.builtin_change_yaw()?,
            CheckBottom => self.builtin_check_bottom(&registry)?,
            CheckClient => self.builtin_check_client()?,
            CoreDump => todo_builtin!(CoreDump),
            Cvar => self.builtin_cvar(&registry)?,
            CvarSet => self.builtin_cvar_set(registry.reborrow())?,
            DPrint => self.builtin_dprint()?,
            DropToFloor => self.builtin_drop_to_floor(registry.reborrow(), vfs)?,
            EPrint => todo_builtin!(EPrint),
            Error => self.builtin_err("Error")?,
            FAbs => self.globals.builtin_f_abs()?,
            FindRadius => self.builtin_find_radius()?,
            Find => self.builtin_find()?,
            Floor => self.globals.builtin_floor()?,
            FToS => self.builtin_ftos()?,
            LightStyle => self.builtin_light_style()?,
            LocalCmd => todo_builtin!(LocalCmd),
            MakeStatic => self.builtin_make_static()?,
            MakeVectors => self.globals.make_vectors()?,
            MoveToGoal => self.builtin_move_to_goal(registry.reborrow(), vfs)?,
            NextEnt => todo_builtin!(NextEnt),
            Normalize => self.builtin_normalize()?,
            ObjError => self.builtin_err("Object error")?,
            Particle => self.builtin_particle()?,
            PointContents => self.builtin_point_contents()?,
            // Only used in `qcc`, does nothing at runtime
            PrecacheFile => {}
            // Only used in `qcc`, does nothing at runtime
            PrecacheFile2 => {}
            // PrecacheModel2/PrecacheSound2 only differ for `qcc`, not at runtime
            PrecacheModel | PrecacheModel2 => self.builtin_precache_model(vfs)?,
            PrecacheSound | PrecacheSound2 => self.builtin_precache_sound()?,
            Random => self.globals.builtin_random()?,
            Remove => self.builtin_remove()?,
            RInt => self.globals.builtin_r_int()?,
            SetModel => self.builtin_set_model()?,
            SetOrigin => self.builtin_set_origin(registry.reborrow(), vfs)?,
            SetSize => self.builtin_set_size()?,
            SetSpawnArgs => todo_builtin!(SetSpawnArgs),
            Sound => self.builtin_sound()?,
            Spawn => self.builtin_spawn(registry.reborrow(), vfs)?,
            SPrint => self.builtin_sprint()?,
            StuffCmd => self.builtin_stuffcmd()?,
            TraceLine => self.builtin_trace_line()?,
            TraceOff => todo_builtin!(TraceOff),
            TraceOn => todo_builtin!(TraceOn),
            VecToAngles => self.globals.builtin_vec_to_angles()?,
            VecToYaw => self.globals.builtin_vec_to_yaw()?,
            VLen => self.globals.builtin_v_len()?,
            VToS => self.builtin_vtos()?,
            WalkMove => self.builtin_walk_move(registry.reborrow(), vfs)?,
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
    pub fn execute_program(
        &mut self,
        f: FunctionId,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> progs::Result<()> {
        use Opcode::*;

        let mut runaway = 10000;

        let exit_depth = self.cx.call_stack_depth();

        self.cx.enter_function(&self.string_table, &mut self.globals, f)?;

        while self.cx.call_stack_depth() != exit_depth {
            runaway -= 1;

            if runaway == 0 {
                self.cx.print_backtrace(&self.string_table, false);
                return Err(ProgsError::LocalStackOverflow { backtrace: Backtrace::capture() });
            }

            let statement = self.cx.load_statement();
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
                        self.enter_builtin(b, registry.reborrow(), vfs)?;
                        debug!(
                            "Returning from built-in function {}",
                            self.string_table.get(name_id).unwrap()
                        );
                    } else {
                        self.cx.enter_function(&self.string_table, &mut self.globals, f_to_call)?;
                        continue;
                    }
                }

                Done | Return => self.op_return(a, b, c)?,

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
                LoadFld => panic!("load_fld not implemented"),
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
                StorePFld => panic!("storep_fld not implemented"),
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
        vfs: &Vfs,
    ) -> progs::Result<()>
    where
        S: AsRef<str>,
    {
        let name = name.as_ref();
        let func_id = self.cx.find_function_by_name(&self.string_table, name)?;
        self.execute_program(func_id, registry, vfs)?;

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
        vfs: &Vfs,
    ) -> progs::Result<()> {
        self.world.link_entity(ent_id)?;

        if touch_triggers {
            self.touch_triggers(ent_id, registry, vfs)?;
        }

        Ok(())
    }

    pub fn spawn_entity(&mut self, registry: Mut<Registry>, vfs: &Vfs) -> progs::Result<EntityId> {
        let ent_id = self.world.alloc_uninitialized()?;

        self.link_entity(ent_id, false, registry, vfs)?;

        self.new_entities.insert(ent_id);

        Ok(ent_id)
    }

    pub fn spawn_entity_from_map<'a, I: IntoIterator<Item = (&'a str, &'a str)>>(
        &mut self,
        map: I,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
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
        self.execute_program_by_name(&classname, registry.reborrow(), vfs)?;

        Ok(ent_id)
    }

    pub fn set_entity_origin(
        &mut self,
        ent_id: EntityId,
        origin: Vec3,
        registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> progs::Result<()> {
        self.world.get_mut(ent_id)?.store(FieldAddrVector::Origin, origin)?;
        self.link_entity(ent_id, false, registry, vfs)?;

        Ok(())
    }

    pub fn set_entity_model(
        &mut self,
        ent_id: EntityId,
        model_name_id: StringId,
    ) -> progs::Result<()> {
        let model_name = self
            .string_table
            .get(model_name_id)
            .ok_or_else(|| ProgsError::with_msg("String not found"))?;
        let model_id = {
            let mut ent = self.world.get_mut(ent_id)?;

            ent.set_model_name(model_name_id)?;

            let model_id = match self.string_table.get(model_name_id) {
                Some(name) => match self.model_precache.find(name.to_str()) {
                    Some(i) => i,
                    None => return Err(ProgsError::with_msg("model not precached")),
                },
                None => return Err(ProgsError::with_msg("invalid StringId")),
            };

            ent.set_model_index(model_id as _)?;

            model_id
        };

        if model_name[0] == b'*' {
            self.world.set_entity_model(ent_id, model_id)?;
        }

        Ok(())
    }

    pub fn think(
        &mut self,
        ent_id: EntityId,
        frame_time: Duration,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> progs::Result<bool> {
        loop {
            let mut ent = self.world.get_mut(ent_id)?;
            let think_time = ent.load(FieldAddrFloat::NextThink)?;

            if think_time <= 0. || think_time > duration_to_f32(self.time + frame_time) {
                // Think either already happened or isn't due yet.
                return Ok(true);
            }

            let think_time = think_time.max(duration_to_f32(self.time));

            // Deschedule next think.
            ent.store(FieldAddrFloat::NextThink, 0.0)?;

            // Call entity's think function.
            let think = ent.load(FieldAddrFunctionId::Think)?;
            self.globals.store(GlobalAddrFloat::Time, think_time)?;
            self.globals.store(GlobalAddrEntity::Self_, ent_id)?;
            self.execute_program(think, registry.reborrow(), vfs)?;

            if !self.world.exists(ent_id) {
                return Ok(false);
            }
        }
    }

    pub fn start_frame(&mut self, mut registry: Mut<Registry>, vfs: &Vfs) -> progs::Result<()> {
        self.cx.reset();

        self.globals.store(GlobalAddrEntity::Self_, EntityId(0))?;
        self.globals.store(GlobalAddrEntity::Other, EntityId(0))?;
        self.globals.store(GlobalAddrEntity::World, EntityId(0))?;
        self.globals.store(GlobalAddrFloat::Time, duration_to_f32(self.time))?;

        let start_frame = self.globals.get_function_id(GlobalAddrFunction::StartFrame as i16)?;
        self.execute_program(start_frame, registry.reborrow(), vfs)?;

        Ok(())
    }

    /// Player fly (entities use `physics_toss`).
    fn physics_player_fly(
        &mut self,
        ent_id: EntityId,
        frame_time: Duration,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> progs::Result<CollisionResult<Option<Trace>>> {
        const BUMP_COUNT: usize = 4;
        const MAX_CLIP_PLANES: usize = 5;

        let mut out = CollisionResult::default();

        let mut ent = self.world.get(ent_id)?;
        let mut original_velocity = ent.velocity()?;
        let primal_velocity = original_velocity;
        let mut planes = ArrayVec::<Hyperplane, MAX_CLIP_PLANES>::new();

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

            if trace.all_solid {
                self.world.get_mut(ent_id)?.set_velocity(Vec3::ZERO)?;
                return Ok(CollisionResult {
                    floor: true,
                    wall_or_step: true,
                    dead_stop: false,
                    ..out
                });
            }

            // TODO: Fix this (`original_velocity` not used)
            #[expect(unused_assignments)]
            if trace.ratio() > f32::EPSILON {
                let mut ent = self.world.get_mut(ent_id)?;
                original_velocity = ent.velocity()?;
                ent.set_origin(trace.end_point())?;
            }

            let Some(plane) = trace.plane().cloned() else {
                // Travelled full trace distance.
                break;
            };

            // let Some(ent) = ent else {
            //   // TODO: Why does Quake assume that ent cannot be world?
            //   return Err(ProgsError::with_msg("!trace.ent"));
            // };

            if plane.is_floor() {
                out.floor = true;
            }

            time_left -= time_left * trace.ratio();

            if plane.normal().z.abs() < f32::EPSILON {
                out.wall_or_step = true;
                out.metadata = Some(trace);
            }

            if let Some(touched_ent_id) = touched_ent_id {
                self.run_impact_callbacks(ent_id, touched_ent_id, registry.reborrow(), vfs)?;
            }

            if planes.try_push(plane.clone()).is_err() {
                self.world.get_mut(ent_id)?.set_velocity(Vec3::ZERO)?;
                return Ok(CollisionResult {
                    floor: true,
                    wall_or_step: true,
                    dead_stop: false,
                    ..out
                });
            }

            let collision_result = Hyperplane::clip_velocity_to_many(&planes[..], velocity, 1.);

            self.world.get_mut(ent_id)?.set_velocity(collision_result.metadata)?;

            if collision_result.dead_stop {
                return Ok(collision_result.map(move |_| out.metadata));
            }

            if collision_result.metadata.dot(primal_velocity) <= 0. {
                self.world.get_mut(ent_id)?.set_velocity(Vec3::ZERO)?;
                break;
            }
        }

        Ok(out)
    }

    fn physics_walk(
        &mut self,
        ent_id: EntityId,
        frame_time: Duration,
        server_vars: &ServerVars,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> progs::Result<()> {
        const ORIGIN_EPSILON: f32 = 0.03125;

        let mut ent = self.world.get_mut(ent_id)?;

        let water_jump = ent.has_flag(EntityFlags::WATER_JUMP)?;
        let was_on_ground = ent.has_flag(EntityFlags::ON_GROUND)?;
        ent.remove_flags(EntityFlags::ON_GROUND)?;

        let old_origin = ent.origin()?;
        let old_velocity = ent.velocity()?;
        let water_level = ent.water_level()?;
        let move_kind = ent.move_kind()?;

        let clip = self.physics_player_fly(ent_id, frame_time, registry.reborrow(), vfs)?;

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

        self.push_entity(ent_id, upmove, registry.reborrow(), vfs)?;

        let clip = self.physics_player_fly(ent_id, frame_time, registry.reborrow(), vfs)?;

        let mut ent = self.world.get(ent_id)?;
        let post_step_origin = ent.origin()?;
        let post_clip_move = (post_step_origin - old_origin).xy().abs();

        if clip.any_collision() && post_clip_move.to_array().iter().all(|v| *v < ORIGIN_EPSILON) {
            error!("TODO: SV_TryUnstick");
        }

        if clip.wall_or_step {
            error!("TODO: SV_WallFriction");
        }

        let (downtrace, down_ent) = self.push_entity(ent_id, downmove, registry.reborrow(), vfs)?;
        if downtrace.plane().map(|plane| plane.is_floor()).unwrap_or_default() {
            let mut ent = self.world.get_mut(ent_id)?;
            if ent.solid()? == EntitySolid::Bsp {
                ent.add_flags(EntityFlags::ON_GROUND)?;
                ent.set_ground(down_ent.unwrap_or(EntityId(-1)))?;
            }
        } else {
            info!("Tried to step but could not");
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
        vfs: &Vfs,
    ) -> progs::Result<()> {
        let server_vars: ServerVars = registry.read_cvars()?;

        let mut ent = self.world.get(ent_id)?;
        match ent.move_kind()? {
            MoveKind::Walk => {
                self.physics_walk(ent_id, frame_time, &server_vars, registry.reborrow(), vfs)?
            }

            MoveKind::Push => self.physics_push(ent_id, frame_time, registry.reborrow(), vfs)?,
            MoveKind::NoClip => {
                self.physics_noclip(ent_id, frame_time, registry.reborrow(), vfs)?
            }
            MoveKind::Step => self.physics_step(ent_id, frame_time, vfs, registry.reborrow())?,
            MoveKind::Fly | MoveKind::FlyMissile => {
                self.physics_toss(ent_id, frame_time, vfs, registry.reborrow(), false, 1.)?;
            }
            MoveKind::Bounce => {
                self.physics_toss(ent_id, frame_time, vfs, registry.reborrow(), true, 1.5)?
            }
            MoveKind::Toss => {
                self.physics_toss(ent_id, frame_time, vfs, registry.reborrow(), true, 1.)?
            }
            // No actual physics for this entity, but still let it think.
            MoveKind::None => {
                self.think(ent_id, frame_time, registry.reborrow(), vfs)?;
            }

            MoveKind::AngleNoClip | MoveKind::AngleClip => {
                warn!("TODO: Fly physics");
                self.think(ent_id, frame_time, registry.reborrow(), vfs)?;
            }
        }

        Ok(())
    }

    pub fn physics_client(
        &mut self,
        player_id: EntityId,
        frame_time: Duration,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> progs::Result<()> {
        let server_vars: ServerVars = registry.read_cvars()?;

        let pre_think = self.globals.load(GlobalAddrFunction::PlayerPreThink)?;
        self.globals.store(GlobalAddrFloat::Time, duration_to_f32(self.time))?;
        self.globals.store(GlobalAddrEntity::Self_, player_id)?;
        self.execute_program(pre_think, registry.reborrow(), vfs)?;

        let mut player = self.world.get_mut(player_id)?;

        player.limit_velocity(server_vars.max_entity_velocity)?;

        match player.move_kind()? {
            MoveKind::None => {
                self.think(player_id, frame_time, registry.reborrow(), vfs)?;
            }
            MoveKind::Walk => {
                if !self.think(player_id, frame_time, registry.reborrow(), vfs)? {
                    return Ok(());
                }

                let mut ent = self.world.get_mut(player_id)?;

                let with_gravity =
                    ent.velocity()? - Vec3::Z * server_vars.gravity * duration_to_f32(frame_time);
                ent.set_velocity(with_gravity)?;

                self.physics_walk(player_id, frame_time, &server_vars, registry.reborrow(), vfs)?;
            }
            _ => error!("TODO: Non-walk players"),
        }

        self.link_entity(player_id, true, registry.reborrow(), vfs)?;

        let post_think = self.globals.load(GlobalAddrFunction::PlayerPostThink)?;
        self.globals.store(GlobalAddrFloat::Time, duration_to_f32(self.time))?;
        self.globals.store(GlobalAddrEntity::Self_, player_id)?;
        self.execute_program(post_think, registry, vfs)?;

        Ok(())
    }

    pub fn physics(
        &mut self,
        clients: &ClientSlots,
        frame_time: Duration,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> progs::Result<HashMap<EntityId, ProgsError>> {
        self.globals.store(GlobalAddrFloat::FrameTime, duration_to_f32(frame_time))?;

        self.start_frame(registry.reborrow(), vfs)?;

        let mut ent_error_map = HashMap::<EntityId, ProgsError>::new();

        for (_, ClientActive { entity_id, .. }) in clients.active_clients() {
            if let Err(e) = self.physics_client(*entity_id, frame_time, registry.reborrow(), vfs) {
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
                if let Err(e) = self.link_entity(ent_id, true, registry.reborrow(), vfs) {
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
                && let Err(e) = self.ent_physics(ent_id, frame_time, registry.reborrow(), vfs)
            {
                ent_error_map.insert(ent_id, e);
            }
        }

        let f = self.globals.load(GlobalAddrFloat::ForceRetouch)?;
        if f > 0.0 {
            self.globals.store(GlobalAddrFloat::ForceRetouch, 0.)?;
        }

        self.time += frame_time;

        Ok(ent_error_map)
    }

    fn player_apply_friction(
        &mut self,
        ent_id: EntityId,
        frame_time: Duration,
        player_vars: &PlayerVars,
    ) -> progs::Result<()> {
        /// If the player speed is below this, zero the X and Y of the player velocity.
        const MINIMUM_SPEED: f32 = 1.;
        /// Use this * normalized velocity as the starting point for dropoff detection.
        const DROPOFF_DETECT_DIST: f32 = 16.;
        /// TODO: _Extremely_ magic number.
        const DROPOFF_TRACE_Z: f32 = 34.;
        /// If the player is about to fall off an edge, increase friction.
        const DROPOFF_FRICTION_MUL: f32 = 2.;

        let mut ent = self.world.get(ent_id)?;

        let velocity = ent.velocity()?;

        let speed = velocity.length_squared();

        if speed < MINIMUM_SPEED {
            self.world.get_mut(ent_id)?.set_velocity(Vec3::ZERO + Vec3::Z * velocity.z)?;
            return Ok(());
        }

        let on_ground = ent.has_flag(EntityFlags::ON_GROUND)?;
        let water_level = ent.water_level()?;

        let base_friction = player_vars.ground_friction;
        let friction = if on_ground {
            let origin = ent.origin()?;
            let min = ent.min()?;
            let max = ent.max()?;

            // We know that velocity must be non-zero because of guard above but we use `_or_zero`
            // just in case. Technically this should probably be
            // `velocity.with_z(0.).clamp_length_min(DROPOFF_DETECT_DIST)` but for now
            // just match Quake.
            let dropoff_dist = (velocity.normalize_or_zero() * DROPOFF_DETECT_DIST).with_z(min.z);
            let start = origin + dropoff_dist;
            let end = start - DROPOFF_TRACE_Z;

            let (trace, _) = self.world.trace_entity_move(
                ent_id,
                start,
                min,
                max,
                end,
                CollideKind::Normal,
                |_| true,
            )?;

            if trace.is_terminal() { base_friction * DROPOFF_FRICTION_MUL } else { base_friction }
        } else {
            base_friction
        };

        let (friction, friction_speed) = if water_level >= 2. {
            (player_vars.water_friction * water_level, speed)
        } else if on_ground {
            (friction, speed.max(player_vars.stop_speed))
        } else {
            (0., speed)
        };

        let new_speed =
            (speed - friction_speed * friction * duration_to_f32(frame_time)).max(0.) / speed;

        self.world.get_mut(ent_id)?.set_velocity(velocity * new_speed)?;

        Ok(())
    }

    fn player_categorize_position(&mut self, ent_id: EntityId) -> progs::Result<()> {
        let mut ent = self.world.get_mut(ent_id)?;

        ent.remove_flags(EntityFlags::ON_GROUND)?;

        let origin = ent.origin()?;
        let min = ent.min()?;
        let max = ent.max()?;
        let check_floor_pos = origin - Vec3::Z;

        if ent.velocity_z()? > 180. {
            return Ok(());
        }

        let (trace, floor_ent) = self.world.trace_entity_move(
            ent_id,
            origin,
            min,
            max,
            check_floor_pos,
            CollideKind::default(),
            |_| true,
        )?;

        if trace.plane().map(|plane| plane.is_floor()) == Some(true) {
            let mut ent = self.world.get_mut(ent_id)?;
            ent.add_flags(EntityFlags::ON_GROUND)?;

            if let Some(floor_ent) = floor_ent {
                ent.set_ground(floor_ent)?;
            } else {
                ent.set_ground(EntityId::NONE)?;
            }
        }

        Ok(())
    }

    fn handle_input_player_air(
        &mut self,
        ent_id: EntityId,
        target_vel: Vec2,
        frame_time: Duration,
        player_vars: &PlayerVars,
    ) -> progs::Result<()> {
        let mut ent = self.world.get_mut(ent_id)?;
        let on_ground = ent.has_flag(EntityFlags::ON_GROUND)?;
        let current_velocity = ent.velocity()?;
        // Don't need to handle water acceleration here, this method is only for air+ground.
        let (max_speed, current_velocity, accel) = if on_ground {
            (player_vars.max_speed, current_velocity.with_z(0.), player_vars.ground_accel)
        } else {
            (player_vars.max_air_speed, current_velocity, player_vars.air_accel)
        };
        let accel = accel * duration_to_f32(frame_time);
        let target_vel =
            angle_vectors(ent.view_angle()?, target_vel.normalize_or_zero().extend(0.)) * max_speed;

        ent.set_velocity(player_accelerate(target_vel, current_velocity, accel))?;

        Ok(())
    }

    pub fn handle_input_player(
        &mut self,
        ent_id: EntityId,
        frame_time: Duration,
        registry: &Registry,
    ) -> progs::Result<()> {
        let player_vars: PlayerVars = registry.read_cvars()?;

        let mut ent = self.world.get(ent_id)?;
        let water_level = ent.water_level()?;

        let target_vel = ent.move_dir()?;

        self.player_categorize_position(ent_id)?;

        self.player_apply_friction(ent_id, frame_time, &player_vars)?;

        if water_level >= 2. {
            error!("TODO: SV_WaterMove");
        } else {
            self.handle_input_player_air(ent_id, target_vel.truncate(), frame_time, &player_vars)?;
        }

        self.player_categorize_position(ent_id)?;

        debug!("TODO: Player physics not fully implemented");
        Ok(())
    }

    pub fn physics_push(
        &mut self,
        ent_id: EntityId,
        frame_time: Duration,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> progs::Result<()> {
        let mut ent = self.world.get_mut(ent_id)?;

        let old_local_time = duration_from_f32(ent.load(FieldAddrFloat::LocalTime)?);
        let next_think = duration_from_f32(ent.load(FieldAddrFloat::NextThink)?);

        let new_local_time = old_local_time + frame_time;

        let move_time = if next_think < new_local_time {
            (next_think - old_local_time).max(Duration::ZERO)
        } else {
            frame_time
        };

        if move_time > Duration::ZERO {
            self.move_push(ent_id, move_time, registry.reborrow(), vfs)?;
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
            self.execute_program(think, registry, vfs)?;
        }

        Ok(())
    }

    pub fn physics_noclip(
        &mut self,
        ent_id: EntityId,
        frame_time: Duration,
        registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> progs::Result<()> {
        let mut ent = self.world.get_mut(ent_id)?;

        let frame_time_f = duration_to_f32(frame_time);

        let angles: Vec3 = ent.load(FieldAddrVector::Angles)?;
        let angle_vel: Vec3 = ent.load(FieldAddrVector::AngularVelocity)?;
        let new_angles = angles + frame_time_f * angle_vel;
        ent.store(FieldAddrVector::Angles, new_angles)?;

        let orig: Vec3 = ent.load(FieldAddrVector::Origin)?;
        let vel: Vec3 = ent.load(FieldAddrVector::Velocity)?;
        let new_orig = orig + frame_time_f * vel;
        ent.store(FieldAddrVector::Origin, new_orig)?;

        let local_time = duration_from_f32(ent.load(FieldAddrFloat::LocalTime)?);
        let next_think = duration_from_f32(ent.load(FieldAddrFloat::NextThink)?);

        let old_local_time = local_time;
        let new_local_time = duration_from_f32(ent.load(FieldAddrFloat::LocalTime)?);

        // Let the entity think if it needs to.
        if old_local_time < next_think && next_think <= new_local_time {
            // Deschedule thinking.
            ent.store(FieldAddrFloat::NextThink, 0.0)?;

            self.globals.put_float(duration_to_f32(self.time), GlobalAddrFloat::Time as i16)?;
            self.globals.put_entity_id(ent_id, GlobalAddrEntity::Self_ as i16)?;
            self.globals.put_entity_id(EntityId(0), GlobalAddrEntity::Other as i16)?;
            self.globals.store(GlobalAddrEntity::World, EntityId(0))?;

            self.think(ent_id, frame_time, registry, vfs)?;
        }

        Ok(())
    }

    pub fn physics_step(
        &mut self,
        ent_id: EntityId,
        frame_time: Duration,
        vfs: &Vfs,
        mut registry: Mut<Registry>,
    ) -> progs::Result<()> {
        let ServerVars { gravity, max_entity_velocity: max_velocity, .. } =
            registry.read_cvars()?;

        let in_freefall = !self
            .world
            .get(ent_id)?
            .flags()?
            .intersects(EntityFlags::ON_GROUND | EntityFlags::FLY | EntityFlags::IN_WATER);

        if in_freefall {
            let vel = self.world.get(ent_id)?.velocity()?;

            // If true, play an impact sound when the entity hits the ground.
            let hit_sound = vel.z < -0.1 * gravity;

            self.world.get_mut(ent_id)?.apply_gravity(&self.string_table, gravity, frame_time)?;

            self.world.get_mut(ent_id)?.limit_velocity(max_velocity)?;

            // Move the entity and relink it.
            self.move_ballistic(frame_time, ent_id, registry.reborrow(), vfs)?;
            self.link_entity(ent_id, true, registry.reborrow(), vfs)?;

            let mut ent = self.world.get_mut(ent_id)?;

            if ent.flags()?.contains(EntityFlags::ON_GROUND) && hit_sound {
                // Entity hit the ground this frame.
                if let Some(sound) = self.string_table.find("demon/dland2.wav") {
                    self.sound(ent_id, 0, sound, 1., 1.)?;
                }
            }
        }

        self.think(ent_id, frame_time, registry, vfs)?;

        debug!("TODO: SV_CheckWaterTransition");

        Ok(())
    }

    pub fn physics_toss(
        &mut self,
        ent_id: EntityId,
        frame_time: Duration,
        vfs: &Vfs,
        mut registry: Mut<Registry>,
        has_gravity: bool,
        overbounce: f32,
    ) -> progs::Result<()> {
        let ServerVars { gravity, max_entity_velocity: max_velocity, .. } =
            registry.read_cvars()?;

        if !self.think(ent_id, frame_time, registry.reborrow(), vfs)? {
            return Ok(());
        }

        let mut ent = self.world.get_mut(ent_id)?;

        if ent.velocity()?.z > 0. {
            ent.remove_flags(EntityFlags::ON_GROUND)?;
        }

        if ent.flags()?.intersects(EntityFlags::ON_GROUND) {
            return Ok(());
        }

        ent.limit_velocity(max_velocity)?;

        if has_gravity {
            ent.apply_gravity(&self.string_table, gravity, frame_time)?;
        }

        let dt = duration_to_f32(frame_time);

        let angles = ent.angles()?;
        let angular_velocity = ent.angular_velocity()?;
        let new_angles = angles + dt * angular_velocity;

        ent.set_angles(new_angles)?;

        let frame_velocity = ent.velocity()? * dt;

        let (trace, _) = self.push_entity(ent_id, frame_velocity, registry, vfs)?;

        if !self.world.exists(ent_id) {
            return Ok(());
        }

        if let Some(plane) = trace.plane() {
            let mut ent = self.world.get_mut(ent_id)?;
            let result = plane.clip_velocity(ent.velocity()?, overbounce);
            ent.set_velocity(result.metadata)?;
        } else {
            return Ok(());
        }

        debug!("TODO: SV_CheckWaterTransition");

        Ok(())
    }

    fn push_entity(
        &mut self,
        ent_id: EntityId,
        move_dir: Vec3,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
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

        self.link_entity(ent_id, true, registry.reborrow(), vfs)?;

        if let Some(hit) = hit {
            self.impact_entities(ent_id, hit, registry, vfs)?;
        }

        Ok((trace, hit))
    }

    fn move_push(
        &mut self,
        pusher_id: EntityId,
        move_time: Duration,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> progs::Result<()> {
        let mut pusher = self.world.get_mut(pusher_id)?;

        let mut moved = Vec::<(EntityId, Vec3)>::new();

        let move_amt = pusher.velocity()? * duration_to_f32(move_time);

        let pusher_bb = pusher.abs_bounding_box()?;
        let pusher_bb = Aabb3d {
            min: pusher_bb.min + Vec3A::from(move_amt),
            max: pusher_bb.max + Vec3A::from(move_amt),
        };

        let move_time_f32 = duration_to_f32(move_time);

        let pusher_origin = pusher.origin()?;

        let new_origin = pusher_origin + move_amt;
        let new_local_time = pusher.local_time()? + move_time_f32;
        pusher.set_local_time(new_local_time)?;

        if move_amt.length_squared() < f32::EPSILON {
            return Ok(());
        }

        debug!("Moving pusher by {move_amt}");
        pusher.set_origin(new_origin)?;

        self.link_entity(pusher_id, false, registry.reborrow(), vfs)?;

        // TODO: Don't need to collect here
        for other_id in self.world.iter().filter(|e| *e != pusher_id).collect::<Vec<_>>() {
            let mut other = self.world.get(other_id)?;
            let other_move_kind = other.move_kind()?;

            // If the other object is already stuck in something, don't try to push.
            if matches!(other_move_kind, MoveKind::Push | MoveKind::None | MoveKind::NoClip)
                || self.world.entity_is_stuck(other_id, |eid| eid != pusher_id)?
            {
                continue;
            }

            if !(other.has_flag(EntityFlags::ON_GROUND)? && other.ground()? == pusher_id) {
                let other_bb = other.abs_bounding_box()?;

                // If the entity doesn't collide with anything, we're done.
                if !other_bb.intersects(&pusher_bb)
                    || !self.world.entity_is_stuck(other_id, |_| true)?
                {
                    continue;
                }
            }

            let mut other = self.world.get_mut(other_id)?;
            let old_origin = other.origin()?;

            moved.push((other_id, old_origin));
            other.set_origin(old_origin + move_amt)?;

            if !self.world.entity_is_stuck(other_id, |_| true)? {
                debug!("Moved pushed by {move_amt}");
                self.link_entity(other_id, false, registry.reborrow(), vfs)?;
                continue;
            }

            self.world.get_mut(other_id)?.set_origin(old_origin)?;

            if !self.world.entity_is_stuck(other_id, |_| true)? {
                moved.pop();
                continue;
            }

            let mut other = self.world.get_mut(other_id)?;

            // Not sure why Quake only checks X here
            if other.min()?.x == other.max()?.x {
                continue;
            }

            // TODO: Why do we only check this here?
            let solid = other.solid()?;
            if solid == EntitySolid::Not || solid == EntitySolid::Trigger {
                // Quake says that this means the other object is a corpse.
                let mins = Vec3 { x: 0., y: 0., z: other.min()?.z };

                other.set_min(mins)?;
                other.set_max(mins)?;

                self.link_entity(other_id, false, registry.reborrow(), vfs)?;
                continue;
            }

            other.set_origin(old_origin)?;
            self.link_entity(other_id, true, registry.reborrow(), vfs)?;

            debug!("Resetting pusher origin");
            self.world.get_mut(pusher_id)?.set_origin(pusher_origin)?;
            self.link_entity(pusher_id, false, registry.reborrow(), vfs)?;

            let mut pusher = self.world.get_mut(pusher_id)?;
            let new_local_time = pusher.local_time()? - move_time_f32;
            pusher.set_local_time(new_local_time)?;

            let pusher_blocked_fn = pusher.load(FieldAddrFunctionId::Blocked)?;

            if !pusher_blocked_fn.is_none() {
                self.globals.store(GlobalAddrEntity::Self_, pusher_id)?;
                self.globals.store(GlobalAddrEntity::Other, other_id)?;
                self.globals.store(GlobalAddrEntity::World, EntityId(0))?;
                self.execute_program(pusher_blocked_fn, registry, vfs)?;
            }

            debug!("Resetting all positions");
            for (ent, original_position) in moved {
                self.world.get_mut(ent)?.set_origin(original_position)?;
            }

            return Ok(());
        }

        Ok(())
    }

    const MAX_BALLISTIC_COLLISIONS: usize = 4;

    /// Movement function for freefalling entities.
    pub fn move_ballistic(
        &mut self,
        sim_time: Duration,
        ent_id: EntityId,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> Result<CollisionResult<Option<Trace>>, ProgsError> {
        let mut sim_time_f = duration_to_f32(sim_time);

        let mut out = CollisionResult::default();
        let mut touching_planes: ArrayVec<Hyperplane, 5> = ArrayVec::new();

        let init_velocity = self.world.get(ent_id)?.velocity()?;
        let mut trace_velocity = init_velocity;

        // Even when the entity collides with something along its path, it may
        // continue moving. This may occur when bouncing or sliding off a solid
        // object, or when moving between media (e.g. from air to water).
        for _ in 0..Self::MAX_BALLISTIC_COLLISIONS {
            let velocity = self.world.get(ent_id)?.velocity()?;

            if velocity == Vec3::ZERO {
                // Not moving.
                break;
            }

            let orig = self.world.get(ent_id)?.origin()?;
            let end = orig + sim_time_f * velocity;
            let min = self.world.get(ent_id)?.min()?;
            let max = self.world.get(ent_id)?.max()?;

            let (trace, hit_entity) = self.world.trace_entity_move(
                ent_id,
                orig,
                min,
                max,
                end,
                CollideKind::Normal,
                |_| true,
            )?;

            if trace.all_solid() {
                // Entity is stuck in a wall.
                self.world.get_mut(ent_id)?.store(FieldAddrVector::Velocity, Vec3::ZERO)?;

                return Ok(CollisionResult { floor: true, wall_or_step: true, ..out });
            }

            if trace.ratio() > 0.0 {
                // If the entity moved at all, update its position.
                self.world.get_mut(ent_id)?.store(FieldAddrVector::Origin, trace.end_point())?;
                touching_planes.clear();

                trace_velocity = self.world.get(ent_id)?.velocity()?;
            }

            // Find the plane the entity hit, if any.
            let boundary = match trace.end().kind() {
                // Entity didn't hit anything.
                TraceEndKind::Terminal => break,
                TraceEndKind::Boundary(b) => b,
            };

            // Sanity check to make sure the trace actually hit something.
            let hit_entity = match hit_entity {
                Some(h) => h,
                None => panic!("trace collided with nothing"),
            };

            // TODO: magic constant
            if boundary.plane.is_floor() {
                out.floor = true;
                if self.world.get(hit_entity)?.solid()? == EntitySolid::Bsp {
                    self.world.get_mut(ent_id)?.add_flags(EntityFlags::ON_GROUND)?;
                    self.world.get_mut(ent_id)?.store(FieldAddrEntityId::Ground, hit_entity)?;
                }
            } else if boundary.plane.normal().z == 0.0 {
                out.wall_or_step = true;
                out.metadata = Some(trace.clone());
            }

            self.impact_entities(ent_id, hit_entity, registry.reborrow(), vfs)?;
            if !self.world.exists(ent_id) {
                // Entity removed by touch function.
                break;
            }

            sim_time_f -= trace.ratio() * sim_time_f;

            if touching_planes.try_push(boundary.plane.clone()).is_err() {
                // Touching too many planes to make much sense of, so stop.
                self.world.get_mut(ent_id)?.set_velocity(Vec3::ZERO)?;
                return Ok(CollisionResult { floor: true, wall_or_step: true, ..out });
            }

            let end_velocity = {
                let move_clip =
                    Hyperplane::clip_velocity_to_many(&touching_planes, trace_velocity, 1.0);
                if move_clip.dead_stop {
                    return Ok(move_clip.map(|_| out.metadata));
                }

                move_clip.metadata
            };

            if init_velocity.dot(end_velocity) <= 0.0 {
                // Avoid bouncing the entity at a sharp angle.
                self.world.get_mut(ent_id)?.set_velocity(Vec3::ZERO)?;
                return Ok(out);
            }

            self.world.get_mut(ent_id)?.store(FieldAddrVector::Velocity, end_velocity)?;
        }

        Ok(out)
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
        vfs: &Vfs,
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

        let actual_dist = (trace.end_point() - origin).length();

        if collide_entity.is_none()
            || (actual_dist - Self::DROP_TO_FLOOR_DIST).abs() < f32::EPSILON
            || trace.all_solid()
        {
            // Entity didn't hit the floor or is stuck.
            Ok(false)
        } else {
            // Entity hit the floor. Update origin, relink and set ON_GROUND flag.
            self.world.get_mut(ent_id)?.set_origin(trace.end_point())?;
            self.link_entity(ent_id, false, registry, vfs)?;
            let mut ent = self.world.get_mut(ent_id)?;
            ent.add_flags(EntityFlags::ON_GROUND)?;
            ent.set_ground(collide_entity.unwrap())?;

            Ok(true)
        }
    }

    pub fn touch_triggers(
        &mut self,
        ent_id: EntityId,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
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
                self.execute_program(trigger_touch, registry.reborrow(), vfs)?;
            }

            // Restore state.
            self.globals.store(GlobalAddrEntity::Self_, restore_self)?;
            self.globals.store(GlobalAddrEntity::Other, restore_other)?;

            Ok(())
        })?;

        Ok(())
    }

    fn run_impact_callbacks(
        &mut self,
        ent_a_id: EntityId,
        ent_b_id: EntityId,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> progs::Result<()> {
        let restore_self = self.globals.load(GlobalAddrEntity::Self_)?;
        let restore_other = self.globals.load(GlobalAddrEntity::Other)?;

        let mut ent_a = self.world.get(ent_a_id)?;
        let touch_a = ent_a.load(FieldAddrFunctionId::Touch)?;
        let solid_a = ent_a.solid()?;

        let mut ent_b = self.world.get(ent_b_id)?;
        let touch_b = ent_b.load(FieldAddrFunctionId::Touch)?;
        let solid_b = ent_b.solid()?;

        if !touch_a.is_none() && solid_a != EntitySolid::Not {
            self.globals.store(GlobalAddrEntity::Self_, ent_a_id)?;
            self.globals.store(GlobalAddrEntity::Other, ent_b_id)?;
            self.execute_program(touch_a, registry.reborrow(), vfs)?;
        }

        if !touch_b.is_none() && solid_b != EntitySolid::Not {
            self.globals.store(GlobalAddrEntity::Self_, ent_b_id)?;
            self.globals.store(GlobalAddrEntity::Other, ent_a_id)?;
            self.execute_program(touch_b, registry.reborrow(), vfs)?;
        }

        // Restore state.
        self.globals.store(GlobalAddrEntity::Self_, restore_self)?;
        self.globals.store(GlobalAddrEntity::Other, restore_other)?;

        Ok(())
    }

    /// Runs two entities' touch functions.
    pub fn impact_entities(
        &mut self,
        ent_a: EntityId,
        ent_b: EntityId,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
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
            self.execute_program(touch_a, registry.reborrow(), vfs)?;
        }

        // Set up and run Entity B's touch function.
        let touch_b = self.world.get(ent_b)?.load(FieldAddrFunctionId::Touch)?;
        let solid_b = self.world.get(ent_b)?.solid()?;
        if touch_b.0 != 0 && solid_b != EntitySolid::Not {
            self.globals.store(GlobalAddrEntity::Self_, ent_b)?;
            self.globals.store(GlobalAddrEntity::Other, ent_a)?;
            self.globals.store(GlobalAddrEntity::World, EntityId(0))?;
            self.execute_program(touch_b, registry.reborrow(), vfs)?;
        }

        self.globals.store(GlobalAddrEntity::Self_, restore_self)?;
        self.globals.store(GlobalAddrEntity::Other, restore_other)?;
        self.globals.store(GlobalAddrEntity::World, EntityId(0))?;

        Ok(())
    }

    pub fn sound(
        &mut self,
        entity: EntityId,
        channel: i8,
        sound: StringId,
        volume: f32,
        attenuation: f32,
    ) -> progs::Result<()> {
        let volume = (volume * 255.) as _;

        let Some(sound_id) = self.sound_id(sound) else {
            error!("Cannot find sound {} in precache", self.string_table.get(sound).unwrap());
            return Ok(());
        };

        let position = self.world.get(entity)?.load(FieldAddrVector::Origin)?;

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
            self.world.ent_fld_addr_from_i32(self.globals.get_entity_field(dst_ent_fld_addr)?);

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
            self.world.ent_fld_addr_from_i32(self.globals.get_entity_field(dst_ent_fld_addr)?);
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
            self.world.ent_fld_addr_from_i32(self.globals.get_entity_field(dst_ent_fld_addr)?);
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
            self.world.ent_fld_addr_from_i32(self.globals.get_entity_field(dst_ent_fld_addr)?);
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
            self.world.ent_fld_addr_from_i32(self.globals.get_entity_field(dst_ent_fld_addr)?);
        self.world
            .get_mut(ent_fld_addr.entity_id)?
            .put_function_id(f, ent_fld_addr.field_addr.0 as i16)?;

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

    pub fn builtin_err(&mut self, err_kind: impl fmt::Display) -> progs::Result<()> {
        let msg = self.globals.string_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let msg = self.string_table.get(msg).unwrap();
        self.cx.print_backtrace(&self.string_table, false);
        Err(ProgsError::with_msg(format!(
            "{} in {}: {}",
            err_kind,
            self.string_table
                .get(self.cx.function_def(self.cx.current_function()).unwrap().name_id)
                .unwrap(),
            msg
        )))
    }

    pub fn builtin_set_origin(&mut self, registry: Mut<Registry>, vfs: &Vfs) -> progs::Result<()> {
        let e_id = self.globals.get_entity_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let origin = self.globals.get_vector(GLOBAL_ADDR_ARG_1 as i16)?;
        self.set_entity_origin(e_id, origin, registry, vfs)?;

        Ok(())
    }

    pub fn builtin_set_model(&mut self) -> progs::Result<()> {
        let ent_id = self.globals.get_entity_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let model_name_id = self.globals.string_id(GLOBAL_ADDR_ARG_1 as i16)?;
        self.set_entity_model(ent_id, model_name_id)?;

        Ok(())
    }

    pub fn builtin_set_size(&mut self) -> progs::Result<()> {
        let e_id = self.globals.get_entity_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let mins = self.globals.get_vector(GLOBAL_ADDR_ARG_1 as i16)?;
        let maxs = self.globals.get_vector(GLOBAL_ADDR_ARG_2 as i16)?;
        self.world.set_entity_size(e_id, mins, maxs)?;

        Ok(())
    }

    // TODO: move to Globals
    pub fn builtin_random(&mut self) -> progs::Result<()> {
        let random = rand::random::<f32>().clamp(f32::EPSILON, 1. - f32::EPSILON);
        self.globals.put_float(random, GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    pub fn builtin_spawn(&mut self, registry: Mut<Registry>, vfs: &Vfs) -> progs::Result<()> {
        let ent_id = self.spawn_entity(registry, vfs)?;
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
        self.precache_sound(s_id);
        self.globals.put_string_id(s_id, GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    pub fn builtin_precache_model(&mut self, vfs: &Vfs) -> progs::Result<()> {
        // TODO: disable precaching after server is active
        let s_id = self.globals.string_id(GLOBAL_ADDR_ARG_0 as i16)?;
        if self.model_id(s_id).is_none() {
            self.precache_model(s_id);
            self.world.add_model(vfs, &self.string_table, s_id)?;
        }

        self.globals.put_string_id(s_id, GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    pub fn builtin_stuffcmd(&mut self) -> progs::Result<()> {
        let ent_id = self.globals.get_entity_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let s_id = self.globals.string_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let text = self.string_table.get(s_id).unwrap().into_owned();

        ServerCmd::StuffText { text }
            .serialize(self.frame_client_messages.entry(ent_id).or_default())?;

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

    pub fn builtin_sprint(&mut self) -> progs::Result<()> {
        let strs = &self.string_table;
        let s_id = self.globals.string_id(GLOBAL_ADDR_ARG_1 as i16)?;
        let text = strs.get(s_id).unwrap().into_owned();
        let mut dest = self.write_dest()?;

        ServerCmd::Print { text }.serialize(&mut dest)?;

        Ok(())
    }

    pub fn builtin_dprint(&mut self) -> progs::Result<()> {
        let strs = &self.string_table;
        let s_id = self.globals.string_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let string = strs.get(s_id).unwrap();
        debug!("DPRINT: {string}");

        Ok(())
    }

    pub fn builtin_ftos(&mut self) -> progs::Result<()> {
        let f = self.globals.get_float(GLOBAL_ADDR_ARG_0 as i16)?;
        // TODO: This leaks data in the string table! We should have a separate temp string area.
        let out = self.string_table.find_or_insert(format!("{f:5.1}"));

        self.globals.put_string_id(out, GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    pub fn builtin_vtos(&mut self) -> progs::Result<()> {
        let vec = self.globals.get_vector(GLOBAL_ADDR_ARG_0 as i16)?;
        // TODO: This leaks data in the string table! We should have a separate temp string area.
        let out = self
            .string_table
            .find_or_insert(format!("{:5.1} {:5.1} {:5.1}", vec[0], vec[1], vec[2]));

        self.globals.put_string_id(out, GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    pub fn builtin_drop_to_floor(
        &mut self,
        registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> progs::Result<()> {
        let ent_id = self.globals.get_entity_id(GlobalAddrEntity::Self_ as i16)?;
        let hit_floor = self.drop_entity_to_floor(ent_id, registry, vfs)?;
        self.globals.put_float(hit_floor as u8 as f32, GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    pub fn builtin_light_style(&mut self) -> progs::Result<()> {
        let index = match self.globals.get_float(GLOBAL_ADDR_ARG_0 as i16)? as i32 {
            i if i < 0 => return Err(ProgsError::with_msg("negative lightstyle ID")),
            i => i as usize,
        };
        let val = self.globals.string_id(GLOBAL_ADDR_ARG_1 as i16)?;
        self.set_lightstyle(index, val);

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

        let contents = self.world.contents_at_point(pos);
        self.globals.put_float(-(contents as usize as f32), GLOBAL_ADDR_RETURN as _)?;

        Ok(())
    }

    pub fn builtin_aim(&mut self) -> progs::Result<()> {
        const AIM_Z: f32 = 20.;

        let player_id = self.globals.get_entity_id(GLOBAL_ADDR_ARG_0 as _)?;
        let _speed = self.globals.get_float(GLOBAL_ADDR_ARG_1 as _)?;

        let mut player = self.world.get(player_id)?;

        let _start = player.origin()? + AIM_Z * Vec3::Z;

        // TODO: This always just emulates the `noaim` client option, we should implement aim
        // properly.
        self.globals
            .put_vector(angle_vectors(player.view_angle()?, Vec3::X), GLOBAL_ADDR_RETURN as _)?;

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

        // TODO: Quake specifically only clamps when the move amount is >0/<0 respectively but this
        // almost certainly isn't necessary.
        let angle_delta = angle_delta.clamp(-speed, speed);

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

        registry.set_cvar(var.to_str(), val.to_str()).unwrap();
        debug!("{} = {}", var, val);

        Ok(())
    }

    pub fn builtin_ambient_sound(&mut self) -> progs::Result<()> {
        let origin = self.globals.get_vector(GLOBAL_ADDR_ARG_0 as i16)?;
        let sample = self.globals.string_id(GLOBAL_ADDR_ARG_1 as i16)?;
        let volume = (self.globals.get_float(GLOBAL_ADDR_ARG_2 as i16)? * 255.) as _;
        let attenuation = (self.globals.get_float(GLOBAL_ADDR_ARG_3 as i16)? * 255.) as _;

        let Some(sound_id) = self.sound_id(sample) else {
            error!("Cannot find sound {} in precache", self.string_table.get(sample).unwrap());
            return Ok(());
        };

        ServerCmd::SpawnStaticSound { origin, sound_id: sound_id as _, volume, attenuation }
            .serialize(&mut self.broadcast)?;

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

        let field = FieldAddrStringId::from_usize(field.0).unwrap();

        for ent in self.world.range((Bound::Excluded(entity.0.max(0) as usize), Bound::Unbounded)) {
            let Ok(field) = self.world.get(ent)?.load(field) else {
                debug!("No field {} on {:?}", field, ent);
                continue;
            };

            let Some(s) = self.string_table.get(field) else {
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

    pub fn builtin_walk_move(&mut self, registry: Mut<Registry>, vfs: &Vfs) -> progs::Result<()> {
        let yaw = self.globals.get_float(GLOBAL_ADDR_ARG_0 as i16)?;
        let dist = self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)?;

        let this_id = self.globals.load(GlobalAddrEntity::Self_)?;
        let mut this = self.world.get(this_id)?;
        let ent_flags = this.flags()?;

        if !ent_flags.intersects(EntityFlags::ON_GROUND | EntityFlags::SWIM | EntityFlags::FLY) {
            self.globals.put_float(0., GLOBAL_ADDR_RETURN as i16)?;
            return Ok(());
        }

        let move_dir = angle_vectors(Vec3::Y * yaw, Vec3::X * dist);

        let func = self.cx.current_function();

        let did_move = self.move_step(this_id, move_dir, true, registry, vfs)?;
        self.globals.put_float(if did_move { 1. } else { 0. }, GLOBAL_ADDR_RETURN as i16)?;

        // Restore after `move_step`.
        self.cx.set_current_function(func);
        self.globals.store(GlobalAddrEntity::Self_, this_id)?;
        self.globals.store(GlobalAddrEntity::World, EntityId(0))?;

        Ok(())
    }

    pub fn builtin_check_client(&mut self) -> progs::Result<()> {
        debug!("TODO: PF_checkclient");
        self.globals.put_entity_id(EntityId(1), GLOBAL_ADDR_RETURN as i16)?;

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
        let model_id = self.model_id(model_name).ok_or_else(|| {
            ProgsError::with_msg(format!(
                "Model not found in precache: {:?}",
                self.string_table.get(model_name)
            ))
        })? as _;

        ServerCmd::SpawnStatic { model_id, frame_id, colormap, skin_id, origin, angles }
            .serialize(&mut self.broadcast)?;

        self.world.free(ent)?;

        Ok(())
    }

    pub fn builtin_sound(&mut self) -> progs::Result<()> {
        let entity = self.globals.get_entity_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let channel = self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)? as _;
        let sample = self.globals.string_id(GLOBAL_ADDR_ARG_2 as i16)?;
        let volume = self.globals.get_float(GLOBAL_ADDR_ARG_3 as i16)?;
        let attenuation = self.globals.get_float(GLOBAL_ADDR_ARG_4 as i16)?;

        if let Some(sample_name) = self.string(sample) {
            debug!(
                "Playing sound on {entity}.{channel}: {sample_name} vol {volume} attn {attenuation}"
            );
        }

        self.sound(entity, channel, sample, volume, attenuation)?;

        Ok(())
    }

    pub fn builtin_write_byte(&mut self) -> progs::Result<()> {
        let val = self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)? as u8;
        let mut dest = self.write_dest()?;
        dest.write_u8(val)?;

        Ok(())
    }

    pub fn builtin_write_char(&mut self) -> progs::Result<()> {
        let val = self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)? as i8;
        let mut dest = self.write_dest()?;
        dest.write_i8(val)?;
        Ok(())
    }

    pub fn builtin_write_short(&mut self) -> progs::Result<()> {
        let val = self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)? as i16;
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
        let val = (self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)? * 8.) as i16;
        let mut dest = self.write_dest()?;
        dest.write_i16::<LittleEndian>(val)?;
        Ok(())
    }

    pub fn builtin_write_angle(&mut self) -> progs::Result<()> {
        let val = (self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)? * 256. / 360.) as u8;
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
            CollideKind::default(),
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
                CollideKind::default(),
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
        vfs: &Vfs,
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
                // TODO: This doesn't need to be hard-coded to +- 8, and it doesn't need to be
                // hard-coded to target the       point 30-40 units above the origin
                // of the target.
                match old_origin.z - self.world.get(ent_id)?.origin()?.z {
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
                if ent_flags.contains(EntityFlags::SWIM) && !trace.in_water {
                    // Swimming monster left water
                    return Ok(false);
                }

                debug!("{} -> {}", old_origin, trace.end_point());
                self.world.get_mut(ent_id)?.set_origin(trace.end_point())?;

                if relink {
                    self.link_entity(ent_id, true, registry, vfs)?;
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
        vfs: &Vfs,
    ) -> progs::Result<bool> {
        let server_vars = registry.read_cvars::<ServerVars>()?;

        let mut ent = self.world.get(ent_id)?;
        let ent_flags = ent.flags()?;

        let min = ent.min()?;
        let max = ent.max()?;
        let old_origin = ent.origin()?;

        if ent_flags.intersects(EntityFlags::SWIM | EntityFlags::FLY) {
            return self.move_step_fly(ent_id, move_dir, relink, registry, vfs);
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
                    self.link_entity(ent_id, true, registry, vfs)?;
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
                    self.link_entity(ent_id, true, registry, vfs)?;
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
            self.link_entity(ent_id, true, registry, vfs)?;
        }

        Ok(true)
    }

    fn step_direction(
        &mut self,
        ent_id: EntityId,
        yaw: f32,
        dist: f32,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> progs::Result<bool> {
        let mut ent = self.world.get_mut(ent_id)?;
        let old_origin = ent.origin()?;
        ent.set_ideal_yaw(yaw)?;
        self.change_yaw(ent_id)?;

        let move_dir = Vec2::from_angle(yaw.to_radians()).extend(0.) * dist;
        let did_move = self.move_step(ent_id, move_dir, false, registry.reborrow(), vfs)?;

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

        self.link_entity(ent_id, true, registry, vfs)?;

        Ok(did_move)
    }

    fn new_chase_dir(
        &mut self,
        this_id: EntityId,
        goal_id: EntityId,
        dist: f32,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> progs::Result<()> {
        let server_vars: ServerVars = registry.read_cvars()?;

        let mut this = self.world.get(this_id)?;
        let old_dir = angle_mod((this.ideal_yaw()? / 45.).floor() * 45.);
        let turn_around = angle_mod(old_dir - 180.);

        let this_origin = this.origin()?;

        let mut goal = self.world.get(goal_id)?;

        let goal_origin = goal.origin()?;

        let delta_x = goal_origin.x - this_origin.x;
        let delta_y = goal_origin.y - this_origin.y;

        let dir_a = match delta_x {
            10f32.. => Some(0f32),
            ..-10f32 => Some(180f32),
            _ => None,
        };
        let dir_b = match delta_y {
            ..-10f32 => Some(90f32),
            10f32.. => Some(270f32),
            _ => None,
        };

        let turn_dir = match (dir_a, dir_b) {
            (Some(0.), Some(90.)) => Some(45.),
            (Some(0.), Some(_)) => Some(315.),
            (Some(_), Some(90.)) => Some(135.),
            (Some(_), Some(_)) => Some(215.),
            _ => None,
        };

        if let Some(turn_dir) = turn_dir
            && (turn_dir - turn_around).abs() > f32::EPSILON
            && self.step_direction(this_id, turn_dir, dist, registry.reborrow(), vfs)?
        {
            return Ok(());
        }

        let (dir_a, dir_b) = if delta_y.abs() > delta_x.abs() && rand::random_bool(0.5) {
            (dir_b, turn_dir)
        } else {
            (dir_a, dir_b)
        };

        if let Some(dir_a) = dir_a
            && (dir_a - turn_around).abs() > f32::EPSILON
            && self.step_direction(this_id, dir_a, dist, registry.reborrow(), vfs)?
        {
            return Ok(());
        }

        if let Some(dir_b) = dir_b
            && (dir_b - turn_around).abs() > f32::EPSILON
            && self.step_direction(this_id, dir_b, dist, registry.reborrow(), vfs)?
        {
            return Ok(());
        }

        const SEARCH_DIRS: [f32; 8] = [0., 45., 90., 135., 180., 225., 270., 315.];
        const SEARCH_DIRS_REV: [f32; 8] = [315., 270., 225., 180., 135., 90., 45., 0.];

        let search_dirs = if rand::random_bool(0.5) { SEARCH_DIRS } else { SEARCH_DIRS_REV };

        for turn_dir in search_dirs {
            if (turn_dir - turn_around).abs() > f32::EPSILON
                && self.step_direction(this_id, turn_dir, dist, registry.reborrow(), vfs)?
            {
                return Ok(());
            }
        }

        if self.step_direction(this_id, turn_around, dist, registry.reborrow(), vfs)? {
            return Ok(());
        }

        self.world.get_mut(this_id)?.set_ideal_yaw(old_dir)?;

        if !self.check_bottom(this_id, &server_vars)? {
            self.world.get_mut(this_id)?.add_flags(EntityFlags::PARTIAL_GROUND)?;
        }

        Ok(())
    }

    pub fn builtin_move_to_goal(
        &mut self,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> progs::Result<()> {
        let dist = self.globals.get_float(GLOBAL_ADDR_ARG_0 as i16)?;
        let this_id = self.globals.load(GlobalAddrEntity::Self_)?;
        let mut this = self.world.get(this_id)?;
        let goal_id = this.goal()?;

        let flags = this.flags()?;

        if !flags.intersects(EntityFlags::ON_GROUND | EntityFlags::FLY | EntityFlags::SWIM) {
            self.globals.put_float(0., GLOBAL_ADDR_RETURN as i16)?;
            return Ok(());
        }

        if !this.enemy()?.is_none() && self.close_enough(this_id, goal_id, dist)? {
            self.globals.put_float(1., GLOBAL_ADDR_RETURN as i16)?;
            return Ok(());
        }

        let ideal_yaw = this.ideal_yaw()?;
        let did_move = self.step_direction(this_id, ideal_yaw, dist, registry.reborrow(), vfs)?;

        if !did_move || rand::random_bool(0.25) {
            self.new_chase_dir(this_id, goal_id, dist, registry.reborrow(), vfs)?;
        }

        self.globals.put_float(if did_move { 1. } else { 0. }, GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    pub fn builtin_center_print(&mut self) -> progs::Result<()> {
        let text = self.globals.string_id(GLOBAL_ADDR_ARG_1 as i16)?;
        let text = self.string_table.get(text).unwrap().into_owned();
        let mut dest = self.write_dest()?;

        ServerCmd::CenterPrint { text }.serialize(&mut dest)?;

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
        self.globals.put_vector(
            trace.plane().map(|plane| plane.normal()).unwrap_or(Vec3::ZERO),
            GlobalAddrVector::TracePlaneNormal as _,
        )?;
        self.globals.put_float(
            trace.plane_dist().unwrap_or_default(),
            GlobalAddrFloat::TracePlaneDist as _,
        )?;
        self.globals
            .put_entity_id(hit_ent.unwrap_or(EntityId(-1)), GlobalAddrEntity::TraceEntity as _)?;

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
    use crate::common::{
        console::CmdName,
        net::{
            self, ClientCmd, ClientMessage, GameType, ItemFlags, PlayerData, ServerMessage,
            SignOnStage,
        },
    };

    use super::*;

    pub fn recv_client_messages(
        mut server: Option<ResMut<Session>>,
        mut client_msgs: MessageReader<ClientMessage>,
        mut server_messages: MessageWriter<ServerMessage>,

        mut run_cmds: MessageWriter<RunCmd<'static>>,
        mut registry: ResMut<Registry>,
        vfs: Res<Vfs>,
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

                                match &*name {
                                    "prespawn" => {
                                        // TODO: Error handling
                                        assert!(args.is_empty());

                                        let Some(server) = &mut server else {
                                            continue;
                                        };

                                        server
                                            .clientcmd_prespawn(
                                                client_id,
                                                registry.reborrow(),
                                                &vfs,
                                            )
                                            .unwrap();

                                        ServerCmd::SignOnStage { stage: SignOnStage::ClientInfo }
                                            .serialize(&mut out_packet)
                                            .unwrap();
                                    }
                                    "name" => {
                                        // TODO: Error handling
                                        assert!(args.len() == 1);

                                        let Some(server) = &mut server else {
                                            continue;
                                        };

                                        server
                                            .clientcmd_name(
                                                client_id,
                                                args.iter().next().unwrap().to_owned().into(),
                                            )
                                            .unwrap();
                                    }
                                    "color" => {
                                        assert!(args.len() == 2);

                                        warn!("TODO: Set color");
                                    }
                                    "spawn" => {
                                        let Some(server) = &mut server else {
                                            continue;
                                        };

                                        server.clientcmd_spawn(client_id).unwrap();

                                        ServerCmd::SignOnStage { stage: SignOnStage::Begin }
                                            .serialize(&mut out_packet)
                                            .unwrap();
                                    }
                                    "begin" => {
                                        // TODO: Error handling
                                        assert!(args.is_empty());

                                        let Some(server) = &mut server else {
                                            continue;
                                        };

                                        server
                                            .clientcmd_begin(client_id, registry.reborrow(), &vfs)
                                            .unwrap();

                                        let client_ent =
                                            server.client(client_id).unwrap().entity().unwrap();

                                        // TODO: Error handling
                                        ServerCmd::SetView { ent_id: client_ent.0 as _ }
                                            .serialize(&mut out_packet)
                                            .unwrap();

                                        ServerCmd::SignOnStage { stage: SignOnStage::Done }
                                            .serialize(&mut out_packet)
                                            .unwrap();
                                    }
                                    _ => {
                                        run_cmds.write(
                                            RunCmd(CmdName { name, trigger }, args).into_owned(),
                                        );
                                        // HACK: We need to wait a frame to process messages.
                                        return;
                                    }
                                }
                            }
                        }
                        ClientCmd::Move {
                            delta_time,
                            angles,
                            fwd_move,
                            side_move,
                            up_move,
                            button_flags,
                            impulse,
                        } => {
                            let Some(Session { persist, level, .. }) = server.as_deref_mut() else {
                                continue;
                            };

                            if let Some(ent_id) = persist
                                .client(client_id)
                                .filter(|c| matches!(c.state, ClientState::Active(_)))
                                .and_then(|c| c.entity())
                            {
                                let mut entity = level.world.get_mut(ent_id).ok().unwrap();

                                entity
                                    .set_move_dir(Vec3::new(
                                        fwd_move as f32,
                                        side_move as f32,
                                        up_move as f32,
                                    ))
                                    .unwrap();
                                entity.set_view_angle(angles).unwrap();
                                entity.set_buttons(button_flags).unwrap();
                                entity.set_impulse(impulse as _).unwrap();

                                level.handle_input_player(ent_id, delta_time, &registry).unwrap();
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
            // TODO: Should not hard-code client id 0
            server_messages.write(ServerMessage { client_id: 0, packet: out_packet.into() });
        }
    }

    pub fn server_spawn(
        mut server: ResMut<Session>,
        mut registry: ResMut<Registry>,
        mut server_messages: MessageWriter<ServerMessage>,
        time: Res<Time<Fixed>>,
        vfs: Res<Vfs>,
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
                &vfs,
            )?;
        }

        server.state = SessionState::Active;

        let teamplay = registry.get_cvar("teamplay").and_then(|t| t.value().as_name());

        // Match string with `starts_with` so we can handle `?GameName`
        let game_type = match teamplay {
            Some(t) if t.starts_with("0") => GameType::Deathmatch,
            Some(t) if t.starts_with("1") || t.starts_with("2") => GameType::CoOp,
            // Invalid game type, default to DM
            _ => GameType::Deathmatch,
        };

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
        vfs: Res<Vfs>,
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
                match level.physics(&persist.client_slots, time.delta(), registry.reborrow(), &vfs)
                {
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

            // TODO: Stop hardcoding `8` for max players
            for client_id in persist
                .client_slots
                .active_clients()
                .map(|(id, _)| id)
                .collect::<ArrayVec<usize, 8>>()
            {
                let mut packet = Vec::new();

                if let Some(messages) = persist
                    .client_slots
                    .get(client_id)
                    .unwrap()
                    .entity()
                    .and_then(|ent| level.frame_client_messages.get(&ent))
                {
                    packet.extend_from_slice(messages);
                }

                ServerCmd::Time { time: seismon_utils::duration_to_f32(level.time) }
                    .serialize(&mut packet)
                    .unwrap();

                for entity_id in &level.new_entities {
                    if level.is_networked(*entity_id)
                        && let Some(state) = level.entity_state(*entity_id)
                    {
                        state.spawn_baseline(entity_id.0 as _).serialize(&mut packet).unwrap();
                        level.world.get_mut(*entity_id).unwrap().baseline = state;
                    }
                }

                // Skip world entity
                for ent in level.world.range(1..) {
                    // TODO: Handle deletions
                    let Ok(mut entity) = level.world.get(ent) else {
                        continue;
                    };

                    let update = entity.state().unwrap().make_update(ent.0 as _, &entity.baseline);

                    ServerCmd::FastUpdate(update).serialize(&mut packet).unwrap();
                }

                if let Some(mut entity) = persist
                    .client(client_id)
                    .and_then(|c| c.entity())
                    .and_then(|ent_id| level.world.get_mut(ent_id).ok())
                {
                    if entity.get_bool(FieldAddrFloat::FixAngle as i16).unwrap() {
                        ServerCmd::SetAngle { angles: entity.angles().unwrap() }
                            .serialize(&mut packet)
                            .unwrap();
                        entity.put_float(0., FieldAddrFloat::FixAngle as i16).unwrap();
                    }

                    let view_ofs = entity.view_ofs().unwrap();
                    let velocity = entity.velocity().unwrap();
                    let ideal_pitch = entity.ideal_pitch().unwrap();
                    let punch_angle = entity.punch_angle().unwrap();
                    let items = entity.items().unwrap();
                    let weapon_frame = entity.weapon_frame().unwrap();
                    let ground = entity.ground().unwrap();
                    let water_level = entity.water_level().unwrap();
                    let armor = entity.armor().unwrap();
                    let health = entity.health().unwrap();
                    let ammo = entity.ammo().unwrap();
                    let ammo_shells = entity.ammo_shells().unwrap();
                    let ammo_nails = entity.ammo_nails().unwrap();
                    let ammo_rockets = entity.ammo_rockets().unwrap();
                    let ammo_cells = entity.ammo_cells().unwrap();
                    let active_weapon = entity.active_weapon().unwrap();
                    let weapon_name = entity.weapon_model_name().unwrap();
                    let weapon_model_id = if weapon_name.is_none() {
                        None
                    } else {
                        level.model_id(weapon_name).map(|m| m as _)
                    };

                    ServerCmd::PlayerData(PlayerData {
                        on_ground: !ground.is_none(),
                        in_water: water_level != 0.,
                        view_height: Some(view_ofs.z),
                        ideal_pitch: Some(ideal_pitch),
                        punch_pitch: Some(punch_angle.x),
                        velocity_x: Some(velocity.x),
                        punch_yaw: Some(punch_angle.y),
                        velocity_y: Some(velocity.x),
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
                    .serialize(&mut packet)
                    .unwrap()
                }

                // We add broadcast packets at the end to ensure that entities can spawn before
                // broadcasted events related to those entities
                packet.extend_from_slice(&level.broadcast);

                server_messages.write(ServerMessage { client_id, packet: packet.into() });
            }
        }
    }
}
