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

mod commands;
mod cvars;
pub mod precache;
pub mod progs;
pub mod world;

use std::{fmt, io::Write, ops::Bound};

use crate::{
    common::{
        console::{Registry, RunCmd},
        engine::{self, duration_from_f32, duration_to_f32},
        math::Hyperplane,
        model::Model,
        net::{EntityState, ServerCmd},
        parse,
        util::QString,
        vfs::Vfs,
    },
    server::{
        progs::{GlobalAddrFunction, functions::FunctionKind},
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
            GLOBAL_ADDR_ARG_4, GLOBAL_ADDR_RETURN, GlobalAddr as _,
        },
    },
    world::{
        EntityFlags, EntitySolid, FieldAddrFloat, FieldAddrFunctionId, FieldAddrStringId, World,
        phys::{self, CollideKind, CollisionFlags, Trace, TraceEndKind},
    },
};

use arrayvec::ArrayVec;
use bevy::prelude::*;
use bitflags::bitflags;
use byteorder::{LittleEndian, WriteBytesExt as _};
use chrono::Duration;
use failure::bail;
use hashbrown::{HashMap, HashSet};
use num::FromPrimitive;
use serde::Deserialize;
use snafu::{Backtrace, Report};

const MAX_LIGHTSTYLES: usize = 256;

// macro_rules! debug {
//     ($($val:tt)*) => { error!($($val)*) }
// }

#[derive(Default, Copy, Clone)]
pub struct SeismonServerPlugin;

impl Plugin for SeismonServerPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(
            FixedUpdate,
            (
                systems::recv_client_messages,
                systems::server_update,
                systems::server_spawn.pipe(
                    |In(res), mut commands: Commands, mut runcmd: EventWriter<RunCmd<'static>>| {
                        if let Err(e) = res {
                            error!("Failed spawning server: {}", Report::from_error(e));
                            commands.remove_resource::<Session>();
                            runcmd.write("startdemos".into());
                        }
                    },
                ),
            )
                .run_if(resource_exists::<Session>),
        );

        commands::register_commands(app);
        cvars::register_cvars(app);
    }
}

#[derive(Debug)]
pub struct Client {
    name: QString,
    color: u8,
    state: ClientState,
    // TODO: Per-client send
    buffer: Vec<u8>,
}

impl Default for Client {
    fn default() -> Self {
        Self {
            name: "player".into(),
            color: 0,
            state: ClientState::Connecting,
            buffer: default(),
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
    privileged: bool,

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
        self.slots
            .iter()
            .enumerate()
            .filter(|(_, v)| v.is_some())
            .map(|(i, _)| i)
    }

    pub fn active_clients(&self) -> impl Iterator<Item = usize> + '_ {
        self.connected_clients()
            .filter(|&i| matches!(self.get(i).map(|c| &c.state), Some(ClientState::Active(_))))
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
    flags: SessionFlags,
}

impl SessionPersistent {
    pub fn new(max_clients: usize) -> SessionPersistent {
        SessionPersistent {
            client_slots: ClientSlots::new(max_clients),
            flags: SessionFlags::empty(),
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

    #[inline]
    pub fn client(&self, slot: usize) -> Option<&Client> {
        self.persist.client(slot)
    }

    #[inline]
    pub fn client_mut(&mut self, slot: usize) -> Option<&mut Client> {
        self.persist.client_mut(slot)
    }

    pub fn new_client(&mut self) -> Option<&mut Client> {
        self.persist.client_slots.find_available()
    }

    pub fn clientcmd_prespawn(&mut self, slot: usize) -> Result<(), failure::Error> {
        self.new_client().unwrap();

        // TODO: Actually run prespawn routines

        Ok(())
    }

    pub fn clientcmd_name(&mut self, slot: usize, name: QString) -> Result<(), failure::Error> {
        let Some(client) = self.persist.client_mut(slot) else {
            bail!("No such client {}", slot);
        };

        ServerCmd::UpdateName {
            player_id: slot as _,
            new_name: name.clone(),
        }
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

        let Some(client) = self.client_mut(slot) else {
            bail!("No such client {}", slot);
        };

        // TODO: All players are currently privileged
        client.state = ClientState::Active(ClientActive {
            privileged: true,
            entity_id: client_entity,
        });

        self.level
            .globals
            .store(GlobalAddrEntity::Self_, client_entity)?;
        self.level
            .globals
            .store(GlobalAddrFloat::Time, duration_to_f32(self.level.time))?;

        let client_connect = self
            .level
            .globals
            .function_id(GlobalAddrFunction::ClientConnect as i16)?;
        self.level
            .execute_program(client_connect, registry.reborrow(), vfs)?;

        self.level
            .globals
            .store(GlobalAddrEntity::Self_, client_entity)?;
        self.level
            .globals
            .store(GlobalAddrFloat::Time, duration_to_f32(self.level.time))?;

        let put_client_in_server = self
            .level
            .globals
            .function_id(GlobalAddrFunction::PutClientInServer as i16)?;
        self.level
            .execute_program(put_client_in_server, registry.reborrow(), vfs)?;

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
        if let SessionState::Loading = self.state {
            true
        } else {
            false
        }
    }

    #[inline]
    fn level(&self) -> &LevelState {
        &self.level
    }

    #[inline]
    fn level_mut(&mut self) -> &mut LevelState {
        &mut self.level
    }

    #[inline]
    pub fn sound_id(&self, name_id: StringId) -> Option<usize> {
        self.level().sound_id(name_id)
    }

    #[inline]
    pub fn model_id(&self, name_id: StringId) -> Option<usize> {
        self.level().model_id(name_id)
    }

    #[inline]
    pub fn set_lightstyle(&mut self, index: usize, val: StringId) {
        self.level_mut().set_lightstyle(index, val);
    }

    /// Returns the amount of time the current level has been active.
    #[inline]
    pub fn time(&self) -> Option<Duration> {
        match self.state {
            SessionState::Loading => None,
            SessionState::Active => Some(self.level.time),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Deserialize)]
pub struct ServerVars {
    #[serde(rename(deserialize = "sv_gravity"))]
    gravity: f32,
    #[serde(rename(deserialize = "sv_maxvelocity"))]
    max_velocity: f32,
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

    broadcast: Vec<u8>,
}

impl LevelState {
    pub fn new(
        map_path: String,
        progs: LoadProgs,
        models: Vec<Model>,
        entmap: String,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> LevelState {
        let LoadProgs {
            cx,
            globals,
            entity_def,
            mut string_table,
        } = progs;

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
        let entity_list = parse::entities(&entmap).unwrap();

        let mut level = LevelState {
            string_table,
            sound_precache,
            model_precache,
            lightstyles: [StringId(0); MAX_LIGHTSTYLES],
            time: Duration::zero(),
            new_entities: default(),
            cx,
            globals,
            world,

            broadcast: default(),
        };

        for entity in entity_list {
            if let Err(e) = level.spawn_entity_from_map(entity, registry.reborrow(), vfs) {
                error!("Failed spawning entity {}", e);
            }
        }

        level
    }

    #[inline]
    pub fn precache_sound(&mut self, name_id: StringId) {
        self.sound_precache
            .precache(self.string_table.get(name_id).unwrap().to_str());
    }

    #[inline]
    pub fn precache_model(&mut self, name_id: StringId) {
        self.model_precache
            .precache(self.string_table.get(name_id).unwrap().to_str())
    }

    #[inline]
    pub fn sound_id(&self, name_id: StringId) -> Option<usize> {
        self.sound_precache
            .find(self.string_table.get(name_id).unwrap().to_str())
    }

    #[inline]
    pub fn model_id(&self, name_id: StringId) -> Option<usize> {
        self.model_precache
            .find(self.string_table.get(name_id).unwrap().to_str())
    }

    #[inline]
    pub fn set_lightstyle(&mut self, index: usize, val: StringId) {
        self.lightstyles[index] = val;
    }

    #[inline]
    pub fn entity_state(&self, id: EntityId) -> Option<EntityState> {
        self.world.entities.get(id)?.state(&self.world.type_def)
    }

    #[inline]
    pub fn spawn_baseline(&self, id: EntityId) -> Option<ServerCmd> {
        self.entity_state(id)
            .map(|state| state.spawn_baseline(id.0 as _))
    }

    /// Execute a QuakeC function in the VM.
    pub fn execute_program(
        &mut self,
        f: FunctionId,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> Result<(), ProgsError> {
        use Opcode::*;

        let mut runaway = 10000;

        let exit_depth = self.cx.call_stack_depth();

        self.cx
            .enter_function(&self.string_table, &mut self.globals, f)?;

        while self.cx.call_stack_depth() != exit_depth {
            runaway -= 1;

            if runaway == 0 {
                self.cx.print_backtrace(&self.string_table);
                return Err(ProgsError::LocalStackOverflow {
                    backtrace: Backtrace::capture(),
                });
            }

            let statement = self.cx.load_statement();
            let op = statement.opcode;
            let a = statement.arg1;
            let b = statement.arg2;
            let c = statement.arg3;

            debug!("{:<12} {:>5} {:>5} {:>5}", op.to_string(), a, b, c);

            // Y'all like jump tables?
            match op {
                // Control flow ================================================
                If => {
                    let cond = self
                        .globals
                        .get_float(a)
                        .map(|f| f as usize)
                        .or_else(|_| self.globals.entity_id(a).map(|e| e.0))?
                        != 0;
                    debug!("{}: cond == {}", op, cond);

                    if cond {
                        self.cx.jump_relative(b);
                        continue;
                    }
                }

                IfNot => {
                    let cond = self
                        .globals
                        .get_float(a)
                        .map(|f| f as usize)
                        .or_else(|_| self.globals.entity_id(a).map(|e| e.0))?
                        != 0;
                    debug!("{}: cond == {}", op, cond);

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
                    let f_to_call = self.globals.function_id(a)?;

                    if f_to_call.0 == 0 {
                        return Err(ProgsError::with_msg("NULL function"));
                    }

                    let Ok(def) = self.cx.function_def(f_to_call) else {
                        return Err(ProgsError::with_msg("NULL function"));
                    };

                    macro_rules! todo_builtin {
                        ($id:ident) => {{
                            self.cx.print_backtrace(&self.string_table);
                            panic!(concat!("TODO: ", stringify!($id)))
                        }};
                    }

                    let name_id = def.name_id;

                    debug!(
                        "Calling function {} ({:?})",
                        self.string_table.get(name_id).unwrap(),
                        f_to_call
                    );

                    let called_with_args = op as usize - Call0 as usize;
                    if def.argc != called_with_args {
                        self.cx.print_backtrace(&self.string_table);
                        let func_name = self.string_table.get(name_id).unwrap();
                        warn!(
                            "Arg count mismatch calling {}: expected {}, found {}",
                            func_name, def.argc, called_with_args,
                        );
                    }

                    if let FunctionKind::BuiltIn(b) = def.kind {
                        use progs::functions::BuiltinFunctionId::*;
                        match b {
                            MakeVectors => self.globals.make_vectors()?,
                            SetOrigin => self.builtin_set_origin(registry.reborrow(), vfs)?,
                            SetModel => self.builtin_set_model()?,
                            SetSize => self.builtin_set_size()?,
                            Break => todo_builtin!(Break),
                            Random => self.globals.builtin_random()?,
                            Sound => self.builtin_sound()?,
                            Normalize => self.builtin_normalize()?,
                            Error => self.builtin_err("Error")?,
                            ObjError => self.builtin_err("Object error")?,
                            VLen => self.globals.builtin_v_len()?,
                            VecToYaw => self.globals.builtin_vec_to_yaw()?,
                            Spawn => self.builtin_spawn(registry.reborrow(), vfs)?,
                            Remove => self.builtin_remove()?,
                            TraceLine => self.builtin_trace_line()?,
                            CheckClient => self.builtin_check_client()?,
                            Find => self.builtin_find()?,
                            PrecacheSound => self.builtin_precache_sound()?,
                            PrecacheModel => self.builtin_precache_model(vfs)?,
                            StuffCmd => todo_builtin!(StuffCmd),
                            FindRadius => todo_builtin!(FindRadius),
                            BPrint => self.builtin_bprint()?,
                            SPrint => self.builtin_sprint()?,
                            DPrint => self.builtin_dprint()?,
                            FToS => todo_builtin!(FToS),
                            VToS => self.builtin_vtos()?,
                            CoreDump => todo_builtin!(CoreDump),
                            TraceOn => todo_builtin!(TraceOn),
                            TraceOff => todo_builtin!(TraceOff),
                            EPrint => todo_builtin!(EPrint),
                            WalkMove => self.builtin_walk_move(registry.reborrow(), vfs)?,

                            DropToFloor => self.builtin_drop_to_floor(registry.reborrow(), vfs)?,
                            LightStyle => self.builtin_light_style()?,
                            RInt => self.globals.builtin_r_int()?,
                            Floor => self.globals.builtin_floor()?,
                            Ceil => self.globals.builtin_ceil()?,
                            CheckBottom => todo_builtin!(CheckBottom),
                            PointContents => todo_builtin!(PointContents),
                            FAbs => self.globals.builtin_f_abs()?,
                            Aim => todo_builtin!(Aim),
                            Cvar => self.builtin_cvar(&*registry)?,
                            LocalCmd => todo_builtin!(LocalCmd),
                            NextEnt => todo_builtin!(NextEnt),
                            Particle => todo_builtin!(Particle),
                            ChangeYaw => todo_builtin!(ChangeYaw),
                            VecToAngles => todo_builtin!(VecToAngles),
                            WriteByte => self.builtin_write_byte()?,
                            WriteChar => self.builtin_write_char()?,
                            WriteShort => self.builtin_write_short()?,
                            WriteLong => self.builtin_write_long()?,
                            WriteCoord => self.builtin_write_coord()?,
                            WriteAngle => self.builtin_write_angle()?,
                            WriteString => self.builtin_write_string()?,
                            WriteEntity => self.builtin_write_entity()?,
                            MoveToGoal => todo_builtin!(MoveToGoal),
                            // Only used in `qcc`, does nothing at runtime
                            PrecacheFile => {}
                            MakeStatic => self.builtin_make_static()?,
                            ChangeLevel => todo_builtin!(ChangeLevel),
                            CvarSet => self.builtin_cvar_set(registry.reborrow())?,
                            CenterPrint => self.builtin_center_print()?,
                            AmbientSound => self.builtin_ambient_sound()?,
                            // PrecacheModel2/PrecacheSound2 only differ for `qcc`, not at runtime
                            PrecacheModel2 => self.builtin_precache_model(vfs)?,
                            PrecacheSound2 => self.builtin_precache_sound()?,
                            // Only used in `qcc`, does nothing at runtime
                            PrecacheFile2 => {}
                            SetSpawnArgs => todo_builtin!(SetSpawnArgs),
                        }
                        debug!(
                            "Returning from built-in function {}",
                            self.string_table.get(name_id).unwrap()
                        );
                    } else {
                        self.cx
                            .enter_function(&self.string_table, &mut self.globals, f_to_call)?;
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
                EqS => self.globals.op_eq_s(a, b, c)?,
                EqEnt => self.globals.op_eq_ent(a, b, c)?,
                EqFnc => self.globals.op_eq_fnc(a, b, c)?,
                NeF => self.globals.op_ne_f(a, b, c)?,
                NeV => self.globals.op_ne_v(a, b, c)?,
                NeS => self.globals.op_ne_s(a, b, c)?,
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
    ) -> Result<(), ProgsError>
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
    ) -> Result<(), ProgsError> {
        self.world.link_entity(ent_id)?;

        if touch_triggers {
            self.touch_triggers(ent_id, registry, vfs)?;
        }

        Ok(())
    }

    pub fn spawn_entity(
        &mut self,
        registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> Result<EntityId, ProgsError> {
        let ent_id = self.world.alloc_uninitialized()?;

        self.link_entity(ent_id, false, registry, vfs)?;

        self.new_entities.insert(ent_id);

        Ok(ent_id)
    }

    pub fn spawn_entity_from_map(
        &mut self,
        map: HashMap<&str, &str>,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> Result<EntityId, ProgsError> {
        let classname = match map.get("classname") {
            Some(c) => c.to_owned(),
            None => return Err(ProgsError::with_msg("No classname for entity")),
        };

        let ent_id = self.world.alloc_from_map(&mut self.string_table, map)?;

        // TODO: set origin, mins and maxs here if needed

        // set `self` before calling spawn function
        self.globals
            .put_entity_id(ent_id, GlobalAddrEntity::Self_ as i16)?;

        self.execute_program_by_name(classname, registry.reborrow(), vfs)?;

        self.link_entity(ent_id, true, registry, vfs)?;

        Ok(ent_id)
    }

    pub fn set_entity_origin(
        &mut self,
        ent_id: EntityId,
        origin: Vec3,
        registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> Result<(), ProgsError> {
        self.world.entities.get_mut(ent_id)?.store(
            &self.world.type_def,
            FieldAddrVector::Origin,
            origin.into(),
        )?;
        self.link_entity(ent_id, false, registry, vfs)?;

        Ok(())
    }

    pub fn set_entity_model(
        &mut self,
        ent_id: EntityId,
        model_name_id: StringId,
    ) -> Result<(), ProgsError> {
        let model_id = {
            let ent = self.world.entities.get_mut(ent_id)?;

            ent.put_string_id(
                &self.world.type_def,
                model_name_id,
                FieldAddrStringId::ModelName as i16,
            )?;

            let model_id = match self.string_table.get(model_name_id) {
                Some(name) => match self.model_precache.find(name.to_str()) {
                    Some(i) => i,
                    None => return Err(ProgsError::with_msg("model not precached")),
                },
                None => return Err(ProgsError::with_msg("invalid StringId")),
            };

            ent.put_float(
                &self.world.type_def,
                model_id as f32,
                FieldAddrFloat::ModelIndex as i16,
            )?;

            model_id
        };

        self.world.set_entity_model(ent_id, model_id)?;

        Ok(())
    }

    pub fn think(
        &mut self,
        ent_id: EntityId,
        frame_time: Duration,
        registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> Result<(), ProgsError> {
        let ent = self.world.entities.get_mut(ent_id)?;
        let think_time =
            duration_from_f32(ent.load(&self.world.type_def, FieldAddrFloat::NextThink)?);

        if think_time <= Duration::zero() || think_time > self.time + frame_time {
            // Think either already happened or isn't due yet.
            return Ok(());
        }

        // Deschedule next think.
        ent.store(&self.world.type_def, FieldAddrFloat::NextThink, 0.0)?;

        // Call entity's think function.
        let think = ent.load(&self.world.type_def, FieldAddrFunctionId::Think)?;
        self.globals
            .store(GlobalAddrFloat::Time, duration_to_f32(think_time))?;
        self.globals.store(GlobalAddrEntity::Self_, ent_id)?;
        self.globals.store(GlobalAddrEntity::Other, EntityId(0))?;
        self.execute_program(think, registry, vfs)?;

        Ok(())
    }

    pub fn start_frame(
        &mut self,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> Result<(), ProgsError> {
        self.cx.reset();

        self.globals.store(GlobalAddrEntity::Self_, EntityId(0))?;
        self.globals.store(GlobalAddrEntity::Other, EntityId(0))?;
        self.globals
            .store(GlobalAddrFloat::Time, duration_to_f32(self.time))?;

        let start_frame = self
            .globals
            .function_id(GlobalAddrFunction::StartFrame as i16)?;
        self.execute_program(start_frame, registry.reborrow(), vfs)?;

        Ok(())
    }

    pub fn physics(
        &mut self,
        clients: &ClientSlots,
        frame_time: Duration,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> Result<(), ProgsError> {
        self.start_frame(registry.reborrow(), vfs)?;

        let server_vars = registry.read_cvars::<ServerVars>()?;

        for ent_id in self.world.entities.list() {
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
                self.link_entity(ent_id, true, registry.reborrow(), vfs)?;
            }

            let max_clients = clients.limit();
            if ent_id.0 != 0 && ent_id.0 < max_clients {
                self.physics_player(clients, ent_id, &server_vars)?;
            } else {
                match self
                    .world
                    .entities
                    .try_get(ent_id)?
                    .move_kind(&self.world.type_def)?
                {
                    MoveKind::Walk => {
                        debug!("TODO: MoveKind::Walk");
                    }

                    MoveKind::Push => {
                        self.physics_push(ent_id, frame_time, registry.reborrow(), vfs)?
                    }
                    MoveKind::NoClip => {
                        self.physics_noclip(ent_id, frame_time, registry.reborrow(), vfs)?
                    }
                    MoveKind::Step => {
                        self.physics_step(ent_id, frame_time, vfs, registry.reborrow())?
                    }
                    // No actual physics for this entity, but still let it think.
                    MoveKind::None => self.think(ent_id, frame_time, registry.reborrow(), vfs)?,

                    MoveKind::AngleNoClip
                    | MoveKind::AngleClip
                    | MoveKind::Fly
                    | MoveKind::Toss
                    | MoveKind::FlyMissile
                    | MoveKind::Bounce => {
                        warn!("TODO: Airborne physics");
                        self.think(ent_id, frame_time, registry.reborrow(), vfs)?;
                    }
                }
            }

            match self.globals.load(GlobalAddrFloat::ForceRetouch)? {
                f if f > 0.0 => self.globals.store(GlobalAddrFloat::ForceRetouch, f - 1.0)?,
                _ => (),
            }
        }

        self.time += frame_time;

        Ok(())
    }

    pub fn physics_player(
        &mut self,
        clients: &ClientSlots,
        ent_id: EntityId,
        server_vars: &ServerVars,
    ) -> Result<(), ProgsError> {
        let client_id = ent_id.0.checked_sub(1).ok_or_else(|| {
            ProgsError::with_msg(format!("Invalid client entity ID: {:?}", ent_id))
        })?;

        if clients.get(client_id).is_none() {
            // No client in this slot.
            return Ok(());
        }

        let ent = self.world.entities.get_mut(ent_id)?;
        ent.limit_velocity(&self.world.type_def, server_vars.max_velocity)?;
        debug!("TODO: Player physics not fully implemented");
        Ok(())
    }

    pub fn physics_push(
        &mut self,
        ent_id: EntityId,
        frame_time: Duration,
        registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> Result<(), ProgsError> {
        let ent = self.world.entities.get_mut(ent_id)?;

        let local_time =
            duration_from_f32(ent.load(&self.world.type_def, FieldAddrFloat::LocalTime)?);
        let next_think =
            duration_from_f32(ent.load(&self.world.type_def, FieldAddrFloat::NextThink)?);

        let move_time = if local_time + frame_time > next_think {
            (next_think - local_time).max(Duration::zero())
        } else {
            frame_time
        };

        if !move_time.is_zero() {
            self.move_push(ent_id, frame_time, move_time)?;
        }

        let ent = self.world.entities.get_mut(ent_id)?;

        let old_local_time = local_time;
        let new_local_time =
            duration_from_f32(ent.load(&self.world.type_def, FieldAddrFloat::LocalTime)?);

        // Let the entity think if it needs to.
        if old_local_time < next_think && next_think <= new_local_time {
            // Deschedule thinking.
            ent.store(
                &self.world.type_def,
                FieldAddrFloat::NextThink,
                engine::duration_to_f32(new_local_time),
            )?;

            self.globals
                .put_float(duration_to_f32(self.time), GlobalAddrFloat::Time as i16)?;
            self.globals
                .put_entity_id(ent_id, GlobalAddrEntity::Self_ as i16)?;
            self.globals
                .put_entity_id(EntityId(0), GlobalAddrEntity::Other as i16)?;

            self.think(ent_id, frame_time, registry, vfs)?;
        }

        Ok(())
    }

    pub fn physics_noclip(
        &mut self,
        ent_id: EntityId,
        frame_time: Duration,
        registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> Result<(), ProgsError> {
        let ent = self.world.entities.get_mut(ent_id)?;

        let frame_time_f = duration_to_f32(frame_time);

        let angles: Vec3 = ent
            .load(&self.world.type_def, FieldAddrVector::Angles)?
            .into();
        let angle_vel: Vec3 = ent
            .load(&self.world.type_def, FieldAddrVector::AngularVelocity)?
            .into();
        let new_angles = angles + frame_time_f * angle_vel;
        ent.store(
            &self.world.type_def,
            FieldAddrVector::Angles,
            new_angles.into(),
        )?;

        let orig: Vec3 = ent
            .load(&self.world.type_def, FieldAddrVector::Origin)?
            .into();
        let vel: Vec3 = ent
            .load(&self.world.type_def, FieldAddrVector::Velocity)?
            .into();
        let new_orig = orig + frame_time_f * vel;
        ent.store(
            &self.world.type_def,
            FieldAddrVector::Origin,
            new_orig.into(),
        )?;

        let local_time =
            duration_from_f32(ent.load(&self.world.type_def, FieldAddrFloat::LocalTime)?);
        let next_think =
            duration_from_f32(ent.load(&self.world.type_def, FieldAddrFloat::NextThink)?);

        let old_local_time = local_time;
        let new_local_time =
            duration_from_f32(ent.load(&self.world.type_def, FieldAddrFloat::LocalTime)?);

        // Let the entity think if it needs to.
        if old_local_time < next_think && next_think <= new_local_time {
            // Deschedule thinking.
            ent.store(
                &self.world.type_def,
                FieldAddrFloat::NextThink,
                engine::duration_to_f32(new_local_time),
            )?;

            self.globals
                .put_float(duration_to_f32(self.time), GlobalAddrFloat::Time as i16)?;
            self.globals
                .put_entity_id(ent_id, GlobalAddrEntity::Self_ as i16)?;
            self.globals
                .put_entity_id(EntityId(0), GlobalAddrEntity::Other as i16)?;

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
    ) -> Result<(), ProgsError> {
        let ServerVars {
            gravity,
            max_velocity,
        } = registry.read_cvars()?;

        let in_freefall = !self
            .world
            .entities
            .try_get(ent_id)?
            .flags(&self.world.type_def)?
            .intersects(EntityFlags::ON_GROUND | EntityFlags::FLY | EntityFlags::IN_WATER);

        if in_freefall {
            let vel: Vec3 = self
                .world
                .entities
                .try_get(ent_id)?
                .load(&self.world.type_def, FieldAddrVector::Velocity)?
                .into();

            // If true, play an impact sound when the entity hits the ground.
            let hit_sound = vel.z < -0.1 * gravity;

            self.world.entities.get_mut(ent_id)?.apply_gravity(
                &self.world.type_def,
                &self.string_table,
                gravity,
                frame_time,
            )?;

            self.world
                .entities
                .get_mut(ent_id)?
                .limit_velocity(&self.world.type_def, max_velocity)?;

            // Move the entity and relink it.
            self.move_ballistic(frame_time, ent_id, registry.reborrow(), vfs)?;
            self.link_entity(ent_id, true, registry.reborrow(), vfs)?;

            let ent = self.world.entities.get_mut(ent_id)?;

            if ent
                .flags(&self.world.type_def)?
                .contains(EntityFlags::ON_GROUND)
                && hit_sound
            {
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

    pub fn move_push(
        &mut self,
        ent_id: EntityId,
        frame_time: Duration,
        move_time: Duration,
    ) -> Result<(), ProgsError> {
        let ent = self.world.entities.get_mut(ent_id)?;

        let vel: Vec3 = ent
            .load(&self.world.type_def, FieldAddrVector::Velocity)?
            .into();
        if vel == Vec3::ZERO {
            // Entity doesn't need to move.
            let local_time = ent.load(&self.world.type_def, FieldAddrFloat::LocalTime)?;
            let new_local_time = local_time + duration_to_f32(move_time);
            ent.store(
                &self.world.type_def,
                FieldAddrFloat::LocalTime,
                new_local_time,
            )?;
            return Ok(());
        }

        let move_time_f = duration_to_f32(move_time);
        let move_vector = vel * move_time_f;
        // TODO let mins =
        // todo!()
        error!("TODO: `move_push`");
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
    ) -> Result<(CollisionFlags, Option<Trace>), ProgsError> {
        let mut sim_time_f = duration_to_f32(sim_time);

        let mut out_trace = None;
        let mut flags = CollisionFlags::empty();
        let mut touching_planes: ArrayVec<Hyperplane, 5> = ArrayVec::new();

        let init_velocity = self
            .world
            .entities
            .try_get(ent_id)?
            .velocity(&self.world.type_def)?;
        let mut trace_velocity = init_velocity;

        // Even when the entity collides with something along its path, it may
        // continue moving. This may occur when bouncing or sliding off a solid
        // object, or when moving between media (e.g. from air to water).
        for _ in 0..Self::MAX_BALLISTIC_COLLISIONS {
            let velocity = self
                .world
                .entities
                .try_get(ent_id)?
                .velocity(&self.world.type_def)?;

            if velocity == Vec3::ZERO {
                // Not moving.
                break;
            }

            let orig = self
                .world
                .entities
                .try_get(ent_id)?
                .origin(&self.world.type_def)?;
            let end = orig + sim_time_f * velocity;
            let min = self
                .world
                .entities
                .try_get(ent_id)?
                .min(&self.world.type_def)?;
            let max = self
                .world
                .entities
                .try_get(ent_id)?
                .max(&self.world.type_def)?;

            let (trace, hit_entity) =
                self.world
                    .trace_entity_move(ent_id, orig, min, max, end, CollideKind::Normal)?;

            if trace.all_solid() {
                // Entity is stuck in a wall.
                self.world.entities.get_mut(ent_id)?.store(
                    &self.world.type_def,
                    FieldAddrVector::Velocity,
                    Vec3::ZERO.into(),
                )?;

                return Ok((CollisionFlags::HORIZONTAL | CollisionFlags::VERTICAL, None));
            }

            if trace.ratio() > 0.0 {
                // If the entity moved at all, update its position.
                self.world.entities.get_mut(ent_id)?.store(
                    &self.world.type_def,
                    FieldAddrVector::Origin,
                    trace.end_point().into(),
                )?;
                touching_planes.clear();

                trace_velocity = self
                    .world
                    .entities
                    .try_get(ent_id)?
                    .velocity(&self.world.type_def)?;
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
            if boundary.plane.normal().z > 0.7 {
                flags |= CollisionFlags::HORIZONTAL;
                if self
                    .world
                    .entities
                    .try_get(hit_entity)?
                    .solid(&self.world.type_def)?
                    == EntitySolid::Bsp
                {
                    self.world
                        .entities
                        .get_mut(ent_id)?
                        .add_flags(&self.world.type_def, EntityFlags::ON_GROUND)?;
                    self.world.entities.get_mut(ent_id)?.store(
                        &self.world.type_def,
                        FieldAddrEntityId::Ground,
                        hit_entity,
                    )?;
                }
            } else if boundary.plane.normal().z == 0.0 {
                flags |= CollisionFlags::VERTICAL;
                out_trace = Some(trace.clone());
            }

            self.impact_entities(ent_id, hit_entity, registry.reborrow(), vfs)?;
            if !self.world.entities.exists(ent_id) {
                // Entity removed by touch function.
                break;
            }

            sim_time_f -= trace.ratio() * sim_time_f;

            if touching_planes.try_push(boundary.plane.clone()).is_err() {
                // Touching too many planes to make much sense of, so stop.
                self.world.entities.get_mut(ent_id)?.store(
                    &self.world.type_def,
                    FieldAddrVector::Velocity,
                    Vec3::ZERO.into(),
                )?;
                return Ok((CollisionFlags::HORIZONTAL | CollisionFlags::VERTICAL, None));
            }

            let end_velocity =
                match phys::velocity_after_multi_collision(trace_velocity, &touching_planes, 1.0) {
                    Some(v) => v,
                    None => {
                        // Entity is wedged in a corner, so it simply stops.
                        self.world.entities.get_mut(ent_id)?.store(
                            &self.world.type_def,
                            FieldAddrVector::Velocity,
                            Vec3::ZERO.into(),
                        )?;

                        return Ok((
                            CollisionFlags::HORIZONTAL
                                | CollisionFlags::VERTICAL
                                | CollisionFlags::STOPPED,
                            None,
                        ));
                    }
                };

            if init_velocity.dot(end_velocity) <= 0.0 {
                // Avoid bouncing the entity at a sharp angle.
                self.world.entities.get_mut(ent_id)?.store(
                    &self.world.type_def,
                    FieldAddrVector::Velocity,
                    Vec3::ZERO.into(),
                )?;
                return Ok((flags, out_trace));
            }

            self.world.entities.get_mut(ent_id)?.store(
                &self.world.type_def,
                FieldAddrVector::Velocity,
                end_velocity.into(),
            )?;
        }

        Ok((flags, out_trace))
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
    ) -> Result<bool, ProgsError> {
        debug!("Finding floor for entity with ID {}", ent_id.0);
        let origin = self
            .world
            .entities
            .try_get(ent_id)?
            .origin(&self.world.type_def)?;

        let end = Vec3::new(origin.x, origin.y, origin.z - Self::DROP_TO_FLOOR_DIST);
        let min = self
            .world
            .entities
            .try_get(ent_id)?
            .min(&self.world.type_def)?;
        let max = self
            .world
            .entities
            .try_get(ent_id)?
            .max(&self.world.type_def)?;

        let (trace, collide_entity) =
            self.world
                .trace_entity_move(ent_id, origin, min, max, end, CollideKind::Normal)?;
        debug!("End position after drop: {:?}", trace.end_point());

        let drop_dist = 256.0;
        let actual_dist = (trace.end_point() - origin).length();

        if collide_entity.is_none() || actual_dist == drop_dist || trace.all_solid() {
            // Entity didn't hit the floor or is stuck.
            Ok(false)
        } else {
            // Entity hit the floor. Update origin, relink and set ON_GROUND flag.
            self.world.entities.get_mut(ent_id)?.put_vector(
                &self.world.type_def,
                trace.end_point().into(),
                FieldAddrVector::Origin as i16,
            )?;
            self.link_entity(ent_id, false, registry, vfs)?;
            self.world
                .entities
                .get_mut(ent_id)?
                .add_flags(&self.world.type_def, EntityFlags::ON_GROUND)?;
            self.world.entities.get_mut(ent_id)?.put_entity_id(
                &self.world.type_def,
                collide_entity.unwrap(),
                FieldAddrEntityId::Ground as i16,
            )?;

            Ok(true)
        }
    }

    pub fn touch_triggers(
        &mut self,
        ent_id: EntityId,
        mut registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> Result<(), ProgsError> {
        // TODO: alloc once
        let mut touched = Vec::new();
        self.world.list_touched_triggers(&mut touched, ent_id, 0)?;

        // Save state.
        let restore_self = self.globals.load(GlobalAddrEntity::Self_)?;
        let restore_other = self.globals.load(GlobalAddrEntity::Other)?;

        // Activate the touched triggers.
        for trigger_id in touched {
            let trigger_touch = self
                .world
                .entities
                .try_get(trigger_id)?
                .load(&self.world.type_def, FieldAddrFunctionId::Touch)?;

            self.globals.store(GlobalAddrEntity::Self_, trigger_id)?;
            self.globals.store(GlobalAddrEntity::Other, ent_id)?;
            self.execute_program(trigger_touch, registry.reborrow(), vfs)?;
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
    ) -> Result<(), ProgsError> {
        let restore_self = self.globals.load(GlobalAddrEntity::Self_)?;
        let restore_other = self.globals.load(GlobalAddrEntity::Other)?;

        self.globals
            .store(GlobalAddrFloat::Time, duration_to_f32(self.time))?;

        // Set up and run Entity A's touch function.
        let touch_a = self
            .world
            .entities
            .try_get(ent_a)?
            .load(&self.world.type_def, FieldAddrFunctionId::Touch)?;
        let solid_a = self
            .world
            .entities
            .try_get(ent_a)?
            .solid(&self.world.type_def)?;
        if touch_a.0 != 0 && solid_a != EntitySolid::Not {
            self.globals.store(GlobalAddrEntity::Self_, ent_a)?;
            self.globals.store(GlobalAddrEntity::Other, ent_b)?;
            self.execute_program(touch_a, registry.reborrow(), vfs)?;
        }

        // Set up and run Entity B's touch function.
        let touch_b = self
            .world
            .entities
            .try_get(ent_b)?
            .load(&self.world.type_def, FieldAddrFunctionId::Touch)?;
        let solid_b = self
            .world
            .entities
            .try_get(ent_b)?
            .solid(&self.world.type_def)?;
        if touch_b.0 != 0 && solid_b != EntitySolid::Not {
            self.globals.store(GlobalAddrEntity::Self_, ent_b)?;
            self.globals.store(GlobalAddrEntity::Other, ent_a)?;
            self.execute_program(touch_b, registry.reborrow(), vfs)?;
        }

        self.globals.store(GlobalAddrEntity::Self_, restore_self)?;
        self.globals.store(GlobalAddrEntity::Other, restore_other)?;

        Ok(())
    }

    pub fn sound(
        &mut self,
        entity: EntityId,
        channel: i8,
        sound: StringId,
        volume: f32,
        attenuation: f32,
    ) -> Result<(), ProgsError> {
        let volume = (volume * 255.) as _;

        let Some(sound_id) = self.sound_id(sound) else {
            error!(
                "Cannot find sound {} in precache",
                self.string_table.get(sound).unwrap()
            );
            return Ok(());
        };

        let position = self
            .world
            .entities
            .try_get(entity)?
            .load(&self.world.type_def, FieldAddrVector::Origin)?;

        ServerCmd::Sound {
            volume: Some(volume),
            attenuation: Some(attenuation),
            entity_id: entity.0 as _,
            channel,
            sound_id: sound_id as _,
            position: position.into(),
        }
        .serialize(&mut self.broadcast)?;

        Ok(())
    }

    // QuakeC instructions ====================================================

    pub fn op_return(&mut self, a: i16, b: i16, c: i16) -> Result<(), ProgsError> {
        let val1 = self.globals.get_bytes(a)?;
        let val2 = self.globals.get_bytes(b)?;
        let val3 = self.globals.get_bytes(c)?;

        self.globals.put_bytes(val1, GLOBAL_ADDR_RETURN as i16)?;
        self.globals
            .put_bytes(val2, GLOBAL_ADDR_RETURN as i16 + 1)?;
        self.globals
            .put_bytes(val3, GLOBAL_ADDR_RETURN as i16 + 2)?;

        debug!(
            "Returning from quakec function {}",
            self.string_table
                .get(
                    self.cx
                        .function_def(self.cx.current_function())
                        .unwrap()
                        .name_id
                )
                .unwrap()
        );

        self.cx
            .leave_function(&self.string_table, &mut self.globals)?;

        Ok(())
    }

    // LOAD_F: load float field from entity
    pub fn op_load_f(&mut self, e_ofs: i16, e_f: i16, dest_ofs: i16) -> Result<(), ProgsError> {
        let ent_id = self.globals.entity_id(e_ofs)?;

        let fld_ofs = self.globals.get_field_addr(e_f)?;

        let f = self
            .world
            .entities
            .try_get(ent_id)?
            .get_float(&self.world.type_def, fld_ofs.0 as i16)?;
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
    ) -> Result<(), ProgsError> {
        let ent_id = self.globals.entity_id(ent_id_addr)?;
        let ent_vector = self.globals.get_field_addr(ent_vector_addr)?;
        let v = self
            .world
            .entities
            .try_get(ent_id)?
            .get_vector(&self.world.type_def, ent_vector.0 as i16)?;
        self.globals.put_vector(v, dest_addr)?;

        Ok(())
    }

    pub fn op_load_s(
        &mut self,
        ent_id_addr: i16,
        ent_string_id_addr: i16,
        dest_addr: i16,
    ) -> Result<(), ProgsError> {
        let ent_id = self.globals.entity_id(ent_id_addr)?;
        let ent_string_id = self.globals.get_field_addr(ent_string_id_addr)?;
        let s = self
            .world
            .entities
            .try_get(ent_id)?
            .string_id(&self.world.type_def, ent_string_id.0 as i16)?;
        self.globals.put_string_id(s, dest_addr)?;

        Ok(())
    }

    pub fn op_load_ent(
        &mut self,
        ent_id_addr: i16,
        ent_entity_id_addr: i16,
        dest_addr: i16,
    ) -> Result<(), ProgsError> {
        let ent_id = self.globals.entity_id(ent_id_addr)?;
        let ent_entity_id = self.globals.get_field_addr(ent_entity_id_addr)?;
        let e = self
            .world
            .entities
            .try_get(ent_id)?
            .entity_id(&self.world.type_def, ent_entity_id.0 as i16)?;
        self.globals.put_entity_id(e, dest_addr)?;

        Ok(())
    }

    pub fn op_load_fnc(
        &mut self,
        ent_id_addr: i16,
        ent_function_id_addr: i16,
        dest_addr: i16,
    ) -> Result<(), ProgsError> {
        let ent_id = self.globals.entity_id(ent_id_addr)?;
        let fnc_function_id = self.globals.get_field_addr(ent_function_id_addr)?;
        let f = self
            .world
            .entities
            .try_get(ent_id)?
            .function_id(&self.world.type_def, fnc_function_id.0 as i16)?;
        self.globals.put_function_id(f, dest_addr)?;

        Ok(())
    }

    pub fn op_address(
        &mut self,
        ent_id_addr: i16,
        fld_addr_addr: i16,
        dest_addr: i16,
    ) -> Result<(), ProgsError> {
        let ent_id = self.globals.entity_id(ent_id_addr)?;
        let fld_addr = self.globals.get_field_addr(fld_addr_addr)?;
        self.globals.put_entity_field(
            self.world.ent_fld_addr_to_i32(EntityFieldAddr {
                entity_id: ent_id,
                field_addr: fld_addr,
            }),
            dest_addr,
        )?;

        Ok(())
    }

    pub fn op_storep_f(
        &mut self,
        src_float_addr: i16,
        dst_ent_fld_addr: i16,
        unused: i16,
    ) -> Result<(), ProgsError> {
        if unused != 0 {
            return Err(ProgsError::with_msg("storep_f: nonzero arg3"));
        }

        let f = self.globals.get_float(src_float_addr)?;
        let ent_fld_addr = self
            .world
            .ent_fld_addr_from_i32(self.globals.get_entity_field(dst_ent_fld_addr)?);
        self.world
            .entities
            .get_mut(ent_fld_addr.entity_id)?
            .put_float(&self.world.type_def, f, ent_fld_addr.field_addr.0 as i16)?;

        Ok(())
    }

    pub fn op_storep_v(
        &mut self,
        src_vector_addr: i16,
        dst_ent_fld_addr: i16,
        unused: i16,
    ) -> Result<(), ProgsError> {
        if unused != 0 {
            return Err(ProgsError::with_msg("storep_v: nonzero arg3"));
        }

        let v = self.globals.get_vector(src_vector_addr)?;
        let ent_fld_addr = self
            .world
            .ent_fld_addr_from_i32(self.globals.get_entity_field(dst_ent_fld_addr)?);
        self.world
            .entities
            .get_mut(ent_fld_addr.entity_id)?
            .put_vector(&self.world.type_def, v, ent_fld_addr.field_addr.0 as i16)?;

        Ok(())
    }

    pub fn op_storep_s(
        &mut self,
        src_string_id_addr: i16,
        dst_ent_fld_addr: i16,
        unused: i16,
    ) -> Result<(), ProgsError> {
        if unused != 0 {
            return Err(ProgsError::with_msg("storep_s: nonzero arg3"));
        }

        let s = self.globals.string_id(src_string_id_addr)?;
        let ent_fld_addr = self
            .world
            .ent_fld_addr_from_i32(self.globals.get_entity_field(dst_ent_fld_addr)?);
        self.world
            .entities
            .get_mut(ent_fld_addr.entity_id)?
            .put_string_id(&self.world.type_def, s, ent_fld_addr.field_addr.0 as i16)?;

        Ok(())
    }

    pub fn op_storep_ent(
        &mut self,
        src_entity_id_addr: i16,
        dst_ent_fld_addr: i16,
        unused: i16,
    ) -> Result<(), ProgsError> {
        if unused != 0 {
            return Err(ProgsError::with_msg("storep_ent: nonzero arg3"));
        }

        let e = self.globals.entity_id(src_entity_id_addr)?;
        let ent_fld_addr = self
            .world
            .ent_fld_addr_from_i32(self.globals.get_entity_field(dst_ent_fld_addr)?);
        self.world
            .entities
            .get_mut(ent_fld_addr.entity_id)?
            .put_entity_id(&self.world.type_def, e, ent_fld_addr.field_addr.0 as i16)?;

        Ok(())
    }

    pub fn op_storep_fnc(
        &mut self,
        src_function_id_addr: i16,
        dst_ent_fld_addr: i16,
        unused: i16,
    ) -> Result<(), ProgsError> {
        if unused != 0 {
            return Err(ProgsError::with_msg(format!(
                "storep_fnc: nonzero arg3 ({})",
                unused
            )));
        }

        let f = self.globals.function_id(src_function_id_addr)?;
        let ent_fld_addr = self
            .world
            .ent_fld_addr_from_i32(self.globals.get_entity_field(dst_ent_fld_addr)?);
        self.world
            .entities
            .get_mut(ent_fld_addr.entity_id)?
            .put_function_id(&self.world.type_def, f, ent_fld_addr.field_addr.0 as i16)?;

        Ok(())
    }

    pub fn op_state(
        &mut self,
        frame_id_addr: i16,
        think_function_addr: i16,
        unused_c: i16,
    ) -> Result<(), ProgsError> {
        if unused_c != 0 {
            return Err(ProgsError::with_msg(format!(
                "state: nonzero arg3 ({})",
                unused_c
            )));
        }

        let self_id = self.globals.entity_id(GlobalAddrEntity::Self_ as i16)?;
        let self_ent = self.world.entities.get_mut(self_id)?;
        let next_think_time = self.globals.get_float(GlobalAddrFloat::Time as i16)? + 0.1;

        self_ent.put_float(
            &self.world.type_def,
            next_think_time,
            FieldAddrFloat::NextThink as i16,
        )?;

        let frame_id = self.globals.get_float(frame_id_addr)?;
        self_ent.put_float(
            &self.world.type_def,
            frame_id,
            FieldAddrFloat::FrameId as i16,
        )?;

        let think_func = self.globals.function_id(think_function_addr)?;
        self_ent.put_function_id(
            &self.world.type_def,
            think_func,
            FieldAddrFunctionId::Think as _,
        )?;

        Ok(())
    }

    // QuakeC built-in functions ==============================================

    #[inline]
    pub fn builtin_err(&mut self, err_kind: impl fmt::Display) -> Result<(), ProgsError> {
        let msg = self.globals.string_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let msg = self.string_table.get(msg).unwrap();
        Err(ProgsError::with_msg(format!(
            "{} in {}: {}",
            err_kind,
            self.string_table
                .get(
                    self.cx
                        .function_def(self.cx.current_function())
                        .unwrap()
                        .name_id
                )
                .unwrap(),
            msg
        )))
    }

    #[inline]
    pub fn builtin_set_origin(
        &mut self,
        registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> Result<(), ProgsError> {
        let e_id = self.globals.entity_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let origin = self.globals.get_vector(GLOBAL_ADDR_ARG_1 as i16)?;
        self.set_entity_origin(e_id, Vec3::from(origin), registry, vfs)?;

        Ok(())
    }

    #[inline]
    pub fn builtin_set_model(&mut self) -> Result<(), ProgsError> {
        let ent_id = self.globals.entity_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let model_name_id = self.globals.string_id(GLOBAL_ADDR_ARG_1 as i16)?;
        self.set_entity_model(ent_id, model_name_id)?;

        Ok(())
    }

    #[inline]
    pub fn builtin_set_size(&mut self) -> Result<(), ProgsError> {
        let e_id = self.globals.entity_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let mins = self.globals.get_vector(GLOBAL_ADDR_ARG_1 as i16)?;
        let maxs = self.globals.get_vector(GLOBAL_ADDR_ARG_2 as i16)?;
        self.world.set_entity_size(e_id, mins.into(), maxs.into())?;

        Ok(())
    }

    // TODO: move to Globals
    // #[inline]
    pub fn builtin_random(&mut self) -> Result<(), ProgsError> {
        self.globals
            .put_float(rand::random(), GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    #[inline]
    pub fn builtin_spawn(&mut self, registry: Mut<Registry>, vfs: &Vfs) -> Result<(), ProgsError> {
        self.cx.print_backtrace(&self.string_table);
        let ent_id = self.spawn_entity(registry, vfs)?;
        self.globals
            .put_entity_id(ent_id, GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    #[inline]
    pub fn builtin_remove(&mut self) -> Result<(), ProgsError> {
        let ent_id = self.globals.entity_id(GLOBAL_ADDR_ARG_0 as i16)?;
        self.world.remove_entity(ent_id)?;

        Ok(())
    }

    #[inline]
    pub fn builtin_precache_sound(&mut self) -> Result<(), ProgsError> {
        // TODO: disable precaching after server is active
        // TODO: precaching doesn't actually load yet
        let s_id = self.globals.string_id(GLOBAL_ADDR_ARG_0 as i16)?;
        self.precache_sound(s_id);
        self.globals
            .put_string_id(s_id, GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    #[inline]
    pub fn builtin_precache_model(&mut self, vfs: &Vfs) -> Result<(), ProgsError> {
        // TODO: disable precaching after server is active
        // TODO: precaching doesn't actually load yet
        let s_id = self.globals.string_id(GLOBAL_ADDR_ARG_0 as i16)?;
        if self.model_id(s_id).is_none() {
            self.precache_model(s_id);
            self.world.add_model(vfs, &self.string_table, s_id)?;
        }

        self.globals
            .put_string_id(s_id, GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    #[inline]
    pub fn builtin_bprint(&mut self) -> Result<(), ProgsError> {
        let strs = &self.string_table;
        let s_id = self.globals.string_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let string = strs.get(s_id).unwrap();
        debug!("BPRINT: {}", string);

        // TODO: Broadcast to all clients

        Ok(())
    }

    #[inline]
    pub fn builtin_sprint(&mut self) -> Result<(), ProgsError> {
        let strs = &self.string_table;
        let _client_id = self.globals.entity_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let s_id = self.globals.string_id(GLOBAL_ADDR_ARG_1 as i16)?;
        let string = strs.get(s_id).unwrap();
        debug!("SPRINT: {}", string);

        // TODO: Send print to client

        Ok(())
    }

    #[inline]
    pub fn builtin_dprint(&mut self) -> Result<(), ProgsError> {
        let strs = &self.string_table;
        let s_id = self.globals.string_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let string = strs.get(s_id).unwrap();
        debug!("DPRINT: {}", string);

        Ok(())
    }

    #[inline]
    pub fn builtin_vtos(&mut self) -> Result<(), ProgsError> {
        let vec = self.globals.get_vector(GLOBAL_ADDR_ARG_0 as i16)?;
        let out = self
            .string_table
            .insert(&format!("{:5.1} {:5.1} {:5.1}", vec[0], vec[1], vec[2]));

        self.globals.put_string_id(out, GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    #[inline]
    pub fn builtin_drop_to_floor(
        &mut self,
        registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> Result<(), ProgsError> {
        let ent_id = self.globals.entity_id(GlobalAddrEntity::Self_ as i16)?;
        let hit_floor = self.drop_entity_to_floor(ent_id, registry, vfs)?;
        self.globals
            .put_float(hit_floor as u8 as f32, GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    #[inline]
    pub fn builtin_light_style(&mut self) -> Result<(), ProgsError> {
        let index = match self.globals.get_float(GLOBAL_ADDR_ARG_0 as i16)? as i32 {
            i if i < 0 => return Err(ProgsError::with_msg("negative lightstyle ID")),
            i => i as usize,
        };
        let val = self.globals.string_id(GLOBAL_ADDR_ARG_1 as i16)?;
        self.set_lightstyle(index, val);

        Ok(())
    }

    #[inline]
    pub fn builtin_cvar(&mut self, registry: &Registry) -> Result<(), ProgsError> {
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

    #[inline]
    pub fn builtin_cvar_set(&mut self, mut registry: Mut<Registry>) -> Result<(), ProgsError> {
        let strs = &self.string_table;

        let var_id = self.globals.string_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let var = strs.get(var_id).unwrap();
        let val_id = self.globals.string_id(GLOBAL_ADDR_ARG_1 as i16)?;
        let val = strs.get(val_id).unwrap();

        registry.set_cvar(var.to_str(), val.to_str()).unwrap();
        debug!("{} = {}", var, val);

        Ok(())
    }

    #[inline]
    pub fn builtin_ambient_sound(&mut self) -> Result<(), ProgsError> {
        let pos = self.globals.get_vector(GLOBAL_ADDR_ARG_0 as i16)?;
        let sample = self.globals.string_id(GLOBAL_ADDR_ARG_1 as i16)?;
        let volume = (self.globals.get_float(GLOBAL_ADDR_ARG_2 as i16)? * 255.) as _;
        let attenuation = (self.globals.get_float(GLOBAL_ADDR_ARG_3 as i16)? * 255.) as _;

        let Some(sound_id) = self.sound_id(sample) else {
            error!(
                "Cannot find sound {} in precache",
                self.string_table.get(sample).unwrap()
            );
            return Ok(());
        };

        ServerCmd::SpawnStaticSound {
            origin: pos.into(),
            sound_id: sound_id as _,
            volume,
            attenuation,
        }
        .serialize(&mut self.broadcast)?;

        Ok(())
    }

    #[inline]
    pub fn builtin_find(&mut self) -> Result<(), ProgsError> {
        let entity = self.globals.entity_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let field = self.globals.get_field_addr(GLOBAL_ADDR_ARG_1 as i16)?;
        let match_str = self.globals.string_id(GLOBAL_ADDR_ARG_2 as i16)?;

        let Some(match_str) = self.string_table.get(match_str) else {
            return Err(ProgsError::with_msg(
                "Failed to find match string".to_owned(),
            ));
        };

        let field = FieldAddrStringId::from_usize(field.0).unwrap();

        for ent in self
            .world
            .entities
            .range((Bound::Excluded(entity.0), Bound::Unbounded))
        {
            let Ok(field) = self
                .world
                .entities
                .try_get(ent)?
                .load(&self.world.type_def, field)
            else {
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

        self.globals
            .put_entity_id(EntityId(0), GLOBAL_ADDR_RETURN as i16)?;
        Ok(())
    }

    #[inline]
    pub fn builtin_walk_move(
        &mut self,
        registry: Mut<Registry>,
        vfs: &Vfs,
    ) -> Result<(), ProgsError> {
        let yaw = self.globals.get_float(GLOBAL_ADDR_ARG_0 as i16)?;
        let dist = self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)?;

        let this = GlobalAddrEntity::Self_.load(&self.globals).unwrap();

        let old_vel = self
            .world
            .entities
            .try_get(this)?
            .velocity(&self.world.type_def)
            .unwrap();
        self.world.entities.get_mut(this).unwrap().set_velocity(
            &self.world.type_def,
            Mat3::from_rotation_y(yaw.to_radians()) * Vec3::X * dist,
        )?;
        self.physics_step(this, Duration::try_seconds(1).unwrap(), vfs, registry)?;
        self.world
            .entities
            .get_mut(this)?
            .set_velocity(&self.world.type_def, old_vel)?;

        self.globals
            .put_entity_id(EntityId(0), GLOBAL_ADDR_RETURN as i16)?;
        Ok(())
    }

    #[inline]
    pub fn builtin_check_client(&mut self) -> Result<(), ProgsError> {
        debug!("TODO: PF_checkclient");
        self.globals
            .put_entity_id(EntityId(1), GLOBAL_ADDR_RETURN as i16)?;

        Ok(())
    }

    #[inline]
    pub fn builtin_make_static(&mut self) -> Result<(), ProgsError> {
        let ent = self.globals.entity_id(GLOBAL_ADDR_ARG_0 as i16)?;

        let Ok(entity) = self.world.entities.try_get(ent) else {
            error!("Tried to call `make_static` on a non-existant entity");
            return Ok(());
        };

        let (model_name, frame_id, colormap, skin_id, origin, angles) = (
            entity.string_id(&self.world.type_def, FieldAddrStringId::ModelName as i16)?,
            entity.get_float(&self.world.type_def, FieldAddrFloat::FrameId as i16)? as _,
            entity.get_float(&self.world.type_def, FieldAddrFloat::Colormap as i16)? as _,
            entity.get_float(&self.world.type_def, FieldAddrFloat::SkinId as i16)? as _,
            entity.get_vector(&self.world.type_def, FieldAddrVector::Origin as i16)?,
            entity.get_vector(&self.world.type_def, FieldAddrVector::Angles as i16)?,
        );

        // Even though there is a `ModelId` field, it seems like quake searches for the model in the precache manually
        let model_id = self.model_id(model_name).ok_or_else(|| {
            ProgsError::with_msg(format!(
                "Model not found in precache: {:?}",
                self.string_table.get(model_name)
            ))
        })? as _;

        ServerCmd::SpawnStatic {
            model_id,
            frame_id,
            colormap,
            skin_id,
            origin: origin.into(),
            angles: angles.into(),
        }
        .serialize(&mut self.broadcast)?;

        self.world.entities.remove(ent)?;

        Ok(())
    }

    #[inline]
    pub fn builtin_sound(&mut self) -> Result<(), ProgsError> {
        let entity = self.globals.entity_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let channel = self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)? as _;
        let sample = self.globals.string_id(GLOBAL_ADDR_ARG_2 as i16)?;
        let volume = self.globals.get_float(GLOBAL_ADDR_ARG_3 as i16)?;
        let attenuation = self.globals.get_float(GLOBAL_ADDR_ARG_4 as i16)?;

        self.sound(entity, channel, sample, volume, attenuation)?;

        Ok(())
    }

    #[inline]
    pub fn builtin_write_byte(&mut self) -> Result<(), ProgsError> {
        let dest = self.globals.get_float(GLOBAL_ADDR_ARG_0 as i16)?;
        if dest != 0. {
            error!("TODO: Non-broadcast write ({})", dest as usize);
            return Ok(());
        }
        let val = self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)? as u8;
        self.broadcast.write_u8(val)?;
        Ok(())
    }

    #[inline]
    pub fn builtin_write_char(&mut self) -> Result<(), ProgsError> {
        let dest = self.globals.get_float(GLOBAL_ADDR_ARG_0 as i16)?;
        if dest != 0. {
            error!("TODO: Non-broadcast write ({})", dest as usize);
            return Ok(());
        }
        let val = self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)? as i8;
        self.broadcast.write_i8(val)?;
        Ok(())
    }

    #[inline]
    pub fn builtin_write_short(&mut self) -> Result<(), ProgsError> {
        let dest = self.globals.get_float(GLOBAL_ADDR_ARG_0 as i16)?;
        if dest != 0. {
            error!("TODO: Non-broadcast write ({})", dest as usize);
            return Ok(());
        }
        let val = self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)? as i16;
        self.broadcast.write_i16::<LittleEndian>(val)?;
        Ok(())
    }

    #[inline]
    pub fn builtin_write_long(&mut self) -> Result<(), ProgsError> {
        let dest = self.globals.get_float(GLOBAL_ADDR_ARG_0 as i16)?;
        if dest != 0. {
            error!("TODO: Non-broadcast write ({})", dest as usize);
            return Ok(());
        }
        let val = self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)? as i32;
        self.broadcast.write_i32::<LittleEndian>(val)?;
        Ok(())
    }

    #[inline]
    pub fn builtin_write_coord(&mut self) -> Result<(), ProgsError> {
        let dest = self.globals.get_float(GLOBAL_ADDR_ARG_0 as i16)?;
        if dest != 0. {
            error!("TODO: Non-broadcast write ({})", dest as usize);
            return Ok(());
        }
        let val = (self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)? * 8.) as i16;
        self.broadcast.write_i16::<LittleEndian>(val)?;
        Ok(())
    }

    #[inline]
    pub fn builtin_write_angle(&mut self) -> Result<(), ProgsError> {
        let dest = self.globals.get_float(GLOBAL_ADDR_ARG_0 as i16)?;
        if dest != 0. {
            error!("TODO: Non-broadcast write ({})", dest as usize);
            return Ok(());
        }
        let val = (self.globals.get_float(GLOBAL_ADDR_ARG_1 as i16)? * 256. / 360.) as u8;
        self.broadcast.write_u8(val)?;
        Ok(())
    }

    #[inline]
    pub fn builtin_write_string(&mut self) -> Result<(), ProgsError> {
        let dest = self.globals.get_float(GLOBAL_ADDR_ARG_0 as i16)?;
        if dest != 0. {
            error!("TODO: Non-broadcast write ({})", dest as usize);
            return Ok(());
        }
        let val = self.globals.string_id(GLOBAL_ADDR_ARG_1 as i16)?;
        let string = self.string_table.get(val).unwrap_or_default();
        self.broadcast.write_all(&*string.raw)?;
        self.broadcast.write_u8(0)?;
        Ok(())
    }

    #[inline]
    pub fn builtin_write_entity(&mut self) -> Result<(), ProgsError> {
        let dest = self.globals.get_float(GLOBAL_ADDR_ARG_0 as i16)?;
        if dest != 0. {
            error!("TODO: Non-broadcast write ({})", dest as usize);
            return Ok(());
        }
        error!("TODO: Broadcast write entity");
        Ok(())
    }

    #[inline]
    pub fn builtin_center_print(&mut self) -> Result<(), ProgsError> {
        let text = self.globals.string_id(GLOBAL_ADDR_ARG_0 as i16)?;
        let text = self.string_table.get(text).unwrap().into_owned();

        ServerCmd::CenterPrint { text }.serialize(&mut self.broadcast)?;

        Ok(())
    }

    #[inline]
    pub fn builtin_trace_line(&mut self) -> Result<(), ProgsError> {
        let v1 = self.globals.get_vector(GLOBAL_ADDR_ARG_0 as i16)?;
        let v2 = self.globals.get_vector(GLOBAL_ADDR_ARG_1 as i16)?;
        let kind = self.globals.get_float(GLOBAL_ADDR_ARG_2 as i16)?;
        let ent = self.globals.entity_id(GLOBAL_ADDR_ARG_3 as i16)?;

        let (trace, hit_ent) = self.world.trace_entity_move(
            ent,
            v1.into(),
            Vec3::ZERO,
            Vec3::ZERO,
            v2.into(),
            CollideKind::from_f32(kind).unwrap_or_default(),
        )?;

        self.globals.put_float(
            if trace.all_solid() { 1. } else { 0. },
            GlobalAddrFloat::TraceAllSolid as _,
        )?;
        self.globals.put_float(
            if trace.start_solid() { 1. } else { 0. },
            GlobalAddrFloat::TraceStartSolid as _,
        )?;
        self.globals
            .put_float(trace.ratio(), GlobalAddrFloat::TraceFraction as _)?;
        self.globals.put_float(
            if trace.in_water() { 1. } else { 0. },
            GlobalAddrFloat::TraceInWater as _,
        )?;
        self.globals.put_float(
            if trace.in_open() { 1. } else { 0. },
            GlobalAddrFloat::TraceInOpen as _,
        )?;
        self.globals
            .put_vector(trace.end_point().into(), GlobalAddrVector::TraceEndPos as _)?;
        self.globals.put_vector(
            trace
                .plane()
                .map(|plane| plane.normal())
                .unwrap_or(Vec3::ZERO)
                .into(),
            GlobalAddrVector::TracePlaneNormal as _,
        )?;
        self.globals.put_float(
            trace.plane_dist().unwrap_or_default(),
            GlobalAddrFloat::TracePlaneDist as _,
        )?;
        self.globals.put_entity_id(
            hit_ent.unwrap_or_default(),
            GlobalAddrEntity::TraceEntity as _,
        )?;

        Ok(())
    }

    #[inline]
    pub fn builtin_normalize(&mut self) -> Result<(), ProgsError> {
        let vec = self.globals.get_vector(GLOBAL_ADDR_ARG_0 as i16)?;
        let normalized = Vec3::from(vec).normalize();
        let normalized = if normalized.is_finite() {
            normalized
        } else {
            Vec3::ZERO
        };

        self.globals
            .put_vector(normalized.into(), GLOBAL_ADDR_RETURN as _)?;
        Ok(())
    }
}

pub mod systems {
    use crate::common::{
        console::CmdName,
        net::{self, ClientCmd, ClientMessage, GameType, ServerMessage, SignOnStage},
    };

    use super::*;

    pub fn recv_client_messages(
        mut server: ResMut<Session>,
        mut client_msgs: EventReader<ClientMessage>,
        mut server_messages: EventWriter<ServerMessage>,
        mut registry: ResMut<Registry>,
        vfs: Res<Vfs>,
    ) {
        let mut out_packet = Vec::new();
        for ClientMessage {
            client_id,
            packet,
            kind: _,
        } in client_msgs.read()
        {
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

                                        server.clientcmd_prespawn(client_id).unwrap();

                                        ServerCmd::SignOnStage {
                                            stage: SignOnStage::ClientInfo,
                                        }
                                        .serialize(&mut out_packet)
                                        .unwrap();
                                    }
                                    "name" => {
                                        // TODO: Error handling
                                        assert!(args.len() == 1);

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
                                        server.clientcmd_spawn(client_id).unwrap();

                                        ServerCmd::SignOnStage {
                                            stage: SignOnStage::Begin,
                                        }
                                        .serialize(&mut out_packet)
                                        .unwrap();
                                    }
                                    "begin" => {
                                        // TODO: Error handling
                                        assert!(args.is_empty());

                                        server
                                            .clientcmd_begin(client_id, registry.reborrow(), &*vfs)
                                            .unwrap();

                                        let client_ent =
                                            server.client(client_id).unwrap().entity().unwrap();

                                        // TODO: Error handling
                                        ServerCmd::SetView {
                                            ent_id: client_ent.0 as _,
                                        }
                                        .serialize(&mut out_packet)
                                        .unwrap();

                                        ServerCmd::SignOnStage {
                                            stage: SignOnStage::Done,
                                        }
                                        .serialize(&mut out_packet)
                                        .unwrap();
                                    }
                                    other => {
                                        error!(
                                            "{}: command unrecognized in connection scope",
                                            other
                                        );
                                    }
                                }
                            }
                        }
                        ClientCmd::Move {
                            send_time,
                            angles,
                            fwd_move,
                            side_move,
                            up_move,
                            button_flags,
                            impulse,
                        } => {
                            let Session { persist, level, .. } = &mut *server;

                            if let Some(entity) = persist
                                .client(client_id)
                                .and_then(|c| c.entity())
                                .and_then(|ent_id| level.world.entities.get_mut(ent_id).ok())
                            {
                                entity
                                    .put_vector(
                                        &level.world.type_def,
                                        [fwd_move as _, side_move as _, up_move as _],
                                        FieldAddrVector::MoveDirection as _,
                                    )
                                    .unwrap();
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
            server_messages.write(ServerMessage {
                client_id: 0,
                packet: out_packet,
            });
        }
    }

    pub fn server_spawn(
        mut server: ResMut<Session>,
        mut registry: ResMut<Registry>,
        mut server_messages: EventWriter<ServerMessage>,
        time: Res<Time<Virtual>>,
        vfs: Res<Vfs>,
    ) -> Result<(), ProgsError> {
        if !server.loading() {
            return Ok(());
        }

        // In `sv_init.c` there is a comment saying to run physics twice before starting the server
        // properly to "allow everything to settle".
        for _ in 0..2 {
            let server = &mut *server;
            server.level.physics(
                &server.persist.client_slots,
                Duration::from_std(time.elapsed())
                    .map_err(|e| ProgsError::with_msg(format!("{}", e)))?,
                registry.reborrow(),
                &*vfs,
            )?;
        }

        server.state = SessionState::Active;

        let teamplay = registry
            .get_cvar("teamplay")
            .and_then(|t| t.value().as_name());

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
            model_precache: server
                .level
                .model_precache
                .iter()
                .map(ToOwned::to_owned)
                .collect(),
            sound_precache: server
                .level
                .sound_precache
                .iter()
                .map(ToOwned::to_owned)
                .collect(),
        }
        .serialize(&mut packet)?;

        for (id, style) in server.level.lightstyles.iter().enumerate() {
            let value = server.level.string_table.get(*style).unwrap_or_default();
            if !value.is_empty() {
                ServerCmd::LightStyle {
                    id: id as _,
                    value: value.into_owned(),
                }
                .serialize(&mut packet)?;
            }
        }

        ServerCmd::SignOnStage {
            stage: SignOnStage::Prespawn,
        }
        .serialize(&mut packet)?;

        server_messages.write(ServerMessage {
            client_id: 0,
            packet,
        });

        Ok(())
    }

    pub fn server_update(
        mut server: ResMut<Session>,
        time: Res<Time<Fixed>>,
        mut server_messages: EventWriter<ServerMessage>,
        mut registry: ResMut<Registry>,
        vfs: Res<Vfs>,
    ) {
        if server.loading()
            || registry.read_cvar::<u8>("sv_paused").unwrap() != 0
            || server.persist.client_slots.active_clients().count() == 0
        {
            return;
        }

        let send_diff = match &mut *server {
            Session {
                persist,
                state: SessionState::Active,
                level,
            } => {
                if let Err(e) = level.physics(
                    &persist.client_slots,
                    Duration::from_std(time.elapsed()).unwrap(),
                    registry.reborrow(),
                    &*vfs,
                ) {
                    error!("Failed running frame: {}", Report::from_error(e));
                    false
                } else {
                    true
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
                .collect::<ArrayVec<usize, 8>>()
            {
                let mut packet = Vec::new();

                ServerCmd::Time {
                    time: engine::duration_to_f32(level.time),
                }
                .serialize(&mut packet)
                .unwrap();

                for entity_id in &level.new_entities {
                    if let Some(state) = level.entity_state(*entity_id) {
                        state
                            .spawn_baseline(entity_id.0 as _)
                            .serialize(&mut packet)
                            .unwrap();
                        level.world.entities.get_mut(*entity_id).unwrap().baseline = state;
                    }
                }

                // Skip world entity
                for ent in level.world.entities.iter().skip(1) {
                    // TODO: Handle deletions
                    let Ok(entity) = level.world.entities.try_get(ent) else {
                        continue;
                    };

                    let update = entity
                        .state(&level.world.type_def)
                        .unwrap()
                        .make_update(ent.0 as _, &entity.baseline);

                    ServerCmd::FastUpdate(update)
                        .serialize(&mut packet)
                        .unwrap();
                }

                if let Some(entity) = persist
                    .client(client_id)
                    .and_then(|c| c.entity())
                    .and_then(|ent_id| level.world.entities.get_mut(ent_id).ok())
                {
                    if entity
                        .get_bool(&level.world.type_def, FieldAddrFloat::FixAngle as i16)
                        .unwrap()
                    {
                        ServerCmd::SetAngle {
                            angles: entity
                                .get_vector(&level.world.type_def, FieldAddrVector::Angles as i16)
                                .unwrap()
                                .into(),
                        }
                        .serialize(&mut packet)
                        .unwrap();
                        entity
                            .put_float(&level.world.type_def, 0., FieldAddrFloat::FixAngle as i16)
                            .unwrap();
                    }
                    // ServerCmd::PlayerData(PlayerData {
                    //     view_height: todo!(),
                    //     ideal_pitch: todo!(),
                    //     punch_pitch: todo!(),
                    //     velocity_x: todo!(),
                    //     punch_yaw: todo!(),
                    //     velocity_y: todo!(),
                    //     punch_roll: todo!(),
                    //     velocity_z: todo!(),
                    //     items: todo!(),
                    //     on_ground: todo!(),
                    //     in_water: todo!(),
                    //     weapon_frame: todo!(),
                    //     armor: todo!(),
                    //     weapon: todo!(),
                    //     health: todo!(),
                    //     ammo: todo!(),
                    //     ammo_shells: todo!(),
                    //     ammo_nails: todo!(),
                    //     ammo_rockets: todo!(),
                    //     ammo_cells: todo!(),
                    //     active_weapon: todo!(),
                    //     // TODO: Send player data
                    // }).serialize(&mut packet).unwrap()
                }

                // We add broadcast packets at the end to ensure that entities can spawn before broadcasted
                // events related to those entities
                packet.extend_from_slice(&level.broadcast);

                server_messages.write(ServerMessage { client_id, packet });
            }

            level.broadcast.clear();
            level.new_entities.clear();
        }
    }
}
