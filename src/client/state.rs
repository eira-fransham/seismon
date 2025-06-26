use std::{
    io::{Cursor, Read}, iter, num::NonZeroU32, path::Path, sync::LazyLock
};

use super::{sound::MixerEvent, view::BobVars};
use crate::{
    client::{
        ClientError, ColorShiftCode, IntermissionKind, MAX_STATS, MoveVars,
        entity::{
            Beam, ClientEntity, Light, LightDesc, Lights, MAX_BEAMS, MAX_TEMP_ENTITIES,
            particle::{Particle, Particles, TrailKind},
        },
        render::Camera,
        sound::{Listener, StartSound},
        view::{IdleVars, KickVars, MouseVars, RollVars, View},
    },
    common::{
        bsp,
        console::Registry,
        engine,
        math::{self, Angles},
        model::{Model, ModelFlags, ModelKind, SyncType},
        net::{
            self, BeamEntityKind, ButtonFlags, ColorShift, EntityEffects, ItemFlags, PlayerData,
            PointEntityKind, TempEntity,
        },
        util::QString,
        vfs::Vfs,
    },
};
use arrayvec::ArrayVec;
use beef::Cow;
use bevy::{
    asset::{AssetServer, Handle},
    ecs::{entity::Entity, event::EventWriter, resource::Resource},
    log::*,
    math::{Mat4, Quat, Vec3},
    prelude::default,
};
use bevy_seedling::sample::Sample;
use chrono::Duration;
use firewheel::sample_resource::DecodedAudioF32;
use hashbrown::HashMap;
use net::{ClientCmd, ClientStat, EntityState, EntityUpdate, PlayerColor};
use rand::{
    SeedableRng as _,
    distr::{Distribution as _, Uniform},
    rngs::SmallRng,
};
use symphonium::{SymphoniumLoader, symphonia::core::probe::Hint};

const CACHED_SOUND_NAMES: &[&str] = &[
    "hknight/hit.wav",
    "weapons/r_exp3.wav",
    "weapons/ric1.wav",
    "weapons/ric2.wav",
    "weapons/ric3.wav",
    "weapons/tink1.wav",
    "wizard/hit.wav",
];

const MAX_LIGHT_STYLES: usize = 64;

#[derive(Clone)]
pub struct PlayerInfo {
    pub name: QString,
    pub frags: i32,
    pub colors: PlayerColor,
    // translations: [u8; VID_GRADES],
}

// TODO: We clone this into the render world but this is inefficient
//       e.g., none of the elements related to sound need to be accessed from the renderer
// client information regarding the current level
#[derive(Clone)]
pub struct ClientState {
    // local rng
    rng: SmallRng,

    // model precache
    pub models: im::Vector<Model>,

    pub worldmodel_id: usize,

    // name-to-id map
    pub model_names: im::HashMap<String, usize>,

    // audio source precache
    pub sounds: im::Vector<Handle<Sample>>,

    // sounds that are always needed even if not in precache
    cached_sounds: im::HashMap<Cow<'static, str>, Handle<Sample>>,

    // entities and entity-like things
    pub entities: im::Vector<ClientEntity>,
    pub static_entities: im::Vector<ClientEntity>,
    pub temp_entities: im::Vector<ClientEntity>,
    // dynamic point lights
    pub lights: Lights,
    // lightning bolts and grappling hook cable
    pub beams: [Option<Beam>; MAX_BEAMS],
    // particle effects
    pub particles: Particles,

    // visible entities, rebuilt per-frame
    pub visible_entity_ids: im::Vector<usize>,

    pub light_styles: im::Vector<String>,

    // various values relevant to the player and level (see common::net::ClientStat)
    pub stats: [i32; MAX_STATS],

    pub max_players: usize,
    pub player_info: [Option<PlayerInfo>; net::MAX_CLIENTS],

    // the last two timestamps sent by the server (for lerping)
    pub msg_times: [Duration; 2],
    pub time: Duration,
    pub lerp_factor: f32,

    pub items: ItemFlags,
    pub item_get_time: [Duration; net::MAX_ITEMS],
    pub face_anim_time: Duration,
    pub color_shifts: [ColorShift; 4],
    pub view: View,

    pub msg_velocity: [Vec3; 2],
    pub velocity: Vec3,

    // paused: bool,
    pub on_ground: bool,
    pub in_water: bool,
    pub intermission: Option<IntermissionKind>,
    pub start_time: Duration,
    pub completion_time: Option<Duration>,
}

impl Default for ClientState {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Resource)]
pub struct ClientInfo {
    pub entity_map: HashMap<usize, Entity>,
}

impl ClientState {
    // TODO: add parameter for number of player slots and reserve them in entity list
    pub fn new() -> ClientState {
        ClientState {
            rng: SmallRng::from_rng(&mut rand::rng()),
            models: iter::once(Model::none()).collect(),
            worldmodel_id: 1,
            model_names: default(),
            sounds: default(),
            cached_sounds: default(),
            entities: default(),
            static_entities: default(),
            temp_entities: default(),
            lights: Lights::new(),
            beams: [None; MAX_BEAMS],
            particles: Particles::new(),
            visible_entity_ids: default(),
            light_styles: iter::repeat_n("".into(), MAX_LIGHT_STYLES).collect(),
            stats: [0; MAX_STATS],
            max_players: 0,
            player_info: default(),
            msg_times: [Duration::zero(), Duration::zero()],
            time: Duration::zero(),
            lerp_factor: 0.0,
            items: ItemFlags::empty(),
            item_get_time: [Duration::zero(); net::MAX_ITEMS],
            color_shifts: default(),
            view: View::new(),
            face_anim_time: Duration::zero(),
            msg_velocity: [Vec3::ZERO, Vec3::ZERO],
            velocity: Vec3::ZERO,
            on_ground: false,
            in_water: false,
            intermission: None,
            start_time: Duration::zero(),
            completion_time: None,
        }
    }

    pub fn from_server_info<SName: AsRef<str>>(
        vfs: &Vfs,
        asset_server: &AssetServer,
        max_clients: u8,
        model_precache: Vec<String>,
        sound_precache: Vec<SName>,
    ) -> Result<ClientState, ClientError> {
        let mut sound_loader = SymphoniumLoader::new();

        // TODO: validate submodel names
        let mut models: im::Vector<_> = iter::once(Model::none()).collect();
        let mut model_names = im::HashMap::new();
        for mod_name in model_precache {
            // BSPs can have more than one model
            if mod_name.ends_with(".bsp") {
                let bsp_data = vfs.open(&mod_name)?;
                let (mut brush_models, _) = bsp::load(bsp_data).unwrap();
                for bmodel in brush_models.drain(..) {
                    let id = models.len();
                    let name = bmodel.name().to_owned();
                    models.push_back(bmodel);
                    model_names.insert(name, id);
                }
            } else if !mod_name.starts_with("*") {
                // model names starting with * are loaded from the world BSP
                debug!("Loading model {mod_name}");
                let id = models.len();
                models.push_back(Model::load(vfs, &mod_name)?);
                model_names.insert(mod_name, id);
            }

            // TODO: send keepalive message?
        }

        let sounds = iter::once("misc/null.wav")
            .chain(sound_precache.iter().map(AsRef::as_ref))
            .enumerate()
            .map(|(i, snd_name)| {
                info!("Loading sound {}: {}", i, snd_name);

                let mut data = Vec::new();
                vfs.open(format!("sound/{snd_name}"))?
                    .read_to_end(&mut data)
                    .unwrap();

                let ext = Path::new(snd_name)
                    .extension()
                    .map(|ext| ext.to_string_lossy());

                let cursor = Cursor::new(data);

                let mut decode_hint = Hint::new();

                if let Some(ext) = ext.as_deref() {
                    decode_hint.with_extension(ext);
                }

                let decoded = firewheel::load_audio_file_from_source(
                    &mut  sound_loader,
                    Box::new(cursor),
                    Some(decode_hint),
                    NonZeroU32::new(44100).unwrap(),
                    default(),
                )?;

                Ok(asset_server.add(Sample::new(decoded)))
                // TODO: send keepalive message?
            })
            .collect::<Result<_, ClientError>>()?;

        let cached_sounds = CACHED_SOUND_NAMES
            .iter()
            .copied()
            .map(|snd_name| {
                let mut data = Vec::new();
                vfs.open(format!("sound/{snd_name}"))?
                    .read_to_end(&mut data)
                    .unwrap();

                let ext = Path::new(snd_name)
                    .extension()
                    .map(|ext| ext.to_string_lossy());

                let cursor = Cursor::new(data);

                let mut decode_hint = Hint::new();

                if let Some(ext) = ext.as_deref() {
                    decode_hint.with_extension(ext);
                }

                let decoded = firewheel::load_audio_file_from_source(
                    &mut  sound_loader,
                    Box::new(cursor),
                    Some(decode_hint),
                    NonZeroU32::new(44100).unwrap(),
                    default(),
                )?;

                Ok((
                    snd_name.into(),
                    asset_server.add(Sample::new(decoded)),
                ))
            })
            .collect::<Result<_, ClientError>>()?;

        Ok(ClientState {
            models,
            model_names,
            sounds,
            cached_sounds,
            max_players: max_clients as usize,
            ..ClientState::new()
        })
    }

    /// Advance the simulation time by the specified amount.
    ///
    /// This method does not change the state of the world to match the new time value.
    pub fn advance_time(&mut self, frame_time: Duration) {
        self.time = self.time + frame_time;
    }

    /// Update the client state interpolation ratio.
    ///
    /// This calculates the ratio used to interpolate entities between the last
    /// two updates from the server.
    pub fn update_interp_ratio(&mut self, cl_nolerp: bool) {
        if cl_nolerp {
            self.time = self.msg_times[0];
            self.lerp_factor = 1.0;
            return;
        }

        let server_delta = engine::duration_to_f32(match self.msg_times[0] - self.msg_times[1] {
            // if no time has passed between updates, don't lerp anything
            d if d == Duration::zero() => {
                self.time = self.msg_times[0];
                self.lerp_factor = 1.0;
                return;
            }

            d if d > Duration::try_milliseconds(100).unwrap() => {
                self.msg_times[1] = self.msg_times[0] - Duration::try_milliseconds(100).unwrap();
                Duration::try_milliseconds(100).unwrap()
            }

            d if d < Duration::zero() => {
                warn!(
                    "Negative time delta from server!: ({})s",
                    engine::duration_to_f32(d)
                );
                d
            }

            d => d,
        });

        let frame_delta = engine::duration_to_f32(self.time - self.msg_times[1]);

        self.lerp_factor = match frame_delta / server_delta {
            f if f < 0.0 => {
                if f < -0.01 {
                    self.time = self.msg_times[1];
                }

                0.0
            }

            f if f > 1.0 => {
                if f > 1.01 {
                    self.time = self.msg_times[0];
                }

                1.0
            }

            f => f,
        }
    }

    /// Update all entities in the game world.
    ///
    /// This method is responsible for the following:
    /// - Updating entity position
    /// - Despawning entities which did not receive an update in the last server
    ///   message
    /// - Spawning particles on entities with particle effects
    /// - Spawning dynamic lights on entities with lighting effects
    pub fn update_entities(&mut self) -> Result<(), ClientError> {
        static MFLASH_DIMLIGHT_DISTRIBUTION: LazyLock<Uniform<f32>> =
            LazyLock::new(|| Uniform::new(200.0, 232.0).unwrap());
        static BRIGHTLIGHT_DISTRIBUTION: LazyLock<Uniform<f32>> =
            LazyLock::new(|| Uniform::new(400.0, 432.0).unwrap());

        let lerp_factor = self.lerp_factor;

        self.velocity =
            self.msg_velocity[1] + lerp_factor * (self.msg_velocity[0] - self.msg_velocity[1]);

        // TODO: if we're in demo playback, interpolate the view angles

        let obj_rotate = 100.0 * engine::duration_to_f32(self.time).rem_euclid(360.);

        // rebuild the list of visible entities
        self.visible_entity_ids.clear();

        // NOTE that we start at entity 1 since we don't need to link the world entity
        for ent in self.entities.iter_mut().skip(1) {
            if ent.model_id == 0 {
                // nothing in this entity slot
                continue;
            }

            // if we didn't get an update this frame, remove the entity
            if ent.msg_time != self.msg_times[0] {
                ent.model_id = 0;
                continue;
            }

            let prev_origin = ent.origin;

            if ent.force_link {
                trace!("force link on entity {}", ent.id);
                ent.origin = ent.msg_origins[0];
                ent.angles = ent.msg_angles[0];
            } else {
                let origin_delta = ent.msg_origins[0] - ent.msg_origins[1];
                let ent_lerp_factor = if origin_delta.length_squared() > 10_000.0 {
                    // if the entity moved more than 100 units in one frame,
                    // assume it was teleported and don't lerp anything
                    1.0
                } else {
                    lerp_factor
                };

                ent.origin = ent.msg_origins[1] + ent_lerp_factor * origin_delta;

                // assume that entities will not whip around 180+ degrees in one
                // frame and adjust the delta accordingly. this avoids a bug
                // where small turns between 0 <-> 359 cause the demo camera to
                // face backwards for one frame.
                for i in 0..3 {
                    let mut angle_delta = ent.msg_angles[0][i] - ent.msg_angles[1][i];
                    if angle_delta > 180.0 {
                        angle_delta = 360.0 - angle_delta;
                    } else if angle_delta < -180.0 {
                        angle_delta += angle_delta;
                    }

                    ent.angles[i] =
                        (ent.msg_angles[1][i] + angle_delta * ent_lerp_factor).rem_euclid(360.);
                }
            }

            let model = &self.models[ent.model_id];
            if model.has_flag(ModelFlags::ROTATE) {
                ent.angles[1] = obj_rotate;
            }

            if ent.effects.contains(EntityEffects::BRIGHT_FIELD) {
                self.particles.create_entity_field(self.time, ent);
            }

            // TODO: factor out EntityEffects->LightDesc mapping
            if ent.effects.contains(EntityEffects::MUZZLE_FLASH) {
                // TODO: angle and move origin to muzzle
                ent.light_id = Some(self.lights.insert(
                    self.time,
                    LightDesc {
                        origin: ent.origin + Vec3::new(0.0, 0.0, 16.0),
                        init_radius: MFLASH_DIMLIGHT_DISTRIBUTION.sample(&mut self.rng),
                        decay_rate: 0.0,
                        min_radius: Some(32.0),
                        ttl: Duration::try_milliseconds(100).unwrap(),
                    },
                    ent.light_id,
                ));
            }

            if ent.effects.contains(EntityEffects::BRIGHT_LIGHT) {
                ent.light_id = Some(self.lights.insert(
                    self.time,
                    LightDesc {
                        origin: ent.origin,
                        init_radius: BRIGHTLIGHT_DISTRIBUTION.sample(&mut self.rng),
                        decay_rate: 0.0,
                        min_radius: None,
                        ttl: Duration::try_milliseconds(1).unwrap(),
                    },
                    ent.light_id,
                ));
            }

            if ent.effects.contains(EntityEffects::DIM_LIGHT) {
                ent.light_id = Some(self.lights.insert(
                    self.time,
                    LightDesc {
                        origin: ent.origin,
                        init_radius: MFLASH_DIMLIGHT_DISTRIBUTION.sample(&mut self.rng),
                        decay_rate: 0.0,
                        min_radius: None,
                        ttl: Duration::try_milliseconds(1).unwrap(),
                    },
                    ent.light_id,
                ));
            }

            // check if this entity leaves a trail
            let trail_kind = if model.has_flag(ModelFlags::GIB) {
                Some(TrailKind::Blood)
            } else if model.has_flag(ModelFlags::ZOMGIB) {
                Some(TrailKind::BloodSlight)
            } else if model.has_flag(ModelFlags::TRACER) {
                Some(TrailKind::TracerGreen)
            } else if model.has_flag(ModelFlags::TRACER2) {
                Some(TrailKind::TracerRed)
            } else if model.has_flag(ModelFlags::ROCKET) {
                ent.light_id = Some(self.lights.insert(
                    self.time,
                    LightDesc {
                        origin: ent.origin,
                        init_radius: 200.0,
                        decay_rate: 0.0,
                        min_radius: None,
                        ttl: Duration::try_milliseconds(10).unwrap(),
                    },
                    ent.light_id,
                ));
                Some(TrailKind::Rocket)
            } else if model.has_flag(ModelFlags::GRENADE) {
                Some(TrailKind::Smoke)
            } else if model.has_flag(ModelFlags::TRACER3) {
                Some(TrailKind::Vore)
            } else {
                None
            };

            // if the entity leaves a trail, generate it
            if let Some(kind) = trail_kind {
                self.particles
                    .create_trail(self.time, prev_origin, ent.origin, kind, false);
            }

            // don't render the player model
            if self.view.entity_id() != ent.id {
                // mark entity for rendering
                self.visible_entity_ids.push_back(ent.id);
            }

            // enable lerp for next frame
            ent.force_link = false;
        }

        // apply effects to static entities as well
        for ent in self.static_entities.iter_mut() {
            if ent.effects.contains(EntityEffects::BRIGHT_LIGHT) {
                debug!("spawn bright light on static entity");
                ent.light_id = Some(self.lights.insert(
                    self.time,
                    LightDesc {
                        origin: ent.origin,
                        init_radius: BRIGHTLIGHT_DISTRIBUTION.sample(&mut self.rng),
                        decay_rate: 0.0,
                        min_radius: None,
                        ttl: Duration::try_milliseconds(1).unwrap(),
                    },
                    ent.light_id,
                ));
            }

            if ent.effects.contains(EntityEffects::DIM_LIGHT) {
                debug!("spawn dim light on static entity");
                ent.light_id = Some(self.lights.insert(
                    self.time,
                    LightDesc {
                        origin: ent.origin,
                        init_radius: MFLASH_DIMLIGHT_DISTRIBUTION.sample(&mut self.rng),
                        decay_rate: 0.0,
                        min_radius: None,
                        ttl: Duration::try_milliseconds(1).unwrap(),
                    },
                    ent.light_id,
                ));
            }
        }

        Ok(())
    }

    pub fn update_temp_entities(&mut self) -> Result<(), ClientError> {
        static ANGLE_DISTRIBUTION: LazyLock<Uniform<f32>> =
            LazyLock::new(|| Uniform::new(0.0, 360.0).unwrap());

        self.temp_entities.clear();
        for id in 0..self.beams.len() {
            // remove beam if expired
            if self.beams[id].is_some_and(|b| b.expire < self.time) {
                self.beams[id] = None;
                continue;
            }

            let view_ent = self.view_entity_id();
            if let Some(ref mut beam) = self.beams[id] {
                // keep lightning gun bolts fixed to player
                if beam.entity_id == view_ent {
                    beam.start = self.entities[view_ent].origin;
                }

                let vec = beam.end - beam.start;
                let yaw = vec.y.atan2(vec.x).to_degrees().rem_euclid(360.);
                let forward = (vec.x.powf(2.0) + vec.y.powf(2.0)).sqrt();
                let pitch = vec.z.atan2(forward).to_degrees().rem_euclid(360.);

                let len = vec.length();
                let direction = vec.normalize();
                for interval in 0..(len / 30.0) as i32 {
                    let id = self.temp_entities.len();
                    let mut ent = ClientEntity::uninitialized(id);
                    ent.origin = beam.start + 30.0 * interval as f32 * direction;
                    ent.angles = Vec3::new(pitch, yaw, ANGLE_DISTRIBUTION.sample(&mut self.rng));

                    if self.temp_entities.len() < MAX_TEMP_ENTITIES {
                        self.temp_entities.push_back(ent);
                    } else {
                        warn!("too many temp entities!");
                    }
                }
            }
        }

        Ok(())
    }

    pub fn update_player(&mut self, update: PlayerData) {
        self.view
            .set_view_height(update.view_height.unwrap_or(net::DEFAULT_VIEWHEIGHT));
        self.view.set_ideal_pitch(update.ideal_pitch.unwrap_or(0.));
        self.view.set_punch_angles(Angles {
            pitch: update.punch_pitch.unwrap_or(0.),
            roll: update.punch_roll.unwrap_or(0.),
            yaw: update.punch_yaw.unwrap_or(0.),
        });

        // store old velocity
        self.msg_velocity[1] = self.msg_velocity[0];
        self.msg_velocity[0].x = update.velocity_x.unwrap_or_default();
        self.msg_velocity[0].y = update.velocity_y.unwrap_or_default();
        self.msg_velocity[0].z = update.velocity_z.unwrap_or_default();

        let item_diff = update.items - self.items;
        if !item_diff.is_empty() {
            // item flags have changed, something got picked up
            let bits = item_diff.bits();
            for i in 0..net::MAX_ITEMS {
                if bits & 1 << i != 0 {
                    // item with flag value `i` was picked up
                    self.item_get_time[i] = self.time;
                }
            }
        }
        self.items = update.items;

        self.on_ground = update.on_ground;
        self.in_water = update.in_water;

        self.stats[ClientStat::WeaponFrame as usize] =
            update.weapon_frame.unwrap_or_default() as i32;
        self.stats[ClientStat::Armor as usize] = update.armor.unwrap_or_default() as i32;
        self.stats[ClientStat::Weapon as usize] = update.weapon.unwrap_or_default() as i32;
        self.stats[ClientStat::Health as usize] = update.health as i32;
        self.stats[ClientStat::Ammo as usize] = update.ammo as i32;
        self.stats[ClientStat::Shells as usize] = update.ammo_shells as i32;
        self.stats[ClientStat::Nails as usize] = update.ammo_nails as i32;
        self.stats[ClientStat::Rockets as usize] = update.ammo_rockets as i32;
        self.stats[ClientStat::Cells as usize] = update.ammo_cells as i32;

        // TODO: this behavior assumes the `standard_quake` behavior and will likely
        // break with the mission packs
        self.stats[ClientStat::ActiveWeapon as usize] = update.active_weapon as i32;
    }

    pub fn handle_input(
        &mut self,
        registry: &Registry,
        frame_time: Duration,
        move_vars: MoveVars,
        mouse_vars: MouseVars,
        impulse: Option<u8>,
    ) -> ClientCmd {
        let mlook = registry.is_pressed("mlook");
        self.view.handle_input(
            frame_time,
            &*registry,
            self.intermission.as_ref(),
            mlook,
            move_vars.cl_anglespeedkey,
            move_vars.cl_pitchspeed,
            move_vars.cl_yawspeed,
            mouse_vars,
        );

        let mut move_left = registry.is_pressed("moveleft");
        let mut move_right = registry.is_pressed("moveright");
        if registry.is_pressed("strafe") {
            move_left |= registry.is_pressed("left");
            move_right |= registry.is_pressed("right");
        }

        let mut sidemove = move_vars.cl_sidespeed * (move_right as i32 - move_left as i32) as f32;

        let mut upmove = move_vars.cl_upspeed
            * (registry.is_pressed("moveup") as i32 - registry.is_pressed("movedown") as i32)
                as f32;

        let mut forwardmove = 0.0;
        if !registry.is_pressed("klook") {
            forwardmove += move_vars.cl_forwardspeed * registry.is_pressed("forward") as i32 as f32;
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

        if !mlook {
            // TODO: IN_Move (mouse / joystick / gamepad)
        }

        let send_time = self.msg_times[0];
        // send "raw" angles without any pitch/roll from movement or damage
        let angles = self.view.input_angles();

        ClientCmd::Move {
            send_time,
            angles: Vec3::new(angles.pitch, angles.yaw, angles.roll),
            fwd_move: forwardmove as i16,
            side_move: sidemove as i16,
            up_move: upmove as i16,
            button_flags,
            // TODO: Is `impulse 0` correct?
            impulse: impulse.unwrap_or_default(),
        }
    }

    pub fn handle_damage(&mut self, armor: u8, health: u8, source: Vec3, kick_vars: KickVars) {
        self.face_anim_time = self.time + Duration::try_milliseconds(200).unwrap();

        let dmg_factor = (armor + health).min(20) as f32 / 2.0;
        let mut cshift = self.color_shifts[ColorShiftCode::Damage as usize];
        cshift.percent += 3 * dmg_factor as i32;
        cshift.percent = cshift.percent.clamp(0, 150);

        if armor > health {
            cshift.dest_color = [200, 100, 100];
        } else if armor > 0 {
            cshift.dest_color = [220, 50, 50];
        } else {
            cshift.dest_color = [255, 0, 0];
        }

        let v_ent = &self.entities[self.view.entity_id()];

        let v_angles = Angles {
            pitch: v_ent.angles.x,
            roll: v_ent.angles.z,
            yaw: v_ent.angles.y,
        };

        self.view.handle_damage(
            self.time,
            armor as f32,
            health as f32,
            v_ent.origin,
            v_angles,
            source,
            kick_vars,
        );
    }

    pub fn calc_final_view(
        &mut self,
        idle_vars: IdleVars,
        kick_vars: KickVars,
        roll_vars: RollVars,
        bob_vars: BobVars,
    ) {
        self.view.calc_final_angles(
            self.time,
            self.intermission.as_ref(),
            self.velocity,
            idle_vars,
            kick_vars,
            roll_vars,
        );
        if let Some(e) = self.entities.get(self.view.entity_id()) {
            self.view
                .calc_final_origin(self.time, e.origin, self.velocity, bob_vars);
        }
    }

    /// Spawn an entity with the given ID, also spawning any uninitialized
    /// entities between the former last entity and the new one.
    // TODO: skipping entities indicates that the entities have been freed by
    // the server. it may make more sense to use a HashMap to store entities by
    // ID since the lookup table is relatively sparse.
    pub fn spawn_entities(&mut self, id: usize, baseline: EntityState) -> Result<(), ClientError> {
        // don't clobber existing entities
        // if id < self.entities.len() {
        //     Err(ClientError::EntityExists(id))?;
        // }

        // spawn intermediate entities (uninitialized)
        self.entities.extend((self.entities.len()..id).map(|i| {
            debug!("Spawning uninitialized entity with ID {}", i);
            ClientEntity::uninitialized(i)
        }));

        debug!(
            "Spawning entity with id {} from baseline {:?}",
            id, baseline
        );
        self.entities
            .push_back(ClientEntity::from_baseline(id, baseline));

        Ok(())
    }

    pub fn update_entity(&mut self, id: usize, update: EntityUpdate) -> Result<(), ClientError> {
        if id >= self.entities.len() {
            let baseline = EntityState {
                origin: Vec3::new(
                    update.origin_x.unwrap_or_default(),
                    update.origin_y.unwrap_or_default(),
                    update.origin_z.unwrap_or_default(),
                ),
                angles: Vec3::new(
                    update.pitch.unwrap_or(0.),
                    update.yaw.unwrap_or(0.),
                    update.roll.unwrap_or(0.),
                ),
                model_id: update.model_id.unwrap_or_default() as usize,
                frame_id: update.frame_id.unwrap_or_default() as usize,
                colormap: update.colormap.unwrap_or_default(),
                skin_id: update.skin_id.unwrap_or_default() as usize,
                effects: EntityEffects::empty(),
            };

            self.spawn_entities(id, baseline)?;
        }

        let entity = &mut self.entities[id];
        entity.update(self.msg_times, update);
        if entity.model_changed() {
            match self.models[entity.model_id].kind() {
                ModelKind::None => (),
                _ => {
                    entity.sync_base = match self.models[entity.model_id].sync_type() {
                        SyncType::Sync => Duration::zero(),
                        SyncType::Rand => unimplemented!(), // TODO
                    }
                }
            }
        }

        if let Some(c) = entity.colormap() {
            if let Some(e) = self.entities.get_mut(id) {
                e.colormap = Some(c);
            }
        }

        Ok(())
    }

    pub fn spawn_temp_entity(
        &mut self,
        events: &mut EventWriter<MixerEvent>,
        temp_entity: &TempEntity,
    ) {
        static ZERO_ONE_DISTRIBUTION: LazyLock<Uniform<f32>> =
            LazyLock::new(|| Uniform::new(0.0, 1.0).unwrap());

        let mut spike_sound = || match ZERO_ONE_DISTRIBUTION.sample(&mut self.rng) {
            x if x < 0.2 => "weapons/tink1.wav",
            x if x < 0.4667 => "weapons/ric1.wav",
            x if x < 0.7333 => "weapons/ric2.wav",
            _ => "weapons/ric3.wav",
        };

        match temp_entity {
            TempEntity::Point { kind, origin } => {
                use PointEntityKind::*;
                match kind {
                    // projectile impacts
                    WizSpike | KnightSpike | Spike | SuperSpike | Gunshot => {
                        let (color, count, sound) = match kind {
                            // TODO: start wizard/hit.wav
                            WizSpike => (20, 30, Some("wizard/hit.wav")),

                            KnightSpike => (226, 20, Some("hknight/hit.wav")),

                            // TODO: for Spike and SuperSpike, start one of:
                            // - 26.67%: weapons/tink1.wav
                            // - 20.0%: weapons/ric1.wav
                            // - 20.0%: weapons/ric2.wav
                            // - 20.0%: weapons/ric3.wav
                            Spike => (0, 10, Some(spike_sound())),
                            SuperSpike => (0, 20, Some(spike_sound())),

                            // no impact sound
                            Gunshot => (0, 20, None),
                            _ => unreachable!(),
                        };

                        self.particles.create_projectile_impact(
                            self.time,
                            *origin,
                            Vec3::ZERO,
                            color,
                            count,
                        );

                        if let Some(snd) = sound {
                            events.write(MixerEvent::StartSound(StartSound {
                                src: self.cached_sounds.get(snd).unwrap().clone(),
                                ent_id: None,
                                ent_channel: 0,
                                volume: 1.0,
                                attenuation: 1.0,
                                origin: (*origin).into(),
                            }));
                        }
                    }

                    Explosion => {
                        self.particles.create_explosion(self.time, *origin);
                        self.lights.insert(
                            self.time,
                            LightDesc {
                                origin: *origin,
                                init_radius: 350.0,
                                decay_rate: 300.0,
                                min_radius: None,
                                ttl: Duration::try_milliseconds(500).unwrap(),
                            },
                            None,
                        );

                        events.write(MixerEvent::StartSound(StartSound {
                            src: self
                                .cached_sounds
                                .get("weapons/r_exp3.wav")
                                .unwrap()
                                .clone(),
                            ent_id: None,
                            ent_channel: 0,
                            volume: 1.0,
                            attenuation: 1.0,
                            origin: (*origin).into(),
                        }));
                    }

                    ColorExplosion {
                        color_start,
                        color_len,
                    } => {
                        self.particles.create_color_explosion(
                            self.time,
                            *origin,
                            (*color_start)..=(*color_start + *color_len - 1),
                        );
                        self.lights.insert(
                            self.time,
                            LightDesc {
                                origin: *origin,
                                init_radius: 350.0,
                                decay_rate: 300.0,
                                min_radius: None,
                                ttl: Duration::try_milliseconds(500).unwrap(),
                            },
                            None,
                        );

                        events.write(MixerEvent::StartSound(StartSound {
                            src: self
                                .cached_sounds
                                .get("weapons/r_exp3.wav")
                                .unwrap()
                                .clone(),
                            ent_id: None,
                            ent_channel: 0,
                            volume: 1.0,
                            attenuation: 1.0,
                            origin: (*origin).into(),
                        }));
                    }

                    TarExplosion => {
                        self.particles.create_spawn_explosion(self.time, *origin);

                        events.write(MixerEvent::StartSound(StartSound {
                            src: self
                                .cached_sounds
                                .get("weapons/r_exp3.wav")
                                .unwrap()
                                .clone(),
                            ent_id: None,
                            ent_channel: 0,
                            volume: 1.0,
                            attenuation: 1.0,
                            origin: (*origin).into(),
                        }));
                    }

                    LavaSplash => self.particles.create_lava_splash(self.time, *origin),
                    Teleport => self.particles.create_teleporter_warp(self.time, *origin),
                }
            }

            TempEntity::Beam {
                kind,
                entity_id,
                start,
                end,
            } => {
                use BeamEntityKind::*;
                let model_name = match kind {
                    Lightning { model_id } => format!(
                        "progs/bolt{}.mdl",
                        match model_id {
                            1 => "",
                            2 => "2",
                            3 => "3",
                            x => panic!("invalid lightning model id: {}", x),
                        }
                    ),
                    Grapple => "progs/beam.mdl".to_string(),
                };

                if let Some(beam) = self.model_names.get(&model_name) {
                    self.spawn_beam(self.time, *entity_id as usize, *beam, *start, *end);
                }
            }
        }
    }

    pub fn spawn_beam(
        &mut self,
        time: Duration,
        entity_id: usize,
        model_id: usize,
        start: Vec3,
        end: Vec3,
    ) {
        // always override beam with same entity_id if it exists
        // otherwise use the first free slot
        let mut free = None;
        for i in 0..self.beams.len() {
            if let Some(ref mut beam) = self.beams[i] {
                if beam.entity_id == entity_id {
                    beam.model_id = model_id;
                    beam.expire = time + Duration::try_milliseconds(200).unwrap();
                    beam.start = start;
                    beam.end = end;
                }
            } else if free.is_none() {
                free = Some(i);
            }
        }

        if let Some(i) = free {
            self.beams[i] = Some(Beam {
                entity_id,
                model_id,
                expire: time + Duration::try_milliseconds(200).unwrap(),
                start,
                end,
            });
        } else {
            warn!("No free beam slots!");
        }
    }

    #[must_use]
    pub fn update_listener(&self) -> Option<Listener> {
        // TODO: update to self.view_origin()
        let origin = self.entities.get(self.view.entity_id()).map(|e| e.origin)?;
        let world_translate = Mat4::from_translation(origin);

        let left_base = Vec3::new(0., 4., self.view.view_height());
        let right_base = Vec3::new(0., -4., self.view.view_height());

        let rotation = Quat::from_mat4(&self.view.input_angles().mat4_quake());

        let left_ear = (world_translate * (rotation * left_base).extend(1.)).truncate();
        let right_ear = (world_translate * (rotation * right_base).extend(1.)).truncate();

        Some(Listener {
            origin,
            rotation,
            left_ear,
            right_ear,
        })
    }

    fn view_leaf_contents(&self) -> Result<bsp::BspLeafContents, ClientError> {
        match self.models.get(1).map(|m| m.kind()) {
            Some(ModelKind::Brush(bmodel)) => {
                let bsp_data = bmodel.bsp_data();
                if let Some(leaf_id) = self
                    .entities
                    .get(self.view.entity_id())
                    .map(|e| bsp_data.find_leaf(e.origin))
                {
                    let leaf = &bsp_data.leaves()[leaf_id];
                    Ok(leaf.contents)
                } else {
                    Ok(bsp::BspLeafContents::Empty)
                }
            }
            None => Ok(bsp::BspLeafContents::Empty),
            _ => panic!("non-brush worldmodel"),
        }
    }

    pub fn update_color_shifts(&mut self, frame_time: Duration) -> Result<(), ClientError> {
        let float_time = engine::duration_to_f32(frame_time);

        // set color for leaf contents
        self.color_shifts[ColorShiftCode::Contents as usize] = match self.view_leaf_contents()? {
            bsp::BspLeafContents::Empty => ColorShift {
                dest_color: [0, 0, 0],
                percent: 0,
            },
            bsp::BspLeafContents::Lava => ColorShift {
                dest_color: [255, 80, 0],
                percent: 150,
            },
            bsp::BspLeafContents::Slime => ColorShift {
                dest_color: [0, 25, 5],
                percent: 150,
            },
            _ => ColorShift {
                dest_color: [130, 80, 50],
                percent: 128,
            },
        };

        // decay damage and item pickup shifts
        // always decay at least 1 "percent" (actually 1/255)
        // TODO: make percent an actual percent ([0.0, 1.0])
        let dmg_shift = &mut self.color_shifts[ColorShiftCode::Damage as usize];
        dmg_shift.percent -= ((float_time * 150.0) as i32).max(1);
        dmg_shift.percent = dmg_shift.percent.max(0);

        let bonus_shift = &mut self.color_shifts[ColorShiftCode::Bonus as usize];
        bonus_shift.percent -= ((float_time * 100.0) as i32).max(1);
        bonus_shift.percent = bonus_shift.percent.max(0);

        // set power-up overlay
        self.color_shifts[ColorShiftCode::Powerup as usize] =
            if self.items.contains(ItemFlags::QUAD) {
                ColorShift {
                    dest_color: [0, 0, 255],
                    percent: 30,
                }
            } else if self.items.contains(ItemFlags::SUIT) {
                ColorShift {
                    dest_color: [0, 255, 0],
                    percent: 20,
                }
            } else if self.items.contains(ItemFlags::INVISIBILITY) {
                ColorShift {
                    dest_color: [100, 100, 100],
                    percent: 100,
                }
            } else if self.items.contains(ItemFlags::INVULNERABILITY) {
                ColorShift {
                    dest_color: [255, 255, 0],
                    percent: 30,
                }
            } else {
                ColorShift {
                    dest_color: [0, 0, 0],
                    percent: 0,
                }
            };

        Ok(())
    }

    /// Update the view angles to the specified value, disabling interpolation.
    pub fn set_view_angles(&mut self, angles: Vec3) {
        self.view.update_input_angles(Angles {
            pitch: angles.x,
            roll: angles.z,
            yaw: angles.y,
        });
        let final_angles = self.view.final_angles();
        if let Some(e) = self.entities.get_mut(self.view.entity_id()) {
            e.set_angles(Vec3::new(
                final_angles.pitch,
                final_angles.yaw,
                final_angles.roll,
            ));
        }
    }

    /// Update the view angles to the specified value, enabling interpolation.
    pub fn update_view_angles(&mut self, angles: Vec3) {
        self.view.update_input_angles(Angles {
            pitch: angles.x,
            roll: angles.z,
            yaw: angles.y,
        });
        let final_angles = self.view.final_angles();
        self.entities[self.view.entity_id()].update_angles(Vec3::new(
            final_angles.pitch,
            final_angles.yaw,
            final_angles.roll,
        ));
    }

    pub fn set_view_entity(&mut self, entity_id: usize) -> Result<(), ClientError> {
        // view entity may not have been spawned yet, so check
        // against both max_players and the current number of
        // entities
        if entity_id > self.max_players && entity_id >= self.entities.len() {
            Err(ClientError::InvalidViewEntity(entity_id))?;
        }
        self.view.set_entity_id(entity_id);
        Ok(())
    }

    pub fn models(&self) -> &im::Vector<Model> {
        &self.models
    }

    pub fn viewmodel_id(&self) -> usize {
        match self.stats[ClientStat::Weapon as usize] as usize {
            0 => 0,
            x => x - 1,
        }
    }

    pub fn iter_visible_entities(&self) -> impl Iterator<Item = &ClientEntity> {
        self.visible_entity_ids
            .iter()
            .map(move |i| &self.entities[*i])
            .chain(self.temp_entities.iter())
            .chain(self.static_entities.iter())
    }

    pub fn iter_particles(&self) -> impl Iterator<Item = &Particle> {
        self.particles.iter()
    }

    pub fn iter_lights(&self) -> impl Iterator<Item = &Light> {
        self.lights.iter()
    }

    pub fn time(&self) -> Duration {
        self.time
    }

    pub fn view_entity_id(&self) -> usize {
        self.view.entity_id()
    }

    pub fn camera(&self, aspect: f32, fov_deg: f32) -> Camera {
        let fov_y = math::fov_x_to_fov_y(fov_deg, aspect).unwrap();
        Camera::new(
            self.view.final_origin(),
            self.view.final_angles(),
            Mat4::perspective_rh_gl(fov_y, aspect, 4.0, 4096.0),
        )
    }

    pub fn demo_camera(&self, aspect: f32, fov_deg: f32) -> Camera {
        let fov_y = math::fov_x_to_fov_y(fov_deg, aspect).unwrap();
        let angles = self
            .entities
            .get(self.view.entity_id())
            .map(|e| Angles {
                pitch: e.angles.x,
                roll: e.angles.z,
                yaw: e.angles.y,
            })
            .unwrap_or_default();
        Camera::new(
            self.view.final_origin(),
            angles,
            Mat4::perspective_rh_gl(fov_y, aspect, 4.0, 4096.0),
        )
    }

    pub fn lightstyle_values(&self) -> ArrayVec<f32, MAX_LIGHT_STYLES> {
        let float_time = engine::duration_to_f32(self.time);
        // 'z' - 'a' = 25, so divide by 12.5 to get range [0, 2]
        let factor = ((b'z' - b'a') as f32 / 2.).recip();
        self.light_styles
            .iter()
            .map(move |ls| {
                let frame = if ls.len() == 0 {
                    None
                } else {
                    Some((float_time * 10.0) as usize % ls.len())
                };

                frame
                    .map(|f| (ls.as_bytes()[f] - b'a') as f32 * factor)
                    .unwrap_or(1.)
            })
            .collect()
    }

    pub fn intermission(&self) -> Option<&IntermissionKind> {
        self.intermission.as_ref()
    }

    pub fn start_time(&self) -> Duration {
        self.start_time
    }

    pub fn completion_time(&self) -> Option<Duration> {
        self.completion_time
    }

    pub fn stats(&self) -> &[i32] {
        &self.stats
    }

    pub fn items(&self) -> ItemFlags {
        self.items
    }

    pub fn item_pickup_times(&self) -> &[Duration] {
        &self.item_get_time
    }

    pub fn face_anim_time(&self) -> Duration {
        self.face_anim_time
    }

    pub fn color_shift(&self) -> [f32; 4] {
        self.color_shifts.iter().fold([0.0; 4], |accum, elem| {
            let elem_a = elem.percent as f32 / 255.0 / 2.0;
            if elem_a == 0.0 {
                return accum;
            }
            let in_a = accum[3];
            let out_a = in_a + elem_a * (1.0 - in_a);
            let color_factor = elem_a / out_a;

            let mut out = [0.0; 4];
            for i in 0..3 {
                out[i] = accum[i] * (1.0 - color_factor)
                    + elem.dest_color[i] as f32 / 255.0 * color_factor;
            }
            out[3] = out_a.min(1.0).max(0.0);
            out
        })
    }

    pub fn check_entity_id(&self, id: usize) -> Result<(), ClientError> {
        match id {
            0 => Err(ClientError::NullEntity),
            e if e >= self.entities.len() => Err(ClientError::NoSuchEntity(id)),
            _ => Ok(()),
        }
    }

    pub fn check_player_id(&self, id: usize) -> Result<(), ClientError> {
        if id >= net::MAX_CLIENTS {
            Err(ClientError::NoSuchClient(id))
        } else if id > self.max_players {
            Err(ClientError::NoSuchPlayer(id))
        } else {
            Ok(())
        }
    }
}

pub mod systems {}
