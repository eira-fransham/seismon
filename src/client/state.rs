use std::{iter, mem};

use super::view::BobVars;
use crate::{
    client::{
        ClientError, Connection,
        interpolation::{Next, NoInterpolation},
        inventory::{Health, RemoveWeapon, UpdateAmmoCount},
        view::{IdleVars, KickVars, RollVars},
    },
    common::net::{EntityState, EntityUpdate, ItemFlags, PlayerColor, PlayerData},
};
use bevy::{
    asset::{AssetPath, AssetServer, Handle},
    camera::visibility::Visibility,
    ecs::{
        component::Component,
        entity::Entity,
        hierarchy::{ChildOf, Children},
        name::Name,
        reflect::ReflectComponent,
        system::{Commands, EntityCommands, In, Query, Res, ResMut},
    },
    log::*,
    math::{EulerRot, Quat, Vec3},
    reflect::Reflect,
    scene::{Scene, SceneRoot},
    transform::components::Transform,
    utils::default,
};
use bevy_mod_mdl::MdlSettings;
use bevy_seedling::sample::AudioSample;
use bevy_trenchbroom::bsp::Bsp;
use bitvec::vec::BitVec;
use hashbrown::HashMap;
use seismon_utils::{QAngles, QString};

/// When certain temporary entities are spawned, Quake has builtin code to
/// start sounds without the server instructing it to.
const TEMP_ENTITY_BUILTIN_SOUNDS: &[&str] = &[
    "hknight/hit.wav",
    "weapons/r_exp3.wav",
    "weapons/ric1.wav",
    "weapons/ric2.wav",
    "weapons/ric3.wav",
    "weapons/tink1.wav",
    "wizard/hit.wav",
];

#[derive(Clone)]
pub struct PlayerInfo {
    pub name: QString,
    pub frags: i32,
    pub colors: PlayerColor,
    // translations: [u8; VID_GRADES],
}

#[derive(Clone)]
pub enum PrecacheModel {
    Invalid,
    WaitingOnWorldspawn(usize),
    Loaded(Handle<Scene>),
}

#[derive(Component, Reflect)]
#[reflect(Component)]
#[require(Transform)]
pub struct Worldspawn {
    pub bsp: Handle<Bsp>,
}

/// Holder for precached models and sounds, to ensure they don't get unloaded. Plus, a map from
/// server entity ID to local [`Entity`].
pub struct ClientState {
    /// The root world entity.
    pub worldspawn: Entity,

    /// The entity that we are viewing the world through.
    pub view: Option<u16>,

    /// Quake 1 deletes any entities that haven't received new messages on a given frame.
    pub frame_keepalive: BitVec,

    /// Model precache.
    pub models: Box<[PrecacheModel]>,

    /// Sound precache.
    pub sounds: Box<[Handle<AudioSample>]>,

    /// Sounds that are always needed, even if not in precache.
    /// This does not need to be read, it's just to ensure that
    /// the handles are not dropped.
    _cached_sounds: Box<[Handle<AudioSample>]>,

    pub server_entity_to_client_entity: HashMap<u16, Entity>,
}

#[derive(Component, Default)]
pub struct PlayerState {
    items: ItemFlags,
}

impl ClientState {
    pub fn populate_precache(&mut self, bsp: &Bsp) {
        for model in &mut self.models {
            if let PrecacheModel::WaitingOnWorldspawn(idx) = *model {
                *model = PrecacheModel::Loaded(bsp.models[idx].clone());
            }
        }
    }

    pub fn from_server_info<SName: AsRef<str>>(
        worldspawn: &mut EntityCommands,
        asset_server: &AssetServer,
        model_precache: Vec<String>,
        sound_precache: Vec<SName>,
    ) -> Result<ClientState, ClientError> {
        trace!("Model precache: {model_precache:?}");

        let mut model_precache = model_precache.into_iter();

        // TODO: Better error
        let worldspawn_path =
            model_precache.next().ok_or_else(|| ClientError::InvalidConnectResponse)?;

        let worldspawn_path = AssetPath::parse(&worldspawn_path).into_owned();

        worldspawn.insert((
            Name::new("worldspawn"),
            Worldspawn { bsp: asset_server.load(worldspawn_path) },
        ));

        // TODO: validate submodel names
        let models = [PrecacheModel::Invalid, PrecacheModel::WaitingOnWorldspawn(0)]
            .into_iter()
            .chain(model_precache.map(|model_name| {
                if model_name.ends_with(".bsp") {
                    // TODO: We want the worldspawn to be `Model0` but other BSPs to be all
                    // models

                    PrecacheModel::Loaded(
                        asset_server
                            .load(AssetPath::parse(&model_name).into_owned().with_label("Model0")),
                    )
                } else if let Some(model_idx) = model_name.strip_prefix('*') {
                    PrecacheModel::WaitingOnWorldspawn(
                        model_idx.parse().expect("TODO: Handle this error"),
                    )
                } else {
                    PrecacheModel::Loaded(asset_server.load(model_name))
                }
            }))
            .collect();

        let sounds = iter::once("misc/null.wav")
            .chain(sound_precache.iter().map(AsRef::as_ref))
            .enumerate()
            .map(|(i, snd_name)| {
                debug!("Loading sound {}: {}", i, snd_name);
                asset_server.load(format!("sound/{snd_name}"))
                // TODO: send keepalive message?
            });

        let cached_sounds = TEMP_ENTITY_BUILTIN_SOUNDS
            .iter()
            .copied()
            .map(|snd_name| asset_server.load(format!("sound/{snd_name}")))
            .collect();

        Ok(ClientState {
            view: None,
            worldspawn: worldspawn.id(),
            models,
            frame_keepalive: default(),
            sounds: sounds.collect(),
            _cached_sounds: cached_sounds,
            server_entity_to_client_entity: Default::default(),
        })
    }

    pub fn dead_entities(&mut self) -> impl Iterator<Item = Entity> {
        let keepalive = mem::take(&mut self.frame_keepalive);

        self.server_entity_to_client_entity.iter().filter_map(move |(k, v)| {
            if *keepalive.get(*k as usize).as_deref().unwrap_or(&false) { None } else { Some(*v) }
        })
    }

    pub fn mark_entity_alive(&mut self, ent_id: u16) {
        self.frame_keepalive.resize(self.frame_keepalive.len().max(ent_id as usize + 1), false);
        self.frame_keepalive.set(ent_id as usize, true);
    }

    pub fn handle_damage(&mut self, _armor: u8, _health: u8, _source: Vec3, _kick_vars: KickVars) {
        // const DMG_DENSITY: f32 = 0.03;

        // self.face_anim_time = self.time + Duration::try_milliseconds(200).unwrap();

        // let dmg_factor = (armor + health).min(20) as f32 / 2.0;
        // let mut cshift = self.color_shifts[ColorShiftCode::Damage as usize];
        // cshift.density += DMG_DENSITY * dmg_factor;
        // cshift.density = cshift.density.clamp(0., 1.5);
        // // Takes one second to decay no matter how intense.
        // cshift.decay = cshift.density;

        // if armor > health {
        //     cshift.dest_color = [200, 100, 100];
        // } else if armor > 0 {
        //     cshift.dest_color = [220, 50, 50];
        // } else {
        //     cshift.dest_color = [255, 0, 0];
        // }

        // let Some(view) = self.view.as_mut() else {
        //     return;
        // };
        // let v_ent = &self.entities[view.entity_id()];

        // let v_angles = Angles {
        //     pitch: v_ent.angles.x,
        //     yaw: v_ent.angles.y,
        //     roll: v_ent.angles.z,
        // };

        // view.handle_damage(
        //     self.time,
        //     armor as f32,
        //     health as f32,
        //     v_ent.origin,
        //     v_angles,
        //     source,
        //     kick_vars,
        // );
    }

    pub fn calc_final_view(
        &mut self,
        _idle_vars: IdleVars,
        _kick_vars: KickVars,
        _roll_vars: RollVars,
        _bob_vars: BobVars,
    ) {
        // let Some(view) = self.view.as_mut() else {
        //     return;
        // };

        // view.calc_final_angles(
        //     self.time,
        //     self.intermission.as_ref(),
        //     self.velocity,
        //     idle_vars,
        //     kick_vars,
        //     roll_vars,
        // );

        // if let Some(e) = self.entities.get(view.entity_id()) {
        //     view.calc_final_origin(self.time, e.origin, self.velocity, bob_vars);
        // }
    }

    /// Spawn an entity with the given ID, also spawning any uninitialized
    /// entities between the former last entity and the new one.
    pub fn spawn_entities(
        &mut self,
        mut commands: Commands<'_, '_>,
        id: u16,
        baseline: EntityState,
        msg_time: f64,
    ) {
        let EntityState {
            origin,
            angles,
            model_id,

            // TODO
            frame_id: _,
            colormap: _,
            skin_id: _,
            effects: _,
        } = baseline;

        let [pitch, yaw, roll] = angles.map(|x| x.to_radians()).into();

        if id == 0 {
            trace!("Spawning world: {baseline:?}");
        }

        let model = self.models.get(model_id).cloned();
        let transform = Transform::from_xyz(origin.x, origin.y, origin.z)
            .with_rotation(QAngles { roll_deg: roll, pitch_deg: pitch, yaw_deg: yaw }.into());
        let transform = Next { component: transform, elapsed_secs: msg_time };

        let mut ent = if let Some(ent) = self.server_entity_to_client_entity.get(&id) {
            let mut entity = commands.entity(*ent);

            entity.insert(transform);

            entity
        } else {
            let entity =
                commands.spawn((transform, Visibility::Inherited, ChildOf(self.worldspawn)));

            // Special-case: bsp model is spawned with `SpawnBaseline` (not `SpawnStatic`) but
            // not handled via the regular update loop.
            if id != 0 {
                self.server_entity_to_client_entity.insert(id, entity.id());
            }

            entity
        };

        if let Some(PrecacheModel::Loaded(model)) = model {
            ent.insert(SceneRoot(model));
        } else {
            error!("Tried to insert model but it wasn't loaded yet");
        }
    }

    /// Sets the entity represented by the server-side `ent_id` to be the current player.
    pub fn update_player(
        In(player_data): In<PlayerData>,
        conn: Res<Connection>,
        mut existing_data: Query<(&mut PlayerState, &mut Health)>,
        mut commands: Commands<'_, '_>,
    ) {
        use crate::client::inventory::{
            AddAmmo, AddWeapon, Cells, GrenadeLauncher, LightningGun, Nailgun, Nails,
            RocketLauncher, Rockets, Shells, Shotgun, SuperNailgun, SuperShotgun,
        };

        let Some(state) = conn.client_state.as_ref() else {
            return;
        };

        let Some(ent) = state
            .view
            .and_then(|ent_id| state.server_entity_to_client_entity.get(&ent_id))
            .copied()
        else {
            error!("No view entity to update");
            return;
        };

        let existing_flags = if let Ok((mut existing, mut health)) = existing_data.get_mut(ent) {
            health.0 = player_data.health;
            std::mem::replace(&mut existing.items, player_data.items)
        } else {
            commands
                .entity(ent)
                .insert((PlayerState { items: player_data.items }, Health(player_data.health)));
            commands.queue(AddAmmo::<Shells>::new(ent));
            commands.queue(AddAmmo::<Nails>::new(ent));
            commands.queue(AddAmmo::<Rockets>::new(ent));
            commands.queue(AddAmmo::<Cells>::new(ent));
            Default::default()
        };

        let new_flags = player_data.items.difference(existing_flags);

        for new_flag in new_flags.iter() {
            if new_flag == ItemFlags::SHOTGUN {
                commands.queue(AddWeapon::<Shotgun>::new(ent));
            } else if new_flag == ItemFlags::SUPER_SHOTGUN {
                commands.queue(AddWeapon::<SuperShotgun>::new(ent));
            } else if new_flag == ItemFlags::NAILGUN {
                commands.queue(AddWeapon::<Nailgun>::new(ent));
            } else if new_flag == ItemFlags::SUPER_NAILGUN {
                commands.queue(AddWeapon::<SuperNailgun>::new(ent));
            } else if new_flag == ItemFlags::ROCKET_LAUNCHER {
                commands.queue(AddWeapon::<RocketLauncher>::new(ent));
            } else if new_flag == ItemFlags::GRENADE_LAUNCHER {
                commands.queue(AddWeapon::<GrenadeLauncher>::new(ent));
            } else if new_flag == ItemFlags::LIGHTNING {
                commands.queue(AddWeapon::<LightningGun>::new(ent));
            }
        }

        let removed_flags = existing_flags.difference(player_data.items);

        for removed_flag in removed_flags.iter() {
            if removed_flag == ItemFlags::SHOTGUN {
                commands.queue(RemoveWeapon::<Shotgun>::new(ent));
            } else if removed_flag == ItemFlags::SUPER_SHOTGUN {
                commands.queue(RemoveWeapon::<SuperShotgun>::new(ent));
            } else if removed_flag == ItemFlags::NAILGUN {
                commands.queue(RemoveWeapon::<Nailgun>::new(ent));
            } else if removed_flag == ItemFlags::SUPER_NAILGUN {
                commands.queue(RemoveWeapon::<SuperNailgun>::new(ent));
            } else if removed_flag == ItemFlags::ROCKET_LAUNCHER {
                commands.queue(RemoveWeapon::<RocketLauncher>::new(ent));
            } else if removed_flag == ItemFlags::GRENADE_LAUNCHER {
                commands.queue(RemoveWeapon::<GrenadeLauncher>::new(ent));
            } else if removed_flag == ItemFlags::LIGHTNING {
                commands.queue(RemoveWeapon::<LightningGun>::new(ent));
            }
        }

        if player_data.items.contains(ItemFlags::SHELLS) {
            commands.queue(UpdateAmmoCount::<Shells>::new(ent, player_data.ammo_shells));
        }

        if player_data.items.contains(ItemFlags::NAILS) {
            commands.queue(UpdateAmmoCount::<Nails>::new(ent, player_data.ammo_nails));
        }

        if player_data.items.contains(ItemFlags::ROCKETS) {
            commands.queue(UpdateAmmoCount::<Rockets>::new(ent, player_data.ammo_rockets));
        }

        if player_data.items.contains(ItemFlags::CELLS) {
            commands.queue(UpdateAmmoCount::<Cells>::new(ent, player_data.ammo_cells));
        }
    }

    pub fn update_entity(
        In((update, msg_time)): In<(EntityUpdate, f64)>,
        mut commands: Commands<'_, '_>,
        mut conn: ResMut<Connection>,
        mut existing_entities: Query<&mut Next<Transform>>,
        children: Query<&Children>,
        mut models: Query<&mut MdlSettings>,
    ) {
        let Some(state) = conn.client_state.as_mut() else {
            return;
        };

        if let Some(entity) = state.server_entity_to_client_entity.get(&update.ent_id) {
            if commands.get_entity(*entity).is_err() {
                warn!("Server tried to update non-existent entity {}", update.ent_id);
                return;
            };

            if let Some(model_id) = update.model_id
                && let Some(PrecacheModel::Loaded(model)) =
                    state.models.get(model_id as usize).cloned()
            {
                commands.entity(*entity).insert(SceneRoot(model));
            }

            let mut ent = commands.entity(*entity);

            if !update.any() {
                return;
            }

            if update.no_lerp {
                ent.insert(NoInterpolation);
            } else {
                ent.remove::<NoInterpolation>();
            }

            if let Ok(mut transform) = existing_entities.get_mut(*entity) {
                if let Some(new_frame) = update.frame_id {
                    for child in children.iter_descendants(*entity) {
                        if let Ok(mut mdl) = models.get_mut(child) {
                            mdl.frame = new_frame as usize;
                        }
                    }
                }

                let mut new_translation = transform.component.translation;

                if let Some(o_x) = update.origin_x {
                    new_translation = new_translation.with_x(o_x);
                }
                if let Some(o_y) = update.origin_y {
                    new_translation = new_translation.with_y(o_y);
                }
                if let Some(o_z) = update.origin_z {
                    new_translation = new_translation.with_z(o_z);
                }

                let new_transform = transform.component.with_translation(new_translation);

                let mut angles: QAngles = transform.component.rotation.into();
                if let Some(new_pitch) = update.pitch {
                    angles.pitch_deg = new_pitch;
                }
                if let Some(new_yaw) = update.yaw {
                    angles.yaw_deg = new_yaw;
                }
                if let Some(new_roll) = update.roll {
                    angles.roll_deg = new_roll;
                }

                let new_transform = new_transform.with_rotation(angles.into());

                transform.component = new_transform;
                transform.elapsed_secs = msg_time;
            } else {
                let origin = Vec3::new(
                    update.origin_x.unwrap_or_default(),
                    update.origin_y.unwrap_or_default(),
                    update.origin_z.unwrap_or_default(),
                );

                let [pitch, yaw, roll] =
                    [update.pitch, update.yaw, update.roll].map(|a| a.unwrap_or(0.).to_radians());

                ent.insert(Transform::from_xyz(origin.x, origin.y, origin.z).with_rotation(
                    QAngles { roll_deg: roll, pitch_deg: pitch, yaw_deg: yaw }.into(),
                ));
            }
        } else {
            let Some(model_id) = update.model_id else {
                warn!("Tried to spawn ent without model");
                return;
            };

            let origin = Vec3::new(
                update.origin_x.unwrap_or_default(),
                update.origin_y.unwrap_or_default(),
                update.origin_z.unwrap_or_default(),
            );

            let [yaw, pitch, roll] =
                [update.yaw, update.pitch, update.roll].map(|a| a.unwrap_or(0.).to_radians());

            let mut ent = commands.spawn((
                Next {
                    component: Transform::from_xyz(origin.x, origin.y, origin.z).with_rotation(
                        QAngles { roll_deg: roll, pitch_deg: pitch, yaw_deg: yaw }.into(),
                    ),
                    elapsed_secs: msg_time,
                },
                Visibility::Inherited,
                ChildOf(state.worldspawn),
            ));

            if let Some(PrecacheModel::Loaded(model)) =
                update.model_id.and_then(|model_id| state.models.get(model_id as usize).cloned())
            {
                ent.insert(SceneRoot(model));
            } else {
                warn!("Model {model_id} not found (TODO)");
            }

            state.server_entity_to_client_entity.insert(update.ent_id, ent.id());
        }
    }
}

pub mod systems {}
