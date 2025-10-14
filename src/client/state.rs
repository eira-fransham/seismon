use std::{convert::identity, iter};

use super::view::BobVars;
use crate::{
    client::{
        ClientError, Connection,
        view::{IdleVars, KickVars, RollVars},
    },
    common::net::{EntityEffects, EntityState, EntityUpdate, PlayerColor},
};
use beef::Cow;
use bevy::{
    asset::{AssetId, AssetPath, AssetServer, Handle},
    ecs::{
        component::Component,
        entity::Entity,
        system::{Commands, In, Query, ResMut},
    },
    log::*,
    math::{EulerRot, Quat, Vec3},
    scene::{Scene, SceneRoot},
    transform::components::Transform,
};
use bevy_seedling::sample::AudioSample;
use hashbrown::HashMap;
use seismon_utils::QString;

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
pub enum ModelPrecache {
    WaitingOnWorld { pending: PendingSceneRoot },
    Loaded { handle: Handle<Scene> },
}

// HACK: https://github.com/bevyengine/bevy/issues/12756
#[derive(Component, Clone)]
pub struct PendingSceneRoot {
    root: AssetId<Scene>,
    asset: AssetPath<'static>,
}

impl PendingSceneRoot {
    pub fn try_resolve(&self, asset_server: &AssetServer) -> Option<Handle<Scene>> {
        if asset_server.is_loaded_with_dependencies(self.root) {
            Some(self.force_resolve(asset_server))
        } else {
            None
        }
    }

    fn force_resolve(&self, asset_server: &AssetServer) -> Handle<Scene> {
        asset_server.load(&self.asset)
    }
}

/// Holder for precached models and sounds, to ensure they don't get unloaded. Plus, a map from
/// server entity ID to local [`Entity`].
#[derive(Default)]
pub struct ClientState {
    pub worldspawn: Handle<Scene>,

    // Model precache.
    pub models: Box<[Option<ModelPrecache>]>,

    models_all_resolved: bool,

    // Sound precache.
    pub sounds: Box<[Handle<AudioSample>]>,

    /// Sounds that are always needed, even if not in precache.
    /// This does not need to be read, it's just to ensure that
    /// the handles are not dropped.
    _cached_sounds: Box<[Handle<AudioSample>]>,

    pub server_entity_to_client_entity: HashMap<u16, Entity>,
}

impl ClientState {
    // TODO: add parameter for number of player slots and reserve them in entity list
    pub fn new() -> ClientState {
        ClientState::default()
    }

    pub fn try_resolve_all(&mut self, asset_server: &AssetServer) {
        if !self.models_all_resolved
            && asset_server.is_loaded_with_dependencies(self.worldspawn.id())
        {
            self.models_all_resolved = true;

            for model in &mut self.models {
                if let Some(ModelPrecache::WaitingOnWorld { pending }) = model {
                    let handle = pending.force_resolve(asset_server);

                    *model = Some(ModelPrecache::Loaded { handle })
                }
            }
        }
    }

    pub fn from_server_info<SName: AsRef<str>>(
        asset_server: &AssetServer,
        model_precache: Vec<String>,
        sound_precache: Vec<SName>,
    ) -> Result<ClientState, ClientError> {
        info!("Model precache: {model_precache:?}");

        let mut model_precache = model_precache.into_iter();

        // TODO: Better error
        let worldspawn_path =
            model_precache.next().ok_or_else(|| ClientError::InvalidConnectResponse)?;

        let worldspawn_path = AssetPath::parse(&worldspawn_path).into_owned().with_label("Model0");

        let worldspawn = asset_server.load(worldspawn_path.clone());

        // TODO: validate submodel names
        let models = [None, Some(ModelPrecache::Loaded { handle: worldspawn.clone() })]
            .into_iter()
            .chain(model_precache.map(|model_name| {
                if model_name.ends_with(".bsp") {
                    // TODO: We want the worldspawn to be `Model0` but other BSPs to be all models
                    Some(ModelPrecache::Loaded {
                        handle: asset_server
                            .load(AssetPath::parse(&model_name).into_owned().with_label("Model0")),
                    })
                } else if let Some(model_idx) = model_name.strip_prefix('*') {
                    Some(ModelPrecache::WaitingOnWorld {
                        pending: PendingSceneRoot {
                            root: worldspawn.id(),
                            asset: worldspawn_path.clone().with_label(format!("Model{model_idx}")),
                        },
                    })
                } else {
                    // TODO: `asset-importer`
                    // model_name.into()
                    None
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
            worldspawn,
            models,
            sounds: sounds.collect(),
            _cached_sounds: cached_sounds,
            ..ClientState::new()
        })
    }

    pub fn handle_damage(&mut self, armor: u8, health: u8, source: Vec3, kick_vars: KickVars) {
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
        //     roll: v_ent.angles.z,
        //     yaw: v_ent.angles.y,
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
        idle_vars: IdleVars,
        kick_vars: KickVars,
        roll_vars: RollVars,
        bob_vars: BobVars,
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
    // TODO: skipping entities indicates that the entities have been freed by
    // the server. it may make more sense to use a HashMap to store entities by
    // ID since the lookup table is relatively sparse.
    pub fn spawn_entities(
        &mut self,
        mut commands: Commands<'_, '_>,
        id: u16,
        baseline: EntityState,
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

        if id == 0 {
            info!("Spawning world: {baseline:?}");
        }

        let model = self.models.get(model_id).cloned().and_then(identity);
        let transform = Transform::from_xyz(origin.x, origin.y, origin.z)
            .with_rotation(Quat::from_euler(EulerRot::YZX, angles.x, angles.y, angles.z));

        let mut ent = if let Some(ent) = self.server_entity_to_client_entity.get(&id) {
            let mut entity = commands.entity(*ent);

            entity.insert(transform);

            entity
        } else {
            let entity = commands.spawn(transform);

            self.server_entity_to_client_entity.insert(id, entity.id());

            entity
        };

        if let Some(model) = model {
            match model {
                ModelPrecache::WaitingOnWorld { pending } => {
                    ent.insert(pending);
                }
                ModelPrecache::Loaded { handle } => {
                    ent.insert(SceneRoot(handle));
                }
            }
        }
    }

    pub fn update_entity(
        In(update): In<EntityUpdate>,
        mut commands: Commands<'_, '_>,
        mut conn: ResMut<Connection>,
        mut existing_entities: Query<&mut Transform>,
    ) {
        if let Some(entity) = conn.client_state.server_entity_to_client_entity.get(&update.ent_id) {
            let Ok(mut ent) = commands.get_entity(*entity) else {
                warn!("Server tried to update non-existent entity {}", update.ent_id);
                return;
            };

            if let Some(model_id) = update.model_id
                && let Some(model) =
                    conn.client_state.models.get(model_id as usize).cloned().and_then(identity)
            {
                match model {
                    ModelPrecache::WaitingOnWorld { pending } => {
                        ent.insert(pending);
                    }
                    ModelPrecache::Loaded { handle } => {
                        ent.insert(SceneRoot(handle));
                    }
                }
            }

            let update_transform = [
                update.origin_x,
                update.origin_y,
                update.origin_z,
                update.pitch,
                update.yaw,
                update.roll,
            ]
            .iter()
            .any(|v| v.is_some());

            if !update_transform {
                return;
            }

            if let Ok(mut transform) = existing_entities.get_mut(*entity) {
                let mut new_trans = transform.translation;
                if let Some(o_x) = update.origin_x {
                    new_trans = new_trans.with_x(o_x);
                }
                if let Some(o_y) = update.origin_y {
                    new_trans = new_trans.with_y(o_y);
                }
                if let Some(o_z) = update.origin_z {
                    new_trans = new_trans.with_z(o_z);
                }

                let new_transform = transform.with_translation(new_trans);

                let (mut pitch, mut yaw, mut roll) = transform.rotation.to_euler(EulerRot::YZX);
                if let Some(new_pitch) = update.pitch {
                    pitch = new_pitch;
                }
                if let Some(new_yaw) = update.yaw {
                    yaw = new_yaw;
                }
                if let Some(new_roll) = update.roll {
                    roll = new_roll;
                }

                let new_transform =
                    new_transform.with_rotation(Quat::from_euler(EulerRot::YZX, pitch, yaw, roll));

                *transform = new_transform;
            } else {
                let origin = Vec3::new(
                    update.origin_x.unwrap_or_default(),
                    update.origin_y.unwrap_or_default(),
                    update.origin_z.unwrap_or_default(),
                );
                let angles = Vec3::new(
                    update.pitch.unwrap_or(0.),
                    update.yaw.unwrap_or(0.),
                    update.roll.unwrap_or(0.),
                );

                ent.insert(
                    Transform::from_xyz(origin.x, origin.y, origin.z).with_rotation(
                        Quat::from_euler(EulerRot::YZX, angles.x, angles.y, angles.z),
                    ),
                );
            }
        } else {
            let Some(model_id) = update.model_id else {
                warn!("Tried to spawn ent without model");
                return;
            };

            let EntityState {
                origin,
                angles,
                model_id,

                // TODO
                frame_id: _,
                colormap: _,
                skin_id: _,
                effects: _,
            } = EntityState {
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
                model_id: model_id as usize,
                frame_id: update.frame_id.unwrap_or_default() as usize,
                colormap: update.colormap.unwrap_or_default(),
                skin_id: update.skin_id.unwrap_or_default() as usize,
                effects: EntityEffects::empty(),
            };

            let mut ent =
                commands.spawn(
                    Transform::from_xyz(origin.x, origin.y, origin.z).with_rotation(
                        Quat::from_euler(EulerRot::YZX, angles.x, angles.y, angles.z),
                    ),
                );

            if let Some(model) = conn.client_state.models.get(model_id).cloned().and_then(identity)
            {
                match model {
                    ModelPrecache::WaitingOnWorld { pending } => {
                        ent.insert(pending);
                    }
                    ModelPrecache::Loaded { handle } => {
                        ent.insert(SceneRoot(handle));
                    }
                }
            } else {
                warn!("Model {model_id} not found (TODO)");
            }

            conn.client_state.server_entity_to_client_entity.insert(update.ent_id, ent.id());
        }
    }
}

pub mod systems {}
