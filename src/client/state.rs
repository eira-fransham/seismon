use std::{convert::identity, iter};

use super::view::BobVars;
use crate::{
    client::{
        ClientError, Connection,
        view::{IdleVars, KickVars, RollVars},
    },
    common::net::{EntityState, EntityUpdate, PlayerColor},
};
use bevy::{
    asset::{AssetId, AssetPath, AssetServer, Handle},
    camera::visibility::Visibility,
    ecs::{
        component::Component,
        entity::Entity,
        hierarchy::ChildOf,
        system::{Commands, In, Query, ResMut},
    },
    log::*,
    math::{Quat, Vec3},
    scene::{Scene, SceneRoot},
    transform::components::Transform,
    utils::default,
};
use bevy_seedling::sample::AudioSample;
use bevy_trenchbroom::util::BevyTrenchbroomCoordinateConversions;
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
pub struct ClientState {
    pub worldspawn: Entity,

    // Model precache.
    pub models: Box<[Option<Handle<Scene>>]>,

    // Sound precache.
    pub sounds: Box<[Handle<AudioSample>]>,

    /// Sounds that are always needed, even if not in precache.
    /// This does not need to be read, it's just to ensure that
    /// the handles are not dropped.
    _cached_sounds: Box<[Handle<AudioSample>]>,

    pub server_entity_to_client_entity: HashMap<u16, Entity>,
}

impl ClientState {
    pub fn from_server_info<SName: AsRef<str>>(
        worldspawn: Entity,
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

        // TODO: validate submodel names
        let models = [None, Some(asset_server.load(worldspawn_path.clone()))]
            .into_iter()
            .chain(model_precache.map(|model_name| {
                if model_name.ends_with(".bsp") {
                    // TODO: We want the worldspawn to be `Model0` but other BSPs to be all models
                    Some(
                        asset_server
                            .load(AssetPath::parse(&model_name).into_owned().with_label("Model0")),
                    )
                } else {
                    model_name.strip_prefix('*').map(|model_idx| {
                        asset_server
                            .load(worldspawn_path.clone().with_label(format!("Model{model_idx}")))
                    })
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
            server_entity_to_client_entity: Default::default(),
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

        let origin = origin.trenchbroom_to_bevy();

        let [pitch, yaw, roll] = angles.map(|x| x.to_radians()).into();

        if id == 0 {
            info!("Spawning world: {baseline:?}");
        }

        let model = self.models.get(model_id).cloned().and_then(identity);
        let transform = Transform::from_xyz(origin.x, origin.y, origin.z)
            .with_rotation(Quat::from_euler(default(), yaw, pitch, roll));

        let mut ent = if let Some(ent) = self.server_entity_to_client_entity.get(&id) {
            let mut entity = commands.entity(*ent);

            entity.insert(transform);

            entity
        } else {
            let entity =
                commands.spawn((transform, Visibility::Inherited, ChildOf(self.worldspawn)));

            self.server_entity_to_client_entity.insert(id, entity.id());

            entity
        };

        if let Some(model) = model {
            ent.insert(SceneRoot(model));
        }
    }

    pub fn update_entity(
        In(update): In<EntityUpdate>,
        mut commands: Commands<'_, '_>,
        mut conn: ResMut<Connection>,
        mut existing_entities: Query<&mut Transform>,
    ) {
        let Some(state) = conn.client_state.as_mut() else {
            return;
        };

        if let Some(entity) = state.server_entity_to_client_entity.get(&update.ent_id) {
            let Ok(mut ent) = commands.get_entity(*entity) else {
                warn!("Server tried to update non-existent entity {}", update.ent_id);
                return;
            };

            if let Some(model_id) = update.model_id
                && let Some(model) = state.models.get(model_id as usize).cloned().and_then(identity)
            {
                ent.insert(SceneRoot(model));
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
                // TODO: Better to keep the client in Quake coordinates?
                let mut new_trans = transform.translation.bevy_to_trenchbroom();

                if let Some(o_x) = update.origin_x {
                    new_trans = new_trans.with_x(o_x);
                }
                if let Some(o_y) = update.origin_y {
                    new_trans = new_trans.with_y(o_y);
                }
                if let Some(o_z) = update.origin_z {
                    new_trans = new_trans.with_z(o_z);
                }

                let new_trans = new_trans.trenchbroom_to_bevy();

                let new_transform = transform.with_translation(new_trans);

                let (mut yaw, mut pitch, mut roll) = transform.rotation.to_euler(default());
                if let Some(new_pitch) = update.pitch {
                    pitch = new_pitch.to_radians();
                }
                if let Some(new_yaw) = update.yaw {
                    yaw = new_yaw.to_radians();
                }
                if let Some(new_roll) = update.roll {
                    roll = new_roll.to_radians();
                }

                // TODO: For some reason the rotation is wrong here(?)
                let new_transform =
                    new_transform.with_rotation(Quat::from_euler(default(), yaw, pitch, roll));

                *transform = new_transform;
            } else {
                let origin = Vec3::new(
                    update.origin_x.unwrap_or_default(),
                    update.origin_y.unwrap_or_default(),
                    update.origin_z.unwrap_or_default(),
                )
                .trenchbroom_to_bevy();
                let [yaw, pitch, roll] =
                    [update.yaw, update.pitch, update.roll].map(|a| a.unwrap_or(0.).to_radians());

                ent.insert(
                    Transform::from_xyz(origin.x, origin.y, origin.z)
                        .with_rotation(Quat::from_euler(default(), yaw, pitch, roll)),
                );
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
            )
            .trenchbroom_to_bevy();

            let [yaw, pitch, roll] =
                [update.yaw, update.pitch, update.roll].map(|a| a.unwrap_or(0.).to_radians());

            let mut ent =
                commands.spawn((
                    Transform::from_xyz(origin.x, origin.y, origin.z)
                        .with_rotation(Quat::from_euler(default(), yaw, pitch, roll)),
                    Visibility::Inherited,
                    ChildOf(state.worldspawn),
                ));

            if let Some(model) = update
                .model_id
                .and_then(|model_id| state.models.get(model_id as usize).cloned())
                .and_then(identity)
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
