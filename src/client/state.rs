use std::{
    io::{Cursor, Read},
    iter,
    num::NonZeroU32,
    path::Path,
};

use super::view::BobVars;
use crate::{
    client::{
        ClientError, ColorShiftCode,
        entity::ClientEntity,
        view::{IdleVars, KickVars, RollVars},
    },
    common::{
        math::Angles,
        net::{EntityState, EntityUpdate, PlayerColor},
    },
};
use beef::Cow;
use bevy::{
    asset::{AssetServer, Handle},
    ecs::{entity::Entity, system::Commands},
    log::*,
    math::{Quat, Vec3},
    prelude::default,
    scene::{Scene, SceneRoot},
    transform::components::Transform,
};
use bevy_seedling::sample::Sample;
use chrono::Duration;
use hashbrown::HashMap;
use rand::distr::Distribution as _;
use seismon_utils::QString;
use symphonium::{SymphoniumLoader, symphonia::core::probe::Hint};

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

const MAX_LIGHT_STYLES: usize = 64;

#[derive(Clone)]
pub struct PlayerInfo {
    pub name: QString,
    pub frags: i32,
    pub colors: PlayerColor,
    // translations: [u8; VID_GRADES],
}

/// Holder for precached models and sounds, to ensure they don't get unloaded. Plus, a map from server
/// entity ID to local [`Entity`].
#[derive(Default)]
pub struct ClientState {
    // model precache
    pub models: Box<[Handle<Scene>]>,

    // audio source precache
    pub sounds: Box<[Handle<Sample>]>,

    // sounds that are always needed even if not in precache
    cached_sounds: HashMap<&'static str, Handle<Sample>>,

    pub server_entity_to_client_entity: HashMap<u16, Entity>,
}

impl ClientState {
    // TODO: add parameter for number of player slots and reserve them in entity list
    pub fn new() -> ClientState {
        ClientState::default()
    }

    pub fn from_server_info<SName: AsRef<str>>(
        asset_server: &AssetServer,
        model_precache: Vec<String>,
        sound_precache: Vec<SName>,
    ) -> Result<ClientState, ClientError> {
        debug!("Model precache: {model_precache:?}");

        let mut model_precache = model_precache.into_iter();

        // TODO: Better error
        let worldspawn_path = model_precache
            .next()
            .ok_or_else(|| ClientError::InvalidConnectResponse)?;

        // TODO: validate submodel names
        let models = model_precache.into_iter().map(|model_name| {
            let path: Cow<str> = if model_name.ends_with(".bsp") {
                // BSPs can have more than one model
                // TODO: Reimplement multiple model support for .bsp files
                format!("{model_name}#Model0").into()
            } else if let Some(model_idx) = model_name.strip_prefix('*') {
                format!("{worldspawn_path}#Model{model_idx}").into()
            } else {
                "{model_name}".into()
            };

            // TODO: send keepalive message?

            asset_server.load(&*path)
        });

        let sounds = iter::once("misc/null.wav")
            .chain(sound_precache.iter().map(AsRef::as_ref))
            .enumerate()
            .map(|(i, snd_name)| {
                debug!("Loading sound {}: {}", i, snd_name);
                asset_server.load(snd_name)
                // TODO: send keepalive message?
            });

        let cached_sounds = TEMP_ENTITY_BUILTIN_SOUNDS
            .iter()
            .copied()
            .map(|snd_name| (snd_name, asset_server.load(format!("sound/{snd_name}"))))
            .collect();

        Ok(ClientState {
            models: models.collect(),
            sounds: sounds.collect(),
            cached_sounds,
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
            "Spawning entity with id {} (real: {}) from baseline {:?}",
            id,
            self.entities.len(),
            baseline
        );
        self.entities
            .push_back(ClientEntity::from_baseline(id, baseline));

        Ok(())
    }

    pub fn update_entity(
        &mut self,
        commands: Commands<'_, '_>,
        update: &EntityUpdate,
    ) -> Result<(), ClientError> {
        if let Some(entity) = self.server_entity_to_client_entity.get(&update.ent_id) {
            // entity.update(self.msg_times, update);
            // if entity.model_changed() {
            //     match self
            //         .models
            //         .get(entity.model_id)
            //         .map(|m| m.kind())
            //         .unwrap_or_default()
            //     {
            //         ModelKind::None => (),
            //         _ => {
            //             entity.sync_base = match self.models[entity.model_id].sync_type() {
            //                 SyncType::Sync => Duration::zero(),
            //                 SyncType::Rand => unimplemented!(), // TODO
            //             }
            //         }
            //     }
            // }

            // if let Some(c) = entity.colormap()
            //     && let Some(e) = self.entities.get_mut(id)
            // {
            //     e.colormap = Some(c);
            // }
        } else {
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
                // If this is the baseline, we probably don't want to accidentally set a model to the worldspawn.
                model_id: update.model_id.unwrap() as usize,
                frame_id: update.frame_id.unwrap_or_default() as usize,
                colormap: update.colormap.unwrap_or_default(),
                skin_id: update.skin_id.unwrap_or_default() as usize,
                effects: EntityEffects::empty(),
            };

            commands.spawn((
                SceneRoot(conn.client_state.models[model_id].clone()),
                Transform::from_xyz(origin.x, origin.y, origin.z).with_rotation(Quat::from_euler(
                    EulerRot::YZX,
                    angles.x,
                    angles.y,
                    angles.z,
                )),
            ));
        }
        Ok(())
    }

    pub fn models(&self) -> &[Model] {
        &self.models
    }
}

pub mod systems {}
