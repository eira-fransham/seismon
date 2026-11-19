//! Handlers for individual [`ServerCmd`]s, dispatched from
//! [`execute_server_msg`](super::systems::frame::execute_server_msg).

use bevy::ecs::{entity_disabling::Disabled, system::SystemParam};
use seismon_utils::QString;

use crate::client::{
    hud::MakeHud,
    lightstyle::LightStyle,
    particles::ParticleEvent,
    state::PrecacheModel,
    systems::frame::{ViewEntities, ViewFor},
};

use super::*;

/// Everything the server message handlers need access to.
#[derive(SystemParam)]
pub struct ServerMsgCtx<'w, 's> {
    pub commands: Commands<'w, 's>,
    pub conn: ResMut<'w, Connection>,
    pub console_output: ResMut<'w, ConsoleOutput>,
    pub settings: Res<'w, SeismonGameSettings>,
    pub wall_time: Res<'w, Time<Real>>,
    pub asset_server: Res<'w, AssetServer>,
    pub transforms: Query<'w, 's, &'static Transform>,
    pub level_stats: ResMut<'w, LevelStats>,
    pub intermission: ResMut<'w, HudIntermission>,
}

impl ServerMsgCtx<'_, '_> {
    /// Starts a new level. The caller must stop processing messages for this frame afterwards,
    /// so that the level can load.
    pub fn server_info(
        &mut self,
        protocol_version: i32,
        message: QString,
        model_precache: Vec<String>,
        sound_precache: Vec<String>,
    ) -> Result<(), ClientError> {
        // check protocol version
        if protocol_version != net::PROTOCOL_VERSION as i32 {
            UnrecognizedProtocolSnafu { version: protocol_version }.fail()?;
        }

        // New level: reset the level stats (`cl.levelname` is the server's message) and leave
        // any intermission.
        *self.level_stats = LevelStats { level_name: message.clone(), ..default() };
        self.intermission.0 = None;

        if !message.raw.is_empty() {
            self.console_output.println_alert(message.raw, &self.wall_time);
        }

        if let Some(state) = self.conn.client_state.as_ref() {
            self.commands.entity(state.worldspawn).despawn();
        }

        let mut new_worldspawn = self.commands.spawn(Visibility::Hidden);

        self.conn.client_state = Some(ClientState::from_server_info(
            &mut new_worldspawn,
            &self.asset_server,
            model_precache,
            sound_precache,
        )?);

        self.commands.set_state_if_different(ClientGameState::Loading);

        Ok(())
    }

    /// Starts a cutscene, finale or intermission.
    pub fn start_intermission(&mut self, kind: IntermissionKind) {
        // TODO: Typewriter reveal of the text (`SCR_DrawCenterString` in finale mode)
        if let IntermissionKind::Cutscene { text } | IntermissionKind::Finale { text } = &kind {
            self.console_output.set_center_print(text.clone(), &self.wall_time);
        }

        self.intermission.0 =
            Some(IntermissionState { kind, completed_secs: self.conn.last_msg_time.as_secs_f32() });
    }

    pub fn fast_update(&mut self, ent_update: EntityUpdate) {
        // first update signals the last sign-on stage
        self.commands.set_state_if_different(SignOn(SignOnStage::Done));

        self.commands.run_system_cached_with(
            ClientState::update_entity,
            (ent_update, self.conn.last_msg_time.as_secs_f64()),
        );
    }

    pub fn set_angle(&mut self, angles: QAngles) {
        let is_demo = matches!(self.conn.target, ConnectionTarget::Demo(_));
        let msg_time = self.conn.last_msg_time.as_secs_f64();

        // Applied to the view entity's `Transform` by `apply_local_view_angles`.
        if let Some(state) = self.conn.client_state.as_mut() {
            // TODO: This feels a bit hacky, but ensures that view angles are interpolated in demos
            if is_demo && state.view.is_some() {
                self.commands
                    .run_system_cached_with(ClientState::set_view_angles, (angles, msg_time));
            } else {
                state.look_angles = angles;
            }
        }
    }

    pub fn set_view(&mut self, ent_id: u16) {
        let Some(state) = self.conn.client_state.as_mut() else {
            return;
        };

        if let Some(prev_view) = state.view.take()
            && let Some(prev_view) = state.server_entity_to_client_entity.get(&prev_view)
        {
            self.commands.entity(*prev_view).despawn_related::<ViewEntities>();
        }

        state.view = Some(ent_id);

        let mut view_entity = match state.server_entity_to_client_entity.get(&ent_id) {
            Some(ent) => self.commands.entity(*ent),
            None => {
                warn!("Server tried to set view on non-existent entity {ent_id}");
                let entity = self.commands.spawn((Transform::default(), ChildOf(state.worldspawn)));
                state.server_entity_to_client_entity.insert(ent_id, entity.id());
                entity
            }
        };

        // Viewing entity should be invisible (e.g. do not show player model)
        view_entity.insert(Visibility::Hidden).insert_if_new(view::ViewEffects::default());

        let view_entity = view_entity.id();
        let worldspawn = state.worldspawn;

        let world_template = self
            .settings
            .camera_templates
            .world
            .filter(|cam| self.commands.get_entity(*cam).is_ok())
            .unwrap_or(self.settings.default_world_camera_template);
        let Ok(mut world_camera_entity) = self.commands.get_entity(world_template) else {
            warn!("No world camera template");
            return;
        };

        world_camera_entity
            .clone_and_spawn_with_opt_out(|builder| {
                builder.deny::<Template>();
            })
            .insert((ViewFor(view_entity), ChildOf(view_entity)))
            .queue(view::capture_view_base);

        let overlay_template = self
            .settings
            .camera_templates
            .overlay
            .filter(|cam| self.commands.get_entity(*cam).is_ok())
            .unwrap_or(self.settings.default_overlay_camera_template);
        let Ok(mut overlay_camera_entity) = self.commands.get_entity(overlay_template) else {
            warn!("No overlay camera template");
            return;
        };

        overlay_camera_entity
            .clone_and_spawn_with_opt_out(|builder| {
                builder.deny::<Template>();
            })
            .insert((ViewFor(view_entity), ChildOf(worldspawn)));

        self.commands.queue(MakeHud { owner: view_entity, components: ViewFor(view_entity) });
    }

    pub fn sound(
        &mut self,
        volume: Option<u8>,
        attenuation: Option<f32>,
        entity_id: u16,
        channel: i8,
        sound_id: u8,
        position: Vec3,
    ) {
        let Some(state) = self.conn.client_state.as_ref() else {
            return;
        };

        trace!("starting sound with id {} on entity {} channel {}", sound_id, entity_id, channel);

        let entity =
            state.server_entity_to_client_entity.get(&entity_id).cloned().unwrap_or_else(|| {
                warn!("server tried to start sound on nonexistent entity {}", entity_id);

                state.worldspawn
            });
        let entity_origin =
            self.transforms.get(entity).map(|trans| trans.translation).unwrap_or_default();

        let origin = position - entity_origin;
        let volume = volume.unwrap_or(DEFAULT_SOUND_PACKET_VOLUME);
        let attenuation = attenuation.unwrap_or(DEFAULT_SOUND_PACKET_ATTENUATION);
        if let Some(sound) = state.sounds.get(sound_id as usize) {
            self.commands.write_message(MixerMessage::StartSound(StartSound {
                entity,
                src: sound.clone(),
                ent_channel: channel,
                volume: volume as f32 / 255.0,
                attenuation,
                origin,
            }));
        }
    }

    pub fn static_sound(&mut self, origin: Vec3, sound_id: u8, volume: u8, attenuation: u8) {
        if let Some(state) = self.conn.client_state.as_ref()
            && let Some(sound) = state.sounds.get(sound_id as usize)
        {
            self.commands.write_message(MixerMessage::StartStaticSound(StartStaticSound {
                world: state.worldspawn,
                src: sound.clone(),
                origin,
                volume: volume as f32 / 255.,
                attenuation: attenuation as f32 / 64.,
            }));
        }
    }

    pub fn stop_sound(&mut self, entity_id: u16, channel: i8) {
        if let Some(state) = self.conn.client_state.as_ref() {
            self.commands.write_message(MixerMessage::StopSound(StopSound {
                entity: state
                    .server_entity_to_client_entity
                    .get(&entity_id)
                    .cloned()
                    .unwrap_or(state.worldspawn),
                ent_channel: channel,
            }));
        }
    }

    pub fn spawn_baseline(&mut self, ent_id: u16, baseline: EntityState) {
        let msg_time = self.conn.last_msg_time.as_secs_f64();
        if let Some(state) = self.conn.client_state.as_mut() {
            state.spawn_entities(self.commands.reborrow(), ent_id, baseline, msg_time);
        }
    }

    pub fn spawn_static(&mut self, model_id: u8, origin: Vec3, angles: Vec3) {
        let Some(state) = self.conn.client_state.as_mut() else {
            return;
        };

        let mut ent = self.commands.spawn((
            // Same conversion as dynamic entities: `angles` is a Quake pitch/yaw/roll vector in
            // degrees.
            Transform::from_translation(origin)
                .with_rotation(QAngles::from(angles.to_array()).into()),
            // TODO: Handle the other fields
            Visibility::Inherited,
            ChildOf(state.worldspawn),
        ));

        if let Some(PrecacheModel::Loaded(model)) = state.models.get(model_id as usize).cloned() {
            ent.insert(WorldAssetRoot(model));
        }
    }

    /// `svc_particle`.
    pub fn particle(&mut self, origin: Vec3, direction: Vec3, count: u8, color: u8) {
        self.commands
            .write_message(ParticleEvent::from_particle_cmd(origin, direction, count, color));
    }

    /// `svc_temp_entity`. Only the particles are handled so far: no beams, sounds or lights.
    pub fn temp_entity(&mut self, temp_entity: &net::TempEntity) {
        if let Some(event) = ParticleEvent::from_temp_entity(temp_entity) {
            self.commands.write_message(event);
        }
    }

    pub fn stuff_text(&mut self, text: QString) {
        match text.to_str().parse::<RunCmd<'static>>() {
            Ok(parsed) => {
                self.commands.write_message(parsed);
            }
            Err(err) => self.console_output.println(format!("{err}"), &self.wall_time),
        }
    }

    pub fn time(&mut self, time: f32) {
        let last_msg_time = self.conn.last_msg_time;
        if self.conn.is_connected()
            && let Some(state) = &mut self.conn.client_state
            && !last_msg_time.is_zero()
        {
            for entity in state.drain_dead_entities() {
                self.commands.entity(entity).insert_recursive::<Children>(Disabled);
            }
        }

        self.conn.last_msg_time = Duration::from_secs_f32(time);
    }

    /// Replaces a lightstyle, like `svc_lightstyle`.
    pub fn lightstyle(&mut self, id: u8, value: QString) -> Result<(), ClientError> {
        let Some(state) = self.conn.client_state.as_mut() else {
            return Ok(());
        };

        let num_styles = state.lightstyles.len();
        state.lightstyles.resize(num_styles.max(id as usize + 1), Default::default());
        let style = &mut state.lightstyles[id as usize];

        if style.source != value {
            *style = LightStyle::parse(value);
            state.lightstyles_changed = true;
        }

        Ok(())
    }

    pub fn update_stat(&mut self, stat: net::ClientStat, value: i32) {
        use net::ClientStat as Stat;

        match stat {
            Stat::TotalSecrets => self.level_stats.total_secrets = value,
            Stat::TotalMonsters => self.level_stats.total_monsters = value,
            Stat::FoundSecrets => self.level_stats.secrets = value,
            Stat::KilledMonsters => self.level_stats.monsters = value,
            // The rest are sent in `PlayerData` by Quake servers.
            stat => debug!("TODO: Update stat {stat:?}"),
        }
    }
}
