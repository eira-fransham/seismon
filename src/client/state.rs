use snafu::OptionExt as _;
use std::{iter, mem};

use crate::{
    client::{
        ClientError, Connection, InvalidConnectResponseSnafu, OVERLAY_RENDER_LAYER,
        hud::HudIntermission,
        interpolation::{Next, NoInterpolation},
        inventory::{Armor, CurrentAmmo, Health},
        lightstyle::LightStyle,
        post_process::{ColorShift, Powerup},
        systems::frame::{ViewEntities, ViewFor},
        view::{ViewEffects, capture_view_base},
    },
    common::net::{
        DEFAULT_VIEWHEIGHT, EntityState, EntityUpdate, ItemFlags, PlayerColor, PlayerData,
    },
};
use bevy::{
    asset::{AssetPath, AssetServer, Handle},
    camera::visibility::{RenderLayers, Visibility},
    color::Color,
    ecs::{
        component::Component,
        entity::Entity,
        hierarchy::ChildOf,
        name::Name,
        query::With,
        reflect::ReflectComponent,
        relationship::RelationshipTarget as _,
        system::{Commands, EntityCommands, In, ParallelCommands, Query, Res, ResMut},
    },
    log::*,
    math::{EulerRot, Quat, Vec3},
    reflect::Reflect,
    transform::components::Transform,
    utils::default,
    world_serialization::{WorldAsset, WorldAssetRoot},
};
use bevy_mod_mdl::MdlSettings;
use bevy_seedling::sample::AudioSample;
use bevy_trenchbroom::bsp::Bsp;
use bitvec::vec::BitVec;
use hashbrown::{HashMap, hash_map::Entry};
use seismon_utils::{QAngles, QString, write_if_neq};

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

/// The baseline state of a server entity, as sent by `SpawnBaseline`. Entity updates are diffs
/// against this, not against the previously-received state: a field missing from an update means
/// "same as the baseline".
#[derive(Component, Clone, Debug)]
pub struct Baseline(pub EntityState);

/// Converts the origin and angles of `state` to a `Transform`.
fn entity_state_transform(state: &EntityState) -> Transform {
    let [pitch, yaw, roll] = state.angles.into();

    // Entity angles are in degrees; the `QAngles` -> `Quat` conversion handles the conversion to
    // radians and Quake's axis conventions.
    Transform::from_translation(state.origin)
        .with_rotation(QAngles { roll_deg: roll, pitch_deg: pitch, yaw_deg: yaw }.into())
}

/// A player slot on the scoreboard (`scoreboard_t`), as sent by `svc_updatename`,
/// `svc_updatefrags` and `svc_updatecolors`. A slot is empty while its name is empty.
#[derive(Clone, Debug)]
pub struct PlayerInfo {
    pub name: QString,
    pub frags: i32,
    pub colors: PlayerColor,
}

impl Default for PlayerInfo {
    fn default() -> Self {
        Self { name: QString::default(), frags: 0, colors: PlayerColor::new(0, 0) }
    }
}

/// The number of stats (`cl.stats`). Quake only uses the first 32, but FTEQW's CSQC can read up
/// to 256.
pub const MAX_CL_STATS: usize = 256;

/// Stat numbers (`STAT_*`), as CSQC sees them. Quake servers send most of these in
/// `svc_clientdata` rather than `svc_updatestat`.
pub mod stat {
    pub const HEALTH: usize = 0;
    pub const FRAGS: usize = 1;
    pub const WEAPON: usize = 2;
    pub const AMMO: usize = 3;
    pub const ARMOR: usize = 4;
    pub const WEAPONFRAME: usize = 5;
    pub const SHELLS: usize = 6;
    pub const NAILS: usize = 7;
    pub const ROCKETS: usize = 8;
    pub const CELLS: usize = 9;
    pub const ACTIVEWEAPON: usize = 10;
    pub const TOTALSECRETS: usize = 11;
    pub const TOTALMONSTERS: usize = 12;
    pub const SECRETS: usize = 13;
    pub const MONSTERS: usize = 14;
    pub const ITEMS: usize = 15;
    pub const VIEWHEIGHT: usize = 16;
}

#[derive(Clone)]
pub struct PrecacheModel {
    pub name: Option<String>,
    pub state: PrecacheModelState,
}

#[derive(Clone)]
pub enum PrecacheModelState {
    Invalid,
    // TODO: This is here to work around a bug in Bevy 0.18 where simultaneously loading
    // `foo.bsp#Model0` at the same time as `foo.bsp#Model1` would cause a race condition
    // and break some internal state. If that bug is fixed on Bevy 0.20, we can remove
    // this special-case and the code that handles it (`populate_model_precache`).
    WaitingOnWorldspawn(usize),
    Loaded(Handle<WorldAsset>),
}

#[derive(Clone)]
pub struct PrecacheSound {
    pub name: String,
    pub sample: Handle<AudioSample>,
}

#[derive(Component, Reflect)]
#[reflect(Component)]
pub struct ViewModel;

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

    pub look_angles: QAngles,

    /// Quake 1 deletes any entities that haven't received new messages on a given frame.
    pub frame_keepalive: BitVec,

    /// Model precache.
    pub models: Box<[PrecacheModel]>,

    /// Sound precache.
    pub sounds: Box<[PrecacheSound]>,

    /// Sounds that are always needed, even if not in precache.
    /// This does not need to be read, it's just to ensure that
    /// the handles are not dropped.
    _cached_sounds: Box<[Handle<AudioSample>]>,

    pub server_entity_to_client_entity: HashMap<u16, Entity>,

    /// The current lightstyles, i.e. `cl_lightstyle`. Sized by [`max_lightstyles`] when the
    /// level starts; use its length rather than a constant as the limit.
    // TODO: Store this on a component on the worldspawn entity instead of globally.
    pub lightstyles: Vec<LightStyle>,

    /// Set when [`Self::lightstyles`] changes, so `sync_lightstyles` rebuilds the lightmap
    /// animators.
    // TODO: Use a marker component on the worldspawn entity rather than storing this globally.
    pub lightstyles_changed: bool,

    /// The number of player slots on the server.
    // TODO: Delegate to cvar
    pub max_clients: u8,

    /// Whether the server is running deathmatch (rather than coop).
    // TODO: Delegate to cvar
    pub deathmatch: bool,

    /// The scoreboard, one entry per player slot.
    // TODO: This should be stored on the `PlayerState` component rather than globally.
    pub players: Vec<PlayerInfo>,

    /// The stats of this client's player (`cl.stats`), indexed by the numbers in [`stat`].
    // TODO: This should be stored on the `PlayerState` component rather than globally.
    pub stats: Box<[i32; MAX_CL_STATS]>,
}

#[derive(Component, Default, Debug)]
pub struct PlayerState {
    pub items: ItemFlags,
}

impl ClientState {
    pub fn populate_model_precache(&mut self, bsp: &Bsp) {
        for model in &mut self.models {
            if let PrecacheModelState::WaitingOnWorldspawn(idx) = model.state {
                model.state = PrecacheModelState::Loaded(bsp.models[idx].clone());
            }
        }
    }

    pub fn from_server_info(
        worldspawn: &mut EntityCommands,
        asset_server: &AssetServer,
        model_precache: Vec<String>,
        sound_precache: Vec<String>,
    ) -> Result<ClientState, ClientError> {
        trace!("Model precache: {model_precache:?}");

        let mut model_precache = model_precache.into_iter();

        // TODO: Better error
        let worldspawn_path_string = model_precache.next().context(InvalidConnectResponseSnafu)?;

        let worldspawn_path = AssetPath::parse(&worldspawn_path_string).into_owned();

        worldspawn.insert((
            Name::new("worldspawn"),
            Worldspawn { bsp: asset_server.load(worldspawn_path) },
        ));

        // TODO: validate submodel names
        let models = [
            PrecacheModel { name: None, state: PrecacheModelState::Invalid },
            PrecacheModel {
                name: Some(worldspawn_path_string),
                state: PrecacheModelState::WaitingOnWorldspawn(0),
            },
        ]
        .into_iter()
        .chain(model_precache.map(|model_name| {
            let model_kind = if model_name.ends_with(".bsp") {
                // TODO: We want the worldspawn to be `Model0` but other BSPs to be all
                // models

                PrecacheModelState::Loaded(
                    asset_server
                        .load(AssetPath::parse(&model_name).into_owned().with_label("Model0")),
                )
            } else if let Some(model_idx) = model_name.strip_prefix('*') {
                PrecacheModelState::WaitingOnWorldspawn(
                    model_idx.parse().expect("TODO: Handle this error"),
                )
            } else {
                PrecacheModelState::Loaded(asset_server.load(&model_name))
            };

            PrecacheModel { name: Some(model_name), state: model_kind }
        }))
        .collect();

        let sounds = iter::once("misc/null.wav".to_owned()).chain(sound_precache).enumerate().map(
            |(i, snd_name)| {
                debug!("Loading sound {}: {}", i, snd_name);
                let sample = asset_server.load(format!("sound/{snd_name}"));

                // TODO: send keepalive message?
                PrecacheSound { name: snd_name, sample }
            },
        );

        let cached_sounds = TEMP_ENTITY_BUILTIN_SOUNDS
            .iter()
            .copied()
            .map(|snd_name| asset_server.load(format!("sound/{snd_name}")))
            .collect();

        Ok(ClientState {
            view: None,
            look_angles: Default::default(),
            worldspawn: worldspawn.id(),
            models,
            frame_keepalive: default(),
            sounds: sounds.collect(),
            _cached_sounds: cached_sounds,
            server_entity_to_client_entity: Default::default(),
            // A new level clears every style, like `CL_ClearState`. The server resends them.
            lightstyles: Default::default(),
            lightstyles_changed: true,
            max_clients: 0,
            deathmatch: false,
            players: Vec::new(),
            stats: Box::new([0; MAX_CL_STATS]),
        })
    }

    pub fn drain_dead_entities(&mut self) -> impl Iterator<Item = Entity> {
        let keepalive = mem::take(&mut self.frame_keepalive);
        let view = self.view;

        // The view entity is never disabled: like `cl_entities[cl.viewentity]` in Quake its state
        // (and the camera parented to it) must persist even if the server stops sending it.
        self.server_entity_to_client_entity.iter().filter_map(move |(k, v)| {
            if Some(*k) == view || *keepalive.get(*k as usize).as_deref().unwrap_or(&false) {
                None
            } else {
                Some(*v)
            }
        })
    }

    pub fn mark_entity_alive(&mut self, ent_id: u16) {
        self.frame_keepalive.resize(self.frame_keepalive.len().max(ent_id as usize + 1), false);
        self.frame_keepalive.set(ent_id as usize, true);
    }

    pub fn update_color_shifts(
        mut players: Query<(Entity, &mut PlayerState)>,
        commands: ParallelCommands,
    ) {
        players.par_iter_mut().for_each(|(player_ent, player)| {
            let overlay = player
                .items
                .iter()
                .find_map(|item_flag| {
                    Some(match item_flag {
                        ItemFlags::QUAD => {
                            ColorShift::<Powerup>::new(Color::linear_rgb(0., 0., 1.), 0.3)
                        }
                        ItemFlags::SUIT => {
                            ColorShift::<Powerup>::new(Color::linear_rgb(0., 1., 0.), 0.2)
                        }
                        ItemFlags::INVISIBILITY => {
                            ColorShift::<Powerup>::new(Color::linear_rgb(0.4, 0.4, 0.4), 1.)
                        }
                        ItemFlags::INVULNERABILITY => {
                            ColorShift::<Powerup>::new(Color::linear_rgb(1., 1., 0.), 0.3)
                        }
                        _ => return None,
                    })
                })
                .unwrap_or_default();

            commands.command_scope(|mut commands| {
                commands.entity(player_ent).insert(overlay);
            })
        });
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
        let EntityState { model_id, frame_id: frame, skin_id: skin, .. } = baseline;

        if id == 0 {
            trace!("Spawning world: {baseline:?}");
        }

        let model = self.models.get(model_id as usize).cloned();
        let transform =
            Next { component: entity_state_transform(&baseline), elapsed_secs: msg_time };
        let baseline = Baseline(baseline);

        let mut ent = if let Some(ent) = self.server_entity_to_client_entity.get(&id) {
            let mut entity = commands.entity(*ent);

            entity.insert((transform, baseline));

            entity
        } else {
            let bundle = (transform, Visibility::Inherited, ChildOf(self.worldspawn));

            if id != 0 {
                match self.server_entity_to_client_entity.entry(id) {
                    Entry::Occupied(existing) => {
                        let existing = existing.get();
                        error!("Tried to spawn duplicate entity (found {existing})");
                        let mut entity = commands.entity(*existing);
                        entity.insert((bundle, baseline));
                        entity
                    }
                    Entry::Vacant(vacant) => {
                        let entity = commands.spawn((bundle, baseline));
                        vacant.insert(entity.id());

                        entity
                    }
                }
            } else {
                // Special-case: bsp model is spawned with `SpawnBaseline` (not `SpawnStatic`) but
                // not handled via the regular update loop.
                commands.spawn(bundle)
            }
        };

        if let Some(PrecacheModel { state: PrecacheModelState::Loaded(model), .. }) = model {
            ent.insert((WorldAssetRoot(model), MdlSettings { frame, skin }));
        } else {
            error!("Tried to insert model but it wasn't loaded yet");
        }
    }

    /// Sets the entity represented by the server-side `ent_id` to be the current player.
    #[expect(clippy::too_many_arguments)]
    pub fn update_player(
        In(player_data): In<PlayerData>,
        mut conn: ResMut<Connection>,
        mut existing_data: Query<(&mut PlayerState, &mut Health, &mut Armor, &mut CurrentAmmo)>,
        // TODO: We should just add a single `IsPlayer` component (or similar) that adds `PlayerState`,
        // `Health` and `ViewEntities`
        view_entities: Query<&ViewEntities>,
        mut view_models: Query<
            (&mut WorldAssetRoot, &mut MdlSettings, &mut Visibility),
            With<ViewModel>,
        >,
        mut view_effects: Query<&mut ViewEffects>,
        intermission: Res<HudIntermission>,
        mut commands: Commands<'_, '_>,
    ) {
        use crate::client::inventory::{
            Cells, GrenadeLauncher, InitItem, InsertItem, LightningGun, Nailgun, Nails, RemoveItem,
            RocketLauncher, Rockets, SetActiveWeaponByOrder, Shells, Shotgun, Sigil1, Sigil2,
            Sigil3, Sigil4, SuperNailgun, SuperShotgun, UpdateAmmoCount,
        };

        let Some(state) = conn.client_state.as_mut() else {
            return;
        };

        // `CL_ParseClientdata` stores these in `cl.stats` (and the items in `cl.items`, which
        // CSQC sees as `STAT_ITEMS`).
        let view_height = player_data.view_height.unwrap_or(DEFAULT_VIEWHEIGHT);
        for (stat, value) in [
            (stat::HEALTH, i32::from(player_data.health)),
            (stat::WEAPON, player_data.weapon_model_id.map_or(0, i32::from)),
            (stat::AMMO, i32::from(player_data.ammo)),
            (stat::ARMOR, player_data.armor.map_or(0, i32::from)),
            (stat::WEAPONFRAME, player_data.weapon_frame.map_or(0, i32::from)),
            (stat::SHELLS, i32::from(player_data.ammo_shells)),
            (stat::NAILS, i32::from(player_data.ammo_nails)),
            (stat::ROCKETS, i32::from(player_data.ammo_rockets)),
            (stat::CELLS, i32::from(player_data.ammo_cells)),
            (stat::ACTIVEWEAPON, i32::from(player_data.active_weapon)),
            (stat::ITEMS, player_data.items.bits() as i32),
            (stat::VIEWHEIGHT, view_height as i32),
        ] {
            state.stats[stat] = value;
        }
        let state = &*state;

        let Some(ent) = state
            .view
            .and_then(|ent_id| state.server_entity_to_client_entity.get(&ent_id))
            .copied()
        else {
            error!("No view entity to update");
            return;
        };

        let punch_angles = QAngles {
            pitch_deg: player_data.punch_pitch.unwrap_or_default(),
            yaw_deg: player_data.punch_yaw.unwrap_or_default(),
            roll_deg: player_data.punch_roll.unwrap_or_default(),
        };
        let velocity = Vec3::new(
            player_data.velocity_x.unwrap_or_default(),
            player_data.velocity_y.unwrap_or_default(),
            player_data.velocity_z.unwrap_or_default(),
        );
        // Missing fields have their defaults, as in `CL_ParseClientdata` (see `view_height`
        // above).

        if let Ok(mut effects) = view_effects.get_mut(ent) {
            write_if_neq!(effects.view_height, view_height);
            write_if_neq!(effects.velocity, velocity);
            write_if_neq!(effects.punch_angles, punch_angles);
            write_if_neq!(effects.health, player_data.health);
        } else {
            commands.entity(ent).insert(ViewEffects {
                view_height,
                velocity,
                punch_angles,
                health: player_data.health,
                ..default()
            });
        }

        // A missing weapon model (or index 0) means "no viewmodel", and vanilla also doesn't draw
        // the viewmodel while dead (`R_DrawViewModel`) or during intermission
        // (`V_CalcIntermissionRefdef`).
        let weapon_model = player_data.weapon_model_id.filter(|&id| id != 0).and_then(|id| {
            match state.models.get(id as usize) {
                Some(PrecacheModel { state: PrecacheModelState::Loaded(model), .. }) => {
                    Some(model.clone())
                }
                _ => None,
            }
        });
        let viewmodel_visibility =
            if weapon_model.is_some() && player_data.health > 0 && intermission.0.is_none() {
                Visibility::Visible
            } else {
                Visibility::Hidden
            };

        if let Some(existing) = view_entities
            .get(ent)
            .ok()
            .and_then(|view_entities| view_entities.iter().find(|e| view_models.contains(*e)))
        {
            let (mut existing_scene, mut existing_settings, mut visibility) =
                view_models.get_mut(existing).expect("Checked above");

            if let Some(model) = weapon_model
                && existing_scene.0 != model
            {
                existing_scene.0 = model;
            }

            write_if_neq!(*visibility, viewmodel_visibility);
            write_if_neq!(existing_settings.frame, player_data.weapon_frame.unwrap_or_default());
        } else if let Some(model) = weapon_model {
            commands
                .spawn((
                    Name::new("Viewmodel"),
                    ViewModel,
                    viewmodel_visibility,
                    WorldAssetRoot(model),
                    Transform::from_translation(Vec3::new(0., -12., 0.)).with_rotation(
                        Quat::from_euler(
                            EulerRot::XYZ,
                            0.,
                            -std::f32::consts::FRAC_PI_2,
                            -std::f32::consts::FRAC_PI_2,
                        ),
                    ),
                    MdlSettings { frame: player_data.weapon_frame.unwrap_or_default(), skin: 0 },
                    RenderLayers::layer(OVERLAY_RENDER_LAYER),
                    ViewFor(ent),
                ))
                .queue(capture_view_base);
        }

        // `CL_ParseClientdata` treats a missing armor value as 0.
        let armor = player_data.armor.unwrap_or_default();

        let existing_flags = if let Ok((mut existing, mut health, mut existing_armor, mut ammo)) =
            existing_data.get_mut(ent)
        {
            write_if_neq!(health.0, player_data.health);
            write_if_neq!(existing_armor.0, armor);
            write_if_neq!(ammo.0, player_data.ammo);

            let existing_flags = existing.items;
            write_if_neq!(existing.items, player_data.items);
            existing_flags
        } else {
            commands.entity(ent).insert((
                PlayerState { items: player_data.items },
                Health(player_data.health),
                Armor(armor),
                CurrentAmmo(player_data.ammo),
            ));

            commands.queue(InitItem::<Shells>::new(ent));
            commands.queue(InitItem::<Nails>::new(ent));
            commands.queue(InitItem::<Rockets>::new(ent));
            commands.queue(InitItem::<Cells>::new(ent));
            Default::default()
        };

        let elapsed_secs_f32 = conn.last_msg_time.as_secs_f32();

        let new_flags = player_data.items.difference(existing_flags);

        for new_flag in new_flags.iter() {
            if new_flag == ItemFlags::SHOTGUN {
                commands.queue(InsertItem::<Shotgun>::new(ent, elapsed_secs_f32));
            } else if new_flag == ItemFlags::SUPER_SHOTGUN {
                commands.queue(InsertItem::<SuperShotgun>::new(ent, elapsed_secs_f32));
            } else if new_flag == ItemFlags::NAILGUN {
                commands.queue(InsertItem::<Nailgun>::new(ent, elapsed_secs_f32));
            } else if new_flag == ItemFlags::SUPER_NAILGUN {
                commands.queue(InsertItem::<SuperNailgun>::new(ent, elapsed_secs_f32));
            } else if new_flag == ItemFlags::ROCKET_LAUNCHER {
                commands.queue(InsertItem::<RocketLauncher>::new(ent, elapsed_secs_f32));
            } else if new_flag == ItemFlags::GRENADE_LAUNCHER {
                commands.queue(InsertItem::<GrenadeLauncher>::new(ent, elapsed_secs_f32));
            } else if new_flag == ItemFlags::LIGHTNING {
                commands.queue(InsertItem::<LightningGun>::new(ent, elapsed_secs_f32));
            } else if new_flag == ItemFlags::SIGIL_1 {
                commands.queue(InitItem::<Sigil1>::new(ent));
            } else if new_flag == ItemFlags::SIGIL_2 {
                commands.queue(InitItem::<Sigil2>::new(ent));
            } else if new_flag == ItemFlags::SIGIL_3 {
                commands.queue(InitItem::<Sigil3>::new(ent));
            } else if new_flag == ItemFlags::SIGIL_4 {
                commands.queue(InitItem::<Sigil4>::new(ent));
            }
        }

        let removed_flags = existing_flags.difference(player_data.items);

        for removed_flag in removed_flags.iter() {
            if removed_flag == ItemFlags::SHOTGUN {
                commands.queue(RemoveItem::<Shotgun>::new(ent));
            } else if removed_flag == ItemFlags::SUPER_SHOTGUN {
                commands.queue(RemoveItem::<SuperShotgun>::new(ent));
            } else if removed_flag == ItemFlags::NAILGUN {
                commands.queue(RemoveItem::<Nailgun>::new(ent));
            } else if removed_flag == ItemFlags::SUPER_NAILGUN {
                commands.queue(RemoveItem::<SuperNailgun>::new(ent));
            } else if removed_flag == ItemFlags::ROCKET_LAUNCHER {
                commands.queue(RemoveItem::<RocketLauncher>::new(ent));
            } else if removed_flag == ItemFlags::GRENADE_LAUNCHER {
                commands.queue(RemoveItem::<GrenadeLauncher>::new(ent));
            } else if removed_flag == ItemFlags::LIGHTNING {
                commands.queue(RemoveItem::<LightningGun>::new(ent));
            } else if removed_flag == ItemFlags::SIGIL_1 {
                commands.queue(RemoveItem::<Sigil1>::new(ent));
            } else if removed_flag == ItemFlags::SIGIL_2 {
                commands.queue(RemoveItem::<Sigil2>::new(ent));
            } else if removed_flag == ItemFlags::SIGIL_3 {
                commands.queue(RemoveItem::<Sigil3>::new(ent));
            } else if removed_flag == ItemFlags::SIGIL_4 {
                commands.queue(RemoveItem::<Sigil4>::new(ent));
            }
        }

        // `STAT_ACTIVEWEAPON` is the `IT_*` flag of the active weapon, so its bit index is the
        // HUD slot. The axe (`IT_AXE`) does not fit in the byte sent over the network and arrives
        // as 0, in which case no slot is highlighted.
        // HACK: This does correctly get the order but it's not good
        let active_weapon_order =
            player_data.active_weapon.checked_ilog2().map(|order| order as usize);
        commands.queue(SetActiveWeaponByOrder(ent, active_weapon_order));

        // The `IT_SHELLS`/`IT_NAILS`/etc. flags only mark the ammo type of the current weapon
        // (`W_SetCurrentAmmo`), so all counters are always updated, as in `Sbar_DrawInventory`.
        commands.queue(UpdateAmmoCount::<Shells>::new(ent, player_data.ammo_shells));
        commands.queue(UpdateAmmoCount::<Nails>::new(ent, player_data.ammo_nails));
        commands.queue(UpdateAmmoCount::<Rockets>::new(ent, player_data.ammo_rockets));
        commands.queue(UpdateAmmoCount::<Cells>::new(ent, player_data.ammo_cells));
    }

    /// Sets the rotation of the view entity's interpolation target, leaving its origin alone. Used
    /// for `SetAngle` in demos, so that view angles are interpolated.
    pub fn set_view_angles(
        In((angles, msg_time)): In<(QAngles, f64)>,
        conn: Res<Connection>,
        mut transforms: Query<&mut Next<Transform>>,
    ) {
        let Some(entity) = conn
            .client_state
            .as_ref()
            .and_then(|state| state.server_entity_to_client_entity.get(&state.view?))
        else {
            return;
        };

        if let Ok(mut transform) = transforms.get_mut(*entity) {
            let QAngles { pitch_deg, yaw_deg, roll_deg } = angles;
            let angles: QAngles = [-pitch_deg, yaw_deg, roll_deg].into();

            transform.component.rotation = angles.into();
            transform.elapsed_secs = msg_time;
        }
    }

    #[expect(
        clippy::type_complexity,
        reason = "Lifetimes make it difficult to extract a type alias"
    )]
    pub fn update_entity(
        In((update, msg_time)): In<(EntityUpdate, f64)>,
        mut commands: Commands<'_, '_>,
        mut conn: ResMut<Connection>,
        mut existing_entities: Query<(
            Option<&Baseline>,
            Option<&mut Next<Transform>>,
            Option<&mut MdlSettings>,
            Option<&WorldAssetRoot>,
        )>,
    ) {
        let Some(state) = conn.client_state.as_mut() else {
            return;
        };

        if let Some(entity) = state.server_entity_to_client_entity.get(&update.ent_id).copied() {
            let Ok((baseline, transform, mdl, scene)) = existing_entities.get_mut(entity) else {
                warn!("Server tried to update non-existent entity {}", update.ent_id);
                return;
            };

            // Updates are diffs against the baseline, so an empty update still has meaning: it
            // resets the entity to its baseline state. Quake zero-initialises baselines, so treat a
            // missing one as all-zeroes.
            let full = match baseline {
                Some(Baseline(baseline)) => baseline.apply_update(&update),
                None => EntityState::uninitialized().apply_update(&update),
            };
            let new_transform = entity_state_transform(&full);

            let mut ent = commands.entity(entity);

            // Like Quake's `CL_RelinkEntities`, don't lerp across large jumps (e.g. teleports),
            // otherwise the entity visibly slides through walls over one message interval.
            let teleported = transform.as_ref().is_some_and(|prev| {
                (new_transform.translation - prev.component.translation).abs().max_element() > 100.
            });

            if update.no_lerp || teleported {
                ent.insert(NoInterpolation);
            } else {
                ent.remove::<NoInterpolation>();
            }

            if let Some(PrecacheModel { state: PrecacheModelState::Loaded(model), .. }) =
                state.models.get(full.model_id as usize).cloned()
                && scene.map(|s| &s.0) != Some(&model)
            {
                ent.insert(WorldAssetRoot(model));
            }

            let new_settings = MdlSettings { frame: full.frame_id, skin: full.skin_id };
            if let Some(mut mdl) = mdl {
                write_if_neq!(mdl.frame, new_settings.frame);
                write_if_neq!(mdl.skin, new_settings.skin);
            } else {
                ent.insert(new_settings);
            }

            if let Some(mut transform) = transform {
                transform.component = new_transform;
                transform.elapsed_secs = msg_time;
            } else {
                ent.insert(Next { component: new_transform, elapsed_secs: msg_time });
            }

            if baseline.is_none() {
                ent.insert(Baseline(EntityState::uninitialized()));
            }
        } else {
            if update.model_id.is_none() {
                warn!("Tried to spawn ent without model");
                return;
            }

            let full = EntityState::uninitialized().apply_update(&update);

            let mut ent = commands.spawn((
                Next { component: entity_state_transform(&full), elapsed_secs: msg_time },
                Baseline(EntityState::uninitialized()),
                Visibility::Inherited,
                ChildOf(state.worldspawn),
            ));

            if let Some(PrecacheModel { state: PrecacheModelState::Loaded(model), .. }) =
                state.models.get(full.model_id as usize).cloned()
            {
                ent.insert((
                    WorldAssetRoot(model),
                    MdlSettings { frame: full.frame_id, skin: full.skin_id },
                ));
            } else {
                warn!("Model {} not found (TODO)", full.model_id);
            }

            state.server_entity_to_client_entity.insert(update.ent_id, ent.id());
        }
    }
}

pub mod systems {}
