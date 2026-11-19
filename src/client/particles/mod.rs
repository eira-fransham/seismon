//! Quake's particle effects, simulated and rendered on the GPU with `bevy_hanabi`.
//!
//! Particles come from three sources, as in Quake:
//!
//! - `svc_particle` ([`ServerCmd::Particle`](crate::common::net::ServerCmd::Particle)), e.g.
//!   blood from damage (`R_RunParticleEffect`).
//! - Point temporary entities (`CL_ParseTEnt`): gunshots, spikes, explosions, teleports, etc.
//! - Trails behind models with trail flags (rockets, grenades, gibs, tracers); see [`trails`].
//!
//! The first two are turned into [`ParticleEvent`]s by the server message handlers. Each event
//! spawns one or two short-lived effect instances. The effects themselves are built in
//! [`effects`] to match the behavior of `r_part.c`.
//!
//! Known differences from Quake:
//!
//! - Particles don't collide with the world.
//! - There is no global particle limit (Quake's default is 2048). Each effect instance has its
//!   own fixed capacity instead.
//! - The grid layouts of lava and teleport splashes, and the alternating sides and colors of
//!   tracer trails, are randomized rather than following spawn order, since hanabi has no
//!   per-spawn particle index.
//! - Particles are solid squares with a fixed world-space size, like software Quake, without
//!   GLQuake's distance-based scaling.

pub mod effects;
pub mod palette;
pub mod trails;

use bevy::prelude::*;
use bevy_hanabi::prelude::*;

use self::effects::{
    PROP_COLOR_BASE, PROP_COLOR_LEN, PROP_COLOR_START, PROP_DIRECTION, PROP_PIXEL_SCALE,
    PROP_VIEW_FORWARD, PROP_VIEW_ORIGIN, PType,
};
use crate::{
    client::{Connection, systems::frame::ViewFor},
    common::{
        net::{PointEntityKind, TempEntity},
        wad::{DEFAULT_PALETTE_PATH, Palette},
    },
};

/// A particle effect to spawn, from a server message.
#[derive(Message, Clone, Debug, PartialEq)]
pub enum ParticleEvent {
    /// `R_RunParticleEffect`: a puff of `count` particles around `color`.
    RunEffect { origin: Vec3, direction: Vec3, color: u8, count: u8 },
    /// `R_ParticleExplosion`.
    Explosion { origin: Vec3 },
    /// `R_ParticleExplosion2`.
    ColorExplosion { origin: Vec3, color_start: u8, color_len: u8 },
    /// `R_BlobExplosion`.
    BlobExplosion { origin: Vec3 },
    /// `R_LavaSplash`.
    LavaSplash { origin: Vec3 },
    /// `R_TeleportSplash`.
    TeleportSplash { origin: Vec3 },
}

impl ParticleEvent {
    /// The particles for an `svc_particle` message (`R_ParseParticleEffect`).
    pub fn from_particle_cmd(origin: Vec3, direction: Vec3, count: u8, color: u8) -> Self {
        // A count of 255 means 1024, which `R_RunParticleEffect` turns into an explosion.
        if count == u8::MAX {
            ParticleEvent::Explosion { origin }
        } else {
            ParticleEvent::RunEffect { origin, direction, color, count }
        }
    }

    /// The particles for a temporary entity (`CL_ParseTEnt`), if it has any.
    pub fn from_temp_entity(temp_entity: &TempEntity) -> Option<Self> {
        let TempEntity::Point { kind, origin } = *temp_entity else {
            // Beams are models, not particles.
            return None;
        };

        let run =
            |color, count| ParticleEvent::RunEffect { origin, direction: Vec3::ZERO, color, count };

        Some(match kind {
            PointEntityKind::WizSpike => run(20, 30),
            PointEntityKind::KnightSpike => run(226, 20),
            PointEntityKind::Spike => run(0, 10),
            PointEntityKind::SuperSpike | PointEntityKind::Gunshot => run(0, 20),
            PointEntityKind::Explosion => ParticleEvent::Explosion { origin },
            PointEntityKind::TarExplosion => ParticleEvent::BlobExplosion { origin },
            PointEntityKind::ColorExplosion { color_start, color_len } => {
                ParticleEvent::ColorExplosion { origin, color_start, color_len }
            }
            PointEntityKind::LavaSplash => ParticleEvent::LavaSplash { origin },
            PointEntityKind::Teleport => ParticleEvent::TeleportSplash { origin },
        })
    }

    fn origin(&self) -> Vec3 {
        match *self {
            ParticleEvent::RunEffect { origin, .. }
            | ParticleEvent::Explosion { origin }
            | ParticleEvent::ColorExplosion { origin, .. }
            | ParticleEvent::BlobExplosion { origin }
            | ParticleEvent::LavaSplash { origin }
            | ParticleEvent::TeleportSplash { origin } => origin,
        }
    }

    /// The effect instances making up this event.
    fn instances(&self) -> Vec<EffectInstance> {
        let one_shot = |kind| EffectInstance { kind, count: None, properties: Vec::new() };

        match *self {
            ParticleEvent::RunEffect { direction, color, count, .. } => {
                vec![EffectInstance {
                    kind: EffectKind::RunEffect,
                    count: Some(count.into()),
                    properties: vec![
                        (PROP_DIRECTION, direction.into()),
                        (PROP_COLOR_BASE, f32::from(color & !7).into()),
                    ],
                }]
            }
            ParticleEvent::Explosion { .. } => {
                vec![one_shot(EffectKind::Explode), one_shot(EffectKind::Explode2)]
            }
            ParticleEvent::ColorExplosion { color_start, color_len, .. } => {
                vec![EffectInstance {
                    kind: EffectKind::ColorExplosion,
                    count: None,
                    properties: vec![
                        (PROP_COLOR_START, f32::from(color_start).into()),
                        // Quake takes this modulo, so 0 would crash it.
                        (PROP_COLOR_LEN, f32::from(color_len.max(1)).into()),
                    ],
                }]
            }
            ParticleEvent::BlobExplosion { .. } => {
                vec![one_shot(EffectKind::Blob), one_shot(EffectKind::Blob2)]
            }
            ParticleEvent::LavaSplash { .. } => vec![one_shot(EffectKind::LavaSplash)],
            ParticleEvent::TeleportSplash { .. } => vec![one_shot(EffectKind::TeleportSplash)],
        }
    }
}

/// The one-shot effects.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum EffectKind {
    RunEffect,
    Explode,
    Explode2,
    ColorExplosion,
    Blob,
    Blob2,
    LavaSplash,
    TeleportSplash,
}

impl EffectKind {
    /// The longest lifetime of the effect's particles, after which the instance is despawned.
    fn max_lifetime(self) -> f32 {
        match self {
            EffectKind::RunEffect => 0.4,
            EffectKind::Explode | EffectKind::Explode2 => 0.8,
            EffectKind::ColorExplosion => 0.3,
            EffectKind::Blob | EffectKind::Blob2 => 1.4,
            EffectKind::LavaSplash => 2.62,
            EffectKind::TeleportSplash => 0.34,
        }
    }
}

#[derive(Debug, PartialEq)]
struct EffectInstance {
    kind: EffectKind,
    /// Overrides the effect's spawn count.
    count: Option<u32>,
    properties: Vec<(&'static str, bevy_hanabi::Value)>,
}

/// Handles to the particle effects and their color LUT.
#[derive(Resource)]
pub struct QuakeParticleEffects {
    lut: Handle<Image>,
    palette: Handle<Palette>,
    lut_ready: bool,

    run_effect: Handle<EffectAsset>,
    explode: Handle<EffectAsset>,
    explode2: Handle<EffectAsset>,
    color_explosion: Handle<EffectAsset>,
    blob: Handle<EffectAsset>,
    blob2: Handle<EffectAsset>,
    lava_splash: Handle<EffectAsset>,
    teleport_splash: Handle<EffectAsset>,

    trail_fire: Handle<EffectAsset>,
    trail_blood: Handle<EffectAsset>,
    trail_tracer: Handle<EffectAsset>,
    trail_voor: Handle<EffectAsset>,
}

impl QuakeParticleEffects {
    /// Whether the palette has loaded, so particles can be colored.
    fn is_ready(&self) -> bool {
        self.lut_ready
    }

    fn effect(&self, kind: EffectKind) -> Handle<EffectAsset> {
        match kind {
            EffectKind::RunEffect => &self.run_effect,
            EffectKind::Explode => &self.explode,
            EffectKind::Explode2 => &self.explode2,
            EffectKind::ColorExplosion => &self.color_explosion,
            EffectKind::Blob => &self.blob,
            EffectKind::Blob2 => &self.blob2,
            EffectKind::LavaSplash => &self.lava_splash,
            EffectKind::TeleportSplash => &self.teleport_splash,
        }
        .clone()
    }
}

/// Despawns an effect instance once its particles have died.
///
/// The countdown only starts once the effect is ready, as it doesn't spawn anything before its
/// shaders are compiled.
#[derive(Component, Debug)]
pub struct DespawnAfter {
    remaining: f32,
    /// Upper bound on the wait for the effect to become ready, in case it never does.
    timeout: f32,
}

impl DespawnAfter {
    const READY_TIMEOUT: f32 = 30.;

    pub fn new(secs: f32) -> Self {
        Self { remaining: secs, timeout: Self::READY_TIMEOUT }
    }
}

#[derive(Default)]
pub struct ParticlesPlugin;

impl Plugin for ParticlesPlugin {
    fn build(&self, app: &mut App) {
        app.add_plugins(HanabiPlugin)
            .add_message::<ParticleEvent>()
            .add_systems(Startup, setup)
            .add_systems(Update, update_lut)
            .add_systems(
                PostUpdate,
                (
                    (spawn_particle_effects, trails::spawn_trail_emitters)
                        .before(EffectSystems::TickSpawners)
                        // So new emitters have a valid `GlobalTransform` for
                        // `update_view_properties`.
                        .before(TransformSystems::Propagate),
                    trails::update_trail_emitters.after(EffectSystems::TickSpawners),
                    despawn_finished_effects,
                ),
            )
            .add_systems(
                PostUpdate,
                update_view_properties
                    .after(TransformSystems::Propagate)
                    .after(spawn_particle_effects)
                    .after(trails::spawn_trail_emitters),
            );
    }
}

fn setup(
    mut commands: Commands,
    asset_server: Res<AssetServer>,
    mut images: ResMut<Assets<Image>>,
    mut effects: ResMut<Assets<EffectAsset>>,
) {
    let mut add = |effect| effects.add(effect);

    commands.insert_resource(QuakeParticleEffects {
        lut: images.add(palette::lut_image(None)),
        palette: asset_server.load(DEFAULT_PALETTE_PATH),
        lut_ready: false,

        run_effect: add(effects::run_effect()),
        explode: add(effects::explode(PType::Explode)),
        explode2: add(effects::explode(PType::Explode2)),
        color_explosion: add(effects::color_explosion()),
        blob: add(effects::blob_explosion(PType::Blob)),
        blob2: add(effects::blob_explosion(PType::Blob2)),
        lava_splash: add(effects::lava_splash()),
        teleport_splash: add(effects::teleport_splash()),

        trail_fire: add(effects::trail_fire()),
        trail_blood: add(effects::trail_blood()),
        trail_tracer: add(effects::trail_tracer()),
        trail_voor: add(effects::trail_voor()),
    });
}

/// Fills in the LUT once the palette has loaded.
fn update_lut(
    mut asset_events: MessageReader<AssetEvent<Palette>>,
    palettes: Res<Assets<Palette>>,
    mut images: ResMut<Assets<Image>>,
    effects: Option<ResMut<QuakeParticleEffects>>,
) {
    let Some(mut effects) = effects else {
        return;
    };

    for event in asset_events.read() {
        if let AssetEvent::LoadedWithDependencies { id } | AssetEvent::Modified { id } = *event
            && id == effects.palette.id()
            && let Some(palette) = palettes.get(id)
        {
            if let Err(e) = images.insert(&effects.lut, palette::lut_image(Some(&palette.rgb))) {
                error!("Failed to update the particle palette: {e}");
                continue;
            }
            effects.lut_ready = true;
        }
    }
}

fn spawn_particle_effects(
    mut commands: Commands,
    mut events: MessageReader<ParticleEvent>,
    conn: Option<Res<Connection>>,
    effects: Option<Res<QuakeParticleEffects>>,
) {
    let worldspawn =
        conn.as_ref().and_then(|conn| conn.client_state.as_ref()).map(|s| s.worldspawn);
    let (Some(worldspawn), Some(effects)) = (worldspawn, effects) else {
        events.clear();
        return;
    };
    if !effects.is_ready() {
        events.clear();
        return;
    }

    for event in events.read() {
        let origin = event.origin();

        for EffectInstance { kind, count, properties } in event.instances() {
            let mut instance = commands.spawn((
                Name::new(format!("{kind:?} particles")),
                ParticleEffect::new(effects.effect(kind)),
                EffectMaterial { images: vec![effects.lut.clone()] },
                EffectProperties::default().with_properties(
                    properties.into_iter().map(|(name, value)| (name.to_owned(), value)),
                ),
                DespawnAfter::new(kind.max_lifetime()),
                Transform::from_translation(origin),
                ChildOf(worldspawn),
            ));

            if let Some(count) = count {
                instance.insert(EffectSpawner::new(&SpawnerSettings::once((count as f32).into())));
            }
        }
    }
}

/// The camera rendering the world, as opposed to the overlay camera (which has render layers).
type WorldCameraFilter =
    (With<Camera3d>, With<ViewFor>, Without<bevy::camera::visibility::RenderLayers>);

/// Tells every effect where the camera is, so it can clamp its particles' on-screen size.
fn update_view_properties(
    cameras: Query<(&GlobalTransform, &Projection), WorldCameraFilter>,
    mut effects: Query<(&mut EffectProperties, &GlobalTransform), With<ParticleEffect>>,
) {
    let Some((camera, projection)) = cameras.iter().next() else {
        return;
    };
    let Projection::Perspective(perspective) = projection else {
        return;
    };

    // Quake's pixel limits are for a 200-line screen. This is a ratio of size to depth, so it
    // doesn't depend on the space they're measured in.
    let pixel_scale = 2. * (perspective.fov / 2.).tan() / 200.;

    for (mut properties, emitter) in &mut effects {
        let (origin, forward) = view_in_effect_space(emitter, camera);
        properties.set(PROP_VIEW_ORIGIN, origin.into());
        properties.set(PROP_VIEW_FORWARD, forward.into());
        properties.set(PROP_PIXEL_SCALE, pixel_scale.into());
    }
}

/// The camera's position and forward vector in an effect's local simulation space.
fn view_in_effect_space(emitter: &GlobalTransform, camera: &GlobalTransform) -> (Vec3, Vec3) {
    let world_to_effect = emitter.affine().inverse();
    let origin = world_to_effect.transform_point3(camera.translation());
    let forward = world_to_effect.transform_vector3(camera.forward().as_vec3()).normalize_or_zero();
    (origin, forward)
}

fn despawn_finished_effects(
    mut commands: Commands,
    time: Res<Time>,
    mut effects: Query<(Entity, &mut DespawnAfter, Option<&CompiledParticleEffect>)>,
) {
    let dt = time.delta_secs();

    for (entity, mut despawn, compiled) in &mut effects {
        if compiled.is_some_and(CompiledParticleEffect::is_ready) {
            despawn.remaining -= dt;
        } else {
            despawn.timeout -= dt;
        }

        if despawn.remaining <= 0. || despawn.timeout <= 0. {
            commands.entity(entity).despawn();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn view_is_converted_to_quake_space() {
        // Like `worldspawn`'s transform from bevy_trenchbroom: Z-up to Y-up, scaled down.
        let quake_to_bevy = Mat3::from_cols(Vec3::NEG_Z, Vec3::NEG_X, Vec3::Y) / 39.37;
        let worldspawn = Transform::from_matrix(Mat4::from_mat3(quake_to_bevy));
        let emitter_origin = Vec3::new(100., -50., 20.);
        let emitter =
            GlobalTransform::from(worldspawn) * GlobalTransform::from_translation(emitter_origin);

        // A camera 64 units above the emitter, in Quake space, looking straight down.
        let camera_quake = emitter_origin + Vec3::Z * 64.;
        let camera = GlobalTransform::from(
            Transform::from_translation(quake_to_bevy * camera_quake)
                .looking_to(quake_to_bevy * Vec3::NEG_Z, quake_to_bevy * Vec3::X),
        );

        let (origin, forward) = view_in_effect_space(&emitter, &camera);
        assert!(origin.abs_diff_eq(Vec3::Z * 64., 1e-3), "{origin}");
        assert!(forward.abs_diff_eq(Vec3::NEG_Z, 1e-5), "{forward}");
    }

    #[test]
    fn particle_cmd_full_count_is_an_explosion() {
        let origin = Vec3::new(1., 2., 3.);
        assert_eq!(
            ParticleEvent::from_particle_cmd(origin, Vec3::X, 255, 73),
            ParticleEvent::Explosion { origin }
        );
        assert_eq!(
            ParticleEvent::from_particle_cmd(origin, Vec3::X, 20, 73),
            ParticleEvent::RunEffect { origin, direction: Vec3::X, color: 73, count: 20 }
        );
    }

    #[test]
    fn temp_entities_match_cl_parse_tent() {
        let origin = Vec3::new(-5., 0., 12.);
        let point = |kind| ParticleEvent::from_temp_entity(&TempEntity::Point { kind, origin });
        let run = |color, count| {
            Some(ParticleEvent::RunEffect { origin, direction: Vec3::ZERO, color, count })
        };

        assert_eq!(point(PointEntityKind::WizSpike), run(20, 30));
        assert_eq!(point(PointEntityKind::KnightSpike), run(226, 20));
        assert_eq!(point(PointEntityKind::Spike), run(0, 10));
        assert_eq!(point(PointEntityKind::SuperSpike), run(0, 20));
        assert_eq!(point(PointEntityKind::Gunshot), run(0, 20));
        assert_eq!(point(PointEntityKind::Explosion), Some(ParticleEvent::Explosion { origin }));
        assert_eq!(
            point(PointEntityKind::TarExplosion),
            Some(ParticleEvent::BlobExplosion { origin })
        );
        assert_eq!(
            point(PointEntityKind::ColorExplosion { color_start: 40, color_len: 8 }),
            Some(ParticleEvent::ColorExplosion { origin, color_start: 40, color_len: 8 })
        );
        assert_eq!(point(PointEntityKind::LavaSplash), Some(ParticleEvent::LavaSplash { origin }));
        assert_eq!(
            point(PointEntityKind::Teleport),
            Some(ParticleEvent::TeleportSplash { origin })
        );

        assert_eq!(
            ParticleEvent::from_temp_entity(&TempEntity::Beam {
                kind: crate::common::net::BeamEntityKind::Grapple,
                entity_id: 1,
                start: Vec3::ZERO,
                end: Vec3::ONE,
            }),
            None
        );
    }

    #[test]
    fn explosions_split_their_particles() {
        let origin = Vec3::ZERO;
        let kinds = |event: ParticleEvent| {
            event.instances().into_iter().map(|i| i.kind).collect::<Vec<_>>()
        };

        assert_eq!(
            kinds(ParticleEvent::Explosion { origin }),
            [EffectKind::Explode, EffectKind::Explode2]
        );
        assert_eq!(
            kinds(ParticleEvent::BlobExplosion { origin }),
            [EffectKind::Blob, EffectKind::Blob2]
        );
        assert_eq!(effects::EXPLOSION_COUNT % 2, 0);
    }

    #[test]
    fn run_effect_clears_low_color_bits() {
        let [instance] = ParticleEvent::RunEffect {
            origin: Vec3::ZERO,
            direction: Vec3::Y,
            color: 0x4d,
            count: 7,
        }
        .instances()
        .try_into()
        .unwrap();

        assert_eq!(instance.count, Some(7));
        assert!(instance.properties.contains(&(PROP_COLOR_BASE, 72f32.into())));
        assert!(instance.properties.contains(&(PROP_DIRECTION, Vec3::Y.into())));
    }
}
