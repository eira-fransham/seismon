//! Particle trails left by models with trail flags (`R_RocketTrail`, called from
//! `CL_RelinkEntities`).
//!
//! The MDL loader puts the model's flags as `MdlFlag*` components on the model's root entity,
//! which is a child of the entity the model is attached to. Each such entity gets a persistent
//! emitter, linked with [`TrailOf`]. Every frame, the emitter spawns particles along the segment
//! the entity moved through since the previous frame.
//!
//! Emitters are not children of the moving entity, so a projectile's trail outlives it. Once
//! the entity is gone, the emitter stops spawning and is despawned after its particles die.

use bevy::prelude::*;
use bevy_hanabi::prelude::*;
use bevy_mod_mdl::{
    MdlFlagGib, MdlFlagGrenade, MdlFlagRocket, MdlFlagTracer, MdlFlagTracer2, MdlFlagTracer3,
    MdlFlagZomgib,
};

use super::{
    DespawnAfter, QuakeParticleEffects,
    effects::{PROP_COLOR_BASE, PROP_RAMP_BASE, PROP_TRAIL_END, PROP_TRAIL_SIDE, PROP_TRAIL_START},
};
use crate::client::Connection;

/// The longest lifetime of any trail particle, in seconds.
const TRAIL_MAX_LIFETIME: f32 = 2.;

/// Movement in a single frame beyond which the entity is assumed to have teleported, so no
/// trail is drawn (`CL_RelinkEntities` uses the same threshold).
const TELEPORT_DISTANCE: f32 = 100.;

/// The trail types of `R_RocketTrail`.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TrailKind {
    /// Type 0, `EF_ROCKET`.
    Rocket,
    /// Type 1, `EF_GRENADE`.
    Smoke,
    /// Type 2, `EF_GIB`.
    Blood,
    /// Type 3, `EF_TRACER` (Scrag).
    Tracer,
    /// Type 4, `EF_ZOMGIB`.
    SlightBlood,
    /// Type 5, `EF_TRACER2` (Death Knight).
    Tracer2,
    /// Type 6, `EF_TRACER3` (Vore).
    Voor,
}

impl TrailKind {
    /// Picks the trail for a model's flags, in the same order of precedence as
    /// `CL_RelinkEntities`.
    pub fn from_flags(
        rocket: bool,
        grenade: bool,
        gib: bool,
        zomgib: bool,
        tracer: bool,
        tracer2: bool,
        tracer3: bool,
    ) -> Option<Self> {
        [
            (gib, TrailKind::Blood),
            (zomgib, TrailKind::SlightBlood),
            (tracer, TrailKind::Tracer),
            (tracer2, TrailKind::Tracer2),
            (rocket, TrailKind::Rocket),
            (grenade, TrailKind::Smoke),
            (tracer3, TrailKind::Voor),
        ]
        .into_iter()
        .find_map(|(set, kind)| set.then_some(kind))
    }

    /// Distance between consecutive particles along the trail. Slight blood takes an extra
    /// 3-unit step after each particle.
    fn spacing(self) -> f32 {
        match self {
            TrailKind::SlightBlood => 6.,
            _ => 3.,
        }
    }

    /// Number of particles to spawn along a segment of length `len`: Quake spawns one, then
    /// steps along the segment, for as long as there is length left.
    pub fn particle_count(self, len: f32) -> u32 {
        if len > 0. { (len / self.spacing()).ceil() as u32 } else { 0 }
    }

    fn effect(self, effects: &QuakeParticleEffects) -> Handle<EffectAsset> {
        match self {
            TrailKind::Rocket | TrailKind::Smoke => effects.trail_fire.clone(),
            TrailKind::Blood | TrailKind::SlightBlood => effects.trail_blood.clone(),
            TrailKind::Tracer | TrailKind::Tracer2 => effects.trail_tracer.clone(),
            TrailKind::Voor => effects.trail_voor.clone(),
        }
    }

    fn is_tracer(self) -> bool {
        matches!(self, TrailKind::Tracer | TrailKind::Tracer2)
    }

    fn properties(self) -> EffectProperties {
        let extra: &[(&str, bevy_hanabi::Value)] = match self {
            TrailKind::Rocket => &[(PROP_RAMP_BASE, 0f32.into())],
            TrailKind::Smoke => &[(PROP_RAMP_BASE, 2f32.into())],
            TrailKind::Tracer => {
                &[(PROP_COLOR_BASE, 52f32.into()), (PROP_TRAIL_SIDE, Vec3::ZERO.into())]
            }
            TrailKind::Tracer2 => {
                &[(PROP_COLOR_BASE, 230f32.into()), (PROP_TRAIL_SIDE, Vec3::ZERO.into())]
            }
            TrailKind::Blood | TrailKind::SlightBlood | TrailKind::Voor => &[],
        };

        EffectProperties::default().with_properties(
            [(PROP_TRAIL_START, Vec3::ZERO.into()), (PROP_TRAIL_END, Vec3::ZERO.into())]
                .iter()
                .chain(extra)
                .map(|&(name, value)| (name.to_owned(), value)),
        )
    }
}

/// Links a trail emitter to the model root entity whose flags requested it.
#[derive(Component, Debug)]
#[relationship(relationship_target = Trails)]
pub struct TrailOf(pub Entity);

/// The trail emitters of a model root entity. Emitters are not despawned along with it.
#[derive(Component, Debug)]
#[relationship_target(relationship = TrailOf)]
pub struct Trails(Vec<Entity>);

#[derive(Component, Debug)]
pub struct TrailEmitter {
    kind: TrailKind,
    /// Where the entity was last frame, if it is being trailed.
    prev_origin: Option<Vec3>,
}

type FlagQuery = (
    Entity,
    Has<MdlFlagRocket>,
    Has<MdlFlagGrenade>,
    Has<MdlFlagGib>,
    Has<MdlFlagZomgib>,
    Has<MdlFlagTracer>,
    Has<MdlFlagTracer2>,
    Has<MdlFlagTracer3>,
);

type FlagFilter = (
    Without<Trails>,
    Or<(
        With<MdlFlagRocket>,
        With<MdlFlagGrenade>,
        With<MdlFlagGib>,
        With<MdlFlagZomgib>,
        With<MdlFlagTracer>,
        With<MdlFlagTracer2>,
        With<MdlFlagTracer3>,
    )>,
);

/// Spawns an emitter for every model with a trail flag that doesn't have one yet.
pub fn spawn_trail_emitters(
    mut commands: Commands,
    conn: Option<Res<Connection>>,
    effects: Option<Res<QuakeParticleEffects>>,
    flagged: Query<FlagQuery, FlagFilter>,
) {
    let (Some(conn), Some(effects)) = (conn, effects) else {
        return;
    };
    let Some(worldspawn) = conn.client_state.as_ref().map(|state| state.worldspawn) else {
        return;
    };
    if !effects.is_ready() {
        return;
    }

    for (entity, rocket, grenade, gib, zomgib, tracer, tracer2, tracer3) in &flagged {
        let Some(kind) =
            TrailKind::from_flags(rocket, grenade, gib, zomgib, tracer, tracer2, tracer3)
        else {
            continue;
        };

        commands.spawn((
            Name::new(format!("{kind:?} trail")),
            ParticleEffect::new(kind.effect(&effects)),
            EffectMaterial { images: vec![effects.lut.clone()] },
            kind.properties(),
            EffectSpawner::new(&SpawnerSettings::rate(0.0.into())),
            TrailEmitter { kind, prev_origin: None },
            TrailOf(entity),
            Transform::IDENTITY,
            ChildOf(worldspawn),
        ));
    }
}

type EmitterQuery = (
    Entity,
    &'static mut TrailEmitter,
    Option<&'static TrailOf>,
    &'static mut EffectSpawner,
    &'static mut EffectProperties,
    Has<DespawnAfter>,
);

/// Spawns each trail's particles for this frame. Runs after hanabi ticks the spawners, to
/// override their spawn counts.
pub fn update_trail_emitters(
    mut commands: Commands,
    mut emitters: Query<EmitterQuery>,
    model_roots: Query<&ChildOf>,
    // The model root's parent is the entity the model is attached to. It shares the emitter's
    // parent (`worldspawn`), so its `Transform` is in the emitter's space.
    owners: Query<&Transform>,
) {
    for (entity, mut emitter, trail_of, mut spawner, mut properties, despawning) in &mut emitters {
        spawner.spawn_count = 0;

        let Some(TrailOf(model_root)) = trail_of else {
            // The model is gone: let the remaining particles die out.
            if !despawning {
                commands.entity(entity).insert(DespawnAfter::new(TRAIL_MAX_LIFETIME));
            }
            continue;
        };

        // Fails if the entity is disabled, for example because the server stopped sending it.
        let Some(origin) = model_roots
            .get(*model_root)
            .ok()
            .and_then(|ChildOf(owner)| owners.get(*owner).ok())
            .map(|transform| transform.translation)
        else {
            emitter.prev_origin = None;
            continue;
        };

        let Some(prev_origin) = emitter.prev_origin.replace(origin) else {
            continue;
        };

        let delta = origin - prev_origin;
        if delta.abs().max_element() > TELEPORT_DISTANCE {
            continue;
        }

        let count = emitter.kind.particle_count(delta.length());
        if count > 0 {
            properties.set(PROP_TRAIL_START, prev_origin.into());
            properties.set(PROP_TRAIL_END, origin.into());
            if emitter.kind.is_tracer() {
                properties.set(PROP_TRAIL_SIDE, tracer_side(delta).into());
            }
            spawner.spawn_count = count;
        }
    }
}

/// The sideways velocity of tracer particles, for a trail moving by `delta`.
fn tracer_side(delta: Vec3) -> Vec3 {
    let dir = delta.normalize_or_zero();
    Vec3::new(dir.y, -dir.x, 0.) * 30.
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn flag_precedence_matches_cl_relink_entities() {
        assert_eq!(
            TrailKind::from_flags(true, true, true, true, true, true, true),
            Some(TrailKind::Blood)
        );
        assert_eq!(
            TrailKind::from_flags(true, true, false, true, true, true, true),
            Some(TrailKind::SlightBlood)
        );
        assert_eq!(
            TrailKind::from_flags(true, true, false, false, true, true, true),
            Some(TrailKind::Tracer)
        );
        assert_eq!(
            TrailKind::from_flags(true, true, false, false, false, true, true),
            Some(TrailKind::Tracer2)
        );
        assert_eq!(
            TrailKind::from_flags(true, true, false, false, false, false, true),
            Some(TrailKind::Rocket)
        );
        assert_eq!(
            TrailKind::from_flags(false, true, false, false, false, false, true),
            Some(TrailKind::Smoke)
        );
        assert_eq!(
            TrailKind::from_flags(false, false, false, false, false, false, true),
            Some(TrailKind::Voor)
        );
        assert_eq!(TrailKind::from_flags(false, false, false, false, false, false, false), None);
    }

    #[test]
    fn particle_count_matches_r_rocket_trail() {
        // `while (len > 0) { len -= dec; ... }`
        fn quake_count(mut len: f32, dec: f32) -> u32 {
            let mut count = 0;
            while len > 0. {
                len -= dec;
                count += 1;
            }
            count
        }

        for len in [0., 0.5, 2.9, 3., 3.1, 6., 10., 100., 333.3] {
            assert_eq!(TrailKind::Rocket.particle_count(len), quake_count(len, 3.), "len {len}");
            assert_eq!(
                TrailKind::SlightBlood.particle_count(len),
                quake_count(len, 6.),
                "len {len}"
            );
        }
    }
}
