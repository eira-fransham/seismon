//! View effects applied on top of the player's look angles and origin: eye height, view bob, view
//! roll, damage kick and weapon punch. Mirrors vanilla `view.c` (`V_CalcRefdef` and friends).

use std::{f32::consts::PI, time::Duration};

use bevy::{ecs::relationship::RelationshipTarget as _, log::error, prelude::*};
use bevy_mod_commands::Registry;
use seismon_utils::{QAngles, duration_to_f32, write_if_neq};
use serde::Deserialize;

use crate::{
    client::{Connection, inventory::FaceAnim, state::ViewModel, systems::frame::ViewEntities},
    common::net::DEFAULT_VIEWHEIGHT,
};

/// Per-view state used to compute the final camera and viewmodel transforms. Lives on the view
/// entity (the entity set by `SetView`).
#[derive(Component, Reflect, Clone, Debug)]
#[reflect(Component)]
pub struct ViewEffects {
    /// Eye height above the view entity's origin (`view_ofs.z` on the server).
    pub view_height: f32,
    /// The player's velocity as sent by the server, used for bob and roll.
    pub velocity: Vec3,
    /// Weapon-fire kick from the server (`punchangle`).
    pub punch_angles: QAngles,
    pub health: i16,
    /// Damage kick roll in degrees at full strength.
    pub dmg_roll: f32,
    /// Damage kick pitch in degrees at full strength.
    pub dmg_pitch: f32,
    /// Remaining damage kick time in seconds; the kick decays linearly to zero over `v_kicktime`.
    pub dmg_time: f32,
}

impl Default for ViewEffects {
    fn default() -> Self {
        Self {
            view_height: DEFAULT_VIEWHEIGHT,
            velocity: Vec3::ZERO,
            punch_angles: QAngles::default(),
            health: 0,
            dmg_roll: 0.,
            dmg_pitch: 0.,
            dmg_time: 0.,
        }
    }
}

/// The local transform of a view-attached entity (world camera, viewmodel) before any view effects
/// are applied. Captured from whatever template the entity was spawned from.
#[derive(Component, Reflect, Clone, Copy, Debug)]
#[reflect(Component)]
pub struct ViewBase(pub Transform);

/// Inserts `ViewBase` on an entity from its current `Transform`.
pub fn capture_view_base(mut entity: EntityWorldMut) {
    let transform = entity.get::<Transform>().copied().unwrap_or_default();
    entity.insert(ViewBase(transform));
}

/// How long the HUD shows the "pain" face after taking damage.
const FACE_ANIM_SECS: f32 = 0.2;

/// Port of `V_ParseDamage`: start a damage kick away from `source`.
pub fn handle_damage(
    In((armor, blood, source)): In<(u8, u8, Vec3)>,
    mut commands: Commands,
    conn: Res<Connection>,
    registry: Res<Registry>,
    time: Res<Time<Virtual>>,
    mut views: Query<(&Transform, &mut ViewEffects)>,
) {
    let kick_vars: KickVars = match registry.read_cvars() {
        Ok(vars) => vars,
        Err(e) => {
            error!("Could not read kick cvars: {e}");
            return;
        }
    };

    let Some(view) = conn
        .client_state
        .as_ref()
        .and_then(|state| state.server_entity_to_client_entity.get(&state.view?))
    else {
        return;
    };

    // Show the "pain" face on the HUD for a moment (`cl.faceanimtime = cl.time + 0.2`).
    commands.entity(*view).insert(FaceAnim { until_secs: time.elapsed_secs() + FACE_ANIM_SECS });

    let Ok((transform, mut effects)) = views.get_mut(*view) else {
        return;
    };

    let (roll, pitch) = damage_kick(
        armor as f32,
        blood as f32,
        source - transform.translation,
        transform.rotation,
        kick_vars,
    );

    effects.dmg_roll = roll;
    effects.dmg_pitch = pitch;
    effects.dmg_time = kick_vars.kick_time;
}

/// Returns the `(roll, pitch)` of a damage kick, given the vector from the view entity to the
/// damage source and the view entity's rotation.
pub fn damage_kick(
    armor: f32,
    blood: f32,
    to_source: Vec3,
    view_rotation: Quat,
    vars: KickVars,
) -> (f32, f32) {
    let count = (blood * 0.5 + armor * 0.5).max(10.);
    let from = to_source.normalize_or_zero();
    // Quake is Z-up with X forward, so Y is left and right is -Y.
    let forward = view_rotation * Vec3::X;
    let right = view_rotation * Vec3::NEG_Y;

    (count * from.dot(right) * vars.kick_roll, count * from.dot(forward) * vars.kick_pitch)
}

/// Fraction (in `[0, 1]`) of the damage kick remaining with `dmg_time` seconds left.
pub fn kick_factor(dmg_time: f32, kick_time: f32) -> f32 {
    if dmg_time > 0. && kick_time > 0. { (dmg_time / kick_time).min(1.) } else { 0. }
}

/// Apply eye height, bob, roll, damage kick and punch to the world camera and viewmodel.
#[expect(clippy::type_complexity)]
pub fn apply_view_effects(
    time: Res<Time<Virtual>>,
    registry: Res<Registry>,
    mut views: Query<(&Transform, &mut ViewEffects, &ViewEntities)>,
    mut cameras: Query<
        (&mut Transform, &ViewBase),
        (With<Camera3d>, Without<ViewModel>, Without<ViewEffects>),
    >,
    mut viewmodels: Query<(&mut Transform, &ViewBase), (With<ViewModel>, Without<ViewEffects>)>,
) {
    if views.is_empty() {
        return;
    }

    let (bob_vars, roll_vars, kick_vars) = match (
        registry.read_cvars::<BobVars>(),
        registry.read_cvars::<RollVars>(),
        registry.read_cvars::<KickVars>(),
    ) {
        (Ok(bob), Ok(roll), Ok(kick)) => (bob, roll, kick),
        (bob, roll, kick) => {
            for e in [bob.err(), roll.err(), kick.err()].into_iter().flatten() {
                error!("Could not read view cvars: {e}");
            }
            return;
        }
    };

    let dt = time.delta_secs();

    for (view_transform, mut effects, view_entities) in &mut views {
        let bob = bob(time.elapsed(), effects.velocity, bob_vars);

        // V_CalcViewRoll
        let mut offset = QAngles {
            roll_deg: roll(view_transform.rotation, effects.velocity, roll_vars),
            ..default()
        };

        if effects.dmg_time > 0. {
            let factor = kick_factor(effects.dmg_time, kick_vars.kick_time);
            offset.roll_deg += factor * effects.dmg_roll;
            offset.pitch_deg += factor * effects.dmg_pitch;
            effects.dmg_time -= dt;
        }

        if effects.health <= 0 {
            offset.roll_deg = 80.;
        }

        // V_CalcRefdef
        offset = offset + effects.punch_angles;

        // Vanilla offsets the eye in world space (Z-up), but the camera is a child of the view
        // entity, which is rotated by the look angles, so undo that rotation.
        let eye_offset = Vec3::new(1. / 32., 1. / 32., effects.view_height + bob + 1. / 32.);
        let local_eye_offset = view_transform.rotation.inverse() * eye_offset;
        #[expect(unused_variables, reason = "TODO: Fix roll so it's not as disorienting")]
        let offset_rotation = Quat::from(offset);

        for child in view_entities.iter() {
            if let Ok((mut transform, base)) = cameras.get_mut(child) {
                let new = Transform {
                    translation: base.0.translation - Vec3::Z * DEFAULT_VIEWHEIGHT
                        + local_eye_offset,
                    // rotation: offset_rotation * base.0.rotation,
                    // scale: base.0.scale,
                    // TODO: Rotation is kinda messed up
                    ..base.0
                };
                write_if_neq!(*transform, new);
            } else if let Ok((_transform, _base)) = viewmodels.get_mut(child) {
                // TODO: Bob cycle is messed up
                // The viewmodel is rendered by the overlay camera, which doesn't move with the
                // view, so only the bob relative to the eye applies (forward * bob * 0.4). The
                // overlay camera looks down -Z.
                // let new = Transform {
                //     translation: base.0.translation + Vec3::NEG_Z * bob * 0.4,
                //     ..base.0
                // };
                // write_if_neq!(*transform, new);
            }
        }
    }
}

#[derive(Clone, Copy, Debug, Deserialize)]
pub struct MouseVars {
    #[serde(rename(deserialize = "m_pitch"))]
    pub pitch_factor: f32,
    #[serde(rename(deserialize = "m_yaw"))]
    pub yaw_factor: f32,
    #[serde(rename(deserialize = "sensitivity"))]
    pub sensitivity: f32,
}

#[derive(Clone, Copy, Debug, Deserialize)]
pub struct KickVars {
    #[serde(rename(deserialize = "v_kickpitch"))]
    pub kick_pitch: f32,
    #[serde(rename(deserialize = "v_kickroll"))]
    pub kick_roll: f32,
    #[serde(rename(deserialize = "v_kicktime"))]
    pub kick_time: f32,
}

#[derive(Deserialize, Clone, Copy, Debug, Default)]
pub struct BobVars {
    pub cl_bob: f32,
    pub cl_bobcycle: f32,
    pub cl_bobup: f32,
}

/// Port of `V_CalcBob`.
pub fn bob(time: Duration, velocity: Vec3, vars: BobVars) -> f32 {
    let time = duration_to_f32(time);
    let ratio = (time % vars.cl_bobcycle) / vars.cl_bobcycle;
    let cycle = if ratio < vars.cl_bobup {
        PI * ratio / vars.cl_bobup
    } else {
        PI + PI * (ratio - vars.cl_bobup) / (1.0 - vars.cl_bobup)
    };

    // drop z coordinate
    let vel_mag = velocity.truncate().length();
    let bob = vars.cl_bob * (vel_mag * 0.3 + vel_mag * 0.7 * cycle.sin());

    bob.clamp(-7., 4.)
}

#[derive(Deserialize, Clone, Copy, Debug)]
pub struct RollVars {
    pub cl_rollangle: f32,
    pub cl_rollspeed: f32,
}

/// Port of `V_CalcRoll`: roll the view towards the direction of strafing.
pub fn roll(view_rotation: Quat, velocity: Vec3, vars: RollVars) -> f32 {
    // Quake is Z-up with X forward, so Y is left and right is -Y.
    let right = view_rotation * Vec3::NEG_Y;
    let side = velocity.dot(right);
    let sign = if side < 0. { -1. } else { 1. };
    let side_abs = side.abs();

    let roll_abs = if side_abs < vars.cl_rollspeed {
        side_abs * vars.cl_rollangle / vars.cl_rollspeed
    } else {
        vars.cl_rollangle
    };

    roll_abs * sign
}

#[cfg(test)]
mod tests {
    use super::*;

    const BOB: BobVars = BobVars { cl_bob: 0.02, cl_bobcycle: 0.6, cl_bobup: 0.5 };
    const ROLL: RollVars = RollVars { cl_rollangle: 2.0, cl_rollspeed: 200.0 };
    const KICK: KickVars = KickVars { kick_pitch: 0.6, kick_roll: 0.6, kick_time: 0.5 };

    #[test]
    fn bob_is_bounded_and_zero_when_still() {
        for i in 0..1000 {
            let time = Duration::from_millis(i * 7);
            assert_eq!(bob(time, Vec3::ZERO, BOB), 0.);
            for speed in [50., 320., 1000., 10000.] {
                let b = bob(time, Vec3::new(speed, speed * 0.5, 100.), BOB);
                assert!((-7. ..=4.).contains(&b), "bob {b} out of range");
            }
        }
    }

    #[test]
    fn roll_follows_strafe_direction() {
        let yaw = Quat::from(QAngles { yaw_deg: 37., ..default() });
        let right = yaw * Vec3::NEG_Y;

        assert!(roll(yaw, right * 100., ROLL) > 0.);
        assert!(roll(yaw, -right * 100., ROLL) < 0.);
        assert_eq!(roll(yaw, Vec3::ZERO, ROLL), 0.);
        // Forward movement doesn't roll.
        assert!(roll(yaw, yaw * Vec3::X * 300., ROLL).abs() < 1e-4);
        // Linear below `cl_rollspeed`, saturates above.
        assert!((roll(yaw, right * 100., ROLL) - 1.).abs() < 1e-4);
        assert!((roll(yaw, right * 1000., ROLL) - ROLL.cl_rollangle).abs() < 1e-4);
        assert!((roll(yaw, -right * 1000., ROLL) + ROLL.cl_rollangle).abs() < 1e-4);
    }

    #[test]
    fn damage_kick_direction() {
        let rot = Quat::IDENTITY;
        // Hit from the right: roll kick, no pitch.
        let (r, p) = damage_kick(0., 40., Vec3::NEG_Y * 100., rot, KICK);
        assert!((r - 20. * 0.6).abs() < 1e-4 && p.abs() < 1e-4);
        // Hit from the front: pitch kick, no roll. Minimum count is 10.
        let (r, p) = damage_kick(0., 2., Vec3::X * 100., rot, KICK);
        assert!(r.abs() < 1e-4 && (p - 10. * 0.6).abs() < 1e-4);
    }

    #[test]
    fn kick_decays_linearly() {
        assert_eq!(kick_factor(KICK.kick_time, KICK.kick_time), 1.);
        assert!((kick_factor(KICK.kick_time / 2., KICK.kick_time) - 0.5).abs() < 1e-6);
        assert_eq!(kick_factor(0., KICK.kick_time), 0.);
        assert_eq!(kick_factor(-0.1, KICK.kick_time), 0.);
    }
}
