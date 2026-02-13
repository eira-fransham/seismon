use std::{f32::consts::PI, time::Duration};

use crate::common::{
    console::Registry,
    math::{self, Angles},
};

use super::IntermissionKind;
use bevy::{
    log::error,
    math::{Vec2, Vec3},
};
use seismon_utils::{duration_from_f32, duration_to_f32};
use serde::Deserialize;

#[derive(Default, Clone)]
pub struct View {
    // entity "holding" the camera
    entity_id: usize,

    // how high the entity is "holding" the camera
    view_height: f32,

    // TODO
    ideal_pitch: f32,

    // view angles from client input
    input_angles: Angles,

    // pitch and roll from damage
    damage_angles: Angles,

    // time at which damage punch decays to zero
    damage_time: Duration,

    // punch angles from server
    punch_angles: Angles,

    // final angles combining all sources
    final_angles: Angles,

    // final origin accounting for view bob
    final_origin: Vec3,
}

impl View {
    pub fn new() -> View {
        Self::default()
    }

    pub fn entity_id(&self) -> usize {
        self.entity_id
    }

    pub fn set_entity_id(&mut self, id: usize) {
        self.entity_id = id;
    }

    pub fn view_height(&self) -> f32 {
        self.view_height
    }

    pub fn set_view_height(&mut self, view_height: f32) {
        self.view_height = view_height;
    }

    pub fn ideal_pitch(&self) -> f32 {
        self.ideal_pitch
    }

    pub fn set_ideal_pitch(&mut self, ideal_pitch: f32) {
        self.ideal_pitch = ideal_pitch;
    }

    pub fn punch_angles(&self) -> Angles {
        self.punch_angles
    }

    pub fn set_punch_angles(&mut self, punch_angles: Angles) {
        self.punch_angles = punch_angles;
    }

    pub fn input_angles(&self) -> Angles {
        self.input_angles
    }

    /// Update the current input angles with a new value.
    pub fn update_input_angles(&mut self, input_angles: Angles) {
        self.input_angles = input_angles;
    }

    // TODO: This should be handled by a system, and the `cl_*` should be handled by `read_cvars`.
    //       Needs a bevy_trenchbroom-like system where we create entities on map load.
    #[expect(clippy::too_many_arguments)]
    pub fn handle_input(
        &mut self,
        frame_time: Duration,
        registry: &Registry,
        intermission: Option<&IntermissionKind>,
        mlook: bool,
        cl_anglespeedkey: f32,
        cl_pitchspeed: f32,
        cl_yawspeed: f32,
        mouse_vars: MouseVars,
    ) {
        let frame_time_f32 = duration_to_f32(frame_time);
        let speed = if registry.is_pressed("speed") {
            frame_time_f32 * cl_anglespeedkey
        } else {
            frame_time_f32
        };

        // ignore camera controls during intermission
        if intermission.is_some() {
            return;
        }

        if !registry.is_pressed("strafe") {
            let right_factor = registry.is_pressed("right") as i32 as f32;
            let left_factor = registry.is_pressed("left") as i32 as f32;
            self.input_angles.yaw += speed * cl_yawspeed * (left_factor - right_factor);
            self.input_angles.yaw = self.input_angles.yaw.rem_euclid(360.);
        }

        let lookup_factor = registry.is_pressed("lookup") as i32 as f32;
        let lookdown_factor = registry.is_pressed("lookdown") as i32 as f32;
        self.input_angles.pitch += speed * cl_pitchspeed * (lookdown_factor - lookup_factor);

        if mlook {
            let pitch_factor = mouse_vars.pitch_factor * mouse_vars.sensitivity;
            let yaw_factor = mouse_vars.yaw_factor * mouse_vars.sensitivity;
            match registry.read_cvar::<Vec2>("mousedelta") {
                Ok(mouse_move) => {
                    let mouse_move = mouse_move * Vec2::new(yaw_factor, pitch_factor);
                    self.input_angles.yaw -= mouse_move.x;
                    self.input_angles.pitch += mouse_move.y;
                }
                Err(e) => {
                    error!("Could not read mouse delta: {e}");
                }
            }
        }

        if lookup_factor != 0.0 || lookdown_factor != 0.0 {
            // TODO: V_StopPitchDrift
        }

        // clamp pitch to [-70, 80] and roll to [-50, 50]
        self.input_angles.pitch = math::clamp_deg(self.input_angles.pitch, -70.0, 80.0);
        self.input_angles.roll = math::clamp_deg(self.input_angles.roll, -50.0, 50.0);
    }

    // TODO: This should be handled by a system, and the `cl_*` should be handled by `read_cvars`.
    //       Needs a bevy_trenchbroom-like system where we create entities on map load.
    #[expect(clippy::too_many_arguments)]
    pub fn handle_damage(
        &mut self,
        time: Duration,
        armor_dmg: f32,
        health_dmg: f32,
        view_ent_origin: Vec3,
        view_ent_angles: Angles,
        src_origin: Vec3,
        vars: KickVars,
    ) {
        self.damage_time = time + duration_from_f32(vars.kick_time);

        // dmg_factor is at most 10.0
        let dmg_factor = (armor_dmg + health_dmg).min(20.0) / 2.0;
        let dmg_vector = (view_ent_origin - src_origin).normalize();
        let rot = view_ent_angles.mat3_quake();

        let roll_factor = dmg_vector.dot(-rot.x_axis);
        self.damage_angles.roll = dmg_factor * roll_factor * vars.kick_roll;

        let pitch_factor = dmg_vector.dot(rot.y_axis);
        self.damage_angles.pitch = dmg_factor * pitch_factor * vars.kick_pitch;
    }

    pub fn calc_final_angles(
        &mut self,
        time: Duration,
        intermission: Option<&IntermissionKind>,
        velocity: Vec3,
        mut idle_vars: IdleVars,
        kick_vars: KickVars,
        roll_vars: RollVars,
    ) {
        let move_angles =
            Angles { pitch: 0.0, roll: roll(self.input_angles, velocity, roll_vars), yaw: 0.0 };

        let kick_factor = duration_to_f32(self.damage_time - time).max(0.0) / kick_vars.kick_time;
        let damage_angles = self.damage_angles * kick_factor;

        // always idle during intermission
        if intermission.is_some() {
            idle_vars.v_idlescale = 1.0;
        }
        let idle_angles = idle(time, idle_vars);

        self.final_angles =
            self.input_angles + move_angles + damage_angles + self.punch_angles + idle_angles;
    }

    pub fn final_angles(&self) -> Angles {
        self.final_angles
    }

    pub fn calc_final_origin(
        &mut self,
        time: Duration,
        origin: Vec3,
        velocity: Vec3,
        bob_vars: BobVars,
    ) {
        // offset the view by 1/32 unit to keep it from intersecting liquid planes
        let plane_offset = Vec3::splat(1.0 / 32.0);
        let height_offset = Vec3::Z * self.view_height;
        let bob_offset = Vec3::Z * bob(time, velocity, bob_vars);
        self.final_origin = origin + plane_offset + height_offset + bob_offset;
    }

    pub fn final_origin(&self) -> Vec3 {
        self.final_origin
    }

    pub fn viewmodel_angle(&self) -> Angles {
        // TODO
        self.final_angles()
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

pub fn roll(angles: Angles, velocity: Vec3, vars: RollVars) -> f32 {
    let rot = angles.mat3_quake();
    let side = velocity.dot(rot.y_axis);
    let sign = side.signum();
    let side_abs = side.abs();

    let roll_abs = if side < vars.cl_rollspeed {
        side_abs * vars.cl_rollangle / vars.cl_rollspeed
    } else {
        vars.cl_rollangle
    };

    roll_abs * sign
}

#[derive(Deserialize, Clone, Copy, Debug)]
pub struct IdleVars {
    pub v_idlescale: f32,
    pub v_ipitch_cycle: f32,
    pub v_ipitch_level: f32,
    pub v_iroll_cycle: f32,
    pub v_iroll_level: f32,
    pub v_iyaw_cycle: f32,
    pub v_iyaw_level: f32,
}

pub fn idle(time: Duration, vars: IdleVars) -> Angles {
    let time = duration_to_f32(time);
    let pitch = vars.v_idlescale * (time * vars.v_ipitch_cycle).sin() * vars.v_ipitch_level;
    let roll = vars.v_idlescale * (time * vars.v_iroll_cycle).sin() * vars.v_iroll_level;
    let yaw = vars.v_idlescale * (time * vars.v_iyaw_cycle).sin() * vars.v_iyaw_level;

    Angles { pitch, roll, yaw }
}
