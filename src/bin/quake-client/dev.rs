use bevy::{input::mouse::AccumulatedMouseMotion, prelude::*, window::PrimaryWindow};
use clap::Parser;
use seismon::common::console::ExecResult;

pub const SENSITIVITY: f32 = 0.0007;
pub const BASE_MOVEMENT_SPEED: f32 = 160.;

#[derive(Parser)]
#[command(name = "dev_toggle_freecam", about = "Switch to freecam mode")]
struct Freecam;

fn cmd_freecam(
    In(Freecam): In<Freecam>,
    mut commands: Commands,
    cameras: Query<(Entity, Option<&DebugCamera>), With<Camera3d>>,
) -> ExecResult {
    let debug_enabled = cameras.iter().all(|(_, d)| d.is_none());

    for (camera, _) in cameras {
        commands.entity(camera).remove::<DebugCamera>().insert_if(DebugCamera, || debug_enabled);
    }

    default()
}

pub struct DevtoolsPlugins;
impl Plugin for DevtoolsPlugins {
    fn build(&self, app: &mut App) {
        app.add_plugins(bevy::dev_tools::fps_overlay::FpsOverlayPlugin::default())
            .add_plugins((
                bevy_inspector_egui::bevy_egui::EguiPlugin::default(),
                bevy_inspector_egui::quick::WorldInspectorPlugin::default(),
            ))
            .add_systems(Update, (Self::move_debug_camera, Self::draw_debug_transforms));
    }
}

impl DevtoolsPlugins {
    pub fn move_debug_camera(
        mouse_motion: Res<AccumulatedMouseMotion>,
        keyboard: Res<ButtonInput<KeyCode>>,
        mut camera_query: Query<&mut Transform, With<DebugCamera>>,
        time: Res<Time>,
    ) {
        for mut transform in &mut camera_query {
            // Mouse movement
            let (mut yaw, mut pitch, _) = transform.rotation.to_euler(EulerRot::YXZ);

            pitch -= mouse_motion.delta.y * SENSITIVITY;
            yaw -= mouse_motion.delta.x * SENSITIVITY;

            pitch = pitch.clamp(-std::f32::consts::FRAC_PI_2, std::f32::consts::FRAC_PI_2);

            transform.rotation =
                Quat::from_axis_angle(Vec3::Y, yaw) * Quat::from_axis_angle(Vec3::X, pitch);

            // Keyboard movement
            let mut movement = Vec3::ZERO;
            if keyboard.pressed(KeyCode::KeyW) {
                movement += *transform.forward()
            }
            if keyboard.pressed(KeyCode::KeyS) {
                movement += *transform.back()
            }
            if keyboard.pressed(KeyCode::KeyA) {
                movement += *transform.left()
            }
            if keyboard.pressed(KeyCode::KeyD) {
                movement += *transform.right()
            }
            movement = movement.normalize_or_zero();
            if keyboard.pressed(KeyCode::Space) {
                movement.y += 1.
            }
            if keyboard.pressed(KeyCode::ControlLeft) {
                movement.y -= 1.
            }

            movement *= BASE_MOVEMENT_SPEED;

            if keyboard.pressed(KeyCode::AltLeft) {
                movement *= 10.;
            } else if keyboard.pressed(KeyCode::ShiftLeft) {
                movement *= 3.;
            }

            transform.translation += movement * time.delta_secs();
        }
    }

    pub fn draw_debug_transforms(
        mut gizmos: Gizmos,
        keyboard: Res<ButtonInput<KeyCode>>,
        query: Query<&GlobalTransform, Without<Camera>>,
        mut enabled: Local<bool>,
    ) {
        if keyboard.just_pressed(KeyCode::KeyG) {
            *enabled = !*enabled;
        }

        if !*enabled {
            return;
        }

        for global_transform in &query {
            let translation = global_transform.translation();
            gizmos.line(
                translation,
                global_transform.transform_point(Vec3::X),
                Color::srgb(1., 0., 0.),
            );
            gizmos.line(
                translation,
                global_transform.transform_point(Vec3::Y),
                Color::srgb(0., 1., 0.),
            );
            gizmos.line(
                translation,
                global_transform.transform_point(Vec3::Z),
                Color::srgb(0., 0., 1.),
            );
        }
    }
}

#[derive(Component)]
#[require(Transform, Camera3d)]
pub struct DebugCamera;
