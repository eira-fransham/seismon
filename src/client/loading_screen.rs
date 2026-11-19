use bevy::{
    asset::AssetServer,
    camera::{Camera, Camera2d, ClearColorConfig, visibility::RenderLayers},
    ecs::{
        component::Component,
        query::With,
        system::{Commands, Query, Res},
    },
    state::state::State,
    ui::{
        Node, UiTargetCamera, Val,
        widget::{ImageNode, NodeImageMode},
    },
};

use crate::client::ClientGameState;

#[derive(Component)]
pub struct LoadingScreen;

#[derive(Component)]
pub struct LoadingScreenCamera;

/// Renders after the world and overlay cameras, so the loading screen covers everything.
const LOADING_SCREEN_CAMERA_ORDER: isize = 100;

pub fn init_loading_screen(mut commands: Commands, asset_server: Res<AssetServer>) {
    let camera = commands
        .spawn((
            Camera2d,
            Camera {
                order: LOADING_SCREEN_CAMERA_ORDER,
                clear_color: ClearColorConfig::None,
                is_active: false,
                ..Default::default()
            },
            RenderLayers::none(),
            LoadingScreenCamera,
        ))
        .id();

    commands.spawn((
        Node { width: Val::Vw(100.), height: Val::Vh(100.), ..Default::default() },
        UiTargetCamera(camera),
        LoadingScreen,
        ImageNode {
            image: asset_server.load("gfx/conback.lmp"),
            image_mode: NodeImageMode::Stretch,
            ..Default::default()
        },
    ));
}

pub fn update_loading_screen_visible(
    state: Res<State<ClientGameState>>,
    mut loading_screen_cameras: Query<&mut Camera, With<LoadingScreenCamera>>,
) {
    let visible = match state.get() {
        ClientGameState::WaitForServer | ClientGameState::Loading => true,
        ClientGameState::Disconnected | ClientGameState::InGame => false,
    };

    for mut camera in &mut loading_screen_cameras {
        // Can't use `Disabled` here, since Bevy ignores it for cameras
        camera.is_active = visible;
    }
}
