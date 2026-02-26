use bevy::{
    asset::AssetServer,
    ecs::{
        component::Component,
        entity::Entity,
        entity_disabling::Disabled,
        query::With,
        system::{Commands, Query, Res},
    },
    state::state::State,
    ui::{GlobalZIndex, Node, Val, widget::ImageNode},
};

use crate::client::ClientGameState;

#[derive(Component)]
pub struct LoadingScreen;

pub fn init_loading_screen(mut commands: Commands, asset_server: Res<AssetServer>) {
    commands.spawn((
        Node { width: Val::Percent(100.), height: Val::Percent(100.), ..Default::default() },
        // Show above everything
        GlobalZIndex(99),
        LoadingScreen,
        ImageNode { image: asset_server.load("gfx/conback.lmp"), ..Default::default() },
    ));
}

pub fn update_loading_screen_visible(
    mut commands: Commands,
    state: Res<State<ClientGameState>>,
    loading_screens: Query<Entity, With<LoadingScreen>>,
) {
    let visible = match state.get() {
        ClientGameState::WaitForServer | ClientGameState::Loading => true,
        ClientGameState::Disconnected | ClientGameState::InGame => false,
    };

    for loading_screen in loading_screens {
        if visible {
            commands.entity(loading_screen).try_remove::<Disabled>();
        } else {
            commands.entity(loading_screen).try_insert(Disabled);
        }
    }
}
