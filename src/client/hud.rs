use bevy::{
    asset::AssetServer,
    ecs::{bundle::Bundle, component::Component, entity::Entity, reflect::ReflectComponent},
    reflect::Reflect,
    ui::{Node, Val, widget::ImageNode},
};

#[derive(Component, Reflect)]
#[reflect(Component)]
#[relationship(relationship_target = HasHud)]
pub struct HudFor(pub Entity);

#[derive(Component, Reflect)]
#[reflect(Component)]
#[relationship_target(relationship = HudFor, linked_spawn)]
pub struct HasHud(Entity);

/// Create a HUD for the given entity.
pub fn build_hud(owner: Entity, asset_server: &AssetServer) -> impl Bundle {
    (
        HudFor(owner),
        Node { bottom: Val::Percent(100.), ..Default::default() },
        ImageNode { image: asset_server.load("gfx.wad#SBAR"), ..Default::default() },
    )
}
