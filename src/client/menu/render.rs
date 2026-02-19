use bevy::ecs::{component::Component, entity::Entity};

#[derive(Component)]
#[relationship(relationship_target = HasHud)]
pub struct HudFor(pub Entity);

#[derive(Component)]
#[relationship_target(relationship = HudFor, linked_spawn)]
pub struct HasHud(Entity);
