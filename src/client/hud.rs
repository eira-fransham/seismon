use std::{io::Write as _, marker::PhantomData};

use bevy::{
    app::{Plugin, PostUpdate},
    asset::AssetServer,
    ecs::{
        bundle::Bundle,
        children,
        component::Component,
        entity::Entity,
        hierarchy::ChildOf,
        lifecycle::{Insert, Remove},
        name::Name,
        observer::On,
        query::{Changed, Has, With, Without},
        reflect::ReflectComponent,
        system::{Command, Commands, Query, Res},
    },
    log::{error, warn},
    reflect::Reflect,
    time::{Time, Virtual},
    ui::{
        Display, FlexDirection, JustifyContent, JustifySelf, Node, PositionType, UiRect, Val,
        widget::ImageNode,
    },
};
use seismon_utils::QStr;

use crate::client::{
    inventory::{ActiveWeaponFor, Ammo, InventoryItem, Item, Sigil, Weapon},
    text::{AtlasText, Conchars},
};

#[derive(Default)]
#[non_exhaustive]
pub struct HudPlugin {}

impl Plugin for HudPlugin {
    fn build(&self, app: &mut bevy::app::App) {
        app.add_observer(observers::add_item_hud::<Weapon, HudWeapons>(24.))
            .add_observer(observers::add_item_hud::<Sigil, HudSigils>(8.))
            .add_observer(observers::add_ammo_hud)
            .add_observer(observers::mark_picking_up)
            .add_observer(observers::make_weapon_active)
            .add_observer(observers::make_weapon_inactive)
            .add_systems(
                PostUpdate,
                (systems::update_ammo, systems::update_pickup_animation, systems::update_health),
            );
    }
}

#[derive(Component, Reflect, Debug)]
#[component(storage = "SparseSet")]
#[reflect(Component)]
#[relationship(relationship_target = HasHud)]
pub struct HudOf(pub Entity);

#[derive(Component, Reflect, Debug)]
#[component(storage = "SparseSet")]
#[reflect(Component)]
#[relationship_target(relationship = HudOf, linked_spawn)]
pub struct HasHud(Entity);

#[derive(Component, Reflect, Debug)]
#[component(storage = "SparseSet")]
#[reflect(Component)]
#[relationship(relationship_target = HudContainers<Kind>)]
pub struct HudContainer<Kind>
where
    Kind: Send + Sync + 'static,
{
    #[relationship]
    pub hud: Entity,
    _phantom: PhantomData<Kind>,
}

// Marker for the HUD health text
pub enum HudHealth {}

// Marker for the HUD weapon container
pub enum HudWeapons {}

// Marker for the HUD sigil container
pub enum HudSigils {}

// Marker for the HUD ammo container
pub enum HudAmmo {}

#[derive(Component, Reflect, Debug)]
#[reflect(Component)]
#[component(storage = "SparseSet")]
#[relationship_target(relationship = HudContainer<Kind>)]
pub struct HudContainers<Kind>
where
    Kind: Send + Sync + 'static,
{
    #[relationship]
    container: Entity,
    _phantom: PhantomData<Kind>,
}

#[derive(Component, Reflect, Debug)]
#[component(storage = "SparseSet")]
#[reflect(Component)]
#[relationship(relationship_target = HudRepresented)]
pub struct HudForItem {
    #[relationship]
    pub representing: Entity,
}

#[derive(Component, Reflect, Debug)]
#[component(storage = "SparseSet")]
#[reflect(Component)]
#[relationship_target(relationship = HudForItem, linked_spawn)]
pub struct HudRepresented(Entity);

const STATUS_BAR_SCALE: f32 = 1.8;

/// This is the default status bar width, and we can use it as a baseline for the
/// rest of the UI.
const STATUS_BAR_WIDTH: f32 = 320.;
const STATUS_BAR_HEIGHT: f32 = 50.;

const HEALTH_CONTAINER_NAME: &str = "health";
const AMMO_CONTAINER_NAME: &str = "ammo";
const WEAPONS_CONTAINER_NAME: &str = "weapons";
const SIGILS_CONTAINER_NAME: &str = "sigils";

fn status_w_px(value: f32) -> Val {
    Val::Px(value * STATUS_BAR_SCALE)
}

fn status_h_px(value: f32) -> Val {
    Val::Px(value * STATUS_BAR_SCALE)
}

// TODO: Turn this into an observer
pub struct MakeHud<B = ()> {
    pub owner: Entity,
    pub components: B,
}

impl<B> Command for MakeHud<B>
where
    B: Bundle,
{
    fn apply(self, world: &mut bevy::ecs::world::World) -> () {
        let asset_server = world.resource::<AssetServer>();
        let conchars = world.resource::<Conchars>().clone();
        let ibar = asset_server.load("gfx.wad#IBAR");
        let sbar = asset_server.load("gfx.wad#SBAR");

        let root = world
            .spawn((
                HudOf(self.owner),
                Node {
                    position_type: PositionType::Absolute,
                    bottom: Val::Percent(0.),
                    justify_self: JustifySelf::Center,
                    width: Val::Px(STATUS_BAR_WIDTH * STATUS_BAR_SCALE),
                    height: Val::Px(STATUS_BAR_HEIGHT * STATUS_BAR_SCALE),
                    ..Default::default()
                },
                self.components,
            ))
            .id();

        world.spawn((
            Node {
                width: Val::Percent(100.),
                height: Val::Percent(100.),
                flex_direction: FlexDirection::Column,
                ..Default::default()
            },
            ChildOf(root),
            children![
                (
                    Node {
                        width: Val::Percent(100.),
                        height: Val::Percent(50.),
                        display: Display::Block,
                        ..Default::default()
                    },
                    ImageNode { image: ibar, ..Default::default() },
                    children![
                        (
                            Name::new(WEAPONS_CONTAINER_NAME),
                            HudContainer::<HudWeapons> { hud: root, _phantom: PhantomData },
                            Node {
                                position_type: PositionType::Absolute,
                                left: Val::Px(0.),
                                bottom: Val::Px(0.),
                                ..Default::default()
                            },
                        ),
                        (
                            Name::new(SIGILS_CONTAINER_NAME),
                            HudContainer::<HudSigils> { hud: root, _phantom: PhantomData },
                            Node {
                                position_type: PositionType::Absolute,
                                left: status_w_px(288.),
                                bottom: Val::Px(0.),
                                ..Default::default()
                            },
                        ),
                        (
                            Name::new(AMMO_CONTAINER_NAME),
                            HudContainer::<HudAmmo> { hud: root, _phantom: PhantomData },
                            Node {
                                position_type: PositionType::Absolute,
                                left: status_w_px(10.),
                                top: status_h_px(-1.),
                                ..Default::default()
                            },
                        )
                    ]
                ),
                (
                    Node {
                        width: Val::Percent(100.),
                        height: Val::Percent(50.),
                        display: Display::Block,
                        ..Default::default()
                    },
                    ImageNode { image: sbar, ..Default::default() },
                    children![(
                        Name::new(HEALTH_CONTAINER_NAME),
                        HudContainer::<HudHealth> { hud: root, _phantom: PhantomData },
                        Node {
                            position_type: PositionType::Absolute,
                            // TODO: Magic numbers
                            left: status_w_px(136.),
                            bottom: status_h_px(3.),
                            ..Default::default()
                        },
                        AtlasText {
                            text: "".into(),
                            image: conchars.image.into(),
                            layout: conchars.layout,
                            glyph_size: conchars.glyph_size.into(),
                            line_padding: UiRect { top: Val::Px(4.), ..Default::default() },
                            justify: JustifyContent::FlexEnd,
                        },
                    )]
                )
            ],
        ));
    }
}

const WEAPON_PICKUP_FPS: f32 = 10.;

#[derive(Component)]
struct PickingUp;

mod systems {
    use seismon_utils::{QString, StringColor, write_if_neq};

    use crate::client::inventory::Health;

    use super::*;

    pub fn update_ammo(
        ammos: Query<(&Ammo, &HudRepresented), Changed<Ammo>>,
        mut atlas_text: Query<&mut AtlasText>,
    ) {
        for (ammo, hud) in ammos {
            let mut text = [0u8; 3];

            if let Err(e) = write!(std::io::Cursor::new(&mut text[..]), "{: >3}", ammo.amount) {
                warn!("Could not fully write ammo counter: {e}");
            }

            let Ok(mut hud) = atlas_text.get_mut(hud.0) else {
                warn!("HUD element representing ammo does not have a text component");
                continue;
            };

            hud.text = QStr::from(&text[..]).into_owned();
        }
    }

    pub fn update_pickup_animation(
        mut commands: Commands,
        mut weapon_images: Query<&mut ImageNode>,
        weapons: Query<
            (Entity, &Weapon, &Item, &HudRepresented, Has<ActiveWeaponFor>),
            With<PickingUp>,
        >,
        time: Res<Time<Virtual>>,
    ) {
        for (entity, weapon, item, hud_element, is_active) in weapons {
            let Ok(mut hud_element) = weapon_images.get_mut(hud_element.0) else {
                warn!("Tried to set weapon active but no HUD element for it was found");
                return;
            };

            let weapon_frame = (time.elapsed_secs() - weapon.pickup_time_secs) * WEAPON_PICKUP_FPS;
            let weapon_frame = weapon_frame.clamp(0., weapon.pickup.len() as f32) as usize;

            let new_image = weapon
                .pickup
                .get(weapon_frame)
                .unwrap_or_else(|| {
                    commands.entity(entity).remove::<PickingUp>();

                    if is_active { &weapon.active } else { &item.gfx }
                })
                .clone();

            write_if_neq!(hud_element.image, new_image);
        }
    }

    pub fn update_health(
        mut atlas_text: Query<&mut AtlasText>,
        hud_owners: Query<(&HasHud, &Health), Changed<Health>>,
        health_container: Query<&HudContainers<HudHealth>>,
    ) {
        for (hud, Health(health)) in hud_owners {
            let Ok(health_container) = health_container.get(hud.0) else {
                continue;
            };
            let Ok(mut text) = atlas_text.get_mut(health_container.container) else {
                continue;
            };

            let mut new_text = QString::from(format!("{health}"));
            if *health <= 25 {
                new_text.set_color(StringColor::Red);
            }

            text.text = new_text;
        }
    }
}

mod observers {
    use bevy::ecs::system::{IntoObserverSystem, ObserverSystem};

    use super::*;

    pub fn add_item_hud<Kind, Container>(width: f32) -> impl ObserverSystem<Insert, Kind, ()>
    where
        Kind: Component,
        Container: Send + Sync + 'static,
    {
        let observer = {
            move |trigger: On<Insert, Kind>,
                  mut commands: Commands,
                  owners: Query<&InventoryItem>,
                  hud_owners: Query<&HasHud>,
                  item_containers: Query<&HudContainers<Container>>,
                  items: Query<&Item>| {
                let new_item = trigger.entity;

                let Ok(item) = items.get(new_item) else {
                    error!("Invalid weapon");
                    return;
                };

                let Ok(InventoryItem { owner }) = owners.get(new_item) else {
                    error!("Weapon added that does not have an owner");
                    return;
                };

                let Ok(HasHud(hud_root)) = hud_owners.get(*owner) else {
                    error!("Weapon owner has no HUD");
                    return;
                };

                let Ok(HudContainers { container: weapon_container, .. }) =
                    item_containers.get(*hud_root)
                else {
                    error!("HUD has no containers");
                    return;
                };

                commands.spawn((
                    Node {
                        position_type: PositionType::Absolute,
                        left: status_w_px(width) * item.order as f32,
                        bottom: status_w_px(0.),
                        width: status_w_px(width),
                        ..Default::default()
                    },
                    HudForItem { representing: new_item },
                    ImageNode { image: item.gfx.clone(), ..Default::default() },
                    ChildOf(*weapon_container),
                ));
            }
        };

        IntoObserverSystem::into_system(observer)
    }

    pub fn add_ammo_hud(
        trigger: On<Insert, Ammo>,
        mut commands: Commands,
        conchars: Res<Conchars>,
        owners: Query<&InventoryItem>,
        hud_owners: Query<&HasHud>,
        ammo_containers: Query<&HudContainers<HudAmmo>>,
        ammos: Query<&Item>,
    ) {
        let new_ammo = trigger.entity;

        let Ok(ammo) = ammos.get(new_ammo) else {
            error!("Invalid weapon");
            return;
        };

        let Ok(InventoryItem { owner }) = owners.get(new_ammo) else {
            error!("Weapon added that does not have an owner");
            return;
        };

        let Ok(HasHud(hud_root)) = hud_owners.get(*owner) else {
            error!("Weapon owner has no HUD");
            return;
        };

        let Ok(HudContainers { container: ammo_container, .. }) = ammo_containers.get(*hud_root)
        else {
            error!("HUD has no containers");
            return;
        };

        commands.spawn((
            Node {
                position_type: PositionType::Absolute,
                left: status_w_px(48.) * ammo.order as f32,
                top: Val::Px(0.),
                width: status_w_px(18.),
                height: status_h_px(8.),
                ..Default::default()
            },
            AtlasText {
                text: "".into(),
                image: conchars.image.clone().into(),
                layout: conchars.layout.clone(),
                glyph_size: (status_w_px(6.), status_h_px(6.)),
                line_padding: UiRect { top: Val::Px(4.), ..Default::default() },
                justify: JustifyContent::FlexEnd,
            },
            HudForItem { representing: new_ammo },
            ChildOf(*ammo_container),
        ));
    }

    pub fn mark_picking_up(
        event: On<Insert, Weapon>,
        mut commands: Commands,
        weapons: Query<&Weapon, Without<PickingUp>>,
        time: Res<Time<Virtual>>,
    ) {
        let Ok(weapon) = weapons.get(event.entity) else {
            return;
        };

        let weapon_frame = (time.elapsed_secs() - weapon.pickup_time_secs) * WEAPON_PICKUP_FPS;
        let weapon_frame = weapon_frame.clamp(0., weapon.pickup.len() as f32) as usize;
        let total_frames = weapon.pickup.len();

        if weapon_frame < total_frames {
            commands.entity(event.entity).insert(PickingUp);
        }
    }

    pub fn make_weapon_active(
        event: On<Insert, ActiveWeaponFor>,
        mut weapon_images: Query<&mut ImageNode>,
        weapons: Query<(&Weapon, &HudRepresented), Without<PickingUp>>,
    ) {
        let Ok((weapon, hud_element)) = weapons.get(event.entity) else {
            return;
        };

        let Ok(mut hud_element) = weapon_images.get_mut(hud_element.0) else {
            warn!("Tried to set weapon active but no HUD element for it was found");
            return;
        };

        hud_element.image = weapon.active.clone();
    }

    pub fn make_weapon_inactive(
        event: On<Remove, ActiveWeaponFor>,
        mut weapon_images: Query<&mut ImageNode>,
        weapons: Query<(&Item, &HudRepresented), Without<PickingUp>>,
    ) {
        let Ok((weapon, hud_element)) = weapons.get(event.entity) else {
            return;
        };

        let Ok(mut hud_element) = weapon_images.get_mut(hud_element.0) else {
            warn!("Tried to set weapon active but no HUD element for it was found");
            return;
        };

        hud_element.image = weapon.gfx.clone();
    }
}
