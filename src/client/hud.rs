use std::{io::Write as _, marker::PhantomData};

use bevy::{
    asset::AssetServer,
    ecs::{
        bundle::Bundle,
        children,
        component::Component,
        entity::Entity,
        hierarchy::ChildOf,
        lifecycle::Insert,
        name::Name,
        observer::On,
        query::Changed,
        reflect::ReflectComponent,
        system::{Command, Commands, Query, Res},
    },
    log::error,
    reflect::Reflect,
    ui::{
        Display, FlexDirection, JustifyContent, JustifySelf, Node, PositionType, UiRect, Val,
        widget::ImageNode,
    },
};
use seismon_utils::QStr;

use crate::client::{
    inventory::{Ammo, InventoryItem, Weapon},
    text::{AtlasText, Conchars},
};

#[derive(Component, Reflect, Debug)]
#[reflect(Component)]
#[relationship(relationship_target = HasHud)]
pub struct HudOf(pub Entity);

#[derive(Component, Reflect, Debug)]
#[reflect(Component)]
#[relationship_target(relationship = HudOf, linked_spawn)]
pub struct HasHud(Entity);

#[derive(Component, Reflect, Debug)]
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

pub enum HudWeapons {}

pub enum HudAmmo {}

#[derive(Component, Reflect, Debug)]
#[reflect(Component)]
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
#[reflect(Component)]
#[relationship(relationship_target = HudRepresented)]
pub struct HudForItem {
    #[relationship]
    pub representing: Entity,
}

#[derive(Component, Reflect, Debug)]
#[reflect(Component)]
#[relationship_target(relationship = HudForItem, linked_spawn)]
pub struct HudRepresented(Entity);

const STATUS_BAR_SCALE: f32 = 1.8;

/// This is the default status bar width, and we can use it as a baseline for the
/// rest of the UI.
const STATUS_BAR_WIDTH: f32 = 320.;
const STATUS_BAR_HEIGHT: f32 = 50.;

const AMMO_CONTAINER_NAME: &str = "ammo";
const WEAPONS_CONTAINER_NAME: &str = "weapons";

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
                                width: Val::Percent(100.),
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
                                width: Val::Percent(100.),
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
                )
            ],
        ));
    }
}

pub fn add_weapon_hud(
    trigger: On<Insert, Weapon>,
    mut commands: Commands,
    owners: Query<&InventoryItem>,
    hud_owners: Query<&HasHud>,
    weapon_containers: Query<&HudContainers<HudWeapons>>,
    weapons: Query<&Weapon>,
) {
    const AMMO_DISPLAY_WIDTH_PX: f32 = 24.;

    let new_weapon = trigger.entity;

    let Ok(weapon) = weapons.get(new_weapon) else {
        error!("Invalid weapon");
        return;
    };

    let Ok(InventoryItem { owner }) = owners.get(new_weapon) else {
        error!("Weapon added that does not have an owner");
        return;
    };

    let Ok(HasHud(hud_root)) = hud_owners.get(*owner) else {
        error!("Weapon owner has no HUD");
        return;
    };

    let Ok(HudContainers { container: weapon_container, .. }) = weapon_containers.get(*hud_root)
    else {
        error!("HUD has no containers");
        return;
    };

    commands.spawn((
        Node {
            position_type: PositionType::Absolute,
            left: status_w_px(AMMO_DISPLAY_WIDTH_PX) * weapon.order as f32,
            bottom: status_w_px(0.),
            width: status_w_px(AMMO_DISPLAY_WIDTH_PX),
            ..Default::default()
        },
        HudForItem { representing: new_weapon },
        ImageNode { image: weapon.inactive.clone(), ..Default::default() },
        ChildOf(*weapon_container),
    ));
}

pub fn add_ammo_hud(
    trigger: On<Insert, Ammo>,
    mut commands: Commands,
    conchars: Res<Conchars>,
    owners: Query<&InventoryItem>,
    hud_owners: Query<&HasHud>,
    ammo_containers: Query<&HudContainers<HudAmmo>>,
    ammos: Query<&Ammo>,
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

    let Ok(HudContainers { container: ammo_container, .. }) = ammo_containers.get(*hud_root) else {
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

pub fn update_ammo(
    ammos: Query<(&Ammo, &HudRepresented), Changed<Ammo>>,
    mut atlas_text: Query<&mut AtlasText>,
) {
    for (ammo, hud) in ammos {
        let mut text = [0u8; 3];

        if let Err(e) = write!(std::io::Cursor::new(&mut text[..]), "{: >3}", ammo.amount) {
            error!("Could not fully write ammo counter: {e}");
        }

        let Ok(mut hud) = atlas_text.get_mut(hud.0) else {
            error!("HUD element representing ammo does not have a text component");
            continue;
        };

        hud.text = QStr::from(&text[..]).into_owned();
    }
}
