use std::{
    any::{Any, TypeId},
    marker::PhantomData,
};

use bevy::{
    app::Plugin,
    asset::{AssetServer, Handle},
    ecs::{component::Component, entity::Entity, reflect::ReflectComponent, system::Command},
    image::Image,
    log::error,
    reflect::Reflect,
};

#[derive(Component, Reflect)]
#[reflect(Component)]
#[relationship(relationship_target = Inventory)]
pub struct InventoryItem {
    pub owner: Entity,
}

#[derive(Component, Reflect)]
#[reflect(Component)]
#[relationship_target(relationship = InventoryItem, linked_spawn)]
pub struct Inventory(Vec<Entity>);

#[derive(Component, Reflect)]
#[reflect(Component)]
pub struct Ammo {
    ammo_type: TypeId,
    pub amount: u8,
    pub gfx: Handle<Image>,
}

impl Ammo {
    pub fn load<T: Any + AmmoType>(asset_server: &AssetServer) -> Self {
        Self { ammo_type: TypeId::of::<T>(), amount: 0, gfx: asset_server.load(T::GFX) }
    }
}

pub struct AddAmmo<T> {
    owner: Entity,
    _phantom: PhantomData<T>,
}

impl<T: AmmoType> AddAmmo<T> {
    pub fn new(owner: Entity) -> Self {
        Self { owner, _phantom: PhantomData }
    }
}

impl<T: AmmoType> Command for AddAmmo<T> {
    fn apply(self, world: &mut bevy::ecs::world::World) -> () {
        let asset_server = world.resource::<AssetServer>();

        let weapon = Ammo::load::<T>(asset_server);

        world.spawn((weapon, InventoryItem { owner: self.owner }));
    }
}

pub trait AmmoType: Any + Send + Sync + 'static {
    /// The path (usually in `gfx.wad`) of this ammo type's GFX.
    const GFX: &'static str;
}

#[derive(Component)]
pub struct Health(pub u16);

#[derive(Component, Reflect)]
#[reflect(Component)]
pub struct Weapon {
    weapon_type: TypeId,
    ammo_type: TypeId,
    pub ammo: Entity,
    pub inactive: Handle<Image>,
    pub active: Handle<Image>,
    pub pickup: Vec<Handle<Image>>,
}

impl Weapon {
    pub fn load<T: Any + WeaponType>(asset_server: &AssetServer) -> Self {
        Self {
            weapon_type: TypeId::of::<T>(),
            ammo_type: TypeId::of::<T::Ammo>(),
            // Will be populated by `on_add` component hook
            ammo: Entity::PLACEHOLDER,
            inactive: asset_server.load(T::INACTIVE_FRAME),
            active: asset_server.load(T::ACTIVE_FRAME),
            pickup: T::PICKUP_FRAMES.iter().map(|s| asset_server.load(*s)).collect(),
        }
    }
}

pub struct AddWeapon<T> {
    owner: Entity,
    _phantom: PhantomData<T>,
}

impl<T: WeaponType> AddWeapon<T> {
    pub fn new(owner: Entity) -> Self {
        Self { owner, _phantom: PhantomData }
    }
}

impl<T: WeaponType> Command for AddWeapon<T> {
    fn apply(self, world: &mut bevy::ecs::world::World) -> () {
        let asset_server = world.resource::<AssetServer>();

        let weapon = Weapon::load::<T>(asset_server);

        world.spawn((weapon, InventoryItem { owner: self.owner }));
    }
}

pub struct RemoveWeapon<T> {
    owner: Entity,
    _phantom: PhantomData<T>,
}

impl<T: WeaponType> RemoveWeapon<T> {
    pub fn new(owner: Entity) -> Self {
        Self { owner, _phantom: PhantomData }
    }
}

impl<T: WeaponType> Command for RemoveWeapon<T> {
    fn apply(self, world: &mut bevy::ecs::world::World) -> () {
        let inventory =
            world.get::<Inventory>(self.owner).expect("No valid target for remove weapon");

        let Some(weapon_to_despawn) = inventory
            .0
            .iter()
            .find(|item| {
                let Some(weapon) = world.get::<Weapon>(**item) else {
                    return false;
                };

                weapon.weapon_type == TypeId::of::<T>()
            })
            .copied()
        else {
            error!("Tried to remove weapon {} but it did not exist", std::any::type_name::<T>());
            return;
        };

        world.despawn(weapon_to_despawn);
    }
}

#[derive(Component, Reflect)]
#[reflect(Component)]
#[relationship_target(relationship = ActiveWeapon)]
pub struct ActiveWeaponFor(Entity);

#[derive(Component, Reflect)]
#[reflect(Component)]
#[relationship(relationship_target = ActiveWeaponFor)]
pub struct ActiveWeapon(pub Entity);

pub trait WeaponType: Any + Send + Sync + 'static {
    type Ammo: AmmoType;

    const INACTIVE_FRAME: &'static str;
    const ACTIVE_FRAME: &'static str;
    const PICKUP_FRAMES: &'static [&'static str];
}

pub enum Shells {}
pub enum Nails {}
pub enum Rockets {}
pub enum Cells {}

impl AmmoType for Shells {
    const GFX: &'static str = "gfx.wad#SHELLS";
}

impl AmmoType for Nails {
    const GFX: &'static str = "gfx.wad#NAILS";
}

impl AmmoType for Rockets {
    const GFX: &'static str = "gfx.wad#ROCKETS";
}

impl AmmoType for Cells {
    const GFX: &'static str = "gfx.wad#CELLS";
}

macro_rules! qweapon {
    ($weapon_name:ident, $ammo_ty:ty, $gfx_base:expr) => {
        pub enum $weapon_name {}

        impl WeaponType for $weapon_name {
            type Ammo = $ammo_ty;

            const INACTIVE_FRAME: &'static str = $gfx_base;
            const ACTIVE_FRAME: &'static str = concat!($gfx_base, "2");
            const PICKUP_FRAMES: &'static [&'static str] = &[
                concat!($gfx_base, "A1"),
                concat!($gfx_base, "A2"),
                concat!($gfx_base, "A3"),
                concat!($gfx_base, "A4"),
                concat!($gfx_base, "A5"),
            ];
        }
    };
}

qweapon!(Shotgun, Shells, "SHOTGUN");
qweapon!(SuperShotgun, Shells, "SSHOTGUN");
qweapon!(Nailgun, Nails, "NAILGUN");
qweapon!(SuperNailgun, Nails, "SNAILGUN");
qweapon!(RocketLauncher, Rockets, "RLAUNCH");
qweapon!(GrenadeLauncher, Rockets, "SRLAUNCH");
qweapon!(LightningGun, Cells, "LIGHTNG");

#[non_exhaustive]
#[derive(Default)]
pub struct InventoryPlugin {}

impl Plugin for InventoryPlugin {
    fn build(&self, app: &mut bevy::app::App) {
        app.register_type::<Weapon>()
            .register_type::<Ammo>()
            .register_type::<InventoryItem>()
            .register_type::<Inventory>()
            .register_type::<ActiveWeapon>()
            .register_type::<ActiveWeaponFor>();

        app.world_mut().register_component_hooks::<Weapon>().on_add(move |mut world, ctx| {
            let owner = world
                .get::<InventoryItem>(ctx.entity)
                .expect("TODO: Handle when a weapon is spawned but isn't an inventory item");
            let ammo_type_id = world.get::<Weapon>(ctx.entity).expect(
                "TODO: Weapon spawned but no weapon on entity, this is almost certainly a bug",
            );
            let other_inventory_items =
                world.get::<Inventory>(owner.owner).expect("Inventory item without inventory");

            let ammo_entity = other_inventory_items
                .0
                .iter()
                .find(|item| {
                    let Some(ammo) = world.get::<Ammo>(**item) else {
                        return false;
                    };

                    ammo.ammo_type == ammo_type_id.ammo_type
                })
                .expect("No relevant ammo type in inventory");
            world
                .get_mut::<Weapon>(ctx.entity)
                .expect(
                    "TODO: Weapon spawned but no weapon on entity, this is almost certainly a bug",
                )
                .ammo = *ammo_entity;
        });
    }
}
