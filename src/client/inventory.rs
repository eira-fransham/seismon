use std::{
    any::{Any, TypeId},
    marker::PhantomData,
};

use bevy::{
    app::Plugin,
    asset::{AssetServer, Handle},
    ecs::{
        component::Component,
        entity::Entity,
        reflect::ReflectComponent,
        relationship::RelationshipTarget as _,
        system::{Command, In, Query},
    },
    image::Image,
    log::error,
    reflect::Reflect,
};

#[derive(Component, Reflect)]
#[reflect(Component)]
#[relationship(relationship_target = Inventory)]
pub struct InventoryItem {
    #[relationship]
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
    pub order: usize,
    pub amount: u8,
    pub gfx: Handle<Image>,
}

impl Ammo {
    pub fn load<T: Any + AmmoType>(asset_server: &AssetServer) -> Self {
        Self {
            ammo_type: TypeId::of::<T>(),
            order: T::ORDER,
            amount: 0,
            gfx: asset_server.load(T::GFX),
        }
    }
}

pub trait AmmoType: Any + Send + Sync + 'static {
    /// The order in the visual HUD for this ammo type (used to calculate position)
    const ORDER: usize;

    /// The path (usually in `gfx.wad`) of this ammo type's GFX.
    const GFX: &'static str;
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
    fn apply(self, world: &mut bevy::ecs::world::World) {
        let asset_server = world.resource::<AssetServer>();

        let weapon = Ammo::load::<T>(asset_server);

        world.spawn((weapon, InventoryItem { owner: self.owner }));
    }
}

pub struct UpdateAmmoCount<T> {
    owner: Entity,
    new_count: u8,
    _phantom: PhantomData<T>,
}

impl<T: AmmoType> UpdateAmmoCount<T> {
    pub fn new(owner: Entity, new_count: u8) -> Self {
        Self { owner, new_count, _phantom: PhantomData }
    }
}

impl<T: AmmoType> Command for UpdateAmmoCount<T> {
    fn apply(self, world: &mut bevy::ecs::world::World) {
        fn update_ammo_count<T: AmmoType>(
            In(update): In<UpdateAmmoCount<T>>,
            inventories: Query<&Inventory>,
            mut ammos: Query<&mut Ammo>,
        ) {
            let Ok(inventory) = inventories.get(update.owner) else {
                error!(
                    "Tried to update ammo count but owner did not have any items in their inventory"
                );
                return;
            };

            for item in inventory.iter() {
                let Ok(ammo) = ammos.get(item) else {
                    continue;
                };

                // Only call `get_mut` if the value is different, to avoid triggering the HUD change hooks
                if ammo.ammo_type == TypeId::of::<T>() && ammo.amount != update.new_count {
                    ammos.get_mut(item).unwrap().amount = update.new_count;
                    return;
                }
            }
        }

        if let Err(e) = world.run_system_cached_with(update_ammo_count, self) {
            error!("Could not update ammo count: {e}");
        }
    }
}

pub struct RemoveAmmo<T> {
    owner: Entity,
    _phantom: PhantomData<T>,
}

impl<T: AmmoType> RemoveAmmo<T> {
    pub fn new(owner: Entity) -> Self {
        Self { owner, _phantom: PhantomData }
    }
}

impl<T: AmmoType> Command for RemoveAmmo<T> {
    fn apply(self, world: &mut bevy::ecs::world::World) {
        let inventory =
            world.get::<Inventory>(self.owner).expect("No valid target for remove weapon");

        let Some(ammo_to_despawn) = inventory
            .0
            .iter()
            .find(|item| {
                let Some(ammo) = world.get::<Ammo>(**item) else {
                    return false;
                };

                ammo.ammo_type == TypeId::of::<T>()
            })
            .copied()
        else {
            error!("Tried to remove ammo {} but it did not exist", std::any::type_name::<T>());
            return;
        };

        world.despawn(ammo_to_despawn);
    }
}

#[derive(Component)]
pub struct Health(pub u16);

#[derive(Component, Reflect)]
#[reflect(Component)]
pub struct Weapon {
    weapon_type: TypeId,
    ammo_type: TypeId,
    pub order: usize,
    pub inactive: Handle<Image>,
    pub active: Handle<Image>,
    pub pickup: Vec<Handle<Image>>,
}

impl Weapon {
    pub fn load<T: Any + WeaponType>(asset_server: &AssetServer) -> Self {
        Self {
            weapon_type: TypeId::of::<T>(),
            ammo_type: TypeId::of::<T::Ammo>(),
            order: T::ORDER,
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
    fn apply(self, world: &mut bevy::ecs::world::World) {
        let asset_server = world.resource::<AssetServer>();

        let weapon = Weapon::load::<T>(asset_server);

        world.spawn((weapon, InventoryItem { owner: self.owner }));
    }
}

// TODO: Use `WeaponTyped<T>` + `Weapon` + a relationship and observer to ensure that it's impossible to have duplicates weapons of a given type
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
    fn apply(self, world: &mut bevy::ecs::world::World) {
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

    const ORDER: usize;
    const INACTIVE_FRAME: &'static str;
    const ACTIVE_FRAME: &'static str;
    const PICKUP_FRAMES: &'static [&'static str];
}

pub enum Shells {}
pub enum Nails {}
pub enum Rockets {}
pub enum Cells {}

impl AmmoType for Shells {
    const ORDER: usize = 0;
    const GFX: &'static str = "gfx.wad#SB_SHELLS";
}

impl AmmoType for Nails {
    const ORDER: usize = 1;
    const GFX: &'static str = "gfx.wad#SB_NAILS";
}

impl AmmoType for Rockets {
    const ORDER: usize = 2;
    const GFX: &'static str = "gfx.wad#SB_ROCKET";
}

impl AmmoType for Cells {
    const ORDER: usize = 3;
    const GFX: &'static str = "gfx.wad#SB_CELLS";
}

macro_rules! qweapon {
    ($visibility:vis $weapon_name:ident, $ammo_ty:ty, $gfx_base:expr, $order:expr) => {
        $visibility enum $weapon_name {}

        impl WeaponType for $weapon_name {
            type Ammo = $ammo_ty;

            const ORDER: usize = $order;
            const INACTIVE_FRAME: &'static str = concat!("gfx.wad#INV_", $gfx_base);
            const ACTIVE_FRAME: &'static str = concat!("gfx.wad#INV2_", $gfx_base);
            const PICKUP_FRAMES: &'static [&'static str] = &[
                concat!("gfx.wad#INVA1_", $gfx_base),
                concat!("gfx.wad#INVA2_", $gfx_base),
                concat!("gfx.wad#INVA3_", $gfx_base),
                concat!("gfx.wad#INVA4_", $gfx_base),
                concat!("gfx.wad#INVA5_", $gfx_base),
            ];
        }
    };
}

qweapon!(pub Shotgun, Shells, "SHOTGUN", 0);
qweapon!(pub SuperShotgun, Shells, "SSHOTGUN", 1);
qweapon!(pub Nailgun, Nails, "NAILGUN", 2);
qweapon!(pub SuperNailgun, Nails, "SNAILGUN", 3);
qweapon!(pub RocketLauncher, Rockets, "RLAUNCH", 4);
qweapon!(pub GrenadeLauncher, Rockets, "SRLAUNCH", 5);
qweapon!(pub LightningGun, Cells, "LIGHTNG", 6);

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
    }
}
