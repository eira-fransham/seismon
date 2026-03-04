use std::{
    any::{Any, TypeId},
    marker::PhantomData,
};

use bevy::{
    app::Plugin,
    asset::{AssetServer, Handle},
    ecs::{
        bundle::Bundle,
        component::Component,
        entity::Entity,
        query::With,
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
pub struct Item {
    item_type: TypeId,
    pub order: usize,
    pub gfx: Handle<Image>,
}

impl Item {
    fn new<T: ItemType>(init: T::Init, asset_server: &AssetServer) -> (Self, T::Components) {
        (
            Self { item_type: TypeId::of::<T>(), order: T::ORDER, gfx: asset_server.load(T::GFX) },
            T::load_components(init, asset_server),
        )
    }
}

#[derive(Component, Reflect, Default)]
#[reflect(Component)]
pub struct Ammo {
    pub amount: u8,
}

#[derive(Component, Reflect)]
#[reflect(Component)]
pub struct Sigil;

pub trait ItemType: Any + Send + Sync + 'static {
    /// The extra component(s) to add to this item
    type Components: Bundle;
    /// Extra data required to initialize this item
    type Init: Send + Sync + 'static;

    /// The order in the visual HUD for this ammo type (used to calculate position)
    const ORDER: usize;

    /// The path (usually in `gfx.wad`) of this ammo type's GFX.
    const GFX: &'static str;

    /// The marker for this type
    fn load_components(init: Self::Init, asset_server: &AssetServer) -> Self::Components;
}

pub struct InitItem<T> {
    owner: Entity,
    _phantom: PhantomData<T>,
}

impl<T: ItemType> InitItem<T> {
    pub fn new(owner: Entity) -> Self {
        Self { owner, _phantom: PhantomData }
    }
}

impl<T> Command for InitItem<T>
where
    T: ItemType,
    T::Init: Default,
{
    fn apply(self, world: &mut bevy::ecs::world::World) {
        let asset_server = world.resource::<AssetServer>();

        let bundle = Item::new::<T>(Default::default(), asset_server);

        world.spawn((bundle, InventoryItem { owner: self.owner }));
    }
}

pub struct InsertItem<T>
where
    T: ItemType,
{
    owner: Entity,
    init_data: T::Init,
    _phantom: PhantomData<T>,
}

impl<T: ItemType> InsertItem<T> {
    pub fn new(owner: Entity, init_data: T::Init) -> Self {
        Self { owner, init_data, _phantom: PhantomData }
    }
}

impl<T> Command for InsertItem<T>
where
    T: ItemType,
{
    fn apply(self, world: &mut bevy::ecs::world::World) {
        let asset_server = world.resource::<AssetServer>();

        let bundle = Item::new::<T>(self.init_data, asset_server);

        world.spawn((bundle, InventoryItem { owner: self.owner }));
    }
}

pub struct UpdateAmmoCount<T> {
    owner: Entity,
    new_count: u8,
    _phantom: PhantomData<T>,
}

impl<T: ItemType> UpdateAmmoCount<T> {
    pub fn new(owner: Entity, new_count: u8) -> Self {
        Self { owner, new_count, _phantom: PhantomData }
    }
}

impl<T: ItemType> Command for UpdateAmmoCount<T> {
    fn apply(self, world: &mut bevy::ecs::world::World) {
        fn update_ammo_count<T: ItemType>(
            In(update): In<UpdateAmmoCount<T>>,
            inventories: Query<&Inventory>,
            mut ammos: Query<(&Item, &mut Ammo)>,
        ) {
            let Ok(inventory) = inventories.get(update.owner) else {
                error!(
                    "Tried to update ammo count but owner did not have any items in their inventory"
                );
                return;
            };

            for item_ent in inventory.iter() {
                let Ok((item, ammo)) = ammos.get(item_ent) else {
                    continue;
                };

                // Only call `get_mut` if the value is different, to avoid triggering the HUD change hooks
                if item.item_type == TypeId::of::<T>() && ammo.amount != update.new_count {
                    ammos.get_mut(item_ent).unwrap().1.amount = update.new_count;
                    return;
                }
            }
        }

        if let Err(e) = world.run_system_cached_with(update_ammo_count, self) {
            error!("Could not update ammo count: {e}");
        }
    }
}

#[derive(Component)]
pub struct Health(pub u16);

// TODO: This probably makes more sense as an asset or similar.
#[derive(Component, Reflect, Debug)]
#[reflect(Component)]
pub struct Weapon {
    ammo_type: TypeId,
    pub active: Handle<Image>,
    pub pickup: Vec<Handle<Image>>,
    // TODO: Maybe put this somewhere else
    pub pickup_time_secs: f32,
}

pub struct SetActiveWeaponByOrder(pub Entity, pub usize);

impl Command for SetActiveWeaponByOrder {
    fn apply(self, world: &mut bevy::ecs::world::World) {
        let mut weapons = world.query_filtered::<&Item, With<Weapon>>();
        let maybe_active_weapon = world.get::<ActiveWeapon>(self.0);

        if let Some(ActiveWeapon(active)) = maybe_active_weapon
            && let Ok(weapon) = weapons.get(world, *active)
            && weapon.order == self.1
        {
            return;
        }

        let inventory = world.get::<Inventory>(self.0).expect("No valid target for remove weapon");

        let Some(new_active_weapon) = inventory
            .0
            .iter()
            .find(|item| weapons.get(world, **item).is_ok_and(|weapon| weapon.order == self.1))
            .copied()
        else {
            error!(
                "Tried to set active weapon to weapon #{} but the player did not have it",
                self.1
            );
            return;
        };

        world.entity_mut(self.0).insert(ActiveWeapon(new_active_weapon));
    }
}

// TODO: Use `WeaponTyped<T>` + `Weapon` + a relationship and observer to ensure that it's impossible to have duplicates weapons of a given type
pub struct RemoveItem<T> {
    owner: Entity,
    _phantom: PhantomData<T>,
}

impl<T: ItemType> RemoveItem<T> {
    pub fn new(owner: Entity) -> Self {
        Self { owner, _phantom: PhantomData }
    }
}

impl<T: ItemType> Command for RemoveItem<T> {
    fn apply(self, world: &mut bevy::ecs::world::World) {
        let inventory =
            world.get::<Inventory>(self.owner).expect("No valid target for remove weapon");

        let Some(item_to_despawn) = inventory
            .0
            .iter()
            .find(|item| {
                let Some(item) = world.get::<Item>(**item) else {
                    return false;
                };

                item.item_type == TypeId::of::<T>()
            })
            .copied()
        else {
            error!("Tried to remove item {} but it did not exist", std::any::type_name::<T>());
            return;
        };

        world.despawn(item_to_despawn);
    }
}

#[derive(Component, Reflect)]
#[reflect(Component)]
#[relationship_target(relationship = ActiveWeapon)]
pub struct ActiveWeaponFor(Entity);

#[derive(Component, Reflect, Debug)]
#[reflect(Component)]
#[relationship(relationship_target = ActiveWeaponFor)]
pub struct ActiveWeapon(pub Entity);

pub trait WeaponType: ItemType {
    type Ammo: ItemType;

    const ACTIVE_FRAME: &'static str;
    const PICKUP_FRAMES: &'static [&'static str];
}

macro_rules! qammo {
    ($visibility:vis $ammo_name:ident, $gfx_base:expr, $order:expr) => {
        $visibility enum $ammo_name {}

        impl ItemType for $ammo_name {
            type Components = Ammo;
            type Init = ();

            const ORDER: usize = $order;
            const GFX: &'static str = concat!("gfx.wad#SB_", $gfx_base);

            fn load_components(_: (), _: &AssetServer) -> Self::Components {
                Ammo::default()
            }
        }
    };
}

qammo!(pub Shells, "SHELLS", 0);
qammo!(pub Nails, "NAILS", 1);
qammo!(pub Rockets, "ROCKET", 2);
qammo!(pub Cells, "CELLS", 3);

macro_rules! qsigil {
    ($visibility:vis $sigil_name:ident, $gfx_base:expr, $order:expr) => {
        $visibility enum $sigil_name {}

        impl ItemType for $sigil_name {
            type Components = Sigil;
            type Init = ();

            const ORDER: usize = $order;
            const GFX: &'static str = concat!("gfx.wad#SB_", $gfx_base);

            fn load_components(_: (), _: &AssetServer) -> Self::Components {
                Sigil
            }
        }
    };
}

qsigil!(pub Sigil1, "SIGIL1", 0);
qsigil!(pub Sigil2, "SIGIL2", 1);
qsigil!(pub Sigil3, "SIGIL3", 2);
qsigil!(pub Sigil4, "SIGIL4", 3);

macro_rules! qweapon {
    ($visibility:vis $weapon_name:ident, $ammo_ty:ty, $gfx_base:expr, $order:expr) => {
        $visibility enum $weapon_name {}

        impl ItemType for $weapon_name {
            type Components = Weapon;
            type Init = f32;

            const ORDER: usize = $order;
            const GFX: &'static str = concat!("gfx.wad#INV_", $gfx_base);

            fn load_components(pickup_time_secs: f32, asset_server: &AssetServer) -> Self::Components {
                Weapon {
                    ammo_type: TypeId::of::<<Self as WeaponType>::Ammo>(),
                    pickup_time_secs,
                    active: asset_server.load(Self::ACTIVE_FRAME),
                    pickup: Self::PICKUP_FRAMES.iter().map(|s| asset_server.load(*s)).collect(),
                }
            }
        }

        impl WeaponType for $weapon_name {
            type Ammo = $ammo_ty;
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
qweapon!(pub GrenadeLauncher, Rockets, "SRLAUNCH", 4);
qweapon!(pub RocketLauncher, Rockets, "RLAUNCH", 5);
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
