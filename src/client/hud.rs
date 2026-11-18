//! The status bar, inventory bar, scoreboard, crosshair and intermission overlay. Mirrors vanilla
//! `sbar.c`.
//!
//! Positions are given in the same coordinate space as `sbar.c`: x from the left of the
//! 320-pixel-wide status bar and y from the top of the bar it is drawn on (`SBAR` or `IBAR`), all
//! scaled by `STATUS_BAR_SCALE`.

use std::marker::PhantomData;

use bevy::{
    app::{Plugin, PostUpdate},
    asset::{AssetServer, Handle},
    camera::visibility::Visibility,
    ecs::{
        bundle::Bundle,
        children,
        component::Component,
        entity::Entity,
        hierarchy::{ChildOf, Children},
        lifecycle::{Insert, Remove},
        name::Name,
        observer::On,
        query::{Changed, Has, Or, With, Without},
        reflect::ReflectComponent,
        resource::Resource,
        schedule::IntoScheduleConfigs as _,
        spawn::SpawnRelated as _,
        system::{Command, Commands, Query, Res},
        world::{FromWorld, World},
    },
    image::Image,
    log::{error, warn, warn_once},
    reflect::Reflect,
    time::{Time, Virtual},
    ui::{
        AlignItems, Display, FlexDirection, JustifyContent, Node, PositionType, UiRect, Val,
        widget::ImageNode,
    },
};
use bevy_mod_commands::Registry;
use seismon_utils::{QStr, QString, write_if_neq};
use serde::Deserialize;

use crate::{
    client::{
        IntermissionKind,
        inventory::{
            ActiveWeaponFor, Ammo, Armor, CurrentAmmo, FaceAnim, Health, InventoryItem, Item,
            Sigil, Weapon,
        },
        state::PlayerState,
        text::{AtlasText, Conchars},
    },
    common::net::ItemFlags,
};

#[derive(Default)]
#[non_exhaustive]
pub struct HudPlugin {}

impl Plugin for HudPlugin {
    fn build(&self, app: &mut bevy::app::App) {
        app.init_resource::<HudGfx>()
            .init_resource::<LevelStats>()
            .init_resource::<HudIntermission>()
            .add_observer(observers::add_item_hud::<Weapon, HudWeapons>(WEAPON_SLOT_WIDTH))
            .add_observer(observers::add_item_hud::<Sigil, HudSigils>(SIGIL_SLOT_WIDTH))
            .add_observer(observers::add_ammo_hud)
            .add_observer(observers::mark_picking_up)
            .add_observer(observers::make_weapon_active)
            .add_observer(observers::make_weapon_inactive)
            .add_systems(
                PostUpdate,
                (
                    (
                        systems::update_ammo,
                        systems::update_pickup_animation,
                        systems::update_health,
                        systems::update_armor,
                        systems::update_face,
                        systems::update_current_ammo,
                        systems::update_items,
                        systems::update_layout,
                        systems::update_scoreboard,
                        systems::update_overlay,
                    ),
                    systems::update_hud_numbers,
                )
                    .chain()
                    .before(crate::client::text::systems::update_atlas_text),
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

impl<Kind> HudContainer<Kind>
where
    Kind: Send + Sync + 'static,
{
    fn new(hud: Entity) -> Self {
        Self { hud, _phantom: PhantomData }
    }
}

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

/// Marker for the whole status bar (inventory bar + status bar)
pub enum HudStatusBar {}

/// Marker for the inventory bar (`IBAR`)
pub enum HudInventoryBar {}

/// Marker for the status bar background image (`SBAR`/`SCOREBAR`)
pub enum HudSbarBackground {}

/// Marker for the group of status bar elements that are hidden when the scoreboard is shown
pub enum HudStatusGroup {}

/// Marker for the solo scoreboard shown instead of the status bar when dead or with `+showscores`
pub enum HudScoreboard {}

/// Marker for the HUD health number
pub enum HudHealth {}

/// Marker for the HUD armor number
pub enum HudArmor {}

/// Marker for the HUD armor icon
pub enum HudArmorIcon {}

/// Marker for the HUD face
pub enum HudFace {}

/// Marker for the HUD current ammo number
pub enum HudCurrentAmmo {}

/// Marker for the HUD current ammo icon
pub enum HudAmmoIcon {}

/// Marker for the HUD weapon container
pub enum HudWeapons {}

/// Marker for the HUD sigil container
pub enum HudSigils {}

/// Marker for the HUD ammo container
pub enum HudAmmo {}

/// Marker for the HUD keys/powerups container
pub enum HudItems {}

/// Marker for the crosshair
pub enum HudCrosshair {}

/// Marker for the intermission/finale overlay
pub enum HudOverlay {}

/// Marker for the intermission part of the overlay
pub enum HudIntermissionGroup {}

/// Marker for the finale part of the overlay
pub enum HudFinaleGroup {}

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

/// A key or powerup slot on the inventory bar, shown when the player has the given item.
#[derive(Component, Debug, Clone, Copy)]
struct HudItemSlot(ItemFlags);

/// A line of the solo scoreboard (`Sbar_SoloScoreboard`).
#[derive(Component, Debug, Clone, Copy, PartialEq, Eq)]
enum ScoreboardLine {
    Monsters,
    Secrets,
    Time,
    LevelName,
}

/// A number on the intermission overlay (`Sbar_IntermissionOverlay`).
#[derive(Component, Debug, Clone, Copy, PartialEq, Eq)]
enum OverlayNumber {
    Minutes,
    SecondsTens,
    SecondsUnits,
    Secrets,
    TotalSecrets,
    Monsters,
    TotalMonsters,
}

/// Level statistics shown on the scoreboard and intermission overlay (`STAT_MONSTERS` etc. and
/// `cl.levelname`).
#[derive(Resource, Default, Debug, Clone, PartialEq)]
pub struct LevelStats {
    pub monsters: i32,
    pub total_monsters: i32,
    pub secrets: i32,
    pub total_secrets: i32,
    pub level_name: QString,
}

#[derive(Debug, Clone)]
pub struct IntermissionState {
    pub kind: IntermissionKind,
    /// The client time at which the intermission started (`cl.completed_time`).
    pub completed_secs: f32,
}

/// The current intermission (`cl.intermission`), if any.
#[derive(Resource, Default, Debug, Clone)]
pub struct HudIntermission(pub Option<IntermissionState>);

/// A number drawn with the large status bar digits (`Sbar_DrawNum`).
#[derive(Component, Debug, Clone, Copy, PartialEq, Eq)]
#[require(Node)]
pub struct HudNumber {
    pub value: i32,
    pub digits: usize,
    /// Use the alternate (red) digits.
    pub alt: bool,
}

impl HudNumber {
    fn new(digits: usize) -> Self {
        Self { value: 0, digits, alt: false }
    }
}

#[derive(Component, Debug)]
#[relationship(relationship_target = HudNumberDigits)]
struct HudNumberDigit(Entity);

#[derive(Component, Debug)]
#[relationship_target(relationship = HudNumberDigit, linked_spawn)]
struct HudNumberDigits(Vec<Entity>);

/// All of the images used by the HUD.
#[derive(Resource, Clone, Debug)]
pub struct HudGfx {
    /// `[alt][digit]`, with index 10 being the minus sign
    nums: [[Handle<Image>; 11]; 2],
    colon: Handle<Image>,
    slash: Handle<Image>,
    /// `[health / 20][pain]`
    faces: [[Handle<Image>; 2]; 5],
    face_invis: Handle<Image>,
    face_invuln: Handle<Image>,
    face_invis_invuln: Handle<Image>,
    face_quad: Handle<Image>,
    armor: [Handle<Image>; 3],
    ammo: [Handle<Image>; 4],
    /// Keys and powerups, in `ItemFlags` order
    items: [Handle<Image>; 6],
    disc: Handle<Image>,
    sbar: Handle<Image>,
    ibar: Handle<Image>,
    scorebar: Handle<Image>,
    complete: Handle<Image>,
    inter: Handle<Image>,
    finale: Handle<Image>,
}

impl FromWorld for HudGfx {
    fn from_world(world: &mut World) -> Self {
        let assets = world.resource::<AssetServer>();
        let wad = |name: &str| -> Handle<Image> { assets.load(format!("gfx.wad#{name}")) };

        let nums = [false, true].map(|alt| {
            let prefix = if alt { "A" } else { "" };
            std::array::from_fn(|i| {
                if i < 10 {
                    wad(&format!("{prefix}NUM_{i}"))
                } else {
                    wad(&format!("{prefix}NUM_MINUS"))
                }
            })
        });

        // `sb_faces[f][0]` is `FACE{5 - f}` and `sb_faces[f][1]` is `FACE_P{5 - f}`.
        let faces = std::array::from_fn(|f| {
            [wad(&format!("FACE{}", 5 - f)), wad(&format!("FACE_P{}", 5 - f))]
        });

        Self {
            nums,
            colon: wad("NUM_COLON"),
            slash: wad("NUM_SLASH"),
            faces,
            face_invis: wad("FACE_INVIS"),
            face_invuln: wad("FACE_INVUL2"),
            face_invis_invuln: wad("FACE_INV2"),
            face_quad: wad("FACE_QUAD"),
            armor: ["SB_ARMOR1", "SB_ARMOR2", "SB_ARMOR3"].map(wad),
            ammo: ["SB_SHELLS", "SB_NAILS", "SB_ROCKET", "SB_CELLS"].map(wad),
            items: ["SB_KEY1", "SB_KEY2", "SB_INVIS", "SB_INVULN", "SB_SUIT", "SB_QUAD"].map(wad),
            disc: wad("DISC"),
            sbar: wad("SBAR"),
            ibar: wad("IBAR"),
            scorebar: wad("SCOREBAR"),
            complete: assets.load("gfx/complete.lmp"),
            inter: assets.load("gfx/inter.lmp"),
            finale: assets.load("gfx/finale.lmp"),
        }
    }
}

impl HudGfx {
    fn num(&self, glyph: NumGlyph, alt: bool) -> Handle<Image> {
        let idx = match glyph {
            NumGlyph::Digit(d) => d as usize,
            NumGlyph::Minus => 10,
        };

        self.nums[alt as usize][idx].clone()
    }

    fn face(&self, face: FaceId) -> Handle<Image> {
        match face {
            FaceId::Normal { health_level, pain } => {
                self.faces[health_level][pain as usize].clone()
            }
            FaceId::InvisInvuln => self.face_invis_invuln.clone(),
            FaceId::Quad => self.face_quad.clone(),
            FaceId::Invis => self.face_invis.clone(),
            FaceId::Invuln => self.face_invuln.clone(),
        }
    }
}

const STATUS_BAR_SCALE: f32 = 1.8;

/// This is the default status bar width, and we can use it as a baseline for the
/// rest of the UI.
const STATUS_BAR_WIDTH: f32 = 320.;
/// The height of each of `SBAR` and `IBAR`.
const BAR_HEIGHT: f32 = 24.;

/// The width and height of the large status bar digits.
const NUM_SIZE: f32 = 24.;
/// The width and height of a conchars glyph.
const GLYPH_SIZE: f32 = 8.;

const WEAPON_SLOT_WIDTH: f32 = 24.;
const SIGIL_SLOT_WIDTH: f32 = 8.;
/// The height of weapon, key, powerup and sigil icons on the inventory bar.
const INVENTORY_ICON_HEIGHT: f32 = 16.;

/// The size of the intermission overlay.
const OVERLAY_WIDTH: f32 = 320.;
const OVERLAY_HEIGHT: f32 = 200.;

/// Weapon pickup animations run at 10 frames per second.
const WEAPON_PICKUP_FPS: f32 = 10.;
/// The number of frames that the pickup animation lasts for (`flashon >= 10` in `sbar.c`).
const WEAPON_PICKUP_TOTAL_FRAMES: usize = 10;

/// Keys and powerups, in the order that they are drawn on the inventory bar.
const INVENTORY_ITEMS: [ItemFlags; 6] = [
    ItemFlags::KEY_1,
    ItemFlags::KEY_2,
    ItemFlags::INVISIBILITY,
    ItemFlags::INVULNERABILITY,
    ItemFlags::SUIT,
    ItemFlags::QUAD,
];

/// Armor types, from weakest to strongest.
const ARMOR_ITEMS: [ItemFlags; 3] = [ItemFlags::ARMOR_1, ItemFlags::ARMOR_2, ItemFlags::ARMOR_3];

/// Ammo types, in the order that their icons are checked.
const AMMO_ITEMS: [ItemFlags; 4] =
    [ItemFlags::SHELLS, ItemFlags::NAILS, ItemFlags::ROCKETS, ItemFlags::CELLS];

/// Conchars index of the small gold `0` digit used for the ammo counters.
const SMALL_DIGIT_BASE: u8 = 18;

/// Scale a size in status bar pixels to UI pixels.
fn px(value: f32) -> Val {
    Val::Px(value * STATUS_BAR_SCALE)
}

/// An absolutely-positioned node at the given position, in status bar pixels.
fn abs_node(left: f32, top: f32) -> Node {
    Node {
        position_type: PositionType::Absolute,
        left: px(left),
        top: px(top),
        ..Default::default()
    }
}

/// An absolutely-positioned node with the given position and size, in status bar pixels.
fn abs_rect(left: f32, top: f32, width: f32, height: f32) -> Node {
    Node { width: px(width), height: px(height), ..abs_node(left, top) }
}

/// A node filling its parent.
fn fill_node(display: Display) -> Node {
    Node {
        position_type: PositionType::Absolute,
        width: Val::Percent(100.),
        height: Val::Percent(100.),
        display,
        ..Default::default()
    }
}

/// Text drawn with conchars at the status bar scale.
fn hud_text(conchars: &Conchars) -> AtlasText {
    AtlasText {
        text: "".into(),
        image: conchars.image.clone().into(),
        layout: conchars.layout.clone(),
        glyph_size: (px(GLYPH_SIZE), px(GLYPH_SIZE)),
        line_padding: UiRect::DEFAULT,
        justify: JustifyContent::FlexStart,
    }
}

/// A large number at the given position, in status bar pixels.
fn hud_number(left: f32, top: f32, digits: usize) -> (HudNumber, Node) {
    (HudNumber::new(digits), abs_rect(left, top, NUM_SIZE * digits as f32, NUM_SIZE))
}

fn hud_image(image: Handle<Image>) -> ImageNode {
    ImageNode { image, ..Default::default() }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NumGlyph {
    Digit(u8),
    Minus,
}

/// Port of the layout logic of `Sbar_DrawNum`: returns the number of empty digit slots to the
/// left of the number, and the glyphs to draw. If the number has more than `digits` characters,
/// only the last `digits` are drawn.
fn number_glyphs(value: i32, digits: usize) -> (usize, Vec<NumGlyph>) {
    let text = value.to_string();
    let bytes = text.as_bytes();
    let skip = bytes.len().saturating_sub(digits);
    let pad = digits.saturating_sub(bytes.len());

    let glyphs = bytes[skip..]
        .iter()
        .map(|b| match b {
            b'-' => NumGlyph::Minus,
            b => NumGlyph::Digit(b - b'0'),
        })
        .collect();

    (pad, glyphs)
}

/// The text of an ammo counter on the inventory bar (`"%3i"`, using the small gold digits).
fn ammo_counter_text(amount: u8) -> [u8; 3] {
    let mut out = [b' '; 3];
    let text = format!("{amount: >3}");

    for (out, b) in out.iter_mut().zip(text.bytes()) {
        if b.is_ascii_digit() {
            *out = SMALL_DIGIT_BASE + (b - b'0');
        }
    }

    out
}

/// Returns `Some(frame)` with the index into the weapon's pickup frames if the pickup animation
/// is still playing `elapsed_secs` after the weapon was picked up, or `None` if it has finished.
fn weapon_pickup_frame(elapsed_secs: f32, num_frames: usize) -> Option<usize> {
    let frame = (elapsed_secs.max(0.) * WEAPON_PICKUP_FPS) as usize;

    (frame < WEAPON_PICKUP_TOTAL_FRAMES && num_frames > 0).then(|| frame % num_frames)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FaceId {
    Normal {
        /// `health / 20`, clamped to `0..=4`
        health_level: usize,
        pain: bool,
    },
    InvisInvuln,
    Quad,
    Invis,
    Invuln,
}

/// Port of the face selection in `Sbar_DrawFace`.
fn face_for(items: ItemFlags, health: i16, pain: bool) -> FaceId {
    if items.contains(ItemFlags::INVISIBILITY | ItemFlags::INVULNERABILITY) {
        FaceId::InvisInvuln
    } else if items.contains(ItemFlags::QUAD) {
        FaceId::Quad
    } else if items.contains(ItemFlags::INVISIBILITY) {
        FaceId::Invis
    } else if items.contains(ItemFlags::INVULNERABILITY) {
        FaceId::Invuln
    } else {
        let health_level = if health >= 100 { 4 } else { (health.max(0) / 20) as usize };
        FaceId::Normal { health_level, pain }
    }
}

/// The number of lines of the status bar to draw for a given `viewsize` (`sb_lines`).
fn status_bar_lines(viewsize: f32) -> u32 {
    if viewsize >= 120. {
        0
    } else if viewsize >= 110. {
        24
    } else {
        48
    }
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
    type Out = ();

    fn apply(self, world: &mut World) {
        let gfx = world.resource::<HudGfx>().clone();
        let conchars = world.resource::<Conchars>().clone();

        let root = world
            .spawn((
                HudOf(self.owner),
                Name::new("hud"),
                Node {
                    position_type: PositionType::Absolute,
                    width: Val::Percent(100.),
                    height: Val::Percent(100.),
                    flex_direction: FlexDirection::Column,
                    justify_content: JustifyContent::FlexEnd,
                    align_items: AlignItems::Center,
                    ..Default::default()
                },
                self.components,
            ))
            .id();

        let items = INVENTORY_ITEMS
            .iter()
            .zip(&gfx.items)
            .enumerate()
            .map(|(i, (flag, image))| {
                (
                    HudItemSlot(*flag),
                    Node {
                        display: Display::None,
                        ..abs_rect(16. * i as f32, 0., 16., INVENTORY_ICON_HEIGHT)
                    },
                    hud_image(image.clone()),
                )
            })
            .collect::<Vec<_>>();

        // Status bar
        world.spawn((
            Name::new("status bar"),
            HudContainer::<HudStatusBar>::new(root),
            Node {
                width: px(STATUS_BAR_WIDTH),
                flex_direction: FlexDirection::Column,
                ..Default::default()
            },
            ChildOf(root),
            children![
                (
                    Name::new("inventory bar"),
                    HudContainer::<HudInventoryBar>::new(root),
                    Node {
                        width: Val::Percent(100.),
                        height: px(BAR_HEIGHT),
                        ..Default::default()
                    },
                    hud_image(gfx.ibar.clone()),
                    children![
                        (
                            Name::new("weapons"),
                            HudContainer::<HudWeapons>::new(root),
                            abs_rect(0., 0., STATUS_BAR_WIDTH, BAR_HEIGHT),
                        ),
                        (
                            Name::new("ammo"),
                            HudContainer::<HudAmmo>::new(root),
                            // `Sbar_DrawCharacter` adds 4 to x, so `(6*i+1)*8 - 2` becomes
                            // `48*i + 10`.
                            abs_node(10., 0.),
                        ),
                        (
                            Name::new("items"),
                            HudContainer::<HudItems>::new(root),
                            abs_node(192., BAR_HEIGHT - INVENTORY_ICON_HEIGHT),
                            Children::spawn(bevy::ecs::spawn::SpawnIter(items.into_iter())),
                        ),
                        (
                            Name::new("sigils"),
                            HudContainer::<HudSigils>::new(root),
                            abs_rect(288., 0., 32., BAR_HEIGHT),
                        ),
                    ]
                ),
                (
                    Name::new("sbar"),
                    HudContainer::<HudSbarBackground>::new(root),
                    Node {
                        width: Val::Percent(100.),
                        height: px(BAR_HEIGHT),
                        ..Default::default()
                    },
                    hud_image(gfx.sbar.clone()),
                    children![
                        (
                            Name::new("status"),
                            HudContainer::<HudStatusGroup>::new(root),
                            fill_node(Display::Flex),
                            children![
                                (
                                    Name::new("armor icon"),
                                    HudContainer::<HudArmorIcon>::new(root),
                                    Node {
                                        display: Display::None,
                                        ..abs_rect(0., 0., NUM_SIZE, NUM_SIZE)
                                    },
                                    hud_image(gfx.armor[0].clone()),
                                ),
                                (
                                    Name::new("armor"),
                                    HudContainer::<HudArmor>::new(root),
                                    hud_number(24., 0., 3),
                                ),
                                (
                                    Name::new("face"),
                                    HudContainer::<HudFace>::new(root),
                                    abs_rect(112., 0., NUM_SIZE, NUM_SIZE),
                                    hud_image(gfx.faces[4][0].clone()),
                                ),
                                (
                                    Name::new("health"),
                                    HudContainer::<HudHealth>::new(root),
                                    hud_number(136., 0., 3),
                                ),
                                (
                                    Name::new("ammo icon"),
                                    HudContainer::<HudAmmoIcon>::new(root),
                                    Node {
                                        display: Display::None,
                                        ..abs_rect(224., 0., NUM_SIZE, NUM_SIZE)
                                    },
                                    hud_image(gfx.ammo[0].clone()),
                                ),
                                (
                                    Name::new("current ammo"),
                                    HudContainer::<HudCurrentAmmo>::new(root),
                                    hud_number(248., 0., 3),
                                ),
                            ]
                        ),
                        (
                            Name::new("scoreboard"),
                            HudContainer::<HudScoreboard>::new(root),
                            fill_node(Display::None),
                            children![
                                (ScoreboardLine::Monsters, abs_node(8., 4.), hud_text(&conchars)),
                                (ScoreboardLine::Secrets, abs_node(8., 12.), hud_text(&conchars)),
                                (ScoreboardLine::Time, abs_node(184., 4.), hud_text(&conchars)),
                                (
                                    ScoreboardLine::LevelName,
                                    abs_node(232., 12.),
                                    hud_text(&conchars)
                                ),
                            ]
                        ),
                    ]
                )
            ],
        ));

        // Crosshair
        world.spawn((
            Name::new("crosshair"),
            HudContainer::<HudCrosshair>::new(root),
            Node {
                position_type: PositionType::Absolute,
                left: Val::Percent(50.),
                top: Val::Percent(50.),
                width: px(GLYPH_SIZE),
                height: px(GLYPH_SIZE),
                display: Display::None,
                ..Default::default()
            },
            AtlasText { text: "+".into(), ..hud_text(&conchars) },
            ChildOf(root),
        ));

        // Intermission/finale overlay
        world.spawn((
            Name::new("overlay"),
            HudContainer::<HudOverlay>::new(root),
            Node {
                position_type: PositionType::Absolute,
                left: Val::Percent(50.),
                top: Val::Percent(50.),
                margin: UiRect {
                    left: px(-OVERLAY_WIDTH / 2.),
                    top: px(-OVERLAY_HEIGHT / 2.),
                    ..Default::default()
                },
                width: px(OVERLAY_WIDTH),
                height: px(OVERLAY_HEIGHT),
                display: Display::None,
                ..Default::default()
            },
            ChildOf(root),
            children![
                (
                    Name::new("intermission"),
                    HudContainer::<HudIntermissionGroup>::new(root),
                    fill_node(Display::None),
                    children![
                        (abs_rect(64., 24., 192., 24.), hud_image(gfx.complete.clone())),
                        (abs_rect(0., 56., 160., 144.), hud_image(gfx.inter.clone())),
                        (OverlayNumber::Minutes, hud_number(160., 64., 3)),
                        (abs_rect(234., 64., 16., NUM_SIZE), hud_image(gfx.colon.clone())),
                        (OverlayNumber::SecondsTens, hud_number(246., 64., 1)),
                        (OverlayNumber::SecondsUnits, hud_number(266., 64., 1)),
                        (OverlayNumber::Secrets, hud_number(160., 104., 3)),
                        (abs_rect(232., 104., 16., NUM_SIZE), hud_image(gfx.slash.clone())),
                        (OverlayNumber::TotalSecrets, hud_number(240., 104., 3)),
                        (OverlayNumber::Monsters, hud_number(160., 144., 3)),
                        (abs_rect(232., 144., 16., NUM_SIZE), hud_image(gfx.slash.clone())),
                        (OverlayNumber::TotalMonsters, hud_number(240., 144., 3)),
                    ]
                ),
                (
                    Name::new("finale"),
                    HudContainer::<HudFinaleGroup>::new(root),
                    fill_node(Display::None),
                    children![(abs_rect(16., 16., 288., 24.), hud_image(gfx.finale.clone()))]
                ),
            ],
        ));
    }
}

#[derive(Component)]
struct PickingUp;

/// The cvars that control the HUD.
#[derive(Deserialize, Clone, Copy, Debug)]
struct HudVars {
    viewsize: f32,
    cl_drawhud: f32,
    crosshair: f32,
    cl_crossx: f32,
    cl_crossy: f32,
}

mod systems {
    use super::*;

    /// Returns the element of kind `Kind` for the given HUD root.
    fn element<Kind: Send + Sync + 'static>(
        containers: &Query<&HudContainers<Kind>>,
        hud: Entity,
    ) -> Option<Entity> {
        containers.get(hud).ok().map(|c| c.container)
    }

    /// Set the HUD number of kind `Kind` for the given HUD root.
    fn set_number<Kind: Send + Sync + 'static>(
        containers: &Query<&HudContainers<Kind>>,
        numbers: &mut Query<&mut HudNumber>,
        hud: Entity,
        value: i32,
        alt: bool,
    ) {
        let Some(mut number) = element(containers, hud).and_then(|e| numbers.get_mut(e).ok())
        else {
            return;
        };

        let new = HudNumber { value, alt, ..*number };
        write_if_neq!(*number, new);
    }

    /// Set the image of the element of kind `Kind`, hiding it if `image` is `None`.
    fn set_icon<Kind: Send + Sync + 'static>(
        containers: &Query<&HudContainers<Kind>>,
        icons: &mut Query<(&mut ImageNode, &mut Node)>,
        hud: Entity,
        image: Option<Handle<Image>>,
    ) {
        let Some((mut image_node, mut node)) =
            element(containers, hud).and_then(|e| icons.get_mut(e).ok())
        else {
            return;
        };

        let display = if image.is_some() { Display::Flex } else { Display::None };
        write_if_neq!(node.display, display);
        if let Some(image) = image {
            write_if_neq!(image_node.image, image);
        }
    }

    fn set_display<Kind: Send + Sync + 'static>(
        containers: &Query<&HudContainers<Kind>>,
        nodes: &mut Query<&mut Node, Without<HudNumber>>,
        hud: Entity,
        visible: bool,
    ) {
        let Some(mut node) = element(containers, hud).and_then(|e| nodes.get_mut(e).ok()) else {
            return;
        };

        let display = if visible { Display::Flex } else { Display::None };
        write_if_neq!(node.display, display);
    }

    /// Port of the drawing logic of `Sbar_DrawNum`.
    pub fn update_hud_numbers(
        mut commands: Commands,
        gfx: Res<HudGfx>,
        numbers: Query<(Entity, &HudNumber), Changed<HudNumber>>,
    ) {
        for (entity, number) in numbers {
            commands.entity(entity).despawn_related::<HudNumberDigits>();

            let (pad, glyphs) = number_glyphs(number.value, number.digits);

            for (i, glyph) in glyphs.into_iter().enumerate() {
                commands.spawn((
                    abs_rect(NUM_SIZE * (pad + i) as f32, 0., NUM_SIZE, NUM_SIZE),
                    hud_image(gfx.num(glyph, number.alt)),
                    HudNumberDigit(entity),
                    ChildOf(entity),
                ));
            }
        }
    }

    type AmmoChanged = Or<(Changed<Ammo>, Changed<HudRepresented>)>;

    pub fn update_ammo(
        ammos: Query<(&Ammo, &HudRepresented), AmmoChanged>,
        mut atlas_text: Query<&mut AtlasText>,
    ) {
        for (ammo, hud) in ammos {
            let Ok(mut hud) = atlas_text.get_mut(hud.0) else {
                warn!("HUD element representing ammo does not have a text component");
                continue;
            };

            write_if_neq!(hud.text, QStr::from(&ammo_counter_text(ammo.amount)[..]).into_owned());
        }
    }

    type PickingUpWeapon =
        (Entity, &'static Weapon, &'static Item, &'static HudRepresented, Has<ActiveWeaponFor>);

    pub fn update_pickup_animation(
        mut commands: Commands,
        mut weapon_images: Query<&mut ImageNode>,
        weapons: Query<PickingUpWeapon, With<PickingUp>>,
        time: Res<Time<Virtual>>,
    ) {
        for (entity, weapon, item, hud_element, is_active) in weapons {
            let Ok(mut hud_element) = weapon_images.get_mut(hud_element.0) else {
                warn!("Tried to animate weapon pickup but no HUD element for it was found");
                continue;
            };

            let frame = weapon_pickup_frame(
                time.elapsed_secs() - weapon.pickup_time_secs,
                weapon.pickup.len(),
            );

            let new_image = match frame {
                Some(frame) => &weapon.pickup[frame],
                None => {
                    commands.entity(entity).remove::<PickingUp>();

                    if is_active { &weapon.active } else { &item.gfx }
                }
            };

            write_if_neq!(hud_element.image, new_image.clone());
        }
    }

    type HudOwnerChanged<C> = Or<(Changed<C>, Changed<HasHud>)>;

    pub fn update_health(
        hud_owners: Query<(&HasHud, &Health), HudOwnerChanged<Health>>,
        containers: Query<&HudContainers<HudHealth>>,
        mut numbers: Query<&mut HudNumber>,
    ) {
        for (hud, Health(health)) in hud_owners {
            set_number(&containers, &mut numbers, hud.0, (*health).into(), *health <= 25);
        }
    }

    type ArmorOwner = (&'static HasHud, &'static Armor, &'static PlayerState);
    type ArmorChanged = Or<(Changed<Armor>, Changed<PlayerState>, Changed<HasHud>)>;

    pub fn update_armor(
        hud_owners: Query<ArmorOwner, ArmorChanged>,
        number_containers: Query<&HudContainers<HudArmor>>,
        icon_containers: Query<&HudContainers<HudArmorIcon>>,
        mut numbers: Query<&mut HudNumber>,
        mut icons: Query<(&mut ImageNode, &mut Node)>,
        gfx: Res<HudGfx>,
    ) {
        for (hud, Armor(armor), player) in hud_owners {
            let (value, alt, icon) = if player.items.contains(ItemFlags::INVULNERABILITY) {
                (666, true, Some(gfx.disc.clone()))
            } else {
                let icon = ARMOR_ITEMS
                    .iter()
                    .zip(&gfx.armor)
                    .rev()
                    .find(|(flag, _)| player.items.contains(**flag))
                    .map(|(_, image)| image.clone());

                ((*armor).into(), *armor <= 25, icon)
            };

            set_number(&number_containers, &mut numbers, hud.0, value, alt);
            set_icon(&icon_containers, &mut icons, hud.0, icon);
        }
    }

    type FaceOwner =
        (&'static HasHud, &'static Health, &'static PlayerState, Option<&'static FaceAnim>);

    pub fn update_face(
        hud_owners: Query<FaceOwner>,
        containers: Query<&HudContainers<HudFace>>,
        mut images: Query<&mut ImageNode>,
        gfx: Res<HudGfx>,
        time: Res<Time<Virtual>>,
    ) {
        for (hud, Health(health), player, face_anim) in hud_owners {
            let pain = face_anim.is_some_and(|anim| time.elapsed_secs() <= anim.until_secs);
            let face = gfx.face(face_for(player.items, *health, pain));

            if let Some(mut image) =
                element(&containers, hud.0).and_then(|e| images.get_mut(e).ok())
            {
                write_if_neq!(image.image, face);
            }
        }
    }

    type CurrentAmmoOwner = (&'static HasHud, &'static CurrentAmmo, &'static PlayerState);
    type CurrentAmmoChanged = Or<(Changed<CurrentAmmo>, Changed<PlayerState>, Changed<HasHud>)>;

    pub fn update_current_ammo(
        hud_owners: Query<CurrentAmmoOwner, CurrentAmmoChanged>,
        number_containers: Query<&HudContainers<HudCurrentAmmo>>,
        icon_containers: Query<&HudContainers<HudAmmoIcon>>,
        mut numbers: Query<&mut HudNumber>,
        mut icons: Query<(&mut ImageNode, &mut Node)>,
        gfx: Res<HudGfx>,
    ) {
        for (hud, CurrentAmmo(ammo), player) in hud_owners {
            let icon = AMMO_ITEMS
                .iter()
                .zip(&gfx.ammo)
                .find(|(flag, _)| player.items.contains(**flag))
                .map(|(_, image)| image.clone());

            set_number(&number_containers, &mut numbers, hud.0, (*ammo).into(), *ammo <= 10);
            set_icon(&icon_containers, &mut icons, hud.0, icon);
        }
    }

    pub fn update_items(
        hud_owners: Query<(&HasHud, &PlayerState), HudOwnerChanged<PlayerState>>,
        containers: Query<&HudContainers<HudItems>>,
        children: Query<&Children>,
        mut slots: Query<(&HudItemSlot, &mut Node)>,
    ) {
        for (hud, player) in hud_owners {
            let Some(slot_entities) =
                element(&containers, hud.0).and_then(|e| children.get(e).ok())
            else {
                continue;
            };

            for slot in slot_entities {
                let Ok((HudItemSlot(flag), mut node)) = slots.get_mut(*slot) else {
                    continue;
                };

                let display =
                    if player.items.contains(*flag) { Display::Flex } else { Display::None };
                write_if_neq!(node.display, display);
            }
        }
    }

    #[expect(clippy::too_many_arguments)]
    pub fn update_layout(
        hud_owners: Query<(&HasHud, &Health)>,
        registry: Res<Registry>,
        intermission: Res<HudIntermission>,
        gfx: Res<HudGfx>,
        status_bars: Query<&HudContainers<HudStatusBar>>,
        inventory_bars: Query<&HudContainers<HudInventoryBar>>,
        sbar_backgrounds: Query<&HudContainers<HudSbarBackground>>,
        status_groups: Query<&HudContainers<HudStatusGroup>>,
        scoreboards: Query<&HudContainers<HudScoreboard>>,
        crosshairs: Query<&HudContainers<HudCrosshair>>,
        mut nodes: Query<&mut Node, Without<HudNumber>>,
        mut images: Query<&mut ImageNode>,
        mut visibilities: Query<&mut Visibility>,
    ) {
        let vars: HudVars = match registry.read_cvars() {
            Ok(vars) => vars,
            Err(e) => {
                warn_once!("Could not read HUD cvars: {e}");
                return;
            }
        };

        let in_intermission = intermission.0.is_some();
        let show_scores = registry.is_pressed("showscores");
        let sb_lines = status_bar_lines(vars.viewsize);

        for (hud, Health(health)) in hud_owners {
            let hud = hud.0;

            if let Ok(mut visibility) = visibilities.get_mut(hud) {
                let new_visibility =
                    if vars.cl_drawhud != 0. { Visibility::Inherited } else { Visibility::Hidden };
                write_if_neq!(*visibility, new_visibility);
            }

            // `Sbar_Draw`: the scoreboard is drawn instead of the status bar when the player is
            // dead or `+showscores` is held, even if the status bar is otherwise hidden.
            let scoreboard = show_scores || *health <= 0;

            set_display(&status_bars, &mut nodes, hud, !in_intermission);
            set_display(&inventory_bars, &mut nodes, hud, sb_lines > 24);
            set_display(&sbar_backgrounds, &mut nodes, hud, scoreboard || sb_lines > 0);
            set_display(&status_groups, &mut nodes, hud, !scoreboard);
            set_display(&scoreboards, &mut nodes, hud, scoreboard);

            if let Some(mut background) =
                element(&sbar_backgrounds, hud).and_then(|e| images.get_mut(e).ok())
            {
                let image = if scoreboard { gfx.scorebar.clone() } else { gfx.sbar.clone() };
                write_if_neq!(background.image, image);
            }

            if let Some(mut crosshair) =
                element(&crosshairs, hud).and_then(|e| nodes.get_mut(e).ok())
            {
                let display = if vars.crosshair != 0. && !in_intermission {
                    Display::Flex
                } else {
                    Display::None
                };
                let margin = UiRect {
                    left: px(vars.cl_crossx),
                    top: px(vars.cl_crossy),
                    ..Default::default()
                };

                write_if_neq!(crosshair.display, display);
                write_if_neq!(crosshair.margin, margin);
            }
        }
    }

    /// Port of `Sbar_SoloScoreboard`.
    pub fn update_scoreboard(
        hud_owners: Query<&HasHud>,
        containers: Query<&HudContainers<HudScoreboard>>,
        children: Query<&Children>,
        mut lines: Query<(&ScoreboardLine, &mut AtlasText, &mut Node)>,
        stats: Res<LevelStats>,
        time: Res<Time<Virtual>>,
    ) {
        let total_secs = time.elapsed_secs() as i32;
        let minutes = total_secs / 60;
        let seconds = total_secs - 60 * minutes;

        for hud in hud_owners {
            let Some(line_entities) =
                element(&containers, hud.0).and_then(|e| children.get(e).ok())
            else {
                continue;
            };

            for line in line_entities {
                let Ok((kind, mut text, mut node)) = lines.get_mut(*line) else {
                    continue;
                };

                let new_text: QString = match kind {
                    ScoreboardLine::Monsters => {
                        format!("Monsters:{:3} /{:3}", stats.monsters, stats.total_monsters).into()
                    }
                    ScoreboardLine::Secrets => {
                        format!("Secrets :{:3} /{:3}", stats.secrets, stats.total_secrets).into()
                    }
                    ScoreboardLine::Time => {
                        format!("Time :{:3}:{}{}", minutes, seconds / 10, seconds % 10).into()
                    }
                    ScoreboardLine::LevelName => stats.level_name.clone(),
                };

                let len = new_text.raw.len() as f32;

                if *kind == ScoreboardLine::LevelName {
                    write_if_neq!(node.left, px(232. - len * 4.));
                }
                write_if_neq!(node.width, px(len * GLYPH_SIZE));
                write_if_neq!(text.text, new_text);
            }
        }
    }

    /// Port of `Sbar_IntermissionOverlay` and `Sbar_FinaleOverlay`.
    #[expect(clippy::too_many_arguments)]
    pub fn update_overlay(
        hud_owners: Query<&HasHud>,
        overlays: Query<&HudContainers<HudOverlay>>,
        intermission_groups: Query<&HudContainers<HudIntermissionGroup>>,
        finale_groups: Query<&HudContainers<HudFinaleGroup>>,
        mut nodes: Query<&mut Node, Without<HudNumber>>,
        mut numbers: Query<(&OverlayNumber, &mut HudNumber)>,
        intermission: Res<HudIntermission>,
        stats: Res<LevelStats>,
    ) {
        let (show_intermission, show_finale) = match intermission.0.as_ref().map(|i| &i.kind) {
            None | Some(IntermissionKind::Cutscene { .. }) => (false, false),
            Some(IntermissionKind::Intermission) => (true, false),
            Some(IntermissionKind::Finale { .. }) => (false, true),
        };

        for hud in hud_owners {
            set_display(&overlays, &mut nodes, hud.0, show_intermission || show_finale);
            set_display(&intermission_groups, &mut nodes, hud.0, show_intermission);
            set_display(&finale_groups, &mut nodes, hud.0, show_finale);
        }

        let Some(state) = intermission.0.as_ref().filter(|_| show_intermission) else {
            return;
        };

        let completed = state.completed_secs as i32;
        let minutes = completed / 60;
        let seconds = completed - minutes * 60;

        for (kind, mut number) in &mut numbers {
            let value = match kind {
                OverlayNumber::Minutes => minutes,
                OverlayNumber::SecondsTens => seconds / 10,
                OverlayNumber::SecondsUnits => seconds % 10,
                OverlayNumber::Secrets => stats.secrets,
                OverlayNumber::TotalSecrets => stats.total_secrets,
                OverlayNumber::Monsters => stats.monsters,
                OverlayNumber::TotalMonsters => stats.total_monsters,
            };

            write_if_neq!(number.value, value);
        }
    }
}

mod observers {
    use bevy::ecs::system::{IntoObserverSystem, ObserverSystem};

    use super::*;

    pub fn add_item_hud<Kind, Container>(width: f32) -> impl ObserverSystem<Insert<Kind>, ()>
    where
        Kind: Component,
        Container: Send + Sync + 'static,
    {
        let observer = {
            move |trigger: On<Insert<Kind>>,
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

                // Only the height is set so that the width follows the image's aspect ratio (the
                // lightning gun icons are twice as wide as the others).
                commands.spawn((
                    Node {
                        position_type: PositionType::Absolute,
                        left: px(width * item.order as f32),
                        bottom: Val::Px(0.),
                        height: px(INVENTORY_ICON_HEIGHT),
                        ..Default::default()
                    },
                    HudForItem { representing: new_item },
                    hud_image(item.gfx.clone()),
                    ChildOf(*weapon_container),
                ));
            }
        };

        IntoObserverSystem::into_system(observer)
    }

    pub fn add_ammo_hud(
        trigger: On<Insert<Ammo>>,
        mut commands: Commands,
        conchars: Res<Conchars>,
        owners: Query<&InventoryItem>,
        hud_owners: Query<&HasHud>,
        ammo_containers: Query<&HudContainers<HudAmmo>>,
        ammos: Query<&Item>,
    ) {
        let new_ammo = trigger.entity;

        let Ok(ammo) = ammos.get(new_ammo) else {
            error!("Invalid ammo");
            return;
        };

        let Ok(InventoryItem { owner }) = owners.get(new_ammo) else {
            error!("Ammo added that does not have an owner");
            return;
        };

        let Ok(HasHud(hud_root)) = hud_owners.get(*owner) else {
            error!("Ammo owner has no HUD");
            return;
        };

        let Ok(HudContainers { container: ammo_container, .. }) = ammo_containers.get(*hud_root)
        else {
            error!("HUD has no containers");
            return;
        };

        commands.spawn((
            abs_rect(48. * ammo.order as f32, 0., 3. * GLYPH_SIZE, GLYPH_SIZE),
            hud_text(&conchars),
            HudForItem { representing: new_ammo },
            ChildOf(*ammo_container),
        ));
    }

    pub fn mark_picking_up(
        event: On<Insert<Weapon>>,
        mut commands: Commands,
        weapons: Query<&Weapon, Without<PickingUp>>,
        time: Res<Time<Virtual>>,
    ) {
        let Ok(weapon) = weapons.get(event.entity) else {
            return;
        };

        if weapon_pickup_frame(time.elapsed_secs() - weapon.pickup_time_secs, weapon.pickup.len())
            .is_some()
        {
            commands.entity(event.entity).insert(PickingUp);
        }
    }

    pub fn make_weapon_active(
        event: On<Insert<ActiveWeaponFor>>,
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
        event: On<Remove<ActiveWeaponFor>>,
        mut weapon_images: Query<&mut ImageNode>,
        weapons: Query<(&Item, &HudRepresented), Without<PickingUp>>,
    ) {
        let Ok((weapon, hud_element)) = weapons.get(event.entity) else {
            return;
        };

        let Ok(mut hud_element) = weapon_images.get_mut(hud_element.0) else {
            warn!("Tried to set weapon inactive but no HUD element for it was found");
            return;
        };

        hud_element.image = weapon.gfx.clone();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn number_glyphs_pads_and_truncates() {
        use NumGlyph::{Digit as D, Minus};

        assert_eq!(number_glyphs(5, 3), (2, vec![D(5)]));
        assert_eq!(number_glyphs(100, 3), (0, vec![D(1), D(0), D(0)]));
        // Only the last `digits` characters are drawn
        assert_eq!(number_glyphs(1234, 3), (0, vec![D(2), D(3), D(4)]));
        assert_eq!(number_glyphs(-5, 3), (1, vec![Minus, D(5)]));
        assert_eq!(number_glyphs(-100, 3), (0, vec![D(1), D(0), D(0)]));
        assert_eq!(number_glyphs(0, 1), (0, vec![D(0)]));
    }

    #[test]
    fn ammo_counter_uses_small_digits() {
        assert_eq!(ammo_counter_text(0), [b' ', b' ', SMALL_DIGIT_BASE]);
        assert_eq!(ammo_counter_text(25), [b' ', SMALL_DIGIT_BASE + 2, SMALL_DIGIT_BASE + 5]);
        assert_eq!(
            ammo_counter_text(255),
            [SMALL_DIGIT_BASE + 2, SMALL_DIGIT_BASE + 5, SMALL_DIGIT_BASE + 5]
        );
    }

    #[test]
    fn weapon_pickup_cycles_twice_over_one_second() {
        assert_eq!(weapon_pickup_frame(0., 5), Some(0));
        assert_eq!(weapon_pickup_frame(0.45, 5), Some(4));
        assert_eq!(weapon_pickup_frame(0.55, 5), Some(0));
        assert_eq!(weapon_pickup_frame(0.95, 5), Some(4));
        assert_eq!(weapon_pickup_frame(1.0, 5), None);
        // Picked up slightly "in the future" relative to the client clock
        assert_eq!(weapon_pickup_frame(-0.1, 5), Some(0));
        assert_eq!(weapon_pickup_frame(0., 0), None);
    }

    #[test]
    fn face_selection() {
        let none = ItemFlags::empty();

        assert_eq!(face_for(none, 100, false), FaceId::Normal { health_level: 4, pain: false });
        assert_eq!(face_for(none, 250, true), FaceId::Normal { health_level: 4, pain: true });
        assert_eq!(face_for(none, 99, false), FaceId::Normal { health_level: 4, pain: false });
        assert_eq!(face_for(none, 19, false), FaceId::Normal { health_level: 0, pain: false });
        assert_eq!(face_for(none, -10, false), FaceId::Normal { health_level: 0, pain: false });

        assert_eq!(
            face_for(
                ItemFlags::INVISIBILITY | ItemFlags::INVULNERABILITY | ItemFlags::QUAD,
                50,
                false
            ),
            FaceId::InvisInvuln
        );
        assert_eq!(face_for(ItemFlags::QUAD | ItemFlags::INVISIBILITY, 50, false), FaceId::Quad);
        assert_eq!(face_for(ItemFlags::INVISIBILITY, 50, false), FaceId::Invis);
        assert_eq!(face_for(ItemFlags::INVULNERABILITY, 50, true), FaceId::Invuln);
    }

    #[test]
    fn viewsize_lines() {
        assert_eq!(status_bar_lines(100.), 48);
        assert_eq!(status_bar_lines(110.), 24);
        assert_eq!(status_bar_lines(120.), 0);
    }
}
