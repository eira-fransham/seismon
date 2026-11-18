//! Drawing of the [`Menu`] with Bevy UI. Mirrors the drawing functions in vanilla `menu.c`.
//!
//! Positions are given in the same coordinate space as `menu.c`: a 320x200 canvas, centered on
//! the screen and scaled by [`UI_SCALE`](crate::client::ui::UI_SCALE).

use bevy::{
    app::{App, Plugin, PostUpdate, Startup},
    asset::{AssetServer, Handle},
    camera::visibility::Visibility,
    ecs::{
        children,
        component::Component,
        entity::Entity,
        hierarchy::ChildOf,
        message::MessageWriter,
        name::Name,
        query::{With, Without},
        resource::Resource,
        schedule::{IntoScheduleConfigs as _, common_conditions::resource_exists},
        system::{Commands, Local, Query, Res, ResMut, Single},
        world::{FromWorld, World},
    },
    image::Image,
    log::warn,
    state::state::{OnEnter, State},
    time::{Real, Time},
    ui::{
        Display, GlobalZIndex, JustifyContent, Node, PositionType, UiRect, Val, widget::ImageNode,
    },
};
use bevy_mod_commands::Registry;
use bevy_seedling::prelude::AudioSample;
use seismon_utils::{QStr, QString, write_if_neq};

use crate::client::{
    Connection,
    input::InputFocus,
    menu::{Item, Menu, MenuBodyView},
    sound::MixerMessage,
    text::{AtlasText, Conchars},
    ui::{GLYPH_SIZE, NativeSize, abs_node, abs_rect, fill_node, px, ui_image, ui_text},
};

/// The size of the menu canvas.
const MENU_WIDTH: f32 = 320.;
const MENU_HEIGHT: f32 = 200.;

/// The position of `gfx/qplaque.lmp`.
const PLAQUE_POS: (f32, f32) = (16., 4.);
/// The y position of the menu title.
const TITLE_TOP: f32 = 4.;
/// The position of a predefined menu body bitmap.
const BODY_POS: (f32, f32) = (72., 32.);
/// The x position of the `menudot` cursor for predefined menu bodies.
const DOT_LEFT: f32 = 54.;
/// The height of an item in a predefined menu body.
const DOT_ROW_HEIGHT: f32 = 20.;
/// The number of frames in the `menudot` animation.
const DOT_FRAMES: usize = 6;
/// The `menudot` animation runs at 10 frames per second.
const DOT_FPS: f32 = 10.;

/// The y position of the first row of a dynamic menu body.
const ROW_TOP: f32 = 32.;
/// The height of a row of a dynamic menu body.
const ROW_HEIGHT: f32 = GLYPH_SIZE;
/// Item labels are right-aligned to this x position (`M_Print (16, y, "    Customize controls")`
/// in `M_Options_Draw` pads every label to 22 characters).
const LABEL_RIGHT: f32 = 16. + 22. * GLYPH_SIZE;
/// The x position of the blinking cursor of a dynamic menu body.
const CURSOR_LEFT: f32 = 200.;
/// The x position of item values (sliders, checkboxes, etc.).
const VALUE_LEFT: f32 = 220.;
/// Blinking cursors toggle 4 times per second (`(int)(realtime*4)&1`).
const BLINK_RATE: f32 = 4.;
/// Conchars index of the blinking menu cursor (alternates with the next glyph).
const CURSOR_GLYPH: u8 = 12;
/// Conchars index of the blinking text field caret (alternates with the next glyph).
const CARET_GLYPH: u8 = 10;

/// The number of segments in the middle of the slider (`SLIDER_RANGE`).
const SLIDER_RANGE: usize = 10;
const SLIDER_LEFT_GLYPH: u8 = 128;
const SLIDER_MIDDLE_GLYPH: u8 = 129;
const SLIDER_RIGHT_GLYPH: u8 = 130;
const SLIDER_KNOB_GLYPH: u8 = 131;

#[derive(Default)]
pub struct MenuRenderPlugin;

impl Plugin for MenuRenderPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<MenuGfx>()
            .init_resource::<MenuSounds>()
            .add_systems(Startup, systems::spawn_menu_ui)
            .add_systems(
                OnEnter(InputFocus::Menu),
                systems::open_menu.run_if(resource_exists::<Menu>),
            )
            .add_systems(
                PostUpdate,
                (
                    systems::update_menu_visibility,
                    (
                        systems::rebuild_menu_page,
                        (systems::update_menu_cursor, systems::update_menu_values),
                    )
                        .chain()
                        .run_if(resource_exists::<Menu>),
                )
                    .before(crate::client::text::systems::update_atlas_text),
            );
    }
}

/// Marker for the root of the menu UI.
#[derive(Component, Debug)]
pub struct MenuRoot;

/// Marker for the background drawn behind the menu when the console is forced up (i.e. when
/// disconnected).
#[derive(Component, Debug)]
pub struct MenuBackground;

/// Marker for the 320x200 canvas that the current menu page is drawn on.
#[derive(Component, Debug)]
pub struct MenuCanvas;

/// The animated `menudot` cursor of predefined menu bodies.
#[derive(Component, Debug)]
struct MenuCursorDot;

/// The blinking cursor of dynamic menu bodies.
#[derive(Component, Debug)]
struct MenuCursorGlyph;

/// The value (checkbox text, enum name, text field contents) of the item at the given index.
#[derive(Component, Debug, Clone, Copy)]
struct MenuItemValue(usize);

/// The knob of the slider at the given index.
#[derive(Component, Debug, Clone, Copy)]
struct MenuSliderKnob(usize);

/// All of the images used by the menu.
#[derive(Resource, Clone, Debug)]
pub struct MenuGfx {
    plaque: Handle<Image>,
    dots: [Handle<Image>; DOT_FRAMES],
    conback: Handle<Image>,
    /// Titles and predefined bodies of every menu, so that they are already loaded when the menu
    /// is opened (as in Richter's `MenuRenderer::new`).
    _preloaded: Vec<Handle<Image>>,
}

impl FromWorld for MenuGfx {
    fn from_world(world: &mut World) -> Self {
        let paths = world
            .get_resource::<Menu>()
            .map(|menu| {
                menu.all_menus()
                    .flat_map(|m| {
                        let body = match m.view().body() {
                            MenuBodyView::Predefined { path } => Some(path.to_string()),
                            MenuBodyView::Dynamic => None,
                        };

                        std::iter::once(m.view().title_path().to_owned()).chain(body)
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();

        let assets = world.resource::<AssetServer>();

        Self {
            plaque: assets.load("gfx/qplaque.lmp"),
            dots: std::array::from_fn(|i| assets.load(format!("gfx/menudot{}.lmp", i + 1))),
            conback: assets.load("gfx/conback.lmp"),
            _preloaded: paths.into_iter().map(|path| assets.load(path)).collect(),
        }
    }
}

/// The sounds played by the menu.
#[derive(Resource, Clone, Debug)]
pub struct MenuSounds {
    /// Played when moving the cursor (`misc/menu1.wav`).
    pub navigate: Handle<AudioSample>,
    /// Played when entering a menu or activating an item (`misc/menu2.wav`).
    pub enter: Handle<AudioSample>,
    /// Played when changing a value (`misc/menu3.wav`).
    pub adjust: Handle<AudioSample>,
}

impl FromWorld for MenuSounds {
    fn from_world(world: &mut World) -> Self {
        let assets = world.resource::<AssetServer>();

        Self {
            navigate: assets.load("sound/misc/menu1.wav"),
            enter: assets.load("sound/misc/menu2.wav"),
            adjust: assets.load("sound/misc/menu3.wav"),
        }
    }
}

/// Text drawn with the alternate (gold) half of conchars, like `M_Print`.
fn gold_text(text: &str) -> QString {
    QStr::from(&text.bytes().map(|b| b | 0x80).collect::<Vec<_>>()[..]).into_owned()
}

fn glyphs(bytes: &[u8]) -> QString {
    QStr::from(bytes).into_owned()
}

/// The x position of a right-aligned label with `len` characters.
fn label_left(len: usize) -> f32 {
    LABEL_RIGHT - len as f32 * GLYPH_SIZE
}

/// The y position of row `index` of a dynamic menu body.
fn row_top(index: usize) -> f32 {
    ROW_TOP + index as f32 * ROW_HEIGHT
}

/// The x position of a slider knob at `position` (from `0` to `1`), as in `M_DrawSlider`.
fn slider_knob_left(position: f32) -> f32 {
    VALUE_LEFT + (SLIDER_RANGE - 1) as f32 * GLYPH_SIZE * position.clamp(0., 1.)
}

/// The frame of the `menudot` animation at `secs` (`(int)(host_time * 10)%6`).
fn dot_frame(secs: f32) -> usize {
    (secs * DOT_FPS) as usize % DOT_FRAMES
}

/// The glyph of a blinking cursor at `secs`, alternating between `base` and `base + 1`.
fn blink_glyph(base: u8, secs: f32) -> u8 {
    base + ((secs * BLINK_RATE) as u32 & 1) as u8
}

/// The slider track: the left end, the middle segments and the right end.
fn slider_track() -> Vec<u8> {
    std::iter::once(SLIDER_LEFT_GLYPH)
        .chain(std::iter::repeat_n(SLIDER_MIDDLE_GLYPH, SLIDER_RANGE))
        .chain(std::iter::once(SLIDER_RIGHT_GLYPH))
        .collect()
}

mod systems {
    use super::*;

    pub fn spawn_menu_ui(mut commands: Commands, gfx: Res<MenuGfx>) {
        commands.spawn((
            Name::new("menu"),
            MenuRoot,
            Node {
                position_type: PositionType::Absolute,
                width: Val::Percent(100.),
                height: Val::Percent(100.),
                ..Default::default()
            },
            // Above the console, below the loading screen.
            GlobalZIndex(3),
            Visibility::Hidden,
            children![
                (
                    Name::new("menu background"),
                    MenuBackground,
                    fill_node(Display::None),
                    ui_image(gfx.conback.clone()),
                ),
                (
                    Name::new("menu canvas"),
                    MenuCanvas,
                    Node {
                        position_type: PositionType::Absolute,
                        left: Val::Percent(50.),
                        top: Val::Percent(50.),
                        margin: UiRect {
                            left: px(-MENU_WIDTH / 2.),
                            top: px(-MENU_HEIGHT / 2.),
                            ..Default::default()
                        },
                        width: px(MENU_WIDTH),
                        height: px(MENU_HEIGHT),
                        ..Default::default()
                    },
                ),
            ],
        ));
    }

    /// Like `M_ToggleMenu_f`/`M_Menu_Main_f`: opening the menu always starts at the main menu
    /// and plays the enter sound.
    pub fn open_menu(
        mut menu: ResMut<Menu>,
        sounds: Res<MenuSounds>,
        mut mixer: MessageWriter<MixerMessage>,
    ) {
        menu.reset();
        mixer.write(MixerMessage::StartLocalSound(sounds.enter.clone()));
    }

    pub fn update_menu_visibility(
        focus: Res<State<InputFocus>>,
        conn: Option<Res<Connection>>,
        mut root: Single<&mut Visibility, With<MenuRoot>>,
        mut background: Single<&mut Node, With<MenuBackground>>,
    ) {
        let visibility =
            if *focus.get() == InputFocus::Menu { Visibility::Visible } else { Visibility::Hidden };
        write_if_neq!(**root, visibility);

        // `M_Draw` draws the console background when the console is forced up, i.e. when we are
        // disconnected. Otherwise the menu is drawn directly over the world.
        let display = if conn.is_none() { Display::Flex } else { Display::None };
        write_if_neq!(background.display, display);
    }

    /// Respawn the contents of the canvas whenever the active (sub)menu changes.
    pub fn rebuild_menu_page(
        mut commands: Commands,
        menu: Res<Menu>,
        gfx: Res<MenuGfx>,
        conchars: Res<Conchars>,
        assets: Res<AssetServer>,
        canvas: Single<Entity, With<MenuCanvas>>,
        mut last_generation: Local<Option<u64>>,
    ) {
        if *last_generation == Some(menu.generation()) {
            return;
        }
        *last_generation = Some(menu.generation());

        let page = match menu.active_submenu() {
            Ok(page) => page,
            Err(e) => {
                warn!("Could not draw menu: {e}");
                return;
            }
        };

        let canvas = *canvas;
        commands.entity(canvas).despawn_children();

        let view = page.view();

        if view.draw_plaque() {
            commands.spawn((
                Name::new("plaque"),
                abs_node(PLAQUE_POS.0, PLAQUE_POS.1),
                ui_image(gfx.plaque.clone()),
                NativeSize,
                ChildOf(canvas),
            ));
        }

        // `M_DrawPic ((320-p->width)/2, 4, p)`
        commands.spawn((
            Name::new("title"),
            Node {
                position_type: PositionType::Absolute,
                top: px(TITLE_TOP),
                width: Val::Percent(100.),
                justify_content: JustifyContent::Center,
                ..Default::default()
            },
            ChildOf(canvas),
            children![(ui_image(assets.load(view.title_path().to_owned())), NativeSize)],
        ));

        match view.body() {
            MenuBodyView::Predefined { path } => {
                commands.spawn((
                    Name::new("body"),
                    abs_node(BODY_POS.0, BODY_POS.1),
                    ui_image(assets.load(path.to_string())),
                    NativeSize,
                    ChildOf(canvas),
                ));
                commands.spawn((
                    Name::new("cursor"),
                    MenuCursorDot,
                    abs_node(DOT_LEFT, BODY_POS.1),
                    ui_image(gfx.dots[0].clone()),
                    NativeSize,
                    ChildOf(canvas),
                ));
            }

            MenuBodyView::Dynamic => {
                for (i, item) in page.items().enumerate() {
                    let top = row_top(i);
                    let name = item.name();
                    let len = name.len();

                    commands.spawn((
                        Name::new(format!("label: {name}")),
                        abs_rect(label_left(len), top, len as f32 * GLYPH_SIZE, ROW_HEIGHT),
                        AtlasText { text: gold_text(name), ..ui_text(&conchars) },
                        ChildOf(canvas),
                    ));

                    match item.item() {
                        Item::Toggle(_) | Item::Enum(_) | Item::TextField(_) => {
                            commands.spawn((
                                MenuItemValue(i),
                                abs_rect(VALUE_LEFT, top, 0., ROW_HEIGHT),
                                ui_text(&conchars),
                                ChildOf(canvas),
                            ));
                        }
                        Item::Slider(_) => {
                            let track = slider_track();
                            commands.spawn((
                                abs_rect(
                                    VALUE_LEFT - GLYPH_SIZE,
                                    top,
                                    track.len() as f32 * GLYPH_SIZE,
                                    ROW_HEIGHT,
                                ),
                                AtlasText { text: glyphs(&track), ..ui_text(&conchars) },
                                ChildOf(canvas),
                            ));
                            commands.spawn((
                                MenuSliderKnob(i),
                                abs_rect(slider_knob_left(0.), top, GLYPH_SIZE, ROW_HEIGHT),
                                AtlasText {
                                    text: glyphs(&[SLIDER_KNOB_GLYPH]),
                                    ..ui_text(&conchars)
                                },
                                ChildOf(canvas),
                            ));
                        }
                        Item::Submenu(_) | Item::Action(_) => {}
                    }
                }

                commands.spawn((
                    Name::new("cursor"),
                    MenuCursorGlyph,
                    abs_rect(CURSOR_LEFT, ROW_TOP, GLYPH_SIZE, ROW_HEIGHT),
                    AtlasText { text: glyphs(&[CURSOR_GLYPH]), ..ui_text(&conchars) },
                    ChildOf(canvas),
                ));
            }
        }
    }

    type GlyphCursor = (&'static mut Node, &'static mut AtlasText);

    /// Move the cursor to the selected item and animate it.
    pub fn update_menu_cursor(
        menu: Res<Menu>,
        gfx: Res<MenuGfx>,
        time: Res<Time<Real>>,
        mut dots: Query<(&mut Node, &mut ImageNode), With<MenuCursorDot>>,
        mut glyph_cursors: Query<GlyphCursor, (With<MenuCursorGlyph>, Without<MenuCursorDot>)>,
    ) {
        let Some(cursor) = menu.cursor() else {
            return;
        };
        let secs = time.elapsed_secs();

        for (mut node, mut image) in &mut dots {
            write_if_neq!(node.top, px(BODY_POS.1 + cursor as f32 * DOT_ROW_HEIGHT));
            write_if_neq!(image.image, gfx.dots[dot_frame(secs)].clone());
        }

        for (mut node, mut text) in &mut glyph_cursors {
            write_if_neq!(node.top, px(row_top(cursor)));
            write_if_neq!(text.text, glyphs(&[blink_glyph(CURSOR_GLYPH, secs)]));
        }
    }

    /// Show the current values of the items, read from their cvars so that changes made
    /// elsewhere (e.g. the console or "Reset to defaults") are reflected.
    pub fn update_menu_values(
        menu: Res<Menu>,
        registry: Res<Registry>,
        time: Res<Time<Real>>,
        mut values: Query<(&MenuItemValue, &mut AtlasText, &mut Node), Without<MenuSliderKnob>>,
        mut knobs: Query<(&MenuSliderKnob, &mut Node), Without<MenuItemValue>>,
    ) {
        let Ok(page) = menu.active_submenu() else {
            return;
        };
        let items = page.items().collect::<Vec<_>>();
        let cursor = menu.cursor();
        let secs = time.elapsed_secs();

        for (&MenuItemValue(i), mut text, mut node) in &mut values {
            let Some(item) = items.get(i) else {
                continue;
            };

            let new_text: QString = match item.item() {
                Item::Toggle(toggle) => {
                    let on = toggle.read(&registry).unwrap_or(toggle.get());
                    // `M_DrawCheckbox`
                    gold_text(if on { "on" } else { "off" })
                }
                Item::Enum(e) => gold_text(e.read_name(&registry)),
                Item::TextField(field) => {
                    let mut bytes = field.text().as_bytes().to_vec();
                    if cursor == Some(i) {
                        bytes.insert(
                            field.cursor().min(bytes.len()),
                            blink_glyph(CARET_GLYPH, secs),
                        );
                    }
                    glyphs(&bytes)
                }
                Item::Slider(_) | Item::Submenu(_) | Item::Action(_) => continue,
            };

            write_if_neq!(node.width, px(new_text.raw.len() as f32 * GLYPH_SIZE));
            write_if_neq!(text.text, new_text);
        }

        for (&MenuSliderKnob(i), mut node) in &mut knobs {
            let Some(Item::Slider(slider)) = items.get(i).map(|item| item.item()) else {
                continue;
            };

            let position =
                slider.read(&registry).map_or(slider.position(), |v| slider.position_of(v));
            write_if_neq!(node.left, px(slider_knob_left(position)));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn labels_are_right_aligned() {
        // "    Customize controls" is printed at x = 16
        assert_eq!(label_left(22), 16.);
        assert_eq!(label_left("Customize controls".len()), 16. + 4. * GLYPH_SIZE);
        assert_eq!(label_left(0), LABEL_RIGHT);
    }

    #[test]
    fn slider_layout() {
        assert_eq!(slider_track().len(), SLIDER_RANGE + 2);
        assert_eq!(slider_knob_left(0.), VALUE_LEFT);
        assert_eq!(slider_knob_left(1.), VALUE_LEFT + 72.);
        assert_eq!(slider_knob_left(2.), VALUE_LEFT + 72.);
        assert_eq!(slider_knob_left(-1.), VALUE_LEFT);
    }

    #[test]
    fn cursor_animation() {
        assert_eq!(dot_frame(0.), 0);
        assert_eq!(dot_frame(0.15), 1);
        assert_eq!(dot_frame(0.55), 5);
        assert_eq!(dot_frame(0.65), 0);

        assert_eq!(blink_glyph(CURSOR_GLYPH, 0.), 12);
        assert_eq!(blink_glyph(CURSOR_GLYPH, 0.3), 13);
        assert_eq!(blink_glyph(CURSOR_GLYPH, 0.5), 12);
    }

    #[test]
    fn gold_text_sets_high_bit() {
        assert_eq!(&*gold_text("Ab").raw, &[b'A' | 0x80, b'b' | 0x80]);
    }
}
