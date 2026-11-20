//! What CSQC draws each frame, and how the engine renders it.
//!
//! The drawing builtins (`drawpic`, `drawfill`, `drawstring` etc.) push [`DrawCmd`]s to
//! [`CsqcDrawList`], which [`render_csqc_2d`] turns into UI nodes. The scene builtins
//! (`clearscene`, `setproperty`, `renderscene` etc.) fill in [`CsqcScene`], which
//! [`apply_csqc_scene`] applies to the world camera. Both are cleared before CSQC is called each
//! frame. Positions and sizes are in virtual (Quake) pixels, as for the rest of the UI (see
//! [`px`]).

use bevy::prelude::*;
use seismon_utils::QString;

use super::{Csqc, CsqcMode};
use crate::client::{
    state::ViewModel,
    systems::frame::ViewFor,
    text::Conchars,
    ui::{abs_rect, px, ui_image, ui_text},
};

/// A 2D drawing operation, in the order that CSQC made it.
#[derive(Debug, Clone)]
pub enum DrawCmd {
    /// `drawpic`: draws `image` at `pos` with size `size`.
    Pic { pos: Vec2, size: Vec2, image: Handle<Image>, color: Color },
    /// `drawsubpic`: draws the part of `image` from `src_pos` with size `src_size` (both
    /// fractions of the image's size) at `pos` with size `size`.
    SubPic {
        pos: Vec2,
        size: Vec2,
        image: Handle<Image>,
        src_pos: Vec2,
        src_size: Vec2,
        color: Color,
    },
    /// `drawfill`: fills a rectangle.
    Fill { pos: Vec2, size: Vec2, color: Color },
    /// `drawstring`/`drawrawstring`/`drawcharacter`: draws text with conchars, where `size` is the
    /// size of each glyph.
    String { pos: Vec2, text: QString, size: Vec2, color: Color },
    /// `drawsetcliparea`: clips the following drawing to a rectangle.
    ClipArea { pos: Vec2, size: Vec2 },
    /// `drawresetcliparea`: stops clipping.
    ResetClip,
}

/// The 2D drawing that CSQC has done this frame.
#[derive(Resource, Debug, Default)]
pub struct CsqcDrawList(pub Vec<DrawCmd>);

impl CsqcDrawList {
    pub fn push(&mut self, cmd: DrawCmd) {
        self.0.push(cmd);
    }
}

/// The view that CSQC set up with `setproperty`, for full CSQC.
#[derive(Debug, Clone, Default)]
pub struct CsqcView {
    pub origin: Option<Vec3>,
    /// Quake `[pitch, yaw, roll]` angles in degrees.
    pub angles: Option<Vec3>,
    /// The horizontal field of view in degrees.
    pub fov_x: Option<f32>,
    /// The position of the view on the screen, in virtual pixels.
    pub pos: Option<Vec2>,
    /// The size of the view on the screen, in virtual pixels.
    pub size: Option<Vec2>,
    /// `VF_DRAWWORLD`.
    pub draw_world: bool,
    /// `VF_DRAWENGINESBAR`: whether the engine should draw its status bar.
    pub draw_engine_sbar: bool,
    /// `VF_DRAWCROSSHAIR`.
    pub draw_crosshair: bool,
}

/// The 3D scene that CSQC has set up this frame, for full CSQC.
#[derive(Resource, Debug, Default)]
pub struct CsqcScene {
    pub view: CsqcView,
    /// Whether `renderscene` was called this frame. If it isn't, the world isn't drawn, so CSQC
    /// that doesn't do anything renders nothing.
    pub rendered_this_frame: bool,
}

impl CsqcScene {
    /// Resets the scene before CSQC is called (`clearscene` does the same).
    pub fn clear(&mut self) {
        *self =
            Self { view: CsqcView { draw_world: true, ..default() }, rendered_this_frame: false };
    }
}

/// The root of the UI nodes that CSQC's 2D drawing is rendered to.
#[derive(Component, Debug)]
pub struct CsqcHudRoot;

/// A world camera that has been turned off because full CSQC didn't render the scene.
#[derive(Component, Debug)]
pub struct CsqcDisabledCamera;

/// Spawns [`CsqcHudRoot`].
pub fn init_csqc_hud(mut commands: Commands) {
    commands.spawn((
        Name::new("CSQC HUD"),
        CsqcHudRoot,
        Node {
            position_type: PositionType::Absolute,
            width: Val::Percent(100.),
            height: Val::Percent(100.),
            ..default()
        },
        // Above the engine's HUD (which CSQC replaces anyway), below the console and menus.
        GlobalZIndex(1),
        Pickable::IGNORE,
    ));
}

/// Rebuilds [`CsqcHudRoot`]'s children from [`CsqcDrawList`].
pub fn render_csqc_2d(
    mut commands: Commands,
    draw_list: Res<CsqcDrawList>,
    root: Single<Entity, With<CsqcHudRoot>>,
    conchars: Res<Conchars>,
    images: Res<Assets<Image>>,
) {
    let root = *root;
    commands.entity(root).despawn_children();

    // The parent that drawing currently goes to: the root, or a clipping node.
    let mut parent = root;
    for cmd in &draw_list.0 {
        match cmd {
            DrawCmd::Pic { pos, size, image, color } => {
                commands.spawn((
                    ChildOf(parent),
                    abs_rect(pos.x, pos.y, size.x, size.y),
                    ImageNode { color: *color, ..ui_image(image.clone()) },
                ));
            }
            DrawCmd::SubPic { pos, size, image, src_pos, src_size, color } => {
                // `ImageNode::rect` is in texels, but CSQC gives fractions of the image.
                let rect = images.get(image).map(|img| {
                    let img_size = img.size_f32();
                    Rect::from_corners(*src_pos * img_size, (*src_pos + *src_size) * img_size)
                });
                commands.spawn((
                    ChildOf(parent),
                    abs_rect(pos.x, pos.y, size.x, size.y),
                    ImageNode { color: *color, rect, ..ui_image(image.clone()) },
                ));
            }
            DrawCmd::Fill { pos, size, color } => {
                commands.spawn((
                    ChildOf(parent),
                    abs_rect(pos.x, pos.y, size.x, size.y),
                    BackgroundColor(*color),
                ));
            }
            DrawCmd::String { pos, text, size, color } => {
                let mut atlas_text = ui_text(&conchars);
                atlas_text.text = text.clone();
                atlas_text.glyph_size = (px(size.x), px(size.y));
                atlas_text.image.color = *color;
                commands.spawn((
                    ChildOf(parent),
                    Node {
                        position_type: PositionType::Absolute,
                        left: px(pos.x),
                        top: px(pos.y),
                        ..default()
                    },
                    atlas_text,
                ));
            }
            DrawCmd::ClipArea { pos, size } => {
                parent = commands
                    .spawn((
                        ChildOf(root),
                        Node {
                            overflow: Overflow::clip(),
                            ..abs_rect(pos.x, pos.y, size.x, size.y)
                        },
                    ))
                    .id();
                // Children of the clipping node are positioned relative to it, but CSQC
                // positions are relative to the screen, so offset them back.
                parent = commands
                    .spawn((
                        ChildOf(parent),
                        Node {
                            position_type: PositionType::Absolute,
                            left: px(-pos.x),
                            top: px(-pos.y),
                            width: Val::Vw(100.),
                            height: Val::Vh(100.),
                            ..default()
                        },
                    ))
                    .id();
            }
            DrawCmd::ResetClip => parent = root,
        }
    }
}

/// Applies [`CsqcScene`] to the world cameras for full CSQC: the world is only drawn if CSQC
/// called `renderscene`, from the view that it set up.
pub fn apply_csqc_scene(
    mut commands: Commands,
    csqc: Option<Res<Csqc>>,
    scene: Res<CsqcScene>,
    mut cameras: Query<(Entity, &mut Camera, &mut Transform, &mut Projection), With<ViewFor>>,
    disabled: Query<Entity, With<CsqcDisabledCamera>>,
) {
    let full = csqc.is_some_and(|csqc| csqc.mode == CsqcMode::Full);
    if !full {
        // Turn the cameras back on once CSQC stops drawing the scene.
        for entity in &disabled {
            if let Ok((_, mut camera, ..)) = cameras.get_mut(entity) {
                camera.is_active = true;
            }
            commands.entity(entity).remove::<CsqcDisabledCamera>();
        }
        return;
    }

    let active = scene.rendered_this_frame && scene.view.draw_world;
    for (entity, mut camera, mut transform, mut projection) in &mut cameras {
        if camera.is_active != active {
            camera.is_active = active;
        }
        if !active {
            commands.entity(entity).insert(CsqcDisabledCamera);
            continue;
        }

        // TODO: The camera is a child of the view entity, so this is relative to it rather than
        //       the world. Once `setproperty` is implemented, the camera should be moved out of
        //       the view entity (or the view entity moved) for CSQC's view.
        if let Some(origin) = scene.view.origin {
            transform.translation = origin;
        }
        if let Some([pitch, yaw, roll]) = scene.view.angles.map(|angles| angles.to_array()) {
            transform.rotation = Quat::from_euler(
                EulerRot::ZYX,
                yaw.to_radians(),
                -pitch.to_radians(),
                roll.to_radians(),
            );
        }
        if let (Some(fov_x), Projection::Perspective(perspective)) =
            (scene.view.fov_x, &mut *projection)
        {
            perspective.fov =
                crate::common::math::fov_x_to_fov_y(fov_x.to_radians(), perspective.aspect_ratio)
                    .unwrap_or(perspective.fov);
        }
    }
}

/// Hides the view model while full CSQC is drawing the view.
pub fn hide_view_model(mut view_models: Query<&mut Visibility, With<ViewModel>>) {
    for mut visibility in &mut view_models {
        if *visibility != Visibility::Hidden {
            *visibility = Visibility::Hidden;
        }
    }
}
