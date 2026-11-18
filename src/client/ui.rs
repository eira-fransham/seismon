//! Layout helpers shared by the HUD and the menu.
//!
//! Both are laid out in "Quake pixels" (the 320x200 coordinate space used by `sbar.c` and
//! `menu.c`), scaled by [`UI_SCALE`].

use bevy::{
    app::{App, Plugin, PostUpdate},
    asset::{Assets, Handle},
    ecs::{
        component::Component,
        query::With,
        system::{Query, Res},
    },
    image::Image,
    ui::{Display, JustifyContent, Node, PositionType, UiRect, Val, widget::ImageNode},
};
use seismon_utils::write_if_neq;

use crate::client::text::{AtlasText, Conchars};

/// The scale applied to Quake-pixel coordinates when converting them to UI pixels.
pub const UI_SCALE: f32 = 1.8;

/// The width and height of a conchars glyph.
pub const GLYPH_SIZE: f32 = 8.;

/// Scale a size in Quake pixels to UI pixels.
pub fn px(value: f32) -> Val {
    Val::Px(value * UI_SCALE)
}

/// An absolutely-positioned node at the given position, in Quake pixels.
pub fn abs_node(left: f32, top: f32) -> Node {
    Node {
        position_type: PositionType::Absolute,
        left: px(left),
        top: px(top),
        ..Default::default()
    }
}

/// An absolutely-positioned node with the given position and size, in Quake pixels.
pub fn abs_rect(left: f32, top: f32, width: f32, height: f32) -> Node {
    Node { width: px(width), height: px(height), ..abs_node(left, top) }
}

/// A node filling its parent.
pub fn fill_node(display: Display) -> Node {
    Node {
        position_type: PositionType::Absolute,
        width: Val::Percent(100.),
        height: Val::Percent(100.),
        display,
        ..Default::default()
    }
}

/// Text drawn with conchars at [`UI_SCALE`].
pub fn ui_text(conchars: &Conchars) -> AtlasText {
    AtlasText {
        text: "".into(),
        image: conchars.image.clone().into(),
        layout: conchars.layout.clone(),
        glyph_size: (px(GLYPH_SIZE), px(GLYPH_SIZE)),
        line_padding: UiRect::DEFAULT,
        justify: JustifyContent::FlexStart,
    }
}

pub fn ui_image(image: Handle<Image>) -> ImageNode {
    ImageNode { image, ..Default::default() }
}

/// Size this node to the native size of its [`ImageNode`]'s image, scaled by [`UI_SCALE`], once
/// the image has loaded. Used for pics whose size isn't known up front (e.g. menu titles).
#[derive(Component, Debug, Default, Clone, Copy)]
#[require(Node, ImageNode)]
pub struct NativeSize;

#[derive(Default)]
pub struct UiPlugin;

impl Plugin for UiPlugin {
    fn build(&self, app: &mut App) {
        app.add_systems(PostUpdate, apply_native_size);
    }
}

/// Sizes every [`NativeSize`] node to its image. This runs every frame (rather than only on
/// `Changed<ImageNode>`) as the image may finish loading after the node is spawned, but it only
/// writes the node when the size actually changes.
fn apply_native_size(
    images: Res<Assets<Image>>,
    mut nodes: Query<(&ImageNode, &mut Node), With<NativeSize>>,
) {
    for (image_node, mut node) in &mut nodes {
        let Some(image) = images.get(&image_node.image) else {
            continue;
        };

        let size = image.size_f32();
        write_if_neq!(node.width, px(size.x));
        write_if_neq!(node.height, px(size.y));
    }
}
