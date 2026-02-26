use bevy::{
    asset::Handle,
    ecs::component::Component,
    image::TextureAtlasLayout,
    ui::{JustifyContent, UiRect, Val, widget::ImageNode},
};
use seismon_utils::QString;

use bevy::prelude::*;

const GLYPH_WIDTH: usize = 8;
const GLYPH_HEIGHT: usize = 8;
const GLYPH_COLS: usize = 16;
const GLYPH_ROWS: usize = 16;
const SCALE: f32 = 2.;

#[derive(Resource, Reflect)]
pub struct Conchars {
    pub image: Handle<Image>,
    pub layout: Handle<TextureAtlasLayout>,
    pub glyph_size: [Val; 2],
}

impl FromWorld for Conchars {
    fn from_world(world: &mut World) -> Self {
        let assets = world.resource::<AssetServer>();
        Self {
            image: assets.load("gfx.wad#CONCHARS"),
            layout: assets.add(TextureAtlasLayout::from_grid(
                UVec2::new(GLYPH_WIDTH as _, GLYPH_HEIGHT as _),
                GLYPH_COLS as _,
                GLYPH_ROWS as _,
                None,
                None,
            )),
            glyph_size: [Val::Px(GLYPH_WIDTH as _) * SCALE, Val::Px(GLYPH_HEIGHT as _) * SCALE],
        }
    }
}

#[derive(Component, Debug)]
#[require(Node)]
pub struct AtlasText {
    pub text: QString,
    pub image: ImageNode,
    pub line_padding: UiRect,
    pub layout: Handle<TextureAtlasLayout>,
    pub glyph_size: (Val, Val),
    pub justify: JustifyContent,
}

#[derive(Component, Reflect, Debug)]
#[reflect(Component)]
#[relationship(relationship_target = AtlasTextLines)]
pub struct AtlasTextLine {
    #[relationship]
    text: Entity,
}

#[derive(Component, Reflect, Debug)]
#[reflect(Component)]
#[relationship_target(relationship = AtlasTextLine, linked_spawn)]
pub struct AtlasTextLines(Vec<Entity>);

pub mod systems {
    use bevy::{
        ecs::{
            entity::Entity,
            query::Changed,
            system::{Commands, Query},
        },
        image::TextureAtlas,
        ui::{FlexDirection, FlexWrap, Node, Val, widget::ImageNode},
    };

    use crate::client::text::{AtlasText, AtlasTextLine, AtlasTextLines};

    pub fn update_atlas_text(
        mut commands: Commands,
        text: Query<(Entity, &AtlasText), Changed<AtlasText>>,
    ) {
        for (entity, text) in text.iter() {
            commands.entity(entity).despawn_related::<AtlasTextLines>();

            commands.entity(entity).with_children(|commands| {
                for line in text.text.lines() {
                    commands
                        .spawn((
                            Node {
                                flex_direction: FlexDirection::Row,
                                min_height: text.glyph_size.1,
                                width: Val::Percent(100.),
                                flex_wrap: FlexWrap::Wrap,
                                padding: text.line_padding,
                                justify_content: text.justify,
                                ..Default::default()
                            },
                            AtlasTextLine { text: entity },
                        ))
                        .with_children(|commands| {
                            for chr in &*line.raw {
                                if chr.is_ascii_whitespace() {
                                    commands.spawn(Node {
                                        width: text.glyph_size.0,
                                        height: text.glyph_size.1,
                                        ..Default::default()
                                    });
                                } else {
                                    commands.spawn((
                                        ImageNode {
                                            texture_atlas: Some(TextureAtlas {
                                                layout: text.layout.clone(),
                                                index: *chr as usize,
                                            }),
                                            ..text.image.clone()
                                        },
                                        Node {
                                            width: text.glyph_size.0,
                                            height: text.glyph_size.1,
                                            ..Default::default()
                                        },
                                    ));
                                }
                            }
                        });
                }
            });
        }
    }
}
