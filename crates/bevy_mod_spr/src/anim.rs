use bevy_asset::{Assets, Handle};
use bevy_ecs::{
    component::Component,
    hierarchy::ChildOf,
    query::Changed,
    reflect::ReflectComponent,
    system::{Query, Res},
};
use bevy_log::error;
use bevy_mod_billboard::BillboardTexture;
use bevy_mod_mdl::MdlSettings;
use bevy_reflect::Reflect;

use crate::{Sprite, SpriteFrame};

#[derive(Component, Reflect)]
#[reflect(Component)]
pub(crate) struct CalculatedSprSettings {
    animation: u8,
    sprite: Handle<Sprite>,
    cur_animation: u8,
}

impl From<Handle<Sprite>> for CalculatedSprSettings {
    fn from(sprite: Handle<Sprite>) -> Self {
        Self { animation: 0, sprite, cur_animation: 0 }
    }
}

pub(crate) fn propagate_spr_settings(
    // TODO: This will propagate the wrong mdl settings in case multiple exist
    mdl_settings: Query<&MdlSettings, Changed<MdlSettings>>,
    calculated_mdl_settings: Query<(&mut CalculatedSprSettings, &ChildOf)>,
) {
    for (mut calculated, ChildOf(parent)) in calculated_mdl_settings {
        let Ok(new_settings) = mdl_settings.get(*parent) else {
            continue;
        };

        calculated.animation = new_settings.frame;
        // TODO: We should share `MdlSettings` between mdls and sprs without this hack. Probably
        // separate skin and frame components.
    }
}

pub(crate) fn update_sprs(
    entities: Query<
        (&mut CalculatedSprSettings, &mut BillboardTexture),
        Changed<CalculatedSprSettings>,
    >,
    sprites: Res<Assets<Sprite>>,
) {
    for (mut settings, mut texture) in entities {
        if settings.animation == settings.cur_animation {
            continue;
        }

        let sprite = sprites.get(&settings.sprite).expect("Missing spr");

        'set_anim: {
            if settings.animation != settings.cur_animation {
                let Some(frame) = sprite.frames.get(settings.animation as usize) else {
                    error!("Missing animation {}", settings.animation);
                    break 'set_anim;
                };

                settings.cur_animation = settings.animation;

                match frame {
                    SpriteFrame::Static { frame } => texture.0 = frame.image.clone(),
                    SpriteFrame::Animated { .. } => todo!("Animated sprite frames"),
                }
            }
        }
    }
}
