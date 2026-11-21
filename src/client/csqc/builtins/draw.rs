//! CSQC's 2D drawing builtins (`drawpic`, `drawstring` etc.), which add to
//! [`CsqcDrawList`](crate::client::csqc::draw::CsqcDrawList). Positions and sizes are in virtual
//! (Quake) pixels.

use std::sync::atomic::{AtomicBool, Ordering};

use bevy::prelude::*;
use bevy_mod_scripting::bindings::NamespaceBuilder;
use bevy_mod_scripting_qcvm::builtin_argc;
use seismon_utils::QString;

use crate::{
    client::csqc::draw::{CsqcDrawList, DrawCmd},
    common::qc::{
        stub::{S, Vec3Arg, string, unimplemented_builtin},
        text::{colored_chars, visible_len},
        with_vm_ctx,
    },
};

/// The size of a glyph when CSQC doesn't give one.
const DEFAULT_GLYPH_SIZE: f32 = 8.;

/// Warns (once) that a draw flag isn't supported. Only normal blending is, so other flags (e.g.
/// additive) are drawn normally.
fn check_draw_flag(flag: f32) {
    static WARNED: AtomicBool = AtomicBool::new(false);
    if flag != 0. && !WARNED.swap(true, Ordering::Relaxed) {
        warn!("CSQC draw flag {flag} isn't supported, drawing normally");
    }
}

fn color(rgb: Vec3Arg, alpha: f32) -> Color {
    Color::srgba(rgb[0], rgb[1], rgb[2], alpha)
}

fn push(world: &mut World, cmd: DrawCmd) {
    if let Some(mut list) = world.get_resource_mut::<CsqcDrawList>() {
        list.push(cmd);
    }
}

/// The draw commands for `text` at `pos`, splitting it where colour codes change the colour (if
/// `colors`).
pub fn string_cmds(
    pos: Vec2,
    text: &[u8],
    size: Vec2,
    rgb: Vec3Arg,
    alpha: f32,
    colors: bool,
) -> Vec<DrawCmd> {
    if !colors {
        return vec![DrawCmd::String {
            pos,
            text: QString::from(text.to_vec()),
            size,
            color: color(rgb, alpha),
        }];
    }

    let mut cmds = Vec::new();
    let mut run: Vec<u8> = Vec::new();
    let mut run_color = None;
    let mut run_start = 0;
    let chars = colored_chars(text);
    for (i, c) in chars.iter().enumerate() {
        if c.color != run_color && !run.is_empty() {
            cmds.push((run_start, std::mem::take(&mut run), run_color));
            run_start = i;
        }
        if run.is_empty() {
            run_start = i;
        }
        run_color = c.color;
        run.push(c.byte);
    }
    if !run.is_empty() {
        cmds.push((run_start, run, run_color));
    }

    cmds.into_iter()
        .map(|(start, text, code)| {
            let rgb = code.map_or(rgb, |code| code.to_array());
            DrawCmd::String {
                pos: pos + Vec2::new(start as f32 * size.x, 0.),
                text: QString::from(text),
                size,
                color: color(rgb, alpha),
            }
        })
        .collect()
}

/// The size of glyphs given as `size` (or the default if it's zero).
fn glyph_size(size: Vec3Arg) -> Vec2 {
    let size = Vec2::new(size[0], size[1]);
    if size == Vec2::ZERO { Vec2::splat(DEFAULT_GLYPH_SIZE) } else { size }
}

pub fn register<N: 'static>(ns: &mut NamespaceBuilder<'_, N>) {
    ns.register("precache_pic", |_name: S, _flags: f32| -> Result<(), _> {
        unimplemented_builtin!("precache_pic", 317)
    })
    .register("iscachedpic", |_name: S| Ok(0f32))
    .register("drawgetimagesize", |_name: S| -> Result<(), _> {
        unimplemented_builtin!("drawgetimagesize", 318)
    })
    .register(
        "drawpic",
        |_pos: Vec3Arg,
         _name: S,
         _size: Vec3Arg,
         _rgb: Vec3Arg,
         _alpha: f32,
         _flag: f32|
         -> Result<(), _> { unimplemented_builtin!("drawpic", 322) },
    )
    .register(
        "drawsubpic",
        |_pos: Vec3Arg,
         _size: Vec3Arg,
         _name: S,
         _src_pos: Vec3Arg,
         _src_size: Vec3Arg,
         _rgb: Vec3Arg,
         _alpha: f32,
         _flag: f32|
         -> Result<(), _> { unimplemented_builtin!("drawsubpic", 328) },
    )
    .register("drawfill", |pos: Vec3Arg, size: Vec3Arg, rgb: Vec3Arg, alpha: f32, flag: f32| {
        check_draw_flag(flag);
        with_vm_ctx(|ctx| {
            push(
                ctx.world,
                DrawCmd::Fill {
                    pos: Vec2::new(pos[0], pos[1]),
                    size: Vec2::new(size[0], size[1]),
                    color: color(rgb, alpha),
                },
            );
            Ok(1.)
        })
    })
    .register(
        "drawcharacter",
        |pos: Vec3Arg, character: f32, size: Vec3Arg, rgb: Vec3Arg, alpha: f32, flag: f32| {
            check_draw_flag(flag);
            let byte = character as i64 as u8;
            with_vm_ctx(|ctx| {
                if byte != 0 {
                    for cmd in string_cmds(
                        Vec2::new(pos[0], pos[1]),
                        &[byte],
                        glyph_size(size),
                        rgb,
                        alpha,
                        false,
                    ) {
                        push(ctx.world, cmd);
                    }
                }
                Ok(1.)
            })
        },
    )
    .register(
        "drawrawstring",
        |pos: Vec3Arg, text: S, size: Vec3Arg, rgb: Vec3Arg, alpha: f32, flag: f32| {
            check_draw_flag(flag);
            with_vm_ctx(|ctx| {
                for cmd in string_cmds(
                    Vec2::new(pos[0], pos[1]),
                    string(&text),
                    glyph_size(size),
                    rgb,
                    alpha,
                    false,
                ) {
                    push(ctx.world, cmd);
                }
                Ok(1.)
            })
        },
    )
    .register(
        "drawstring",
        |pos: Vec3Arg, text: S, size: Vec3Arg, rgb: Vec3Arg, alpha: f32, flag: f32| {
            check_draw_flag(flag);
            with_vm_ctx(|ctx| {
                for cmd in string_cmds(
                    Vec2::new(pos[0], pos[1]),
                    string(&text),
                    glyph_size(size),
                    rgb,
                    alpha,
                    true,
                ) {
                    push(ctx.world, cmd);
                }
                Ok(1.)
            })
        },
    )
    .register("stringwidth", |text: S, use_colours: f32, size: Vec3Arg| {
        let size =
            if builtin_argc() > 2 { glyph_size(size) } else { Vec2::splat(DEFAULT_GLYPH_SIZE) };
        let text = string(&text);
        let len = if use_colours != 0. { visible_len(text) } else { text.len() };
        len as f32 * size.x
    })
    .register("drawsetcliparea", |x: f32, y: f32, width: f32, height: f32| {
        with_vm_ctx(|ctx| {
            push(
                ctx.world,
                DrawCmd::ClipArea { pos: Vec2::new(x, y), size: Vec2::new(width, height) },
            );
            Ok(())
        })
    })
    .register("drawresetcliparea", || {
        with_vm_ctx(|ctx| {
            push(ctx.world, DrawCmd::ResetClip);
            Ok(())
        })
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    fn texts(cmds: &[DrawCmd]) -> Vec<(f32, String, Color)> {
        cmds.iter()
            .map(|cmd| match cmd {
                DrawCmd::String { pos, text, color, .. } => {
                    (pos.x, text.to_str().into_owned(), *color)
                }
                other => panic!("{other:?}"),
            })
            .collect()
    }

    #[test]
    fn colour_codes_split_strings() {
        let white = [1., 1., 1.];
        let cmds = string_cmds(Vec2::ZERO, b"ab^1cd^7e", Vec2::splat(8.), white, 1., true);
        assert_eq!(
            texts(&cmds),
            [
                (0., "ab".to_owned(), Color::srgba(1., 1., 1., 1.)),
                (16., "cd".to_owned(), Color::srgba(1., 0., 0., 1.)),
                (32., "e".to_owned(), Color::srgba(1., 1., 1., 1.)),
            ]
        );

        let raw = string_cmds(Vec2::ZERO, b"^1x", Vec2::splat(8.), white, 0.5, false);
        assert_eq!(texts(&raw), [(0., "^1x".to_owned(), Color::srgba(1., 1., 1., 0.5))]);
    }
}
