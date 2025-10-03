pub mod glyph;
pub mod hud;
pub mod layout;
pub mod menu;
pub mod quad;

use crate::{
    client::{
        input::InputFocus,
        menu::Menu,
        render::{
            Extent2d, GraphicsState,
            ui::{
                glyph::{GlyphRenderer, GlyphRendererCommand},
                hud::{HudRenderer, HudState},
                menu::MenuRenderer,
                quad::{QuadRenderer, QuadRendererCommand},
            },
        },
    },
    common::vfs::Vfs,
};

use bevy::{
    prelude::*,
    render::{
        render_graph::{RenderLabel, ViewNode},
        render_phase::TrackedRenderPass,
        renderer::{RenderDevice, RenderQueue},
        view::ViewTarget,
    },
};
use chrono::Duration;

use self::hud::HudVars;

use super::{RenderResolution, RenderState};

pub fn screen_space_vertex_translate(
    display_w: u32,
    display_h: u32,
    pos_x: i32,
    pos_y: i32,
) -> Vec2 {
    // rescale from [0, DISPLAY_*] to [-1, 1] (NDC)
    Vec2::new(
        (pos_x * 2 - display_w as i32) as f32 / display_w as f32,
        (pos_y * 2 - display_h as i32) as f32 / display_h as f32,
    )
}

pub fn screen_space_vertex_scale(display_w: u32, display_h: u32, quad_w: u32, quad_h: u32) -> Vec2 {
    Vec2::new(
        (quad_w * 2) as f32 / display_w as f32,
        (quad_h * 2) as f32 / display_h as f32,
    )
}

pub fn screen_space_vertex_transform(
    display_w: u32,
    display_h: u32,
    quad_w: u32,
    quad_h: u32,
    pos_x: i32,
    pos_y: i32,
) -> Mat4 {
    let Vec2 { x: ndc_x, y: ndc_y } =
        screen_space_vertex_translate(display_w, display_h, pos_x, pos_y);

    let Vec2 {
        x: scale_x,
        y: scale_y,
    } = screen_space_vertex_scale(display_w, display_h, quad_w, quad_h);

    Mat4::from_translation([ndc_x, ndc_y, 0.0].into())
        * Mat4::from_scale(Vec3::new(scale_x, scale_y, 1.0))
}

pub enum UiState<'a> {
    Title {
        overlay: Option<&'a Menu>,
    },
    InGame {
        hud: HudState<'a>,
        overlay: Option<&'a Menu>,
    },
}

#[derive(Resource)]
pub struct UiRenderer {
    menu_renderer: MenuRenderer,
    hud_renderer: HudRenderer,
    glyph_renderer: GlyphRenderer,
    quad_renderer: QuadRenderer,
}

impl UiRenderer {
    pub fn new(
        state: &GraphicsState,
        vfs: &Vfs,
        device: &RenderDevice,
        queue: &RenderQueue,
        menu: &Menu,
    ) -> UiRenderer {
        UiRenderer {
            menu_renderer: MenuRenderer::new(state, vfs, device, queue, menu),
            hud_renderer: HudRenderer::new(state, vfs, device, queue),
            glyph_renderer: GlyphRenderer::new(state, device, queue),
            quad_renderer: QuadRenderer::new(state, device),
        }
    }

    // TODO: This is a mess, but we should get rid of the entire `render` module in favour of directly using Bevy functionality anyway.
    #[expect(clippy::too_many_arguments)]
    pub fn render_pass<'this, 'a>(
        &'this self,
        state: &'this GraphicsState,
        queue: &'a RenderQueue,
        pass: &'a mut TrackedRenderPass<'this>,
        target_size: Extent2d,
        time: Duration,
        ui_state: &'a UiState<'this>,
        hud_cvars: &'a HudVars,
        quad_commands: &'a mut Vec<QuadRendererCommand<'this>>,
        glyph_commands: &'a mut Vec<GlyphRendererCommand>,
    ) {
        let (hud_state, overlay) = match ui_state {
            UiState::Title { overlay } => (None, overlay.as_ref()),
            UiState::InGame { hud, overlay } => (Some(hud), overlay.as_ref()),
        };

        if let Some(hstate) = hud_state {
            self.hud_renderer.generate_commands(
                hstate,
                time,
                hud_cvars,
                quad_commands,
                glyph_commands,
            );
        }

        if let Some(menu) = overlay {
            self.menu_renderer
                .generate_commands(menu, time, quad_commands, glyph_commands);
        }

        self.quad_renderer
            .update_uniforms(state, queue, target_size, quad_commands);
        self.quad_renderer.record_draw(state, pass, quad_commands);
        self.glyph_renderer
            .record_draw(state, queue, pass, target_size, glyph_commands);
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, RenderLabel)]
pub struct UiPassLabel;

#[derive(Default)]
pub struct UiPass;

impl ViewNode for UiPass {
    type ViewQuery = (&'static ViewTarget, &'static Camera3d);

    fn run<'w>(
        &self,
        _graph: &mut bevy::render::render_graph::RenderGraphContext,
        render_context: &mut bevy::render::renderer::RenderContext<'w>,
        (view_target, _): (&ViewTarget, &Camera3d),
        world: &'w World,
    ) -> Result<(), bevy::render::render_graph::NodeRunError> {
        let gfx_state = world.resource::<GraphicsState>();
        let ui_renderer = world.resource::<UiRenderer>();
        let hud_cvars = world.resource::<HudVars>();
        let conn = world.get_resource::<RenderState>();
        let queue = world.resource::<RenderQueue>();
        let device = world.resource::<RenderDevice>();
        let Some(&RenderResolution(width, height)) = world.get_resource::<RenderResolution>()
        else {
            return Ok(());
        };
        let menu = world.get_resource::<Menu>();
        let focus = world.resource::<InputFocus>();

        let mut quad_commands = Vec::new();
        let mut glyph_commands = Vec::new();

        let encoder = render_context.command_encoder();
        let diffuse_target = view_target.get_unsampled_color_attachment();

        {
            let final_pass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                label: Some("Ui pass"),
                color_attachments: &[Some(diffuse_target)],
                depth_stencil_attachment: None,
                ..default()
            });

            let mut final_pass = TrackedRenderPass::new(device, final_pass);

            if let Some(RenderState { .. }) = conn {
                let ui_state = match conn {
                    Some(RenderState {
                        state: cl_state, ..
                    }) => UiState::InGame {
                        hud: match cl_state.intermission() {
                            Some(kind) => HudState::Intermission {
                                kind,
                                completion_duration: cl_state.completion_time().unwrap()
                                    - cl_state.start_time(),
                                stats: cl_state.stats(),
                            },

                            None => HudState::InGame {
                                items: cl_state.items(),
                                item_pickup_time: cl_state.item_pickup_times(),
                                stats: cl_state.stats(),
                                face_anim_time: cl_state.face_anim_time(),
                            },
                        },

                        overlay: match (focus, menu) {
                            (InputFocus::Game, _) => None,
                            (InputFocus::Menu, menu) => menu,
                            _ => None,
                        },
                    },

                    None => UiState::Title {
                        overlay: match (focus, menu) {
                            (InputFocus::Menu, menu) => menu,
                            (InputFocus::Game, _) => unreachable!(),
                            _ => return Ok(()),
                        },
                    },
                };

                let elapsed = conn.as_ref().map(|c| c.state.time).unwrap_or_default();
                ui_renderer.render_pass(
                    gfx_state,
                    queue,
                    &mut final_pass,
                    Extent2d { width, height },
                    // use client time when in game, renderer time otherwise
                    elapsed,
                    &ui_state,
                    hud_cvars,
                    &mut quad_commands,
                    &mut glyph_commands,
                );
            }

            Ok(())
        }
    }
}
