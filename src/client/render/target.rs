// Copyright © 2020 Cormac O'Brien.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

use std::cell::RefCell;

use bevy::{
    core_pipeline::{core_3d::Camera3d, prepass::ViewPrepassTextures},
    render::{
        render_graph::{RenderLabel, ViewNode},
        render_phase::TrackedRenderPass,
        render_resource::{RenderPassColorAttachment, Texture, TextureView},
        renderer::{RenderDevice, RenderQueue},
        view::ViewTarget,
    },
};
use bumpalo::Bump;

use crate::client::render::{
    GraphicsState, RenderConnectionKind, RenderResolution, RenderState, RenderVars,
    world::WorldRenderer,
};

/// Intermediate object that can generate `RenderPassDescriptor`s.
pub struct RenderPassBuilder<'a> {
    color_attachments: Vec<Option<wgpu::RenderPassColorAttachment<'a>>>,
    depth_attachment: Option<wgpu::RenderPassDepthStencilAttachment<'a>>,
}

impl<'a> RenderPassBuilder<'a> {
    pub fn descriptor(&self) -> wgpu::RenderPassDescriptor<'_> {
        wgpu::RenderPassDescriptor {
            label: None,
            color_attachments: &self.color_attachments,
            depth_stencil_attachment: self.depth_attachment.clone(),
            timestamp_writes: Default::default(),
            occlusion_query_set: Default::default(),
        }
    }
}

/// A trait describing a render target.
///
/// A render target consists of a series of color attachments and an optional depth-stencil
/// attachment.
pub trait RenderTarget {
    fn render_pass_builder(&self) -> RenderPassBuilder<'_>;
}

impl<T> RenderTarget for &'_ T
where
    T: RenderTarget,
{
    fn render_pass_builder(&self) -> RenderPassBuilder<'_> {
        (**self).render_pass_builder()
    }
}

pub trait PreferredFormat {
    fn preferred_format(&self) -> wgpu::TextureFormat;
}

/// A trait describing a render target with a built-in resolve attachment.
pub trait RenderTargetResolve: RenderTarget {
    fn resolve_attachment(&self) -> &Texture;
    fn resolve_view(&self) -> &TextureView;
}

// TODO: use ArrayVec<TextureView> in concrete types so it can be passed
// as Cow::Borrowed in RenderPassDescriptor

#[derive(Debug, Hash, PartialEq, Eq, Clone, RenderLabel)]
pub struct InitPassLabel;

#[derive(Default)]
pub struct InitPass;

impl ViewNode for InitPass {
    type ViewQuery = (
        &'static ViewTarget,
        &'static ViewPrepassTextures,
        &'static Camera3d,
    );

    fn run<'w>(
        &self,
        _graph: &mut bevy::render::render_graph::RenderGraphContext,
        render_context: &mut bevy::render::renderer::RenderContext<'w>,
        (target, prepass, _): (&ViewTarget, &ViewPrepassTextures, &Camera3d),
        world: &'w bevy::prelude::World,
    ) -> Result<(), bevy::render::render_graph::NodeRunError> {
        let gfx_state = world.resource::<GraphicsState>();
        let queue = world.resource::<RenderQueue>();
        let device = world.resource::<RenderDevice>();
        let render_state = world.get_resource::<RenderState>();
        let world_renderer = world.get_resource::<WorldRenderer>();
        let &RenderResolution(width, height) = world.resource::<RenderResolution>();
        let render_vars = world.resource::<RenderVars>();

        let diffuse_target = target.get_unsampled_color_attachment().view;
        let ViewPrepassTextures {
            normal: Some(normal_target),
            depth: Some(depth_target),
            ..
        } = prepass
        else {
            return Ok(());
        };

        let normal_target = normal_target.get_unsampled_attachment();
        let RenderPassColorAttachment {
            view: depth_target, ..
        } = depth_target.get_unsampled_attachment();

        // TODO: Remove this
        thread_local! {
            static BUMP: RefCell<Bump> =Bump::new().into();
        }

        let encoder = render_context.command_encoder();

        BUMP.with_borrow_mut(|bump| bump.reset());
        BUMP.with_borrow(|bump| {
            if let (
                Some(RenderState {
                    state: cl_state,
                    kind,
                }),
                Some(world),
            ) = (render_state, world_renderer)
            {
                // if client is fully connected, draw world
                let camera = match kind {
                    RenderConnectionKind::Demo => {
                        cl_state.demo_camera(width as f32 / height as f32, render_vars.fov)
                    }
                    RenderConnectionKind::Server => {
                        cl_state.camera(width as f32 / height as f32, render_vars.fov)
                    }
                };

                // initial render pass
                {
                    let lightstyle_values = cl_state.lightstyle_values();
                    world.update_uniform_buffers(
                        gfx_state,
                        queue,
                        &camera,
                        cl_state.time(),
                        cl_state.iter_visible_entities(),
                        &lightstyle_values,
                        render_vars,
                    );

                    let mut init_pass = TrackedRenderPass::new(
                        device,
                        encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
                            label: Some("Initial pass"),
                            color_attachments: &[
                                Some(wgpu::RenderPassColorAttachment {
                                    view: diffuse_target,
                                    resolve_target: None,
                                    ops: wgpu::Operations {
                                        load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                                        store: wgpu::StoreOp::Store,
                                    },
                                }),
                                Some(normal_target),
                            ],
                            depth_stencil_attachment: Some(
                                wgpu::RenderPassDepthStencilAttachment {
                                    view: depth_target,
                                    depth_ops: Some(wgpu::Operations {
                                        load: wgpu::LoadOp::Clear(1.0),
                                        store: wgpu::StoreOp::Store,
                                    }),
                                    stencil_ops: None,
                                },
                            ),
                            timestamp_writes: Default::default(),
                            occlusion_query_set: Default::default(),
                        }),
                    );

                    world.render_pass(
                        gfx_state,
                        &mut init_pass,
                        bump,
                        &camera,
                        cl_state.time(),
                        cl_state.iter_visible_entities(),
                        cl_state.iter_particles(),
                        if cl_state.intermission().is_none() {
                            Some(cl_state.viewmodel_id())
                        } else {
                            None
                        },
                    );
                }
            }
        });

        Ok(())
    }
}
