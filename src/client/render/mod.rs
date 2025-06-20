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

/// Rendering functionality.
///
/// # Pipeline stages
///
/// The current rendering implementation consists of the following stages:
/// - Initial geometry pass
///   - Inputs:
///     - `AliasPipeline`
///     - `BrushPipeline`
///     - `SpritePipeline`
///   - Output: `InitialPassTarget`
/// - Deferred lighting pass
///   - Inputs:
///     - `DeferredPipeline`
///     - `QuadPipeline`
///     - `GlyphPipeline`
///   - Output: `DeferredPassTarget`
/// - Final pass
///   - Inputs:
///     - `PostProcessPipeline`
///   - Output: `FinalPassTarget`
/// - Blit to swap chain
///   - Inputs:
///     - `BlitPipeline`
///   - Output: `SwapChainTarget`
mod cvars;
mod error;
pub mod palette;
mod pipeline;
mod target;
mod ui;
mod uniform;
mod warp;
mod world;

use beef::Cow;
use bevy::{
    core_pipeline::{
        core_3d::graph::{Core3d, Node3d},
        prepass::NORMAL_PREPASS_FORMAT,
    },
    ecs::{
        schedule::ScheduleLabel,
        system::{Res, Resource},
    },
    prelude::*,
    render::{
        Render, RenderApp, RenderSet,
        extract_resource::{ExtractResource, ExtractResourcePlugin},
        render_graph::{RenderGraphApp, ViewNodeRunner},
        render_resource::{
            BindGroup, BindGroupLayout, Buffer, Sampler, SpecializedRenderPipelines, Texture,
            TextureView,
        },
        renderer::{RenderDevice, RenderQueue},
        view::ViewTarget,
    },
    ui::graph::NodeUi,
    window::PrimaryWindow,
};
pub use cvars::register_cvars;
pub use error::{RenderError, RenderErrorKind};
pub use palette::Palette;
use parking_lot::RwLock;
pub use pipeline::Pipeline;
pub use postprocess::PostProcessBindGroup;
use serde::{Deserialize, Serialize};
pub use target::{PreferredFormat, RenderTarget, RenderTargetResolve};
pub use ui::{UiRenderer, UiState, hud::HudState};
pub use world::{
    Camera,
    deferred::{DeferredRenderer, DeferredUniforms, PointLight},
};

use std::{
    cell::RefCell,
    mem::size_of,
    num::NonZeroU64,
    ops::{Deref, DerefMut},
};

use crate::{
    client::{
        input::InputFocus,
        menu::Menu,
        render::{
            ui::{glyph::GlyphPipeline, hud::HudVars, quad::QuadPipeline},
            uniform::DynamicUniformBuffer,
            world::{
                EntityUniforms,
                alias::AliasPipeline,
                brush::BrushPipeline,
                deferred::DeferredPipeline,
                particle::ParticlePipeline,
                postprocess::{self, PostProcessPipeline, PostProcessVars},
                sprite::SpritePipeline,
            },
        },
    },
    common::{console::Registry, vfs::Vfs, wad::Wad},
};

use self::{
    target::{InitPass, InitPassLabel},
    ui::{UiPass, UiPassLabel},
    world::{
        deferred::{DeferredPass, DeferredPassLabel},
        extract_world_renderer,
        postprocess::{PostProcessPass, PostProcessPassLabel},
    },
};

use failure::Error;

use super::{Connection, ConnectionKind, ConnectionState, state::ClientState};

pub struct SeismonRenderPlugin;

fn extract_now<U: Resource, V: ExtractResource<Source = U>>(app: &mut App) {
    let res = V::extract_resource(app.world().resource::<U>());
    let Some(render_app) = app.get_sub_app_mut(RenderApp) else {
        return;
    };
    render_app.insert_resource(res);
}

impl Plugin for SeismonRenderPlugin {
    fn build(&self, app: &mut bevy::prelude::App) {
        #[derive(Hash, Debug, PartialEq, Eq, Copy, Clone, ScheduleLabel)]
        struct RenderSetup;

        app.add_plugins((
            ExtractResourcePlugin::<Menu>::default(),
            ExtractResourcePlugin::<RenderState>::default(),
            ExtractResourcePlugin::<InputFocus>::default(),
            ExtractResourcePlugin::<RenderVars>::default(),
            ExtractResourcePlugin::<HudVars>::default(),
            ExtractResourcePlugin::<PostProcessVars>::default(),
            ExtractResourcePlugin::<ConnectionState>::default(),
            // TODO: Do all loading on the main thread (this is currently just for the palette and gfx wad)
            ExtractResourcePlugin::<Vfs>::default(),
        ));

        register_cvars(app);

        extract_now::<Menu, Menu>(app);
        extract_now::<Vfs, Vfs>(app);
        extract_now::<ConnectionState, ConnectionState>(app);
    }

    fn finish(&self, app: &mut bevy::prelude::App) {
        extract_now::<RenderResolution, RenderResolution>(app);

        let Some(render_app) = app.get_sub_app_mut(RenderApp) else {
            return;
        };

        render_app
            .init_resource::<PostProcessPipeline>()
            .init_resource::<SpecializedRenderPipelines<PostProcessPipeline>>()
            .add_systems(
                Render,
                (
                    systems::create_graphics_state.run_if(
                        not(resource_exists::<GraphicsState>)
                            .or_else(resource_changed::<RenderResolution>),
                    ),
                    systems::create_menu_renderer.run_if(
                        resource_exists::<GraphicsState>.and_then(
                            not(resource_exists::<UiRenderer>).or_else(resource_changed::<Menu>),
                        ),
                    ),
                    extract_world_renderer.run_if(
                        resource_changed::<ConnectionState>
                            .and_then(resource_exists::<GraphicsState>),
                    ),
                )
                    .chain()
                    .in_set(RenderSet::Prepare),
            )
            .add_render_graph_node::<ViewNodeRunner<InitPass>>(Core3d, InitPassLabel)
            .add_render_graph_node::<ViewNodeRunner<DeferredPass>>(Core3d, DeferredPassLabel)
            .add_render_graph_node::<ViewNodeRunner<PostProcessPass>>(Core3d, PostProcessPassLabel)
            .add_render_graph_node::<ViewNodeRunner<UiPass>>(Core3d, UiPassLabel)
            .add_render_graph_edges(
                Core3d,
                (
                    Node3d::MainOpaquePass,
                    InitPassLabel,
                    DeferredPassLabel,
                    PostProcessPassLabel,
                    Node3d::EndMainPass,
                ),
            )
            .add_render_graph_edges(Core3d, (NodeUi::UiPass, UiPassLabel, Node3d::Upscaling));
    }
}

const DIFFUSE_TEXTURE_FORMAT: wgpu::TextureFormat = wgpu::TextureFormat::Rgba8UnormSrgb;
const FULLBRIGHT_TEXTURE_FORMAT: wgpu::TextureFormat = wgpu::TextureFormat::R8Unorm;
const LIGHTMAP_TEXTURE_FORMAT: wgpu::TextureFormat = wgpu::TextureFormat::R8Unorm;

/// Create a `wgpu::TextureDescriptor` appropriate for the provided texture data.
pub fn texture_descriptor<'a>(
    label: Option<&'a str>,
    width: u32,
    height: u32,
    format: wgpu::TextureFormat,
) -> wgpu::TextureDescriptor<'a> {
    wgpu::TextureDescriptor {
        label,
        size: wgpu::Extent3d {
            width,
            height,
            depth_or_array_layers: 1,
        },
        mip_level_count: 1,
        sample_count: 1,
        dimension: wgpu::TextureDimension::D2,
        format,
        usage: wgpu::TextureUsages::COPY_DST | wgpu::TextureUsages::TEXTURE_BINDING,
        view_formats: Default::default(),
    }
}

pub fn create_texture<'a>(
    device: &RenderDevice,
    queue: &RenderQueue,
    label: Option<&'a str>,
    width: u32,
    height: u32,
    data: &TextureData,
) -> Texture {
    trace!(
        "Creating texture ({:?}: {}x{})",
        data.format(),
        width,
        height
    );

    // It looks like sometimes quake includes textures with at least one zero aspect?
    let texture = device.create_texture(&texture_descriptor(
        label,
        width.max(1),
        height.max(1),
        data.format(),
    ));
    queue.write_texture(
        wgpu::ImageCopyTexture {
            texture: &texture,
            mip_level: 0,
            origin: wgpu::Origin3d::ZERO,
            aspect: Default::default(),
        },
        data.data(),
        wgpu::ImageDataLayout {
            offset: 0,
            bytes_per_row: Some(width * data.stride()),
            rows_per_image: None,
        },
        wgpu::Extent3d {
            width,
            height,
            depth_or_array_layers: 1,
        },
    );

    texture
}

pub struct DiffuseData<'a> {
    pub rgba: Cow<'a, [u8]>,
}

pub struct FullbrightData<'a> {
    pub fullbright: Cow<'a, [u8]>,
}

pub struct LightmapData<'a> {
    pub lightmap: Cow<'a, [u8]>,
}

pub enum TextureData<'a> {
    Diffuse(DiffuseData<'a>),
    Fullbright(FullbrightData<'a>),
    Lightmap(LightmapData<'a>),
}

impl<'a> TextureData<'a> {
    pub fn format(&self) -> wgpu::TextureFormat {
        match self {
            TextureData::Diffuse(_) => DIFFUSE_TEXTURE_FORMAT,
            TextureData::Fullbright(_) => FULLBRIGHT_TEXTURE_FORMAT,
            TextureData::Lightmap(_) => LIGHTMAP_TEXTURE_FORMAT,
        }
    }

    pub fn data(&self) -> &[u8] {
        match self {
            TextureData::Diffuse(d) => &d.rgba,
            TextureData::Fullbright(d) => &d.fullbright,
            TextureData::Lightmap(d) => &d.lightmap,
        }
    }

    pub fn stride(&self) -> u32 {
        use std::mem;
        use wgpu::TextureFormat::*;

        (match self.format() {
            Rg8Unorm | Rg8Snorm | Rg8Uint | Rg8Sint => mem::size_of::<[u8; 2]>(),
            R8Unorm | R8Snorm | R8Uint | R8Sint => mem::size_of::<u8>(),
            Bgra8Unorm | Bgra8UnormSrgb | Rgba8Unorm | Rgba8UnormSrgb => mem::size_of::<[u8; 4]>(),
            R16Uint | R16Sint | R16Unorm | R16Snorm | R16Float => mem::size_of::<u16>(),
            Rg16Uint | Rg16Sint | Rg16Unorm | Rg16Snorm | Rg16Float => mem::size_of::<[u16; 2]>(),
            Rgba16Uint | Rgba16Sint | Rgba16Unorm | Rgba16Snorm | Rgba16Float => {
                mem::size_of::<[u16; 4]>()
            }
            _ => todo!(),
        }) as u32
    }

    pub fn size(&self) -> wgpu::BufferAddress {
        self.data().len() as wgpu::BufferAddress
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Extent2d {
    pub width: u32,
    pub height: u32,
}

impl std::convert::Into<wgpu::Extent3d> for Extent2d {
    fn into(self) -> wgpu::Extent3d {
        wgpu::Extent3d {
            width: self.width,
            height: self.height,
            depth_or_array_layers: 1,
        }
    }
}

impl std::convert::From<winit::dpi::PhysicalSize<u32>> for Extent2d {
    fn from(other: winit::dpi::PhysicalSize<u32>) -> Extent2d {
        let winit::dpi::PhysicalSize { width, height } = other;
        Extent2d { width, height }
    }
}

#[derive(Resource, ExtractResource, PartialEq, Eq, Clone, Copy)]
pub struct RenderResolution(pub u32, pub u32);

impl FromWorld for RenderResolution {
    fn from_world(world: &mut World) -> Self {
        let res = &world
            .query_filtered::<&Window, With<PrimaryWindow>>()
            .single(world)
            .resolution;

        RenderResolution(res.width() as _, res.height() as _)
    }
}

pub enum RenderConnectionKind {
    Server,
    Demo,
}

#[derive(Resource)]
pub struct RenderState {
    // TODO: Make a stripped-down version of this
    state: ClientState,
    kind: RenderConnectionKind,
}

impl ExtractResource for RenderState {
    type Source = Connection;

    fn extract_resource(source: &Self::Source) -> Self {
        let Connection { state, kind } = source;

        RenderState {
            state: state.clone(),
            kind: match kind {
                ConnectionKind::Server { .. } => RenderConnectionKind::Server,
                ConnectionKind::Demo(_) => RenderConnectionKind::Demo,
            },
        }
    }
}

#[derive(Resource)]
pub struct GraphicsState {
    world_bind_group_layouts: Vec<BindGroupLayout>,
    world_bind_groups: Vec<BindGroup>,

    frame_uniform_buffer: Buffer,

    // TODO: This probably doesn't need to be a rwlock
    entity_uniform_buffer: RwLock<DynamicUniformBuffer<EntityUniforms>>,

    diffuse_sampler: Sampler,
    nearest_sampler: Sampler,
    lightmap_sampler: Sampler,

    alias_pipeline: AliasPipeline,
    brush_pipeline: BrushPipeline,
    sprite_pipeline: SpritePipeline,
    deferred_pipeline: DeferredPipeline,
    particle_pipeline: ParticlePipeline,
    glyph_pipeline: GlyphPipeline,
    quad_pipeline: QuadPipeline,

    default_lightmap: Texture,
    default_lightmap_view: TextureView,

    palette: Palette,
    gfx_wad: Wad,
}

thread_local! {
    static COMPILER: RefCell<shaderc::Compiler> = shaderc::Compiler::new().unwrap().into();
}

impl GraphicsState {
    pub fn new(
        device: &RenderDevice,
        queue: &RenderQueue,
        view_target: &ViewTarget,
        sample_count: u32,
        vfs: &Vfs,
    ) -> Result<GraphicsState, Error> {
        let diffuse_format = view_target.main_texture_format();
        let normal_format = NORMAL_PREPASS_FORMAT;

        let palette = Palette::load(&vfs, "gfx/palette.lmp");
        let gfx_wad = Wad::load(vfs.open("gfx.wad")?).unwrap();

        let frame_uniform_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("frame uniform buffer"),
            size: size_of::<world::FrameUniforms>() as wgpu::BufferAddress,
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });
        let entity_uniform_buffer = DynamicUniformBuffer::new(device);

        let diffuse_sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: None,
            address_mode_u: wgpu::AddressMode::Repeat,
            address_mode_v: wgpu::AddressMode::Repeat,
            address_mode_w: wgpu::AddressMode::Repeat,
            mag_filter: wgpu::FilterMode::Nearest,
            min_filter: wgpu::FilterMode::Linear,
            mipmap_filter: wgpu::FilterMode::Linear,
            // TODO: these are the OpenGL defaults; see if there's a better choice for us
            lod_max_clamp: 1000.0,
            compare: None,
            ..Default::default()
        });

        let nearest_sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: None,
            address_mode_u: wgpu::AddressMode::Repeat,
            address_mode_v: wgpu::AddressMode::Repeat,
            address_mode_w: wgpu::AddressMode::Repeat,
            mag_filter: wgpu::FilterMode::Nearest,
            min_filter: wgpu::FilterMode::Nearest,
            mipmap_filter: wgpu::FilterMode::Nearest,
            // TODO: these are the OpenGL defaults; see if there's a better choice for us
            lod_max_clamp: 1000.0,
            compare: None,
            ..Default::default()
        });

        let lightmap_sampler = device.create_sampler(&wgpu::SamplerDescriptor {
            label: None,
            address_mode_u: wgpu::AddressMode::ClampToEdge,
            address_mode_v: wgpu::AddressMode::ClampToEdge,
            address_mode_w: wgpu::AddressMode::ClampToEdge,
            mag_filter: wgpu::FilterMode::Linear,
            min_filter: wgpu::FilterMode::Linear,
            mipmap_filter: wgpu::FilterMode::Linear,
            // TODO: these are the OpenGL defaults; see if there's a better choice for us
            lod_max_clamp: 1000.0,
            compare: None,
            ..Default::default()
        });

        let world_bind_group_layouts: Vec<BindGroupLayout> = world::BIND_GROUP_LAYOUT_DESCRIPTORS
            .iter()
            .map(|desc| device.create_bind_group_layout(None, desc))
            .collect();
        let world_bind_groups = vec![
            device.create_bind_group(
                Some("per-frame bind group"),
                &world_bind_group_layouts[world::BindGroupLayoutId::PerFrame as usize],
                &[wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::Buffer(wgpu::BufferBinding {
                        buffer: &frame_uniform_buffer,
                        offset: 0,
                        size: None,
                    }),
                }],
            ),
            device.create_bind_group(
                Some("brush per-entity bind group"),
                &world_bind_group_layouts[world::BindGroupLayoutId::PerEntity as usize],
                &[
                    wgpu::BindGroupEntry {
                        binding: 0,
                        resource: wgpu::BindingResource::Buffer(wgpu::BufferBinding {
                            buffer: &entity_uniform_buffer.buffer(),
                            offset: 0,
                            size: Some(
                                NonZeroU64::new(size_of::<EntityUniforms>() as u64).unwrap(),
                            ),
                        }),
                    },
                    wgpu::BindGroupEntry {
                        binding: 1,
                        resource: wgpu::BindingResource::Sampler(&diffuse_sampler),
                    },
                    wgpu::BindGroupEntry {
                        binding: 2,
                        resource: wgpu::BindingResource::Sampler(&lightmap_sampler),
                    },
                ],
            ),
        ];

        let (
            alias_pipeline,
            brush_pipeline,
            sprite_pipeline,
            deferred_pipeline,
            particle_pipeline,
            quad_pipeline,
            glyph_pipeline,
        ) = COMPILER.with_borrow_mut(|compiler| {
            let alias_pipeline = AliasPipeline::new(
                device,
                compiler,
                &world_bind_group_layouts,
                diffuse_format,
                normal_format,
                sample_count,
            );
            let brush_pipeline = BrushPipeline::new(
                device,
                compiler,
                &world_bind_group_layouts,
                diffuse_format,
                normal_format,
                sample_count,
            );
            let sprite_pipeline = SpritePipeline::new(
                device,
                compiler,
                &world_bind_group_layouts,
                diffuse_format,
                normal_format,
                sample_count,
            );
            let particle_pipeline = ParticlePipeline::new(
                device,
                &queue,
                compiler,
                diffuse_format,
                normal_format,
                sample_count,
                &palette,
            );
            let deferred_pipeline =
                DeferredPipeline::new(device, compiler, diffuse_format, sample_count);
            let quad_pipeline = QuadPipeline::new(device, compiler, diffuse_format, sample_count);
            let glyph_pipeline = GlyphPipeline::new(device, compiler, diffuse_format, sample_count);

            (
                alias_pipeline,
                brush_pipeline,
                sprite_pipeline,
                deferred_pipeline,
                particle_pipeline,
                quad_pipeline,
                glyph_pipeline,
            )
        });

        let default_lightmap = create_texture(
            device,
            queue,
            None,
            1,
            1,
            &TextureData::Lightmap(LightmapData {
                lightmap: (&[0xFF][..]).into(),
            }),
        );
        let default_lightmap_view = default_lightmap.create_view(&Default::default());

        Ok(GraphicsState {
            frame_uniform_buffer,
            entity_uniform_buffer: entity_uniform_buffer.into(),

            world_bind_group_layouts,
            world_bind_groups,

            alias_pipeline,
            brush_pipeline,
            sprite_pipeline,
            deferred_pipeline,
            particle_pipeline,
            glyph_pipeline,
            quad_pipeline,

            diffuse_sampler,
            nearest_sampler,
            lightmap_sampler,

            default_lightmap,
            default_lightmap_view,
            palette,
            gfx_wad,
        })
    }

    pub fn create_texture<'a>(
        &self,
        device: &RenderDevice,
        queue: &RenderQueue,
        label: Option<&'a str>,
        width: u32,
        height: u32,
        data: &TextureData,
    ) -> Texture {
        create_texture(device, queue, label, width, height, data)
    }

    pub fn frame_uniform_buffer(&self) -> &Buffer {
        &self.frame_uniform_buffer
    }

    pub fn entity_uniform_buffer(
        &self,
    ) -> impl Deref<Target = DynamicUniformBuffer<EntityUniforms>> + '_ {
        self.entity_uniform_buffer.read()
    }

    pub fn entity_uniform_buffer_mut(
        &self,
    ) -> impl DerefMut<Target = DynamicUniformBuffer<EntityUniforms>> + '_ {
        self.entity_uniform_buffer.write()
    }

    pub fn diffuse_sampler(&self) -> &Sampler {
        &self.diffuse_sampler
    }

    pub fn nearest_sampler(&self) -> &Sampler {
        &self.nearest_sampler
    }

    pub fn default_lightmap(&self) -> &Texture {
        &self.default_lightmap
    }

    pub fn default_lightmap_view(&self) -> &TextureView {
        &self.default_lightmap_view
    }

    pub fn lightmap_sampler(&self) -> &Sampler {
        &self.lightmap_sampler
    }

    pub fn world_bind_group_layouts(&self) -> &[BindGroupLayout] {
        &self.world_bind_group_layouts
    }

    pub fn world_bind_groups(&self) -> &[BindGroup] {
        &self.world_bind_groups
    }

    // pipelines

    pub fn alias_pipeline(&self) -> &AliasPipeline {
        &self.alias_pipeline
    }

    pub fn brush_pipeline(&self) -> &BrushPipeline {
        &self.brush_pipeline
    }

    pub fn sprite_pipeline(&self) -> &SpritePipeline {
        &self.sprite_pipeline
    }

    pub fn deferred_pipeline(&self) -> &DeferredPipeline {
        &self.deferred_pipeline
    }

    pub fn particle_pipeline(&self) -> &ParticlePipeline {
        &self.particle_pipeline
    }

    pub fn glyph_pipeline(&self) -> &GlyphPipeline {
        &self.glyph_pipeline
    }

    pub fn quad_pipeline(&self) -> &QuadPipeline {
        &self.quad_pipeline
    }

    pub fn palette(&self) -> &Palette {
        &self.palette
    }

    pub fn gfx_wad(&self) -> &Wad {
        &self.gfx_wad
    }
}

#[derive(Debug, Resource, Deserialize)]
pub struct RenderVars {
    pub fov: f32,
    #[serde(rename(deserialize = "r_lightmap"))]
    pub lightmap: u8,
    #[serde(rename(deserialize = "r_sky_scollspeed"))]
    pub sky_scroll_speed: f32,
    #[serde(rename(deserialize = "r_msaa_samples"))]
    pub msaa_samples: u32,
}

impl Default for RenderVars {
    fn default() -> Self {
        Self {
            fov: 90.,
            lightmap: 0,
            sky_scroll_speed: 32.,
            msaa_samples: 1,
        }
    }
}

impl ExtractResource for RenderVars {
    type Source = Registry;

    fn extract_resource(source: &Self::Source) -> Self {
        source.read_cvars().unwrap_or_default()
    }
}

mod systems {
    use super::*;

    pub fn create_graphics_state(
        targets: Query<&ViewTarget, With<Camera3d>>,
        mut commands: Commands,
        device: Res<RenderDevice>,
        queue: Res<RenderQueue>,
        vfs: Res<Vfs>,
        render_vars: Res<RenderVars>,
    ) {
        let sample_count = render_vars.msaa_samples;

        if let Ok(view_target) = targets.get_single() {
            match GraphicsState::new(&*device, &*queue, view_target, sample_count, &*vfs) {
                Ok(state) => {
                    commands.insert_resource(state);
                }
                Err(e) => {
                    warn!("Failed to create graphics state: {}", e);
                }
            }
        }
    }

    pub fn create_menu_renderer(
        mut commands: Commands,
        state: Option<Res<GraphicsState>>,
        vfs: Res<Vfs>,
        device: Res<RenderDevice>,
        queue: Res<RenderQueue>,
        menu: Res<Menu>,
    ) {
        if let Some(state) = state.as_ref() {
            commands.insert_resource(UiRenderer::new(&*state, &*vfs, &*device, &*queue, &*menu));
        }
    }
}
