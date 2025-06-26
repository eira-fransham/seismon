use std::{mem::size_of, num::NonZeroU64};

use bevy::{
    core_pipeline::fullscreen_vertex_shader::fullscreen_shader_vertex_state,
    ecs::system::SystemState,
    prelude::*,
    render::{
        extract_resource::ExtractResource,
        render_graph::{RenderLabel, ViewNode},
        render_phase::TrackedRenderPass,
        render_resource::{
            BindGroup, BindGroupLayout, Buffer, CachedRenderPipelineId, FragmentState,
            PipelineCache, RenderPassDescriptor, RenderPipeline, RenderPipelineDescriptor,
            ShaderDefVal, SpecializedRenderPipeline, SpecializedRenderPipelines,
        },
        renderer::{RenderDevice, RenderQueue},
        view::{PostProcessWrite, ViewTarget},
    },
};
use serde::Deserialize;
use wgpu::{BindGroupLayoutEntry, BlendState, ColorTargetState, ColorWrites};

use crate::{
    client::render::{GraphicsState, RenderState, pipeline::Pipeline, ui::quad::QuadPipeline},
    common::{console::Registry, net::ColorShift, util::any_as_bytes},
};

#[repr(C, align(256))]
#[derive(Clone, Copy, Debug, Default)]
pub struct PostProcessUniforms {
    pub color_shift: [[f32; 4]; 4],
}

#[derive(Resource)]
pub struct PostProcessPipeline {
    uniform_buffer: Buffer,
    bind_group_layouts: Vec<BindGroupLayout>,
    shader: Handle<Shader>,
    sample_count: u32,
}

impl PostProcessPipeline {
    pub fn new(
        device: &RenderDevice,
        shader: Handle<Shader>,
        sample_count: u32,
    ) -> PostProcessPipeline {
        let bind_group_layouts = Self::bind_group_layout_descriptors()
            .into_iter()
            .map(|desc| device.create_bind_group_layout(Some(Self::name().into()), &*desc))
            .collect();
        let uniforms = PostProcessUniforms::default();
        let uniform_buffer = device.create_buffer_with_data(&wgpu::util::BufferInitDescriptor {
            label: None,
            contents: unsafe { any_as_bytes(&uniforms) },
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
        });

        PostProcessPipeline {
            uniform_buffer,
            shader,
            bind_group_layouts,
            sample_count,
        }
    }

    pub fn uniform_buffer(&self) -> &wgpu::Buffer {
        &self.uniform_buffer
    }
}

impl FromWorld for PostProcessPipeline {
    fn from_world(world: &mut World) -> Self {
        let device = world.resource::<RenderDevice>();
        let shader = world
            .resource::<AssetServer>()
            .load("shaders/postprocess.wgsl");

        Self::new(device, shader, 1)
    }
}

impl SpecializedRenderPipeline for PostProcessPipeline {
    type Key = PostProcessVars;

    fn specialize(&self, key: Self::Key) -> RenderPipelineDescriptor {
        RenderPipelineDescriptor {
            label: Some(Self::name().into()),
            layout: self.bind_group_layouts.clone(),
            push_constant_ranges: Self::push_constant_ranges(),
            vertex: fullscreen_shader_vertex_state(),
            fragment: Some(FragmentState {
                shader: self.shader.clone(),
                shader_defs: vec![
                    ShaderDefVal::UInt("COLORSPACE".into(), key.color_space as _),
                    ShaderDefVal::UInt("BLENDMODE".into(), key.blend_mode as _),
                ],
                entry_point: "main".into(),
                targets: vec![Some(ColorTargetState {
                    format: ViewTarget::TEXTURE_FORMAT_HDR,
                    blend: Some(BlendState::REPLACE),
                    write_mask: ColorWrites::ALL,
                })],
            }),
            primitive: default(),
            depth_stencil: Self::depth_stencil_state(),
            multisample: wgpu::MultisampleState {
                count: self.sample_count,
                mask: !0,
                alpha_to_coverage_enabled: false,
            },
            zero_initialize_workgroup_memory: false,
        }
    }
}

const BIND_GROUP_LAYOUT_ENTRIES: &[wgpu::BindGroupLayoutEntry] = &[
    // color buffer
    wgpu::BindGroupLayoutEntry {
        binding: 0,
        visibility: wgpu::ShaderStages::FRAGMENT,
        ty: wgpu::BindingType::Texture {
            view_dimension: wgpu::TextureViewDimension::D2,
            sample_type: wgpu::TextureSampleType::Float { filterable: true },
            multisampled: false,
        },
        count: None,
    },
    // sampler
    wgpu::BindGroupLayoutEntry {
        binding: 1,
        visibility: wgpu::ShaderStages::FRAGMENT,
        ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
        count: None,
    },
    // PostProcessUniforms
    wgpu::BindGroupLayoutEntry {
        binding: 2,
        visibility: wgpu::ShaderStages::FRAGMENT,
        ty: wgpu::BindingType::Buffer {
            ty: wgpu::BufferBindingType::Uniform,
            has_dynamic_offset: false,
            min_binding_size: NonZeroU64::new(size_of::<PostProcessUniforms>() as u64),
        },
        count: None,
    },
];

impl Pipeline for PostProcessPipeline {
    type VertexPushConstants = ();
    type SharedPushConstants = ();
    type FragmentPushConstants = ();

    type Args = wgpu::TextureFormat;

    fn name() -> &'static str {
        "postprocess"
    }

    fn bind_group_layout_descriptors() -> Vec<Vec<BindGroupLayoutEntry>> {
        vec![BIND_GROUP_LAYOUT_ENTRIES.to_owned()]
    }

    fn vertex_shader() -> &'static str {
        unreachable!()
    }

    fn fragment_shader() -> &'static str {
        unreachable!()
    }

    fn primitive_state() -> wgpu::PrimitiveState {
        QuadPipeline::primitive_state()
    }

    fn color_target_states_with_args(args: &Self::Args) -> Vec<Option<wgpu::ColorTargetState>> {
        QuadPipeline::color_target_states_with_args(args)
    }

    fn depth_stencil_state() -> Option<wgpu::DepthStencilState> {
        None
    }

    fn vertex_buffer_layouts() -> Vec<wgpu::VertexBufferLayout<'static>> {
        QuadPipeline::vertex_buffer_layouts()
    }
}

#[derive(Resource)]
pub struct PostProcessBindGroup {
    bind_group: BindGroup,
}

impl PostProcessBindGroup {
    pub fn new(
        device: &RenderDevice,
        state: &GraphicsState,
        post_pipeline: &PostProcessPipeline,
        color_buffer: &wgpu::TextureView,
    ) -> Self {
        Self {
            bind_group: device.create_bind_group(
                Some("postprocess bind group"),
                &post_pipeline.bind_group_layouts[0],
                &[
                    // color buffer
                    wgpu::BindGroupEntry {
                        binding: 0,
                        resource: wgpu::BindingResource::TextureView(color_buffer),
                    },
                    // sampler
                    wgpu::BindGroupEntry {
                        binding: 1,
                        resource: wgpu::BindingResource::Sampler(state.diffuse_sampler()),
                    },
                    // uniform buffer
                    wgpu::BindGroupEntry {
                        binding: 2,
                        resource: wgpu::BindingResource::Buffer(wgpu::BufferBinding {
                            buffer: &post_pipeline.uniform_buffer,
                            offset: 0,
                            size: None,
                        }),
                    },
                ],
            ),
        }
    }

    // TODO: Move this to `PostProcessPipeline`
    pub fn update_uniform_buffers(
        &self,
        queue: &RenderQueue,
        post_pipeline: &PostProcessPipeline,
        color_shift: [[f32; 4]; 4],
    ) {
        let uniforms = PostProcessUniforms { color_shift }; // update color shift
        queue.write_buffer(&post_pipeline.uniform_buffer, 0, unsafe {
            any_as_bytes(&uniforms)
        });
    }

    pub fn record_draw<'this, 'a>(
        &'this self,
        pipeline: &'this RenderPipeline,
        pass: &'a mut TrackedRenderPass<'this>,
    ) {
        pass.set_render_pipeline(pipeline);
        pass.set_bind_group(0, &self.bind_group, &[]);
        pass.draw(0..3, 0..1);
    }
}

#[derive(Hash, Copy, Clone, Debug, PartialEq, Eq, Default, Deserialize)]
#[serde(rename_all(deserialize = "lowercase"))]
enum BlendMode {
    Add = 1,
    Average = 2,
    ColorBurn = 3,
    ColorDodge = 4,
    Darken = 5,
    Difference = 6,
    Exclusion = 7,
    Glow = 8,
    HardLight = 9,
    HardMix = 10,
    Lighten = 11,
    LinearBurn = 12,
    LinearDodge = 13,
    LinearLight = 14,
    Multiply = 15,
    Negation = 16,
    #[default]
    Normal = 17,
    Overlay = 18,
    Phoenix = 19,
    PinLight = 20,
    Reflect = 21,
    Screen = 22,
    SoftLight = 23,
    Subtract = 24,
    VividLight = 25,
}

#[derive(Hash, Copy, Clone, Debug, PartialEq, Eq, Default, Deserialize)]
#[serde(rename_all(deserialize = "lowercase"))]
enum ColorSpace {
    #[default]
    Rgb = 0,
    Xyz = 1,
    XyY = 2,
    Hsl = 3,
    Hsv = 4,
    Srgb = 5,
    Hcy = 6,
    Ycbcr = 7,
    Oklab = 8,
}

#[derive(Hash, Copy, Clone, PartialEq, Eq, Default, Debug, Resource, Deserialize)]
pub struct PostProcessVars {
    #[serde(default, rename(deserialize = "post_blendmode"))]
    blend_mode: BlendMode,
    #[serde(rename(deserialize = "post_colorspace"))]
    color_space: ColorSpace,
}

impl ExtractResource for PostProcessVars {
    type Source = Registry;

    fn extract_resource(source: &Self::Source) -> Self {
        source.read_cvars().unwrap_or_default()
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, RenderLabel)]
pub struct PostProcessPassLabel;

pub struct PostProcessPass {
    pipeline: Option<CachedRenderPipelineId>,
    system_state: SystemState<(
        ResMut<'static, PipelineCache>,
        Res<'static, PostProcessPipeline>,
        Res<'static, PostProcessVars>,
        ResMut<'static, SpecializedRenderPipelines<PostProcessPipeline>>,
    )>,
}

impl FromWorld for PostProcessPass {
    fn from_world(world: &mut World) -> Self {
        Self {
            pipeline: None,
            system_state: SystemState::new(world),
        }
    }
}

impl ViewNode for PostProcessPass {
    type ViewQuery = &'static ViewTarget;

    fn update(&mut self, world: &mut World) {
        let (mut pipeline_cache, post_pipeline, postprocess_vars, mut pipelines) =
            self.system_state.get_mut(world);
        let pipeline_id = pipelines.specialize(&pipeline_cache, &post_pipeline, *postprocess_vars);

        self.pipeline = Some(pipeline_id);

        pipeline_cache.block_on_render_pipeline(pipeline_id);
    }

    fn run<'w>(
        &self,
        _graph: &mut bevy::render::render_graph::RenderGraphContext,
        render_context: &mut bevy::render::renderer::RenderContext<'w>,
        target: &ViewTarget,
        world: &'w bevy::prelude::World,
    ) -> Result<(), bevy::render::render_graph::NodeRunError> {
        let gfx_state = world.resource::<GraphicsState>();
        let queue = world.resource::<RenderQueue>();
        let pipeline_cache = world.resource::<PipelineCache>();
        let post_pipeline = world.resource::<PostProcessPipeline>();
        let conn = world.get_resource::<RenderState>();

        let Some(conn) = conn else {
            return Ok(());
        };

        let PostProcessPass {
            pipeline: Some(pipeline_id),
            ..
        } = self
        else {
            return Ok(());
        };

        if conn
            .state
            .color_shifts
            .iter()
            .all(|ColorShift { percent, .. }| *percent == 0)
        {
            return Ok(());
        }

        let PostProcessWrite {
            source: diffuse_input,
            destination: diffuse_target,
            ..
        } = target.post_process_write();

        let pipeline = pipeline_cache.get_render_pipeline(*pipeline_id).unwrap();

        // TODO: Cache
        let bind_group = PostProcessBindGroup::new(
            render_context.render_device(),
            gfx_state,
            post_pipeline,
            diffuse_input,
        );

        let mut post_pass = render_context.begin_tracked_render_pass(RenderPassDescriptor {
            label: Some("postprocess"),
            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                view: diffuse_target,
                resolve_target: None,
                ops: default(),
            })],
            ..default()
        });

        bind_group.update_uniform_buffers(
            queue,
            post_pipeline,
            conn.state
                .color_shifts
                .map(
                    |ColorShift {
                         dest_color: [r, g, b],
                         percent,
                     }| [r, g, b, ((percent * 256) / 100).min(255) as u8],
                )
                .map(|rgba| rgba.map(|v| v as f32 / 255.)),
        );
        bind_group.record_draw(pipeline, &mut post_pass);

        Ok(())
    }
}
