use std::{
    mem::{self, size_of},
    num::NonZeroU64,
    ops::{Deref, DerefMut},
};

use crate::{
    client::render::{
        Extent2d, GraphicsState, Pipeline, TextureData,
        ui::{
            layout::{Layout, Size},
            screen_space_vertex_transform,
        },
        uniform::{self, DynamicUniformBuffer, DynamicUniformBufferBlock},
    },
    common::wad::QPic,
};

use bevy::{
    math::Mat4,
    render::{
        render_phase::TrackedRenderPass,
        render_resource::{
            BindGroup, BindGroupLayout, BindGroupLayoutEntry, Buffer, RenderPipeline, Texture,
            TextureView,
        },
        renderer::{RenderDevice, RenderQueue},
    },
};
use bytemuck::{Pod, Zeroable};
use parking_lot::RwLock;

pub const VERTICES: [QuadVertex; 6] = [
    QuadVertex {
        position: [0.0, 0.0],
        texcoord: [0.0, 1.0],
    },
    QuadVertex {
        position: [0.0, 1.0],
        texcoord: [0.0, 0.0],
    },
    QuadVertex {
        position: [1.0, 1.0],
        texcoord: [1.0, 0.0],
    },
    QuadVertex {
        position: [0.0, 0.0],
        texcoord: [0.0, 1.0],
    },
    QuadVertex {
        position: [1.0, 1.0],
        texcoord: [1.0, 0.0],
    },
    QuadVertex {
        position: [1.0, 0.0],
        texcoord: [1.0, 1.0],
    },
];

// these type aliases are here to aid readability of e.g. size_of::<Position>()
pub type Position = [f32; 2];
pub type Texcoord = [f32; 2];

#[repr(C)]
#[derive(Zeroable, Pod, Clone, Copy, Debug)]
pub struct QuadVertex {
    position: Position,
    texcoord: Texcoord,
}

pub static VERTEX_BUFFER_ATTRIBUTES: [wgpu::VertexAttribute; 2] = [
    // position
    wgpu::VertexAttribute {
        offset: 0,
        format: wgpu::VertexFormat::Float32x2,
        shader_location: 0,
    },
    // diffuse texcoord
    wgpu::VertexAttribute {
        offset: size_of::<Position>() as u64,
        format: wgpu::VertexFormat::Float32x2,
        shader_location: 1,
    },
];

struct UniformBuffer {
    buffer: DynamicUniformBuffer<QuadUniforms>,
    blocks: Vec<DynamicUniformBufferBlock<QuadUniforms>>,
}

pub struct QuadPipeline {
    pipeline: RenderPipeline,
    bind_group_layouts: Vec<BindGroupLayout>,
    format: wgpu::TextureFormat,
    vertex_buffer: Buffer,
    uniform_buffer: RwLock<UniformBuffer>,
}

impl QuadPipeline {
    pub fn new(
        device: &RenderDevice,
        compiler: &mut shaderc::Compiler,
        format: wgpu::TextureFormat,
        sample_count: u32,
    ) -> QuadPipeline {
        let (pipeline, bind_group_layouts) =
            QuadPipeline::create(device, compiler, &[], sample_count, format);

        let vertex_buffer = device.create_buffer_with_data(&wgpu::util::BufferInitDescriptor {
            label: None,
            contents: bytemuck::cast_slice(&VERTICES),
            usage: wgpu::BufferUsages::VERTEX,
        });

        let uniform_buffer = DynamicUniformBuffer::new(device);

        QuadPipeline {
            pipeline,
            bind_group_layouts,
            format,
            vertex_buffer,
            uniform_buffer: UniformBuffer {
                buffer: uniform_buffer,
                blocks: vec![],
            }
            .into(),
        }
    }

    pub fn set_format(&mut self, format: wgpu::TextureFormat) {
        self.format = format;
    }

    pub fn rebuild(
        &mut self,
        device: &RenderDevice,
        compiler: &mut shaderc::Compiler,
        sample_count: u32,
    ) {
        let layout_refs = self.bind_group_layouts.iter();
        self.pipeline = Self::recreate(device, compiler, layout_refs, sample_count, self.format);
    }

    pub fn pipeline(&self) -> &RenderPipeline {
        &self.pipeline
    }

    pub fn bind_group_layouts(&self) -> &[BindGroupLayout] {
        &self.bind_group_layouts
    }

    pub fn vertex_buffer(&self) -> &Buffer {
        &self.vertex_buffer
    }

    fn uniform_buffer(&self) -> impl Deref<Target = UniformBuffer> + '_ {
        self.uniform_buffer.read()
    }

    fn uniform_buffer_mut(&self) -> impl DerefMut<Target = UniformBuffer> + '_ {
        self.uniform_buffer.write()
    }
}

const BIND_GROUP_LAYOUT_ENTRIES: &[&[wgpu::BindGroupLayoutEntry]] = &[
    &[
        // sampler
        wgpu::BindGroupLayoutEntry {
            binding: 0,
            visibility: wgpu::ShaderStages::FRAGMENT,
            ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
            count: None,
        },
    ],
    &[
        // texture
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
    ],
    &[
        // transform matrix
        // TODO: move to push constants once they're exposed in wgpu
        wgpu::BindGroupLayoutEntry {
            binding: 0,
            visibility: wgpu::ShaderStages::all(),
            ty: wgpu::BindingType::Buffer {
                ty: wgpu::BufferBindingType::Uniform,
                has_dynamic_offset: true,
                min_binding_size: NonZeroU64::new(size_of::<QuadUniforms>() as u64),
            },
            count: None,
        },
    ],
];

impl Pipeline for QuadPipeline {
    type VertexPushConstants = ();
    type SharedPushConstants = ();
    type FragmentPushConstants = ();

    type Args = wgpu::TextureFormat;

    fn name() -> &'static str {
        "quad"
    }

    fn bind_group_layout_descriptors() -> Vec<Vec<BindGroupLayoutEntry>> {
        vec![
            // group 0: per-frame
            BIND_GROUP_LAYOUT_ENTRIES[0].to_owned(),
            // group 1: per-texture
            BIND_GROUP_LAYOUT_ENTRIES[1].to_owned(),
            // group 2: per-quad
            BIND_GROUP_LAYOUT_ENTRIES[2].to_owned(),
        ]
    }

    fn vertex_shader() -> &'static str {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/shaders/quad.vert"))
    }

    fn fragment_shader() -> &'static str {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/shaders/quad.frag"))
    }

    fn primitive_state() -> wgpu::PrimitiveState {
        wgpu::PrimitiveState {
            topology: wgpu::PrimitiveTopology::TriangleList,
            strip_index_format: None,
            front_face: wgpu::FrontFace::Cw,
            cull_mode: Some(wgpu::Face::Back),
            polygon_mode: wgpu::PolygonMode::Fill,
            conservative: false,
            ..Default::default()
        }
    }

    fn color_target_states_with_args(&format: &Self::Args) -> Vec<Option<wgpu::ColorTargetState>> {
        vec![Some(wgpu::ColorTargetState {
            format,
            blend: Some(wgpu::BlendState::REPLACE),
            write_mask: wgpu::ColorWrites::ALL,
        })]
    }

    fn depth_stencil_state() -> Option<wgpu::DepthStencilState> {
        None
    }

    // NOTE: if the vertex format is changed, this descriptor must also be changed accordingly.
    fn vertex_buffer_layouts() -> Vec<wgpu::VertexBufferLayout<'static>> {
        vec![wgpu::VertexBufferLayout {
            array_stride: size_of::<QuadVertex>() as u64,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &VERTEX_BUFFER_ATTRIBUTES[..],
        }]
    }
}

const QUAD_UNIFORMS_PAD_AMT: usize = 256 - mem::size_of::<Mat4>();
type QuadUniformsPadding = [u8; QUAD_UNIFORMS_PAD_AMT];

#[repr(C, align(256))]
#[derive(Zeroable, Pod, Clone, Copy, Debug)]
pub struct QuadUniforms {
    transform: Mat4,
    // Prevents undef.
    _pad: QuadUniformsPadding,
}

pub struct QuadTexture {
    #[expect(dead_code)]
    texture: Texture,
    #[expect(dead_code)]
    texture_view: TextureView,
    bind_group: BindGroup,
    width: u32,
    height: u32,
}

impl QuadTexture {
    pub fn from_qpic(
        state: &GraphicsState,
        device: &RenderDevice,
        queue: &RenderQueue,
        qpic: &QPic,
    ) -> QuadTexture {
        let (diffuse_data, _) = state.palette().translate(qpic.indices());
        let texture = state.create_texture(
            device,
            queue,
            None,
            qpic.width(),
            qpic.height(),
            &TextureData::Diffuse(diffuse_data),
        );
        let texture_view = texture.create_view(&Default::default());
        let bind_group = device.create_bind_group(
            None,
            &state.quad_pipeline().bind_group_layouts()[1],
            &[wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::TextureView(&texture_view),
            }],
        );

        QuadTexture {
            texture,
            texture_view,
            bind_group,
            width: qpic.width(),
            height: qpic.height(),
        }
    }

    pub fn width(&self) -> u32 {
        self.width
    }

    pub fn height(&self) -> u32 {
        self.height
    }

    pub fn scale_width(&self, scale: f32) -> u32 {
        (self.width as f32 * scale) as u32
    }

    pub fn scale_height(&self, scale: f32) -> u32 {
        (self.height as f32 * scale) as u32
    }
}

/// A command which specifies how a quad should be rendered.
pub struct QuadRendererCommand<'a> {
    /// The texture to be mapped to the quad.
    pub texture: &'a QuadTexture,

    /// The layout specifying the size and position of the quad on the screen.
    pub layout: Layout,
}

pub struct QuadRenderer {
    sampler_bind_group: BindGroup,
    transform_bind_group: BindGroup,
}

impl QuadRenderer {
    pub fn new(state: &GraphicsState, device: &RenderDevice) -> QuadRenderer {
        let sampler_bind_group = device.create_bind_group(
            Some("quad sampler bind group"),
            &state.quad_pipeline().bind_group_layouts()[0],
            &[wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::Sampler(state.nearest_sampler()),
            }],
        );
        let transform_bind_group = device.create_bind_group(
            Some("quad transform bind group"),
            &state.quad_pipeline().bind_group_layouts()[2],
            &[wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::Buffer(wgpu::BufferBinding {
                    buffer: state.quad_pipeline().uniform_buffer().buffer.buffer(),
                    offset: 0,
                    size: Some(NonZeroU64::new(size_of::<QuadUniforms>() as u64).unwrap()),
                }),
            }],
        );

        QuadRenderer {
            sampler_bind_group,
            transform_bind_group,
        }
    }

    fn generate_uniforms(
        &self,
        commands: &[QuadRendererCommand<'_>],
        target_size: Extent2d,
    ) -> Vec<QuadUniforms> {
        let mut uniforms = Vec::new();

        for cmd in commands {
            let QuadRendererCommand {
                texture,
                layout:
                    Layout {
                        position,
                        anchor,
                        size,
                    },
            } = *cmd;

            let scale = match size {
                Size::Scale { factor } => factor,
                _ => 1.0,
            };

            let Extent2d {
                width: display_width,
                height: display_height,
            } = target_size;

            let (screen_x, screen_y) = position.to_xy(display_width, display_height, scale);
            let (quad_x, quad_y) = anchor.to_xy(texture.width, texture.height);
            let x = screen_x - (quad_x as f32 * scale) as i32;
            let y = screen_y - (quad_y as f32 * scale) as i32;
            let (quad_width, quad_height) =
                size.to_wh(texture.width, texture.height, display_width, display_height);

            uniforms.push(QuadUniforms {
                transform: screen_space_vertex_transform(
                    display_width,
                    display_height,
                    quad_width,
                    quad_height,
                    x,
                    y,
                ),
                _pad: [0; QUAD_UNIFORMS_PAD_AMT],
            });
        }

        uniforms
    }

    pub fn update_uniforms(
        &self,
        state: &GraphicsState,
        queue: &RenderQueue,
        target_size: Extent2d,
        commands: &[QuadRendererCommand<'_>],
    ) {
        // update uniform buffer
        let uniforms = self.generate_uniforms(commands, target_size);
        let mut uniform_buffer = state.quad_pipeline().uniform_buffer_mut();
        let uniform_buffer = &mut *uniform_buffer;
        uniform::clear_and_rewrite(
            queue,
            &mut uniform_buffer.buffer,
            &mut uniform_buffer.blocks,
            &uniforms,
        );
    }

    pub fn record_draw<'this, 'a>(
        &'this self,
        state: &'this GraphicsState,
        pass: &'a mut TrackedRenderPass<'this>,
        commands: &'a [QuadRendererCommand<'this>],
    ) {
        pass.set_render_pipeline(state.quad_pipeline().pipeline());
        pass.set_vertex_buffer(0, state.quad_pipeline().vertex_buffer().slice(..));
        pass.set_bind_group(0, &self.sampler_bind_group, &[]);
        for (cmd, block) in commands
            .iter()
            .zip(state.quad_pipeline().uniform_buffer().blocks.iter())
        {
            pass.set_bind_group(1, &cmd.texture.bind_group, &[]);
            pass.set_bind_group(2, &self.transform_bind_group, &[block.offset()]);
            pass.draw(0..6, 0..1);
        }
    }
}
