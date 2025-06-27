use std::mem::size_of;

use crate::{
    client::render::{
        GraphicsState, Pipeline, TextureData,
        world::{BindGroupLayoutId, WorldPipelineBase},
    },
    common::{
        sprite::{SpriteFrame, SpriteKind, SpriteModel, SpriteSubframe},
        util::any_slice_as_bytes,
    },
};

use bevy::{
    ecs::component::Component,
    render::{
        render_phase::TrackedRenderPass,
        render_resource::{
            BindGroup, BindGroupLayout, BindGroupLayoutEntry, Buffer, RenderPipeline, Texture,
            TextureView,
        },
        renderer::{RenderDevice, RenderQueue},
    },
};
use chrono::Duration;

pub struct SpritePipeline {
    pipeline: RenderPipeline,
    bind_group_layouts: Vec<BindGroupLayout>,
    vertex_buffer: Buffer,
}

impl SpritePipeline {
    pub fn new(
        device: &RenderDevice,
        compiler: &mut shaderc::Compiler,
        world_bind_group_layouts: &[BindGroupLayout],
        diffuse_format: wgpu::TextureFormat,
        normal_format: wgpu::TextureFormat,
        sample_count: u32,
    ) -> SpritePipeline {
        let (pipeline, bind_group_layouts) = SpritePipeline::create(
            device,
            compiler,
            world_bind_group_layouts,
            sample_count,
            (diffuse_format, normal_format),
        );

        let vertex_buffer = device.create_buffer_with_data(&wgpu::util::BufferInitDescriptor {
            label: None,
            contents: unsafe { any_slice_as_bytes(&VERTICES) },
            usage: wgpu::BufferUsages::VERTEX,
        });

        SpritePipeline {
            pipeline,
            bind_group_layouts,
            vertex_buffer,
        }
    }

    pub fn rebuild(
        &mut self,
        device: &RenderDevice,
        compiler: &mut shaderc::Compiler,
        diffuse_format: wgpu::TextureFormat,
        normal_format: wgpu::TextureFormat,
        world_bind_group_layouts: &[BindGroupLayout],
        sample_count: u32,
    ) {
        let layout_refs = world_bind_group_layouts
            .iter()
            .chain(self.bind_group_layouts.iter());
        self.pipeline = Self::recreate(
            device,
            compiler,
            layout_refs,
            sample_count,
            (diffuse_format, normal_format),
        );
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
}

static VERTEX_BUFFER_ATTRIBUTES: [wgpu::VertexAttribute; 3] = wgpu::vertex_attr_array![
    // position
    0 => Float32x3,
    // normal
    1 => Float32x3,
    // texcoord
    2 => Float32x2,
];

impl Pipeline for SpritePipeline {
    type VertexPushConstants = ();
    type SharedPushConstants = ();
    type FragmentPushConstants = ();

    type Args = <WorldPipelineBase as Pipeline>::Args;

    fn name() -> &'static str {
        "sprite"
    }

    fn vertex_shader() -> &'static str {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/shaders/sprite.vert"))
    }

    fn fragment_shader() -> &'static str {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/shaders/sprite.frag"))
    }

    // NOTE: if any of the binding indices are changed, they must also be changed in
    // the corresponding shaders and the BindGroupLayout generation functions.
    fn bind_group_layout_descriptors() -> Vec<Vec<BindGroupLayoutEntry>> {
        vec![
            // group 2: updated per-texture
            vec![
                // diffuse texture, updated once per face
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
        ]
    }

    fn primitive_state() -> wgpu::PrimitiveState {
        WorldPipelineBase::primitive_state()
    }

    fn color_target_states_with_args(args: &Self::Args) -> Vec<Option<wgpu::ColorTargetState>> {
        WorldPipelineBase::color_target_states_with_args(args)
    }

    fn depth_stencil_state() -> Option<wgpu::DepthStencilState> {
        WorldPipelineBase::depth_stencil_state()
    }

    // NOTE: if the vertex format is changed, this descriptor must also be changed accordingly.
    fn vertex_buffer_layouts() -> Vec<wgpu::VertexBufferLayout<'static>> {
        vec![wgpu::VertexBufferLayout {
            array_stride: size_of::<SpriteVertex>() as u64,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &VERTEX_BUFFER_ATTRIBUTES[..],
        }]
    }
}

// these type aliases are here to aid readability of e.g. size_of::<Position>()
type Position = [f32; 3];
type Normal = [f32; 3];
type DiffuseTexcoord = [f32; 2];

#[repr(C)]
#[derive(Clone, Copy, Debug)]
pub struct SpriteVertex {
    position: Position,
    normal: Normal,
    diffuse_texcoord: DiffuseTexcoord,
}

pub const VERTICES: [SpriteVertex; 6] = [
    SpriteVertex {
        position: [0.0, 0.0, 0.0],
        normal: [0.0, 0.0, 1.0],
        diffuse_texcoord: [0.0, 1.0],
    },
    SpriteVertex {
        position: [0.0, 1.0, 0.0],
        normal: [0.0, 0.0, 1.0],
        diffuse_texcoord: [0.0, 0.0],
    },
    SpriteVertex {
        position: [1.0, 1.0, 0.0],
        normal: [0.0, 0.0, 1.0],
        diffuse_texcoord: [1.0, 0.0],
    },
    SpriteVertex {
        position: [0.0, 0.0, 0.0],
        normal: [0.0, 0.0, 1.0],
        diffuse_texcoord: [0.0, 1.0],
    },
    SpriteVertex {
        position: [1.0, 1.0, 0.0],
        normal: [0.0, 0.0, 1.0],
        diffuse_texcoord: [1.0, 0.0],
    },
    SpriteVertex {
        position: [1.0, 0.0, 0.0],
        normal: [0.0, 0.0, 1.0],
        diffuse_texcoord: [1.0, 1.0],
    },
];

enum Frame {
    Static {
        _diffuse: Texture,
        bind_group: BindGroup,
    },
    Animated {
        _diffuses: Vec<Texture>,
        bind_groups: Vec<BindGroup>,
        total_duration: Duration,
        durations: Vec<Duration>,
    },
}

impl Frame {
    fn new(
        state: &GraphicsState,
        device: &RenderDevice,
        queue: &RenderQueue,
        sframe: &SpriteFrame,
    ) -> Frame {
        fn convert_subframe(
            state: &GraphicsState,
            device: &RenderDevice,
            queue: &RenderQueue,
            subframe: &SpriteSubframe,
        ) -> (Texture, TextureView, BindGroup) {
            let (diffuse_data, _fullbright_data) = state.palette.translate(subframe.indexed());
            let diffuse = state.create_texture(
                device,
                queue,
                None,
                subframe.width(),
                subframe.height(),
                &TextureData::Diffuse(diffuse_data),
            );
            let diffuse_view = diffuse.create_view(&Default::default());
            let bind_group = device.create_bind_group(
                None,
                &state.sprite_pipeline().bind_group_layouts()
                    [BindGroupLayoutId::PerTexture as usize - 2],
                &[wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(&diffuse_view),
                }],
            );
            (diffuse, diffuse_view, bind_group)
        }

        match sframe {
            SpriteFrame::Static { frame } => {
                let (diffuse, _, bind_group) = convert_subframe(state, device, queue, frame);

                Frame::Static {
                    _diffuse: diffuse,
                    bind_group,
                }
            }

            SpriteFrame::Animated {
                subframes,
                durations,
            } => {
                let mut diffuses = Vec::new();
                let mut bind_groups = Vec::new();

                for (diffuse, _, bind_group) in subframes
                    .iter()
                    .map(|subframe| convert_subframe(state, device, queue, subframe))
                {
                    diffuses.push(diffuse);
                    bind_groups.push(bind_group);
                }

                let total_duration = durations.iter().fold(Duration::zero(), |init, d| init + *d);

                Frame::Animated {
                    _diffuses: diffuses,
                    bind_groups,
                    total_duration,
                    durations: durations.clone(),
                }
            }
        }
    }

    fn animate(&self, time: Duration) -> &BindGroup {
        match self {
            Frame::Static { bind_group, .. } => &bind_group,
            Frame::Animated {
                bind_groups,
                total_duration,
                durations,
                ..
            } => {
                let mut time_ms = time.num_milliseconds() % total_duration.num_milliseconds();
                for (i, d) in durations.iter().enumerate() {
                    time_ms -= d.num_milliseconds();
                    if time_ms <= 0 {
                        return &bind_groups[i];
                    }
                }

                unreachable!()
            }
        }
    }
}

#[derive(Component)]
pub struct SpriteRenderer {
    kind: SpriteKind,
    frames: Vec<Frame>,
}

impl SpriteRenderer {
    pub fn new(
        state: &GraphicsState,
        device: &RenderDevice,
        queue: &RenderQueue,
        sprite: &SpriteModel,
    ) -> SpriteRenderer {
        let frames = sprite
            .frames()
            .map(|f| Frame::new(state, device, queue, f))
            .collect();

        SpriteRenderer {
            kind: sprite.kind(),
            frames,
        }
    }

    pub fn record_draw<'a>(
        &'a self,
        state: &'a GraphicsState,
        pass: &mut TrackedRenderPass<'a>,
        frame_id: usize,
        time: Duration,
    ) {
        pass.set_render_pipeline(state.sprite_pipeline().pipeline());
        pass.set_vertex_buffer(0, state.sprite_pipeline().vertex_buffer().slice(..));
        pass.set_bind_group(
            BindGroupLayoutId::PerTexture as usize,
            self.frames[frame_id % self.frames.len()].animate(time),
            &[],
        );
        pass.draw(0..VERTICES.len() as u32, 0..1);
    }

    pub fn kind(&self) -> SpriteKind {
        self.kind
    }
}
