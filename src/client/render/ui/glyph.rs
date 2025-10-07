use std::{mem::size_of, num::NonZeroU32};

use crate::client::render::{
    Extent2d, GraphicsState, Pipeline, TextureData,
    ui::{
        layout::{Anchor, ScreenPosition},
        quad::{QuadPipeline, QuadVertex},
        screen_space_vertex_scale, screen_space_vertex_translate,
    },
};

use bevy::{
    math::Vec2,
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

pub const GLYPH_WIDTH: usize = 8;
pub const GLYPH_HEIGHT: usize = 8;
const GLYPH_COLS: usize = 16;
const GLYPH_ROWS: usize = 8;
const GLYPH_COUNT: usize = GLYPH_ROWS * GLYPH_COLS;
const GLYPH_TEXTURE_WIDTH: usize = GLYPH_WIDTH * GLYPH_COLS;

/// The maximum number of glyphs that can be rendered at once.
pub const MAX_INSTANCES: usize = 65536;

static VERTEX_BUFFER_ATTRIBUTES: [&[wgpu::VertexAttribute]; 2] = [
    &wgpu::vertex_attr_array![
        0 => Float32x2, // a_position
        1 => Float32x2 // a_texcoord
    ],
    &wgpu::vertex_attr_array![
        2 => Float32x2, // a_instance_position
        3 => Float32x2, // a_instance_scale
        4 => Uint32 // a_instance_layer
    ],
];

pub struct GlyphPipeline {
    pipeline: RenderPipeline,
    bind_group_layouts: Vec<BindGroupLayout>,
    format: wgpu::TextureFormat,
    instance_buffer: Buffer,
}

impl GlyphPipeline {
    pub fn new(
        device: &RenderDevice,
        compiler: &mut shaderc::Compiler,
        format: wgpu::TextureFormat,
        sample_count: u32,
    ) -> GlyphPipeline {
        let (pipeline, bind_group_layouts) =
            GlyphPipeline::create(device, compiler, &[], sample_count, format);

        let instance_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("quad instance buffer"),
            size: (MAX_INSTANCES * size_of::<GlyphInstance>()) as u64,
            usage: wgpu::BufferUsages::VERTEX | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        GlyphPipeline {
            pipeline,
            bind_group_layouts,
            format,
            instance_buffer,
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

    pub fn instance_buffer(&self) -> &Buffer {
        &self.instance_buffer
    }
}

const BIND_GROUP_LAYOUT_ENTRIES: &[wgpu::BindGroupLayoutEntry] = &[
    // sampler
    wgpu::BindGroupLayoutEntry {
        binding: 0,
        visibility: wgpu::ShaderStages::FRAGMENT,
        ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
        count: None,
    },
    // glyph texture array
    wgpu::BindGroupLayoutEntry {
        binding: 1,
        visibility: wgpu::ShaderStages::FRAGMENT,
        ty: wgpu::BindingType::Texture {
            view_dimension: wgpu::TextureViewDimension::D2,
            sample_type: wgpu::TextureSampleType::Float { filterable: true },
            multisampled: false,
        },
        count: NonZeroU32::new(GLYPH_COUNT as u32),
    },
];

impl Pipeline for GlyphPipeline {
    type VertexPushConstants = ();
    type SharedPushConstants = ();
    type FragmentPushConstants = ();

    type Args = wgpu::TextureFormat;

    fn name() -> &'static str {
        "glyph"
    }

    fn vertex_shader() -> &'static str {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/shaders/glyph.vert"))
    }

    fn fragment_shader() -> &'static str {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/shaders/glyph.frag"))
    }

    fn primitive_state() -> wgpu::PrimitiveState {
        QuadPipeline::primitive_state()
    }

    fn bind_group_layout_descriptors() -> Vec<Vec<BindGroupLayoutEntry>> {
        vec![BIND_GROUP_LAYOUT_ENTRIES.to_owned()]
    }

    fn color_target_states_with_args(format: &Self::Args) -> Vec<Option<wgpu::ColorTargetState>> {
        QuadPipeline::color_target_states_with_args(format)
    }

    fn depth_stencil_state() -> Option<wgpu::DepthStencilState> {
        QuadPipeline::depth_stencil_state()
    }

    fn vertex_buffer_layouts() -> Vec<wgpu::VertexBufferLayout<'static>> {
        vec![
            wgpu::VertexBufferLayout {
                array_stride: size_of::<QuadVertex>() as u64,
                step_mode: wgpu::VertexStepMode::Vertex,
                attributes: VERTEX_BUFFER_ATTRIBUTES[0],
            },
            wgpu::VertexBufferLayout {
                array_stride: size_of::<GlyphInstance>() as u64,
                step_mode: wgpu::VertexStepMode::Instance,
                attributes: VERTEX_BUFFER_ATTRIBUTES[1],
            },
        ]
    }
}

#[repr(C)]
#[derive(Zeroable, Pod, Copy, Clone, Debug)]
pub struct GlyphInstance {
    pub position: Vec2,
    pub scale: Vec2,
    pub layer: u32,
}

pub enum GlyphRendererCommand {
    Glyph {
        glyph_id: u8,
        position: ScreenPosition,
        anchor: Anchor,
        scale: f32,
    },
    Text {
        text: String,
        position: ScreenPosition,
        anchor: Anchor,
        scale: f32,
    },
}

pub struct GlyphRenderer {
    #[expect(dead_code)]
    textures: Vec<Texture>,
    #[expect(dead_code)]
    texture_views: Vec<TextureView>,
    const_bind_group: BindGroup,
}

impl GlyphRenderer {
    pub fn new(state: &GraphicsState, device: &RenderDevice, queue: &RenderQueue) -> GlyphRenderer {
        let conchars = state.gfx_wad().open_conchars().unwrap();

        // TODO: validate conchars dimensions

        let indices = conchars
            .indices()
            .iter()
            .map(|i| if *i == 0 { 0xFF } else { *i })
            .collect::<Vec<_>>();

        // reorder indices from atlas order to array order
        let mut array_order = Vec::new();
        for glyph_id in 0..GLYPH_COUNT {
            for glyph_r in 0..GLYPH_HEIGHT {
                for glyph_c in 0..GLYPH_WIDTH {
                    let atlas_r = GLYPH_HEIGHT * (glyph_id / GLYPH_COLS) + glyph_r;
                    let atlas_c = GLYPH_WIDTH * (glyph_id % GLYPH_COLS) + glyph_c;
                    array_order.push(indices[atlas_r * GLYPH_TEXTURE_WIDTH + atlas_c]);
                }
            }
        }

        let textures = array_order
            .chunks_exact(GLYPH_WIDTH * GLYPH_HEIGHT)
            .enumerate()
            .map(|(id, indices)| {
                let (diffuse_data, _) = state.palette().translate(indices);
                state.create_texture(
                    device,
                    queue,
                    Some(&format!("conchars[{id}]")),
                    GLYPH_WIDTH as u32,
                    GLYPH_HEIGHT as u32,
                    &TextureData::Diffuse(diffuse_data),
                )
            })
            .collect::<Vec<_>>();

        let texture_views = textures
            .iter()
            .map(|tex| tex.create_view(&Default::default()))
            .collect::<Vec<_>>();
        let texture_view_refs = texture_views.iter().map(|t| &**t).collect::<Vec<_>>();

        let const_bind_group = device.create_bind_group(
            Some("glyph constant bind group"),
            &state.glyph_pipeline().bind_group_layouts()[0],
            &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::Sampler(state.nearest_sampler()),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::TextureViewArray(&texture_view_refs[..]),
                },
            ],
        );

        GlyphRenderer {
            textures,
            texture_views,
            const_bind_group,
        }
    }

    pub fn generate_instances(
        &self,
        commands: &[GlyphRendererCommand],
        target_size: Extent2d,
    ) -> Vec<GlyphInstance> {
        let mut instances = Vec::new();
        let Extent2d {
            width: display_width,
            height: display_height,
        } = target_size;
        for cmd in commands {
            match cmd {
                GlyphRendererCommand::Glyph {
                    glyph_id,
                    position,
                    anchor,
                    scale,
                } => {
                    let (screen_x, screen_y) =
                        position.to_xy(display_width, display_height, *scale);
                    let (glyph_x, glyph_y) = anchor.to_xy(
                        (GLYPH_WIDTH as f32 * scale) as u32,
                        (GLYPH_HEIGHT as f32 * scale) as u32,
                    );
                    let x = screen_x - glyph_x;
                    let y = screen_y - glyph_y;

                    instances.push(GlyphInstance {
                        position: screen_space_vertex_translate(
                            display_width,
                            display_height,
                            x,
                            y,
                        ),
                        scale: screen_space_vertex_scale(
                            display_width,
                            display_height,
                            (GLYPH_WIDTH as f32 * scale) as u32,
                            (GLYPH_HEIGHT as f32 * scale) as u32,
                        ),
                        layer: *glyph_id as u32,
                    });
                }
                GlyphRendererCommand::Text {
                    text,
                    position,
                    anchor,
                    scale,
                } => {
                    let (screen_x, screen_y) =
                        position.to_xy(display_width, display_height, *scale);
                    let (text_x, text_y) = anchor.to_xy(
                        ((text.chars().count() * GLYPH_WIDTH) as f32 * scale) as u32,
                        (GLYPH_HEIGHT as f32 * scale) as u32,
                    );
                    let x = screen_x - text_x;
                    let y = screen_y - text_y;

                    for (chr_id, chr) in text.chars().enumerate() {
                        let abs_x = x + ((GLYPH_WIDTH * chr_id) as f32 * scale) as i32;

                        instances.push(GlyphInstance {
                            position: screen_space_vertex_translate(
                                display_width,
                                display_height,
                                abs_x,
                                y,
                            ),
                            scale: screen_space_vertex_scale(
                                display_width,
                                display_height,
                                (GLYPH_WIDTH as f32 * scale) as u32,
                                (GLYPH_HEIGHT as f32 * scale) as u32,
                            ),
                            layer: chr as u32,
                        });
                    }
                }
            }
        }

        instances
    }

    pub fn record_draw<'a>(
        &'a self,
        state: &'a GraphicsState,
        queue: &RenderQueue,
        pass: &mut TrackedRenderPass<'a>,
        target_size: Extent2d,
        commands: &[GlyphRendererCommand],
    ) {
        let instances = self.generate_instances(commands, target_size);
        queue.write_buffer(
            state.glyph_pipeline().instance_buffer(),
            0,
            bytemuck::cast_slice(&instances),
        );
        pass.set_render_pipeline(state.glyph_pipeline().pipeline());
        pass.set_vertex_buffer(0, state.quad_pipeline().vertex_buffer().slice(..));
        pass.set_vertex_buffer(1, state.glyph_pipeline().instance_buffer().slice(..));
        pass.set_bind_group(0, &self.const_bind_group, &[]);
        pass.draw(0..6, 0..instances.len() as u32);
    }
}
