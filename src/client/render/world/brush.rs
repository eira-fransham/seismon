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

use std::{
    mem::size_of,
    num::NonZeroU32,
    ops::Range,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
};

use crate::{
    client::render::{
        Camera, GraphicsState, LightmapData, Pipeline, TextureData,
        pipeline::PushConstantUpdate,
        warp,
        world::{BindGroupLayoutId, WorldPipelineBase},
    },
    common::{
        bsp::{
            self, BspData, BspFace, BspLeaf, BspModel, BspTexInfo, BspTexture, BspTextureKind,
            BspTextureMipmap,
        },
        math,
        util::any_slice_as_bytes,
    },
};

use beef::Cow;
use bevy::{
    prelude::*,
    render::{
        render_phase::TrackedRenderPass,
        render_resource::{
            BindGroup, BindGroupLayout, BindGroupLayoutEntry, Buffer, RenderPipeline, Texture,
        },
        renderer::{RenderDevice, RenderQueue},
        texture::CachedTexture,
    },
};
use bumpalo::Bump;
use cgmath::{InnerSpace as _, Matrix3, Matrix4, Vector3};
use chrono::Duration;
use failure::Error;
use hashbrown::HashMap;
use num::Zero;

pub struct BrushPipeline {
    pipeline: RenderPipeline,
    bind_group_layouts: Vec<BindGroupLayout>,
}

impl BrushPipeline {
    pub fn new(
        device: &RenderDevice,
        compiler: &mut shaderc::Compiler,
        world_bind_group_layouts: &[BindGroupLayout],
        diffuse_format: wgpu::TextureFormat,
        normal_format: wgpu::TextureFormat,
        sample_count: u32,
    ) -> BrushPipeline {
        let (pipeline, bind_group_layouts) = BrushPipeline::create(
            device,
            compiler,
            world_bind_group_layouts,
            sample_count,
            (diffuse_format, normal_format),
        );

        BrushPipeline {
            pipeline,
            // TODO: pick a starting capacity
            bind_group_layouts,
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

    pub fn bind_group_layout(&self, id: BindGroupLayoutId) -> &BindGroupLayout {
        assert!(id as usize >= BindGroupLayoutId::PerTexture as usize);
        &self.bind_group_layouts[id as usize - BindGroupLayoutId::PerTexture as usize]
    }
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct VertexPushConstants {
    pub transform: Matrix4<f32>,
    pub model_view: Matrix3<f32>,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct SharedPushConstants {
    pub texture_kind: u32,
}

const BIND_GROUP_LAYOUT_ENTRIES: &[&[BindGroupLayoutEntry]] = &[
    &[
        // diffuse texture, updated once per face
        BindGroupLayoutEntry {
            binding: 0,
            visibility: wgpu::ShaderStages::FRAGMENT,
            ty: wgpu::BindingType::Texture {
                view_dimension: wgpu::TextureViewDimension::D2,
                sample_type: wgpu::TextureSampleType::Float { filterable: true },
                multisampled: false,
            },
            count: None,
        },
        // fullbright texture
        BindGroupLayoutEntry {
            binding: 1,
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
        // lightmap texture array
        BindGroupLayoutEntry {
            count: NonZeroU32::new(4),
            binding: 0,
            visibility: wgpu::ShaderStages::FRAGMENT,
            ty: wgpu::BindingType::Texture {
                view_dimension: wgpu::TextureViewDimension::D2,
                sample_type: wgpu::TextureSampleType::Float { filterable: true },
                multisampled: false,
            },
        },
    ],
];

static VERTEX_ATTRIBUTES: [wgpu::VertexAttribute; 5] = wgpu::vertex_attr_array![
    // position
    0 => Float32x3,
    // normal
    1 => Float32x3,
    // diffuse texcoord
    2 => Float32x2,
    // lightmap texcoord
    3 => Float32x2,
    // lightmap animation ids
    4 => Uint8x4,
];

impl Pipeline for BrushPipeline {
    type VertexPushConstants = VertexPushConstants;
    type SharedPushConstants = SharedPushConstants;
    type FragmentPushConstants = ();

    type Args = <WorldPipelineBase as Pipeline>::Args;

    fn name() -> &'static str {
        "brush"
    }

    fn vertex_shader() -> &'static str {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/shaders/brush.vert"))
    }

    fn fragment_shader() -> &'static str {
        include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/shaders/brush.frag"))
    }

    // NOTE: if any of the binding indices are changed, they must also be changed in
    // the corresponding shaders and the BindGroupLayout generation functions.
    fn bind_group_layout_descriptors() -> Vec<Vec<BindGroupLayoutEntry>> {
        vec![
            BIND_GROUP_LAYOUT_ENTRIES[0].to_owned(),
            BIND_GROUP_LAYOUT_ENTRIES[1].to_owned(),
        ]
    }

    fn primitive_state() -> wgpu::PrimitiveState {
        WorldPipelineBase::primitive_state()
    }

    fn color_target_states_with_args(args: Self::Args) -> Vec<Option<wgpu::ColorTargetState>> {
        WorldPipelineBase::color_target_states_with_args(args)
    }

    fn depth_stencil_state() -> Option<wgpu::DepthStencilState> {
        WorldPipelineBase::depth_stencil_state()
    }

    // NOTE: if the vertex format is changed, this descriptor must also be changed accordingly.
    fn vertex_buffer_layouts() -> Vec<wgpu::VertexBufferLayout<'static>> {
        vec![wgpu::VertexBufferLayout {
            array_stride: size_of::<BrushVertex>() as u64,
            step_mode: wgpu::VertexStepMode::Vertex,
            attributes: &VERTEX_ATTRIBUTES[..],
        }]
    }
}

fn calculate_lightmap_texcoords(
    position: Vector3<f32>,
    face: &BspFace,
    texinfo: &BspTexInfo,
) -> [f32; 2] {
    let mut s = texinfo.s_vector.dot(position) + texinfo.s_offset;
    s -= (face.texture_mins[0] as f32 / 16.0).floor() * 16.0;
    s += 0.5;
    s /= face.extents[0] as f32;

    let mut t = texinfo.t_vector.dot(position) + texinfo.t_offset;
    t -= (face.texture_mins[1] as f32 / 16.0).floor() * 16.0;
    t += 0.5;
    t /= face.extents[1] as f32;
    [s, t]
}

type Position = [f32; 3];
type Normal = [f32; 3];
type DiffuseTexcoord = [f32; 2];
type LightmapTexcoord = [f32; 2];
type LightmapAnim = [u8; 4];

#[repr(C)]
#[derive(Clone, Copy, Debug)]
struct BrushVertex {
    position: Position,
    normal: Normal,
    diffuse_texcoord: DiffuseTexcoord,
    lightmap_texcoord: LightmapTexcoord,
    lightmap_anim: LightmapAnim,
}

#[repr(u32)]
#[derive(Clone, Copy, Debug)]
pub enum TextureKind {
    Normal = 0,
    Warp = 1,
    Sky = 2,
}

/// A single frame of a brush texture.
pub struct BrushTextureFrame {
    bind_group_id: usize,
    diffuse: CachedTexture,
    fullbright: CachedTexture,
    kind: TextureKind,
}

/// A brush texture.
pub enum BrushTexture {
    /// A brush texture with a single frame.
    Static(BrushTextureFrame),

    /// A brush texture with multiple frames.
    ///
    /// Animated brush textures advance one frame every 200 milliseconds, i.e.,
    /// they have a framerate of 5 fps.
    Animated {
        primary: Vec<BrushTextureFrame>,
        alternate: Option<Vec<BrushTextureFrame>>,
    },
}

impl BrushTexture {
    fn kind(&self) -> TextureKind {
        match self {
            BrushTexture::Static(frame) => frame.kind,
            BrushTexture::Animated { primary, .. } => primary[0].kind,
        }
    }
}

#[derive(Debug)]
struct BrushFace {
    vertices: Range<u32>,
    _min: Vector3<f32>,
    _max: Vector3<f32>,

    texture_id: usize,

    lightmap_ids: Vec<usize>,
    _light_styles: [u8; 4],

    /// Indicates whether the face should be drawn this frame.
    ///
    /// This is set to false by default, and will be set to true if the model is
    /// a worldmodel and the containing leaf is in the PVS. If the model is not
    /// a worldmodel, this flag is ignored.
    draw_flag: AtomicBool,
}

struct BrushLeaf {
    facelist_ids: Range<usize>,
}

impl<B> std::convert::From<B> for BrushLeaf
where
    B: std::borrow::Borrow<BspLeaf>,
{
    fn from(bsp_leaf: B) -> Self {
        let bsp_leaf = bsp_leaf.borrow();
        BrushLeaf {
            facelist_ids: bsp_leaf.facelist_id..bsp_leaf.facelist_id + bsp_leaf.facelist_count,
        }
    }
}

pub struct BrushRendererBuilder {
    bsp_data: Arc<BspData>,
    face_range: Range<usize>,

    leaves: Option<Vec<BrushLeaf>>,

    per_texture_bind_groups: Vec<BindGroup>,
    per_face_bind_groups: Vec<BindGroup>,

    vertices: Vec<BrushVertex>,
    faces: Vec<BrushFace>,
    texture_chains: HashMap<usize, Vec<usize>>,
    textures: Vec<BrushTexture>,
    lightmaps: Vec<Texture>,
    //lightmap_views: Vec<TextureView>,
}

impl BrushRendererBuilder {
    pub fn new(bsp_model: &BspModel, worldmodel: bool) -> BrushRendererBuilder {
        BrushRendererBuilder {
            bsp_data: bsp_model.bsp_data(),
            face_range: bsp_model.face_id..bsp_model.face_id + bsp_model.face_count,
            leaves: if worldmodel {
                Some(
                    bsp_model
                        .iter_leaves()
                        .map(|leaf| BrushLeaf::from(leaf))
                        .collect(),
                )
            } else {
                None
            },
            per_texture_bind_groups: Default::default(),
            per_face_bind_groups: Vec::new(),
            vertices: Vec::new(),
            faces: Vec::new(),
            texture_chains: HashMap::default(),
            textures: Vec::new(),
            lightmaps: Vec::new(),
            //lightmap_views: Vec::new(),
        }
    }

    fn create_face(
        &mut self,
        state: &GraphicsState,
        device: &RenderDevice,
        queue: &RenderQueue,
        face_id: usize,
    ) -> BrushFace {
        let face = &self.bsp_data.faces()[face_id];
        let face_vert_id = self.vertices.len();
        let texinfo = &self.bsp_data.texinfo()[face.texinfo_id];
        let tex = &self.bsp_data.textures()[texinfo.tex_id];

        let mut min = Vector3::new(f32::INFINITY, f32::INFINITY, f32::INFINITY);
        let mut max = Vector3::new(f32::NEG_INFINITY, f32::NEG_INFINITY, f32::NEG_INFINITY);

        let no_collinear =
            math::remove_collinear(self.bsp_data.face_iter_vertices(face_id).collect());

        for vert in no_collinear.iter() {
            for component in 0..3 {
                min[component] = min[component].min(vert[component]);
                max[component] = max[component].max(vert[component]);
            }
        }

        if tex.name().starts_with("*") {
            // tessellate the surface so we can do texcoord warping
            let verts = warp::subdivide(no_collinear);
            let normal = match &*verts {
                [a, b, c, ..] => (a - b).cross(c - b).normalize(),
                _ => Vector3::zero(),
            };
            for vert in verts.into_iter() {
                self.vertices.push(BrushVertex {
                    position: vert.into(),
                    normal: normal.into(),
                    diffuse_texcoord: [
                        ((vert.dot(texinfo.s_vector) + texinfo.s_offset) / tex.width() as f32),
                        ((vert.dot(texinfo.t_vector) + texinfo.t_offset) / tex.height() as f32),
                    ],
                    lightmap_texcoord: calculate_lightmap_texcoords(vert.into(), face, texinfo),
                    lightmap_anim: face.light_styles,
                })
            }
        } else {
            // expand the vertices into a triangle list.
            // the vertices are guaranteed to be in valid triangle fan order (that's
            // how GLQuake renders them) so we expand from triangle fan to triangle
            // list order.
            //
            // v1 is the base vertex, so it remains constant.
            // v2 takes the previous value of v3.
            // v3 is the newest vertex.
            let verts = no_collinear;
            let normal = match &*verts {
                [a, b, c, ..] => (a - b).cross(c - b).normalize(),
                _ => Vector3::zero(),
            };
            let mut vert_iter = verts.into_iter();

            let v1 = vert_iter.next().unwrap();
            let mut v2 = vert_iter.next().unwrap();
            for v3 in vert_iter {
                let tri = &[v1, v2, v3];

                // skip collinear points
                for vert in tri.iter() {
                    self.vertices.push(BrushVertex {
                        position: (*vert).into(),
                        normal: normal.into(),
                        diffuse_texcoord: [
                            ((vert.dot(texinfo.s_vector) + texinfo.s_offset) / tex.width() as f32),
                            ((vert.dot(texinfo.t_vector) + texinfo.t_offset) / tex.height() as f32),
                        ],
                        lightmap_texcoord: calculate_lightmap_texcoords(
                            (*vert).into(),
                            face,
                            texinfo,
                        ),
                        lightmap_anim: face.light_styles,
                    });
                }

                v2 = v3;
            }
        }

        // build the lightmaps
        let lightmaps = if !texinfo.special {
            self.bsp_data.face_lightmaps(face_id)
        } else {
            Vec::new()
        };

        let mut lightmap_ids = Vec::new();
        for lightmap in lightmaps {
            let lightmap_data = TextureData::Lightmap(LightmapData {
                lightmap: Cow::borrowed(lightmap.data()),
            });

            let texture = state.create_texture(
                device,
                queue,
                None,
                lightmap.width(),
                lightmap.height(),
                &lightmap_data,
            );

            let id = self.lightmaps.len();
            self.lightmaps.push(texture);
            //self.lightmap_views
            //.push(self.lightmaps[id].create_view(&Default::default()));
            lightmap_ids.push(id);
        }

        BrushFace {
            vertices: face_vert_id as u32..self.vertices.len() as u32,
            _min: min,
            _max: min,
            texture_id: texinfo.tex_id as usize,
            lightmap_ids,
            _light_styles: face.light_styles,
            draw_flag: true.into(),
        }
    }

    fn create_per_texture_bind_group(
        &self,
        state: &GraphicsState,
        device: &RenderDevice,
        tex: &BrushTextureFrame,
    ) -> BindGroup {
        let layout = &state
            .brush_pipeline()
            .bind_group_layout(BindGroupLayoutId::PerTexture);
        device.create_bind_group(
            Some("per-texture bind group"),
            layout,
            &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(&tex.diffuse.default_view),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::TextureView(&tex.fullbright.default_view),
                },
            ],
        )
    }

    fn create_per_face_bind_group(
        &self,
        state: &GraphicsState,
        device: &RenderDevice,
        face_id: usize,
    ) -> BindGroup {
        let mut lightmap_views: Vec<_> = self.faces[face_id]
            .lightmap_ids
            .iter()
            .map(|id| self.lightmaps[*id].create_view(&Default::default()))
            .collect();
        lightmap_views.resize_with(4, || {
            state.default_lightmap().create_view(&Default::default())
        });

        let lightmap_view_refs = lightmap_views.iter().map(|t| &**t).collect::<Vec<_>>();

        let layout = &state
            .brush_pipeline()
            .bind_group_layout(BindGroupLayoutId::PerFace);
        device.create_bind_group(
            Some("per-face bind group"),
            layout,
            &[wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::TextureViewArray(&lightmap_view_refs[..]),
            }],
        )
    }

    fn create_brush_texture_frame<S>(
        &mut self,
        state: &GraphicsState,
        device: &RenderDevice,
        queue: &RenderQueue,
        mipmap: &[u8],
        width: u32,
        height: u32,
        name: S,
    ) -> BrushTextureFrame
    where
        S: AsRef<str>,
    {
        let name = name.as_ref();

        let (diffuse_data, fullbright_data) = state.palette().translate(mipmap);
        let diffuse = state.create_texture(
            device,
            queue,
            None,
            width,
            height,
            &TextureData::Diffuse(diffuse_data),
        );
        let fullbright = state.create_texture(
            device,
            queue,
            None,
            width,
            height,
            &TextureData::Fullbright(fullbright_data),
        );

        let kind = if name.starts_with("sky") {
            TextureKind::Sky
        } else if name.starts_with("*") {
            TextureKind::Warp
        } else {
            TextureKind::Normal
        };

        let diffuse_view = diffuse.create_view(&default());
        let fullbright_view = fullbright.create_view(&default());

        let mut frame = BrushTextureFrame {
            bind_group_id: 0,
            diffuse: CachedTexture {
                texture: diffuse,
                default_view: diffuse_view,
            },
            fullbright: CachedTexture {
                texture: fullbright,
                default_view: fullbright_view,
            },
            kind,
        };

        // generate texture bind group
        let per_texture_bind_group = self.create_per_texture_bind_group(state, device, &frame);
        let bind_group_id = self.per_texture_bind_groups.len();
        self.per_texture_bind_groups.push(per_texture_bind_group);

        frame.bind_group_id = bind_group_id;
        frame
    }

    pub fn create_brush_texture(
        &mut self,
        state: &GraphicsState,
        device: &RenderDevice,
        queue: &RenderQueue,
        tex: &BspTexture,
    ) -> BrushTexture {
        // TODO: upload mipmaps
        let (width, height) = tex.dimensions();

        match tex.kind() {
            // sequence animated textures
            BspTextureKind::Animated { primary, alternate } => {
                let primary_frames: Vec<_> = primary
                    .iter()
                    .map(|f| {
                        self.create_brush_texture_frame(
                            state,
                            device,
                            queue,
                            f.mipmap(BspTextureMipmap::Full),
                            width,
                            height,
                            tex.name(),
                        )
                    })
                    .collect();

                let alternate_frames: Option<Vec<_>> = alternate.as_ref().map(|a| {
                    a.iter()
                        .map(|f| {
                            self.create_brush_texture_frame(
                                state,
                                device,
                                queue,
                                f.mipmap(BspTextureMipmap::Full),
                                width,
                                height,
                                tex.name(),
                            )
                        })
                        .collect()
                });

                BrushTexture::Animated {
                    primary: primary_frames,
                    alternate: alternate_frames,
                }
            }

            BspTextureKind::Static(bsp_tex) => {
                BrushTexture::Static(self.create_brush_texture_frame(
                    state,
                    device,
                    queue,
                    bsp_tex.mipmap(BspTextureMipmap::Full),
                    tex.width(),
                    tex.height(),
                    tex.name(),
                ))
            }
        }
    }

    pub fn build(
        mut self,
        state: &GraphicsState,
        device: &RenderDevice,
        queue: &RenderQueue,
    ) -> Result<BrushRenderer, Error> {
        // create the diffuse and fullbright textures
        for tex in self.bsp_data.clone().textures().iter() {
            let tex = self.create_brush_texture(state, device, queue, tex);
            self.textures.push(tex);
        }

        // generate faces, vertices and lightmaps
        // bsp_face_id is the id of the face in the bsp data
        // face_id is the new id of the face in the renderer
        for bsp_face_id in self.face_range.start..self.face_range.end {
            let face_id = self.faces.len();
            let face = self.create_face(state, device, queue, bsp_face_id);
            self.faces.push(face);

            let face_tex_id = self.faces[face_id].texture_id;
            // update the corresponding texture chain
            self.texture_chains
                .entry(face_tex_id)
                .or_insert(Vec::new())
                .push(face_id);

            // generate face bind group
            let per_face_bind_group = self.create_per_face_bind_group(state, device, face_id);
            self.per_face_bind_groups.push(per_face_bind_group);
        }

        let vertex_buffer = device.create_buffer_with_data(&wgpu::util::BufferInitDescriptor {
            label: None,
            contents: unsafe { any_slice_as_bytes(self.vertices.as_slice()) },
            usage: wgpu::BufferUsages::VERTEX,
        });

        Ok(BrushRenderer {
            bsp_data: self.bsp_data,
            vertex_buffer,
            leaves: self.leaves,
            per_texture_bind_groups: self.per_texture_bind_groups,
            per_face_bind_groups: self.per_face_bind_groups,
            texture_chains: self.texture_chains,
            faces: self.faces,
            textures: self.textures,
            _lightmaps: self.lightmaps,
        })
    }
}

#[derive(Component)]
pub struct BrushRenderer {
    bsp_data: Arc<BspData>,

    leaves: Option<Vec<BrushLeaf>>,

    vertex_buffer: Buffer,
    per_texture_bind_groups: Vec<BindGroup>,
    per_face_bind_groups: Vec<BindGroup>,

    // faces are grouped by texture to reduce the number of texture rebinds
    // texture_chains maps texture ids to face ids
    texture_chains: HashMap<usize, Vec<usize>>,
    faces: Vec<BrushFace>,
    textures: Vec<BrushTexture>,
    _lightmaps: Vec<Texture>,
}

impl BrushRenderer {
    /// Record the draw commands for this brush model to the given `wgpu::RenderPass`.
    pub fn record_draw<'a>(
        &'a self,
        state: &'a GraphicsState,
        pass: &mut TrackedRenderPass<'a>,
        bump: &'a Bump,
        time: Duration,
        camera: &Camera,
        frame_id: usize,
    ) {
        pass.set_render_pipeline(state.brush_pipeline().pipeline());
        pass.set_vertex_buffer(0, self.vertex_buffer.slice(..));

        // if this is a worldmodel, mark faces to be drawn
        if let Some(ref leaves) = self.leaves {
            let pvs = self
                .bsp_data
                .get_pvs(self.bsp_data.find_leaf(camera.origin), leaves.len());

            // only draw faces in pvs
            for leaf_id in pvs {
                for facelist_id in leaves[leaf_id].facelist_ids.clone() {
                    let face = &self.faces[self.bsp_data.facelist()[facelist_id]];

                    // TODO: frustum culling
                    face.draw_flag.store(true, Ordering::SeqCst);
                }
            }
        }

        for (tex_id, face_ids) in self.texture_chains.iter() {
            use PushConstantUpdate::*;
            BrushPipeline::set_push_constants(
                pass,
                Retain,
                Update(bump.alloc(SharedPushConstants {
                    texture_kind: self.textures[*tex_id].kind() as u32,
                })),
                Retain,
            );

            let bind_group_id = match &self.textures[*tex_id] {
                BrushTexture::Static(frame) => frame.bind_group_id,
                BrushTexture::Animated { primary, alternate } => {
                    // if frame is not zero and this texture has an alternate
                    // animation, use it
                    let anim = if frame_id == 0 {
                        primary
                    } else if let Some(a) = alternate {
                        a
                    } else {
                        primary
                    };

                    let time_ms = time.num_milliseconds();
                    let total_ms = (bsp::frame_duration() * anim.len() as i32).num_milliseconds();
                    let anim_ms = if total_ms == 0 { 0 } else { time_ms % total_ms };
                    anim[(anim_ms / bsp::frame_duration().num_milliseconds()) as usize]
                        .bind_group_id
                }
            };

            pass.set_bind_group(
                BindGroupLayoutId::PerTexture as usize,
                &self.per_texture_bind_groups[bind_group_id],
                &[],
            );

            for face_id in face_ids.iter() {
                let face = &self.faces[*face_id];

                // only skip the face if we have visibility data but it's not marked
                if self.leaves.is_some() && !face.draw_flag.swap(false, Ordering::SeqCst) {
                    continue;
                }

                pass.set_bind_group(
                    BindGroupLayoutId::PerFace as usize,
                    &self.per_face_bind_groups[*face_id],
                    &[],
                );

                pass.draw(face.vertices.clone(), 0..1);
            }
        }
    }
}
