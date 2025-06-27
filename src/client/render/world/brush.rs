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
    mem::{self, size_of},
    ops::Range,
    sync::{
        atomic::{AtomicBool, Ordering}, Arc
    },
};

use crate::{
    client::render::{
        Atlases, Camera, CompiledAtlases, DIFFUSE_TEXTURE_FORMAT, FULLBRIGHT_TEXTURE_FORMAT,
        GraphicsState, LIGHTMAP_TEXTURE_FORMAT, Pipeline,
        mapped::Mapped,
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
    asset::RenderAssetUsages,
    math::vec2,
    prelude::*,
    render::{
        render_phase::TrackedRenderPass,
        render_resource::{BindGroupLayout, BindGroupLayoutEntry, Buffer, RenderPipeline},
        renderer::RenderDevice,
    },
};
use bumpalo::Bump;
use chrono::Duration;
use failure::Error;
use hashbrown::HashMap;
use wgpu::Extent3d;

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
        texture_kind: TextureKind,
        sample_count: u32,
    ) -> BrushPipeline {
        let (pipeline, bind_group_layouts) = BrushPipeline::create(
            device,
            compiler,
            world_bind_group_layouts,
            sample_count,
            ((diffuse_format, normal_format), texture_kind),
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
        kind: TextureKind,
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
            ((diffuse_format, normal_format), kind),
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
    pub transform: [[f32; 4]; 4],
    pub model_view: [[f32; 3]; 3],
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct FramePushConstants {
    pub diffuse_index: u16,
    pub fullbright_index: u16,
}

// TODO: This vertex layout is heinous
const VERTEX_ATTRIBUTES: &[wgpu::VertexAttribute] = &wgpu::vertex_attr_array![
    // position
    0 => Float32x3,
    // normal
    1 => Float32x3,
    // diffuse texcoord
    2 => Float32x2,
    // lightmap animation ids
    3 => Uint8x4,
    // lightmap texcoords 0
    4 => Float32x2,
    // lightmap texcoords 1
    5 => Float32x2,
    // lightmap texcoords 2
    6 => Float32x2,
    // lightmap texcoords 3
    7 => Float32x2,
];

impl Pipeline for BrushPipeline {
    type VertexPushConstants = VertexPushConstants;
    type SharedPushConstants = ();
    type FragmentPushConstants = FramePushConstants;

    type Args = (<WorldPipelineBase as Pipeline>::Args, TextureKind);

    fn name() -> &'static str {
        "brush"
    }

    fn constants(
        (_, texture_kind): &Self::Args,
    ) -> impl Iterator<Item = (Cow<'static, str>, Cow<'static, str>)> {
        std::iter::once((
            "INPUT_TEXTURE_KIND".into(),
            (*texture_kind as usize).to_string().into(),
        ))
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
        vec![]
    }

    fn primitive_state() -> wgpu::PrimitiveState {
        WorldPipelineBase::primitive_state()
    }

    fn color_target_states_with_args(
        (args, _): &Self::Args,
    ) -> Vec<Option<wgpu::ColorTargetState>> {
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
            attributes: VERTEX_ATTRIBUTES,
        }]
    }
}

fn calculate_lightmap_texcoords(position: Vec3, face: &BspFace, texinfo: &BspTexInfo) -> Vec2 {
    let mut s = texinfo.s_vector.dot(position) + texinfo.s_offset;
    s -= (face.texture_mins[0] as f32 / 16.0).floor() * 16.0;
    s += 0.5;
    s /= face.extents[0] as f32;

    let mut t = texinfo.t_vector.dot(position) + texinfo.t_offset;
    t -= (face.texture_mins[1] as f32 / 16.0).floor() * 16.0;
    t += 0.5;
    t /= face.extents[1] as f32;

    vec2(s, t)
}

type Position = [f32; 3];
type Normal = [f32; 3];
type Texcoord = [f32; 2];
type CompressedTexcoord = [u16; 2];
type LightmapAnim = [u8; 4];

fn compress<const N: usize>(input: [f32; N]) -> [u16; N] {
    input.map(|f| (f.clamp(0., 1.) * u16::MAX as f32) as u16)
}

#[repr(C)]
#[derive(Clone, Debug)]
struct BrushVertexInput {
    position: Vec3,
    normal: Vec3,
    diffuse_texcoord: Vec2,
    lightmap_anim: LightmapAnim,
    lightmap_uv: Vec2,
    lightmaps: [AssetId<Image>; 4],
}

#[repr(C)]
#[derive(Default, Clone, Copy, Debug)]
struct BrushVertex {
    position: Position,
    normal: Normal,
    diffuse_texcoord: Texcoord,
    lightmap_anim: LightmapAnim,
    lightmap_texcoords: [Texcoord; 4],
}

#[repr(u32)]
#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub enum TextureKind {
    Normal = 0,
    Warp = 1,
    Sky = 2,
}

/// A single frame of a brush texture.
#[derive(Copy, Clone, Debug)]
pub struct BrushTextureFrame {
    diffuse: AssetId<Image>,
    fullbright: AssetId<Image>,
    kind: TextureKind,
}

/// A brush texture.
#[derive(Clone, Debug)]
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

    fn get_frame(&self, frame_id: usize, time: Duration) -> BrushTextureFrame {
        let idx = (time.num_milliseconds() / bsp::frame_duration().num_milliseconds()) as usize;

        match self {
            Self::Static(frame) => *frame,
            Self::Animated { primary, alternate } => {
                let anim = if frame_id == 0 {
                    primary
                } else if let Some(a) = alternate {
                    a
                } else {
                    primary
                };
                anim[idx % anim.len()]
            }
        }
    }
}

#[derive(Debug)]
struct BrushFace {
    vertices: Range<u32>,
    texture_id: usize,

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

pub struct BrushRendererBuilder<'a> {
    bsp_data: Arc<BspData>,
    state: &'a mut GraphicsState,

    face_range: Range<usize>,

    leaves: Option<Vec<BrushLeaf>>,

    texture_chains: HashMap<TextureKind, HashMap<usize, Vec<usize>>>,

    textures: Vec<BrushTexture>,
    default_lightmap_id: AssetId<Image>,
    //lightmap_views: Vec<TextureView>,
}

impl<'a> BrushRendererBuilder<'a> {
    pub fn new(bsp_model: &'a BspModel, state: &'a mut GraphicsState, worldmodel: bool) -> Self {
        let default_lightmap_id = state.new_lightmap(Image::new(
            Extent3d {
                width: 1,
                height: 1,
                depth_or_array_layers: 1,
            },
            wgpu::TextureDimension::D2,
            vec![u8::MAX],
            LIGHTMAP_TEXTURE_FORMAT,
            RenderAssetUsages::RENDER_WORLD,
        ));

        BrushRendererBuilder {
            bsp_data: bsp_model.bsp_data(),
            state,
            face_range: bsp_model.face_id..bsp_model.face_id + bsp_model.face_count,
            leaves: if worldmodel {
                Some(bsp_model.iter_leaves().map(BrushLeaf::from).collect())
            } else {
                None
            },
            texture_chains: default(),
            textures: default(),
            default_lightmap_id,
        }
    }

    fn create_face(&mut self, vertices: &mut Vec<BrushVertexInput>, face_id: usize) -> BrushFace {
        let face = &self.bsp_data.faces()[face_id];
        let face_vert_id = vertices.len();
        let texinfo = &self.bsp_data.texinfo()[face.texinfo_id];
        let tex = &self.bsp_data.textures()[texinfo.tex_id];

        let mut min = Vec3::INFINITY;
        let mut max = Vec3::NEG_INFINITY;

        let no_collinear =
            math::remove_collinear(self.bsp_data.face_iter_vertices(face_id).collect());

        for vert in no_collinear.iter() {
            for component in 0..3 {
                min[component] = min[component].min(vert[component]);
                max[component] = max[component].max(vert[component]);
            }
        }

        // build the lightmaps
        let lightmaps = if !texinfo.special {
            self.bsp_data.face_lightmaps(face_id)
        } else {
            Vec::new()
        };

        let mut lightmap_ids = [self.default_lightmap_id; 4];
        for (lightmap, id) in lightmaps.into_iter().zip(&mut lightmap_ids) {
            *id = self.state.new_lightmap(Image::new(
                Extent3d {
                    width: lightmap.width(),
                    height: lightmap.height(),
                    depth_or_array_layers: 1,
                },
                wgpu::TextureDimension::D2,
                lightmap.data().to_vec(),
                LIGHTMAP_TEXTURE_FORMAT,
                RenderAssetUsages::RENDER_WORLD,
            ));
        }

        if tex.name().starts_with("*") {
            // tessellate the surface so we can do texcoord warping
            let verts = warp::subdivide(no_collinear);
            let normal = match &*verts {
                [a, b, c, ..] => (a - b).cross(c - b).normalize(),
                _ => Vec3::ZERO,
            };
            for vert in verts.into_iter() {
                let lightmap_texcoords = calculate_lightmap_texcoords(vert, face, texinfo);
                let position: [f32; 3] = vert.into();
                let normal: [f32; 3] = normal.into();
                vertices.push(BrushVertexInput {
                    position: position.into(),
                    normal: normal.into(),
                    diffuse_texcoord: vec2(
                        (vert.dot(texinfo.s_vector) + texinfo.s_offset) / tex.width() as f32,
                        (vert.dot(texinfo.t_vector) + texinfo.t_offset) / tex.height() as f32,
                    ),
                    lightmaps: lightmap_ids,
                    lightmap_uv: lightmap_texcoords,
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
                _ => Vec3::ZERO,
            };
            let mut vert_iter = verts.into_iter();

            let v1 = vert_iter.next().unwrap();
            let mut v2 = vert_iter.next().unwrap();
            for v3 in vert_iter {
                let tri = [v1, v2, v3];

                // skip collinear points
                for vert in tri {
                    let lightmap_texcoords = calculate_lightmap_texcoords(vert, face, texinfo);
                    let position: [f32; 3] = vert.into();
                    let normal: [f32; 3] = normal.into();
                    vertices.push(BrushVertexInput {
                        position: position.into(),
                        normal: normal.into(),
                        diffuse_texcoord: vec2(
                            (vert.dot(texinfo.s_vector) + texinfo.s_offset) / tex.width() as f32,
                            (vert.dot(texinfo.t_vector) + texinfo.t_offset) / tex.height() as f32,
                        ),
                        lightmaps: lightmap_ids,
                        lightmap_uv: lightmap_texcoords,
                        lightmap_anim: face.light_styles,
                    });
                }

                v2 = v3;
            }
        }

        BrushFace {
            vertices: face_vert_id as u32..vertices.len() as u32,
            texture_id: texinfo.tex_id,
            draw_flag: true.into(),
        }
    }

    fn create_brush_texture_frame<S>(
        &mut self,
        mipmap: &[u8],
        width: u32,
        height: u32,
        name: S,
    ) -> BrushTextureFrame
    where
        S: AsRef<str>,
    {
        let name = name.as_ref();

        let (diffuse_data, fullbright_data) = self.state.palette().translate(mipmap);
        let diffuse = self.state.new_diffuse(Image::new(
            Extent3d {
                width,
                height,
                depth_or_array_layers: 1,
            },
            wgpu::TextureDimension::D2,
            diffuse_data.rgba,
            DIFFUSE_TEXTURE_FORMAT,
            RenderAssetUsages::RENDER_WORLD,
        ));
        let fullbright = self.state.new_fullbright(Image::new(
            Extent3d {
                width,
                height,
                depth_or_array_layers: 1,
            },
            wgpu::TextureDimension::D2,
            fullbright_data.fullbright,
            FULLBRIGHT_TEXTURE_FORMAT,
            RenderAssetUsages::RENDER_WORLD,
        ));

        let kind = if name.starts_with("sky") {
            TextureKind::Sky
        } else if name.starts_with("*") {
            TextureKind::Warp
        } else {
            TextureKind::Normal
        };

        BrushTextureFrame {
            diffuse,
            fullbright,
            kind,
        }
    }

    pub fn create_brush_texture(&mut self, tex: &BspTexture) -> BrushTexture {
        // TODO: upload mipmaps
        let (width, height) = tex.dimensions();

        match tex.kind() {
            // sequence animated textures
            BspTextureKind::Animated { primary, alternate } => {
                let primary_frames: Vec<_> = primary
                    .iter()
                    .map(|f| {
                        self.create_brush_texture_frame(
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
                    bsp_tex.mipmap(BspTextureMipmap::Full),
                    tex.width(),
                    tex.height(),
                    tex.name(),
                ))
            }
        }
    }

    pub fn build(mut self) -> Result<BrushRenderer, Error> {
        // create the diffuse and fullbright textures
        for tex in self.bsp_data.clone().textures().iter() {
            let tex = self.create_brush_texture(tex);
            self.textures.push(tex);
        }

        let mut vertices = Vec::new();

        // generate faces, vertices and lightmaps
        // bsp_face_id is the id of the face in the bsp data
        // face_id is the new id of the face in the renderer
        let faces = (self.face_range.start..self.face_range.end)
            .map(|bsp_face_id| {
                let face = self.create_face(&mut vertices, bsp_face_id);

                let face_tex_id = face.texture_id;
                // update the corresponding texture chain
                self.texture_chains
                    .entry(self.textures[face_tex_id].kind())
                    .or_default()
                    .entry(face_tex_id)
                    .or_default()
                    .push(bsp_face_id);

                (bsp_face_id, face)
            })
            .collect();

        let out_vertices = vec![default(); vertices.len()];

        Ok(BrushRenderer {
            bsp_data: self.bsp_data.clone(),
            leaves: self.leaves,
            vertex_buffer: None,
            vertices: Mapped::new(
                vertices,
                out_vertices,
                |Atlases { lightmap, .. }, in_brush_vertex| {
                    let lightmap_texcoords: [Texcoord; 4] = in_brush_vertex
                        .lightmaps
                        .map(|id| lightmap.translate(id, in_brush_vertex.lightmap_uv).into());

                    BrushVertex {
                        position: in_brush_vertex.position.into(),
                        normal: in_brush_vertex.normal.into(),
                        diffuse_texcoord: in_brush_vertex.diffuse_texcoord.into(),
                        lightmap_anim: in_brush_vertex.lightmap_anim,
                        lightmap_texcoords,
                    }
                } as _,
            ),
            texture_chains: self.texture_chains,
            faces,
            textures: self.textures,
        })
    }
}

#[derive(Component)]
pub struct BrushRenderer {
    bsp_data: Arc<BspData>,

    leaves: Option<Vec<BrushLeaf>>,

    vertex_buffer: Option<Buffer>,

    vertices: Mapped<CompiledAtlases, BrushVertexInput, BrushVertex>,

    // faces are grouped by texture to reduce the number of texture rebinds
    // texture_chains maps texture ids to face ids
    texture_chains: HashMap<TextureKind, HashMap<usize, Vec<usize>>>,
    faces: HashMap<usize, BrushFace>,
    textures: Vec<BrushTexture>,
}

impl BrushRenderer {
    pub fn update_vertices(&mut self, meta: &CompiledAtlases, device: &RenderDevice) {
        self.vertices.update(meta);
        self.vertex_buffer = Some(device.create_buffer_with_data(
            &wgpu::util::BufferInitDescriptor {
                label: None,
                contents: unsafe { any_slice_as_bytes(self.vertices.outputs()) },
                usage: wgpu::BufferUsages::VERTEX,
            },
        ));
    }

    /// Record the draw commands for this brush model to the given `wgpu::RenderPass`.
    pub fn record_draw<'a, F>(
        &'a self,
        state: &'a GraphicsState,
        pass: &mut TrackedRenderPass<'a>,
        bump: &'a Bump,
        time: Duration,
        camera: &Camera,
        frame_id: usize,
        mut init_pass: F,
    ) where
        F: FnMut(&mut TrackedRenderPass<'a>, TextureKind),
    {
        use PushConstantUpdate::*;

        assert_eq!(mem::size_of::<<BrushPipeline as Pipeline>::VertexPushConstants>(), 100);

        let Some(vbuf) = self.vertex_buffer.as_ref() else {
            return;
        };

        pass.set_vertex_buffer(0, vbuf.slice(..));

        // if this is a worldmodel, mark faces to be drawn
        if let Some(leaves) = &self.leaves {
            let pvs = self
                .bsp_data
                .get_pvs(self.bsp_data.find_leaf(camera.origin), leaves.len());

            // only draw faces in pvs
            for leaf_id in pvs {
                for facelist_id in leaves[leaf_id].facelist_ids.clone() {
                    let face = &self.faces[&self.bsp_data.facelist()[facelist_id]];

                    // TODO: frustum culling
                    face.draw_flag.store(true, Ordering::SeqCst);
                }
            }
        }

        for (tex_kind, face_id_map) in self.texture_chains.iter() {
            init_pass(pass, *tex_kind);

            for (tex_id, face_ids) in face_id_map {
                let texture = self.textures[*tex_id].get_frame(frame_id, time);

                let diffuse_index =
                    state.compiled_atlases().unwrap().diffuse.layout[&texture.diffuse].0 as u16;
                let fullbright_index = state.compiled_atlases().unwrap().fullbright.layout
                    [&texture.fullbright]
                    .0 as u16;

                BrushPipeline::set_push_constants(
                    pass,
                    Retain,
                    Retain,
                    Update(bump.alloc(FramePushConstants {
                        diffuse_index,
                        fullbright_index,
                    })),
                );

                for face_id in face_ids {
                    let face = &self.faces[face_id];
                    if self.leaves.is_some() && !face.draw_flag.swap(false, Ordering::SeqCst) {
                        continue;
                    }

                    pass.draw(face.vertices.clone(), 0..1);
                }
            }
        }
        // }
    }
}
