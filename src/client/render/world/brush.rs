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
        Camera, GraphicsState, LIGHTMAP_TEXTURE_FORMAT, LightmapData, Pipeline, TextureData,
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

use bevy::{
    asset::RenderAssetUsages,
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
use uuid::Uuid;
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

static VERTEX_ATTRIBUTES: [wgpu::VertexAttribute; 8] = wgpu::vertex_attr_array![
    // position
    0 => Float32x3,
    // normal
    1 => Float32x3,
    // diffuse texcoord
    2 => Float32x2,
    // lightmap animation ids
    3 => Uint8x4,
    // lightmap texcoord 0
    4 => Float32x2,
    // lightmap texcoord 1
    5 => Float32x2,
    // lightmap texcoord 2
    6 => Float32x2,
    // lightmap texcoord 3
    7 => Float32x2,
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
    lightmap_anim: LightmapAnim,
    lightmap_texcoords: [LightmapTexcoord; 4],
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

    lightmap_ids: [AssetId<Image>; 4],
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

    vertices: Vec<BrushVertex>,
    texture_chains: HashMap<usize, Vec<usize>>,
    // diffuse_atlas: TextureAtlasBuilder<'a>,
    // fullbright_atlas: TextureAtlasBuilder<'a>,
    textures: Vec<BrushTexture>,
    default_lightmap_id: AssetId<Image>,
    //lightmap_views: Vec<TextureView>,
}

impl BrushRendererBuilder {
    pub fn new(bsp_model: &BspModel, worldmodel: bool) -> Self {
        let default_lightmap_id = AssetId::<Image>::from(Uuid::new_v4());

        BrushRendererBuilder {
            bsp_data: bsp_model.bsp_data(),
            face_range: bsp_model.face_id..bsp_model.face_id + bsp_model.face_count,
            leaves: if worldmodel {
                Some(bsp_model.iter_leaves().map(BrushLeaf::from).collect())
            } else {
                None
            },
            per_texture_bind_groups: default(),
            vertices: default(),
            texture_chains: default(),
            textures: default(),
            default_lightmap_id,
        }
    }

    fn create_face<'a>(
        bsp_data: &'a BspData,
        vertices: &mut Vec<BrushVertex>,
        default_lightmap_id: AssetId<Image>,
        face_id: usize,
    ) -> (
        BrushFace,
        impl Iterator<Item = AssetId<Image>> + Clone + use<>,
        impl Iterator<Item = Image> + use<'a>,
    ) {
        let face = &bsp_data.faces()[face_id];
        let face_vert_id = vertices.len();
        let texinfo = &bsp_data.texinfo()[face.texinfo_id];
        let tex = &bsp_data.textures()[texinfo.tex_id];

        let mut min = Vector3::new(f32::INFINITY, f32::INFINITY, f32::INFINITY);
        let mut max = Vector3::new(f32::NEG_INFINITY, f32::NEG_INFINITY, f32::NEG_INFINITY);

        let no_collinear = math::remove_collinear(bsp_data.face_iter_vertices(face_id).collect());

        for vert in no_collinear.iter() {
            for component in 0..3 {
                min[component] = min[component].min(vert[component]);
                max[component] = max[component].max(vert[component]);
            }
        }

        // build the lightmaps
        let lightmaps = if !texinfo.special {
            bsp_data.face_lightmaps(face_id)
        } else {
            Vec::new()
        };

        let mut lightmap_ids = [default_lightmap_id; 4];
        for lightmap_id in &mut lightmap_ids[..lightmaps.len()] {
            *lightmap_id = AssetId::<Image>::from(Uuid::new_v4());
        }
        let lightmaps = lightmaps.into_iter().map(|lightmap| {
            Image::new(
                Extent3d {
                    width: lightmap.width(),
                    height: lightmap.height(),
                    depth_or_array_layers: 1,
                },
                wgpu::TextureDimension::D2,
                lightmap.data().to_vec(),
                LIGHTMAP_TEXTURE_FORMAT,
                RenderAssetUsages::RENDER_WORLD,
            )
        });

        if tex.name().starts_with("*") {
            // tessellate the surface so we can do texcoord warping
            let verts = warp::subdivide(no_collinear);
            let normal = match &*verts {
                [a, b, c, ..] => (a - b).cross(c - b).normalize(),
                _ => Vector3::zero(),
            };
            for vert in verts.into_iter() {
                let lightmap_texcoords = calculate_lightmap_texcoords(vert, face, texinfo);
                vertices.push(BrushVertex {
                    position: vert.into(),
                    normal: normal.into(),
                    diffuse_texcoord: [
                        ((vert.dot(texinfo.s_vector) + texinfo.s_offset) / tex.width() as f32),
                        ((vert.dot(texinfo.t_vector) + texinfo.t_offset) / tex.height() as f32),
                    ],
                    lightmap_texcoords: [lightmap_texcoords, default(), default(), default()],
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
                    let lightmap_texcoords =
                        calculate_lightmap_texcoords((*vert).into(), face, texinfo);
                    vertices.push(BrushVertex {
                        position: (*vert).into(),
                        normal: normal.into(),
                        diffuse_texcoord: [
                            ((vert.dot(texinfo.s_vector) + texinfo.s_offset) / tex.width() as f32),
                            ((vert.dot(texinfo.t_vector) + texinfo.t_offset) / tex.height() as f32),
                        ],
                        lightmap_texcoords: [lightmap_texcoords, default(), default(), default()],
                        lightmap_anim: face.light_styles,
                    });
                }

                v2 = v3;
            }
        }

        (
            BrushFace {
                vertices: face_vert_id as u32..vertices.len() as u32,
                _min: min,
                _max: min,
                texture_id: texinfo.tex_id,
                lightmap_ids,
                _light_styles: face.light_styles,
                draw_flag: true.into(),
            },
            lightmap_ids.into_iter(),
            lightmaps,
        )
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

    fn create_lightmap_bind_group(
        &self,
        state: &GraphicsState,
        device: &RenderDevice,
        lightmap_tex: &Texture,
    ) -> BindGroup {
        let lightmap_view = lightmap_tex.create_view(&Default::default());

        let layout = &state
            .brush_pipeline()
            .bind_group_layout(BindGroupLayoutId::Lightmap);
        device.create_bind_group(
            Some("lightmap bind group"),
            layout,
            &[wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::TextureView(&lightmap_view),
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

        let default_lightmap = Image::new(
            Extent3d {
                width: 1,
                height: 1,
                depth_or_array_layers: 1,
            },
            wgpu::TextureDimension::D2,
            vec![0xFF],
            LIGHTMAP_TEXTURE_FORMAT,
            RenderAssetUsages::RENDER_WORLD,
        );

        // Max upper bound of number of lightmaps
        let mut lightmaps = Vec::with_capacity((self.face_range.end - self.face_range.start) * 4);

        let mut vertices = Vec::new();

        // generate faces, vertices and lightmaps
        // bsp_face_id is the id of the face in the bsp data
        // face_id is the new id of the face in the renderer
        let faces_with_lightmap_ids = (self.face_range.start..self.face_range.end)
            .map(|bsp_face_id| {
                let (face, lightmap_ids, face_lightmaps) = Self::create_face(
                    &self.bsp_data,
                    &mut vertices,
                    self.default_lightmap_id,
                    bsp_face_id,
                );

                lightmaps.extend(lightmap_ids.clone().zip(face_lightmaps));

                let face_tex_id = face.texture_id;
                // update the corresponding texture chain
                self.texture_chains
                    .entry(face_tex_id)
                    .or_default()
                    .push(bsp_face_id);

                (bsp_face_id, face, lightmap_ids)
            })
            .collect::<Vec<_>>();

        let mut lightmap_atlas = TextureAtlasBuilder::default();
        lightmap_atlas.format(LIGHTMAP_TEXTURE_FORMAT);
        lightmap_atlas.padding(UVec2 { x: 2, y: 2 });

        lightmap_atlas.add_texture(Some(self.default_lightmap_id), &default_lightmap);

        for (id, lightmap) in lightmaps.iter() {
            lightmap_atlas.add_texture(Some(*id), lightmap);
        }

        let (lightmap_layout, lightmap_id_map, lightmap_image) = lightmap_atlas.build()?;

        let lightmap = state.create_texture(
            device,
            queue,
            Some("lightmap"),
            lightmap_image.size().x,
            lightmap_image.size().y,
            &TextureData::Lightmap(LightmapData {
                lightmap: lightmap_image.data[..].into(),
            }),
        );

        // generate face bind group
        let lightmap_bind_group = self.create_lightmap_bind_group(state, device, &lightmap);

        let get_lightmap_bounds = |id, coords: Vec2| {
            let lightmap_size = lightmap_layout.size;
            let raw_coords = lightmap_layout.textures[lightmap_id_map.texture_ids[&id]];
            let real_coords = Rect {
                min: raw_coords.min.as_vec2() / lightmap_size.as_vec2(),
                max: raw_coords.max.as_vec2() / lightmap_size.as_vec2(),
            };

            coords * (real_coords.max - real_coords.min) + real_coords.min
        };

        let faces = faces_with_lightmap_ids
            .into_iter()
            .map(|(face_id, brush_face, lightmap_ids)| {
                for i in brush_face.vertices.clone() {
                    let i = i as usize;
                    let mut mapped_texcoords: [[f32; 2]; 4] = default();
                    for (id, texcoord) in lightmap_ids.clone().zip(&mut mapped_texcoords) {
                        *texcoord =
                            get_lightmap_bounds(id, vertices[i].lightmap_texcoords[0].into())
                                .into();
                    }
                    vertices[i].lightmap_texcoords = mapped_texcoords;
                }
                (face_id, brush_face)
            })
            .collect();

        let vertex_buffer = device.create_buffer_with_data(&wgpu::util::BufferInitDescriptor {
            label: None,
            contents: unsafe { any_slice_as_bytes(vertices.as_slice()) },
            usage: wgpu::BufferUsages::VERTEX,
        });

        Ok(BrushRenderer {
            bsp_data: self.bsp_data.clone(),
            vertex_buffer,
            leaves: self.leaves,
            per_texture_bind_groups: self.per_texture_bind_groups,
            lightmap_bind_group,
            texture_chains: self.texture_chains,
            faces,
            textures: self.textures,
            _lightmap: lightmap,
        })
    }
}

#[derive(Component)]
pub struct BrushRenderer {
    bsp_data: Arc<BspData>,

    leaves: Option<Vec<BrushLeaf>>,

    vertex_buffer: Buffer,
    per_texture_bind_groups: Vec<BindGroup>,
    lightmap_bind_group: BindGroup,

    // faces are grouped by texture to reduce the number of texture rebinds
    // texture_chains maps texture ids to face ids
    texture_chains: HashMap<usize, Vec<usize>>,
    faces: HashMap<usize, BrushFace>,
    textures: Vec<BrushTexture>,
    _lightmap: Texture,
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
                    let face = &self.faces[&self.bsp_data.facelist()[facelist_id]];

                    // TODO: frustum culling
                    face.draw_flag.store(true, Ordering::SeqCst);
                }
            }
        }

        pass.set_bind_group(
            BindGroupLayoutId::Lightmap as usize,
            &self.lightmap_bind_group,
            &[],
        );

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
                let face = &self.faces[face_id];

                // only skip the face if we have visibility data but it's not marked
                if self.leaves.is_some() && !face.draw_flag.swap(false, Ordering::SeqCst) {
                    continue;
                }

                pass.draw(face.vertices.clone(), 0..1);
            }
        }
    }
}
