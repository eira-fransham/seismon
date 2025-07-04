pub mod alias;
pub mod brush;
pub mod deferred;
pub mod particle;
pub mod postprocess;
pub mod sprite;

use std::mem::size_of;

use crate::{
    client::{
        ClientEntity, ConnectionState,
        entity::particle::Particle,
        render::{
            GraphicsState,
            pipeline::{Pipeline, PushConstantUpdate},
            uniform::{DynamicUniformBufferBlock, UniformBool},
            world::{
                alias::{AliasPipeline, AliasRenderer},
                brush::{BrushPipeline, BrushRenderer, BrushRendererBuilder},
                sprite::{SpritePipeline, SpriteRenderer},
            },
        },
    },
    common::{
        engine,
        math::Angles,
        model::{Model, ModelKind},
        sprite::SpriteKind,
        util::any_as_bytes,
    },
};

use bevy::{
    core_pipeline::core_3d::CORE_3D_DEPTH_FORMAT,
    prelude::*,
    render::{
        render_phase::TrackedRenderPass,
        render_resource::BindGroupLayoutEntry,
        renderer::{RenderDevice, RenderQueue},
    },
};
use bumpalo::Bump;
use chrono::Duration;
use parking_lot::RwLock;

use super::RenderVars;

pub const BIND_GROUP_LAYOUT_DESCRIPTORS: &[&[wgpu::BindGroupLayoutEntry]] = &[
    &[wgpu::BindGroupLayoutEntry {
        binding: 0,
        visibility: wgpu::ShaderStages::all(),
        ty: wgpu::BindingType::Buffer {
            ty: wgpu::BufferBindingType::Uniform,
            has_dynamic_offset: false,
            min_binding_size: std::num::NonZeroU64::new(size_of::<FrameUniforms>() as u64),
        },
        count: None,
    }],
    &[
        // transform matrix
        // TODO: move this to push constants once they're exposed in wgpu
        wgpu::BindGroupLayoutEntry {
            binding: 0,
            visibility: wgpu::ShaderStages::VERTEX,
            ty: wgpu::BindingType::Buffer {
                ty: wgpu::BufferBindingType::Uniform,
                has_dynamic_offset: true,
                min_binding_size: std::num::NonZeroU64::new(size_of::<EntityUniforms>() as u64),
            },
            count: None,
        },
        // diffuse and fullbright sampler
        wgpu::BindGroupLayoutEntry {
            binding: 1,
            visibility: wgpu::ShaderStages::FRAGMENT,
            ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
            count: None,
        },
        // lightmap sampler
        wgpu::BindGroupLayoutEntry {
            binding: 2,
            visibility: wgpu::ShaderStages::FRAGMENT,
            ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
            count: None,
        },
        // diffuse texture array
        wgpu::BindGroupLayoutEntry {
            binding: 3,
            visibility: wgpu::ShaderStages::FRAGMENT,
            ty: wgpu::BindingType::Texture {
                view_dimension: wgpu::TextureViewDimension::D2Array,
                sample_type: wgpu::TextureSampleType::Float { filterable: true },
                multisampled: false,
            },
            count: None,
        },
        // fullbright texture array
        wgpu::BindGroupLayoutEntry {
            binding: 4,
            visibility: wgpu::ShaderStages::FRAGMENT,
            ty: wgpu::BindingType::Texture {
                view_dimension: wgpu::TextureViewDimension::D2Array,
                sample_type: wgpu::TextureSampleType::Float { filterable: true },
                multisampled: false,
            },
            count: None,
        },
        // lightmap texture atlas
        wgpu::BindGroupLayoutEntry {
            binding: 5,
            visibility: wgpu::ShaderStages::FRAGMENT,
            ty: wgpu::BindingType::Texture {
                view_dimension: wgpu::TextureViewDimension::D2,
                sample_type: wgpu::TextureSampleType::Float { filterable: true },
                multisampled: false,
            },
            count: None,
        },
    ],
];

pub struct WorldPipelineBase;

impl Pipeline for WorldPipelineBase {
    type VertexPushConstants = ();
    type SharedPushConstants = ();
    type FragmentPushConstants = ();

    type Args = (wgpu::TextureFormat, wgpu::TextureFormat);

    fn name() -> &'static str {
        "world"
    }

    fn vertex_shader() -> &'static str {
        ""
    }

    fn fragment_shader() -> &'static str {
        ""
    }

    fn bind_group_layout_descriptors() -> Vec<Vec<BindGroupLayoutEntry>> {
        // TODO
        vec![]
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

    fn color_target_states_with_args(
        (diffuse_format, normal_format): &Self::Args,
    ) -> Vec<Option<wgpu::ColorTargetState>> {
        vec![
            // diffuse attachment
            Some(wgpu::ColorTargetState {
                format: *diffuse_format,
                blend: Some(wgpu::BlendState::REPLACE),
                write_mask: wgpu::ColorWrites::ALL,
            }),
            // normal attachment
            Some(wgpu::ColorTargetState {
                format: *normal_format,
                blend: Some(wgpu::BlendState::REPLACE),
                write_mask: wgpu::ColorWrites::ALL,
            }),
        ]
    }

    fn depth_stencil_state() -> Option<wgpu::DepthStencilState> {
        Some(wgpu::DepthStencilState {
            format: CORE_3D_DEPTH_FORMAT,
            depth_write_enabled: true,
            depth_compare: wgpu::CompareFunction::LessEqual,
            stencil: wgpu::StencilState {
                front: wgpu::StencilFaceState::IGNORE,
                back: wgpu::StencilFaceState::IGNORE,
                read_mask: 0,
                write_mask: 0,
            },
            bias: wgpu::DepthBiasState {
                constant: 0,
                slope_scale: 0.0,
                clamp: 0.0,
            },
        })
    }

    fn vertex_buffer_layouts() -> Vec<wgpu::VertexBufferLayout<'static>> {
        Vec::new()
    }
}

#[derive(Clone, Copy, Debug)]
pub enum BindGroupLayoutId {
    PerFrame = 0,
    PerEntity = 1,
    PerTexture = 2,
}

pub struct Camera {
    origin: Vec3,
    angles: Angles,
    view: Mat4,
    view_projection: Mat4,
    projection: Mat4,
    inverse_projection: Mat4,
    clipping_planes: [Vec4; 6],
}

impl Camera {
    pub fn new(origin: Vec3, angles: Angles, projection: Mat4) -> Camera {
        // convert coordinates
        let converted_origin = Vec3::new(-origin.y, origin.z, -origin.x);

        // translate the world by inverse of camera position
        let translation = Mat4::from_translation(-converted_origin);
        let rotation = angles.mat4_wgpu();
        let view = rotation * translation;
        let view_projection = projection * view;

        // see https://www.gamedevs.org/uploads/fast-extraction-viewing-frustum-planes-from-world-view-projection-matrix.pdf
        let clipping_planes = [
            // left
            view_projection.w_axis + view_projection.x_axis,
            // right
            view_projection.w_axis - view_projection.x_axis,
            // bottom
            view_projection.w_axis + view_projection.y_axis,
            // top
            view_projection.w_axis - view_projection.y_axis,
            // near
            view_projection.w_axis + view_projection.z_axis,
            // far
            view_projection.w_axis - view_projection.z_axis,
        ];

        Camera {
            origin,
            angles,
            view,
            view_projection,
            projection,
            inverse_projection: projection.inverse(),
            clipping_planes,
        }
    }

    pub fn origin(&self) -> Vec3 {
        self.origin
    }

    pub fn angles(&self) -> Angles {
        self.angles
    }

    pub fn view(&self) -> Mat4 {
        self.view
    }

    pub fn view_projection(&self) -> Mat4 {
        self.view_projection
    }

    pub fn projection(&self) -> Mat4 {
        self.projection
    }

    pub fn inverse_projection(&self) -> Mat4 {
        self.inverse_projection
    }

    // TODO: this seems to be too lenient
    /// Determines whether a point falls outside the viewing frustum.
    pub fn cull_point(&self, p: Vec3) -> bool {
        for plane in self.clipping_planes.iter() {
            if (self.view_projection() * p.extend(1.0)).dot(*plane) < 0.0 {
                return true;
            }
        }

        false
    }
}

#[repr(C, align(256))]
#[derive(Copy, Clone, Debug)]
pub struct FrameUniforms {
    lightmap_anim_frames: [Vec4; 16],
    camera_pos: Vec4,
    time: f32,
    sky_time: f32,

    // TODO: pack flags into a bit string
    r_lightmap: UniformBool,
}

#[repr(C, align(256))]
#[derive(Clone, Copy, Debug)]
pub struct EntityUniforms {
    /// Model-view-projection transform matrix
    transform: Mat4,

    /// Model-only transform matrix
    model: Mat4,
}

enum EntityRenderer {
    Alias(AliasRenderer),
    Brush(BrushRenderer),
    Sprite(SpriteRenderer),
    None,
}

static NO_ENTITY_RENDERER: EntityRenderer = EntityRenderer::None;

/// Top-level renderer.
#[derive(Resource)]
pub struct WorldRenderer {
    worldmodel_renderer: BrushRenderer,
    entity_renderers: Vec<EntityRenderer>,

    world_uniform_block: DynamicUniformBufferBlock<EntityUniforms>,
    entity_uniform_blocks: RwLock<Vec<DynamicUniformBufferBlock<EntityUniforms>>>,
}

pub fn extract_world_renderer(
    mut commands: Commands,
    world_renderer: Option<ResMut<WorldRenderer>>,
    mut gfx_state: ResMut<GraphicsState>,
    device: Res<RenderDevice>,
    queue: Res<RenderQueue>,
    game_state: Res<ConnectionState>,
) {
    info!("Updating world renderer");
    match &*game_state {
        ConnectionState::Connected(state) => {
            let Some(new_renderer) = WorldRenderer::new(
                &mut gfx_state,
                &device,
                &queue,
                state.model_precache.iter(),
                state.worldmodel_id,
            ) else {
                return;
            };
            match world_renderer {
                // TODO: Actually track changes to the connection
                Some(mut world_renderer) => *world_renderer = new_renderer,
                None => commands.insert_resource(new_renderer),
            }
        }
        _ => {
            commands.remove_resource::<WorldRenderer>();
        }
    }
}

impl WorldRenderer {
    pub fn new<'a, M: Iterator<Item = &'a Model>>(
        state: &'a mut GraphicsState,
        device: &RenderDevice,
        queue: &RenderQueue,
        models: M,
        worldmodel_id: usize,
    ) -> Option<WorldRenderer> {
        let mut worldmodel_renderer = None;
        let mut entity_renderers = Vec::new();

        let world_uniform_block = state.entity_uniform_buffer_mut().allocate(EntityUniforms {
            transform: Mat4::IDENTITY,
            model: Mat4::IDENTITY,
        });

        for (i, model) in models.enumerate() {
            if i == worldmodel_id {
                match model.kind() {
                    ModelKind::Brush(bmodel) => {
                        worldmodel_renderer = Some(
                            BrushRendererBuilder::new(bmodel, state, true)
                                .build()
                                .unwrap(),
                        );
                    }
                    _ => panic!("Invalid worldmodel"),
                }
            } else {
                match model.kind() {
                    ModelKind::Alias(amodel) => entity_renderers.push(EntityRenderer::Alias(
                        AliasRenderer::new(state, device, queue, amodel).unwrap(),
                    )),

                    ModelKind::Brush(bmodel) => {
                        entity_renderers.push(EntityRenderer::Brush(
                            BrushRendererBuilder::new(bmodel, state, false)
                                .build()
                                .unwrap(),
                        ));
                    }

                    ModelKind::Sprite(smodel) => {
                        entity_renderers.push(EntityRenderer::Sprite(SpriteRenderer::new(
                            state, device, queue, smodel,
                        )));
                    }

                    _ => {
                        warn!("Non-brush renderers not implemented!");
                        entity_renderers.push(EntityRenderer::None);
                    }
                }
            }
        }

        match state.rebuild_atlases(device, queue) {
            Ok(atlases) => {
                if let Some(world) = worldmodel_renderer.as_mut() {
                    world.update_vertices(&atlases, device);
                }

                for ent in entity_renderers.iter_mut().filter_map(|e| {
                    if let EntityRenderer::Brush(b) = e {
                        Some(b)
                    } else {
                        None
                    }
                }) {
                    ent.update_vertices(&atlases, device);
                }
            }
            Err(e) => error!("{e}"),
        }

        Some(WorldRenderer {
            worldmodel_renderer: worldmodel_renderer?,
            entity_renderers,
            world_uniform_block,
            entity_uniform_blocks: Default::default(),
        })
    }

    pub fn update_uniform_buffers<'a, I>(
        &'a self,
        state: &GraphicsState,
        queue: &RenderQueue,
        camera: &Camera,
        time: Duration,
        entities: I,
        lightstyle_values: &[f32],
        render_vars: &RenderVars,
    ) where
        I: Iterator<Item = &'a ClientEntity>,
    {
        trace!("Updating frame uniform buffer");
        let time_secs = engine::duration_to_f32(time);
        let uniforms = FrameUniforms {
            lightmap_anim_frames: {
                let mut frames = [Vec4::X; 16];
                for i in 0..16 {
                    for j in 0..4 {
                        frames[i] = Vec4::new(
                            lightstyle_values[i * j],
                            lightstyle_values[i * j + 1],
                            lightstyle_values[i * j + 2],
                            lightstyle_values[i * j + 3],
                        );
                    }
                }
                frames
            },
            camera_pos: camera.origin.extend(1.0),
            time: time_secs,
            sky_time: time_secs * render_vars.sky_scroll_speed as f32,
            r_lightmap: UniformBool::new(render_vars.lightmap != 0),
        };
        queue.write_buffer(state.frame_uniform_buffer(), 0, unsafe {
            any_as_bytes(&uniforms)
        });

        trace!("Updating entity uniform buffer");
        let world_uniforms = EntityUniforms {
            transform: camera.view_projection(),
            model: Mat4::IDENTITY,
        };
        state
            .entity_uniform_buffer_mut()
            .write_block(&self.world_uniform_block, world_uniforms);

        for (ent_pos, ent) in entities.into_iter().enumerate() {
            let ent_uniforms = EntityUniforms {
                transform: self.calculate_mvp_transform(camera, ent),
                model: self.calculate_model_transform(camera, ent),
            };

            if ent_pos >= self.entity_uniform_blocks.read().len() {
                // if we don't have enough blocks, get a new one
                let block = state.entity_uniform_buffer_mut().allocate(ent_uniforms);
                self.entity_uniform_blocks.write().push(block);
            } else {
                state
                    .entity_uniform_buffer_mut()
                    .write_block(&self.entity_uniform_blocks.read()[ent_pos], ent_uniforms);
            }
        }

        state.entity_uniform_buffer().flush(queue);
    }

    pub fn render_pass<'a, E, P>(
        &'a self,
        state: &'a GraphicsState,
        pass: &mut TrackedRenderPass<'a>,
        bump: &'a Bump,
        camera: &Camera,
        time: Duration,
        entities: E,
        particles: P,
        viewmodel_id: Option<usize>,
    ) where
        E: Iterator<Item = &'a ClientEntity>,
        P: Iterator<Item = &'a Particle>,
    {
        use PushConstantUpdate::*;

        pass.set_bind_group(
            BindGroupLayoutId::PerFrame as usize,
            &state.world_bind_groups()[BindGroupLayoutId::PerFrame as usize],
            &[],
        );

        self.worldmodel_renderer
            .record_draw(state, pass, bump, time, camera, 0, |pass, kind| {
                pass.set_render_pipeline(state.brush_pipeline(kind).pipeline());
                BrushPipeline::set_push_constants(
                    pass,
                    Update(bump.alloc(brush::VertexPushConstants {
                        transform: camera.view_projection().to_cols_array_2d(),
                                            inv_view: Mat3::from_mat4(
                                                camera.view()
                                            .inverse()
                                            .transpose())
                                            .to_cols_array_2d(),
                    })),
                    Clear,
                    Clear,
                );
                pass.set_bind_group(
                    BindGroupLayoutId::PerEntity as usize,
                    &state.world_bind_groups()[BindGroupLayoutId::PerEntity as usize],
                    &[self.world_uniform_block.offset()],
                );
            });

        for (ent_pos, ent) in entities.enumerate() {
            if let Some(uniforms) = self.entity_uniform_blocks.read().get(ent_pos) {
                pass.set_bind_group(
                    BindGroupLayoutId::PerEntity as usize,
                    &state.world_bind_groups()[BindGroupLayoutId::PerEntity as usize],
                    &[uniforms.offset()],
                );

                match self.renderer_for_entity(ent) {
                    EntityRenderer::Brush(bmodel) => {
                        bmodel.record_draw(
                            state,
                            pass,
                            bump,
                            time,
                            camera,
                            ent.frame_id,
                            |pass, kind| {
                                pass.set_render_pipeline(state.brush_pipeline(kind).pipeline());
                                BrushPipeline::set_push_constants(
                                    pass,
                                    Update(
                                        bump.alloc(brush::VertexPushConstants {
                                            transform: self
                                                .calculate_mvp_transform(camera, ent)
                                                .to_cols_array_2d(),
                                            inv_view: Mat3::from_mat4(
                                                self.calculate_mv_transform(camera, ent)
                                            .inverse()
                                            .transpose())
                                            .to_cols_array_2d(),
                                        }),
                                    ),
                                    Clear,
                                    Clear,
                                );
                            },
                        );
                    }
                    EntityRenderer::Alias(alias) => {
                        pass.set_render_pipeline(state.alias_pipeline().pipeline());
                        AliasPipeline::set_push_constants(
                            pass,
                            Update(bump.alloc(alias::VertexPushConstants {
                                transform: self.calculate_mvp_transform(camera, ent),
                                model_view: Mat3::from_mat4(
                                    self.calculate_mv_transform(camera, ent),
                                ),
                            })),
                            Clear,
                            Clear,
                        );
                        alias.record_draw(state, pass, time, ent.frame_id(), ent.skin_id());
                    }
                    EntityRenderer::Sprite(sprite) => {
                        pass.set_render_pipeline(state.sprite_pipeline().pipeline());
                        SpritePipeline::set_push_constants(pass, Clear, Clear, Clear);
                        sprite.record_draw(state, pass, ent.frame_id(), time);
                    }
                    EntityRenderer::None => {}
                }
            }
        }

        let viewmodel_orig = camera.origin();
        let cam_angles = camera.angles();
        let viewmodel_mat = Mat4::from_translation(Vec3::new(
            -viewmodel_orig.y,
            viewmodel_orig.z,
            -viewmodel_orig.x,
        )) * Mat4::from_euler(
            EulerRot::YXZ,
            cam_angles.yaw.to_radians(),
            -cam_angles.pitch.to_radians(),
            cam_angles.roll.to_radians(),
        );
        match viewmodel_id.and_then(|vid| self.entity_renderers.get(vid)) {
            Some(EntityRenderer::Alias(alias)) => {
                pass.set_render_pipeline(state.alias_pipeline().pipeline());
                AliasPipeline::set_push_constants(
                    pass,
                    Update(bump.alloc(alias::VertexPushConstants {
                        transform: camera.view_projection() * viewmodel_mat,
                        model_view: Mat3::from_mat4(camera.view() * viewmodel_mat),
                    })),
                    Clear,
                    Clear,
                );
                alias.record_draw(state, pass, time, 0, 0);
            }
            Some(EntityRenderer::Brush(..)) => {
                unreachable!("Viewmodel is brush - this should never happen")
            }
            Some(EntityRenderer::Sprite(..)) => {
                // TODO: This is actually ok, how should we handle it?
            }
            None | Some(EntityRenderer::None) => {}
        }

        debug!("Drawing particles");
        state
            .particle_pipeline()
            .record_draw(pass, bump, camera, particles);
    }

    fn renderer_for_entity(&self, ent: &ClientEntity) -> &EntityRenderer {
        // subtract 1 from index because world entity isn't counted
        match &self.entity_renderers.get(ent.model_id().saturating_sub(1)) {
            Some(r) => r,
            None => &NO_ENTITY_RENDERER,
        }
    }

    fn calculate_mvp_transform(&self, camera: &Camera, entity: &ClientEntity) -> Mat4 {
        let model_transform = self.calculate_model_transform(camera, entity);

        camera.view_projection() * model_transform
    }

    fn calculate_mv_transform(&self, camera: &Camera, entity: &ClientEntity) -> Mat4 {
        let model_transform = self.calculate_model_transform(camera, entity);

        camera.view() * model_transform
    }

    fn calculate_model_transform(&self, camera: &Camera, entity: &ClientEntity) -> Mat4 {
        let origin = entity.get_origin();
        let angles = entity.get_angles();
        let rotation = match self.renderer_for_entity(entity) {
            EntityRenderer::Sprite(sprite) => match sprite.kind() {
                // used for decals
                SpriteKind::Oriented => Mat4::from_euler(
                    EulerRot::YZX,
                    -angles.x.to_radians(),
                    angles.y.to_radians(),
                    angles.z.to_radians(),
                ),

                _ => {
                    // keep sprite facing player, but preserve roll
                    let cam_angles = camera.angles();

                    Angles {
                        pitch: -cam_angles.pitch,
                        roll: angles.x,
                        yaw: -cam_angles.yaw,
                    }
                    .mat4_quake()
                }
            },

            _ => Mat4::from_euler(
                EulerRot::XYX,
                angles.x.to_radians(),
                angles.y.to_radians(),
                angles.z.to_radians(),
            ),
        };

        Mat4::from_translation(Vec3::new(-origin.y, origin.z, -origin.x)) * rotation
    }
}
