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
use cgmath::{Euler, InnerSpace, Matrix3, Matrix4, SquareMatrix as _, Vector3, Vector4};
use chrono::Duration;
use parking_lot::RwLock;

use super::RenderVars;

pub static BIND_GROUP_LAYOUT_DESCRIPTORS: [&[BindGroupLayoutEntry]; 2] = [
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
            cull_mode: None,
            polygon_mode: wgpu::PolygonMode::Fill,
            conservative: false,
            ..Default::default()
        }
    }

    fn color_target_states_with_args(
        (diffuse_format, normal_format): Self::Args,
    ) -> Vec<Option<wgpu::ColorTargetState>> {
        vec![
            // diffuse attachment
            Some(wgpu::ColorTargetState {
                format: diffuse_format,
                blend: Some(wgpu::BlendState::REPLACE),
                write_mask: wgpu::ColorWrites::ALL,
            }),
            // normal attachment
            Some(wgpu::ColorTargetState {
                format: normal_format,
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
    Lightmap = 3,
}

pub struct Camera {
    origin: Vector3<f32>,
    angles: Angles,
    view: Matrix4<f32>,
    view_projection: Matrix4<f32>,
    projection: Matrix4<f32>,
    inverse_projection: Matrix4<f32>,
    clipping_planes: [Vector4<f32>; 6],
}

impl Camera {
    pub fn new(origin: Vector3<f32>, angles: Angles, projection: Matrix4<f32>) -> Camera {
        // convert coordinates
        let converted_origin = Vector3::new(-origin.y, origin.z, -origin.x);

        // translate the world by inverse of camera position
        let translation = Matrix4::from_translation(-converted_origin);
        let rotation = angles.mat4_wgpu();
        let view = rotation * translation;
        let view_projection = projection * view;

        // see https://www.gamedevs.org/uploads/fast-extraction-viewing-frustum-planes-from-world-view-projection-matrix.pdf
        let clipping_planes = [
            // left
            view_projection.w + view_projection.x,
            // right
            view_projection.w - view_projection.x,
            // bottom
            view_projection.w + view_projection.y,
            // top
            view_projection.w - view_projection.y,
            // near
            view_projection.w + view_projection.z,
            // far
            view_projection.w - view_projection.z,
        ];

        Camera {
            origin,
            angles,
            view,
            view_projection,
            projection,
            inverse_projection: projection.invert().unwrap(),
            clipping_planes,
        }
    }

    pub fn origin(&self) -> Vector3<f32> {
        self.origin
    }

    pub fn angles(&self) -> Angles {
        self.angles
    }

    pub fn view(&self) -> Matrix4<f32> {
        self.view
    }

    pub fn view_projection(&self) -> Matrix4<f32> {
        self.view_projection
    }

    pub fn projection(&self) -> Matrix4<f32> {
        self.projection
    }

    pub fn inverse_projection(&self) -> Matrix4<f32> {
        self.inverse_projection
    }

    // TODO: this seems to be too lenient
    /// Determines whether a point falls outside the viewing frustum.
    pub fn cull_point(&self, p: Vector3<f32>) -> bool {
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
    lightmap_anim_frames: [Vector4<f32>; 16],
    camera_pos: Vector4<f32>,
    time: f32,
    sky_time: f32,

    // TODO: pack flags into a bit string
    r_lightmap: UniformBool,
}

#[repr(C, align(256))]
#[derive(Clone, Copy, Debug)]
pub struct EntityUniforms {
    /// Model-view-projection transform matrix
    transform: Matrix4<f32>,

    /// Model-only transform matrix
    model: Matrix4<f32>,
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
            let new_renderer = WorldRenderer::new(
                &mut *gfx_state,
                &*device,
                &*queue,
                state.model_precache.iter(),
                state.worldmodel_id,
            );
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

fn to_mat3(mat4: Matrix4<f32>) -> Matrix3<f32> {
    Matrix3 {
        x: mat4.x.truncate(),
        y: mat4.y.truncate(),
        z: mat4.z.truncate(),
    }
}

impl WorldRenderer {
    pub fn new<'a, M: Iterator<Item = &'a Model>>(
        state: &'a mut GraphicsState,
        device: &RenderDevice,
        queue: &RenderQueue,
        models: M,
        worldmodel_id: usize,
    ) -> WorldRenderer {
        let mut worldmodel_renderer = None;
        let mut entity_renderers = Vec::new();

        let world_uniform_block = state.entity_uniform_buffer_mut().allocate(EntityUniforms {
            transform: Matrix4::identity(),
            model: Matrix4::identity(),
        });

        for (i, model) in models.enumerate() {
            if i == worldmodel_id {
                match model.kind() {
                    ModelKind::Brush(bmodel) => {
                        worldmodel_renderer = Some(
                            BrushRendererBuilder::new(bmodel, true)
                                .build(state, device, queue)
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
                            BrushRendererBuilder::new(bmodel, false)
                                .build(state, device, queue)
                                .unwrap(),
                        ));
                    }

                    ModelKind::Sprite(smodel) => {
                        entity_renderers.push(EntityRenderer::Sprite(SpriteRenderer::new(
                            &state, device, queue, smodel,
                        )));
                    }

                    _ => {
                        warn!("Non-brush renderers not implemented!");
                        entity_renderers.push(EntityRenderer::None);
                    }
                }
            }
        }

        WorldRenderer {
            worldmodel_renderer: worldmodel_renderer.unwrap(),
            entity_renderers,
            world_uniform_block,
            entity_uniform_blocks: Default::default(),
        }
    }

    pub fn update_uniform_buffers<'a, I>(
        &self,
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
                let mut frames = [Vector4::<f32>::unit_x(); 16];
                for i in 0..16 {
                    for j in 0..4 {
                        frames[i] = Vector4::<f32>::new(
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
            model: Matrix4::identity(),
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

        pass.set_render_pipeline(state.brush_pipeline().pipeline());
        BrushPipeline::set_push_constants(
            pass,
            Update(bump.alloc(brush::VertexPushConstants {
                transform: camera.view_projection(),
                model_view: to_mat3(camera.view()),
            })),
            Clear,
            Clear,
        );
        pass.set_bind_group(
            BindGroupLayoutId::PerEntity as usize,
            &state.world_bind_groups()[BindGroupLayoutId::PerEntity as usize],
            &[self.world_uniform_block.offset()],
        );
        // HACK: Hardcoded frame time (TODO: Actually track frame number)
        self.worldmodel_renderer.record_draw(
            state,
            pass,
            bump,
            time,
            camera,
            ((engine::duration_to_f32(time) + (0.05 / 2.)) / 0.05) as usize,
        );

        for (ent_pos, ent) in entities.enumerate() {
            if let Some(uniforms) = self.entity_uniform_blocks.read().get(ent_pos) {
                pass.set_bind_group(
                    BindGroupLayoutId::PerEntity as usize,
                    &state.world_bind_groups()[BindGroupLayoutId::PerEntity as usize],
                    &[uniforms.offset()],
                );

                match self.renderer_for_entity(&ent) {
                    EntityRenderer::Brush(bmodel) => {
                        pass.set_render_pipeline(state.brush_pipeline().pipeline());
                        BrushPipeline::set_push_constants(
                            pass,
                            Update(bump.alloc(brush::VertexPushConstants {
                                transform: self.calculate_mvp_transform(camera, ent),
                                model_view: to_mat3(self.calculate_mv_transform(camera, ent)),
                            })),
                            Clear,
                            Clear,
                        );
                        bmodel.record_draw(state, pass, &bump, time, camera, ent.frame_id);
                    }
                    EntityRenderer::Alias(alias) => {
                        pass.set_render_pipeline(state.alias_pipeline().pipeline());
                        AliasPipeline::set_push_constants(
                            pass,
                            Update(bump.alloc(alias::VertexPushConstants {
                                transform: self.calculate_mvp_transform(camera, ent),
                                model_view: to_mat3(self.calculate_mv_transform(camera, ent)),
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
        let viewmodel_mat = Matrix4::from_translation(Vector3::new(
            -viewmodel_orig.y,
            viewmodel_orig.z,
            -viewmodel_orig.x,
        )) * Matrix4::from_angle_y(cam_angles.yaw)
            * Matrix4::from_angle_x(-cam_angles.pitch)
            * Matrix4::from_angle_z(cam_angles.roll);
        match viewmodel_id.and_then(|vid| self.entity_renderers.get(vid)) {
            Some(EntityRenderer::Alias(alias)) => {
                pass.set_render_pipeline(state.alias_pipeline().pipeline());
                AliasPipeline::set_push_constants(
                    pass,
                    Update(bump.alloc(alias::VertexPushConstants {
                        transform: camera.view_projection() * viewmodel_mat,
                        model_view: to_mat3(camera.view() * viewmodel_mat),
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
            .record_draw(pass, &bump, camera, particles);
    }

    fn renderer_for_entity(&self, ent: &ClientEntity) -> &EntityRenderer {
        // subtract 1 from index because world entity isn't counted
        match &self.entity_renderers.get(ent.model_id().saturating_sub(1)) {
            Some(r) => r,
            None => &NO_ENTITY_RENDERER,
        }
    }

    fn calculate_mvp_transform(&self, camera: &Camera, entity: &ClientEntity) -> Matrix4<f32> {
        let model_transform = self.calculate_model_transform(camera, entity);

        camera.view_projection() * model_transform
    }

    fn calculate_mv_transform(&self, camera: &Camera, entity: &ClientEntity) -> Matrix4<f32> {
        let model_transform = self.calculate_model_transform(camera, entity);

        camera.view() * model_transform
    }

    fn calculate_model_transform(&self, camera: &Camera, entity: &ClientEntity) -> Matrix4<f32> {
        let origin = entity.get_origin();
        let angles = entity.get_angles();
        let rotation = match self.renderer_for_entity(entity) {
            EntityRenderer::Sprite(sprite) => match sprite.kind() {
                // used for decals
                SpriteKind::Oriented => Matrix4::from(Euler::new(angles.z, -angles.x, angles.y)),

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

            _ => Matrix4::from(Euler::new(angles.x, angles.y, angles.z)),
        };

        Matrix4::from_translation(Vector3::new(-origin.y, origin.z, -origin.x)) * rotation
    }
}
