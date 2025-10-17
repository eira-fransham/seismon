use std::{
    borrow::Cow,
    ffi::OsStr,
    ops::Deref,
    sync::{Arc, LazyLock},
};

use asset_importer::{
    Matrix4x4, Texel, TextureData, TextureType, node::Node, postprocess::PostProcessSteps,
};
use bevy_animation::{
    AnimationClip, AnimationPlayer, AnimationTargetId, animated_field,
    animation_curves::{AnimatableCurve, AnimatedField},
};
use bevy_asset::{Asset, AssetLoader, AssetServer, Assets, Handle, LoadContext, RenderAssetUsages};
use bevy_camera::visibility::Visibility;
use bevy_color::{Color, Srgba};
use bevy_ecs::{
    change_detection::DetectChanges,
    component::Component,
    entity::Entity,
    hierarchy::ChildOf,
    name::Name,
    system::{Query, Res, SystemChangeTick},
    world::{Mut, Ref, World},
};
use bevy_image::Image;
use bevy_log::info;
use bevy_math::curve::{ConstantCurve, CurveExt, Interval, LinearCurve, UnevenSampleCurve};
use bevy_mesh::{Indices, Mesh, Mesh3d, PrimitiveTopology};
use bevy_pbr::{MeshMaterial3d, StandardMaterial};
use bevy_reflect::Reflect;
use bevy_render::render_resource::{Extent3d, TextureFormat};
use bevy_scene::Scene;
use bevy_tasks::{AsyncComputeTaskPool, ConditionalSendFuture, TaskPool};
use bevy_transform::components::Transform;
use enumflags2::{BitFlags, bitflags};
use hashbrown::HashMap;
use serde::{Deserialize, Serialize};

#[derive(Default)]
pub struct AssimpLoader;

#[bitflags]
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum AnimationKind {
    /// Load mesh animations.
    Mesh,
    /// Load skeletal animations (TODO).
    Skeletal,
    /// Load morph mesh animations (TODO).
    MorphMesh,
}

mod post_process_serde {
    use asset_importer::postprocess::PostProcessSteps;
    use serde::{Deserializer, Serialize, Serializer};

    pub fn serialize<S>(steps: &PostProcessSteps, s: S) -> Result<S::Ok, S::Error>
    where S: Serializer {
        steps.as_raw().serialize(s)
    }

    pub fn deserialize<'de, D>(d: D) -> Result<PostProcessSteps, D::Error>
    where D: Deserializer<'de> {
        // define a visitor that deserializes
        // `ActualData` encoded as json within a string
        struct PostProcessStepsVisitor;

        impl<'de> serde::de::Visitor<'de> for PostProcessStepsVisitor {
            type Value = PostProcessSteps;

            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "A 32-bit integer")
            }

            fn visit_u32<E>(self, value: u32) -> Result<Self::Value, E>
            where E: serde::de::Error {
                Ok(PostProcessSteps::from_raw(value))
            }
        }

        d.deserialize_u32(PostProcessStepsVisitor)
    }
}

#[derive(Serialize, Deserialize)]
pub struct AssimpSettings {
    /// The Assimp post-processing steps to include. Note that for now,
    /// [`PostProcessSteps::TRIANGULATE`] is required for this library to function correctly,
    /// so it is always added whether or not it is specified here.
    #[serde(with = "post_process_serde")]
    pub post_process: PostProcessSteps,
    pub animations: BitFlags<AnimationKind>,
    pub usages: RenderAssetUsages,
    pub transform: Matrix4x4,
}

impl Default for AssimpSettings {
    fn default() -> Self {
        AssimpSettings {
            post_process: PostProcessSteps::TRIANGULATE
                | PostProcessSteps::FLIP_UVS
                | PostProcessSteps::GEN_SMOOTH_NORMALS
                | PostProcessSteps::CALC_TANGENT_SPACE, // PostProcessSteps::REALTIME,
            animations: BitFlags::<AnimationKind>::all(),
            usages: RenderAssetUsages::RENDER_WORLD,
            transform: Matrix4x4::IDENTITY,
        }
    }
}

static SUPPORTED_EXTENSIONS: LazyLock<Vec<String>> =
    LazyLock::new(asset_importer::get_import_extensions);

static SUPPORTED_EXTENSIONS_STRS: LazyLock<Vec<&str>> = LazyLock::new(|| {
    SUPPORTED_EXTENSIONS
        .iter()
        .map(Deref::deref)
        .map(|ext| ext.strip_prefix('.').unwrap_or(ext))
        .collect()
});

impl AssetLoader for AssimpLoader {
    type Asset = Scene;
    type Error = anyhow::Error;
    type Settings = AssimpSettings;

    fn load(
        &self,
        reader: &mut dyn bevy_asset::io::Reader,
        settings: &Self::Settings,
        load_context: &mut LoadContext,
    ) -> impl ConditionalSendFuture<Output = Result<Self::Asset, Self::Error>> {
        async {
            let mut in_memory_bytes = vec![];

            reader.read_to_end(&mut in_memory_bytes).await?;

            let extension =
                load_context.path().extension().map(OsStr::to_string_lossy).map(Cow::into_owned);

            let post_process_settings = settings.post_process | PostProcessSteps::TRIANGULATE;

            // Try to get an executor to run the synchronous model importing task in another thread.
            // This will not run the task on another thread in single-threaded contexts e.g. the
            // default Wasm config, but we can survive some degraded behaviour on the web.
            let async_executor = AsyncComputeTaskPool::get_or_init(TaskPool::new);
            let scene = async_executor
                .spawn(async move {
                    asset_importer::Scene::from_memory_with_flags(
                        &in_memory_bytes,
                        extension.as_deref(),
                        post_process_settings,
                    )
                })
                .await?;

            #[derive(Debug)]
            struct SceneCounts {
                num_meshes: usize,
                num_materials: usize,
                num_animations: usize,
                num_cameras: usize,
                num_lights: usize,
                num_textures: usize,
            }

            info!(
                "{:?}: {:#?}",
                load_context.path(),
                SceneCounts {
                    num_meshes: scene.num_meshes(),
                    num_materials: scene.num_materials(),
                    num_animations: scene.num_animations(),
                    num_cameras: scene.num_cameras(),
                    num_lights: scene.num_lights(),
                    num_textures: scene.num_textures(),
                }
            );

            let mut paths_to_textures = HashMap::<String, Handle<Image>>::new();

            let default_material = StandardMaterial::default();

            let materials = scene
                .materials()
                .enumerate()
                .map(|(index, mat)| {
                    let tex_handle = mat.texture(TextureType::Diffuse, 0).map(|tex| {
                        paths_to_textures
                            .entry(tex.path.clone())
                            .or_insert_with(|| {
                                if let Some(embedded) = tex
                                    .path
                                    .strip_prefix('*')
                                    .and_then(|index| scene.texture(index.parse().ok()?))
                                    .or_else(|| scene.find_texture_by_filename(&tex.path))
                                {
                                    assert!(["", "rgba8888"].contains(&&*embedded.format_hint()));
                                    let TextureData::Texels(uncompressed) =
                                        embedded.data().unwrap()
                                    else {
                                        panic!("Can't handle compressed yet!");
                                    };
                                    let image = Image::new(
                                        Extent3d {
                                            width: embedded.width(),
                                            height: embedded.height(),
                                            depth_or_array_layers: 1,
                                        },
                                        bevy_render::render_resource::TextureDimension::D2,
                                        uncompressed
                                            .into_iter()
                                            .flat_map(|Texel { r, g, b, a }| [r, g, b, a])
                                            .collect(),
                                        TextureFormat::Rgba8UnormSrgb,
                                        settings.usages,
                                    );

                                    load_context.add_labeled_asset(tex.path, image)
                                } else {
                                    load_context.load(tex.path)
                                }
                            })
                            .clone()
                    });

                    let mat_name = mat.name();
                    let name = if mat_name.is_empty() { format!("Mat{index}") } else { mat_name };

                    load_context.add_labeled_asset(
                        name,
                        StandardMaterial {
                            base_color: mat
                                .base_color()
                                .map(|rgba| {
                                    let (r, g, b, a) = rgba.into();
                                    Color::Srgba(Srgba::new(r, g, b, a))
                                })
                                .unwrap_or(default_material.base_color),
                            base_color_texture: tex_handle,
                            // TODO
                            unlit: true,
                            ..default_material.clone()
                        },
                    )
                })
                .collect::<Vec<_>>();

            let meshes = scene
                .meshes()
                .enumerate()
                .map(|(index, assimp_mesh)| {
                    #[derive(Debug)]
                    struct MeshCounts {
                        num_anim_meshes: usize,
                        num_bones: usize,
                        num_faces: usize,
                        num_vertices: usize,
                    }

                    if !assimp_mesh.has_triangles() {
                        return Err(anyhow::Error::msg("Mesh wasn't triangulated (TODO)"));
                    }

                    let mut mesh = Mesh::new(PrimitiveTopology::TriangleList, settings.usages);

                    mesh.insert_indices(Indices::U32(
                        assimp_mesh
                            .faces()
                            .flat_map(|face| {
                                <[u32; 3]>::try_from(face.indices()).expect(
                                    "TODO: Assimp mesh has both triangles and other kinds of faces",
                                )
                            })
                            .collect(),
                    ));

                    mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, assimp_mesh.vertices());
                    if let Some(normals) = assimp_mesh.normals() {
                        mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
                    } else {
                        mesh.compute_normals();
                    }

                    if let Some(uvs) = assimp_mesh.texture_coords(0) {
                        mesh.insert_attribute(
                            Mesh::ATTRIBUTE_UV_0,
                            uvs.into_iter()
                                .map(|v3| {
                                    let (x, y, _z) = v3.into();
                                    [x, y]
                                })
                                .collect::<Vec<_>>(),
                        );
                    }

                    let mesh_name = assimp_mesh.name();
                    let name = if mesh_name.is_empty() { format!("{index}") } else { mesh_name };

                    let mesh_label = format!("Mesh.{name}");

                    info!(
                        "{:?}: {:#?}",
                        format!("{}#{}", load_context.path().display(), mesh_label),
                        MeshCounts {
                            num_anim_meshes: assimp_mesh.num_anim_meshes(),
                            num_bones: assimp_mesh.num_bones(),
                            num_faces: assimp_mesh.num_faces(),
                            num_vertices: assimp_mesh.num_vertices(),
                        }
                    );

                    let anim_meshes = assimp_mesh
                        .anim_meshes()
                        .map(|assimp_anim_mesh| {
                            let mut anim_mesh = mesh.clone();

                            let opt_positions = assimp_anim_mesh.vertices();
                            let has_positions = opt_positions.is_some();

                            if let Some(positions) = opt_positions {
                                anim_mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, positions);
                            }
                            if let Some(normals) = assimp_anim_mesh.normals() {
                                anim_mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL, normals);
                            } else if has_positions {
                                anim_mesh.compute_normals()
                            }

                            if let Some(tex_coords) = assimp_anim_mesh.texture_coords(0) {
                                anim_mesh.insert_attribute(
                                    Mesh::ATTRIBUTE_UV_0,
                                    tex_coords
                                        .into_iter()
                                        .map(|v3| {
                                            let (x, y, _z) = v3.into();
                                            [x, y]
                                        })
                                        .collect::<Vec<_>>(),
                                );
                            }

                            let mesh_name = assimp_anim_mesh.name();
                            let name =
                                if mesh_name.is_empty() { format!("{index}") } else { mesh_name };

                            load_context
                                .add_labeled_asset(format!("{mesh_label}.Frame.{name}"), anim_mesh)
                        })
                        .collect::<Vec<_>>();

                    Ok((load_context.add_labeled_asset(mesh_label, mesh), anim_meshes))
                })
                .collect::<anyhow::Result<Vec<_>>>()?;

            // TODO: Add animations to the scene
            let _animations = scene
                .animations()
                .enumerate()
                .map(|(i, animation)| {
                    fn step_interp(a: &f64, b: &f64, factor: f32) -> f64 {
                        *[a, b][factor.rem_euclid(2.) as usize]
                    }

                    let duration = animation.duration_in_seconds() as f32;

                    let anim_name = animation.name();
                    let anim_name = if anim_name.is_empty() { format!("{i}") } else { anim_name };

                    let mut anim_clip = AnimationClip::default();

                    for mesh_anim in animation.mesh_channels() {
                        let target = AnimationTargetId::from_name(&Name::new(format!(
                            "AnimTarget.{anim_name}"
                        )));

                        if mesh_anim.num_keys() > 0 {
                            let sample_points = mesh_anim
                                .keys()
                                .iter()
                                .map(|key| (key.time as f32, key.value as f64))
                                .chain(
                                    mesh_anim.keys().last().map(|key| (duration, key.value as f64)),
                                );

                            anim_clip.add_curve_to_target(
                                target,
                                AnimatableCurve::new(
                                    animated_field!(AssimpMeshAnimation::frame),
                                    UnevenSampleCurve::new(sample_points, step_interp)?,
                                ),
                            );
                        } else {
                            anim_clip.add_curve_to_target(
                                target,
                                AnimatableCurve::new(
                                    animated_field!(AssimpMeshAnimation::frame),
                                    ConstantCurve::new(
                                        Interval::new(0., animation.duration_in_seconds() as f32)?,
                                        0.,
                                    ),
                                ),
                            );
                        }
                    }

                    Ok(load_context.add_labeled_asset(format!("Anim.{anim_name}"), anim_clip))
                })
                .collect::<anyhow::Result<Vec<_>>>()?;

            let mut out_scene = World::new();

            if let Some(node) = scene.root_node() {
                add_node_to_scene(
                    &materials,
                    &meshes,
                    settings,
                    &mut out_scene,
                    node,
                    None,
                    &scene,
                )?;
            }

            Ok(Scene { world: out_scene })
        }
    }

    fn extensions(&self) -> &[&str] {
        &SUPPORTED_EXTENSIONS_STRS
    }
}

// TODO: Make a struct for this.
fn add_node_to_scene(
    materials: &[Handle<StandardMaterial>],
    meshes: &[(Handle<Mesh>, Vec<Handle<Mesh>>)],
    settings: &AssimpSettings,
    scene: &mut World,
    node: Node,
    parent: Option<Entity>,
    assimp_scene: &asset_importer::Scene,
) -> anyhow::Result<()> {
    let transformation = node.transformation();

    let mut node_ent = scene.spawn(Visibility::Inherited);
    if let Some(ent) = parent {
        node_ent.insert((ChildOf(ent), Transform::from_matrix(transformation)));
    } else {
        node_ent.insert(Transform::from_matrix(settings.transform * transformation));
    }

    let node_ent = node_ent.id();

    // TODO: This creates the same mesh multiple times! Should create all meshes first
    // as labelled and then access them via path.
    for i in node.mesh_indices() {
        let Some(assimp_mesh) = assimp_scene.mesh(i) else {
            continue;
        };
        let mesh_handle = meshes[i].0.clone();

        scene.spawn((
            Mesh3d(mesh_handle),
            MeshMaterial3d(materials[assimp_mesh.material_index()].clone()),
            Transform::default(),
            Visibility::Inherited,
            ChildOf(node_ent),
        ));
    }

    for node in node.children() {
        add_node_to_scene(materials, meshes, settings, scene, node, Some(node_ent), assimp_scene)?;
    }

    Ok(())
}
