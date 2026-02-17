use bevy_asset::{Asset, Assets, Handle};
use bevy_ecs::{
    change_detection::DetectChanges as _,
    component::Component,
    reflect::ReflectComponent,
    system::{Query, Res, SystemChangeTick},
    world::Mut,
};
use bevy_mesh::{Mesh, Mesh3d};
use bevy_reflect::Reflect;

#[derive(Asset, Reflect, Clone)]
pub struct AnimMeshes {
    pub frames: Vec<Handle<Mesh>>,
}

// TODO: Link this up with `AnimationPlayer`?
#[derive(Component, Reflect, Clone)]
#[reflect(Component)]
pub struct MeshAnimPlayer {
    pub anim_meshes: Handle<AnimMeshes>,
    /// The current point in the animation, in frames.
    pub frame: f64,
    /// The last index that was set on the [`Mesh3d`], to prevent too many updates.
    last_index: usize,
}

impl MeshAnimPlayer {
    pub fn new(anim_meshes: Handle<AnimMeshes>) -> Self {
        Self { anim_meshes, frame: 0., last_index: 0 }
    }
}

pub fn animate_mesh_animations(
    anim_meshes: Res<Assets<AnimMeshes>>,
    entities: Query<(&mut Mesh3d, Mut<MeshAnimPlayer>)>,
    ticks: SystemChangeTick,
) {
    for (mut mesh, mut anim) in entities {
        if !anim.last_changed().is_newer_than(ticks.last_run(), ticks.this_run()) {
            continue;
        }

        let Some(anim_mesh_frames) = anim_meshes.get(&anim.anim_meshes) else {
            continue;
        };

        let cur_index = anim.frame.clamp(0., anim_mesh_frames.frames.len() as f64 - 1.) as usize;

        if anim.last_index == cur_index {
            continue;
        }

        mesh.0 = anim_mesh_frames.frames[cur_index].clone();
        anim.last_index = cur_index;
    }
}
