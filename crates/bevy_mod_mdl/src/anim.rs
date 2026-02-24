use bevy_asset::{Asset, Assets, Handle};
use bevy_ecs::{
    component::Component,
    reflect::ReflectComponent,
    system::{Query, Res},
    world::Mut,
};
use bevy_mesh::{Mesh, Mesh3d};
use bevy_reflect::Reflect;
use bevy_time::{Time, Virtual};

#[derive(Reflect, Clone)]
pub struct AnimMeshFrame {
    pub duration_secs: f64,
    pub mesh: Handle<Mesh>,
}

#[derive(Asset, Reflect, Clone)]
pub struct AnimMeshes {
    pub frames: Vec<AnimMeshFrame>,
}

// TODO: Link this up with `AnimationPlayer`?
#[derive(Debug, Component, Reflect, Clone)]
#[reflect(Component)]
#[require(Mesh3d)]
pub struct MeshAnimPlayer {
    anim_meshes: Handle<AnimMeshes>,
    dirty: bool,
    next_frame_time: f64,
    /// The index of the current frame
    frame: usize,
}

impl MeshAnimPlayer {
    pub fn new(anim_meshes: Handle<AnimMeshes>) -> Self {
        Self { anim_meshes, frame: 0, next_frame_time: f64::NEG_INFINITY, dirty: true }
    }

    pub fn set_anim_meshes(&mut self, anim_meshes: Handle<AnimMeshes>) {
        self.anim_meshes = anim_meshes;
        self.frame = 0;
        self.dirty = true;
    }
}

pub(crate) fn animate_mesh_animations(
    time: Res<Time<Virtual>>,
    anim_meshes: Res<Assets<AnimMeshes>>,
    entities: Query<(&mut Mesh3d, Mut<MeshAnimPlayer>)>,
) {
    for (mut mesh, mut anim) in entities {
        let Some(anim_mesh_frames) = anim_meshes.get(&anim.anim_meshes) else {
            continue;
        };

        let time = time.elapsed().as_secs_f64();

        let (last_frame_time, new_frame_index) = if anim.dirty {
            (time, anim.frame)
        } else if time >= anim.next_frame_time {
            (anim.next_frame_time, anim.frame + 1)
        } else {
            continue;
        };

        let new_frame_index = new_frame_index % anim_mesh_frames.frames.len();

        let new_frame = &anim_mesh_frames.frames[new_frame_index];

        if anim.dirty || new_frame_index != anim.frame {
            mesh.0 = new_frame.mesh.clone();
        }

        anim.frame = new_frame_index;
        anim.next_frame_time = last_frame_time + new_frame.duration_secs;
        anim.dirty = false;
    }
}
