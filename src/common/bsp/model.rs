// TODO: Replace this with `bevy_trenchbroom` so we can upstream our work
#![cfg(false)]

use bevy::prelude::*;
use bitvec::vec::BitVec;
use hashbrown::HashSet;
use qbsp::BspData;
use std::ops;

#[derive(Component)]
#[relationship_target(linked_spawn, relationship = LeafOf)]
pub struct Leaves(Vec<Entity>);

impl ops::Deref for Leaves {
    type Target = [Entity];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Component)]
#[relationship(relationship_target = Leaves)]
pub struct LeafOf(#[relationship] Entity);

#[derive(Component)]
#[relationship_target(linked_spawn, relationship = ModelOf)]
pub struct Models(#[relationship] Vec<Entity>);

impl ops::Deref for Models {
    type Target = [Entity];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Component)]
#[relationship(relationship_target = Models)]
pub struct ModelOf(#[relationship] Entity);

#[derive(Component)]
#[require(LastLeaf)]
pub struct WorldModel {
    visdata: Box<[u8]>,
}

#[derive(Component, Default)]
struct LastLeaf {
    last_leaves_seen: HashSet<usize>,
    last_vis_bits: BitVec,
}

#[derive(Asset, TypePath)]
pub struct BspAsset(pub BspData);

#[derive(Component)]
pub struct Bsp {
    data: Handle<BspAsset>,
}

pub mod systems {
    use std::mem;

    use super::{Bsp, BspAsset, LastLeaf, Leaves, ModelOf, Models, WorldModel};
    use crate::common::bsp::utils;
    use bevy::{ecs::entity_disabling::Disabled, prelude::*};
    use bitvec::vec::BitVec;
    use hashbrown::HashSet;
    use qbsp::prelude::*;

    pub fn spawn_models(
        mut commands: Commands,
        bsp_data: Res<Assets<BspAsset>>,
        bsps: Query<(Entity, &Bsp), Without<Models>>,
    ) {
        for (ent, bsp) in bsps {
            let Some(bsp_data) = bsp_data.get(&bsp.data) else {
                commands.entity(ent).despawn();
                continue;
            };

            let lightmap_atlas = match bsp_data
                .0
                .compute_lightmap_atlas(PerStyleLightmapPacker::new(default()))
            {
                Ok(atlas) => Some(atlas),
                Err(e) => {
                    error!("Error computing lightmap for BSP: {e}");
                    None
                }
            };

            let mut bsp_model_iter = bsp_data.0.models.iter().enumerate();

            let Some(world_model) = bsp_model_iter.next() else {
                commands.entity(ent).despawn();
                continue;
            };

            // Non-world models (do not require vis calculation)
            for (i, bsp_model) in bsp_model_iter {
                let mesh = bsp_data
                    .0
                    .mesh_model(i, lightmap_atlas.as_ref().map(|atlas| &atlas.uvs));
            }
        }
    }

    pub fn worldmodel_vis(
        mut commands: Commands,
        cameras: Query<&Transform, With<Camera3d>>,
        worldmodels: Query<(Entity, &WorldModel, &Leaves, &ModelOf, &mut LastLeaf)>,
        bsps: Query<&Bsp>,
        bsp_data: Res<Assets<BspAsset>>,
    ) {
        for (model_ent, worldmodel, leaves, bsp_target, mut cached_vis) in worldmodels {
            let Some(bsp_data) = bsps
                .get(bsp_target.0)
                .ok()
                .and_then(|bsp| bsp_data.get(&bsp.data))
            else {
                commands.entity(model_ent).despawn();
                continue;
            };

            let cam_count = cached_vis.last_leaves_seen.len();
            let prev_vis = mem::replace(
                &mut cached_vis.last_leaves_seen,
                HashSet::with_capacity(cam_count),
            );
            let mut visible = BitVec::repeat(false, leaves.len());

            for camera in cameras {
                let leaf_id = bsp_data.0.leaf_at_point(0, camera.translation);
                cached_vis.last_leaves_seen.insert(leaf_id);

                if !prev_vis.contains(&leaf_id) {
                    for leaf in utils::get_pvs(&worldmodel.visdata, leaf_id, leaves.len()) {
                        visible.set(leaf, true);
                    }
                }
            }

            for ((is_enabled, last_enabled), leaf) in visible
                .iter()
                .zip(&cached_vis.last_vis_bits)
                .zip(&leaves[..])
            {
                if is_enabled != last_enabled {
                    commands
                        .entity(*leaf)
                        .remove_recursive::<Children, Disabled>();
                } else {
                    commands
                        .entity(*leaf)
                        .insert_recursive::<Children>(Disabled);
                }
            }

            cached_vis.last_vis_bits = visible;
        }
    }
}
