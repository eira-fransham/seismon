//! The models that CSQC can use, by model index.
//!
//! Like FTEQW, positive model indices are the server's model precache (as the engine received it),
//! and negative ones are models that only CSQC precached (`-1` is the first). Model 0 is no model.

use bevy::{prelude::*, world_serialization::WorldAsset};
use bevy_mod_mdl::Mdl;

use crate::client::{
    Connection,
    state::{PrecacheModel, PrecacheModelState},
};

/// The models that CSQC precached that the server didn't, on the CSQC worldspawn.
#[derive(Component, Debug, Default)]
pub struct CsqcModels {
    client: Vec<(String, Handle<WorldAsset>)>,
}

/// The name of the model with index `index`.
pub fn model_name(world: &World, root: Entity, index: i32) -> Option<String> {
    if index > 0 {
        let state = world.get_resource::<Connection>()?.client_state()?;
        state.models.get(index as usize).and_then(|m| m.name.as_ref()).cloned()
    } else if index < 0 {
        let models = world.get::<CsqcModels>(root)?;
        models.client.get((-index - 1) as usize).map(|(name, _)| name.clone())
    } else {
        None
    }
}

/// The index of the model called `name`, if it has been precached by the server or CSQC.
pub fn model_index(world: &World, root: Entity, name: &str) -> Option<i32> {
    if name.is_empty() {
        return None;
    }
    if let Some(state) = world.get_resource::<Connection>().and_then(Connection::client_state)
        && let Some(index) = state.models.iter().position(|m| m.name.as_deref() == Some(name))
    {
        return Some(index as i32);
    }
    let models = world.get::<CsqcModels>(root)?;
    let index = models.client.iter().position(|(n, _)| n == name)?;
    Some(-(index as i32) - 1)
}

/// Precaches the model called `name` for CSQC (if the server hasn't), returning its index.
pub fn precache_model(world: &mut World, root: Entity, name: &str) -> i32 {
    if name.is_empty() {
        return 0;
    }
    if let Some(index) = model_index(world, root, name) {
        return index;
    }

    let path = if name.ends_with(".bsp") { format!("{name}#Model0") } else { name.to_owned() };
    let handle = world.resource::<AssetServer>().load(path);
    let mut entity = world.entity_mut(root);
    let mut models = entity.entry::<CsqcModels>().or_default();
    let mut models = models.get_mut();
    models.client.push((name.to_owned(), handle));
    -(models.client.len() as i32)
}

/// The model with index `index`, if it has been loaded.
pub fn model_handle(world: &World, root: Entity, index: i32) -> Option<Handle<WorldAsset>> {
    if index > 0 {
        let state = world.get_resource::<Connection>()?.client_state()?;
        match state.models.get(index as usize)? {
            PrecacheModel { state: PrecacheModelState::Loaded(handle), .. } => Some(handle.clone()),
            _ => None,
        }
    } else if index < 0 {
        let models = world.get::<CsqcModels>(root)?;
        models.client.get((-index - 1) as usize).map(|(_, handle)| handle.clone())
    } else {
        None
    }
}

/// The MDL data of the model with index `index`, if it is a loaded `.mdl`.
pub fn mdl(world: &World, root: Entity, index: i32) -> Option<&Mdl> {
    let name = model_name(world, root, index)?;
    if !name.ends_with(".mdl") {
        return None;
    }
    let handle = world.resource::<AssetServer>().get_handle::<Mdl>(format!("{name}#mdldata"))?;
    world.get_resource::<Assets<Mdl>>()?.get(&handle)
}
