use std::{any::TypeId, sync::Arc};

use bevy::{
    app::App,
    ecs::{
        component::Component,
        entity::Entity,
        query::{QueryData, QueryItem, QueryState, ROQueryItem},
        world::Mut,
    },
    math::{Vec2, Vec3, Vec3Swizzles as _},
    transform::components::Transform,
};
use bevy_mod_scripting::{
    bindings::{
        ExternalError, FromScript, InteropError, ReflectBase, ReflectReference,
        ThreadWorldContainer,
    },
    prelude::*,
};

use crate::server::progs::globals::make_vectors;

// fn makevectors(vector ang) -> void
// fn setorigin(entity e, vector o) -> void
// fn setmodel(entity e, string m) -> void
// fn setsize(entity e, vector min, vector max) -> void
// fn break() -> void
// fn random() -> float
// fn sound(entity e, float chan, string samp, float vol, float atten) -> void
// fn normalize(vector v) -> vector
// fn error(string e) -> void
// fn objerror(string e) -> void
// fn vlen(vector v) -> float
// fn vectoyaw(vector v) -> float
// fn spawn() -> entity
// fn remove(entity e) -> void
// fn traceline(vector v1, vector v2, float nomonsters, entity forent) -> void
// fn checkclient() -> entity
// fn find(entity start, .string fld, string match) -> entity
// fn precache_sound(string s) -> string
// fn precache_model(string s) -> string
// fn stuffcmd(entity client, string s) -> void
// fn findradius(vector org, float rad) -> entity
// fn bprint(float level, string s) -> void
// fn sprint(entity client, float level, string s) -> void
// fn dprint(string s) -> void
// fn ftos(float f) -> string
// fn vtos(vector v) -> string
// fn coredump() -> void
// fn traceon() -> void
// fn traceoff() -> void
// fn eprint(entity e) -> void
// fn walkmove(float yaw, float dist) -> float
// fn droptofloor(float yaw, float dist) -> float
// fn lightstyle(float style, string value) -> void
// fn rint(float v) -> float
// fn floor(float v) -> float
// fn ceil(float v) -> float
// fn checkbottom(entity e) -> float
// fn pointcontents(vector v) -> float
// fn fabs(float f) -> float
// fn aim(entity e, float speed) -> vector
// fn cvar(string s) -> float
// fn localcmd(string s) -> void
// fn nextent(entity e) -> entity
// fn ChangeYaw() -> void
// fn vectoangles(vector v) -> vector
// fn WriteByte(float to, float f) -> void
// fn WriteChar(float to, float f) -> void
// fn WriteShort(float to, float f) -> void
// fn WriteLong(float to, float f) -> void
// fn WriteCoord(float to, float f) -> void
// fn WriteAngle(float to, float f) -> void
// fn WriteString(float to, string s) -> void
// fn WriteEntity(float to, entity s) -> void
// fn movetogoal(float step) -> void
// fn precache_file(string s) -> string
// fn makestatic(entity e) -> void
// fn changelevel(string s) -> void
// fn cvar_set(string var, string val) -> void
// fn centerprint(entity client, string s) -> void
// fn ambientsound(vector pos, string samp, float vol, float atten) -> void
// fn precache_model2(string s) -> string
// fn precache_sound2(string s) -> string
// fn precache_file2(string s) -> string
// fn setspawnparms(entity e) -> void
// fn logfrag(entity killer, entity killee) -> void
// fn infokey(entity e, string key) -> string
// fn stof(string s) -> float
// fn multicast(vector where, float set) -> void

fn ent_ref<D, O, F>(reference: &ReflectReference, func: F) -> Result<O, InteropError>
where
    D: QueryData,
    F: for<'a, 'b> FnOnce(ROQueryItem<'a, 'b, D>) -> Result<O, InteropError>,
{
    match reference.base.base_id {
        // TODO: Should probably confirm that this is the right component.
        ReflectBase::Component(entity, _) => {
            let ctx = ThreadWorldContainer.try_get_context()?;

            ctx.world.with_global_access(|world| {
                let mut query = world.query::<D>();
                let result = query
                    .get(world, entity)
                    .map_err(|e| InteropError::External(ExternalError(Arc::new(e))))?;

                func(result)
            })?
        }
        // TODO: Proper error
        _ => Err(InteropError::NotImplemented),
    }
}

fn ent_mut<D, O, F>(reference: &ReflectReference, func: F) -> Result<O, InteropError>
where
    D: QueryData,
    F: for<'a, 'b> FnOnce(QueryItem<'a, 'b, D>) -> Result<O, InteropError>,
{
    match reference.base.base_id {
        // TODO: Should probably confirm that this is the right component.
        ReflectBase::Component(entity, _) => {
            let ctx = ThreadWorldContainer.try_get_context()?;

            ctx.world.with_global_access(|world| {
                let mut query = world.query::<D>();
                let result = query
                    .get_mut(world, entity)
                    .map_err(|e| InteropError::External(ExternalError(Arc::new(e))))?;

                func(result)
            })?
        }
        // TODO: Proper error
        _ => Err(InteropError::NotImplemented),
    }
}

fn worldspawn_ref<D, O, F>(func: F) -> Result<O, InteropError>
where
    D: QueryData,
    F: for<'a, 'b> FnOnce(ROQueryItem<'a, 'b, D>) -> Result<O, InteropError>,
{
    let ctx = ThreadWorldContainer.try_get_context()?;

    ctx.world.with_global_access(|world| {
        let mut query = world.query::<D>();
        let result = query
            .get(world, ctx.attachment.entity().ok_or(InteropError::NotImplemented)?)
            .map_err(|e| InteropError::External(ExternalError(Arc::new(e))))?;

        func(result)
    })?
}

fn worldspawn_mut<D, O, F>(func: F) -> Result<O, InteropError>
where
    D: QueryData,
    F: for<'a, 'b> FnOnce(QueryItem<'a, 'b, D>) -> Result<O, InteropError>,
{
    let ctx = ThreadWorldContainer.try_get_context()?;

    ctx.world.with_global_access(|world| {
        let mut query = world.query::<D>();
        let result = query
            .get_mut(world, ctx.attachment.entity().ok_or(InteropError::NotImplemented)?)
            .map_err(|e| InteropError::External(ExternalError(Arc::new(e))))?;

        func(result)
    })?
}

#[derive(Component)]
struct GlobalDirections {
    forward: Vec3,
    up: Vec3,
    right: Vec3,
}

fn angle(vec: Vec2) -> f32 {
    if vec == Vec2::ZERO { 0. } else { vec.y.atan2(vec.x).to_degrees().rem_euclid(360.) }
}

pub fn register_builtins_quake1(app: &mut App) -> &mut App {
    NamespaceBuilder::<bevy_mod_scripting_qcvm::QCBuiltin>::new_unregistered(app.world_mut())
        // --------------- MUTATE WORLD ---------------
        .register("makevectors", |angles: [f32; 3]| -> Result<(), InteropError> {
            worldspawn_mut::<Mut<GlobalDirections>, (), _>(|mut global_directions| {
                let mat = make_vectors(angles);
                global_directions.forward = mat.x_axis;
                global_directions.right = mat.y_axis;
                global_directions.up = mat.z_axis;

                Ok(())
            })
        })
        // --------------- MUTATE ENTITY ---------------
        .register(
            "setorigin",
            |entity: ReflectReference, origin: [f32; 3]| -> Result<(), InteropError> {
                ent_mut::<Mut<Transform>, (), _>(&entity, |mut transform| {
                    // TODO: Relink?
                    transform.translation = origin.into();

                    Ok(())
                })
            },
        )
        .register("setmodel", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("setsize", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        // --------------- PURE ---------------
        .register("rint", |value: f32| -> f32 { value.round() })
        .register("floor", |value: f32| -> f32 { value.floor() })
        .register("ceil", |value: f32| -> f32 { value.ceil() })
        .register("fabs", |value: f32| -> f32 { value.abs() })
        .register("random", || rand::random::<f32>())
        .register("normalize", |vector: [f32; 3]| -> [f32; 3] {
            Vec3::from(vector).normalize_or_zero().into()
        })
        .register("vlen", |vector: [f32; 3]| -> f32 { Vec3::from(vector).length() })
        .register("vectoyaw", |[x, y, _]: [f32; 3]| angle(Vec2::new(x, y)))
        .register("vectoangles", |vector: [f32; 3]| {
            let vector = Vec3::from(vector);
            [angle(vector.xy()), angle(vector.yz()), 0.]
        })
        .register("ftos", |value: f32| value.to_string())
        .register("vtos", |[x, y, z]: [f32; 3]| format!("{x} {y} {z}"))
        .register("stof", |value: String| -> Result<f32, InteropError> {
            value.parse().map_err(|e| InteropError::External(ExternalError(Arc::new(e))))
        })
        // --------------- NETWORKING ---------------
        .register("WriteByte", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("WriteChar", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("WriteShort", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("WriteLong", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("WriteCoord", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("WriteAngle", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("WriteString", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("WriteEntity", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        // --------------- INVALID AT RUNTIME ---------------
        .register("precache_file", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("precache_file2", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        // --------------- UNCATEGORIZED ---------------
        // TODO: Categorize these
        .register("break", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("sound", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("error", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("objerror", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("spawn", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("remove", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("traceline", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("checkclient", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("find", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("precache_sound", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("precache_sound2", || -> Result<(), InteropError> {
            // Same as `precache_sound` at runtime
            Err(InteropError::NotImplemented)
        })
        .register("precache_model", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("precache_model2", || -> Result<(), InteropError> {
            // Same as `precache_model` at runtime
            Err(InteropError::NotImplemented)
        })
        .register("stuffcmd", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("findradius", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("bprint", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("sprint", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("dprint", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("coredump", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("traceon", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("traceoff", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("eprint", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("walkmove", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("droptofloor", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("lightstyle", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("checkbottom", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("pointcontents", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("aim", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("cvar", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("localcmd", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("nextent", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("ChangeYaw", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("movetogoal", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("makestatic", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("changelevel", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("cvar_set", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("centerprint", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("ambientsound", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("setspawnparms", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("logfrag", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("infokey", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("multicast", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        });

    app
}
