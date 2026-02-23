#![expect(dead_code, reason = "TODO: Migrate server to use `bevy_mod_scripting_qcvm`")]

use std::{any::TypeId, sync::Arc};

use bevy::{
    ecs::{
        component::{Component, ComponentId},
        entity::Entity,
        query::{QueryData, QueryItem, ROQueryItem},
        world::{Mut, Ref, World},
    },
    math::{Vec2, Vec3, Vec3Swizzles as _},
    transform::components::Transform,
};
use bevy_mod_scripting::{
    bindings::{
        ExternalError, FunctionCallContext, InteropError, IntoNamespace, MagicFunctions,
        ReflectBase, ReflectReference, ThreadScriptContext, ThreadWorldContainer, WorldAccessGuard,
    },
    prelude::*,
};
use bevy_mod_scripting_qcvm::{QCEntity, QCWorldspawn};
use hashbrown::HashMap;
use seismon_utils::QAngles;

use crate::{
    common::net::ServerCmd,
    server::{precache::Precache, progs::globals::make_vectors},
};

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

// TODO: Persistent `QueryState`.
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

#[derive(Component)]
struct LevelPrecache {
    sound: Precache,
    model: Precache,
}

#[derive(Component)]
struct MessagesReliable {
    messages: Vec<u8>,
}

#[derive(Component)]
struct EntityAngles {
    ideal_yaw_deg: f32,
    yaw_speed_deg: f32,
}

fn angle(vec: Vec2) -> f32 {
    if vec == Vec2::ZERO { 0. } else { vec.y.atan2(vec.x).to_degrees().rem_euclid(360.) }
}

#[derive(Clone)]
struct FieldAccessor {
    component_id: ComponentId,
    type_id: TypeId,
    field_name: &'static str,
}

impl FieldAccessor {
    fn reflect_reference(&self, entity: Entity) -> ReflectReference {
        ReflectReference::new_component_ref_by_id(entity, self.component_id, self.type_id)
    }
}

struct AccessorBuilder<'a> {
    fields: HashMap<&'static str, FieldAccessor>,
    world: &'a World,
}

impl<'a> AccessorBuilder<'a> {
    fn new(world: &'a World) -> Self {
        Self { world, fields: Default::default() }
    }

    fn field<C>(mut self, field_name: &'static str, component_field_name: &'static str) -> Self
    where
        C: Component + 'static,
    {
        let component_id = self.world.component_id::<C>().expect("Component was not registered");

        self.fields.insert(
            field_name,
            FieldAccessor {
                component_id,
                type_id: TypeId::of::<C>(),
                field_name: component_field_name,
            },
        );

        self
    }

    fn build(self) -> HashMap<&'static str, FieldAccessor> {
        self.fields
    }
}

#[derive(Component)]
struct Fields {
    fields: HashMap<&'static str, FieldAccessor>,
}

#[derive(Component)]
struct Globals {
    globals: HashMap<&'static str, FieldAccessor>,
}

fn field_accessor(
    ctx: &ThreadScriptContext<'_>,
    field_name: ScriptValue,
) -> Result<FieldAccessor, InteropError> {
    let field_name = field_name.as_string().map_err(|_| InteropError::NotImplemented)?;

    let worldspawn = ctx.attachment.entity().ok_or(InteropError::NotImplemented)?;

    let fields_ref = ReflectReference::new_component_ref::<Fields>(worldspawn, ctx.world.clone())?;

    fields_ref.with_reflect(ctx.world.clone(), |fields| {
        let fields: &Fields = fields.try_downcast_ref().ok_or(InteropError::NotImplemented)?;

        let accessor = fields.fields.get(&*field_name).ok_or(InteropError::NotImplemented)?.clone();

        Ok(accessor.clone())
    })?
}

fn global_accessor(
    ctx: &ThreadScriptContext<'_>,
    field_name: ScriptValue,
) -> Result<FieldAccessor, InteropError> {
    let field_name = field_name.as_string().map_err(|_| InteropError::NotImplemented)?;

    let worldspawn = ctx.attachment.entity().ok_or(InteropError::NotImplemented)?;

    let fields_ref = ReflectReference::new_component_ref::<Globals>(worldspawn, ctx.world.clone())?;

    fields_ref.with_reflect(ctx.world.clone(), |fields| {
        let fields: &Globals = fields.try_downcast_ref().ok_or(InteropError::NotImplemented)?;

        let accessor =
            fields.globals.get(&*field_name).ok_or(InteropError::NotImplemented)?.clone();

        Ok(accessor.clone())
    })?
}

fn override_get(
    ctx: FunctionCallContext,
    ent: ReflectReference,
    field_name: ScriptValue,
) -> Result<ScriptValue, InteropError> {
    let world_ctx = ThreadWorldContainer.try_get_context()?;

    let qc_entity_id = world_ctx
        .world
        .get_component_id(TypeId::of::<QCEntity>())?
        .ok_or(InteropError::NotImplemented)?;

    let qc_worldspawn_id = world_ctx
        .world
        .get_component_id(TypeId::of::<QCWorldspawn>())?
        .ok_or(InteropError::NotImplemented)?;

    // TODO: We should handle "sub-field" references.
    let (entity, field_name) = match ent.base.base_id {
        ReflectBase::Component(entity, component_id) if component_id == qc_entity_id => {
            let accessor = field_accessor(&world_ctx, field_name)?;
            (accessor.reflect_reference(entity), accessor.field_name.into())
        }
        ReflectBase::Component(entity, component_id) if component_id == qc_worldspawn_id => {
            let accessor = global_accessor(&world_ctx, field_name)?;
            (accessor.reflect_reference(entity), accessor.field_name.into())
        }
        _ => (ent, field_name),
    };

    MagicFunctions::default_get(ctx, entity, field_name)
}

fn override_set(
    ctx: FunctionCallContext,
    ent: ReflectReference,
    field_name: ScriptValue,
    value: ScriptValue,
) -> Result<(), InteropError> {
    let world_ctx = ThreadWorldContainer.try_get_context()?;

    let qc_entity_id = world_ctx
        .world
        .get_component_id(TypeId::of::<QCEntity>())?
        .ok_or(InteropError::NotImplemented)?;

    let qc_worldspawn_id = world_ctx
        .world
        .get_component_id(TypeId::of::<QCWorldspawn>())?
        .ok_or(InteropError::NotImplemented)?;

    // TODO: We should handle "sub-field" references.
    let (entity, field_name) = match ent.base.base_id {
        ReflectBase::Component(entity, component_id) if component_id == qc_entity_id => {
            let accessor = field_accessor(&world_ctx, field_name)?;
            (accessor.reflect_reference(entity), accessor.field_name.into())
        }
        ReflectBase::Component(entity, component_id) if component_id == qc_worldspawn_id => {
            let accessor = global_accessor(&world_ctx, field_name)?;
            (accessor.reflect_reference(entity), accessor.field_name.into())
        }
        _ => (ent, field_name),
    };

    MagicFunctions::default_set(ctx, entity, field_name, value)
}

pub fn fields_and_globals_quake1(world: &mut World, worldspawn: Entity) {
    let fields = AccessorBuilder::new(world)
        .field::<Transform>("origin", "translation")
        .field::<EntityAngles>("ideal_yaw", "ideal_yaw_deg")
        .field::<EntityAngles>("yaw_speed", "yaw_speed_deg")
        // TODO: Fill in the rest of the fields
        .build();
    let globals = AccessorBuilder::new(world)
        .field::<GlobalDirections>("v_forward", "forward")
        .field::<GlobalDirections>("v_up", "up")
        .field::<GlobalDirections>("v_right", "right")
        // TODO: Fill in the rest of the globals
        .build();

    world.entity_mut(worldspawn).insert(Fields { fields }).insert(Globals { globals });

    let mut registry = WorldAccessGuard::new_exclusive(world).script_function_registry();

    registry.write().magic_functions = MagicFunctions { get: override_get, set: override_set };
}

// TODO: It would probably be useful to implement this in Lua or something similar
pub fn register_builtins_quake1<'b, 'ns, N: IntoNamespace>(
    namespace: &'b mut NamespaceBuilder<'ns, N>,
) -> &'b mut NamespaceBuilder<'ns, N> {
    fn precache_sound(name: String) -> Result<(), InteropError> {
        worldspawn_mut::<Mut<LevelPrecache>, (), _>(|mut precache| {
            precache.sound.precache(name);

            Ok(())
        })
    }

    fn precache_model(name: String) -> Result<(), InteropError> {
        worldspawn_mut::<Mut<LevelPrecache>, (), _>(|mut precache| {
            precache.model.precache(name);

            Ok(())
        })
    }

    namespace
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
        .register("ChangeYaw", |entity: ReflectReference| -> Result<(), InteropError> {
            ent_mut::<(Ref<EntityAngles>, Mut<Transform>), (), _>(
                &entity,
                |(ent_angles, mut transform)| {
                    let EntityAngles { ideal_yaw_deg: ideal, yaw_speed_deg: speed } = *ent_angles;
                    let angles: QAngles = transform.rotation.into();
                    let cur_angles @ QAngles { yaw_deg: cur, .. } = angles.quantize();

                    if (cur - ideal).abs() < f32::EPSILON {
                        return Ok(());
                    }

                    let angle_delta = ideal - cur;

                    let angle_delta = match angle_delta {
                        180f32.. => angle_delta - 360.,
                        ..-180f32 => angle_delta + 360.,
                        angle_delta => angle_delta,
                    }
                    .clamp(-speed, speed);

                    transform.rotation =
                        QAngles { yaw_deg: cur + angle_delta, ..cur_angles }.quantize().into();

                    Ok(())
                },
            )
        })
        // --------------- MATHS ---------------
        .register("makevectors", |angles: [f32; 3]| -> Result<(), InteropError> {
            worldspawn_mut::<Mut<GlobalDirections>, (), _>(|mut global_directions| {
                let mat = make_vectors(angles);
                global_directions.forward = mat.x_axis;
                global_directions.right = mat.y_axis;
                global_directions.up = mat.z_axis;

                Ok(())
            })
        })
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
        // --------------- STRING ---------------
        .register("ftos", |value: f32| value.to_string())
        .register("vtos", |[x, y, z]: [f32; 3]| format!("{x} {y} {z}"))
        .register("stof", |value: String| -> Result<f32, InteropError> {
            value.parse().map_err(|e| InteropError::External(ExternalError(Arc::new(e))))
        })
        // --------------- NETWORKING ---------------
        .register(
            "stuffcmd",
            |entity: ReflectReference, text: String| -> Result<(), InteropError> {
                ent_mut::<Mut<MessagesReliable>, (), _>(&entity, |mut player_msgs| {
                    ServerCmd::StuffText { text: text.into() }
                        .serialize(&mut player_msgs.messages)
                        .map_err(|e| InteropError::External(ExternalError(Arc::new(e))))?;

                    Ok(())
                })
            },
        )
        .register("multicast", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("bprint", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("sprint", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("dprint", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("eprint", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
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
        // --------------- PHYSICS ---------------
        .register("walkmove", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("droptofloor", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("checkbottom", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("pointcontents", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("traceline", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        // --------------- MUTATE WORLD ---------------
        .register("spawn", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("remove", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("makestatic", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        // --------------- STARTUP ---------------
        .register("precache_sound", precache_sound)
        .register("precache_sound2", precache_sound)
        .register("precache_model", precache_model)
        .register("precache_model2", precache_model)
        .register("setspawnparms", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        // --------------- SOUND ---------------
        .register("sound", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("ambientsound", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        // --------------- DEBUGGING ---------------
        .register("coredump", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("traceon", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("traceoff", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("break", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        // --------------- ERRORS ---------------
        .register("error", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("objerror", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        // --------------- CONSOLE ---------------
        .register("cvar", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("cvar_set", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("localcmd", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        // --------------- INVALID AT RUNTIME ---------------
        .register("precache_file", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("precache_file2", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        // --------------- UNCATEGORIZED ---------------
        // TODO: Categorize these
        .register("checkclient", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("find", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("findradius", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("lightstyle", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("aim", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("nextent", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("movetogoal", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("changelevel", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("centerprint", || -> Result<(), InteropError> {
            Err(InteropError::NotImplemented)
        })
        .register("logfrag", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
        .register("infokey", || -> Result<(), InteropError> { Err(InteropError::NotImplemented) })
}
