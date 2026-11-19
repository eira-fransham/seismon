//! QuakeC entity fields, stored in components.
//!
//! Every QuakeC entity (including worldspawn) has every field, so every entity has all of the
//! components here (see [`qc_entity_bundle`]). The fields defined by `progdefs.h` are split by
//! subsystem, and fields defined by the `progs.dat` itself are in [`QcExtraFields`].
//!
//! [`QcFieldTable`] maps field names (as QuakeC uses them) to accessors, which is how QuakeC reads
//! and writes fields (through `bevy_mod_scripting`'s magic functions), and how the fields are listed
//! for debugging.

use std::sync::Arc;

use bevy::prelude::*;
use bevy_mod_scripting_qcvm::{
    QCEntity, QCFunction, QCString,
    qcvm::{FieldDef, Type, VmScalarType, VmType},
};
use hashbrown::HashMap;
use snafu::Snafu;

use crate::server::{
    limit_vec3,
    qc::{edict::Edict, is_storable},
};

/// The first field offset after the fields defined by `progdefs.h`.
pub const PROGDEFS_FIELD_COUNT: u16 = 105;

/// The value of a field or global.
#[derive(Debug, Clone, PartialEq)]
pub enum QcValue {
    Float(f32),
    Vector(Vec3),
    String(QCString),
    /// An entity. `world` is the worldspawn entity.
    Entity(Entity),
    Function(QCFunction),
}

impl QcValue {
    /// The zero value of type `ty`, or `None` if fields and globals can't have that type (see
    /// [`is_storable`]). Entities are zero when they're `world`.
    pub fn zero(ty: Type, world: Entity) -> Option<Self> {
        Some(match ty {
            Type::Float => Self::Float(0.),
            Type::Vector => Self::Vector(Vec3::ZERO),
            Type::String => Self::String(QCString::null()),
            Type::Entity => Self::Entity(world),
            Type::Function => Self::Function(QCFunction::NULL),
            Type::AnyScalar | Type::Void | Type::Field => return None,
        })
    }

    pub fn kind(&self) -> Type {
        match self {
            Self::Float(_) => Type::Float,
            Self::Vector(_) => Type::Vector,
            Self::String(_) => Type::String,
            Self::Entity(_) => Type::Entity,
            Self::Function(_) => Type::Function,
        }
    }
}

#[derive(Debug, Snafu)]
pub enum FieldError {
    #[snafu(display("no field or global named {name}"))]
    NoSuchField { name: String },
    #[snafu(display("{entity} has no QuakeC fields"))]
    NoEntity { entity: Entity },
    #[snafu(display("type mismatch: expected {expected}, found {found}"))]
    TypeMismatch { expected: Type, found: Type },
}

pub type Getter = fn(&World, Entity) -> Option<QcValue>;
pub type Setter = fn(&mut World, Entity, QcValue) -> Result<(), FieldError>;

/// Accessors for a field (or global) stored in a component.
#[derive(Copy, Clone)]
pub struct Accessor {
    pub kind: Type,
    pub get: Getter,
    pub set: Setter,
}

pub(crate) fn type_mismatch(expected: Type, value: &QcValue) -> FieldError {
    FieldError::TypeMismatch { expected, found: value.kind() }
}

/// Defines components holding QuakeC fields (or globals), along with accessors for each field by
/// its QuakeC name. Vectors also get accessors for `{name}_x` etc.
macro_rules! qc_components {
    ($accessors:ident; $($(#[$attr:meta])* $component:ident { $($field:ident: $kind:ident = $name:literal),* $(,)? })*) => {
        $(
            $(#[$attr])*
            #[derive(Component, Debug, Clone, PartialEq)]
            pub struct $component {
                $(pub $field: qc_components!(@type $kind),)*
            }

            impl $component {
                /// All fields zero, with entities referring to `world`.
                #[allow(unused_variables)]
                pub fn new(world: Entity) -> Self {
                    Self { $($field: qc_components!(@zero $kind, world),)* }
                }
            }
        )*

        /// Accessors for every field, by QuakeC name.
        pub fn $accessors() -> Vec<(&'static str, Accessor)> {
            #[allow(unused_mut)]
            let mut out = Vec::new();
            $($(qc_components!(@accessor out, $component, $field, $kind, $name);)*)*
            out
        }
    };

    (@type F) => { f32 };
    (@type V) => { Vec3 };
    (@type S) => { QCString };
    (@type E) => { Entity };
    (@type Fn) => { QCFunction };

    (@zero F, $world:ident) => { 0. };
    (@zero V, $world:ident) => { Vec3::ZERO };
    (@zero S, $world:ident) => { QCString::null() };
    (@zero E, $world:ident) => { $world };
    (@zero Fn, $world:ident) => { QCFunction::NULL };

    (@accessor $out:ident, $component:ident, $field:ident, F, $name:literal) => {
        $out.push(($name, Accessor {
            kind: Type::Float,
            get: |world, entity| Some(QcValue::Float(world.get::<$component>(entity)?.$field)),
            set: |world, entity, value| {
                let QcValue::Float(value) = value else {
                    return Err(type_mismatch(Type::Float, &value));
                };
                world.get_mut::<$component>(entity).ok_or(FieldError::NoEntity { entity })?.$field = value;
                Ok(())
            },
        }));
    };
    (@accessor $out:ident, $component:ident, $field:ident, V, $name:literal) => {
        $out.push(($name, Accessor {
            kind: Type::Vector,
            get: |world, entity| Some(QcValue::Vector(world.get::<$component>(entity)?.$field)),
            set: |world, entity, value| {
                let QcValue::Vector(value) = value else {
                    return Err(type_mismatch(Type::Vector, &value));
                };
                // Any invalid values are just nulled out.
                world.get_mut::<$component>(entity).ok_or(FieldError::NoEntity { entity })?.$field =
                    limit_vec3(value, None);
                Ok(())
            },
        }));
        qc_components!(@component $out, $component, $field, x, concat!($name, "_x"));
        qc_components!(@component $out, $component, $field, y, concat!($name, "_y"));
        qc_components!(@component $out, $component, $field, z, concat!($name, "_z"));
    };
    (@component $out:ident, $component:ident, $field:ident, $axis:ident, $name:expr) => {
        $out.push(($name, Accessor {
            kind: Type::Float,
            get: |world, entity| Some(QcValue::Float(world.get::<$component>(entity)?.$field.$axis)),
            set: |world, entity, value| {
                let QcValue::Float(value) = value else {
                    return Err(type_mismatch(Type::Float, &value));
                };
                world.get_mut::<$component>(entity).ok_or(FieldError::NoEntity { entity })?.$field.$axis =
                    value;
                Ok(())
            },
        }));
    };
    (@accessor $out:ident, $component:ident, $field:ident, S, $name:literal) => {
        $out.push(($name, Accessor {
            kind: Type::String,
            get: |world, entity| Some(QcValue::String(world.get::<$component>(entity)?.$field.clone())),
            set: |world, entity, value| {
                let QcValue::String(value) = value else {
                    return Err(type_mismatch(Type::String, &value));
                };
                world.get_mut::<$component>(entity).ok_or(FieldError::NoEntity { entity })?.$field = value;
                Ok(())
            },
        }));
    };
    (@accessor $out:ident, $component:ident, $field:ident, E, $name:literal) => {
        $out.push(($name, Accessor {
            kind: Type::Entity,
            get: |world, entity| Some(QcValue::Entity(world.get::<$component>(entity)?.$field)),
            set: |world, entity, value| {
                let QcValue::Entity(value) = value else {
                    return Err(type_mismatch(Type::Entity, &value));
                };
                world.get_mut::<$component>(entity).ok_or(FieldError::NoEntity { entity })?.$field = value;
                Ok(())
            },
        }));
    };
    (@accessor $out:ident, $component:ident, $field:ident, Fn, $name:literal) => {
        $out.push(($name, Accessor {
            kind: Type::Function,
            get: |world, entity| Some(QcValue::Function(world.get::<$component>(entity)?.$field)),
            set: |world, entity, value| {
                let QcValue::Function(value) = value else {
                    return Err(type_mismatch(Type::Function, &value));
                };
                world.get_mut::<$component>(entity).ok_or(FieldError::NoEntity { entity })?.$field = value;
                Ok(())
            },
        }));
    };
}

pub(crate) use qc_components;

qc_components! {
    progdefs_field_accessors;

    /// Fields used for movement and collision.
    EntPhysics {
        absmin: V = "absmin",
        absmax: V = "absmax",
        ltime: F = "ltime",
        movetype: F = "movetype",
        solid: F = "solid",
        origin: V = "origin",
        oldorigin: V = "oldorigin",
        velocity: V = "velocity",
        angles: V = "angles",
        avelocity: V = "avelocity",
        mins: V = "mins",
        maxs: V = "maxs",
        size: V = "size",
        groundentity: E = "groundentity",
        flags: F = "flags",
        waterlevel: F = "waterlevel",
        watertype: F = "watertype",
    }

    /// Fields sent to clients to draw the entity.
    EntRender {
        modelindex: F = "modelindex",
        model: S = "model",
        frame: F = "frame",
        skin: F = "skin",
        effects: F = "effects",
        colormap: F = "colormap",
    }

    /// Functions called by the engine.
    EntCallbacks {
        touch: Fn = "touch",
        use_: Fn = "use",
        think: Fn = "think",
        blocked: Fn = "blocked",
        nextthink: F = "nextthink",
    }

    /// Fields set by the map.
    EntInfo {
        classname: S = "classname",
        netname: S = "netname",
        target: S = "target",
        targetname: S = "targetname",
        message: S = "message",
        noise: S = "noise",
        noise1: S = "noise1",
        noise2: S = "noise2",
        noise3: S = "noise3",
        spawnflags: F = "spawnflags",
        sounds: F = "sounds",
    }

    /// Fields used by players (though any entity can have them).
    EntPlayer {
        punchangle: V = "punchangle",
        health: F = "health",
        frags: F = "frags",
        weapon: F = "weapon",
        weaponmodel: S = "weaponmodel",
        weaponframe: F = "weaponframe",
        currentammo: F = "currentammo",
        ammo_shells: F = "ammo_shells",
        ammo_nails: F = "ammo_nails",
        ammo_rockets: F = "ammo_rockets",
        ammo_cells: F = "ammo_cells",
        items: F = "items",
        takedamage: F = "takedamage",
        deadflag: F = "deadflag",
        view_ofs: V = "view_ofs",
        button0: F = "button0",
        button1: F = "button1",
        button2: F = "button2",
        impulse: F = "impulse",
        fixangle: F = "fixangle",
        v_angle: V = "v_angle",
        idealpitch: F = "idealpitch",
        team: F = "team",
        max_health: F = "max_health",
        teleport_time: F = "teleport_time",
        armortype: F = "armortype",
        armorvalue: F = "armorvalue",
    }

    /// Fields used for monster AI and relationships between entities.
    EntAi {
        chain: E = "chain",
        enemy: E = "enemy",
        ideal_yaw: F = "ideal_yaw",
        yaw_speed: F = "yaw_speed",
        aiment: E = "aiment",
        goalentity: E = "goalentity",
        dmg_take: F = "dmg_take",
        dmg_save: F = "dmg_save",
        dmg_inflictor: E = "dmg_inflictor",
        owner: E = "owner",
        movedir: V = "movedir",
    }
}

/// Fields defined by the `progs.dat` rather than `progdefs.h`, by scalar offset from
/// [`PROGDEFS_FIELD_COUNT`]. Vectors take three slots.
///
/// Slots that haven't been written yet have their zero value (see [`QcFieldTable`]).
#[derive(Component, Debug, Clone, Default, PartialEq)]
pub struct QcExtraFields(pub Vec<QcValue>);

/// The entity's baseline, as last sent to clients.
#[derive(Component, Debug, Clone, Default)]
pub struct Baseline(pub crate::common::net::EntityState);

/// The area node that the entity is linked into, if any.
#[derive(Component, Debug, Clone, Default)]
pub struct AreaLink(pub Option<usize>);

/// All of the components of a QuakeC entity, with every field zero.
pub fn qc_entity_bundle(world: Entity, edict: u16) -> impl Bundle {
    (
        QCEntity,
        Edict(edict),
        EntPhysics::new(world),
        EntRender::new(world),
        EntCallbacks::new(world),
        EntInfo::new(world),
        EntPlayer::new(world),
        EntAi::new(world),
        (QcExtraFields::default(), Baseline::default(), AreaLink::default()),
    )
}

#[derive(Copy, Clone)]
enum FieldAccess {
    Component(Accessor),
    /// A field in [`QcExtraFields`], with the slot it (or its first component) is in.
    Extra {
        slot: usize,
        kind: Type,
    },
}

impl FieldAccess {
    fn kind(&self) -> Type {
        match self {
            Self::Component(accessor) => accessor.kind,
            Self::Extra { kind, .. } => *kind,
        }
    }
}

/// A field, as listed by [`QcFieldTable::fields`].
#[derive(Debug, Clone)]
pub struct FieldInfo {
    pub name: String,
    pub offset: u16,
    pub kind: Type,
}

/// Accessors for every entity field, by name.
#[derive(Clone)]
pub struct QcFieldTable {
    by_name: HashMap<String, FieldAccess>,
    /// Names of the fields at each offset, used for `find`, which is passed a field offset.
    by_offset: HashMap<u16, String>,
    /// The fields in the `progs.dat` (without the `_x` etc. aliases of vectors), in offset order.
    fields: Vec<FieldInfo>,
    /// The zero value of each slot in [`QcExtraFields`].
    extra_zero: Vec<QcValue>,
}

impl QcFieldTable {
    /// Builds the table for the fields defined by a `progs.dat`.
    pub fn new(field_defs: &[FieldDef], world: Entity) -> Self {
        let mut by_name = progdefs_field_accessors()
            .into_iter()
            .map(|(name, accessor)| (name.to_owned(), FieldAccess::Component(accessor)))
            .collect::<HashMap<_, _>>();
        let mut by_offset = HashMap::new();
        let mut extra_zero = Vec::new();
        let mut fields = Vec::new();

        let mut defs = field_defs.iter().collect::<Vec<_>>();
        // Vectors first, so that the `_x` etc. aliases can be recognised.
        defs.sort_by_key(|def| (def.offset, def.type_ != VmType::Vector));

        let is_alias = |def: &FieldDef| {
            let name = def.name.to_string_lossy();
            def.type_ == VmType::Scalar(VmScalarType::Float)
                && ["_x", "_y", "_z"].iter().enumerate().any(|(i, suffix)| {
                    name.strip_suffix(suffix).is_some_and(|base| {
                        field_defs.iter().any(|other| {
                            other.type_ == VmType::Vector
                                && other.offset + i as u16 == def.offset
                                && *other.name.to_string_lossy() == *base
                        })
                    })
                })
        };

        for def in defs {
            let Some(kind) = Type::try_from(def.type_).ok().filter(|ty| is_storable(*ty)) else {
                continue;
            };
            let name = def.name.to_string_lossy().into_owned();

            by_offset.entry(def.offset).or_insert_with(|| name.clone());

            if is_alias(def) {
                continue;
            }

            fields.push(FieldInfo { name: name.clone(), offset: def.offset, kind });

            if def.offset < PROGDEFS_FIELD_COUNT {
                continue;
            }

            let slot = (def.offset - PROGDEFS_FIELD_COUNT) as usize;
            let len = if kind == Type::Vector { 3 } else { 1 };
            if extra_zero.len() < slot + len {
                extra_zero.resize(slot + len, QcValue::Float(0.));
            }

            if kind == Type::Vector {
                for (i, axis) in ["x", "y", "z"].iter().enumerate() {
                    extra_zero[slot + i] = QcValue::Float(0.);
                    by_name.insert(
                        format!("{name}_{axis}"),
                        FieldAccess::Extra { slot: slot + i, kind: Type::Float },
                    );
                }
            } else {
                let Some(zero) = QcValue::zero(kind, world) else {
                    continue;
                };
                extra_zero[slot] = zero;
            }

            by_name.insert(name, FieldAccess::Extra { slot, kind });
        }

        Self { by_name, by_offset, fields, extra_zero }
    }

    /// The fields defined by the `progs.dat`, in offset order (without vector component aliases).
    pub fn fields(&self) -> &[FieldInfo] {
        &self.fields
    }

    /// The name of the field at `offset`.
    pub fn name_at(&self, offset: u16) -> Option<&str> {
        self.by_offset.get(&offset).map(String::as_str)
    }

    /// The type of the field named `name`.
    pub fn kind(&self, name: &str) -> Option<Type> {
        self.by_name.get(name).map(FieldAccess::kind)
    }

    fn access(&self, name: &str) -> Result<FieldAccess, FieldError> {
        self.by_name
            .get(name)
            .copied()
            .ok_or_else(|| FieldError::NoSuchField { name: name.to_owned() })
    }

    pub fn get(&self, world: &World, entity: Entity, name: &str) -> Result<QcValue, FieldError> {
        match self.access(name)? {
            FieldAccess::Component(accessor) => {
                (accessor.get)(world, entity).ok_or(FieldError::NoEntity { entity })
            }
            FieldAccess::Extra { slot, kind } => {
                let extra =
                    world.get::<QcExtraFields>(entity).ok_or(FieldError::NoEntity { entity })?;
                let scalar = |slot: usize| {
                    extra
                        .0
                        .get(slot)
                        .or_else(|| self.extra_zero.get(slot))
                        .cloned()
                        .ok_or_else(|| FieldError::NoSuchField { name: name.to_owned() })
                };

                if kind == Type::Vector {
                    let [x, y, z] = [slot, slot + 1, slot + 2].map(scalar);
                    match (x?, y?, z?) {
                        (QcValue::Float(x), QcValue::Float(y), QcValue::Float(z)) => {
                            Ok(QcValue::Vector(Vec3::new(x, y, z)))
                        }
                        (other, ..) => Err(type_mismatch(Type::Float, &other)),
                    }
                } else {
                    scalar(slot)
                }
            }
        }
    }

    pub fn set(
        &self,
        world: &mut World,
        entity: Entity,
        name: &str,
        value: QcValue,
    ) -> Result<(), FieldError> {
        match self.access(name)? {
            FieldAccess::Component(accessor) => (accessor.set)(world, entity, value),
            FieldAccess::Extra { slot, kind } => {
                if value.kind() != kind {
                    return Err(type_mismatch(kind, &value));
                }

                let mut extra = world
                    .get_mut::<QcExtraFields>(entity)
                    .ok_or(FieldError::NoEntity { entity })?;
                let len = if kind == Type::Vector { 3 } else { 1 };
                if extra.0.len() < slot + len {
                    let start = extra.0.len();
                    extra.0.extend_from_slice(&self.extra_zero[start..slot + len]);
                }

                match value {
                    QcValue::Vector(v) => {
                        let v = limit_vec3(v, None);
                        for (i, value) in v.to_array().into_iter().enumerate() {
                            extra.0[slot + i] = QcValue::Float(value);
                        }
                    }
                    scalar => extra.0[slot] = scalar,
                }

                Ok(())
            }
        }
    }
}

/// The field and global tables for the current level.
#[derive(Resource, Clone)]
pub struct QcTables {
    pub fields: Arc<QcFieldTable>,
    pub globals: Arc<super::globals::QcGlobalTable>,
    pub worldspawn: Entity,
    pub qc_entity_component: bevy::ecs::component::ComponentId,
    pub qc_worldspawn_component: bevy::ecs::component::ComponentId,
}

#[cfg(test)]
mod tests {
    use super::*;
    use bevy_mod_scripting_qcvm::qcvm::strum::VariantArray as _;
    use qcvm_defs_quake1::FieldAddr;

    #[test]
    fn every_progdefs_field_has_an_accessor() {
        let accessors = progdefs_field_accessors();
        for field in FieldAddr::VARIANTS {
            use bevy_mod_scripting_qcvm::qcvm::Address as _;
            assert!(
                accessors.iter().any(|(name, _)| *name == field.name()),
                "No accessor for {}",
                field.name()
            );
        }
        assert_eq!(accessors.len(), FieldAddr::VARIANTS.len());
    }
}
