// Generated from the core CSQC builtin table (the builtins that both FTEQW and QuakeSpasm-Spiked
// support in CSQC, as in `qcvm_defs_csqc`), with signatures from FTEQW's `pr_dumpplatform`.
// Builtins are implemented by replacing the body of their stub (and moving shared ones that don't
// depend on the client to `core_builtins`).

//! Stubs for the CSQC builtins that aren't [`core_builtins`](crate::common::qc::core_builtins).
//!
//! Each stub has the Rust signature of its QuakeC prototype, and returns an error until it is
//! implemented. Calling one shuts CSQC down (see [`super::call_entry`]).

use bevy_mod_scripting::bindings::{InteropError, NamespaceBuilder, V};
use bevy_mod_scripting_qcvm::QCString;

use crate::common::qc::stub::{Any, Ent, S, Vec3Arg, unimplemented_builtin};

/// Quake builtins whose behaviour depends on running on the client (e.g. local sounds, the
/// client's precaches and collision), so they can't share the server's implementation.
pub fn engine_builtins<N: 'static>(ns: &mut NamespaceBuilder<'_, N>) {
    ns
        // `void(entity e, vector o)`
        .register("setorigin", |_e: Ent, _o: Vec3Arg| -> Result<(), InteropError> {
            unimplemented_builtin!("setorigin", 2)
        })
        // `void(entity e, string m)`
        .register("setmodel", |_e: Ent, _m: S| -> Result<(), InteropError> {
            unimplemented_builtin!("setmodel", 3)
        })
        // `void(entity e, vector min, vector max)`
        .register("setsize", |_e: Ent, _min: Vec3Arg, _max: Vec3Arg| -> Result<(), InteropError> {
            unimplemented_builtin!("setsize", 4)
        })
        // `void(entity e, float chan, string samp, float vol, float atten, optional float speedpct, optional float flags, optional float timeofs)`
        .register(
            "sound",
            |_e: Ent,
             _chan: f32,
             _samp: S,
             _vol: f32,
             _atten: f32,
             _speedpct: f32,
             _flags: f32,
             _timeofs: f32|
             -> Result<(), InteropError> { unimplemented_builtin!("sound", 8) },
        )
        // `void(vector v1, vector v2, float flags, entity ent)`
        .register(
            "traceline",
            |_v1: Vec3Arg, _v2: Vec3Arg, _flags: f32, _ent: Ent| -> Result<(), InteropError> {
                unimplemented_builtin!("traceline", 16)
            },
        )
        // `string(string s)`
        .register("precache_sound", |_s: S| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("precache_sound", 19)
        })
        // `string(string s)`
        .register("precache_model", |_s: S| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("precache_model", 20)
        })
        // `float(float yaw, float dist, optional float settraceglobals)`
        .register(
            "walkmove",
            |_yaw: f32, _dist: f32, _settraceglobals: f32| -> Result<f32, InteropError> {
                unimplemented_builtin!("walkmove", 32)
            },
        )
        // `float()`
        .register("droptofloor", || -> Result<f32, InteropError> {
            unimplemented_builtin!("droptofloor", 34)
        })
        // `void(float lightstyle, string stylestring, optional vector rgb)`
        .register(
            "lightstyle",
            |_lightstyle: f32, _stylestring: S, _rgb: Vec3Arg| -> Result<(), InteropError> {
                unimplemented_builtin!("lightstyle", 35)
            },
        )
        // `float(entity ent)`
        .register("checkbottom", |_ent: Ent| -> Result<f32, InteropError> {
            unimplemented_builtin!("checkbottom", 40)
        })
        // `float(vector pos)`
        .register("pointcontents", |_pos: Vec3Arg| -> Result<f32, InteropError> {
            unimplemented_builtin!("pointcontents", 41)
        })
        // `void(vector pos, vector dir, float colour, float count)`
        .register(
            "particle",
            |_pos: Vec3Arg, _dir: Vec3Arg, _colour: f32, _count: f32| -> Result<(), InteropError> {
                unimplemented_builtin!("particle", 48)
            },
        )
        // `void()`
        .register("ChangeYaw", || -> Result<(), InteropError> {
            unimplemented_builtin!("ChangeYaw", 49)
        })
        // `void(float step)`
        .register("movetogoal", |_step: f32| -> Result<(), InteropError> {
            unimplemented_builtin!("movetogoal", 67)
        })
        // `void(entity e)`
        .register("makestatic", |_e: Ent| -> Result<(), InteropError> {
            unimplemented_builtin!("makestatic", 69)
        })
        // `void(vector pos, string samp, float vol, float atten)`
        .register(
            "ambientsound",
            |_pos: Vec3Arg, _samp: S, _vol: f32, _atten: f32| -> Result<(), InteropError> {
                unimplemented_builtin!("ambientsound", 74)
            },
        )
        // `string(string str)`
        .register("precache_model2", |_str: S| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("precache_model2", 75)
        })
        // `string(string str)`
        .register("precache_sound2", |_str: S| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("precache_sound2", 76)
        });
}

/// Extension builtins that both SSQC and CSQC have in FTEQW, which Seismon's SSQC doesn't
/// implement yet.
pub fn extension_builtins<N: 'static>(ns: &mut NamespaceBuilder<'_, N>) {
    ns
        // `void(entity ent, entity ignore)`
        .register("tracetoss", |_ent: Ent, _ignore: Ent| -> Result<(), InteropError> {
            unimplemented_builtin!("tracetoss", 64)
        })
        // `void(vector start, vector mins, vector maxs, vector end, float nomonsters, entity ent)`
        .register(
            "tracebox",
            |_start: Vec3Arg,
             _mins: Vec3Arg,
             _maxs: Vec3Arg,
             _end: Vec3Arg,
             _nomonsters: f32,
             _ent: Ent|
             -> Result<(), InteropError> { unimplemented_builtin!("tracebox", 90) },
        )
        // `vector(vector org)`
        .register("getlight", |_org: Vec3Arg| -> Result<[f32; 3], InteropError> {
            unimplemented_builtin!("getlight", 92)
        })
        // `void(string soundname, optional float channel, optional float volume)`
        .register(
            "localsound",
            |_soundname: S, _channel: f32, _volume: f32| -> Result<(), InteropError> {
                unimplemented_builtin!("localsound", 177)
            },
        )
        // `float(vector viewpos, entity entity)`
        .register("checkpvs", |_viewpos: Vec3Arg, _entity: Ent| -> Result<f32, InteropError> {
            unimplemented_builtin!("checkpvs", 240)
        })
        // `void(optional entity ent, optional vector neworigin)`
        .register("touchtriggers", |_ent: Ent, _neworigin: Vec3Arg| -> Result<(), InteropError> {
            unimplemented_builtin!("touchtriggers", 279)
        })
        // `void(string texturename, optional float flags, optional float is2d)`
        .register(
            "R_BeginPolygon",
            |_texturename: S, _flags: f32, _is2d: f32| -> Result<(), InteropError> {
                unimplemented_builtin!("R_BeginPolygon", 306)
            },
        )
        // `void(vector org, vector texcoords, vector rgb, float alpha)`
        .register(
            "R_PolygonVertex",
            |_org: Vec3Arg,
             _texcoords: Vec3Arg,
             _rgb: Vec3Arg,
             _alpha: f32|
             -> Result<(), InteropError> {
                unimplemented_builtin!("R_PolygonVertex", 307)
            },
        )
        // `void()`
        .register("R_EndPolygon", || -> Result<(), InteropError> {
            unimplemented_builtin!("R_EndPolygon", 308)
        })
        // `float(string effectname)`
        .register("particleeffectnum", |_effectname: S| -> Result<f32, InteropError> {
            unimplemented_builtin!("particleeffectnum", 335)
        })
        // `void(float effectnum, entity ent, vector start, vector end)`
        .register(
            "trailparticles",
            |_effectnum: f32,
             _ent: Ent,
             _start: Vec3Arg,
             _end: Vec3Arg|
             -> Result<(), InteropError> {
                unimplemented_builtin!("trailparticles", 336)
            },
        )
        // `void(float effectnum, vector origin, optional vector dir, optional float count)`
        .register(
            "pointparticles",
            |_effectnum: f32,
             _origin: Vec3Arg,
             _dir: Vec3Arg,
             _count: f32|
             -> Result<(), InteropError> {
                unimplemented_builtin!("pointparticles", 337)
            },
        )
        // `void(entity ent)`
        .register("runstandardplayerphysics", |_ent: Ent| -> Result<(), InteropError> {
            unimplemented_builtin!("runstandardplayerphysics", 347)
        })
        // `void(vector org, optional float count)`
        .register("te_gunshot", |_org: Vec3Arg, _count: f32| -> Result<(), InteropError> {
            unimplemented_builtin!("te_gunshot", 418)
        })
        // `void(vector org)`
        .register("te_spike", |_org: Vec3Arg| -> Result<(), InteropError> {
            unimplemented_builtin!("te_spike", 419)
        })
        // `void(vector org)`
        .register("te_superspike", |_org: Vec3Arg| -> Result<(), InteropError> {
            unimplemented_builtin!("te_superspike", 420)
        })
        // `void(vector org)`
        .register("te_explosion", |_org: Vec3Arg| -> Result<(), InteropError> {
            unimplemented_builtin!("te_explosion", 421)
        })
        // `void(vector org)`
        .register("te_tarexplosion", |_org: Vec3Arg| -> Result<(), InteropError> {
            unimplemented_builtin!("te_tarexplosion", 422)
        })
        // `void(vector org)`
        .register("te_wizspike", |_org: Vec3Arg| -> Result<(), InteropError> {
            unimplemented_builtin!("te_wizspike", 423)
        })
        // `void(vector org)`
        .register("te_knightspike", |_org: Vec3Arg| -> Result<(), InteropError> {
            unimplemented_builtin!("te_knightspike", 424)
        })
        // `void(vector org)`
        .register("te_lavasplash", |_org: Vec3Arg| -> Result<(), InteropError> {
            unimplemented_builtin!("te_lavasplash", 425)
        })
        // `void(vector org)`
        .register("te_teleport", |_org: Vec3Arg| -> Result<(), InteropError> {
            unimplemented_builtin!("te_teleport", 426)
        })
        // `void(vector org, float color, float colorlength)`
        .register(
            "te_explosion2",
            |_org: Vec3Arg, _color: f32, _colorlength: f32| -> Result<(), InteropError> {
                unimplemented_builtin!("te_explosion2", 427)
            },
        )
        // `void(entity own, vector start, vector end)`
        .register(
            "te_lightning1",
            |_own: Ent, _start: Vec3Arg, _end: Vec3Arg| -> Result<(), InteropError> {
                unimplemented_builtin!("te_lightning1", 428)
            },
        )
        // `void(entity own, vector start, vector end)`
        .register(
            "te_lightning2",
            |_own: Ent, _start: Vec3Arg, _end: Vec3Arg| -> Result<(), InteropError> {
                unimplemented_builtin!("te_lightning2", 429)
            },
        )
        // `void(entity own, vector start, vector end)`
        .register(
            "te_lightning3",
            |_own: Ent, _start: Vec3Arg, _end: Vec3Arg| -> Result<(), InteropError> {
                unimplemented_builtin!("te_lightning3", 430)
            },
        )
        // `void(entity own, vector start, vector end)`
        .register(
            "te_beam",
            |_own: Ent, _start: Vec3Arg, _end: Vec3Arg| -> Result<(), InteropError> {
                unimplemented_builtin!("te_beam", 431)
            },
        )
        // `float(entity e, float s)`
        .register("getsurfacenumpoints", |_e: Ent, _s: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("getsurfacenumpoints", 434)
        })
        // `vector(entity e, float s, float n)`
        .register(
            "getsurfacepoint",
            |_e: Ent, _s: f32, _n: f32| -> Result<[f32; 3], InteropError> {
                unimplemented_builtin!("getsurfacepoint", 435)
            },
        )
        // `vector(entity e, float s)`
        .register("getsurfacenormal", |_e: Ent, _s: f32| -> Result<[f32; 3], InteropError> {
            unimplemented_builtin!("getsurfacenormal", 436)
        })
        // `string(entity e, float s)`
        .register("getsurfacetexture", |_e: Ent, _s: f32| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("getsurfacetexture", 437)
        })
        // `float(entity e, vector p)`
        .register("getsurfacenearpoint", |_e: Ent, _p: Vec3Arg| -> Result<f32, InteropError> {
            unimplemented_builtin!("getsurfacenearpoint", 438)
        })
        // `vector(entity e, float s, vector p)`
        .register(
            "getsurfaceclippedpoint",
            |_e: Ent, _s: f32, _p: Vec3Arg| -> Result<[f32; 3], InteropError> {
                unimplemented_builtin!("getsurfaceclippedpoint", 439)
            },
        )
        // `void(entity e, entity tagentity, string tagname)`
        .register(
            "setattachment",
            |_e: Ent, _tagentity: Ent, _tagname: S| -> Result<(), InteropError> {
                unimplemented_builtin!("setattachment", 443)
            },
        )
        // `void(vector origin, string sample, float volume, float attenuation)`
        .register(
            "pointsound",
            |_origin: Vec3Arg,
             _sample: S,
             _volume: f32,
             _attenuation: f32|
             -> Result<(), InteropError> { unimplemented_builtin!("pointsound", 483) },
        )
        // `vector(entity e, float s, float n, float a)`
        .register(
            "getsurfacepointattribute",
            |_e: Ent, _s: f32, _n: f32, _a: f32| -> Result<[f32; 3], InteropError> {
                unimplemented_builtin!("getsurfacepointattribute", 486)
            },
        )
        // `float(entity e, float s)`
        .register("getsurfacenumtriangles", |_e: Ent, _s: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("getsurfacenumtriangles", 628)
        })
        // `vector(entity e, float s, float n)`
        .register(
            "getsurfacetriangle",
            |_e: Ent, _s: f32, _n: f32| -> Result<[f32; 3], InteropError> {
                unimplemented_builtin!("getsurfacetriangle", 629)
            },
        );
}

/// Builtins that only CSQC has.
pub fn csqc_only_builtins<N: 'static>(ns: &mut NamespaceBuilder<'_, N>) {
    ns
        // `void()`
        .register("clearscene", || -> Result<(), InteropError> {
            unimplemented_builtin!("clearscene", 300)
        })
        // `void(float mask)`
        .register("addentities", |_mask: f32| -> Result<(), InteropError> {
            unimplemented_builtin!("addentities", 301)
        })
        // `void(entity ent)`
        .register("addentity", |_ent: Ent| -> Result<(), InteropError> {
            unimplemented_builtin!("addentity", 302)
        })
        // `float(float property, ...)`
        .register(
            "setproperty",
            |_property: f32,
             _va0: Any,
             _va1: Any,
             _va2: Any,
             _va3: Any,
             _va4: Any,
             _va5: Any,
             _va6: Any|
             -> Result<f32, InteropError> {
                unimplemented_builtin!("setproperty", 303)
            },
        )
        // `void()`
        .register("renderscene", || -> Result<(), InteropError> {
            unimplemented_builtin!("renderscene", 304)
        })
        // `float(vector org, float radius, vector lightcolours, optional float style, optional string cubemapname, optional float pflags)`
        .register(
            "dynamiclight_add",
            |_org: Vec3Arg,
             _radius: f32,
             _lightcolours: Vec3Arg,
             _style: f32,
             _cubemapname: S,
             _pflags: f32|
             -> Result<f32, InteropError> {
                unimplemented_builtin!("dynamiclight_add", 305)
            },
        )
        // `__variant(float property)`
        .register("getproperty", |_property: f32| -> Result<Any, InteropError> {
            unimplemented_builtin!("getproperty", 309)
        })
        // `vector(vector v)`
        .register("unproject", |_v: Vec3Arg| -> Result<[f32; 3], InteropError> {
            unimplemented_builtin!("unproject", 310)
        })
        // `vector(vector v)`
        .register("project", |_v: Vec3Arg| -> Result<[f32; 3], InteropError> {
            unimplemented_builtin!("project", 311)
        })
        // `void(vector origin, vector forward, vector right, vector up, optional float reverbtype)`
        .register(
            "SetListener",
            |_origin: Vec3Arg,
             _forward: Vec3Arg,
             _right: Vec3Arg,
             _up: Vec3Arg,
             _reverbtype: f32|
             -> Result<(), InteropError> { unimplemented_builtin!("SetListener", 351) },
        )
        // `void(string evname, string evargs, ...)`
        .register(
            "sendevent",
            |_evname: S,
             _evargs: S,
             _va0: Any,
             _va1: Any,
             _va2: Any,
             _va3: Any,
             _va4: Any,
             _va5: Any|
             -> Result<(), InteropError> { unimplemented_builtin!("sendevent", 359) },
        )
        // `float()`
        .register("readbyte", || -> Result<f32, InteropError> {
            unimplemented_builtin!("readbyte", 360)
        })
        // `float()`
        .register("readchar", || -> Result<f32, InteropError> {
            unimplemented_builtin!("readchar", 361)
        })
        // `float()`
        .register("readshort", || -> Result<f32, InteropError> {
            unimplemented_builtin!("readshort", 362)
        })
        // `float()`
        .register("readlong", || -> Result<f32, InteropError> {
            unimplemented_builtin!("readlong", 363)
        })
        // `float()`
        .register("readcoord", || -> Result<f32, InteropError> {
            unimplemented_builtin!("readcoord", 364)
        })
        // `float()`
        .register("readangle", || -> Result<f32, InteropError> {
            unimplemented_builtin!("readangle", 365)
        })
        // `string()`
        .register("readstring", || -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("readstring", 366)
        })
        // `float()`
        .register("readfloat", || -> Result<f32, InteropError> {
            unimplemented_builtin!("readfloat", 367)
        })
        // `float()`
        .register("readentitynum", || -> Result<f32, InteropError> {
            unimplemented_builtin!("readentitynum", 368)
        })
        // `__variant(float entnum, float fieldnum)`
        .register("getentity", |_entnum: f32, _fieldnum: f32| -> Result<Any, InteropError> {
            unimplemented_builtin!("getentity", 504)
        })
        // `float(string sample)`
        .register("soundlength", |_sample: S| -> Result<f32, InteropError> {
            unimplemented_builtin!("soundlength", 534)
        })
        // `__double()`
        .register("readdouble", || -> Result<Any, InteropError> {
            unimplemented_builtin!("readdouble", 0)
        })
        // `int()`
        .register("readint", || -> Result<Any, InteropError> {
            unimplemented_builtin!("readint", 0)
        })
        // `__int64()`
        .register("readint64", || -> Result<Any, InteropError> {
            unimplemented_builtin!("readint64", 0)
        })
        // `__uint64()`
        .register("readuint64", || -> Result<Any, InteropError> {
            unimplemented_builtin!("readuint64", 0)
        });
}
