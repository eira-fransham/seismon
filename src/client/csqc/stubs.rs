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

use crate::common::qc::stub::{Any, Ent, Fld, S, Vec3Arg, unimplemented_builtin};

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
        // `float(float angle)`
        .register("sin", |_angle: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("sin", 60)
        })
        // `float(float angle)`
        .register("cos", |_angle: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("cos", 61)
        })
        // `float(float value)`
        .register("sqrt", |_value: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("sqrt", 62)
        })
        // `void(entity ent, entity ignore)`
        .register("tracetoss", |_ent: Ent, _ignore: Ent| -> Result<(), InteropError> {
            unimplemented_builtin!("tracetoss", 64)
        })
        // `string(entity ent)`
        .register("etos", |_ent: Ent| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("etos", 65)
        })
        // `float(string)`
        .register("stof", |_arg0: S| -> Result<f32, InteropError> {
            unimplemented_builtin!("stof", 81)
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
        // `vector()`
        .register("randomvec", || -> Result<[f32; 3], InteropError> {
            unimplemented_builtin!("randomvec", 91)
        })
        // `vector(vector org)`
        .register("getlight", |_org: Vec3Arg| -> Result<[f32; 3], InteropError> {
            unimplemented_builtin!("getlight", 92)
        })
        // `float(string cvarname, string defaultvalue, optional float flags)`
        .register(
            "registercvar",
            |_cvarname: S, _defaultvalue: S, _flags: f32| -> Result<f32, InteropError> {
                unimplemented_builtin!("registercvar", 93)
            },
        )
        // `float(float a, float b, ...)`
        .register(
            "min",
            |_a: f32,
             _b: f32,
             _va0: Any,
             _va1: Any,
             _va2: Any,
             _va3: Any,
             _va4: Any,
             _va5: Any|
             -> Result<f32, InteropError> { unimplemented_builtin!("min", 94) },
        )
        // `float(float a, float b, ...)`
        .register(
            "max",
            |_a: f32,
             _b: f32,
             _va0: Any,
             _va1: Any,
             _va2: Any,
             _va3: Any,
             _va4: Any,
             _va5: Any|
             -> Result<f32, InteropError> { unimplemented_builtin!("max", 95) },
        )
        // `float(float minimum, float val, float maximum)`
        .register("bound", |_minimum: f32, _val: f32, _maximum: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("bound", 96)
        })
        // `float(float value, float exp)`
        .register("pow", |_value: f32, _exp: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("pow", 97)
        })
        // `entity(entity start, .__variant fld, __variant match)`
        .register("findfloat", |_start: Ent, _fld: Fld, _match: Any| -> Result<Ent, InteropError> {
            unimplemented_builtin!("findfloat", 98)
        })
        // `float(string extname)`
        .register("checkextension", |_extname: S| -> Result<f32, InteropError> {
            unimplemented_builtin!("checkextension", 99)
        })
        // `float(float value)`
        .register("anglemod", |_value: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("anglemod", 102)
        })
        // `filestream(string filename, float mode, optional float mmapminsize)`
        .register(
            "fopen",
            |_filename: S, _mode: f32, _mmapminsize: f32| -> Result<f32, InteropError> {
                unimplemented_builtin!("fopen", 110)
            },
        )
        // `void(filestream fhandle)`
        .register("fclose", |_fhandle: f32| -> Result<(), InteropError> {
            unimplemented_builtin!("fclose", 111)
        })
        // `string(filestream fhandle)`
        .register("fgets", |_fhandle: f32| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("fgets", 112)
        })
        // `void(filestream fhandle, string s, optional string s2, optional string s3, optional string s4, optional string s5, optional string s6, optional string s7)`
        .register(
            "fputs",
            |_fhandle: f32,
             _s: S,
             _s2: S,
             _s3: S,
             _s4: S,
             _s5: S,
             _s6: S,
             _s7: S|
             -> Result<(), InteropError> { unimplemented_builtin!("fputs", 113) },
        )
        // `float(string s)`
        .register("strlen", |_s: S| -> Result<f32, InteropError> {
            unimplemented_builtin!("strlen", 114)
        })
        // `string(string s1, optional string s2, optional string s3, optional string s4, optional string s5, optional string s6, optional string s7, optional string s8)`
        .register(
            "strcat",
            |_s1: S,
             _s2: S,
             _s3: S,
             _s4: S,
             _s5: S,
             _s6: S,
             _s7: S,
             _s8: S|
             -> Result<V<QCString>, InteropError> {
                unimplemented_builtin!("strcat", 115)
            },
        )
        // `string(string s, float start, float length)`
        .register(
            "substring",
            |_s: S, _start: f32, _length: f32| -> Result<V<QCString>, InteropError> {
                unimplemented_builtin!("substring", 116)
            },
        )
        // `vector(string s)`
        .register("stov", |_s: S| -> Result<[f32; 3], InteropError> {
            unimplemented_builtin!("stov", 117)
        })
        // `string(string s, ...)`
        .register(
            "strzone",
            |_s: S,
             _va0: Any,
             _va1: Any,
             _va2: Any,
             _va3: Any,
             _va4: Any,
             _va5: Any,
             _va6: Any|
             -> Result<V<QCString>, InteropError> {
                unimplemented_builtin!("strzone", 118)
            },
        )
        // `void(string s)`
        .register("strunzone", |_s: S| -> Result<(), InteropError> {
            unimplemented_builtin!("strunzone", 119)
        })
        // `void(string soundname, optional float channel, optional float volume)`
        .register(
            "localsound",
            |_soundname: S, _channel: f32, _volume: f32| -> Result<(), InteropError> {
                unimplemented_builtin!("localsound", 177)
            },
        )
        // `float(float number, float quantity)`
        .register("bitshift", |_number: f32, _quantity: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("bitshift", 218)
        })
        // `float(string s1, string sub, optional float startidx)`
        .register("strstrofs", |_s1: S, _sub: S, _startidx: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("strstrofs", 221)
        })
        // `float(string str, float index)`
        .register("str2chr", |_str: S, _index: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("str2chr", 222)
        })
        // `string(float chr, ...)`
        .register(
            "chr2str",
            |_chr: f32,
             _va0: Any,
             _va1: Any,
             _va2: Any,
             _va3: Any,
             _va4: Any,
             _va5: Any,
             _va6: Any|
             -> Result<V<QCString>, InteropError> {
                unimplemented_builtin!("chr2str", 223)
            },
        )
        // `string(float ccase, float redalpha, float redchars, string str, ...)`
        .register(
            "strconv",
            |_ccase: f32,
             _redalpha: f32,
             _redchars: f32,
             _str: S,
             _va0: Any,
             _va1: Any,
             _va2: Any,
             _va3: Any|
             -> Result<V<QCString>, InteropError> {
                unimplemented_builtin!("strconv", 224)
            },
        )
        // `string(float pad, string str1, ...)`
        .register(
            "strpad",
            |_pad: f32,
             _str1: S,
             _va0: Any,
             _va1: Any,
             _va2: Any,
             _va3: Any,
             _va4: Any,
             _va5: Any|
             -> Result<V<QCString>, InteropError> {
                unimplemented_builtin!("strpad", 225)
            },
        )
        // `infostring(infostring old, string key, string value)`
        .register("infoadd", |_old: S, _key: S, _value: S| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("infoadd", 226)
        })
        // `string(infostring info, string key)`
        .register("infoget", |_info: S, _key: S| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("infoget", 227)
        })
        // `float(string s1, string s2, optional float len, optional float s1ofs, optional float s2ofs)`
        .register(
            "strncmp",
            |_s1: S, _s2: S, _len: f32, _s1ofs: f32, _s2ofs: f32| -> Result<f32, InteropError> {
                unimplemented_builtin!("strncmp", 228)
            },
        )
        // `float(string s1, string s2)`
        .register("strcasecmp", |_s1: S, _s2: S| -> Result<f32, InteropError> {
            unimplemented_builtin!("strcasecmp", 229)
        })
        // `float(string s1, string s2, float len, optional float s1ofs, optional float s2ofs)`
        .register(
            "strncasecmp",
            |_s1: S, _s2: S, _len: f32, _s1ofs: f32, _s2ofs: f32| -> Result<f32, InteropError> {
                unimplemented_builtin!("strncasecmp", 230)
            },
        )
        // `float(vector viewpos, entity entity)`
        .register("checkpvs", |_viewpos: Vec3Arg, _entity: Ent| -> Result<f32, InteropError> {
            unimplemented_builtin!("checkpvs", 240)
        })
        // `float(float dividend, float divisor)`
        .register("mod", |_dividend: f32, _divisor: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("mod", 245)
        })
        // `int(string)`
        .register("stoi", |_arg0: S| -> Result<Any, InteropError> {
            unimplemented_builtin!("stoi", 259)
        })
        // `string(int)`
        .register("itos", |_arg0: Any| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("itos", 260)
        })
        // `int(string)`
        .register("stoh", |_arg0: S| -> Result<Any, InteropError> {
            unimplemented_builtin!("stoh", 261)
        })
        // `string(int)`
        .register("htos", |_arg0: Any| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("htos", 262)
        })
        // `float(float modidx, string framename)`
        .register("frameforname", |_modidx: f32, _framename: S| -> Result<f32, InteropError> {
            unimplemented_builtin!("frameforname", 276)
        })
        // `float(float modidx, float framenum)`
        .register("frameduration", |_modidx: f32, _framenum: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("frameduration", 277)
        })
        // `void(optional entity ent, optional vector neworigin)`
        .register("touchtriggers", |_ent: Ent, _neworigin: Vec3Arg| -> Result<(), InteropError> {
            unimplemented_builtin!("touchtriggers", 279)
        })
        // `string(float modidx, float framenum)`
        .register(
            "frametoname",
            |_modidx: f32, _framenum: f32| -> Result<V<QCString>, InteropError> {
                unimplemented_builtin!("frametoname", 284)
            },
        )
        // `float(string name)`
        .register("checkcommand", |_name: S| -> Result<f32, InteropError> {
            unimplemented_builtin!("checkcommand", 294)
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
        // `string(float mdlindex)`
        .register("modelnameforindex", |_mdlindex: f32| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("modelnameforindex", 334)
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
        // `void(string s, ...)`
        .register(
            "print",
            |_s: S,
             _va0: Any,
             _va1: Any,
             _va2: Any,
             _va3: Any,
             _va4: Any,
             _va5: Any,
             _va6: Any|
             -> Result<(), InteropError> { unimplemented_builtin!("print", 339) },
        )
        // `void(entity ent)`
        .register("runstandardplayerphysics", |_ent: Ent| -> Result<(), InteropError> {
            unimplemented_builtin!("runstandardplayerphysics", 347)
        })
        // `void(string cmdname, optional string desc)`
        .register("registercommand", |_cmdname: S, _desc: S| -> Result<(), InteropError> {
            unimplemented_builtin!("registercommand", 352)
        })
        // `float(entity ent)`
        .register("wasfreed", |_ent: Ent| -> Result<f32, InteropError> {
            unimplemented_builtin!("wasfreed", 353)
        })
        // `string(string key)`
        .register("serverkey", |_key: S| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("serverkey", 354)
        })
        // `entity(entity from, optional entity to)`
        .register("copyentity", |_from: Ent, _to: Ent| -> Result<Ent, InteropError> {
            unimplemented_builtin!("copyentity", 400)
        })
        // `entity(.string field, string match, optional .entity chainfield)`
        .register(
            "findchain",
            |_field: Fld, _match: S, _chainfield: Fld| -> Result<Ent, InteropError> {
                unimplemented_builtin!("findchain", 402)
            },
        )
        // `entity(.float fld, float match, optional .entity chainfield)`
        .register(
            "findchainfloat",
            |_fld: Fld, _match: f32, _chainfield: Fld| -> Result<Ent, InteropError> {
                unimplemented_builtin!("findchainfloat", 403)
            },
        )
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
        // `void(vector dir)`
        .register("vectorvectors", |_dir: Vec3Arg| -> Result<(), InteropError> {
            unimplemented_builtin!("vectorvectors", 432)
        })
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
        // `float(string s)`
        .register("tokenize", |_s: S| -> Result<f32, InteropError> {
            unimplemented_builtin!("tokenize", 441)
        })
        // `string(float n)`
        .register("argv", |_n: f32| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("argv", 442)
        })
        // `void(entity e, entity tagentity, string tagname)`
        .register(
            "setattachment",
            |_e: Ent, _tagentity: Ent, _tagname: S| -> Result<(), InteropError> {
                unimplemented_builtin!("setattachment", 443)
            },
        )
        // `searchhandle(string pattern, float flags, float quiet, optional string filterpackage)`
        .register(
            "search_begin",
            |_pattern: S,
             _flags: f32,
             _quiet: f32,
             _filterpackage: S|
             -> Result<f32, InteropError> {
                unimplemented_builtin!("search_begin", 444)
            },
        )
        // `void(searchhandle handle)`
        .register("search_end", |_handle: f32| -> Result<(), InteropError> {
            unimplemented_builtin!("search_end", 445)
        })
        // `float(searchhandle handle)`
        .register("search_getsize", |_handle: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("search_getsize", 446)
        })
        // `string(searchhandle handle, float num)`
        .register(
            "search_getfilename",
            |_handle: f32, _num: f32| -> Result<V<QCString>, InteropError> {
                unimplemented_builtin!("search_getfilename", 447)
            },
        )
        // `string(string cvarname)`
        .register("cvar_string", |_cvarname: S| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("cvar_string", 448)
        })
        // `entity(entity start, .float fld, float match)`
        .register("findflags", |_start: Ent, _fld: Fld, _match: f32| -> Result<Ent, InteropError> {
            unimplemented_builtin!("findflags", 449)
        })
        // `entity(.float fld, float match, optional .entity chainfield)`
        .register(
            "findchainflags",
            |_fld: Fld, _match: f32, _chainfield: Fld| -> Result<Ent, InteropError> {
                unimplemented_builtin!("findchainflags", 450)
            },
        )
        // `entity(float entnum)`
        .register("edict_num", |_entnum: f32| -> Result<Ent, InteropError> {
            unimplemented_builtin!("edict_num", 459)
        })
        // `strbuf()`
        .register("buf_create", || -> Result<f32, InteropError> {
            unimplemented_builtin!("buf_create", 460)
        })
        // `void(strbuf bufhandle)`
        .register("buf_del", |_bufhandle: f32| -> Result<(), InteropError> {
            unimplemented_builtin!("buf_del", 461)
        })
        // `float(strbuf bufhandle)`
        .register("buf_getsize", |_bufhandle: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("buf_getsize", 462)
        })
        // `void(strbuf bufhandle_from, strbuf bufhandle_to)`
        .register(
            "buf_copy",
            |_bufhandle_from: f32, _bufhandle_to: f32| -> Result<(), InteropError> {
                unimplemented_builtin!("buf_copy", 463)
            },
        )
        // `void(strbuf bufhandle, float sortprefixlen, float backward)`
        .register(
            "buf_sort",
            |_bufhandle: f32, _sortprefixlen: f32, _backward: f32| -> Result<(), InteropError> {
                unimplemented_builtin!("buf_sort", 464)
            },
        )
        // `string(strbuf bufhandle, string glue)`
        .register("buf_implode", |_bufhandle: f32, _glue: S| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("buf_implode", 465)
        })
        // `string(strbuf bufhandle, float string_index)`
        .register(
            "bufstr_get",
            |_bufhandle: f32, _string_index: f32| -> Result<V<QCString>, InteropError> {
                unimplemented_builtin!("bufstr_get", 466)
            },
        )
        // `void(strbuf bufhandle, float string_index, string str)`
        .register(
            "bufstr_set",
            |_bufhandle: f32, _string_index: f32, _str: S| -> Result<(), InteropError> {
                unimplemented_builtin!("bufstr_set", 467)
            },
        )
        // `float(strbuf bufhandle, string str, float ordered)`
        .register(
            "bufstr_add",
            |_bufhandle: f32, _str: S, _ordered: f32| -> Result<f32, InteropError> {
                unimplemented_builtin!("bufstr_add", 468)
            },
        )
        // `void(strbuf bufhandle, float string_index)`
        .register(
            "bufstr_free",
            |_bufhandle: f32, _string_index: f32| -> Result<(), InteropError> {
                unimplemented_builtin!("bufstr_free", 469)
            },
        )
        // `float(float s)`
        .register("asin", |_s: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("asin", 471)
        })
        // `float(float c)`
        .register("acos", |_c: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("acos", 472)
        })
        // `float(float t)`
        .register("atan", |_t: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("atan", 473)
        })
        // `float(float c, float s)`
        .register("atan2", |_c: f32, _s: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("atan2", 474)
        })
        // `float(float a)`
        .register("tan", |_a: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("tan", 475)
        })
        // `float(string s)`
        .register("strlennocol", |_s: S| -> Result<f32, InteropError> {
            unimplemented_builtin!("strlennocol", 476)
        })
        // `string(string s)`
        .register("strdecolorize", |_s: S| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("strdecolorize", 477)
        })
        // `string(float uselocaltime, string format, ...)`
        .register(
            "strftime",
            |_uselocaltime: f32,
             _format: S,
             _va0: Any,
             _va1: Any,
             _va2: Any,
             _va3: Any,
             _va4: Any,
             _va5: Any|
             -> Result<V<QCString>, InteropError> {
                unimplemented_builtin!("strftime", 478)
            },
        )
        // `float(string s, string separator1, ...)`
        .register(
            "tokenizebyseparator",
            |_s: S,
             _separator1: S,
             _va0: Any,
             _va1: Any,
             _va2: Any,
             _va3: Any,
             _va4: Any,
             _va5: Any|
             -> Result<f32, InteropError> {
                unimplemented_builtin!("tokenizebyseparator", 479)
            },
        )
        // `string(string s)`
        .register("strtolower", |_s: S| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("strtolower", 480)
        })
        // `string(string s)`
        .register("strtoupper", |_s: S| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("strtoupper", 481)
        })
        // `string(string s)`
        .register("cvar_defstring", |_s: S| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("cvar_defstring", 482)
        })
        // `void(vector origin, string sample, float volume, float attenuation)`
        .register(
            "pointsound",
            |_origin: Vec3Arg,
             _sample: S,
             _volume: f32,
             _attenuation: f32|
             -> Result<(), InteropError> { unimplemented_builtin!("pointsound", 483) },
        )
        // `string(string search, string replace, string subject)`
        .register(
            "strreplace",
            |_search: S, _replace: S, _subject: S| -> Result<V<QCString>, InteropError> {
                unimplemented_builtin!("strreplace", 484)
            },
        )
        // `string(string search, string replace, string subject)`
        .register(
            "strireplace",
            |_search: S, _replace: S, _subject: S| -> Result<V<QCString>, InteropError> {
                unimplemented_builtin!("strireplace", 485)
            },
        )
        // `vector(entity e, float s, float n, float a)`
        .register(
            "getsurfacepointattribute",
            |_e: Ent, _s: f32, _n: f32, _a: f32| -> Result<[f32; 3], InteropError> {
                unimplemented_builtin!("getsurfacepointattribute", 486)
            },
        )
        // `float(float caseinsensitive, string s, ...)`
        .register(
            "crc16",
            |_caseinsensitive: f32,
             _s: S,
             _va0: Any,
             _va1: Any,
             _va2: Any,
             _va3: Any,
             _va4: Any,
             _va5: Any|
             -> Result<f32, InteropError> { unimplemented_builtin!("crc16", 494) },
        )
        // `float(string name)`
        .register("cvar_type", |_name: S| -> Result<f32, InteropError> {
            unimplemented_builtin!("cvar_type", 495)
        })
        // `float()`
        .register("numentityfields", || -> Result<f32, InteropError> {
            unimplemented_builtin!("numentityfields", 496)
        })
        // `string(float fieldnum)`
        .register("entityfieldname", |_fieldnum: f32| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("entityfieldname", 497)
        })
        // `float(float fieldnum)`
        .register("entityfieldtype", |_fieldnum: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("entityfieldtype", 498)
        })
        // `string(float fieldnum, entity ent)`
        .register(
            "getentityfieldstring",
            |_fieldnum: f32, _ent: Ent| -> Result<V<QCString>, InteropError> {
                unimplemented_builtin!("getentityfieldstring", 499)
            },
        )
        // `float(float fieldnum, entity ent, string s)`
        .register(
            "putentityfieldstring",
            |_fieldnum: f32, _ent: Ent, _s: S| -> Result<f32, InteropError> {
                unimplemented_builtin!("putentityfieldstring", 500)
            },
        )
        // `string(string filename, optional float flags)`
        .register("whichpack", |_filename: S, _flags: f32| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("whichpack", 503)
        })
        // `string(string in)`
        .register("uri_escape", |_in: S| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("uri_escape", 510)
        })
        // `string(string in)`
        .register("uri_unescape", |_in: S| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("uri_unescape", 511)
        })
        // `float(entity ent)`
        .register("num_for_edict", |_ent: Ent| -> Result<f32, InteropError> {
            unimplemented_builtin!("num_for_edict", 512)
        })
        // `float(string uril, float id, optional string postmimetype, optional string postdata)`
        .register(
            "uri_get",
            |_uril: S, _id: f32, _postmimetype: S, _postdata: S| -> Result<f32, InteropError> {
                unimplemented_builtin!("uri_get", 513)
            },
        )
        // `float(string str)`
        .register("tokenize_console", |_str: S| -> Result<f32, InteropError> {
            unimplemented_builtin!("tokenize_console", 514)
        })
        // `float(float idx)`
        .register("argv_start_index", |_idx: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("argv_start_index", 515)
        })
        // `float(float idx)`
        .register("argv_end_index", |_idx: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("argv_end_index", 516)
        })
        // `void(strbuf strbuf, string pattern, string antipattern)`
        .register(
            "buf_cvarlist",
            |_strbuf: f32, _pattern: S, _antipattern: S| -> Result<(), InteropError> {
                unimplemented_builtin!("buf_cvarlist", 517)
            },
        )
        // `string(string cvarname)`
        .register("cvar_description", |_cvarname: S| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("cvar_description", 518)
        })
        // `float(float v, optional float base)`
        .register("log", |_v: f32, _base: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("log", 532)
        })
        // `float(string filename, strbuf bufhandle)`
        .register("buf_loadfile", |_filename: S, _bufhandle: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("buf_loadfile", 535)
        })
        // `float(filestream filehandle, strbuf bufhandle, optional float startpos, optional float numstrings)`
        .register(
            "buf_writefile",
            |_filehandle: f32,
             _bufhandle: f32,
             _startpos: f32,
             _numstrings: f32|
             -> Result<f32, InteropError> {
                unimplemented_builtin!("buf_writefile", 536)
            },
        )
        // `void(...)`
        .register(
            "callfunction",
            |_va0: Any,
             _va1: Any,
             _va2: Any,
             _va3: Any,
             _va4: Any,
             _va5: Any,
             _va6: Any,
             _va7: Any|
             -> Result<(), InteropError> {
                unimplemented_builtin!("callfunction", 605)
            },
        )
        // `void(filestream fh, entity e)`
        .register("writetofile", |_fh: f32, _e: Ent| -> Result<(), InteropError> {
            unimplemented_builtin!("writetofile", 606)
        })
        // `float(string s)`
        .register("isfunction", |_s: S| -> Result<f32, InteropError> {
            unimplemented_builtin!("isfunction", 607)
        })
        // `float(entity e, string s, optional float offset)`
        .register("parseentitydata", |_e: Ent, _s: S, _offset: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("parseentitydata", 613)
        })
        // `string(string fmt, ...)`
        .register(
            "sprintf",
            |_fmt: S,
             _va0: Any,
             _va1: Any,
             _va2: Any,
             _va3: Any,
             _va4: Any,
             _va5: Any,
             _va6: Any|
             -> Result<V<QCString>, InteropError> {
                unimplemented_builtin!("sprintf", 627)
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
        )
        // `string(string digest, string data, ...)`
        .register(
            "digest_hex",
            |_digest: S,
             _data: S,
             _va0: Any,
             _va1: Any,
             _va2: Any,
             _va3: Any,
             _va4: Any,
             _va5: Any|
             -> Result<V<QCString>, InteropError> {
                unimplemented_builtin!("digest_hex", 639)
            },
        )
        // `float(__variant funcref)`
        .register("checkbuiltin", |_funcref: Any| -> Result<f32, InteropError> {
            unimplemented_builtin!("checkbuiltin", 0)
        })
        // `vector(vector v1, vector v2)`
        .register("crossproduct", |_v1: Vec3Arg, _v2: Vec3Arg| -> Result<[f32; 3], InteropError> {
            unimplemented_builtin!("crossproduct", 0)
        })
        // `field_t(float fieldnum)`
        .register("entityfieldref", |_fieldnum: f32| -> Result<Fld, InteropError> {
            unimplemented_builtin!("entityfieldref", 0)
        })
        // `float(string fieldname)`
        .register("findentityfield", |_fieldname: S| -> Result<f32, InteropError> {
            unimplemented_builtin!("findentityfield", 0)
        })
        // `int(filestream fhandle, optional int newoffset)`
        .register("fseek", |_fhandle: f32, _newoffset: Any| -> Result<Any, InteropError> {
            unimplemented_builtin!("fseek", 0)
        })
        // `int(float)`
        .register("ftoi", |_arg0: f32| -> Result<Any, InteropError> {
            unimplemented_builtin!("ftoi", 0)
        })
        // `__uint(float)`
        .register("ftou", |_arg0: f32| -> Result<Any, InteropError> {
            unimplemented_builtin!("ftou", 0)
        })
        // `float(int, optional float shift, float mask)`
        .register("itof", |_arg0: Any, _shift: f32, _mask: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("itof", 0)
        })
        // `string(searchhandle handle, float num)`
        .register(
            "search_getfilemtime",
            |_handle: f32, _num: f32| -> Result<V<QCString>, InteropError> {
                unimplemented_builtin!("search_getfilemtime", 0)
            },
        )
        // `float(searchhandle handle, float num)`
        .register("search_getfilesize", |_handle: f32, _num: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("search_getfilesize", 0)
        })
        // `string(searchhandle handle, float num)`
        .register(
            "search_getpackagename",
            |_handle: f32, _num: f32| -> Result<V<QCString>, InteropError> {
                unimplemented_builtin!("search_getpackagename", 0)
            },
        )
        // `float(string key, optional float assumevalue)`
        .register("serverkeyfloat", |_key: S, _assumevalue: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("serverkeyfloat", 0)
        })
        // `string(string s)`
        .register("strtrim", |_s: S| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("strtrim", 0)
        })
        // `float(__uint, optional float shift, float mask)`
        .register("utof", |_arg0: Any, _shift: f32, _mask: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("utof", 0)
        });
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
        // `float(string name)`
        .register("iscachedpic", |_name: S| -> Result<f32, InteropError> {
            unimplemented_builtin!("iscachedpic", 316)
        })
        // `string(string name, optional float flags)`
        .register("precache_pic", |_name: S, _flags: f32| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("precache_pic", 317)
        })
        // `vector(string picname)`
        .register("drawgetimagesize", |_picname: S| -> Result<[f32; 3], InteropError> {
            unimplemented_builtin!("drawgetimagesize", 318)
        })
        // `float(vector position, float character, vector size, vector rgb, float alpha, optional float drawflag)`
        .register(
            "drawcharacter",
            |_position: Vec3Arg,
             _character: f32,
             _size: Vec3Arg,
             _rgb: Vec3Arg,
             _alpha: f32,
             _drawflag: f32|
             -> Result<f32, InteropError> {
                unimplemented_builtin!("drawcharacter", 320)
            },
        )
        // `float(vector position, string text, vector size, vector rgb, float alpha, optional float drawflag)`
        .register(
            "drawrawstring",
            |_position: Vec3Arg,
             _text: S,
             _size: Vec3Arg,
             _rgb: Vec3Arg,
             _alpha: f32,
             _drawflag: f32|
             -> Result<f32, InteropError> {
                unimplemented_builtin!("drawrawstring", 321)
            },
        )
        // `float(vector position, string pic, vector size, vector rgb, float alpha, optional float drawflag)`
        .register(
            "drawpic",
            |_position: Vec3Arg,
             _pic: S,
             _size: Vec3Arg,
             _rgb: Vec3Arg,
             _alpha: f32,
             _drawflag: f32|
             -> Result<f32, InteropError> { unimplemented_builtin!("drawpic", 322) },
        )
        // `float(vector position, vector size, vector rgb, float alpha, optional float drawflag)`
        .register(
            "drawfill",
            |_position: Vec3Arg,
             _size: Vec3Arg,
             _rgb: Vec3Arg,
             _alpha: f32,
             _drawflag: f32|
             -> Result<f32, InteropError> { unimplemented_builtin!("drawfill", 323) },
        )
        // `void(float x, float y, float width, float height)`
        .register(
            "drawsetcliparea",
            |_x: f32, _y: f32, _width: f32, _height: f32| -> Result<(), InteropError> {
                unimplemented_builtin!("drawsetcliparea", 324)
            },
        )
        // `void()`
        .register("drawresetcliparea", || -> Result<(), InteropError> {
            unimplemented_builtin!("drawresetcliparea", 325)
        })
        // `float(vector position, string text, vector size, vector rgb, float alpha, float drawflag)`
        .register(
            "drawstring",
            |_position: Vec3Arg,
             _text: S,
             _size: Vec3Arg,
             _rgb: Vec3Arg,
             _alpha: f32,
             _drawflag: f32|
             -> Result<f32, InteropError> { unimplemented_builtin!("drawstring", 326) },
        )
        // `float(string text, float usecolours, vector fontsize)`
        .register(
            "stringwidth",
            |_text: S, _usecolours: f32, _fontsize: Vec3Arg| -> Result<f32, InteropError> {
                unimplemented_builtin!("stringwidth", 327)
            },
        )
        // `void(vector pos, vector sz, string pic, vector srcpos, vector srcsz, vector rgb, float alpha, optional float drawflag)`
        .register(
            "drawsubpic",
            |_pos: Vec3Arg,
             _sz: Vec3Arg,
             _pic: S,
             _srcpos: Vec3Arg,
             _srcsz: Vec3Arg,
             _rgb: Vec3Arg,
             _alpha: f32,
             _drawflag: f32|
             -> Result<(), InteropError> { unimplemented_builtin!("drawsubpic", 328) },
        )
        // `int(float stnum)`
        .register("getstati", |_stnum: f32| -> Result<Any, InteropError> {
            unimplemented_builtin!("getstati", 330)
        })
        // `float(float stnum, optional float firstbit, optional float bitcount)`
        .register(
            "getstatf",
            |_stnum: f32, _firstbit: f32, _bitcount: f32| -> Result<f32, InteropError> {
                unimplemented_builtin!("getstatf", 331)
            },
        )
        // `string(float stnum)`
        .register("getstats", |_stnum: f32| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("getstats", 332)
        })
        // `void(entity e, float mdlindex)`
        .register("setmodelindex", |_e: Ent, _mdlindex: f32| -> Result<(), InteropError> {
            unimplemented_builtin!("setmodelindex", 333)
        })
        // `void(string s, ...)`
        .register(
            "cprint",
            |_s: S,
             _va0: Any,
             _va1: Any,
             _va2: Any,
             _va3: Any,
             _va4: Any,
             _va5: Any,
             _va6: Any|
             -> Result<(), InteropError> { unimplemented_builtin!("cprint", 338) },
        )
        // `string(float keynum)`
        .register("keynumtostring", |_keynum: f32| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("keynumtostring", 340)
        })
        // `float(string keyname)`
        .register("stringtokeynum", |_keyname: S| -> Result<f32, InteropError> {
            unimplemented_builtin!("stringtokeynum", 341)
        })
        // `string(float keynum, optional float bindmap, optional float modifier)`
        .register(
            "getkeybind",
            |_keynum: f32, _bindmap: f32, _modifier: f32| -> Result<V<QCString>, InteropError> {
                unimplemented_builtin!("getkeybind", 342)
            },
        )
        // `void(float usecursor, optional string cursorimage, optional vector hotspot, optional float scale)`
        .register(
            "setcursormode",
            |_usecursor: f32,
             _cursorimage: S,
             _hotspot: Vec3Arg,
             _scale: f32|
             -> Result<(), InteropError> {
                unimplemented_builtin!("setcursormode", 343)
            },
        )
        // `float(float inputsequencenum)`
        .register("getinputstate", |_inputsequencenum: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("getinputstate", 345)
        })
        // `void(float sens)`
        .register("setsensitivityscaler", |_sens: f32| -> Result<(), InteropError> {
            unimplemented_builtin!("setsensitivityscaler", 346)
        })
        // `string(float playernum, string keyname)`
        .register(
            "getplayerkeyvalue",
            |_playernum: f32, _keyname: S| -> Result<V<QCString>, InteropError> {
                unimplemented_builtin!("getplayerkeyvalue", 348)
            },
        )
        // `float()`
        .register("isdemo", || -> Result<f32, InteropError> {
            unimplemented_builtin!("isdemo", 349)
        })
        // `float()`
        .register("isserver", || -> Result<f32, InteropError> {
            unimplemented_builtin!("isserver", 350)
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
        // `string(optional string resetstring)`
        .register("getentitytoken", |_resetstring: S| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("getentitytoken", 355)
        })
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
        // `string(string command, optional float bindmap)`
        .register(
            "findkeysforcommand",
            |_command: S, _bindmap: f32| -> Result<V<QCString>, InteropError> {
                unimplemented_builtin!("findkeysforcommand", 521)
            },
        )
        // `float(string sample)`
        .register("soundlength", |_sample: S| -> Result<f32, InteropError> {
            unimplemented_builtin!("soundlength", 534)
        })
        // `vector(float vidmode, optional float forfullscreen)`
        .register(
            "getresolution",
            |_vidmode: f32, _forfullscreen: f32| -> Result<[f32; 3], InteropError> {
                unimplemented_builtin!("getresolution", 608)
            },
        )
        // `float(float type)`
        .register("gethostcachevalue", |_type: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("gethostcachevalue", 611)
        })
        // `string(float fld, float hostnr)`
        .register(
            "gethostcachestring",
            |_fld: f32, _hostnr: f32| -> Result<V<QCString>, InteropError> {
                unimplemented_builtin!("gethostcachestring", 612)
            },
        )
        // `void()`
        .register("resethostcachemasks", || -> Result<(), InteropError> {
            unimplemented_builtin!("resethostcachemasks", 615)
        })
        // `void(float mask, float fld, string str, float op)`
        .register(
            "sethostcachemaskstring",
            |_mask: f32, _fld: f32, _str: S, _op: f32| -> Result<(), InteropError> {
                unimplemented_builtin!("sethostcachemaskstring", 616)
            },
        )
        // `void(float mask, float fld, float num, float op)`
        .register(
            "sethostcachemasknumber",
            |_mask: f32, _fld: f32, _num: f32, _op: f32| -> Result<(), InteropError> {
                unimplemented_builtin!("sethostcachemasknumber", 617)
            },
        )
        // `void()`
        .register("resorthostcache", || -> Result<(), InteropError> {
            unimplemented_builtin!("resorthostcache", 618)
        })
        // `void(float fld, float descending)`
        .register("sethostcachesort", |_fld: f32, _descending: f32| -> Result<(), InteropError> {
            unimplemented_builtin!("sethostcachesort", 619)
        })
        // `void(optional float dopurge)`
        .register("refreshhostcache", |_dopurge: f32| -> Result<(), InteropError> {
            unimplemented_builtin!("refreshhostcache", 620)
        })
        // `float(float fld, float hostnr)`
        .register("gethostcachenumber", |_fld: f32, _hostnr: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("gethostcachenumber", 621)
        })
        // `float(string key)`
        .register("gethostcacheindexforkey", |_key: S| -> Result<f32, InteropError> {
            unimplemented_builtin!("gethostcacheindexforkey", 622)
        })
        // `void(string key)`
        .register("addwantedhostcachekey", |_key: S| -> Result<(), InteropError> {
            unimplemented_builtin!("addwantedhostcachekey", 623)
        })
        // `string(float n, float prop)`
        .register("getgamedirinfo", |_n: f32, _prop: f32| -> Result<V<QCString>, InteropError> {
            unimplemented_builtin!("getgamedirinfo", 626)
        })
        // `float(float key, string bind, optional float bindmap, optional float modifier)`
        .register(
            "setkeybind",
            |_key: f32, _bind: S, _bindmap: f32, _modifier: f32| -> Result<f32, InteropError> {
                unimplemented_builtin!("setkeybind", 630)
            },
        )
        // `vector()`
        .register("getbindmaps", || -> Result<[f32; 3], InteropError> {
            unimplemented_builtin!("getbindmaps", 631)
        })
        // `float(vector bm)`
        .register("setbindmaps", |_bm: Vec3Arg| -> Result<f32, InteropError> {
            unimplemented_builtin!("setbindmaps", 632)
        })
        // `string(string command, optional float bindmap)`
        .register(
            "findkeysforcommandex",
            |_command: S, _bindmap: f32| -> Result<V<QCString>, InteropError> {
                unimplemented_builtin!("findkeysforcommandex", 0)
            },
        )
        // `float(float effective)`
        .register("getcursormode", |_effective: f32| -> Result<f32, InteropError> {
            unimplemented_builtin!("getcursormode", 0)
        })
        // `float(float playernum, string keyname, optional float assumevalue)`
        .register(
            "getplayerkeyfloat",
            |_playernum: f32, _keyname: S, _assumevalue: f32| -> Result<f32, InteropError> {
                unimplemented_builtin!("getplayerkeyfloat", 0)
            },
        )
        // `__double(optional int timetype)`
        .register("gettimed", |_timetype: Any| -> Result<Any, InteropError> {
            unimplemented_builtin!("gettimed", 0)
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
        })
        // `void(string newcaption)`
        .register("setwindowcaption", |_newcaption: S| -> Result<(), InteropError> {
            unimplemented_builtin!("setwindowcaption", 0)
        });
}

/// The names of every stubbed builtin.
pub const STUB_BUILTINS: &[&str] = &[
    "setorigin",
    "setmodel",
    "setsize",
    "sound",
    "traceline",
    "precache_sound",
    "precache_model",
    "walkmove",
    "droptofloor",
    "lightstyle",
    "checkbottom",
    "pointcontents",
    "particle",
    "ChangeYaw",
    "movetogoal",
    "makestatic",
    "ambientsound",
    "precache_model2",
    "precache_sound2",
    "sin",
    "cos",
    "sqrt",
    "tracetoss",
    "etos",
    "stof",
    "tracebox",
    "randomvec",
    "getlight",
    "registercvar",
    "min",
    "max",
    "bound",
    "pow",
    "findfloat",
    "checkextension",
    "anglemod",
    "fopen",
    "fclose",
    "fgets",
    "fputs",
    "strlen",
    "strcat",
    "substring",
    "stov",
    "strzone",
    "strunzone",
    "localsound",
    "bitshift",
    "strstrofs",
    "str2chr",
    "chr2str",
    "strconv",
    "strpad",
    "infoadd",
    "infoget",
    "strncmp",
    "strcasecmp",
    "strncasecmp",
    "checkpvs",
    "mod",
    "stoi",
    "itos",
    "stoh",
    "htos",
    "frameforname",
    "frameduration",
    "touchtriggers",
    "frametoname",
    "checkcommand",
    "R_BeginPolygon",
    "R_PolygonVertex",
    "R_EndPolygon",
    "modelnameforindex",
    "particleeffectnum",
    "trailparticles",
    "pointparticles",
    "print",
    "runstandardplayerphysics",
    "registercommand",
    "wasfreed",
    "serverkey",
    "copyentity",
    "findchain",
    "findchainfloat",
    "te_gunshot",
    "te_spike",
    "te_superspike",
    "te_explosion",
    "te_tarexplosion",
    "te_wizspike",
    "te_knightspike",
    "te_lavasplash",
    "te_teleport",
    "te_explosion2",
    "te_lightning1",
    "te_lightning2",
    "te_lightning3",
    "te_beam",
    "vectorvectors",
    "getsurfacenumpoints",
    "getsurfacepoint",
    "getsurfacenormal",
    "getsurfacetexture",
    "getsurfacenearpoint",
    "getsurfaceclippedpoint",
    "tokenize",
    "argv",
    "setattachment",
    "search_begin",
    "search_end",
    "search_getsize",
    "search_getfilename",
    "cvar_string",
    "findflags",
    "findchainflags",
    "edict_num",
    "buf_create",
    "buf_del",
    "buf_getsize",
    "buf_copy",
    "buf_sort",
    "buf_implode",
    "bufstr_get",
    "bufstr_set",
    "bufstr_add",
    "bufstr_free",
    "asin",
    "acos",
    "atan",
    "atan2",
    "tan",
    "strlennocol",
    "strdecolorize",
    "strftime",
    "tokenizebyseparator",
    "strtolower",
    "strtoupper",
    "cvar_defstring",
    "pointsound",
    "strreplace",
    "strireplace",
    "getsurfacepointattribute",
    "crc16",
    "cvar_type",
    "numentityfields",
    "entityfieldname",
    "entityfieldtype",
    "getentityfieldstring",
    "putentityfieldstring",
    "whichpack",
    "uri_escape",
    "uri_unescape",
    "num_for_edict",
    "uri_get",
    "tokenize_console",
    "argv_start_index",
    "argv_end_index",
    "buf_cvarlist",
    "cvar_description",
    "log",
    "buf_loadfile",
    "buf_writefile",
    "callfunction",
    "writetofile",
    "isfunction",
    "parseentitydata",
    "sprintf",
    "getsurfacenumtriangles",
    "getsurfacetriangle",
    "digest_hex",
    "checkbuiltin",
    "crossproduct",
    "entityfieldref",
    "findentityfield",
    "fseek",
    "ftoi",
    "ftou",
    "itof",
    "search_getfilemtime",
    "search_getfilesize",
    "search_getpackagename",
    "serverkeyfloat",
    "strtrim",
    "utof",
    "clearscene",
    "addentities",
    "addentity",
    "setproperty",
    "renderscene",
    "dynamiclight_add",
    "getproperty",
    "unproject",
    "project",
    "iscachedpic",
    "precache_pic",
    "drawgetimagesize",
    "drawcharacter",
    "drawrawstring",
    "drawpic",
    "drawfill",
    "drawsetcliparea",
    "drawresetcliparea",
    "drawstring",
    "stringwidth",
    "drawsubpic",
    "getstati",
    "getstatf",
    "getstats",
    "setmodelindex",
    "cprint",
    "keynumtostring",
    "stringtokeynum",
    "getkeybind",
    "setcursormode",
    "getinputstate",
    "setsensitivityscaler",
    "getplayerkeyvalue",
    "isdemo",
    "isserver",
    "SetListener",
    "getentitytoken",
    "sendevent",
    "readbyte",
    "readchar",
    "readshort",
    "readlong",
    "readcoord",
    "readangle",
    "readstring",
    "readfloat",
    "readentitynum",
    "getentity",
    "findkeysforcommand",
    "soundlength",
    "getresolution",
    "gethostcachevalue",
    "gethostcachestring",
    "resethostcachemasks",
    "sethostcachemaskstring",
    "sethostcachemasknumber",
    "resorthostcache",
    "sethostcachesort",
    "refreshhostcache",
    "gethostcachenumber",
    "gethostcacheindexforkey",
    "addwantedhostcachekey",
    "getgamedirinfo",
    "setkeybind",
    "getbindmaps",
    "setbindmaps",
    "findkeysforcommandex",
    "getcursormode",
    "getplayerkeyfloat",
    "gettimed",
    "readdouble",
    "readint",
    "readint64",
    "readuint64",
    "setwindowcaption",
];
