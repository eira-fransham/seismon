//! Runs small `csprogs.dat`s, compiled from QuakeC in these tests with fteqcc, to check that CSQC
//! builtins behave like they do in FTEQW.
//!
//! The QuakeC is compiled against Quake's system defs (`tests/csqc/sys.qc`, which gives simple
//! CSQC's CRC) and the core CSQC builtins (`tests/csqc/builtins.qc`). Each test stores its results
//! in globals, which are then checked from Rust.
//!
//! fteqcc is found from `$FTEQCC`, `~/.ericw-tools/fteqcc` or `$PATH`. If it isn't found, the tests
//! are skipped (they pass without doing anything).

// Not every helper is used here.
#[allow(dead_code)]
mod common;

use std::{
    path::{Path, PathBuf},
    process::Command as Process,
};

use bevy::prelude::*;
use bevy_mod_scripting::{
    asset::ScriptAsset,
    core::{commands::AttachScript, script::ScriptContexts},
    script::ScriptAttachment,
};
use bevy_mod_scripting_qcvm::{QCEntity, QCScriptingPlugin, QCVmEntities, QCWorldspawn};
use common::{MAX_LOAD_FRAMES, build_app};
use seismon::{
    client::csqc::{
        self, Csqc, CsqcMode, call_entry,
        draw::{CsqcDrawList, CsqcScene},
        hooks::CsqcCommands,
    },
    common::qc::{QcStorage, QcValue},
};

const SYS_QC: &str = include_str!("csqc/sys.qc");
const BUILTINS_QC: &str = include_str!("csqc/builtins.qc");

/// The system functions that Quake's defs declare, which every `progs.dat` must define.
const SYS_FUNCTIONS: &str = "
void() main = {}; void() StartFrame = {}; void() PlayerPreThink = {};
void() PlayerPostThink = {}; void() ClientKill = {}; void() ClientConnect = {};
void() PutClientInServer = {}; void() ClientDisconnect = {}; void() SetNewParms = {};
void() SetChangeParms = {};
void(vector size, float scoresshown) CSQC_DrawHud = {};
";

fn fteqcc() -> Option<PathBuf> {
    if let Some(path) = std::env::var_os("FTEQCC") {
        return Some(path.into());
    }
    if let Some(home) = std::env::var_os("HOME") {
        let path = Path::new(&home).join(".ericw-tools/fteqcc");
        if path.is_file() {
            return Some(path);
        }
    }
    std::env::var_os("PATH")?
        .to_str()?
        .split(':')
        .map(|dir| Path::new(dir).join("fteqcc"))
        .find(|path| path.is_file())
}

/// A CSQC VM running a `csprogs.dat` compiled from test QuakeC.
struct TestCsqc {
    app: App,
    worldspawn: Entity,
    game_dir: PathBuf,
}

impl Drop for TestCsqc {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(self.game_dir.parent().unwrap());
    }
}

impl TestCsqc {
    /// Compiles `source` and loads it as CSQC, or returns `None` if there's no fteqcc. `files`
    /// are written to the game directory first.
    fn new(name: &str, source: &str, files: &[(&str, &[u8])]) -> Option<Self> {
        let Some(fteqcc) = fteqcc() else {
            eprintln!("Skipping: fteqcc wasn't found");
            return None;
        };

        let root = std::env::temp_dir().join(format!("seismon-csqc-{name}-{}", std::process::id()));
        let (src, game_dir) = (root.join("src"), root.join("id1"));
        std::fs::create_dir_all(&src).unwrap();
        std::fs::create_dir_all(&game_dir).unwrap();
        for (path, contents) in files {
            let path = game_dir.join(path);
            std::fs::create_dir_all(path.parent().unwrap()).unwrap();
            std::fs::write(path, contents).unwrap();
        }

        std::fs::write(src.join("sys.qc"), SYS_QC).unwrap();
        std::fs::write(src.join("builtins.qc"), BUILTINS_QC).unwrap();
        std::fs::write(src.join("test.qc"), format!("{SYS_FUNCTIONS}\n{source}")).unwrap();
        std::fs::write(src.join("progs.src"), "../id1/csprogs.dat\nsys.qc\nbuiltins.qc\ntest.qc\n")
            .unwrap();
        let output = Process::new(fteqcc).arg("-srcfile").arg("progs.src").current_dir(&src).output().unwrap();
        assert!(
            game_dir.join("csprogs.dat").is_file(),
            "fteqcc failed:\n{}",
            String::from_utf8_lossy(&output.stdout)
        );

        let mut app = build_app(game_dir.clone());
        csqc::register_builtins(app.world_mut());
        app.init_resource::<CsqcDrawList>()
            .init_resource::<CsqcScene>()
            .init_resource::<CsqcCommands>();

        let handle: Handle<ScriptAsset> = app.world().resource::<AssetServer>().load("csprogs.dat");
        let worldspawn = app.world_mut().spawn((QCWorldspawn, QCEntity, csqc::vm_config())).id();
        let attachment = ScriptAttachment::EntityScript(worldspawn, handle);
        AttachScript::<QCScriptingPlugin>::new(attachment.clone()).apply(app.world_mut());

        let vm = (0..)
            .find_map(|frame| {
                assert!(frame < MAX_LOAD_FRAMES, "csprogs.dat didn't load");
                app.update();
                app.world()
                    .resource::<ScriptContexts<QCScriptingPlugin>>()
                    .read()
                    .get_context(&attachment)
                    .and_then(|context| context.as_loaded().map(|vm| vm.lock().clone()))
            })
            .unwrap();

        csqc::storage::init_storage(app.world_mut(), &vm);
        app.world_mut().insert_resource(Csqc { vm, worldspawn, attachment, mode: CsqcMode::Simple });

        Some(Self { app, worldspawn, game_dir })
    }

    /// Calls the QuakeC function `name`, which must succeed.
    fn call(&mut self, name: &str) {
        call_entry::<()>(self.app.world_mut(), name, vec![]);
        assert!(self.app.world().contains_resource::<Csqc>(), "{name} failed, so CSQC shut down");
    }

    fn global(&self, name: &str) -> QcValue {
        let storage = self.app.world().get::<QcStorage>(self.worldspawn).unwrap().0.clone();
        storage.get_global(self.app.world(), name).unwrap_or_else(|e| panic!("{name}: {e}"))
    }

    fn float(&self, name: &str) -> f32 {
        match self.global(name) {
            QcValue::Float(f) => f,
            other => panic!("{name} is {other:?}"),
        }
    }

    fn vector(&self, name: &str) -> Vec3 {
        match self.global(name) {
            QcValue::Vector(v) => v,
            other => panic!("{name} is {other:?}"),
        }
    }

    fn string(&self, name: &str) -> String {
        match self.global(name) {
            QcValue::String(s) => s.to_string_lossy().into_owned(),
            other => panic!("{name} is {other:?}"),
        }
    }

    fn entities(&self) -> Vec<Entity> {
        self.app
            .world()
            .get::<QCVmEntities>(self.worldspawn)
            .map(|e| e.iter().collect())
            .unwrap_or_default()
    }
}

#[test]
fn maths() {
    let Some(mut csqc) = TestCsqc::new(
        "maths",
        "
        float t_min, t_max, t_min3, t_bound, t_mod, t_shift, t_anglemod, t_pow, t_log, t_log2;
        vector t_cross, t_rand;
        void() test = {
            t_min = min(3, 1);
            t_max = max(3, 7, 5);
            t_min3 = min(4, 2, 8);
            t_bound = bound(0, 15, 10);
            t_mod = mod(7, 3);
            t_shift = bitshift(1, 4);
            t_anglemod = anglemod(-90);
            t_pow = pow(2, 10);
            t_log = log(1);
            t_log2 = log(8, 2);
            t_cross = crossproduct('1 0 0', '0 1 0');
            t_rand = randomvec();
            vectorvectors('0 0 1');
        };
        ",
        &[],
    ) else {
        return;
    };

    csqc.call("test");
    assert_eq!(csqc.float("t_min"), 1.);
    assert_eq!(csqc.float("t_max"), 7.);
    assert_eq!(csqc.float("t_min3"), 2.);
    assert_eq!(csqc.float("t_bound"), 10.);
    assert_eq!(csqc.float("t_mod"), 1.);
    assert_eq!(csqc.float("t_shift"), 16.);
    assert_eq!(csqc.float("t_anglemod"), 270.);
    assert_eq!(csqc.float("t_pow"), 1024.);
    assert_eq!(csqc.float("t_log"), 0.);
    assert!((csqc.float("t_log2") - 3.).abs() < 1e-5);
    assert_eq!(csqc.vector("t_cross"), Vec3::Z);
    assert!(csqc.vector("t_rand").length() < 1.);
    assert_eq!(csqc.vector("v_forward"), Vec3::Z);
    assert_eq!(csqc.vector("v_right"), Vec3::NEG_Y);
}

#[test]
fn strings() {
    let Some(mut csqc) = TestCsqc::new(
        "strings",
        "
        string t_cat, t_sub, t_fmt, t_upper, t_replace, t_pad, t_trim, t_info, t_decol, t_chr;
        string t_etos, t_itos, t_htos, t_digest;
        float t_len, t_ofs, t_cmp, t_casecmp, t_ncmp, t_chr2, t_stof, t_nocol, t_crc;
        vector t_stov;
        void() test = {
            t_cat = strcat(\"a\", \"b\", \"c\");
            t_len = strlen(t_cat);
            t_sub = substring(\"hello\", 1, 3);
            t_ofs = strstrofs(\"abcabc\", \"c\", 3);
            t_fmt = sprintf(\"%d|%5.2f|%s|%v|%x\", 42, 1.23456, \"str\", '1 2 3', 255);
            t_upper = strtoupper(\"Hello\");
            t_replace = strreplace(\"a\", \"o\", \"banana\");
            t_pad = strpad(-5, \"ab\");
            t_trim = strtrim(\"  x  \");
            t_info = infoget(infoadd(\"\\\\a\\\\1\", \"b\", \"2\"), \"b\");
            t_decol = strdecolorize(\"^1red^7\");
            t_nocol = strlennocol(\"^1red^7\");
            t_cmp = strncmp(\"a\", \"b\");
            t_casecmp = strcasecmp(\"ABC\", \"abc\");
            t_ncmp = strncmp(\"abcd\", \"abxx\", 2);
            t_chr = chr2str(104, 105);
            t_chr2 = str2chr(\"hi\", 1);
            t_stof = stof(\"12.5x\");
            t_stov = stov(\"'1 2 3'\");
            t_etos = etos(world);
            t_itos = itos(42);
            t_htos = htos(255);
            t_crc = crc16(0, \"123456789\");
            t_digest = digest_hex(\"SHA1\", \"abc\");
        };
        ",
        &[],
    ) else {
        return;
    };

    csqc.call("test");
    assert_eq!(csqc.string("t_cat"), "abc");
    assert_eq!(csqc.float("t_len"), 3.);
    assert_eq!(csqc.string("t_sub"), "ell");
    assert_eq!(csqc.float("t_ofs"), 5.);
    assert_eq!(csqc.string("t_fmt"), "42| 1.23|str|1 2 3|ff");
    assert_eq!(csqc.string("t_upper"), "HELLO");
    assert_eq!(csqc.string("t_replace"), "bonono");
    assert_eq!(csqc.string("t_pad"), "   ab");
    assert_eq!(csqc.string("t_trim"), "x");
    assert_eq!(csqc.string("t_info"), "2");
    assert_eq!(csqc.string("t_decol"), "red");
    assert_eq!(csqc.float("t_nocol"), 3.);
    assert_eq!(csqc.float("t_cmp"), -1.);
    assert_eq!(csqc.float("t_casecmp"), 0.);
    assert_eq!(csqc.float("t_ncmp"), 0.);
    assert_eq!(csqc.string("t_chr"), "hi");
    assert_eq!(csqc.float("t_chr2"), f32::from(b'i'));
    assert_eq!(csqc.float("t_stof"), 12.5);
    assert_eq!(csqc.vector("t_stov"), Vec3::new(1., 2., 3.));
    assert_eq!(csqc.string("t_etos"), "entity 0");
    assert_eq!(csqc.string("t_itos"), "42");
    assert_eq!(csqc.string("t_htos"), "000000ff");
    assert_eq!(csqc.float("t_crc"), f32::from(0x29b1u16));
    assert_eq!(csqc.string("t_digest"), "a9993e364706816aba3e25717850c26c9cd0d89d");
}

#[test]
fn buffers_and_tokens() {
    let Some(mut csqc) = TestCsqc::new(
        "buffers",
        "
        float t_buf, t_size, t_count, t_start, t_end, t_sepcount, t_concount;
        string t_implode, t_first, t_arg, t_last, t_sep, t_con;
        void() test = {
            t_buf = buf_create();
            bufstr_add(t_buf, \"b\", 1);
            bufstr_add(t_buf, \"a\", 1);
            bufstr_set(t_buf, 3, \"c\");
            t_size = buf_getsize(t_buf);
            buf_sort(t_buf, 0, 0);
            t_implode = buf_implode(t_buf, \",\");
            t_first = bufstr_get(t_buf, 0);
            buf_del(t_buf);

            t_count = tokenize(\"foo \\\"bar baz\\\" {qux}\");
            t_arg = argv(1);
            t_last = argv(-1);
            t_start = argv_start_index(1);
            t_end = argv_end_index(1);
            t_sepcount = tokenizebyseparator(\"a,b;c\", \",\", \";\");
            t_sep = argv(2);
            t_concount = tokenize_console(\"bind {x} y\");
            t_con = argv(1);
        };
        ",
        &[],
    ) else {
        return;
    };

    csqc.call("test");
    assert_eq!(csqc.float("t_buf"), 1.);
    assert_eq!(csqc.float("t_size"), 4.);
    assert_eq!(csqc.string("t_implode"), "a,b,c");
    assert_eq!(csqc.string("t_first"), "a");
    assert_eq!(csqc.float("t_count"), 5.);
    assert_eq!(csqc.string("t_arg"), "bar baz");
    assert_eq!(csqc.string("t_last"), "}");
    assert_eq!((csqc.float("t_start"), csqc.float("t_end")), (4., 13.));
    assert_eq!(csqc.float("t_sepcount"), 3.);
    assert_eq!(csqc.string("t_sep"), "c");
    assert_eq!(csqc.float("t_concount"), 3.);
    assert_eq!(csqc.string("t_con"), "{x}");
}

#[test]
fn cvars() {
    let Some(mut csqc) = TestCsqc::new(
        "cvars",
        "
        float t_registered, t_again, t_type, t_missing, t_check;
        string t_value, t_default;
        void() test = {
            t_registered = registercvar(\"cl_test\", \"hello world\", 0);
            t_again = registercvar(\"cl_test\", \"x\", 0);
            cvar_set(\"cl_test\", \"changed\");
            t_value = cvar_string(\"cl_test\");
            t_default = cvar_defstring(\"cl_test\");
            t_type = cvar_type(\"cl_test\");
            t_missing = cvar_type(\"cl_doesnt_exist\");
            t_check = checkcommand(\"cl_test\");
        };
        ",
        &[],
    ) else {
        return;
    };

    csqc.call("test");
    assert_eq!(csqc.float("t_registered"), 1.);
    assert_eq!(csqc.float("t_again"), 0.);
    assert_eq!(csqc.string("t_value"), "changed");
    assert_eq!(csqc.string("t_default"), "hello world");
    // Exists, with a description, but not created by the engine.
    assert_eq!(csqc.float("t_type"), 17.);
    assert_eq!(csqc.float("t_missing"), 0.);
    assert_eq!(csqc.float("t_check"), 3.);
}

#[test]
fn entities() {
    let Some(mut csqc) = TestCsqc::new(
        "entities",
        "
        .float flagsfield;
        entity t_a, t_b, t_c, t_chain, t_copy, t_flags, t_num;
        float t_chainlen, t_fieldnum, t_parsed, t_freed, t_numfields;
        string t_fieldstr, t_fieldname;
        void() test = {
            t_a = spawn(); t_a.classname = \"thing\"; t_a.flagsfield = 4;
            t_b = spawn(); t_b.classname = \"other\";
            t_c = spawn(); t_c.classname = \"thing\"; t_c.origin = '1 2 3';

            t_chain = findchain(classname, \"thing\");
            entity e = t_chain;
            while (e) { t_chainlen = t_chainlen + 1; e = e.chain; }

            t_flags = findflags(world, flagsfield, 4);
            t_copy = copyentity(t_c);
            t_num = edict_num(num_for_edict(t_b));

            t_fieldnum = findentityfield(\"origin\");
            t_fieldname = entityfieldname(t_fieldnum);
            t_fieldstr = getentityfieldstring(t_fieldnum, t_c);
            t_numfields = numentityfields();

            t_parsed = parseentitydata(t_b, \"{ \\\"classname\\\" \\\"parsed\\\" \\\"angle\\\" \\\"90\\\" }\");
            remove(t_a);
            t_freed = wasfreed(t_a);
        };
        ",
        &[],
    ) else {
        return;
    };

    csqc.call("test");
    assert_eq!(csqc.entities().len(), 3);
    assert_eq!(csqc.float("t_chainlen"), 2.);
    assert_eq!(csqc.global("t_flags"), csqc.global("t_a"));
    assert_ne!(csqc.global("t_copy"), csqc.global("t_c"));
    assert_eq!(csqc.global("t_num"), csqc.global("t_b"));
    assert_eq!(csqc.string("t_fieldname"), "origin");
    assert_eq!(csqc.string("t_fieldstr"), "1 2 3");
    assert!(csqc.float("t_numfields") > 0.);
    assert!(csqc.float("t_parsed") > 0.);
    assert_eq!(csqc.float("t_freed"), 1.);

    let storage = csqc.app.world().get::<QcStorage>(csqc.worldspawn).unwrap().0.clone();
    let QcValue::Entity(b) = csqc.global("t_b") else { panic!() };
    let QcValue::Entity(copy) = csqc.global("t_copy") else { panic!() };
    let world = csqc.app.world();
    assert_eq!(storage.get_field(world, b, "classname").unwrap(), QcValue::String("parsed".into()));
    assert_eq!(storage.get_field(world, b, "angles").unwrap(), QcValue::Vector(Vec3::new(0., 90., 0.)));
    assert_eq!(storage.get_field(world, copy, "origin").unwrap(), QcValue::Vector(Vec3::new(1., 2., 3.)));
}

#[test]
fn functions_and_extensions() {
    let Some(mut csqc) = TestCsqc::new(
        "functions",
        "
        float t_called, t_isfunc, t_notfunc, t_ext, t_noext, t_builtin;
        void(float x, string s) t_target = { if (s == \"yes\") t_called = x; };
        void() test = {
            callfunction(5, \"yes\", \"t_target\");
            callfunction(\"doesnt_exist\");
            t_isfunc = isfunction(\"t_target\");
            t_notfunc = isfunction(\"doesnt_exist\");
            t_ext = checkextension(\"DP_QC_SPRINTF\");
            t_noext = checkextension(\"DP_NOT_A_THING\");
            t_builtin = checkbuiltin(strlen);
        };
        ",
        &[],
    ) else {
        return;
    };

    csqc.call("test");
    assert_eq!(csqc.float("t_called"), 5.);
    assert_eq!(csqc.float("t_isfunc"), 1.);
    assert_eq!(csqc.float("t_notfunc"), 0.);
    assert_eq!(csqc.float("t_ext"), 1.);
    assert_eq!(csqc.float("t_noext"), 0.);
    assert_eq!(csqc.float("t_builtin"), 1.);
}

#[test]
fn files() {
    let Some(mut csqc) = TestCsqc::new(
        "files",
        "
        float t_fh, t_write, t_search, t_count, t_loaded, t_buf;
        string t_line1, t_line2, t_eof, t_found, t_pack, t_bufline;
        void() test = {
            t_fh = fopen(\"test.txt\", 0);
            t_line1 = fgets(t_fh);
            t_line2 = fgets(t_fh);
            t_eof = fgets(t_fh);
            fclose(t_fh);
            t_write = fopen(\"out.txt\", 2);

            t_search = search_begin(\"maps/*.txt\", 0, 0);
            t_count = search_getsize(t_search);
            t_found = search_getfilename(t_search, 0);
            search_end(t_search);
            t_pack = whichpack(\"maps/a.txt\");

            t_buf = buf_create();
            t_loaded = buf_loadfile(\"test.txt\", t_buf);
            t_bufline = bufstr_get(t_buf, 1);
        };
        ",
        &[("data/test.txt", b"first\r\nsecond\n"), ("maps/a.txt", b""), ("maps/b.dat", b"")],
    ) else {
        return;
    };

    csqc.call("test");
    assert_eq!(csqc.float("t_fh"), 1000.);
    assert_eq!(csqc.string("t_line1"), "first");
    assert_eq!(csqc.string("t_line2"), "second");
    assert!(matches!(csqc.global("t_eof"), QcValue::String(s) if s.is_null()));
    assert_eq!(csqc.float("t_write"), -1.);
    assert_eq!(csqc.float("t_count"), 1.);
    assert_eq!(csqc.string("t_found"), "maps/a.txt");
    // Loose files aren't in a package.
    assert_eq!(csqc.string("t_pack"), "");
    assert_eq!(csqc.float("t_loaded"), 1.);
    assert_eq!(csqc.string("t_bufline"), "second");
}

#[test]
fn client_state() {
    let Some(mut csqc) = TestCsqc::new(
        "client",
        "
        string t_space, t_mouse, t_bind, t_keys, t_keysex, t_unknown, t_constate, t_token1;
        string t_token2, t_token3, t_player;
        float t_spacenum, t_badnum, t_set, t_stat, t_demo, t_hcount, t_hkey, t_hkey2, t_hsort;
        void() test = {
            t_space = keynumtostring(32);
            t_mouse = keynumtostring(512);
            t_unknown = keynumtostring(1);
            t_spacenum = stringtokeynum(\"SPACE\");
            t_badnum = stringtokeynum(\"NOTAKEY\");
            t_set = setkeybind(98, \"+csqc_test\");
            t_bind = getkeybind(98);
            t_keys = findkeysforcommand(\"+csqc_test\");
            t_keysex = findkeysforcommandex(\"+csqc_test\");
            registercommand(\"csqc_cmd\");

            t_stat = getstati(0);
            t_demo = isdemo();
            t_constate = serverkey(\"constate\");
            t_player = getplayerkeyvalue(0, \"name\");

            t_hcount = gethostcachevalue(1);
            t_hkey = gethostcacheindexforkey(\"map\");
            t_hkey2 = gethostcacheindexforkey(\"custom\");
            sethostcachesort(2, 1);
            t_hsort = gethostcachevalue(7);

            getentitytoken(\"{ \\\"classname\\\" \\\"worldspawn\\\" }\");
            t_token1 = getentitytoken();
            t_token2 = getentitytoken();
            t_token3 = getentitytoken();
        };
        ",
        &[],
    ) else {
        return;
    };
    csqc.app.init_resource::<seismon::client::input::game::GameInput>();

    csqc.call("test");
    assert_eq!(csqc.string("t_space"), "SPACE");
    assert_eq!(csqc.string("t_mouse"), "MOUSE1");
    assert_eq!(csqc.string("t_unknown"), "<UNKNOWN KEYNUM>");
    assert_eq!(csqc.float("t_spacenum"), 32.);
    assert_eq!(csqc.float("t_badnum"), -1.);
    assert_eq!(csqc.float("t_set"), 1.);
    assert_eq!(csqc.string("t_bind"), "+csqc_test");
    assert_eq!(csqc.string("t_keys"), " '98' '-1'");
    assert_eq!(csqc.string("t_keysex"), "b");
    assert!(csqc.app.world().resource::<CsqcCommands>().0.contains("csqc_cmd"));

    assert_eq!(csqc.float("t_stat"), 0.);
    assert_eq!(csqc.float("t_demo"), 0.);
    assert_eq!(csqc.string("t_constate"), "disconnected");
    assert!(matches!(csqc.global("t_player"), QcValue::String(s) if s.is_null()));

    assert_eq!(csqc.float("t_hcount"), 0.);
    assert_eq!(csqc.float("t_hkey"), 1.);
    assert_eq!(csqc.float("t_hkey2"), 279.);
    assert_eq!(csqc.float("t_hsort"), 1.);

    assert_eq!(csqc.string("t_token1"), "{");
    assert_eq!(csqc.string("t_token2"), "classname");
    assert_eq!(csqc.string("t_token3"), "worldspawn");
}

#[test]
fn drawing() {
    // A 4x2 QPic, and a palette for it.
    let mut pic = Vec::new();
    pic.extend_from_slice(&4u32.to_le_bytes());
    pic.extend_from_slice(&2u32.to_le_bytes());
    pic.extend_from_slice(&[1; 8]);
    let palette = [7u8; 768];
    let Some(mut csqc) = TestCsqc::new(
        "drawing",
        "
        vector t_size, t_missing_size;
        string t_precached, t_missing;
        float t_cached, t_width, t_width_raw, t_drew;
        void() test = {
            t_precached = precache_pic(\"gfx/test\");
            t_missing = precache_pic(\"gfx/doesnt_exist\");
            t_cached = iscachedpic(\"gfx/test\");
            t_size = drawgetimagesize(\"gfx/test\");
            t_missing_size = drawgetimagesize(\"gfx/doesnt_exist\");
            t_width = stringwidth(\"^1ab^7c\", 1, '8 8 0');
            t_width_raw = stringwidth(\"^1ab^7c\", 0, '8 8 0');

            t_drew = drawpic('10 20 0', \"gfx/test\", '4 2 0', '1 1 1', 1, 0);
            drawfill('0 0 0', '5 5 0', '1 0 0', 0.5, 0);
            drawsetcliparea(1, 2, 3, 4);
            drawstring('0 0 0', \"a^1b\", '8 8 0', '1 1 1', 1, 0);
            drawresetcliparea();
            drawcharacter('0 0 0', 65, '8 8 0', '1 1 1', 1, 0);
        };
        ",
        &[("gfx/test.lmp", &pic), ("gfx/palette.lmp", &palette)],
    ) else {
        return;
    };
    csqc.app.init_resource::<Assets<Image>>();

    csqc.call("test");
    assert_eq!(csqc.string("t_precached"), "gfx/test");
    assert!(matches!(csqc.global("t_missing"), QcValue::String(s) if s.is_null()));
    assert_eq!(csqc.float("t_cached"), 1.);
    assert_eq!(csqc.vector("t_size"), Vec3::new(4., 2., 0.));
    assert_eq!(csqc.vector("t_missing_size"), Vec3::ZERO);
    assert_eq!(csqc.float("t_width"), 24.);
    assert_eq!(csqc.float("t_width_raw"), 56.);
    assert_eq!(csqc.float("t_drew"), 1.);

    use seismon::client::csqc::draw::DrawCmd;
    let cmds = &csqc.app.world().resource::<CsqcDrawList>().0;
    let kinds = cmds
        .iter()
        .map(|cmd| match cmd {
            DrawCmd::Pic { .. } => "pic",
            DrawCmd::SubPic { .. } => "subpic",
            DrawCmd::Fill { .. } => "fill",
            DrawCmd::String { .. } => "string",
            DrawCmd::ClipArea { .. } => "clip",
            DrawCmd::ResetClip => "reset",
        })
        .collect::<Vec<_>>();
    assert_eq!(kinds, ["pic", "fill", "clip", "string", "string", "reset", "string"]);
    let DrawCmd::Pic { pos, size, .. } = &cmds[0] else { unreachable!() };
    assert_eq!((*pos, *size), (Vec2::new(10., 20.), Vec2::new(4., 2.)));
}
