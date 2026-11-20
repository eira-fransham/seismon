//! Client-side QuakeC runs in its own VM, separate from the server's even when both are in the
//! same world (as on a listen server) and use the same defs.
//!
//! There's no `csprogs.dat` in the Quake data, but simple CSQC uses the same system defs (and CRC)
//! as `progs.dat`, so this loads id1's `progs.dat` as CSQC too and calls its functions as if they
//! were CSQC entry points. Like `tests/parity.rs`, this needs the Quake data, so it is ignored by
//! default:
//!
//! ```sh
//! SEISMON_QUAKE_DIR=/path/to/quake/id1 cargo test --test csqc -- --ignored
//! ```

// Not every helper is used here.
#[allow(dead_code)]
mod common;

use bevy::prelude::*;
use bevy_mod_commands::RunCmd;
use bevy_mod_scripting::{
    asset::ScriptAsset,
    core::{commands::AttachScript, script::ScriptContexts},
    script::ScriptAttachment,
};
use bevy_mod_scripting_qcvm::{QCEntity, QCOwnedBy, QCScriptingPlugin, QCVmEntities, QCWorldspawn};
use common::{MAX_LOAD_FRAMES, build_app, quake_dir};
use seismon::{
    client::csqc::{
        self, Csqc, CsqcMode, call_entry,
        draw::{CsqcDrawList, CsqcScene},
        hooks::CsqcCommands,
    },
    common::qc::{QcStorage, QcValue},
    server::qc::{LevelVm, edict::EdictTable},
};

/// Loads `map` on the server.
fn load_map(app: &mut App, map: &str) {
    let old = app.world().get_resource::<LevelVm>().map(|vm| vm.0.worldspawn);
    app.world_mut().write_message(RunCmd::parse(&format!("map {map}")).unwrap().into_owned());
    for frame in 0.. {
        assert!(frame < MAX_LOAD_FRAMES, "{map} didn't load");
        app.update();
        if let Some(vm) = app.world().get_resource::<LevelVm>()
            && Some(vm.0.worldspawn) != old
        {
            break;
        }
    }
}

/// Loads id1's `progs.dat` as CSQC, as `advance_csqc_load` would (other than checking which entry
/// points it defines).
fn load_csqc(app: &mut App) -> Entity {
    let handle: Handle<ScriptAsset> = app.world().resource::<AssetServer>().load("progs.dat");
    for frame in 0.. {
        assert!(frame < MAX_LOAD_FRAMES, "progs.dat didn't load");
        if app.world().resource::<Assets<ScriptAsset>>().contains(&handle) {
            break;
        }
        app.update();
    }

    let worldspawn = app.world_mut().spawn((QCWorldspawn, QCEntity, csqc::vm_config())).id();
    let attachment = ScriptAttachment::EntityScript(worldspawn, handle);
    AttachScript::<QCScriptingPlugin>::new(attachment.clone()).apply(app.world_mut());

    let vm = (0..)
        .find_map(|frame| {
            assert!(frame < MAX_LOAD_FRAMES, "the CSQC VM didn't load");
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
    worldspawn
}

fn server_entities(world: &World) -> Vec<Entity> {
    world.resource::<EdictTable>().iter().map(|(_, entity)| entity).collect()
}

fn csqc_entities(world: &World, worldspawn: Entity) -> Vec<Entity> {
    world.get::<QCVmEntities>(worldspawn).map(|e| e.iter().collect()).unwrap_or_default()
}

#[test]
#[ignore = "needs the Quake data"]
fn csqc_and_server_vms_are_separate() {
    let mut app = build_app(quake_dir());
    csqc::register_builtins(app.world_mut());
    app.init_resource::<CsqcDrawList>()
        .init_resource::<CsqcScene>()
        .init_resource::<CsqcCommands>();

    load_map(&mut app, "start");
    let csqc_world = load_csqc(&mut app);
    let server_before = server_entities(app.world());

    // `InitBodyQue` spawns 4 entities with the core `spawn` builtin and sets their fields, which
    // must all go to CSQC's VM rather than the server's.
    call_entry::<()>(app.world_mut(), "InitBodyQue", vec![]).expect("InitBodyQue failed");
    assert!(app.world().contains_resource::<Csqc>());

    let spawned = csqc_entities(app.world(), csqc_world);
    assert_eq!(spawned.len(), 4);
    assert_eq!(server_entities(app.world()), server_before);
    let storage = app.world().get::<QcStorage>(csqc_world).unwrap().0.clone();
    for &entity in &spawned {
        assert_eq!(app.world().get::<QCOwnedBy>(entity), Some(&QCOwnedBy(csqc_world)));
        assert!(app.world().get::<seismon::server::qc::edict::Edict>(entity).is_none());
        assert_eq!(
            storage.get_field(app.world(), entity, "classname").unwrap(),
            QcValue::String("bodyque".into())
        );
    }

    // Changing level on the server leaves CSQC alone.
    load_map(&mut app, "start");
    assert!(app.world().get_entity(csqc_world).is_ok());
    assert_eq!(csqc_entities(app.world(), csqc_world), spawned);
    let server_after = server_entities(app.world());
    assert!(!server_after.is_empty());

    // `W_Precache` calls `precache_sound`, which isn't implemented for CSQC yet. That shuts CSQC
    // down, which leaves the server alone.
    assert!(call_entry::<()>(app.world_mut(), "W_Precache", vec![]).is_none());
    assert!(!app.world().contains_resource::<Csqc>());
    assert!(app.world().get_entity(csqc_world).is_err());
    for entity in spawned {
        assert!(app.world().get_entity(entity).is_err());
    }
    assert_eq!(server_entities(app.world()), server_after);
    for entity in server_after {
        assert!(app.world().get_entity(entity).is_ok());
    }

    // The server keeps running.
    for _ in 0..10 {
        app.update();
    }
}
