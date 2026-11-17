//! Checks that map entities are spawned as Quake's `ED_LoadFromFile` does: the first entity is
//! worldspawn (edict 0), and entities are filtered by `skill` and `deathmatch`.
//!
//! These are facts about Quake's behaviour, rather than goldens, so that a bug can't be blessed
//! into them.
//!
//! Like `tests/parity.rs`, this needs the Quake data, so it is ignored by default:
//!
//! ```sh
//! SEISMON_QUAKE_DIR=/path/to/quake/id1 cargo test --test spawn -- --ignored
//! ```

// Not every helper is used here.
#[allow(dead_code)]
mod common;

use std::io::BufReader;

use bevy::prelude::*;
use bevy_mod_commands::RunCmd;
use common::{MAX_LOAD_FRAMES, build_app, quake_dir};
use seismon::{
    common::net::{ServerCmd, ServerMessage},
    server::parity::{self, Snapshot},
};

/// A loaded level.
struct Loaded {
    snapshot: Snapshot,
    /// The level title from the first `ServerInfo` sent to the client.
    title: Vec<u8>,
}

/// Runs each of `cvars` (e.g. `"skill 0"`), then `map <map>`, and waits until the server sends
/// the client its first message.
fn load(map: &str, cvars: &[&str]) -> Loaded {
    let mut app = build_app(quake_dir());
    for cmd in cvars {
        app.world_mut().write_message(RunCmd::parse(cmd).unwrap().into_owned());
        app.update();
    }
    app.world_mut().write_message(RunCmd::parse(&format!("map {map}")).unwrap().into_owned());

    let mut cursor = app.world().resource::<Messages<ServerMessage>>().get_cursor();
    let mut sent = Vec::new();
    for frame in 0.. {
        assert!(frame < MAX_LOAD_FRAMES, "{map} didn't load");
        app.update();
        let messages = app.world().resource::<Messages<ServerMessage>>();
        sent.extend(cursor.read(messages).flat_map(|msg| msg.packet.to_vec()));
        if !sent.is_empty() {
            break;
        }
    }
    assert_eq!(parity::map_path(app.world()), Some(format!("maps/{map}.bsp").as_str()));

    let mut reader = BufReader::new(sent.as_slice());
    let title = loop {
        match ServerCmd::deserialize(&mut reader) {
            Ok(Some(ServerCmd::ServerInfo { message, .. })) => break message.raw.into_owned(),
            Ok(Some(_)) => {}
            Ok(None) => panic!("the server didn't send ServerInfo"),
            Err(e) => panic!("failed parsing the sign-on data: {e}"),
        }
    };

    Loaded { snapshot: parity::snapshot(app.world()).expect("a level is loaded"), title }
}

fn field<'a>(fields: &'a [(String, String)], name: &str) -> Option<&'a str> {
    fields.iter().find(|(field, _)| field == name).map(|(_, value)| value.as_str())
}

fn classname(fields: &[(String, String)]) -> &str {
    field(fields, "classname").unwrap_or_default()
}

fn count_monsters(snapshot: &Snapshot) -> usize {
    snapshot.edicts.iter().filter(|(_, fields)| classname(fields).starts_with("\"monster_")).count()
}

#[test]
#[ignore = "needs the Quake data, see the module docs"]
fn start_worldspawn_is_edict_0() {
    let Loaded { snapshot, .. } = load("start", &[]);

    let worldspawns = snapshot
        .edicts
        .iter()
        .filter(|(_, fields)| classname(fields) == "\"worldspawn\"")
        .map(|(edict, _)| *edict)
        .collect::<Vec<_>>();
    assert_eq!(worldspawns, [0]);
}

#[test]
#[ignore = "needs the Quake data, see the module docs"]
fn e1m1_level_title() {
    let Loaded { snapshot, title } = load("e1m1", &[]);

    let (edict, world) = &snapshot.edicts[0];
    assert_eq!(*edict, 0);
    assert_eq!(field(world, "message"), Some("\"the Slipgate Complex\""));
    assert_eq!(title, b"the Slipgate Complex");
}

#[test]
#[ignore = "needs the Quake data, see the module docs"]
fn e1m1_skill_filters_monsters() {
    let easy = count_monsters(&load("e1m1", &["skill 0"]).snapshot);
    let hard = count_monsters(&load("e1m1", &["skill 2"]).snapshot);
    assert_ne!(easy, hard, "skill doesn't change e1m1's monsters");
    assert!(hard >= easy, "{hard} monsters on hard, but {easy} on easy");
}

#[test]
#[ignore = "needs the Quake data, see the module docs"]
fn deathmatch_filters_entities() {
    fn not_deathmatch(snapshot: &Snapshot) -> impl Iterator<Item = (u16, &str)> {
        snapshot.edicts.iter().filter_map(|(edict, fields)| {
            let spawnflags: f32 =
                field(fields, "spawnflags").and_then(|f| f.parse().ok()).unwrap_or_default();
            (spawnflags as i32 & 2048 != 0).then(|| (*edict, classname(fields)))
        })
    }

    // Otherwise, this test would pass trivially.
    let single = load("e1m1", &[]).snapshot;
    assert!(not_deathmatch(&single).next().is_some(), "e1m1 has no entities to filter");

    let deathmatch = load("e1m1", &["deathmatch 1"]).snapshot;
    let unfiltered = not_deathmatch(&deathmatch).collect::<Vec<_>>();
    assert!(unfiltered.is_empty(), "spawned in deathmatch: {unfiltered:?}");
}
