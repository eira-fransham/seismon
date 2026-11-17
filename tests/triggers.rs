//! Checks that walking into the triggers in `start` runs them.
//!
//! The other slipgates (`e2m1` to `e4m1`, and `end`) need the registered data, so they aren't
//! tested here.
//!
//! Like `tests/parity.rs`, this needs the Quake data, so it is ignored by default:
//!
//! ```sh
//! SEISMON_QUAKE_DIR=/path/to/quake/id1 cargo test --test triggers -- --ignored
//! ```

mod common;

use bevy::prelude::*;
use bevy_mod_commands::RunCmd;
use common::{MAX_LOAD_FRAMES, build_app, quake_dir, string_cmd};
use seismon::{common::net::ServerMessage, server::parity};

/// The player's edict.
const PLAYER: u16 = 1;

/// Runs `map start` and signs on a client, returning the app and a cursor past the sign-on data.
fn start() -> App {
    let mut app = build_app(quake_dir());
    app.world_mut().write_message(RunCmd::parse("map start").unwrap().into_owned());

    let mut cursor = app.world().resource::<Messages<ServerMessage>>().get_cursor();
    for frame in 0.. {
        assert!(frame < MAX_LOAD_FRAMES, "start didn't load");
        app.update();
        let messages = app.world().resource::<Messages<ServerMessage>>();
        if cursor.read(messages).next().is_some() {
            break;
        }
    }

    for cmd in ["prespawn", "spawn", "begin"] {
        string_cmd(&mut app, cmd);
        app.update();
    }
    assert_eq!(parity::map_path(app.world()), Some("maps/start.bsp"));

    app
}

/// Moves the player to the centre of brush model `model`, and returns the bytes sent to the
/// client over the next few ticks.
fn walk_into(app: &mut App, model: &str) -> Vec<u8> {
    let (min, max) = parity::model_bounds(app.world(), model)
        .unwrap_or_else(|| panic!("start has no model {model}"));
    let centre = (min + max) / 2.;

    let mut cursor = app.world().resource::<Messages<ServerMessage>>().get_cursor();
    parity::move_edict(app.world_mut(), PLAYER, centre).unwrap();

    let mut sent = Vec::new();
    for _ in 0..5 {
        app.update();
        let messages = app.world().resource::<Messages<ServerMessage>>();
        sent.extend(cursor.read(messages).flat_map(|msg| msg.packet.to_vec()));
    }
    sent
}

fn changing_to(app: &App) -> Option<&str> {
    parity::pending_map_path(app.world()).or(parity::map_path(app.world()))
}

/// The "Walk into the Slipgate" message trigger, which straddles the root split of the area tree.
#[test]
#[ignore = "needs the Quake data, see the module docs"]
fn start_root_linked_trigger() {
    let mut app = start();
    let sent = walk_into(&mut app, "*21");
    let needle = b"Slipgate";
    assert!(
        sent.windows(needle.len()).any(|window| window == needle),
        "the `*21` trigger_multiple didn't centerprint its message"
    );
}

/// The `e1m1` slipgate. This trigger is in a leaf of the area tree, but it (and every entity after
/// the first `\n` in the entity lump) used to be dropped by the map parser.
#[test]
#[ignore = "needs the Quake data, see the module docs"]
fn start_e1m1_slipgate() {
    let mut app = start();
    walk_into(&mut app, "*14");
    assert_eq!(changing_to(&app), Some("maps/e1m1.bsp"));
}
