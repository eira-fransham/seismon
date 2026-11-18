//! Checks that a player who ends up inside a wall or floor is moved back out (`SV_CheckStuck`).
//!
//! Like `tests/parity.rs`, this needs the Quake data, so it is ignored by default:
//!
//! ```sh
//! SEISMON_QUAKE_DIR=/path/to/quake/id1 cargo test --test stuck -- --ignored
//! ```

mod common;

use bevy::prelude::*;
use bevy_mod_commands::RunCmd;
use common::{MAX_LOAD_FRAMES, build_app, quake_dir, string_cmd};
use seismon::{common::net::ServerMessage, server::parity};

/// The player's edict.
const PLAYER: u16 = 1;

/// Runs `map start` and signs on a client.
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

/// Runs frames until the server has sent `ticks` more updates.
fn run_ticks(app: &mut App, ticks: usize) {
    let mut cursor = app.world().resource::<Messages<ServerMessage>>().get_cursor();
    let mut seen = 0;
    for frame in 0.. {
        assert!(frame < MAX_LOAD_FRAMES, "the server stopped sending updates");
        app.update();
        let messages = app.world().resource::<Messages<ServerMessage>>();
        seen += cursor.read(messages).count();
        if seen >= ticks {
            break;
        }
    }
}

#[test]
#[ignore = "needs the Quake data, see the module docs"]
fn player_sunk_into_floor_is_unstuck() {
    let mut app = start();
    // Let the player land, so that `oldorigin` is a good position.
    run_ticks(&mut app, 10);

    let world = app.world_mut();
    let good = parity::edict_origin(world, PLAYER).unwrap();
    assert!(!parity::edict_is_stuck(world, PLAYER).unwrap(), "player spawned stuck at {good}");

    // Deeper than the step-up in `SV_WalkMove` (18 units) can rescue, so only `SV_CheckStuck`
    // (moving back to `oldorigin`) can get the player out.
    let sunk = good - Vec3::Z * 40.;
    parity::move_edict(world, PLAYER, sunk).unwrap();
    assert!(parity::edict_is_stuck(world, PLAYER).unwrap(), "{sunk} isn't inside the floor");

    run_ticks(&mut app, 2);

    let world = app.world_mut();
    let origin = parity::edict_origin(world, PLAYER).unwrap();
    assert!(!parity::edict_is_stuck(world, PLAYER).unwrap(), "player still stuck at {origin}");
    assert!(origin.distance(good) < 1., "player unstuck to {origin} rather than back to {good}");
}
