//! Golden-trace parity harness for the server.
//!
//! This runs a few levels headlessly with a scripted client, and compares a per-tick trace of the
//! server state against goldens in `tests/parity/`. The trace only uses QuakeC-level concepts
//! (field names, edict numbers, function indices) and the raw bytes sent to the client, so it stays
//! valid across changes to how the server stores entities or runs QuakeC.
//!
//! It needs the Quake data, so it is ignored by default:
//!
//! ```sh
//! SEISMON_QUAKE_DIR=/path/to/quake/id1 cargo test --test parity -- --ignored
//! ```
//!
//! Set `SEISMON_PARITY_BLESS=1` to overwrite the goldens. The full traces are always written to
//! `target/parity/<map>.trace`, so a failing run can be diffed against a run of an older commit.

use std::{fmt::Write as _, path::PathBuf};

use bevy::prelude::*;
use bevy_mod_commands::RunCmd;
use seismon::{
    common::net::{ButtonFlags, ClientCmd, ServerMessage},
    server::parity,
};
use seismon_utils::QAngles;

mod common;

use common::{MAX_LOAD_FRAMES, TICK, build_app, send, string_cmd};

/// The scripted input for tick `tick`.
///
/// This walks, strafes, jumps, shoots, uses and turns, but in a cycle that roughly returns to the
/// same spot, so the player doesn't wander off into slime or a skill-select teleporter: those
/// restart or change the level (with `localcmd`), and the harness doesn't sign on again after that.
fn scripted_move(tick: usize) -> ClientCmd {
    const CYCLE: usize = 80;

    let cycle = tick / CYCLE;
    let t = tick % CYCLE;

    // Turn 45 degrees at the end of each cycle.
    let yaw = (cycle as f32 * 45. + if t >= 70 { (t - 70) as f32 * 4.5 } else { 0. }) % 360.;
    let (fwd_move, side_move) = match t {
        0..10 => (200, 0),
        20..30 => (-200, 0),
        40..50 => (0, 200),
        60..70 => (0, -200),
        _ => (0, 0),
    };

    let mut button_flags = ButtonFlags::empty();
    if (30..32).contains(&t) {
        button_flags |= ButtonFlags::JUMP;
    }
    if (50..53).contains(&t) {
        button_flags |= ButtonFlags::ATTACK;
    }
    if t == 15 {
        button_flags |= ButtonFlags::USE;
    }

    ClientCmd::Move {
        delta_time: TICK,
        angles: QAngles { pitch_deg: (t as f32 * 0.2).sin() * 10., yaw_deg: yaw, roll_deg: 0. },
        fwd_move,
        side_move,
        up_move: 0,
        button_flags,
        // Switch between the axe and the shotgun now and then.
        impulse: if t == 55 { (1 + cycle % 2) as u8 } else { 0 },
    }
}

/// 64-bit FNV-1a, so the goldens don't depend on a hashing crate.
fn fnv1a(bytes: &[u8]) -> u64 {
    bytes.iter().fold(0xcbf29ce484222325, |hash, &b| (hash ^ b as u64).wrapping_mul(0x100000001b3))
}

/// Runs `map`, writing the full trace to `trace_path`, and returns the golden.
fn run_map(quake_dir: PathBuf, map: &str, ticks: usize, trace_path: &std::path::Path) -> String {
    let mut app = build_app(quake_dir);
    app.world_mut().write_message(RunCmd::parse(&format!("map {map}")).unwrap().into_owned());

    let mut cursor = app.world().resource::<Messages<ServerMessage>>().get_cursor();
    let mut take_messages = |app: &mut App| -> Vec<Vec<u8>> {
        let messages = app.world().resource::<Messages<ServerMessage>>();
        cursor.read(messages).map(|msg| msg.packet.to_vec()).collect()
    };

    // Wait for the level to load and the server to spawn (it sends the sign-on data).
    let mut signon = Vec::new();
    for frame in 0.. {
        assert!(frame < MAX_LOAD_FRAMES, "{map} didn't load");
        app.update();
        signon = take_messages(&mut app);
        if !signon.is_empty() {
            break;
        }
    }

    let mut golden = String::new();
    // Written as we go, so that it's useful even if the run panics.
    let mut trace = std::io::BufWriter::new(std::fs::File::create(trace_path).unwrap());
    let mut record = |tick: &str, app: &App, messages: &[Vec<u8>]| {
        let snapshot = parity::snapshot(app.world()).map(|snap| snap.to_text()).unwrap_or_default();
        let mut message_text = String::new();
        for msg in messages {
            let _ = writeln!(
                message_text,
                "msg {}",
                msg.iter().map(|b| format!("{b:02x}")).collect::<String>()
            );
        }
        let _ = writeln!(
            golden,
            "{tick} {:016x} {:016x}",
            fnv1a(snapshot.as_bytes()),
            fnv1a(message_text.as_bytes())
        );
        use std::io::Write as _;
        write!(trace, "== {tick}\n{snapshot}{message_text}").unwrap();
        trace.flush().unwrap();
    };

    record("signon", &app, &signon);

    for cmd in ["prespawn", "spawn", "begin"] {
        string_cmd(&mut app, cmd);
        app.update();
        let messages = take_messages(&mut app);
        record(cmd, &app, &messages);
    }

    for tick in 0..ticks {
        debug!(target: "parity", "== tick {tick}");
        send(&mut app, scripted_move(tick));
        app.update();
        let messages = take_messages(&mut app);
        record(&tick.to_string(), &app, &messages);
    }

    golden
}

/// Runs `map` for `ticks` ticks after the client spawns, and checks it against the golden.
fn check_map(map: &str, ticks: usize) {
    let quake_dir = common::quake_dir();

    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let trace_dir = manifest.join("target/parity");
    std::fs::create_dir_all(&trace_dir).unwrap();
    let golden = run_map(quake_dir, map, ticks, &trace_dir.join(format!("{map}.trace")));

    let golden_path = manifest.join(format!("tests/parity/{map}.golden"));
    if std::env::var_os("SEISMON_PARITY_BLESS").is_some() {
        std::fs::write(&golden_path, &golden).unwrap();
        return;
    }

    let expected = std::fs::read_to_string(&golden_path)
        .unwrap_or_else(|e| panic!("Couldn't read {}: {e}", golden_path.display()));
    if let Some((expected, found)) =
        expected.lines().zip(golden.lines()).find(|(expected, found)| expected != found)
    {
        panic!(
            "{map} diverged from the golden trace (expected `{expected}`, found `{found}`). \
             The full trace is in {}",
            trace_dir.join(format!("{map}.trace")).display()
        );
    }
    assert_eq!(expected.lines().count(), golden.lines().count(), "{map}: trace length changed");
}

#[test]
#[ignore = "needs the Quake data, see the module docs"]
fn parity_start() {
    check_map("start", 400);
}

#[test]
#[ignore = "needs the Quake data, see the module docs"]
fn parity_e1m1() {
    check_map("e1m1", 400);
}

#[test]
#[ignore = "needs the Quake data, see the module docs"]
fn parity_e1m2() {
    // The player is killed by the monsters at around 231 ticks, and respawning in single player
    // runs `localcmd("restart")`. The level restarts, but the harness doesn't sign on again
    // afterwards, so the server stays paused and the rest of the trace would be meaningless.
    check_map("e1m2", 220);
}
