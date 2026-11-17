use std::path::PathBuf;

use bevy::prelude::*;
use clap::Parser;

use bevy_mod_commands::{ExecResult, RegisterCmdExt};

use super::*;

pub fn register_commands<A: RegisterCmdExt>(app: &mut A) {
    app.command(cmd_map).command(cmd_changelevel).command(cmd_restart).command(cmd_tickrate);
}

#[derive(Parser)]
#[command(name = "sys_ticrate", about = "Set the frame time for the server")]
struct Tickrate {
    tick_rate: f32,
}

fn cmd_tickrate(
    In(Tickrate { tick_rate }): In<Tickrate>,
    mut time: ResMut<Time<Fixed>>,
) -> ExecResult {
    *time = Time::from_seconds(tick_rate as _);
    default()
}

#[derive(Parser)]
#[command(name = "map", about = "Load and start a new map")]
struct Map {
    map_name: PathBuf,
}

fn cmd_map(In(Map { map_name }): In<Map>, world: &mut World) -> ExecResult {
    // A fresh session: clients reconnect with new spawn parms, and `serverflags` is reset.
    start_level(world, map_path(map_name), None);

    default()
}

#[derive(Parser)]
#[command(
    name = "changelevel",
    about = "Move to a new level, keeping the players' items and stats"
)]
struct Changelevel {
    map_name: PathBuf,
}

/// Equivalent to `Host_Changelevel_f`.
fn cmd_changelevel(In(Changelevel { map_name }): In<Changelevel>, world: &mut World) -> ExecResult {
    if !world.contains_resource::<Session>() || !world.contains_resource::<LevelState>() {
        return "Only the server may changelevel".into();
    }

    if let Err(e) = ServerCtx::new(world).save_spawn_parms() {
        error!("changelevel: failed saving spawn parms: {e}");
    }

    let session = world.remove_resource::<Session>().expect("Checked above");
    start_level(world, map_path(map_name), Some(session.persist));

    default()
}

#[derive(Parser)]
#[command(name = "restart", about = "Restart the current level")]
struct Restart;

/// Equivalent to `Host_Restart_f`: clients keep the spawn parms they had at the start of the
/// level, as `SetChangeParms` isn't run.
fn cmd_restart(In(Restart): In<Restart>, world: &mut World) -> ExecResult {
    // Like Quake, do nothing if there's no active level.
    let Some(map_path) = world.get_resource::<LevelState>().map(|level| level.map_path.clone())
    else {
        return default();
    };
    let Some(session) = world.remove_resource::<Session>() else {
        return default();
    };

    start_level(world, map_path, Some(session.persist));

    default()
}

/// Converts a map name given to a command (e.g. `e1m1`) into its asset path (`maps/e1m1.bsp`).
fn map_path(mut map_name: PathBuf) -> String {
    if map_name.extension().is_none() {
        map_name.set_extension("bsp");
    }

    // TODO: Asset paths always use `/`, should we handle this more robustly?
    format!("{}", PathBuf::from("maps").join(map_name).display())
}

/// Closes the current session and starts loading the level at `map_path` (e.g. `maps/e1m1.bsp`).
///
/// `persist` is the state to carry over from the previous level, or `None` for a fresh session.
fn start_level(world: &mut World, map_path: String, persist: Option<SessionPersistent>) {
    // Immediately close the session for the previous level, to prevent clients from
    // receiving messages while the level load is in progress.
    world.remove_resource::<Session>();

    // See the `advance_pending_level` system.
    let asset_server = world.resource::<AssetServer>();
    let pending = PendingLevel {
        bsp: asset_server.load(&map_path),
        progs: asset_server.load("progs.dat"),
        map_name: map_path,
        worldspawn: None,
        persist,
    };
    world.insert_resource(pending);

    world.write_message(LocalLevelStarted);
}
