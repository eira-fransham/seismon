use std::path::PathBuf;

use bevy::prelude::*;
use clap::Parser;

use crate::common::console::{ExecResult, RegisterCmdExt};

use super::*;

pub fn register_commands<A: RegisterCmdExt>(app: &mut A) {
    // TODO: Implement `changelevel` (move to new level without resetting persistant state
    app.command(cmd_map.map(|res| -> ExecResult {
        if let Err(e) = res { format!("{e}").into() } else { default() }
    }))
    .command(cmd_tickrate);
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

fn cmd_map(
    In(Map { mut map_name }): In<Map>,
    mut commands: Commands,
    asset_server: Res<AssetServer>,
) -> anyhow::Result<()> {
    if map_name.extension().is_none() {
        map_name.set_extension("bsp");
    }

    let path = PathBuf::from("maps").join(map_name);

    // TODO: Asset paths always use `/`, should we handle this more robustly?
    let map_name = format!("{}", path.display());

    // Immediately close the session for the previous level, to prevent clients from
    // receiving messages while the level load is in progress.
    // TODO: Keep `SessionPersistent` when implementing `changelevel`.
    commands.remove_resource::<Session>();

    // See the `advance_pending_level` system.
    commands.insert_resource(PendingLevel {
        bsp: asset_server.load(&map_name),
        progs: asset_server.load("progs.dat"),
        map_name,
    });

    commands.write_message(LocalLevelStarted);

    Ok(())
}
