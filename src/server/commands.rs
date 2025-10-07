use std::path::PathBuf;

use bevy::prelude::*;
use clap::Parser;
use failure::Error;

use crate::common::console::{ExecResult, RegisterCmdExt};

use super::*;

pub fn register_commands<A: RegisterCmdExt>(app: &mut A) {
    // TODO: Implement `changelevel` (move to new level without resetting persistant state
    app.command(cmd_map.map(|res| -> ExecResult {
        if let Err(e) = res {
            format!("{e}").into()
        } else {
            default()
        }
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
    session: Option<ResMut<Session>>,
    vfs: Res<Vfs>,
    mut registry: ResMut<Registry>,
) -> Result<(), Error> {
    if map_name.extension().is_none() {
        map_name.set_extension("bsp");
    }

    let mut path = PathBuf::from("maps");
    path.push(map_name);

    let bsp_name = format!("{}", path.display());
    let bsp = vfs.open(&bsp_name)?;
    let (models, entmap) = crate::common::bsp::load(bsp)?;

    let models = models.into_iter().map(|m| m.cast()).collect();

    let progs = vfs.open("progs.dat")?;
    let progs = crate::server::progs::load(progs)?;

    // TODO: Make `max_clients` a cvar
    let new_session = Session::new(
        bsp_name,
        8,
        registry.reborrow(),
        &vfs,
        progs,
        models,
        entmap,
    );

    if let Some(mut session) = session {
        *session = new_session;
    } else {
        commands.insert_resource(new_session);
    }

    Ok(())
}
