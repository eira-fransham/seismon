use std::path::PathBuf;

use bevy::prelude::*;
use clap::Parser;
use failure::Error;

use crate::{
    client::{Connection, ConnectionState, input::InputFocus},
    common::{
        console::{ExecResult, RegisterCmdExt},
        net::{ClientMessage, ServerMessage, SignOnStage},
    },
};

use super::*;

pub fn register_commands(app: &mut App) {
    // TODO: Implement `changelevel` (move to new level without resetting persistant state
    app.command(cmd_map.map(|res| -> ExecResult {
        if let Err(e) = res {
            format!("{}", e).into()
        } else {
            default()
        }
    }));
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
    mut focus: ResMut<InputFocus>,
    vfs: Res<Vfs>,
    mut registry: ResMut<Registry>,
    mut client_events: ResMut<Events<ClientMessage>>,
    mut server_events: ResMut<Events<ServerMessage>>,
) -> Result<(), Error> {
    if map_name.extension().is_none() {
        map_name.set_extension("bsp");
    }

    let mut path = PathBuf::from("maps");
    path.push(map_name);

    let bsp_name = format!("{}", path.display());
    let bsp = vfs.open(&bsp_name)?;
    let (models, entmap) = crate::common::bsp::load(bsp)?;
    let progs = vfs.open("progs.dat")?;
    let progs = crate::server::progs::load(progs)?;

    // TODO: Make `max_clients` a cvar
    let new_session = Session::new(
        bsp_name,
        8,
        registry.reborrow(),
        &*vfs,
        progs,
        models,
        entmap,
    );

    if let Some(mut session) = session {
        *session = new_session;
    } else {
        commands.insert_resource(new_session);
    }

    client_events.clear();
    server_events.clear();

    // TODO: This should not be handled here, server and client should be decoupled
    commands.insert_resource(Connection::new_server());
    commands.insert_resource(ConnectionState::SignOn(SignOnStage::Not));
    *focus = InputFocus::Game;

    Ok(())
}
