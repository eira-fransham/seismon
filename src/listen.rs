//! Glue for running a listen server that the local client connects to in-process, e.g. for
//! single-player games.
//!
//! This is the only module that depends on both [`crate::client`] and [`crate::server`]; neither
//! of those depends on the other.

use bevy::prelude::*;

use crate::{
    client::{ConnectionStage, NewConnection, commands::LocalServerActive},
    common::net::{ClientMessage, ServerMessage, SignOnStage},
    server::{LocalLevelStarted, PendingLevel, SeismonServerPlugin, Session},
};

/// Enable running a listen server that the local client can connect to directly,
/// e.g. for single-player games.
#[derive(Default, Copy, Clone)]
pub struct SeismonListenServerPlugin;

impl Plugin for SeismonListenServerPlugin {
    fn build(&self, app: &mut App) {
        /// Connect the local client to the level that the local server just started loading.
        fn connect_to_local_server(
            mut client_messages: ResMut<Messages<ClientMessage>>,
            mut server_messages: ResMut<Messages<ServerMessage>>,
            mut new_connection: MessageWriter<NewConnection>,
        ) {
            // Drop any messages that were sent to or from the previous level.
            client_messages.clear();
            server_messages.clear();

            new_connection
                .write(NewConnection::Server(None, ConnectionStage::SignOn(SignOnStage::Prespawn)));
        }

        fn shutdown_on_remote_connect(
            mut commands: Commands,
            mut new_connections: MessageReader<NewConnection>,
        ) {
            // If we connect to a remote server, i.e. `NewConnection::Server` with a `QSocket`,
            // shut down the listen server session.
            if new_connections.read().any(|conn| matches!(conn, NewConnection::Server(Some(_), _)))
            {
                commands.remove_resource::<Session>();
                commands.remove_resource::<PendingLevel>();
            }
        }

        fn local_server_active(world: &World) -> bool {
            world.contains_resource::<Session>() || world.contains_resource::<PendingLevel>()
        }

        app.add_plugins(SeismonServerPlugin)
            .insert_resource(LocalServerActive(local_server_active))
            .add_systems(
                Last,
                (
                    connect_to_local_server.run_if(on_message::<LocalLevelStarted>),
                    shutdown_on_remote_connect.run_if(on_message::<NewConnection>),
                ),
            );
    }
}
