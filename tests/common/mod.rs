//! Helpers shared by the integration tests that run the server headlessly.

use std::{path::PathBuf, time::Duration};

use bevy::{prelude::*, time::TimeUpdateStrategy};
use bevy_mod_pakfile::PakfilePlugin;
use bevy_trenchbroom::{
    TrenchBroomPlugins,
    config::{TrenchBroomConfig, WriteTrenchBroomConfigOnStartPlugin},
};
use seismon::{
    common::net::{ClientCmd, ClientMessage, MessageKind},
    server::SeismonServerPlugin,
};

/// The server's tick length (`TICK_RATE`).
pub const TICK: Duration = Duration::from_millis(50);
/// Give up if a level hasn't loaded after this many frames.
pub const MAX_LOAD_FRAMES: usize = 10_000;

pub fn build_app(quake_dir: PathBuf) -> App {
    let mut app = App::new();
    app.add_plugins(PakfilePlugin::from_paths([quake_dir]))
        .add_plugins((
            MinimalPlugins,
            bevy::log::LogPlugin {
                level: bevy::log::Level::WARN,
                filter: std::env::var("RUST_LOG").unwrap_or_else(|_| "warn".into()),
                ..default()
            },
            AssetPlugin::default(),
            bevy::transform::TransformPlugin,
            bevy::image::ImagePlugin::default(),
            bevy::mesh::MeshPlugin,
            bevy::world_serialization::WorldSerializationPlugin,
        ))
        // The BSP loader creates materials, but nothing renders them.
        .init_asset::<StandardMaterial>()
        .init_asset::<bevy_trenchbroom::bsp::lighting::AnimatedLighting>()
        .add_plugins(
            TrenchBroomPlugins(TrenchBroomConfig { no_bsp_lighting: true, ..default() })
                .build()
                .disable::<WriteTrenchBroomConfigOnStartPlugin>(),
        )
        .add_plugins(SeismonServerPlugin)
        // Every `update` advances exactly one server tick.
        .insert_resource(TimeUpdateStrategy::ManualDuration(TICK));
    app.finish();
    app.cleanup();
    app
}

pub fn send(app: &mut App, cmd: ClientCmd) {
    let mut packet = Vec::new();
    cmd.serialize(&mut packet).unwrap();
    app.world_mut().write_message(ClientMessage {
        client_id: 0,
        packet: packet.into(),
        kind: MessageKind::Reliable,
    });
}

pub fn string_cmd(app: &mut App, cmd: &str) {
    send(app, ClientCmd::StringCmd { cmd: cmd.to_owned().into() });
}

/// The `id1` directory given by `SEISMON_QUAKE_DIR`.
pub fn quake_dir() -> PathBuf {
    std::env::var_os("SEISMON_QUAKE_DIR").map(PathBuf::from).unwrap_or_else(|| {
        panic!("Set `SEISMON_QUAKE_DIR` to the path of the `id1` directory to run this test")
    })
}
