// Copyright © 2018 Cormac O'Brien
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

mod music;

use bevy::{app::Plugin, prelude::*};
use bevy_seedling::prelude::*;

pub use music::MusicPlayer;

use std::io;

use snafu::Snafu;

pub const REVERB_AMT: f32 = 0.1;

#[derive(Snafu, Debug)]
pub enum SoundError {
    #[snafu(display("No such music track: {name}"))]
    NoSuchTrack { name: String },
    #[snafu(context(false), display("I/O error: {source}"))]
    Io { source: io::Error },
}

/// Data needed for sound spatialization.
///
/// This struct is updated every frame.
#[derive(Default, Debug, Clone, Resource)]
pub struct Listener {
    pub origin: Vec3,
    pub rotation: Quat,
    pub left_ear: Vec3,
    pub right_ear: Vec3,
}

impl Listener {
    pub fn origin(&self) -> Vec3 {
        self.origin
    }

    pub fn left_ear(&self) -> Vec3 {
        self.left_ear
    }

    pub fn right_ear(&self) -> Vec3 {
        self.right_ear
    }

    pub fn set_origin(&mut self, new_origin: Vec3) {
        self.origin = new_origin;
    }

    pub fn set_left_ear(&mut self, new_origin: Vec3) {
        self.left_ear = new_origin;
    }

    pub fn set_right_ear(&mut self, new_origin: Vec3) {
        self.right_ear = new_origin;
    }
}

pub struct SeismonSoundPlugin;

#[derive(NodeLabel, PartialEq, Eq, Debug, Hash, Clone, Default)]
pub struct ReverbBus;

#[derive(NodeLabel, PartialEq, Eq, Debug, Hash, Clone, Default)]
pub struct MasterOut;

/// Gate for all spatialized output (the main sample pool and its reverb send).
///
/// Silenced whenever there is no active [`SpatialListener3D`], since
/// `bevy_seedling` leaves spatial offsets at zero (full volume, no panning)
/// when there is no listener, e.g. between `ServerInfo` and `SetView` on map load.
#[derive(NodeLabel, PartialEq, Eq, Debug, Hash, Clone, Default)]
pub struct SpatialOut;

#[derive(PoolLabel, PartialEq, Eq, Debug, Hash, Clone, Default)]
pub struct MainPool;

#[derive(PoolLabel, PartialEq, Eq, Debug, Hash, Clone, Default)]
pub struct MusicPool;

/// Pool for non-spatialized sounds that should play even without a listener, e.g. menu sounds
/// (`S_LocalSound`).
#[derive(PoolLabel, PartialEq, Eq, Debug, Hash, Clone, Default)]
pub struct LocalPool;

impl Plugin for SeismonSoundPlugin {
    fn build(&self, app: &mut bevy::prelude::App) {
        let mut commands = app.world_mut().commands();

        commands
            .spawn((VolumeNode { volume: Volume::Decibels(-40.), ..Default::default() }, ReverbBus))
            .chain_node(FreeverbNode::default())
            .connect(SpatialOut);

        commands.spawn((VolumeNode::default(), MasterOut));
        // Starts silent, as there is no listener at startup.
        commands
            .spawn((VolumeNode { volume: Volume::SILENT, ..Default::default() }, SpatialOut))
            .connect(MasterOut);

        commands.spawn((SamplerPool(MusicPool), PoolSize(1..=1)));
        commands.spawn((SamplerPool(LocalPool), PoolSize(4..=16)));
        commands
            .spawn((
                SamplerPool(MainPool),
                PoolSize(256..=256),
                sample_effects![
                    (SpatialBasicNode::default(), SpatialScale::default()),
                    SendNode::new(Volume::default(), ReverbBus)
                ],
            ))
            .connect(SpatialOut);

        app.init_resource::<MusicPlayer>()
            .init_resource::<Listener>()
            .add_message::<MixerMessage>()
            .add_systems(Update, systems::update_mixer)
            // Run before `Queue` so the gate change is sent in the same frame as the
            // first real spatial offsets computed by `bevy_seedling`.
            .add_systems(Last, systems::gate_spatial_output.before(SeedlingSystems::Queue));
    }
}

#[derive(Component)]
#[require(Transform, SamplePlayer, MainPool)]
pub struct Sound;

#[derive(Debug, Clone)]
pub struct StartStaticSound {
    pub world: Entity,
    pub src: Handle<AudioSample>,
    pub origin: Vec3,
    pub volume: f32,
    pub attenuation: f32,
}

#[derive(Clone, Debug, Resource)]
pub struct GlobalMixer {
    pub mixer: Entity,
}

/// Represents a single audio channel, capable of playing one sound at a time.
#[derive(Clone, Debug, Component)]
pub struct Channel {
    channel: i8,
}

#[derive(Debug, Clone)]
pub struct StartSound {
    pub entity: Entity,
    pub src: Handle<AudioSample>,
    pub ent_channel: i8,
    pub volume: f32,
    pub attenuation: f32,
    pub origin: Vec3,
}

#[derive(Debug, Clone, Copy)]
pub struct StopSound {
    pub entity: Entity,
    pub ent_channel: i8,
}

#[derive(Debug, Clone)]
// TODO: Make this an asset
pub enum MusicSource {
    Named(String),
    TrackId(usize),
}

#[derive(Message, Debug, Clone)]
pub enum MixerMessage {
    StartSound(StartSound),
    StopSound(StopSound),
    StartStaticSound(StartStaticSound),
    /// Play a non-spatialized sound, like `S_LocalSound`.
    StartLocalSound(Handle<AudioSample>),
    /// If None, restarts already-playing music
    StartMusic(Option<MusicSource>),
    PauseMusic,
    StopMusic,
}

mod systems {
    use crate::client::DEFAULT_SOUND_PACKET_ATTENUATION;

    use super::*;

    /// Mute spatialized output while no [`SpatialListener3D`] is active.
    ///
    /// Disabled entities (such as the `Template` world camera) are skipped by the query.
    pub fn gate_spatial_output(
        listeners: Query<(), With<SpatialListener3D>>,
        mut gate: Query<&mut VolumeNode, With<SpatialOut>>,
    ) {
        let target = if listeners.is_empty() { Volume::SILENT } else { Volume::UNITY_GAIN };
        for mut node in &mut gate {
            // Only write on change, to avoid sending a patch every frame.
            if node.volume != target {
                node.volume = target;
            }
        }
    }

    pub fn update_mixer(
        channels: Query<&Channel, With<Sound>>,
        mut music_player: ResMut<MusicPlayer>,
        asset_server: Res<AssetServer>,
        mut events: MessageReader<MixerMessage>,
        children: Query<&Children>,
        mut commands: Commands,
        mut all_sounds: Query<&mut SamplerNode>,
    ) {
        for event in events.read() {
            match *event {
                MixerMessage::StartSound(StartSound { entity, ent_channel, .. })
                | MixerMessage::StopSound(StopSound { entity, ent_channel }) => {
                    if let Ok(children) = children.get(entity) {
                        for entity_sound in children {
                            if let Ok(Channel { channel }) = channels.get(*entity_sound)
                                && *channel == ent_channel
                                && let Ok(mut e) = commands.get_entity(*entity_sound)
                            {
                                e.try_despawn();
                            }
                        }
                    } else {
                        continue;
                    }
                }
                _ => {}
            }

            match event {
                MixerMessage::StartSound(sound) => {
                    let attenuation = if sound.attenuation.is_finite() {
                        sound.attenuation
                    } else {
                        DEFAULT_SOUND_PACKET_ATTENUATION
                    };
                    commands.spawn((
                        Sound,
                        ChildOf(sound.entity),
                        Channel { channel: sound.ent_channel },
                        SamplePlayer::new(sound.src.clone()),
                        Transform::from_translation(sound.origin),
                        sample_effects![
                            (
                                SpatialBasicNode {
                                    volume: Volume::Linear(sound.volume),
                                    ..Default::default()
                                },
                                SpatialScale(Vec3::splat(attenuation))
                            ),
                            SendNode::new(Volume::Linear(REVERB_AMT), ReverbBus)
                        ],
                    ));
                }
                MixerMessage::StopSound(StopSound { .. }) => {
                    // Handled by previous match
                }
                MixerMessage::StartStaticSound(sound) => {
                    let attenuation = if sound.attenuation.is_finite() {
                        sound.attenuation
                    } else {
                        DEFAULT_SOUND_PACKET_ATTENUATION
                    };
                    commands.spawn((
                        Sound,
                        ChildOf(sound.world),
                        SamplePlayer::new(sound.src.clone()).looping(),
                        Transform::from_translation(sound.origin),
                        sample_effects![
                            (
                                SpatialBasicNode {
                                    volume: Volume::Linear(sound.volume),
                                    ..Default::default()
                                },
                                SpatialScale(Vec3::splat(attenuation))
                            ),
                            SendNode::new(Volume::Linear(REVERB_AMT), ReverbBus)
                        ],
                    ));
                }
                MixerMessage::StartLocalSound(src) => {
                    commands.spawn((SamplePlayer::new(src.clone()), LocalPool));
                }
                MixerMessage::StartMusic(Some(MusicSource::Named(named))) => {
                    // TODO: Error handling
                    music_player
                        .play_named(&asset_server, &mut commands, MusicPool, named)
                        .unwrap();
                }
                MixerMessage::StartMusic(Some(MusicSource::TrackId(id))) => {
                    // TODO: Error handling
                    music_player.play_track(&asset_server, &mut commands, MusicPool, *id).unwrap();
                }
                MixerMessage::StartMusic(None) => music_player.resume(&mut all_sounds),
                MixerMessage::StopMusic => music_player.stop(&mut commands),
                MixerMessage::PauseMusic => music_player.pause(&mut all_sounds),
            }
        }
    }
}
