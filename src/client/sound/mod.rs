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

use bevy::{
    app::{Main, Plugin},
    prelude::*,
};
use bevy_seedling::{prelude::*, sample::Sample};
use firewheel::sample_resource::DecodedAudioF32;

pub use music::MusicPlayer;
use symphonium::symphonia::core::probe::Hint;

use std::{
    io::{self, Cursor, Read as _},
    path::Path,
};

use crate::common::vfs::{Vfs, VfsError};

use thiserror::Error;

pub const DISTANCE_ATTENUATION_FACTOR: f32 = 0.001;

fn attenuation_factor(attenuation: f32) -> Vec3 {
    Vec3::splat(attenuation * DISTANCE_ATTENUATION_FACTOR)
}

#[derive(Error, Debug)]
pub enum SoundError {
    #[error("No such music track: {0}")]
    NoSuchTrack(String),
    #[error("I/O error: {0}")]
    Io(#[from] io::Error),
    #[error("Virtual filesystem error: {0}")]
    Vfs(#[from] VfsError),
    #[error("Decoding error: {0}")]
    DecodeError(#[from] symphonium::error::LoadError),
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

pub fn load<S>(
    sound_loader: &mut symphonium::SymphoniumLoader,
    vfs: &Vfs,
    name: S,
) -> Result<Sample, SoundError>
where
    S: AsRef<str>,
{
    let name = name.as_ref();
    let full_path = "sound/".to_owned() + name;
    let mut file = vfs.open(&full_path)?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;

    let ext = Path::new(name).extension().map(|ext| ext.to_string_lossy());

    let cursor = Cursor::new(data);

    let mut decode_hint = Hint::new();

    if let Some(ext) = ext.as_deref() {
        decode_hint.with_extension(ext);
    }

    let decoded = sound_loader.load_f32_from_source(
        Box::new(cursor),
        Some(decode_hint),
        None,
        Default::default(),
        None,
    )?;

    Ok(Sample::new(DecodedAudioF32::from(decoded)))
}

pub struct SeismonSoundPlugin;

#[derive(NodeLabel, PartialEq, Eq, Debug, Hash, Clone, Default)]
pub struct ReverbBus;

#[derive(NodeLabel, PartialEq, Eq, Debug, Hash, Clone, Default)]
pub struct MasterOut;

#[derive(PoolLabel, PartialEq, Eq, Debug, Hash, Clone, Default)]
pub struct MainPool;

#[derive(PoolLabel, PartialEq, Eq, Debug, Hash, Clone, Default)]
pub struct MusicPool;

impl Plugin for SeismonSoundPlugin {
    fn build(&self, app: &mut bevy::prelude::App) {
        let mut commands = app.world_mut().commands();

        commands
            .spawn((VolumeNode { volume: Volume::Decibels(-40.) }, ReverbBus))
            .chain_node(FreeverbNode::default())
            .connect(MasterOut);

        commands.spawn((VolumeNode::default(), MasterOut));

        commands.spawn(SpatialListener3D);

        commands.spawn((SamplerPool(MusicPool), PoolSize(1..=1)));
        commands
            .spawn((
                SamplerPool(MainPool),
                PoolSize(256..=256),
                sample_effects![
                    (SpatialBasicNode::default(), SpatialScale::default()),
                    SendNode::new(Volume::default(), ReverbBus)
                ],
            ))
            .connect(MasterOut);

        app.init_resource::<MusicPlayer>()
            .init_resource::<Listener>()
            .add_event::<MixerEvent>()
            .add_systems(
                Main,
                (systems::update_entities, systems::update_mixer, systems::update_listener),
            );
    }
}

#[derive(Component)]
#[require(Transform, SamplePlayer, MainPool)]
pub struct Sound;

#[derive(Debug, Clone)]
pub struct StartStaticSound {
    pub src: Handle<Sample>,
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

/// If a `Sound` does not include this component, sound is associated with a temp entity
#[derive(Clone, Debug, Component)]
pub struct EntityChannel {
    id: usize,
}

#[derive(Debug, Default, Clone)]
pub struct StartSound {
    pub src: Handle<Sample>,
    pub ent_id: Option<usize>,
    pub ent_channel: i8,
    pub volume: f32,
    pub attenuation: f32,
    pub origin: [f32; 3],
}

#[derive(Debug, Default, Clone, Copy)]
pub struct StopSound {
    pub ent_id: Option<usize>,
    pub ent_channel: i8,
}

#[derive(Debug, Clone)]
// TODO: Make this an asset
pub enum MusicSource {
    Named(String),
    TrackId(usize),
}

#[derive(Event, Debug, Clone)]
pub enum MixerEvent {
    StartSound(StartSound),
    StopSound(StopSound),
    StartStaticSound(StartStaticSound),
    /// If None, restarts already-playing music
    StartMusic(Option<MusicSource>),
    PauseMusic,
    StopMusic,
}

mod systems {
    use crate::client::Connection;

    use super::*;

    pub fn update_mixer(
        channels: Query<(Entity, &Channel, Option<&EntityChannel>)>,
        vfs: Res<Vfs>,
        mut music_player: ResMut<MusicPlayer>,
        asset_server: Res<AssetServer>,
        mut events: EventReader<MixerEvent>,
        mut commands: Commands,
        mut all_sounds: Query<&mut SamplerNode>,
    ) {
        for event in events.read() {
            match *event {
                MixerEvent::StartSound(StartSound { ent_id, ent_channel, .. })
                | MixerEvent::StopSound(StopSound { ent_id, ent_channel }) => {
                    for (e, chan, e_chan) in channels.iter() {
                        if chan.channel == ent_channel
                            && e_chan.map(|e| e.id) == ent_id
                            && let Ok(mut e) = commands.get_entity(e)
                        {
                            e.try_despawn();
                        }
                    }
                }
                _ => {}
            }

            match event {
                MixerEvent::StartSound(start) => {
                    let attenuation =
                        if start.attenuation.is_finite() { start.attenuation } else { 1. };
                    // TODO: Entity channels should be children of the enitities they're spawned on.
                    let mut new_sound = commands.spawn((
                        Sound,
                        Channel { channel: start.ent_channel },
                        SamplePlayer::new(start.src.clone()),
                        Transform::from_translation(start.origin.into()),
                        sample_effects![
                            (
                                SpatialBasicNode {
                                    volume: Volume::Linear(start.volume),
                                    ..Default::default()
                                },
                                SpatialScale(attenuation_factor(attenuation))
                            ),
                            SendNode::new(Volume::Linear(attenuation), ReverbBus)
                        ],
                    ));

                    if let Some(id) = start.ent_id {
                        new_sound.insert(EntityChannel { id });
                    }
                }
                MixerEvent::StopSound(StopSound { .. }) => {
                    // Handled by previous match
                }
                MixerEvent::StartStaticSound(static_sound) => {
                    let attenuation = if static_sound.attenuation.is_finite() {
                        static_sound.attenuation
                    } else {
                        1.
                    };
                    commands.spawn((
                        Sound,
                        SamplePlayer::new(static_sound.src.clone()).looping(),
                        Transform::from_translation(static_sound.origin),
                        sample_effects![
                            (
                                SpatialBasicNode {
                                    // TODO: Fudge factor - ambient sounds are REALLY loud for some
                                    // reason
                                    volume: Volume::Linear(
                                        static_sound.volume
                                            * (1. - static_sound.attenuation).max(0.5)
                                    ),
                                    ..Default::default()
                                },
                                SpatialScale(attenuation_factor(static_sound.attenuation))
                            ),
                            SendNode::new(Volume::Linear(attenuation), ReverbBus)
                        ],
                    ));
                }
                MixerEvent::StartMusic(Some(MusicSource::Named(named))) => {
                    // TODO: Error handling
                    music_player
                        .play_named(&asset_server, &mut commands, MusicPool, named)
                        .unwrap();
                }
                MixerEvent::StartMusic(Some(MusicSource::TrackId(id))) => {
                    // TODO: Error handling
                    music_player.play_track(&asset_server, &mut commands, MusicPool, *id).unwrap();
                }
                MixerEvent::StartMusic(None) => music_player.resume(&mut all_sounds),
                MixerEvent::StopMusic => music_player.stop(&mut commands),
                MixerEvent::PauseMusic => music_player.pause(&mut all_sounds),
            }
        }
    }

    pub fn update_entities(
        entities: Query<(&mut Transform, &EntityChannel), With<Sound>>,
        conn: Option<Res<Connection>>,
    ) {
        // let Some(conn) = conn else {
        //     return;
        // };

        // for (mut transform, e_chan) in &mut entities {
        //     if let Some(e) = conn.client_state.entities.get(e_chan.id) {
        //         *transform = Transform::from_translation(e.origin);
        //     }
        // }
    }

    pub fn update_listener(
        listeners: Query<&mut Transform, With<SpatialListener3D>>,
        conn: Option<Res<Connection>>,
    ) {
        // if let Some(new_listener) = conn.and_then(|conn| conn.state.update_listener()) {
        //     for mut transform in &mut listeners {
        //         *transform = Transform::from_rotation(new_listener.rotation)
        //             .with_translation(new_listener.origin);
        //     }
        // }
    }
}
