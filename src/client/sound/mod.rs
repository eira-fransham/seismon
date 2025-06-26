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

mod limiter;
mod music;

use bevy::{
    app::{Main, Plugin},
    prelude::*,
};
use bevy_seedling::{
    prelude::{Volume, *},
    sample::Sample,
};
use firewheel::sample_resource::DecodedAudioF32;
use fundsp::snoop::{Snoop, SnoopBackend};

pub use music::MusicPlayer;
use symphonium::symphonia::core::probe::Hint;

use std::{
    io::{self, Cursor, Read as _},
    num::NonZeroU32,
    path::Path,
};

use crate::common::vfs::{Vfs, VfsError};

use cgmath::{InnerSpace, Vector3};
use thiserror::Error;

pub const DISTANCE_ATTENUATION_FACTOR: f32 = 0.001;

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
#[derive(Debug, Clone, Resource)]
pub struct Listener {
    pub origin: Vector3<f32>,
    pub left_ear: Vector3<f32>,
    pub right_ear: Vector3<f32>,
}

impl Default for Listener {
    fn default() -> Self {
        Listener {
            origin: Vector3::new(0.0, 0.0, 0.0),
            left_ear: Vector3::new(0.0, 0.0, 0.0),
            right_ear: Vector3::new(0.0, 0.0, 0.0),
        }
    }
}

impl Listener {
    pub fn origin(&self) -> Vector3<f32> {
        self.origin
    }

    pub fn left_ear(&self) -> Vector3<f32> {
        self.left_ear
    }

    pub fn right_ear(&self) -> Vector3<f32> {
        self.right_ear
    }

    pub fn set_origin(&mut self, new_origin: Vector3<f32>) {
        self.origin = new_origin;
    }

    pub fn set_left_ear(&mut self, new_origin: Vector3<f32>) {
        self.left_ear = new_origin;
    }

    pub fn set_right_ear(&mut self, new_origin: Vector3<f32>) {
        self.right_ear = new_origin;
    }

    pub fn attenuate(
        &self,
        emitter_origin: Vector3<f32>,
        base_volume: f32,
        attenuation: f32,
    ) -> f32 {
        let decay =
            (emitter_origin - self.origin).magnitude() * attenuation * DISTANCE_ATTENUATION_FACTOR;
        let volume = ((1.0 - decay) * base_volume).max(0.0);
        volume
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

    if let Some(ext) = ext.as_ref().map(|ext| &**ext) {
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

type ReverbNode = impl fundsp::audionode::AudioNode + Send + Sync + 'static;

#[define_opaque(ReverbNode)]
fn create_mixer(sender_l: SnoopBackend, sender_r: SnoopBackend) -> ReverbNode {
    use self::limiter::limiter_stereo;
    use fundsp::hacker32::*;

    let sender_l = An(sender_l);
    let sender_r = An(sender_r);

    let delay_time = 0.15;
    let delay = feedback(
        0.4 * ((delay(delay_time) | delay(delay_time))
            >> (moog_hz(1500., 0.) | moog_hz(1500., 0.))),
    );

    ((multipass() & (0.3 * reverb_stereo(20.0, 0.8, 0.5)) & (0.2 * delay))
        >> limiter_stereo(0., 0.7, 0.7)
        >> limiter_stereo(0., 0.03, 0.05)
        >> (sender_l | sender_r))
        .0
}

pub struct SeismonSoundPlugin;

#[derive(NodeLabel, PartialEq, Eq, Debug, Hash, Clone)]
pub struct ReverbBus;

#[derive(NodeLabel, PartialEq, Eq, Debug, Hash, Clone)]
pub struct MasterBus;

#[derive(PoolLabel, PartialEq, Eq, Debug, Hash, Clone)]
pub struct MainPool;

#[derive(PoolLabel, PartialEq, Eq, Debug, Hash, Clone)]
pub struct MusicPool;

impl Plugin for SeismonSoundPlugin {
    fn build(&self, app: &mut bevy::prelude::App) {
        let mut commands = app.world_mut().commands();

        commands.spawn(MasterBus);

        commands
            .spawn((
                VolumeNode {
                    volume: Volume::Decibels(-12.),
                },
                ReverbBus,
            ))
            .chain_node(FreeverbNode::default())
            .connect(MasterBus);

        commands
            .spawn((SpatialListener3D, SamplerPool(MainPool)))
            .connect(ReverbBus)
            .connect(MasterBus);

        commands.spawn(SamplerPool(MusicPool)).connect(MasterBus);

        app.init_resource::<MusicPlayer>()
            .init_resource::<Listener>()
            .add_event::<MixerEvent>()
            .add_systems(
                Main,
                (
                    systems::update_entities,
                    systems::update_mixer,
                    systems::update_listener,
                ),
            );
    }
}

#[derive(Component)]
#[require(Transform, SamplePlayer)]
pub struct StaticSound;

#[derive(Debug, Clone)]
pub struct StartStaticSound {
    pub src: Handle<Sample>,
    pub origin: Vector3<f32>,
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
    origin: Vector3<f32>,
}

#[derive(Clone, Debug, Component)]
pub struct EntityChannel {
    // if None, sound is associated with a temp entity
    id: usize,
}

#[derive(Bundle)]
struct EntitySoundBundle {
    entity: EntityChannel,
    chan: Channel,
    audio: SamplePlayer,
    settings: PlaybackSettings,
}

#[derive(Bundle)]
struct TempEntitySoundBundle {
    chan: Channel,
    audio: SamplePlayer,
    settings: PlaybackSettings,
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
                MixerEvent::StartSound(StartSound {
                    ent_id,
                    ent_channel,
                    ..
                })
                | MixerEvent::StopSound(StopSound {
                    ent_id,
                    ent_channel,
                }) => {
                    for (e, chan, e_chan) in channels.iter() {
                        if chan.channel == ent_channel
                            && e_chan.map(|e| e.id) == ent_id
                            && let Ok(mut e) = commands.get_entity(e)
                        {
                            e.despawn();
                        }
                    }
                }
                _ => {}
            }

            match event {
                MixerEvent::StartSound(start) => {
                    commands.spawn((
                        Channel {
                            origin: start.origin.into(),
                            channel: start.ent_channel,
                        },
                        SamplePlayer::new(start.src.clone()),
                        SpatialScale(Vec3::new(
                            start.attenuation,
                            start.attenuation,
                            start.attenuation,
                        )),
                        MainPool,
                        sample_effects![
                            VolumeNode {
                                volume: Volume::Linear(start.volume),
                            },
                            SpatialBasicNode {
                                panning_threshold: 0.3,
                                ..Default::default()
                            }
                        ],
                    ));
                }
                MixerEvent::StopSound(StopSound { .. }) => {
                    // Handled by previous match
                }
                MixerEvent::StartStaticSound(static_sound) => {
                    let origin: [f32; 3] = static_sound.origin.into();
                    commands.spawn((
                        Transform::from_translation(origin.into()),
                        SamplePlayer::new(static_sound.src.clone()).looping(),
                        SpatialScale(Vec3::new(
                            static_sound.attenuation,
                            static_sound.attenuation,
                            static_sound.attenuation,
                        )),
                        MainPool,
                        sample_effects![
                            VolumeNode {
                                volume: Volume::Linear(static_sound.volume),
                            },
                            SpatialBasicNode {
                                panning_threshold: 0.3,
                                ..Default::default()
                            }
                        ],
                    ));
                }
                MixerEvent::StartMusic(Some(MusicSource::Named(named))) => {
                    // TODO: Error handling
                    music_player
                        .play_named(&*asset_server, &mut commands, &*vfs, MusicPool, named)
                        .unwrap();
                }
                MixerEvent::StartMusic(Some(MusicSource::TrackId(id))) => {
                    // TODO: Error handling
                    music_player
                        .play_track(&*asset_server, &mut commands, &*vfs, MusicPool, *id)
                        .unwrap();
                }
                MixerEvent::StartMusic(None) => music_player.resume(&mut all_sounds),
                MixerEvent::StopMusic => music_player.stop(&mut commands),
                MixerEvent::PauseMusic => music_player.pause(&mut all_sounds),
            }
        }
    }

    pub fn update_entities(
        mut entities: Query<(&mut SpatialBasicNode, Option<&EntityChannel>, &mut Channel)>,
        listener: Res<Listener>,
        conn: Option<Res<Connection>>,
    ) {
        let Some(conn) = conn else {
            return;
        };

        for (mut spatial_element, e_chan, mut chan) in entities.iter_mut() {
            if let Some(e) = e_chan.and_then(|e| conn.state.entities.get(e.id)) {
                let offset: [f32; 3] = (e.origin - listener.origin).into();
                spatial_element.offset = offset.into();
            }
        }
    }

    pub fn update_listener(mut listener: ResMut<Listener>, conn: Option<Res<Connection>>) {
        if let Some(new_listener) = conn.and_then(|conn| conn.state.update_listener()) {
            *listener = new_listener;
        }
    }
}
