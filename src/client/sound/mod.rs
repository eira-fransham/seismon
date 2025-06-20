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
    asset::{AssetServer, Handle},
    audio::{
        AudioPlayer, AudioSinkPlayback as _, AudioSource, PlaybackMode, PlaybackSettings, Volume,
    },
    ecs::{
        bundle::Bundle,
        component::Component,
        entity::Entity,
        event::{Event, EventReader},
        system::{Commands, Query, Res, ResMut, Resource},
    },
};
use fundsp::snoop::{Snoop, SnoopBackend};

use bevy_mod_dynamicaudio::{
    AddAudioMixer,
    audio::{AudioSink, Mixer},
};

pub use music::MusicPlayer;

use std::io::{self, Read as _};

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

pub fn load<S>(vfs: &Vfs, name: S) -> Result<AudioSource, SoundError>
where
    S: AsRef<str>,
{
    let name = name.as_ref();
    let full_path = "sound/".to_owned() + name;
    let mut file = vfs.open(&full_path)?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;
    Ok(AudioSource { bytes: data.into() })
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

impl Plugin for SeismonSoundPlugin {
    fn build(&self, app: &mut bevy::prelude::App) {
        let (snoop_l, send_l) = Snoop::new(1024);
        let (snoop_r, send_r) = Snoop::new(1024);
        let mixer = create_mixer(send_l, send_r);

        let global_audio = GetGlobalAudio {
            left: snoop_l,
            right: snoop_r,
        };

        app.add_audio_mixer::<ReverbNode>();
        let mixer_id = app
            .world_mut()
            .spawn(Mixer {
                processor: Some(mixer),
            })
            .id();
        app.insert_resource(GlobalMixer { mixer: mixer_id })
            .insert_resource(global_audio)
            .init_resource::<MusicPlayer>()
            .init_resource::<Listener>()
            .add_event::<MixerEvent>()
            .add_systems(
                Main,
                (
                    systems::update_entities,
                    systems::update_mixer,
                    systems::update_listener,
                    systems::write_audio,
                ),
            );
    }
}

#[derive(Component)]
pub struct StaticSound {
    pub origin: Vector3<f32>,
    pub volume: f32,
    pub attenuation: f32,
}

pub fn update_static_sounds(
    static_sounds: Query<(&AudioSink, &StaticSound)>,
    listener: Res<Listener>,
) {
    for (sink, sound) in static_sounds.iter() {
        sound.update(sink, &*listener);
    }
}

#[derive(Debug, Clone)]
pub struct StartStaticSound {
    pub src: Handle<AudioSource>,
    pub origin: Vector3<f32>,
    pub volume: f32,
    pub attenuation: f32,
}

#[derive(Bundle)]
struct StaticSoundBundle {
    static_sound: StaticSound,
    audio: AudioPlayer,
    settings: PlaybackSettings,
}

impl StaticSoundBundle {
    fn new(value: &StartStaticSound, listener: &Listener) -> Self {
        Self {
            static_sound: StaticSound {
                origin: value.origin,
                volume: value.volume,
                attenuation: value.attenuation,
            },
            audio: AudioPlayer(value.src.clone()),
            settings: PlaybackSettings {
                mode: PlaybackMode::Loop,
                // TODO: Use Bevy's built-in spacialiser
                volume: Volume::new(listener.attenuate(
                    value.origin,
                    value.volume,
                    value.attenuation,
                )),
                ..Default::default()
            },
        }
    }
}

impl StaticSound {
    fn update(&self, audio_sink: &AudioSink, listener: &Listener) {
        // attenuate using quake coordinates since distance is the same either way
        // TODO: Use Bevy's built-in spacialiser
        audio_sink.set_volume(listener.attenuate(self.origin, self.volume, self.attenuation));
    }
}

#[derive(Clone, Debug, Resource)]
pub struct GlobalMixer {
    pub mixer: Entity,
}

/// Represents a single audio channel, capable of playing one sound at a time.
#[derive(Clone, Debug, Component)]
pub struct Channel {
    channel: i8,
    master_vol: f32,
    attenuation: f32,
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
    audio: AudioPlayer,
    settings: PlaybackSettings,
}

#[derive(Bundle)]
struct TempEntitySoundBundle {
    chan: Channel,
    audio: AudioPlayer,
    settings: PlaybackSettings,
}

fn make_bundle(
    value: &StartSound,
    listener: &Listener,
) -> Result<EntitySoundBundle, TempEntitySoundBundle> {
    let chan = Channel {
        origin: value.origin.into(),
        master_vol: value.volume,
        attenuation: value.attenuation,
        channel: value.ent_channel,
    };
    let audio = AudioPlayer(value.src.clone());
    let settings = PlaybackSettings {
        mode: PlaybackMode::Despawn,
        // TODO: Use Bevy's built-in spacialiser
        volume: Volume::new(listener.attenuate(
            value.origin.into(),
            value.volume,
            value.attenuation,
        )),
        ..Default::default()
    };

    match value.ent_id {
        Some(id) => Ok(EntitySoundBundle {
            chan,
            audio,
            settings,
            entity: EntityChannel { id },
        }),
        None => Err(TempEntitySoundBundle {
            chan,
            audio,
            settings,
        }),
    }
}

impl Channel {
    pub fn update(&self, sink: &mut AudioSink, listener: &Listener) {
        // attenuate using quake coordinates since distance is the same either way
        // TODO: Use Bevy's built-in spacialiser
        sink.set_volume(listener.attenuate(self.origin, self.master_vol, self.attenuation));
    }
}

#[derive(Debug, Default, Clone)]
pub struct StartSound {
    pub src: Handle<AudioSource>,
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

#[derive(Resource)]
pub struct GetGlobalAudio {
    pub left: Snoop,
    pub right: Snoop,
}

mod systems {
    use bevy_mod_dynamicaudio::audio::AudioTarget;

    use crate::client::Connection;

    use super::*;

    pub fn update_mixer(
        channels: Query<(Entity, &Channel, Option<&EntityChannel>)>,
        vfs: Res<Vfs>,
        listener: Res<Listener>,
        mut music_player: ResMut<MusicPlayer>,
        asset_server: Res<AssetServer>,
        mixer: Res<GlobalMixer>,
        mut events: EventReader<MixerEvent>,
        mut commands: Commands,
        all_sounds: Query<&AudioSink>,
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
                            && let Some(mut e) = commands.get_entity(e)
                        {
                            e.despawn();
                        }
                    }
                }
                _ => {}
            }

            match *event {
                MixerEvent::StartSound(ref start) => match make_bundle(start, &*listener) {
                    Ok(bundle) => {
                        commands.spawn((
                            bundle,
                            AudioTarget {
                                target: mixer.mixer,
                            },
                        ));
                    }
                    Err(bundle) => {
                        commands.spawn((
                            bundle,
                            AudioTarget {
                                target: mixer.mixer,
                            },
                        ));
                    }
                },
                MixerEvent::StopSound(StopSound { .. }) => {
                    // Handled by previous match
                }
                MixerEvent::StartStaticSound(ref static_sound) => {
                    commands.spawn(StaticSoundBundle::new(static_sound, &*listener));
                }
                MixerEvent::StartMusic(Some(MusicSource::Named(ref named))) => {
                    // TODO: Error handling
                    music_player
                        .play_named(
                            &*asset_server,
                            &mut commands,
                            &*vfs,
                            Some(AudioTarget {
                                target: mixer.mixer,
                            }),
                            named,
                        )
                        .unwrap();
                }
                MixerEvent::StartMusic(Some(MusicSource::TrackId(id))) => {
                    // TODO: Error handling
                    music_player
                        .play_track(
                            &*asset_server,
                            &mut commands,
                            &*vfs,
                            Some(AudioTarget {
                                target: mixer.mixer,
                            }),
                            id,
                        )
                        .unwrap();
                }
                MixerEvent::StartMusic(None) => music_player.resume(&all_sounds),
                MixerEvent::StopMusic => music_player.stop(&mut commands),
                MixerEvent::PauseMusic => music_player.pause(&all_sounds),
            }
        }
    }

    pub fn update_entities(
        mut entities: Query<(&mut AudioSink, Option<&EntityChannel>, &mut Channel)>,
        listener: Res<Listener>,
        conn: Option<Res<Connection>>,
    ) {
        let Some(conn) = conn else {
            return;
        };

        for (mut sink, e_chan, mut chan) in entities.iter_mut() {
            if let Some(e) = e_chan.and_then(|e| conn.state.entities.get(e.id)) {
                chan.origin = e.origin;
            }

            chan.update(&mut *sink, &*listener)
        }
    }

    pub fn update_listener(mut listener: ResMut<Listener>, conn: Option<Res<Connection>>) {
        if let Some(new_listener) = conn.and_then(|conn| conn.state.update_listener()) {
            *listener = new_listener;
        }
    }

    // TODO: Use this for `startvideo`
    pub fn write_audio(mut global_audio: ResMut<GetGlobalAudio>) {
        global_audio.left.update();
        global_audio.right.update();
    }
}
