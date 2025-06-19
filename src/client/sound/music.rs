use std::io::Read;

use crate::{client::sound::SoundError, common::vfs::Vfs};

use bevy::{
    asset::AssetServer,
    audio::{AudioBundle, AudioSinkPlayback as _, AudioSource, PlaybackMode, PlaybackSettings},
    ecs::{
        entity::Entity,
        system::{Commands, Query, Resource},
    },
    prelude::*,
};

use bevy_mod_dynamicaudio::audio::{AudioSink, AudioTarget};

/// Plays music tracks.
#[derive(Resource, Default)]
pub struct MusicPlayer {
    playing: Option<(String, Entity)>,
}

impl MusicPlayer {
    pub fn new() -> MusicPlayer {
        MusicPlayer { playing: None }
    }

    /// Start playing the track with the given name.
    ///
    /// Music tracks are expected to be in the "music/" directory of the virtual
    /// filesystem, so they can be placed either in an actual directory
    /// `"id1/music/"` or packaged in a PAK archive with a path beginning with
    /// `"music/"`.
    ///
    /// If the specified track is already playing, this has no effect.
    pub fn play_named<S>(
        &mut self,
        asset_server: &AssetServer,
        commands: &mut Commands,
        vfs: &Vfs,
        mixer: Option<AudioTarget>,
        name: S,
    ) -> Result<(), SoundError>
    where
        S: AsRef<str>,
    {
        let name = name.as_ref();

        // don't replay the same track
        if let Some((playing, _)) = &self.playing {
            if playing == name {
                return Ok(());
            }
        }

        // TODO: there's probably a better way to do this extension check
        let mut file = if !name.contains('.') {
            // try all supported formats
            let Ok(file) = vfs
                .open(format!("music/{}.flac", name))
                .or_else(|_| vfs.open(format!("music/{}.wav", name)))
                .or_else(|_| vfs.open(format!("music/{}.mp3", name)))
                .or_else(|_| vfs.open(format!("music/{}.ogg", name)))
                .or(Err(SoundError::NoSuchTrack(name.to_owned())))
            else {
                return Ok(());
            };

            file
        } else {
            vfs.open(name)?
        };

        // TODO: Turn VFS into an AssetReader so that this is asynchronous
        let mut data = Vec::new();
        file.read_to_end(&mut data)?;

        let source = AudioPlayer::new(asset_server.add(AudioSource { bytes: data.into() }));

        self.stop(commands);

        let entity = match mixer {
            Some(target) => commands.spawn((
                AudioBundle {
                    source,
                    settings: PlaybackSettings {
                        mode: PlaybackMode::Loop,
                        ..Default::default()
                    },
                },
                target,
            )),
            None => commands.spawn(AudioBundle {
                source,
                settings: PlaybackSettings {
                    mode: PlaybackMode::Loop,
                    ..Default::default()
                },
            }),
        }
        .id();
        self.playing = Some((name.to_string(), entity));

        Ok(())
    }

    /// Start playing the track with the given number.
    ///
    /// Note that the first actual music track is track 2; track 1 on the
    /// original Quake CD-ROM held the game data.
    pub fn play_track(
        &mut self,
        asset_server: &AssetServer,
        commands: &mut Commands,
        vfs: &Vfs,
        mixer: Option<AudioTarget>,
        track_id: usize,
    ) -> Result<(), SoundError> {
        self.play_named(
            asset_server,
            commands,
            vfs,
            mixer,
            format!("track{:02}", track_id),
        )
    }

    /// Stop the current music track.
    ///
    /// This ceases playback entirely. To pause the track, allowing it to be
    /// resumed later, use `MusicPlayer::pause()`.
    ///
    /// If no music track is currently playing, this has no effect.
    pub fn stop(&self, commands: &mut Commands) {
        if let Some(mut entity) = self
            .playing
            .as_ref()
            .and_then(|(_, e)| commands.get_entity(*e))
        {
            entity.despawn();
        }
    }

    /// Pause the current music track.
    ///
    /// If no music track is currently playing, or if the current track is
    /// already paused, this has no effect.
    pub fn pause(&self, query: &Query<&AudioSink>) {
        if let Some(sink) = self.playing.as_ref().and_then(|(_, e)| query.get(*e).ok()) {
            sink.pause();
        }
    }

    /// Resume playback of the current music track.
    ///
    /// If no music track is currently playing, or if the current track is not
    /// paused, this has no effect.
    pub fn resume(&self, query: &Query<&AudioSink>) {
        if let Some(sink) = self.playing.as_ref().and_then(|(_, e)| query.get(*e).ok()) {
            sink.play();
        }
    }
}
