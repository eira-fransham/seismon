use std::{
    io::{Cursor, Read},
    num::NonZeroU32,
    path::Path,
};

use crate::{client::sound::SoundError, common::vfs::Vfs};

use beef::Cow;
use bevy::{
    asset::AssetServer,
    ecs::{
        entity::Entity,
        resource::Resource,
        system::{Commands, Query},
    },
    prelude::*,
};

use bevy_seedling::{
    prelude::{PoolLabel, SamplerNode},
    sample::{Sample, SamplePlayer},
};
use firewheel::sample_resource::DecodedAudioF32;
use symphonium::{SymphoniumLoader, symphonia::core::probe::Hint};

/// Plays music tracks.
#[derive(Resource)]
pub struct MusicPlayer {
    loader: SymphoniumLoader,
    playing: Option<(String, Entity)>,
}

impl Default for MusicPlayer {
    fn default() -> Self {
        Self {
            loader: SymphoniumLoader::new(),
            playing: None,
        }
    }
}

impl MusicPlayer {
    pub fn new() -> MusicPlayer {
        MusicPlayer {
            loader: SymphoniumLoader::new(),
            playing: None,
        }
    }

    /// Start playing the track with the given name.
    ///
    /// Music tracks are expected to be in the "music/" directory of the virtual
    /// filesystem, so they can be placed either in an actual directory
    /// `"id1/music/"` or packaged in a PAK archive with a path beginning with
    /// `"music/"`.
    ///
    /// If the specified track is already playing, this has no effect.
    pub fn play_named<S, Pool: Component + PoolLabel>(
        &mut self,
        asset_server: &AssetServer,
        commands: &mut Commands,
        vfs: &Vfs,
        pool: Pool,
        name: S,
    ) -> Result<(), SoundError>
    where
        S: AsRef<str>,
    {
        let name = name.as_ref();

        // don't replay the same track
        if let Some((playing, _)) = &self.playing && playing == name {
            return Ok(());
        }

        // TODO: there's probably a better way to do this extension check
        let (mut file, extension) = if !name.contains('.') {
            'out: {
                for extension in ["flac", "wav", "mp3", "ogg"] {
                    if let Ok(file) = vfs.open(format!("music/{name}.{extension}")) {
                        break 'out (file, Some(Cow::from(extension)));
                    }
                }

                return Ok(());
            }
        } else {
            let path = Path::new(name);
            let ext = path.extension().map(|e| e.to_string_lossy().into());
            (vfs.open(name)?, ext)
        };

        // TODO: Turn VFS into an AssetReader so that this is asynchronous
        let mut data = Vec::new();
        file.read_to_end(&mut data)?;

        let cursor = Cursor::new(data);

        let mut decode_hint = Hint::new();

        if let Some(extension) = extension {
            decode_hint.with_extension(&extension);
        }

        let decoded = self.loader.load_f32_from_source(
            Box::new(cursor),
            Some(decode_hint),
            None,
            Default::default(),
            None,
        )?;

        let source =
            SamplePlayer::new(asset_server.add(Sample::new(DecodedAudioF32::from(decoded)))).looping();

        self.stop(commands);

        let entity = commands.spawn((source, pool)).id();
        self.playing = Some((name.to_string(), entity));

        Ok(())
    }

    /// Start playing the track with the given number.
    ///
    /// Note that the first actual music track is track 2; track 1 on the
    /// original Quake CD-ROM held the game data.
    pub fn play_track<Pool: Component + PoolLabel>(
        &mut self,
        asset_server: &AssetServer,
        commands: &mut Commands,
        vfs: &Vfs,
        pool: Pool,
        track_id: usize,
    ) -> Result<(), SoundError> {
        self.play_named(
            asset_server,
            commands,
            vfs,
            pool,
            format!("track{track_id:02}"),
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
            .and_then(|(_, e)| commands.get_entity(*e).ok())
        {
            entity.despawn();
        }
    }

    /// Pause the current music track.
    ///
    /// If no music track is currently playing, or if the current track is
    /// already paused, this has no effect.
    pub fn pause(&self, query: &mut Query<&mut SamplerNode>) {
        if let Some(mut sink) = self
            .playing
            .as_ref()
            .and_then(|(_, e)| query.get_mut(*e).ok())
        {
            sink.pause();
        }
    }

    /// Resume playback of the current music track.
    ///
    /// If no music track is currently playing, or if the current track is not
    /// paused, this has no effect.
    pub fn resume(&self, query: &mut Query<&mut SamplerNode>) {
        if let Some(mut sink) = self
            .playing
            .as_ref()
            .and_then(|(_, e)| query.get_mut(*e).ok())
        {
            sink.resume(None);
        }
    }
}
