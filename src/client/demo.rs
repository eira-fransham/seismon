use std::{ops::Range, sync::Arc};

use crate::common::net::{self, NetError};

use arrayvec::ArrayVec;
use bevy::{
    asset::{Asset, AssetLoadError, AssetLoader, Assets, AsyncReadExt, Handle},
    log::warn,
    reflect::Reflect,
    tasks::ConditionalSendFuture,
};
use futures_byteorder::{AsyncReadBytes, LittleEndian};
use seismon_utils::{QAngles, read_f32_3_async};
use thiserror::Error;

/// An error returned by a demo server.
#[derive(Error, Debug)]
pub enum DemoError {
    #[error("Invalid CD track number")]
    InvalidCdTrack,
    #[error("No such CD track: {0}")]
    NoSuchCdTrack(i32),
    #[error("Message size ({0}) exceeds maximum allowed size {}", net::MAX_MESSAGE)]
    MessageTooLong(u32),
    #[error("Load error: {0}")]
    Load(#[from] Arc<AssetLoadError>),
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("Network error: {0}")]
    Net(#[from] NetError),
}

#[derive(Clone, Reflect)]
struct DemoMessage {
    view_angles: QAngles,
    msg_range: Range<usize>,
}

/// A view of a server message from a demo.
#[derive(Debug, Copy, Clone)]
pub struct DemoMessageView<'a> {
    pub view_angles: QAngles,
    pub message: &'a [u8],
    pub track_override: Option<u32>,
}

pub struct DemoServer {
    demo: Handle<Demo>,
    message_id: usize,
}

impl From<Handle<Demo>> for DemoServer {
    fn from(value: Handle<Demo>) -> Self {
        Self::new(value)
    }
}

impl DemoServer {
    pub fn new(demo: Handle<Demo>) -> Self {
        Self { demo, message_id: 0 }
    }

    pub fn demo(&self) -> &Handle<Demo> {
        &self.demo
    }

    /// Retrieve the next server message from the currently playing demo.
    ///
    /// If this returns `None`, the demo is complete.
    pub fn next_msg<'a>(&mut self, demos: &'a Assets<Demo>) -> Option<DemoMessageView<'a>> {
        let demo = demos.get(&self.demo)?;
        let msg = &demo.messages.get(self.message_id)?;
        self.message_id += 1;

        Some(DemoMessageView {
            view_angles: msg.view_angles,
            track_override: demo.track_override,
            message: &demo.message_data.get(msg.msg_range.clone())?,
        })
    }
}

/// A server that yields commands from a demo file.
#[derive(Asset, Clone, Reflect)]
pub struct Demo {
    track_override: Option<u32>,

    messages: Vec<DemoMessage>,

    // all message data
    message_data: Vec<u8>,
}

impl Demo {
    /// Construct a new `DemoServer` from the specified demo file.
    pub async fn new(mut reader: &mut dyn bevy::asset::io::Reader) -> Result<Demo, DemoError> {
        let mut reader = AsyncReadBytes::new(&mut reader);
        let mut buf = ArrayVec::<u8, 3>::new();

        // copy CD track number (terminated by newline) into buffer
        for i in 0..buf.capacity() {
            match reader.read_u8().await? {
                b'\n' => break,
                // cannot panic because we won't exceed capacity with a loop this small
                b => buf.push(b),
            }

            if i >= buf.capacity() - 1 {
                // CD track would be more than 2 digits long, which is impossible
                warn!("{}", DemoError::InvalidCdTrack);
            }
        }

        let track_override = {
            let track_str = match std::str::from_utf8(&buf) {
                Ok(s) => Some(s),
                Err(_) => {
                    warn!("{}", DemoError::InvalidCdTrack);
                    None
                }
            };

            match track_str {
                // if track is empty, default to track 0
                Some("") => Some(0),
                Some(s) => match s.parse::<i32>() {
                    Ok(track) => match track {
                        // if track is -1, allow demo to specify tracks in messages
                        -1 => None,
                        t if t < -1 => {
                            warn!("{}", DemoError::InvalidCdTrack);
                            None
                        }
                        _ => Some(track as u32),
                    },
                    Err(_) => {
                        warn!("{}", DemoError::InvalidCdTrack);
                        None
                    }
                },
                None => None,
            }
        };

        let mut message_data = Vec::new();
        let mut messages = Vec::new();

        // read all messages
        while let Ok(msg_len) = reader.read_u32::<LittleEndian>().await {
            // get view angles
            let view_angles: QAngles = read_f32_3_async(&mut reader).await?.into();

            message_data.reserve(msg_len as usize);

            // read next message
            let msg_start = message_data.len();
            reader.take(msg_len as _).read_to_end(&mut message_data).await?;
            let msg_end = message_data.len();

            messages.push(DemoMessage { view_angles, msg_range: msg_start..msg_end });
        }

        Ok(Demo { track_override, messages, message_data })
    }

    /// Returns the currently playing demo's music track override, if any.
    ///
    /// If this is `Some`, any `CdTrack` commands from the demo server should
    /// cause the client to play this track instead of the one specified by the
    /// command.
    pub fn track_override(&self) -> Option<u32> {
        self.track_override
    }
}

#[non_exhaustive]
#[derive(Default, Debug, Reflect)]
pub struct DemoLoader {}

impl AssetLoader for DemoLoader {
    type Asset = Demo;
    type Settings = ();
    type Error = DemoError;

    fn load(
        &self,
        reader: &mut dyn bevy::asset::io::Reader,
        _settings: &Self::Settings,
        _load_context: &mut bevy::asset::LoadContext,
    ) -> impl ConditionalSendFuture<Output = Result<Self::Asset, Self::Error>> {
        Demo::new(reader)
    }

    fn extensions(&self) -> &[&str] {
        &["dem"]
    }
}
