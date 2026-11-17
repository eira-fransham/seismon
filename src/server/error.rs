//! Errors from running the server.

use bevy_mod_commands::ConsoleError;
use bevy_mod_scripting::bindings::InteropError;
use snafu::{Backtrace, prelude::*};

use crate::{
    common::net::NetError,
    server::{
        precache::PrecacheError,
        world::{EntityError, hull::HullError},
    },
};

pub type Result<T, E = ServerError> = std::result::Result<T, E>;

#[derive(Snafu, Debug)]
#[snafu(visibility(pub))]
pub enum ServerError {
    #[snafu(context(false), display("{source:?}"))]
    Io { source: ::std::io::Error, backtrace: Backtrace },
    #[snafu(context(false))]
    Net { source: NetError },
    #[snafu(context(false))]
    Console { source: ConsoleError, backtrace: Backtrace },
    #[snafu(context(false))]
    Entity { source: EntityError, backtrace: Backtrace },
    #[snafu(context(false))]
    Hull { source: HullError, backtrace: Backtrace },
    #[snafu(context(false))]
    Precache { source: PrecacheError, backtrace: Backtrace },
    /// An error running QuakeC (including errors raised by QuakeC itself with `error`).
    #[snafu(context(false), display("{source}"))]
    Qc { source: InteropError },
    /// An entity which doesn't exist (e.g. because it was removed) was accessed.
    #[snafu(display("no such entity: {entity}"))]
    NoSuchEntity { entity: bevy::ecs::entity::Entity, backtrace: Backtrace },
    #[snafu(display("{message}"))]
    Other { message: String, backtrace: Backtrace },
}

impl ServerError {
    pub fn with_msg<S>(msg: S) -> Self
    where
        S: Into<String>,
    {
        ServerError::Other { message: msg.into(), backtrace: Backtrace::capture() }
    }
}
