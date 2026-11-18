//! Network errors.

use bevy::prelude::*;
use snafu::{Backtrace, prelude::*};

#[derive(Snafu, Debug)]
pub enum NetError {
    #[snafu(context(false))]
    Io { source: ::std::io::Error, backtrace: Backtrace },
    #[snafu(display("Invalid data: {msg}"))]
    InvalidData { msg: String, backtrace: Backtrace },
    #[snafu(display("{msg}"))]
    Other { msg: String, backtrace: Backtrace },
}

impl NetError {
    pub fn with_msg<S>(msg: S) -> Self
    where
        S: Into<String>,
    {
        NetError::Other { msg: msg.into(), backtrace: Backtrace::capture() }
    }

    pub fn invalid_data<S>(msg: S) -> Self
    where
        S: Into<String>,
    {
        NetError::Other { msg: msg.into(), backtrace: Backtrace::capture() }
    }
}
