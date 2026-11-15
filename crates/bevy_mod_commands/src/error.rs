use bevy_ecs::system::In;
use bevy_log::warn;
use serde::de::{Expected, Unexpected};
use serde_lexpr::Value;
use snafu::{Backtrace, prelude::*};

use crate::CName;

#[derive(Snafu, Debug)]
pub enum ConsoleError {
    #[snafu(display("{error}"))]
    CmdError { error: CName },
    #[snafu(display("Could not parse cvar: {name} = \"{value}\""))]
    CvarParseFailed { name: CName, value: Value },
    #[snafu(display("Could not parse cvar"), context(false))]
    CvarFieldParseFailed { source: serde::de::value::Error },
    #[snafu(display("Could not parse cvar"))]
    CvarParseInvalid { backtrace: Backtrace },
    #[snafu(display("No such command: {cmd}"))]
    NoSuchCommand { cmd: CName },
    #[snafu(display("No such alias: {name}"))]
    NoSuchAlias { name: CName },
    #[snafu(display("No such cvar: {name}"))]
    NoSuchCvar { name: CName },
}

impl serde::de::Error for ConsoleError {
    // Required method
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        Self::CmdError { error: msg.to_string().into() }
    }

    #[cold]
    fn invalid_type(unexp: Unexpected, exp: &dyn Expected) -> Self {
        serde::de::value::Error::invalid_type(unexp, exp).into()
    }

    #[cold]
    fn invalid_value(unexp: Unexpected, exp: &dyn Expected) -> Self {
        serde::de::value::Error::invalid_value(unexp, exp).into()
    }

    #[cold]
    fn invalid_length(len: usize, exp: &dyn Expected) -> Self {
        serde::de::value::Error::invalid_length(len, exp).into()
    }

    #[cold]
    fn unknown_variant(variant: &str, expected: &'static [&'static str]) -> Self {
        serde::de::value::Error::unknown_variant(variant, expected).into()
    }

    #[cold]
    fn unknown_field(field: &str, expected: &'static [&'static str]) -> Self {
        serde::de::value::Error::unknown_field(field, expected).into()
    }

    #[cold]
    fn missing_field(field: &'static str) -> Self {
        serde::de::value::Error::missing_field(field).into()
    }

    #[cold]
    fn duplicate_field(field: &'static str) -> Self {
        serde::de::value::Error::duplicate_field(field).into()
    }
}

/// A system that logs any error piped into it, for use with `.pipe(cvar_error_handler)`.
pub fn cvar_error_handler(In(result): In<Result<(), ConsoleError>>) {
    if let Err(err) = result {
        warn!("encountered an error {:?}", err);
    }
}
