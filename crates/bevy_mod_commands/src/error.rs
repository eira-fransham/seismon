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

    fn invalid_type(unexp: Unexpected, exp: &dyn Expected) -> Self {
        serde::de::value::Error::invalid_type(unexp, exp).into()
    }

    fn invalid_value(unexp: Unexpected, exp: &dyn Expected) -> Self {
        serde::de::value::Error::invalid_value(unexp, exp).into()
    }

    fn invalid_length(len: usize, exp: &dyn Expected) -> Self {
        serde::de::value::Error::invalid_length(len, exp).into()
    }

    fn unknown_variant(variant: &str, expected: &'static [&'static str]) -> Self {
        serde::de::value::Error::unknown_variant(variant, expected).into()
    }

    fn unknown_field(field: &str, expected: &'static [&'static str]) -> Self {
        serde::de::value::Error::unknown_field(field, expected).into()
    }

    fn missing_field(field: &'static str) -> Self {
        serde::de::value::Error::missing_field(field).into()
    }

    fn duplicate_field(field: &'static str) -> Self {
        serde::de::value::Error::duplicate_field(field).into()
    }
}
