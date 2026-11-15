use std::{fmt, ops::Not, str::FromStr};

use beef::Cow;
use bevy_ecs::message::Message;

use crate::parse;

/// The name of a command, cvar, alias or action.
pub type CName = Cow<'static, str>;

/// Whether to trigger an action on pressing or releasing a key
#[derive(Default, Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Trigger {
    /// "Positive edge" - trigger on press
    #[default]
    Positive,
    /// "Negative edge" - trigger on release
    Negative,
}

impl Not for Trigger {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Positive => Self::Negative,
            Self::Negative => Self::Positive,
        }
    }
}

impl fmt::Display for Trigger {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Trigger::Positive => write!(f, "+"),
            Trigger::Negative => write!(f, "-"),
        }
    }
}

/// A command name, optionally prefixed with `+` or `-` to invoke an action.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CmdName<'a> {
    pub trigger: Option<Trigger>,
    pub name: Cow<'a, str>,
}

impl CmdName<'_> {
    pub fn into_owned(self) -> CmdName<'static> {
        let CmdName { trigger, name } = self;

        CmdName { name: name.into_owned().into(), trigger }
    }
}

impl FromStr for CmdName<'static> {
    type Err = nom::Err<nom::error::Error<String>>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse::command_name(s) {
            Ok((rest, val)) if rest.chars().all(|c| c.is_ascii_whitespace()) => {
                Ok(val.into_owned())
            }
            Ok((rest, _)) => Err(nom::Err::Failure(nom::error::Error::new(
                rest.to_owned(),
                nom::error::ErrorKind::Verify,
            ))),
            Err(e) => Err(e.to_owned()),
        }
    }
}

impl From<&'static str> for CmdName<'static> {
    fn from(s: &'static str) -> Self {
        Self { trigger: None, name: s.into() }
    }
}

impl From<String> for CmdName<'static> {
    fn from(s: String) -> Self {
        Self { trigger: None, name: s.into() }
    }
}

impl fmt::Display for CmdName<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(trigger) = &self.trigger {
            write!(f, "{}{}", trigger, self.name)
        } else {
            write!(f, "{}", self.name)
        }
    }
}

/// A command that didn't match anything in the [`Registry`](crate::Registry).
///
/// This message is opt-in: if `Messages<UnhandledCmd>` is registered (with
/// `app.add_message::<UnhandledCmd>()`), unknown commands are forwarded here and no output is
/// produced for them. Otherwise, an "Unrecognized command" [`CommandOutput`](crate::CommandOutput)
/// is emitted instead.
#[derive(Message, PartialEq, Eq, Clone, Debug)]
#[repr(transparent)]
pub struct UnhandledCmd(pub RunCmd<'static>);

/// A command invocation: a name and its arguments.
///
/// Write this as a message to queue a command for execution.
#[derive(Message, PartialEq, Eq, Clone, Debug)]
pub struct RunCmd<'a>(pub CmdName<'a>, pub Box<[String]>);

impl<'a> RunCmd<'a> {
    pub fn into_owned(self) -> RunCmd<'static> {
        let RunCmd(name, args) = self;
        RunCmd(name.into_owned(), args)
    }

    pub fn parse(s: &'a str) -> Result<Self, <RunCmd<'static> as FromStr>::Err> {
        match parse::command(s) {
            Ok((rest, val)) if rest.chars().all(|c| c.is_ascii_whitespace()) => Ok(val),
            Ok((rest, _)) => Err(nom::Err::Failure(nom::error::Error::new(
                rest.to_owned(),
                nom::error::ErrorKind::Verify,
            ))),
            Err(e) => Err(e.to_owned()),
        }
    }

    pub fn parse_many(s: &'a str) -> Result<Vec<Self>, nom::Err<nom::error::Error<&'a str>>> {
        parse::commands(s).map(|(_, cmds)| cmds)
    }

    pub fn invert(self) -> Option<Self> {
        self.0.trigger.map(|t| RunCmd(CmdName { trigger: Some(!t), name: self.0.name }, self.1))
    }
}

impl fmt::Display for RunCmd<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)?;

        for arg in self.1.iter() {
            // TODO: This doesn't work if the value is a string that requires quotes - use
            // `lexpr::Value`?
            write!(f, " {arg:?}")?;
        }

        Ok(())
    }
}

impl FromStr for RunCmd<'static> {
    type Err = nom::Err<nom::error::Error<String>>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        RunCmd::parse(s).map(RunCmd::into_owned)
    }
}

impl From<&'static str> for RunCmd<'static> {
    fn from(s: &'static str) -> Self {
        Self(s.into(), Default::default())
    }
}

impl From<String> for RunCmd<'static> {
    fn from(s: String) -> Self {
        Self(s.into(), Default::default())
    }
}
