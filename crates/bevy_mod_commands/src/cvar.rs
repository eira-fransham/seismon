use std::str::FromStr;

use serde_lexpr::Value;

use crate::CName;

/// A configuration variable.
#[derive(Debug, Clone)]
pub struct Cvar {
    /// The current value of this variable, or `None` if it has never been set (in which case
    /// [`Cvar::default`](#structfield.default) is used).
    pub value: Option<Value>,

    /// Marks this variable as one that should be persisted.
    ///
    /// This crate doesn't act on the flag itself; consumers can read it to decide which cvars to
    /// save and restore.
    pub archive: bool,

    /// Marks this variable as one whose changes should be propagated.
    ///
    /// This crate doesn't act on the flag itself; consumers can read it to decide which cvar
    /// changes to announce or send elsewhere.
    pub notify: bool,

    /// The default value of this variable
    pub default: Value,
}

impl Default for Cvar {
    fn default() -> Self {
        Self {
            value: Default::default(),
            archive: Default::default(),
            notify: Default::default(),
            default: Value::Nil,
        }
    }
}

impl From<&'static str> for Cvar {
    fn from(value: &'static str) -> Self {
        Self::new(value)
    }
}

impl Cvar {
    pub fn new<D: Into<CName>>(default: D) -> Self {
        Self {
            // TODO: Error handling
            default: Value::from_str(default.into().as_ref()).unwrap(),
            ..Default::default()
        }
    }

    /// Set the [`archive`](#structfield.archive) flag.
    pub fn archive(mut self) -> Self {
        self.archive = true;

        self
    }

    /// Set the [`notify`](#structfield.notify) flag.
    pub fn notify(mut self) -> Self {
        self.notify = true;

        self
    }

    pub fn value(&self) -> &Value {
        self.value.as_ref().unwrap_or(&self.default)
    }
}
