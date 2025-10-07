//! # `lined`
//!
//! A fork of [`redox_liner`](https://crates.io/crates/redox_liner) to allow embedding
//! the core editing functionality in non-TUI contexts, particularly in games.
//!
//! For an example of a real-world usecase, see
//! [its use in `seismon`](https://github.com/eira-fransham/seismon/blob/69c410f6149874e7ba58ee7c35070ee544db8c5f/src/common/console/mod.rs#L1822-L1904).
//!
//! The core of the library is the [`Editor`] type, see the documentation there for details.

#![deny(unused_must_use)]
#![warn(missing_docs)]

mod event;
pub use event::*;

mod editor;
pub use editor::*;

mod complete;
pub use complete::*;

mod context;
pub use context::*;

mod buffer;
pub use buffer::*;

mod history;
pub use history::*;

mod keymap;
pub use keymap::*;

pub use termion::event::Key;

mod util;

#[cfg(test)]
mod test;
