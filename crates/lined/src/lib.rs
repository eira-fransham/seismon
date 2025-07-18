#![deny(unused_must_use)]

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
