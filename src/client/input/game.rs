// Copyright © 2018 Cormac O'Brien
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software
// and associated documentation files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or
// substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
// BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

use std::{fmt::Display, hash::Hash, ops::Not, str::FromStr, sync::LazyLock};

use crate::common::{console::RunCmd, parse};

use bevy::{
    input::{keyboard::Key, prelude::*},
    prelude::*,
};
use bitflags::bitflags;
use failure::{Error, bail, format_err};
use hashbrown::HashMap;
use smol_str::SmolStr;
use strum_macros::EnumIter;
use winit::event::MouseButton;

#[derive(Debug, Copy, Clone, Eq)]
#[repr(transparent)]
struct UppercaseStr<'a>(&'a str);

impl Display for UppercaseStr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const BLOCKS: usize = 128;

        let mut buf = arrayvec::ArrayVec::<u8, BLOCKS>::new();

        let chars = self.0.as_bytes();

        for block in chars.chunks(BLOCKS) {
            buf.clear();
            buf.extend(block.iter().copied());
            (&mut *buf).make_ascii_uppercase();
            write!(f, "{}", std::str::from_utf8(&*buf).unwrap())?;
        }

        Ok(())
    }
}

impl Hash for UppercaseStr<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        const BLOCKS: usize = 128;

        let mut buf = arrayvec::ArrayVec::<u8, BLOCKS>::new();

        let chars = self.0.as_bytes();

        for block in chars.chunks(BLOCKS) {
            buf.clear();
            buf.extend(block.iter().copied());
            (&mut buf).make_ascii_uppercase();
            std::str::from_utf8(&buf).unwrap().hash(state);
        }
    }
}

impl PartialEq for UppercaseStr<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq_ignore_ascii_case(other.0)
    }
}

static KEYMAP: LazyLock<HashMap<UppercaseStr<'static>, AnyInput>> = LazyLock::new(|| {
    KEYBOARD_NAMES
        .iter()
        .chain(MOUSE_NAMES)
        .map(|(n, i)| (UppercaseStr(n), i.clone()))
        .collect()
});
static INVERSE_KEYMAP: LazyLock<HashMap<AnyInput, UppercaseStr<'static>>> = LazyLock::new(|| {
    KEYBOARD_NAMES
        .iter()
        .chain(MOUSE_NAMES)
        .map(|(n, i)| (i.clone(), UppercaseStr(n)))
        .collect()
});

macro_rules! buttons {
    (@inner ($name:literal, $val:literal) ($any:path, $tname:ident, $sname:path)) => {
        ($name, $any($sname(SmolStr::new_inline($val))))
    };
    (@inner ($name:literal, $val:ident) ($any:path, $tname:ident $(, $sname:path)?)) => {
        ($name, $any($tname::$val))
    };
    (@inner $name:literal ($any:path, $tname:ident, $sname:path)) => {
        buttons!(@inner ($name, $name) ($any, $tname, $sname))
    };
    () => {};
    (($any:path, $tname:ident) $($e:tt,)*) => {
        [$(buttons!(@inner $e ($any, $tname))),*]
    };
    (($any:path, $tname:ident, $sname:path) $($e:tt,)*) => {
        [$(buttons!(@inner $e ($any, $tname, $sname))),*]
    };
}

macro_rules! keys {
    ($($inner:tt)*) => {
        buttons!((AnyInput::Keyboard, Key, Key::Character) $($inner)*)
    }
}

macro_rules! mouse {
    ($($inner:tt)*) => {
        buttons!((AnyInput::Mouse, MouseButton) $($inner)*)
    }
}

const KEYBOARD_NAMES: &[(&str, AnyInput)] = &keys![
    ",",
    ".",
    "/",
    "0",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "A",
    ("ALT", Alt),
    "B",
    ("BACKSPACE", Backspace),
    "C",
    ("CTRL", Control),
    "D",
    ("DEL", Delete),
    ("DOWNARROW", ArrowDown),
    "E",
    ("END", End),
    ("ENTER", Enter),
    ("ESCAPE", Escape),
    ("PAUSE", Pause),
    "F",
    ("F1", F1),
    ("F10", F10),
    ("F11", F11),
    ("F12", F12),
    ("F2", F2),
    ("F3", F3),
    ("F4", F4),
    ("F5", F5),
    ("F6", F6),
    ("F7", F7),
    ("F8", F8),
    ("F9", F9),
    "G",
    "H",
    ("HOME", Home),
    "I",
    ("INS", Insert),
    "J",
    "K",
    "L",
    ("LEFTARROW", ArrowLeft),
    "M",
    "N",
    "O",
    "P",
    ("PGDN", PageDown),
    ("PGUP", PageUp),
    "Q",
    "R",
    ("RIGHTARROW", ArrowRight),
    "S",
    ("SEMICOLON", ";"),
    ("SHIFT", Shift),
    ("SPACE", " "),
    "T",
    ("TAB", Tab),
    "U",
    ("UPARROW", ArrowUp),
    "V",
    "W",
    "X",
    "Y",
    "Z",
    "[",
    "\\",
    "]",
    "`",
    "~",
    "+",
    "-",
    "=",
];

const MOUSE_NAMES: &[(&str, AnyInput)] = &mouse![
    ("MOUSE1", Left),
    ("MOUSE2", Right),
    ("MOUSE3", Middle),
    // TODO: "MWHEELDOWN"
    // TODO: "MWHEELUP"
];

/// A unique identifier for an in-game action.
#[derive(Clone, Copy, Debug, Eq, PartialEq, EnumIter)]
pub enum Action {
    /// Move forward.
    Forward = 0,
    /// Move backward.
    Back = 1,
    /// Strafe left.
    MoveLeft = 2,
    /// Strafe right.
    MoveRight = 3,
    /// Move up (when swimming).
    MoveUp = 4,
    /// Move down (when swimming).
    MoveDown = 5,
    /// Look up.
    LookUp = 6,
    /// Look down.
    LookDown = 7,
    /// Look left.
    Left = 8,
    /// Look right.
    Right = 9,
    /// Change move speed (walk/run).
    Speed = 10,
    /// Jump.
    Jump = 11,
    /// Interpret `Left`/`Right` like `MoveLeft`/`MoveRight`.
    Strafe = 12,
    /// Attack with the current weapon.
    Attack = 13,
    /// Interact with an object (not used).
    Use = 14,
    /// Interpret `Forward`/`Back` like `LookUp`/`LookDown`.
    KLook = 15,
    /// Interpret upward/downward vertical mouse movements like `LookUp`/`LookDown`.
    MLook = 16,
    /// If in single-player, show the current level stats. If in multiplayer, show the scoreboard.
    ShowScores = 17,
    /// Show the team scoreboard.
    ShowTeamScores = 18,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AnyInput {
    Mouse(MouseButton),
    Keyboard(Key),
    Gamepad(GamepadButton),
}

impl AnyInput {
    pub const ALT: Self = AnyInput::Keyboard(Key::Alt);
    pub const BACKSPACE: Self = AnyInput::Keyboard(Key::Backspace);
    pub const CTRL: Self = AnyInput::Keyboard(Key::Control);
    pub const DEL: Self = AnyInput::Keyboard(Key::Delete);
    pub const DOWNARROW: Self = AnyInput::Keyboard(Key::ArrowDown);
    pub const END: Self = AnyInput::Keyboard(Key::End);
    pub const ENTER: Self = AnyInput::Keyboard(Key::Enter);
    pub const PAUSE: Self = AnyInput::Keyboard(Key::Pause);
    pub const ESCAPE: Self = AnyInput::Keyboard(Key::Escape);
    pub const F1: Self = AnyInput::Keyboard(Key::F1);
    pub const F10: Self = AnyInput::Keyboard(Key::F10);
    pub const F11: Self = AnyInput::Keyboard(Key::F11);
    pub const F12: Self = AnyInput::Keyboard(Key::F12);
    pub const F2: Self = AnyInput::Keyboard(Key::F2);
    pub const F3: Self = AnyInput::Keyboard(Key::F3);
    pub const F4: Self = AnyInput::Keyboard(Key::F4);
    pub const F5: Self = AnyInput::Keyboard(Key::F5);
    pub const F6: Self = AnyInput::Keyboard(Key::F6);
    pub const F7: Self = AnyInput::Keyboard(Key::F7);
    pub const F8: Self = AnyInput::Keyboard(Key::F8);
    pub const F9: Self = AnyInput::Keyboard(Key::F9);
    pub const HOME: Self = AnyInput::Keyboard(Key::Home);
    pub const INS: Self = AnyInput::Keyboard(Key::Insert);
    pub const LEFTARROW: Self = AnyInput::Keyboard(Key::ArrowLeft);
    pub const PGDN: Self = AnyInput::Keyboard(Key::PageDown);
    pub const PGUP: Self = AnyInput::Keyboard(Key::PageUp);
    pub const RIGHTARROW: Self = AnyInput::Keyboard(Key::ArrowRight);
    pub const SHIFT: Self = AnyInput::Keyboard(Key::Shift);
    pub const TAB: Self = AnyInput::Keyboard(Key::Tab);
    pub const UPARROW: Self = AnyInput::Keyboard(Key::ArrowUp);

    pub fn char(char: &str) -> Self {
        Self::Keyboard(Key::Character(char.into()))
    }
}

impl From<Key> for AnyInput {
    fn from(mut value: Key) -> Self {
        // TODO: This means we allocate for every single input, unless the compiler can elide the
        //       allocation.
        if let Key::Character(k) = &mut value
            && k.chars().any(|c| c.is_ascii_lowercase())
        {
            *k = k.to_ascii_uppercase().into();
        }

        Self::Keyboard(value)
    }
}

impl From<MouseButton> for AnyInput {
    fn from(value: MouseButton) -> Self {
        Self::Mouse(value)
    }
}

impl FromStr for AnyInput {
    type Err = Error;

    fn from_str(src: &str) -> Result<Self, Error> {
        let Some(out) = KEYMAP.get(&UppercaseStr(src)) else {
            bail!("\"{}\" isn't a valid key", src);
        };

        Ok(out.clone())
    }
}

impl TryInto<AnyInput> for &'_ str {
    type Error = <AnyInput as FromStr>::Err;

    fn try_into(self) -> Result<AnyInput, Self::Error> {
        self.parse()
    }
}

impl Display for AnyInput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            INVERSE_KEYMAP.get(self).unwrap_or(&UppercaseStr("UNKNOWN"))
        )
    }
}

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

#[derive(Default, Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum BindingValidState {
    #[default]
    Game,
    Any,
}

impl Display for BindingValidState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if *self == BindingValidState::Any {
            write!(f, "*")?;
        }

        Ok(())
    }
}

impl Display for Trigger {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Trigger::Positive => write!(f, "+"),
            Trigger::Negative => write!(f, "-"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Binding<'a> {
    pub commands: Vec<RunCmd<'a>>,
    pub valid: BindingValidState,
}

impl Binding<'_> {
    pub fn into_owned(self) -> Binding<'static> {
        Binding {
            commands: self.commands.into_iter().map(RunCmd::into_owned).collect(),
            valid: self.valid,
        }
    }
}

impl FromStr for Binding<'static> {
    type Err = nom::Err<nom::error::Error<String>>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parse::console::binding(s) {
            Ok(("", val)) => Ok(val.into_owned().into()),
            Ok((rest, _)) => Err(nom::Err::Failure(nom::error::Error::new(
                rest.to_owned(),
                nom::error::ErrorKind::Verify,
            ))),
            Err(e) => Err(e.to_owned()),
        }
    }
}

impl Display for Binding<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.valid)?;

        let mut cmds = self.commands.iter();
        if let Some(first_cmd) = cmds.next() {
            write!(f, "{first_cmd}")?;

            for cmd in cmds {
                write!(f, "; {cmd}")?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Resource)]
pub struct GameInput {
    pub bindings: HashMap<AnyInput, Binding<'static>>,
    pub mouse_delta: (f64, f64),
}

impl Default for GameInput {
    fn default() -> Self {
        Self::new()
    }
}

impl GameInput {
    pub fn new() -> GameInput {
        let mut out = Self {
            bindings: default(),
            mouse_delta: default(),
        };

        out.bind_defaults();

        out
    }

    pub fn mouse_delta(&self) -> (f64, f64) {
        self.mouse_delta
    }

    /// Bind the default controls.
    pub fn bind_defaults(&mut self) {
        self.bind("W", "+forward").unwrap();
        self.bind("A", "+moveleft").unwrap();
        self.bind("S", "+back").unwrap();
        self.bind("D", "+moveright").unwrap();
        self.bind("SPACE", "+jump").unwrap();
        self.bind("UPARROW", "+lookup").unwrap();
        self.bind("LEFTARROW", "+left").unwrap();
        self.bind("DOWNARROW", "+lookdown").unwrap();
        self.bind("RIGHTARROW", "+right").unwrap();
        self.bind("CTRL", "+attack").unwrap();
        self.bind("E", "+use").unwrap();
        self.bind("`", "toggleconsole").unwrap();
        self.bind("ESCAPE", "togglemenu").unwrap();
        self.bind("1", "impulse 1").unwrap();
        self.bind("2", "impulse 2").unwrap();
        self.bind("3", "impulse 3").unwrap();
        self.bind("4", "impulse 4").unwrap();
        self.bind("5", "impulse 5").unwrap();
        self.bind("6", "impulse 6").unwrap();
        self.bind("7", "impulse 7").unwrap();
        self.bind("8", "impulse 8").unwrap();
        self.bind("9", "impulse 9").unwrap();
    }

    /// Bind a `BindInput` to a `BindTarget`.
    pub fn bind<I, T>(&mut self, input: I, target: T) -> Result<Option<Binding<'static>>, Error>
    where
        I: TryInto<AnyInput>,
        T: AsRef<str>,
        I::Error: Display,
    {
        let target: Binding = target
            .as_ref()
            .parse()
            .map_err(|e| format_err!("Failed to parse target: {}", e))?;
        let input = input
            .try_into()
            .map_err(|e| format_err!("Failed to parse input: {}", e))?;

        Ok(self.bindings.insert(input, target))
    }

    /// Return the `BindTarget` that `input` is bound to, or `None` if `input` is not present.
    pub fn binding<I>(&self, input: I) -> Result<Option<&Binding<'static>>, Error>
    where
        I: TryInto<AnyInput>,
        I::Error: Display,
    {
        Ok(self.bindings.get(
            &input
                .try_into()
                .map_err(|e| format_err!("Failed to parse input: {}", e))?,
        ))
    }
}
