//! Calls into CSQC for events that it can handle instead of the engine: input, console commands,
//! and prints and damage from the server.
//!
//! Systems that see these events can't call QuakeC directly, so they queue the commands here when
//! CSQC is loaded. Each command calls CSQC, and does what the engine would have done if CSQC
//! doesn't handle the event (including if CSQC isn't loaded any more, or fails).

use std::ffi::CString;

use bevy::{
    ecs::message::{MessageCursor, MessageId},
    input::{
        ButtonState,
        keyboard::{Key, KeyboardInput},
        mouse::{MouseButton, MouseButtonInput, MouseMotion},
    },
    prelude::*,
};
use bevy_mod_scripting_qcvm::qcvm;
use hashbrown::HashSet;
use qcvm_defs_csqc::entry_points;
use seismon_utils::QString;

use super::{Csqc, call_entry};
use crate::{
    client::{
        console::string_cmd_message,
        input::game::{AnyInput, named_inputs},
        view,
    },
    common::console::ConsoleOutput,
};

/// `CSQC_InputEvent` event types (`IE_*`).
pub mod input_event {
    pub const KEY_DOWN: f32 = 0.;
    pub const KEY_UP: f32 = 1.;
    pub const MOUSE_DELTA: f32 = 2.;
    pub const MOUSE_ABS: f32 = 3.;
}

/// Whether CSQC has asked for a mouse cursor (`setcursormode`). While it has, the cursor is free
/// and mouse movement is passed to CSQC as the cursor's position (`IE_MOUSEABS`) rather than as
/// movement.
#[derive(Resource, Debug, Default, Clone, Copy, PartialEq)]
pub struct CsqcCursor {
    pub enabled: bool,
}

/// A multiplier for the mouse sensitivity that CSQC has set (`setsensitivityscaler`).
#[derive(Resource, Debug, Clone, Copy, PartialEq)]
pub struct CsqcSensitivity(pub f32);

impl Default for CsqcSensitivity {
    fn default() -> Self {
        Self(1.)
    }
}

/// A QuakeC string value.
pub fn qc_string(bytes: &[u8]) -> qcvm::Value {
    let bytes = bytes.iter().copied().filter(|b| *b != 0).collect::<Vec<_>>();
    qcvm::Value::String(CString::new(bytes).expect("NULs were removed").into())
}

fn qc_vector(v: Vec3) -> qcvm::Value {
    qcvm::Value::Vector(v.to_array().into())
}

/// Shows a centerprint from the server, unless `CSQC_Parse_CenterPrint` handles it.
pub struct CenterPrint(pub QString);

impl Command for CenterPrint {
    type Out = ();

    fn apply(self, world: &mut World) {
        let handled = call_entry::<f32>(
            world,
            entry_points::PARSE_CENTER_PRINT,
            vec![qc_string(&self.0.raw)],
        )
        .is_some_and(|handled| handled != 0.);
        if !handled {
            world.resource_scope(|world, mut output: Mut<ConsoleOutput>| {
                output.set_center_print(self.0, world.resource::<Time<Real>>());
            });
        }
    }
}

/// Shows a print from the server, unless CSQC defines `CSQC_Parse_Print` (in which case it is
/// responsible for showing prints).
pub struct Print(pub QString);

impl Command for Print {
    type Out = ();

    fn apply(self, world: &mut World) {
        // FTEQW's print levels; Quake's prints are all `PRINT_HIGH`.
        const PRINT_HIGH: f32 = 2.;

        let handled = call_entry::<()>(
            world,
            entry_points::PARSE_PRINT,
            vec![qc_string(&self.0.raw), qcvm::Value::Float(PRINT_HIGH)],
        )
        .is_some();
        if !handled {
            world.resource_scope(|world, mut output: Mut<ConsoleOutput>| {
                output.print_alert(self.0.raw, world.resource::<Time<Real>>());
            });
        }
    }
}

/// Shows the effects of the player taking damage, unless `CSQC_Parse_Damage` handles it.
pub struct Damage {
    pub armor: u8,
    pub blood: u8,
    pub source: Vec3,
}

impl Command for Damage {
    type Out = ();

    fn apply(self, world: &mut World) {
        let Self { armor, blood, source } = self;
        let handled = call_entry::<f32>(
            world,
            entry_points::PARSE_DAMAGE,
            vec![
                qcvm::Value::Float(armor as f32),
                qcvm::Value::Float(blood as f32),
                qc_vector(source),
            ],
        )
        .is_some_and(|handled| handled != 0.);
        if !handled
            && let Err(e) =
                world.run_system_cached_with(view::handle_damage, (armor, blood, source))
        {
            error!("Failed to handle damage: {e}");
        }
    }
}

/// The console commands that CSQC has registered with `registercommand`, which are passed to
/// `CSQC_ConsoleCommand` rather than the server.
#[derive(Resource, Debug, Default)]
pub struct CsqcCommands(pub HashSet<String>);

/// Runs a console command registered by CSQC, sending it to the server if CSQC doesn't handle it.
pub struct ConsoleCommand(pub String);

impl Command for ConsoleCommand {
    type Out = ();

    fn apply(self, world: &mut World) {
        let handled = call_entry::<f32>(
            world,
            entry_points::CONSOLE_COMMAND,
            vec![qc_string(self.0.as_bytes())],
        )
        .is_some_and(|handled| handled != 0.);
        if !handled {
            world.write_message(string_cmd_message(&self.0));
        }
    }
}

/// Input events that CSQC handled this frame, which the engine ignores.
#[derive(Resource, Debug, Default)]
pub struct ConsumedInput {
    pub keys: HashSet<MessageId<KeyboardInput>>,
    pub buttons: HashSet<MessageId<MouseButtonInput>>,
    pub mouse_motion: bool,
}

/// What [`csqc_input_events`] has read.
#[derive(Resource, Default)]
pub struct CsqcInputCursors {
    keys: MessageCursor<KeyboardInput>,
    buttons: MessageCursor<MouseButtonInput>,
    motion: MessageCursor<MouseMotion>,
}

/// Passes input events to `CSQC_InputEvent`, recording the ones that it handles in
/// [`ConsumedInput`]. This runs before the engine handles input in the game.
pub fn csqc_input_events(world: &mut World) {
    world.resource_mut::<ConsumedInput>().keys.clear();
    world.resource_mut::<ConsumedInput>().buttons.clear();
    world.resource_mut::<ConsumedInput>().mouse_motion = false;

    let (keys, buttons, motion) =
        world.resource_scope(|world, mut cursors: Mut<CsqcInputCursors>| {
            let pressed = world.resource::<ButtonInput<KeyCode>>();
            let keys = cursors
                .keys
                .read_with_id(world.resource::<Messages<KeyboardInput>>())
                // Ignore key-repeat events, as the engine does.
                .filter(|(key, _)| match key.state {
                    ButtonState::Pressed => pressed.just_pressed(key.key_code),
                    ButtonState::Released => pressed.just_released(key.key_code),
                })
                .map(|(key, id)| (key.clone(), id))
                .collect::<Vec<_>>();
            let buttons = cursors
                .buttons
                .read_with_id(world.resource::<Messages<MouseButtonInput>>())
                .map(|(button, id)| (*button, id))
                .collect::<Vec<_>>();
            let motion: Vec2 = cursors
                .motion
                .read(world.resource::<Messages<MouseMotion>>())
                .map(|m| m.delta)
                .sum();
            (keys, buttons, motion)
        });

    if !world.contains_resource::<Csqc>() {
        return;
    }

    let input_event = |world: &mut World, ty: f32, scan: f32, chr: f32| {
        call_entry::<f32>(
            world,
            entry_points::INPUT_EVENT,
            vec![
                qcvm::Value::Float(ty),
                qcvm::Value::Float(scan),
                qcvm::Value::Float(chr),
                qcvm::Value::Float(0.),
            ],
        )
        .is_some_and(|handled| handled != 0.)
    };
    let event_type = |state: ButtonState| match state {
        ButtonState::Pressed => input_event::KEY_DOWN,
        ButtonState::Released => input_event::KEY_UP,
    };

    for (key, id) in keys {
        let Some(keynum) = key_number(&key.logical_key) else {
            continue;
        };
        let chr = match (&key.logical_key, key.state) {
            (Key::Character(s), ButtonState::Pressed) => {
                s.chars().next().map_or(0., |c| c as u32 as f32)
            }
            _ => 0.,
        };
        if input_event(world, event_type(key.state), keynum as f32, chr) {
            world.resource_mut::<ConsumedInput>().keys.insert(id);
        }
    }

    for (button, id) in buttons {
        let Some(keynum) = mouse_button_number(button.button) else {
            continue;
        };
        if input_event(world, event_type(button.state), keynum as f32, 0.) {
            world.resource_mut::<ConsumedInput>().buttons.insert(id);
        }
    }

    let cursor_enabled = world.get_resource::<CsqcCursor>().is_some_and(|c| c.enabled);
    if cursor_enabled {
        // The cursor's position in virtual pixels, as CSQC draws in.
        let mut windows = world.query_filtered::<&Window, With<bevy::window::PrimaryWindow>>();
        let position = windows.iter(world).next().and_then(Window::cursor_position);
        if motion != Vec2::ZERO
            && let Some(position) = position
        {
            let position = position / crate::client::ui::UI_SCALE;
            if input_event(world, input_event::MOUSE_ABS, position.x, position.y) {
                world.resource_mut::<ConsumedInput>().mouse_motion = true;
            }
        }
    } else if motion != Vec2::ZERO
        && input_event(world, input_event::MOUSE_DELTA, motion.x, motion.y)
    {
        world.resource_mut::<ConsumedInput>().mouse_motion = true;
    }
}

/// Quake's key number for a key (as in `keys.h`), as passed to `CSQC_InputEvent`.
pub fn key_number(key: &Key) -> Option<u32> {
    Some(match key {
        Key::Tab => 9,
        Key::Enter => 13,
        Key::Escape => 27,
        Key::Space => 32,
        Key::Backspace => 127,
        Key::ArrowUp => 128,
        Key::ArrowDown => 129,
        Key::ArrowLeft => 130,
        Key::ArrowRight => 131,
        Key::Alt => 132,
        Key::Control => 133,
        Key::Shift => 134,
        Key::F1 => 135,
        Key::F2 => 136,
        Key::F3 => 137,
        Key::F4 => 138,
        Key::F5 => 139,
        Key::F6 => 140,
        Key::F7 => 141,
        Key::F8 => 142,
        Key::F9 => 143,
        Key::F10 => 144,
        Key::F11 => 145,
        Key::F12 => 146,
        Key::Insert => 147,
        Key::Delete => 148,
        Key::PageDown => 149,
        Key::PageUp => 150,
        Key::Home => 151,
        Key::End => 152,
        Key::Pause => 255,
        Key::Character(s) => {
            let mut chars = s.chars();
            let c = chars.next()?.to_ascii_lowercase();
            if chars.next().is_some() || !c.is_ascii() {
                return None;
            }
            c as u32
        }
        _ => return None,
    })
}

/// Quake's key number for a mouse button (`K_MOUSE1` etc.).
pub fn mouse_button_number(button: MouseButton) -> Option<u32> {
    Some(match button {
        MouseButton::Left => 512,
        MouseButton::Right => 513,
        MouseButton::Middle => 514,
        _ => return None,
    })
}

/// Quake's key number for an input that can be bound.
pub fn input_number(input: &AnyInput) -> Option<u32> {
    match input {
        AnyInput::Keyboard(key) => key_number(key),
        AnyInput::Mouse(button) => mouse_button_number(*button),
        _ => None,
    }
}

/// The input with Quake's key number `number`.
pub fn number_input(number: u32) -> Option<AnyInput> {
    named_inputs().map(|(_, input)| input).find(|input| input_number(input) == Some(number))
}

/// The name of the key with Quake's key number `number`, as `keynumtostring` gives it: the
/// character for printable keys, and the binding name otherwise.
pub fn key_name(number: u32) -> Option<String> {
    let (name, _) = named_inputs().find(|(_, input)| input_number(input) == Some(number))?;
    Some(if name.len() == 1 { name.to_ascii_lowercase() } else { name.to_owned() })
}
