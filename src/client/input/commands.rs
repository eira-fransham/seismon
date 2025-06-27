use crate::{client, common::console::RegisterCmdExt};

use bevy::prelude::*;
use clap::Parser;

use super::game::GameInput;

pub fn register_commands(app: &mut App) {
    #[derive(Parser)]
    #[command(name = "bind", about = "Attach a command to a key")]
    struct Bind {
        from: String,
        to: Option<String>,
    }

    app.command(
        |In(Bind { from, to }), mut game_input: ResMut<GameInput>| match to {
            None => match game_input.binding(&from[..]) {
                Ok(Some(t)) => format!("\"{}\" = \"{}\"", from.to_string(), t.to_string()).into(),
                _ => format!("\"{from}\" is not bound").into(),
            },
            // bind (key) [command]
            Some(to) => match game_input.bind(&from[..], &to[..]) {
                Ok(_) => {
                    debug!("Bound {:?} to {:?}", from, to);
                    default()
                }
                Err(e) => format!("Bind failed: {e}").into(),
            },
        },
    );

    #[derive(Parser)]
    #[command(name = "unbindall", about = "Delete all keybindings")]
    struct UnbindAll;

    // "unbindall"
    app.command(|In(UnbindAll), mut game_input: ResMut<GameInput>| {
        game_input.bindings = default();
        default()
    });

    #[derive(Parser)]
    #[command(name = "impulse", about = "Apply various effects depending on number")]
    /// Apply various effects depending on number:
    ///
    /// impulse 1-8: Select corresponding weapon
    /// impulse 9: All weapons at full ammo, plus both keys
    /// impulse 10: Select next weapon
    /// impulse 11: Receive a single rune, repeat 4 times to get all 4 runes
    /// impulse 12: Select previous weapon
    struct Impulse {
        number: u8,
    }

    // "impulse"
    // TODO: Add "extended help" for cases like this
    app.command(
        move |In(Impulse { number }), mut impulse: EventWriter<client::Impulse>| {
            impulse.write(client::Impulse(number));
            default()
        },
    );
}
