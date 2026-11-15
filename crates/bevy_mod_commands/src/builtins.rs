//! Commands registered by default by [`CommandsPlugin`](crate::CommandsPlugin).

use std::{fmt::Write as _, iter};

use bevy_app::App;
use bevy_ecs::system::{In, Res, ResMut};
use clap::Parser;

use crate::{CommandImpl, ExecResult, RegisterCmdExt as _, Registry};

#[derive(Parser)]
#[command(name = "help", about = "Show help text for a command or cvar")]
struct Help {
    #[arg(value_name = "COMMAND")]
    arg_name: Option<String>,
}

#[derive(Parser)]
#[command(name = "reset", about = "Reset a cvar to its initial value")]
struct Reset {
    #[arg(required = true)]
    cvars: Vec<String>,
}

#[derive(Parser)]
#[command(name = "resetall", about = "Reset all cvars to their initial values")]
struct ResetAll;

pub(crate) fn register(app: &mut App) {
    app.command(|In(Help { arg_name }), registry: Res<Registry>| -> ExecResult {
        let args =
            arg_name.map(|arg| itertools::Either::Left(iter::once(arg))).unwrap_or_else(|| {
                itertools::Either::Right(registry.all_names().map(ToString::to_string))
            });

        let mut out = String::new();

        for arg in args {
            let Some(CommandImpl { help, .. }) = registry.get(&arg) else {
                out.push_str("Unknown command: ");
                out.push_str(&arg);
                out.push('\n');
                continue;
            };

            out.push_str(&arg);
            out.push_str(": ");
            out.push_str(help);
            out.push('\n');
        }

        out.into()
    })
    .command(|In(Reset { cvars }), mut registry: ResMut<Registry>| -> ExecResult {
        let mut out = String::new();

        for arg in cvars {
            if let Err(e) = registry.reset_cvar(arg) {
                writeln!(&mut out, "{e}").unwrap();
            }
        }

        out.into()
    })
    .command(|In(ResetAll), mut registry: ResMut<Registry>| -> ExecResult {
        let all_cvars = registry.cvar_names().map(ToString::to_string).collect::<Vec<_>>();
        for arg in all_cvars {
            registry.reset_cvar(arg).unwrap();
        }

        Default::default()
    });
}
