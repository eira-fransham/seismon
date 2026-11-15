//! Console-style command execution for Bevy.
//!
//! This crate provides a [`Registry`] of named commands, cvars (configuration variables), aliases
//! and `+`/`-` actions, plus the systems that execute queued [`RunCmd`]s against it. Commands are
//! defined as systems taking a [`clap::Parser`] type as input, and are registered with
//! [`RegisterCmdExt`].
//!
//! Add [`CommandsPlugin`] to get started. The `help`, `reset` and `resetall` commands are
//! registered by default.
//!
//! # Opt-in messages
//!
//! - [`CommandOutput`]: if `Messages<CommandOutput>` is registered, command output is written
//!   there. Otherwise, it is logged.
//! - [`UnhandledCmd`]: if `Messages<UnhandledCmd>` is registered, commands that don't match
//!   anything in the [`Registry`] are forwarded there, and produce no output. Otherwise, they
//!   produce an "Unrecognized command" output.

mod builtins;
mod cmd;
mod cvar;
mod error;
mod exec;
pub mod parse;
mod register;
mod registry;

use bevy_app::{App, Plugin, PostUpdate};
use bevy_ecs::schedule::{IntoScheduleConfigs as _, ScheduleLabel, SystemSet};

pub use clap;
pub use serde_lexpr;

pub use self::{
    cmd::{CName, CmdName, RunCmd, Trigger, UnhandledCmd},
    cvar::Cvar,
    error::{ConsoleError, cvar_error_handler},
    exec::{
        CmdExt, CommandIncomplete, CommandOutput, ErasedBuiltinSystemReturn, ExecResult,
        FromWithWorld, IntoWithWorld, OutputType, PendingCommands, ResetCvar, SetCvar,
    },
    register::RegisterCmdExt,
    registry::{AliasInfo, CmdKind, CommandImpl, Registry},
};

pub mod prelude {
    #[doc(hidden)]
    pub use crate::{
        CName, CmdName, CommandOutput, CommandsPlugin, CommandsSystems, Cvar, ExecResult,
        OutputType, PendingCommands, RegisterCmdExt, Registry, RunCmd, Trigger, UnhandledCmd,
    };
}

/// Systems that execute pending commands and run cvar change handlers.
///
/// Order systems that consume [`CommandOutput`] or [`UnhandledCmd`] `.after(CommandsSystems)` to
/// see them in the same frame they were produced.
#[derive(SystemSet, Debug, Clone, PartialEq, Eq, Hash)]
pub struct CommandsSystems;

/// Adds the [`Registry`], [`PendingCommands`] and the [`RunCmd`] message, registers the `help`,
/// `reset` and `resetall` commands, and executes pending commands in schedule `T`.
///
/// Two messages are opt-in, and are only used if registered by the app:
///
/// - [`CommandOutput`]: receives command output. If not registered, output is logged instead.
/// - [`UnhandledCmd`]: receives commands that aren't in the [`Registry`]. If not registered, an
///   "Unrecognized command" output is produced instead.
#[derive(Default)]
pub struct CommandsPlugin<T = PostUpdate>(pub T)
where
    T: ScheduleLabel;

impl CommandsPlugin {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<T> Plugin for CommandsPlugin<T>
where
    T: ScheduleLabel + Clone,
{
    fn build(&self, app: &mut App) {
        app.init_resource::<Registry>()
            .init_resource::<PendingCommands>()
            .add_message::<RunCmd<'static>>()
            .add_systems(
                self.0.clone(),
                ((exec::populate_pending, exec::execute_console).chain(), exec::update_cvars)
                    .in_set(CommandsSystems),
            );

        builtins::register(app);
    }
}

#[cfg(test)]
mod tests {
    use bevy_ecs::message::Messages;

    use super::*;

    fn app() -> App {
        let mut app = App::new();
        app.add_plugins(CommandsPlugin::new());
        app
    }

    fn run(app: &mut App, cmd: &str) {
        let cmds = RunCmd::parse_many(cmd)
            .unwrap()
            .into_iter()
            .map(RunCmd::into_owned)
            .collect::<Vec<_>>();
        app.world_mut().resource_mut::<Messages<RunCmd<'static>>>().write_batch(cmds);
        app.update();
    }

    fn outputs(app: &mut App) -> Vec<String> {
        app.world_mut()
            .resource_mut::<Messages<CommandOutput>>()
            .drain()
            .map(|o| o.output.to_string())
            .collect()
    }

    #[test]
    fn output_is_routed_to_message() {
        let mut app = app();
        app.add_message::<CommandOutput>();
        app.cvar("sensitivity", "3", "Mouse sensitivity");

        run(&mut app, "sensitivity");
        assert_eq!(outputs(&mut app), ["\"sensitivity\" is \"3\""]);

        run(&mut app, "help sensitivity");
        assert_eq!(outputs(&mut app), ["sensitivity: Mouse sensitivity\n"]);
    }

    #[test]
    fn unknown_command_without_unhandled_message() {
        let mut app = app();
        app.add_message::<CommandOutput>();

        run(&mut app, "frobnicate 1");
        assert_eq!(outputs(&mut app), ["Unrecognized command \"frobnicate\""]);
    }

    #[test]
    fn unknown_command_with_unhandled_message() {
        let mut app = app();
        app.add_message::<CommandOutput>().add_message::<UnhandledCmd>();

        run(&mut app, "frobnicate 1");
        assert!(outputs(&mut app).is_empty());

        let unhandled =
            app.world_mut().resource_mut::<Messages<UnhandledCmd>>().drain().collect::<Vec<_>>();
        assert_eq!(unhandled, [UnhandledCmd(RunCmd("frobnicate".into(), ["1".to_owned()].into()))]);
    }

    #[test]
    fn set_and_reset_cvars() {
        let mut app = app();
        app.cvar("a", "1", "").cvar("b", "2", "");

        run(&mut app, "a 10");
        run(&mut app, "b 20");
        let registry = app.world().resource::<Registry>();
        assert_eq!(registry.read_cvar::<i32>("a").unwrap(), 10);
        assert_eq!(registry.read_cvar::<i32>("b").unwrap(), 20);

        run(&mut app, "reset a");
        let registry = app.world().resource::<Registry>();
        assert_eq!(registry.read_cvar::<i32>("a").unwrap(), 1);
        assert_eq!(registry.read_cvar::<i32>("b").unwrap(), 20);

        run(&mut app, "resetall");
        assert_eq!(app.world().resource::<Registry>().read_cvar::<i32>("b").unwrap(), 2);
    }

    #[test]
    fn actions() {
        let mut app = app();
        app.action("forward");

        assert!(!app.world().resource::<Registry>().is_pressed("forward"));
        run(&mut app, "+forward");
        assert!(app.world().resource::<Registry>().is_pressed("forward"));
        run(&mut app, "-forward");
        assert!(!app.world().resource::<Registry>().is_pressed("forward"));
    }
}
