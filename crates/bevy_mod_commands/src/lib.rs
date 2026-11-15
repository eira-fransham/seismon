//! Command-line support for actions and inputs, modeled after the console in engines such as
//! Goldsrc, Source and the idtech lineage.
//!
//! This crate provides a [`Registry`] of named commands, cvars (configuration variables), aliases
//! and `+`/`-` actions. Commands are defined as systems taking a [`clap::Parser`] type as input.
//! See [`RegisterCmdExt`] for more information about registering commands.
//!
//! Add [`CommandsPlugin`] to get started.

mod builtins;
mod cmd;
mod cvar;
mod error;
mod exec;
pub mod parse;
mod register;
mod registry;

use bevy_app::{App, Plugin, PostUpdate};
use bevy_ecs::schedule::{IntoScheduleConfigs as _, SystemSet};

pub use clap;
pub use serde_lexpr;

pub use self::{
    cmd::{CName, CmdName, RunCmd, Trigger, UnhandledCmd},
    cvar::Cvar,
    error::ConsoleError,
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

/// Adds [`Registry`], [`PendingCommands`] and the [`RunCmd`] message.
pub struct CommandsPlugin {
    /// If true, registers the [`CommandOutput`] message and sends command output using
    /// it. Otherwise, command output is logged using `info!`/`warn!`.
    ///
    /// Defaults to false.
    pub capture_out: bool,
    /// If true, registers the [`UnhandledCmd`] message. Attempting to execute an unknown
    /// command will result in that command invocation being sent using that message.
    ///
    /// Defaults to false.
    pub capture_unhandled: bool,
    /// If true, registers `help`, `reset` and `resetall` commands.
    ///
    /// Defaults to true.
    pub register_builtins: bool,
}

impl Default for CommandsPlugin {
    fn default() -> Self {
        Self { capture_out: false, capture_unhandled: false, register_builtins: true }
    }
}

impl CommandsPlugin {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Plugin for CommandsPlugin {
    fn build(&self, app: &mut App) {
        app.init_resource::<Registry>()
            .init_resource::<PendingCommands>()
            .add_message::<RunCmd<'static>>()
            .add_systems(
                // TODO: Should this be configurable?
                PostUpdate,
                ((exec::populate_pending, exec::execute_console).chain(), exec::update_cvars)
                    .in_set(CommandsSystems),
            );

        if self.capture_out {
            app.add_message::<CommandOutput>();
        }

        if self.capture_unhandled {
            app.add_message::<UnhandledCmd>();
        }

        if self.register_builtins {
            builtins::register(app);
        }
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

        run(&mut app, "some_unknown_cmd 1");
        assert_eq!(outputs(&mut app), ["Unrecognized command \"some_unknown_cmd\""]);
    }

    #[test]
    fn unknown_command_with_unhandled_message() {
        let mut app = app();
        app.add_message::<CommandOutput>().add_message::<UnhandledCmd>();

        run(&mut app, "some_unknown_cmd 1");
        assert!(outputs(&mut app).is_empty());

        let unhandled =
            app.world_mut().resource_mut::<Messages<UnhandledCmd>>().drain().collect::<Vec<_>>();
        assert_eq!(
            unhandled,
            [UnhandledCmd(RunCmd("some_unknown_cmd".into(), ["1".to_owned()].into()))]
        );
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
