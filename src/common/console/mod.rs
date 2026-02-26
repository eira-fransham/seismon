use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::{self, Write},
    io, iter,
    marker::PhantomData,
    mem,
    str::FromStr,
    time::Duration,
};

use beef::Cow;
use bevy::{
    ecs::{prelude::Command, resource::Resource, system::SystemId, world::World},
    prelude::*,
};
use clap::{FromArgMatches, Parser};
use hashbrown::{HashMap, hash_map::Entry};
use lined::{
    DefaultWordDivider, Editor, EditorContext, Emacs, Key, KeyBindings, KeyMap as _, Prompt, Tty,
    WordDivider,
};
use seismon_utils::{QStr, QString};
use serde::{
    Deserializer,
    de::{Error, Expected, MapAccess, Unexpected, value::StrDeserializer},
};
use serde_lexpr::Value;
use snafu::{Backtrace, prelude::*};

use crate::client::{
    Disconnect, NewConnection,
    input::{InputFocus, game::Trigger},
};

use super::{parse, vfs::Vfs};

pub struct SeismonConsoleCorePlugin;

impl Plugin for SeismonConsoleCorePlugin {
    fn build(&self, app: &mut App) {
        #[derive(Parser)]
        #[command(name = "stuffcmds", about = "Run the commands from the CLI input arguments")]
        struct StuffCmds;

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

        app.init_resource::<Registry>()
            .add_message::<RunCmd<'static>>()
            .command(|In(StuffCmds), mut input: ResMut<ConsoleInput>| -> ExecResult {
                ExecResult {
                    extra_commands: Box::new(mem::take(&mut input.stuffcmds).into_iter()),
                    ..default()
                }
            })
            .command(|In(Help { arg_name }), registry: Res<Registry>| -> ExecResult {
                let args =
                    arg_name.map(|arg| itertools::Either::Left(iter::once(arg))).unwrap_or_else(
                        || itertools::Either::Right(registry.all_names().map(ToString::to_string)),
                    );

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

                default()
            })
            .add_systems(PostUpdate, (systems::execute_console, systems::update_cvars));
    }
}

pub struct SeismonClientConsolePlugin;

impl Plugin for SeismonClientConsolePlugin {
    fn build(&self, app: &mut App) {
        let vfs = app.world().resource::<Vfs>();

        let mut history = lined::History::default();

        // TODO: Remove `Vfs`
        if let Ok(history_path) = vfs.find_writable_filename("history.cfg") {
            match history.set_file_name_and_load_history(history_path) {
                Ok(_) => history.inc_append = true,
                Err(e) => {
                    warn!(target: "console", "Error loading history: {}", e);
                    history = lined::History::default();
                }
            }
        }

        fn resize_small(mut console_ui: Query<&mut Node, With<ConsoleUi>>) {
            for mut style in &mut console_ui {
                style.height = Val::Percent(30.);
            }
        }

        fn resize_large(mut console_ui: Query<&mut Node, With<ConsoleUi>>) {
            for mut style in &mut console_ui {
                style.height = Val::Percent(100.);
            }
        }

        app.add_plugins(SeismonConsoleCorePlugin)
            .add_message::<UnhandledCmd>()
            .init_resource::<ConsoleOutput>()
            .insert_resource(ConsoleInput::new(history).unwrap())
            .init_resource::<RenderConsoleOutput>()
            .init_resource::<RenderConsoleInput>()
            .init_resource::<ConsoleAlertSettings>()
            .add_systems(
                Startup,
                (systems::startup::init_alert_output, systems::startup::init_console),
            )
            .add_systems(
                PreUpdate,
                (
                    resize_large.run_if(on_message::<Disconnect>),
                    resize_small.run_if(on_message::<NewConnection>),
                ),
            )
            .add_systems(
                Update,
                (
                    systems::update_render_console,
                    systems::write_alert,
                    (systems::write_console_out, systems::write_center_print)
                        .run_if(resource_changed::<RenderConsoleOutput>),
                    systems::write_console_in.run_if(resource_changed::<RenderConsoleInput>),
                    systems::update_console_visibility.run_if(state_changed::<InputFocus>),
                ),
            )
            .add_systems(PostUpdate, systems::send_unhandled_commands_to_server);
    }
}

pub type CName = Cow<'static, str>;

#[derive(Snafu, Debug)]
pub enum ConsoleError {
    #[snafu(display("{error}"))]
    CmdError { error: CName },
    #[snafu(display("Could not parse cvar: {name} = \"{value}\""))]
    CvarParseFailed { name: CName, value: Value },
    #[snafu(display("Could not parse cvar"), context(false))]
    CvarFieldParseFailed { source: serde::de::value::Error },
    #[snafu(display("Could not parse cvar"))]
    CvarParseInvalid { backtrace: Backtrace },
    #[snafu(display("No such command: {cmd}"))]
    NoSuchCommand { cmd: CName },
    #[snafu(display("No such alias: {name}"))]
    NoSuchAlias { name: CName },
    #[snafu(display("No such cvar: {name}"))]
    NoSuchCvar { name: CName },
}

impl serde::de::Error for ConsoleError {
    // Required method
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        Self::CmdError { error: msg.to_string().into() }
    }

    #[cold]
    fn invalid_type(unexp: Unexpected, exp: &dyn Expected) -> Self {
        serde::de::value::Error::invalid_type(unexp, exp).into()
    }

    #[cold]
    fn invalid_value(unexp: Unexpected, exp: &dyn Expected) -> Self {
        serde::de::value::Error::invalid_value(unexp, exp).into()
    }

    #[cold]
    fn invalid_length(len: usize, exp: &dyn Expected) -> Self {
        serde::de::value::Error::invalid_length(len, exp).into()
    }

    #[cold]
    fn unknown_variant(variant: &str, expected: &'static [&'static str]) -> Self {
        serde::de::value::Error::unknown_variant(variant, expected).into()
    }

    #[cold]
    fn unknown_field(field: &str, expected: &'static [&'static str]) -> Self {
        serde::de::value::Error::unknown_field(field, expected).into()
    }

    #[cold]
    fn missing_field(field: &'static str) -> Self {
        serde::de::value::Error::missing_field(field).into()
    }

    #[cold]
    fn duplicate_field(field: &'static str) -> Self {
        serde::de::value::Error::duplicate_field(field).into()
    }
}

pub fn cvar_error_handler(In(result): In<Result<(), ConsoleError>>) {
    if let Err(err) = result {
        warn!(target: "console", "encountered an error {:?}", err);
    }
}

type BuiltinSystem = SystemId<In<Box<[String]>>, ExecResult>;
type ActionSystem = SystemId<In<(Trigger, Box<[String]>)>, ()>;
type OnSetCvarSystem = SystemId<In<Value>>;

// TODO: Add more-complex scripting language
#[derive(Clone, Debug)]
pub enum CmdKind {
    Builtin(BuiltinSystem),
    Action {
        system: Option<ActionSystem>,
        state: Trigger,
        // TODO: Mark when the last state update was, so we know how long a key has been pressed
    },
    // TODO: Allow `Alias` to invoke an arbitrary sequence of commands
    Alias(CName),
    Cvar {
        cvar: Cvar,
        on_set: Option<OnSetCvarSystem>,
    },
}

#[derive(Clone, Debug)]
pub struct CommandImpl {
    pub kind: CmdKind,
    pub help: CName,
}

pub struct AliasInfo<'a> {
    pub name: &'a str,
    pub target: &'a str,
    pub help: &'a str,
}

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

impl std::fmt::Display for CmdName<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(trigger) = &self.trigger {
            write!(f, "{}{}", trigger, self.name)
        } else {
            write!(f, "{}", self.name)
        }
    }
}

#[derive(Message, PartialEq, Eq, Clone, Debug)]
#[repr(transparent)]
pub struct UnhandledCmd(pub RunCmd<'static>);

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

impl std::fmt::Display for RunCmd<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.0)?;

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
        Self(s.into(), default())
    }
}

impl From<String> for RunCmd<'static> {
    fn from(s: String) -> Self {
        Self(s.into(), default())
    }
}

pub trait RegisterCmdExt {
    fn command<A, S, M>(&mut self, run: S) -> &mut Self
    where
        A: Parser + 'static,
        S: IntoSystem<In<A>, ExecResult, M> + 'static;

    fn action<N>(&mut self, name: N) -> &mut Self
    where
        N: Into<CName>;

    fn cvar_on_set<N, I, S, C, M>(&mut self, name: N, value: C, on_set: S, usage: I) -> &mut Self
    where
        S: IntoSystem<In<Value>, (), M> + 'static,
        N: Into<CName>,
        C: Into<Cvar>,
        I: Into<CName>;

    fn alias<S, C>(&mut self, name: S, command: C) -> &mut Self
    where
        S: Into<CName>,
        C: Into<CName>;

    fn cvar<N, I, C>(&mut self, name: N, value: C, usage: I) -> &mut Self
    where
        N: Into<CName>,
        C: Into<Cvar>,
        I: Into<CName>;
}

impl RegisterCmdExt for App {
    fn command<A, S, M>(&mut self, run: S) -> &mut Self
    where
        A: Parser + 'static,
        S: IntoSystem<In<A>, ExecResult, M> + 'static,
    {
        self.world_mut().command::<A, S, M>(run);

        self
    }

    fn action<N>(&mut self, name: N) -> &mut Self
    where
        N: Into<CName>,
    {
        self.world_mut().action(name);
        self
    }

    fn alias<S, C>(&mut self, name: S, command: C) -> &mut Self
    where
        S: Into<CName>,
        C: Into<CName>,
    {
        self.world_mut().alias(name, command);
        self
    }

    fn cvar_on_set<N, I, S, C, M>(&mut self, name: N, value: C, on_set: S, usage: I) -> &mut Self
    where
        S: IntoSystem<In<Value>, (), M> + 'static,
        N: Into<CName>,
        C: Into<Cvar>,
        I: Into<CName>,
    {
        self.world_mut().cvar_on_set(name, value, on_set, usage);

        self
    }

    fn cvar<N, I, C>(&mut self, name: N, value: C, usage: I) -> &mut Self
    where
        N: Into<CName>,
        C: Into<Cvar>,
        I: Into<CName>,
    {
        self.world_mut().cvar(name, value, usage);

        self
    }
}

impl RegisterCmdExt for SubApp {
    fn command<A, S, M>(&mut self, run: S) -> &mut Self
    where
        A: Parser + 'static,
        S: IntoSystem<In<A>, ExecResult, M> + 'static,
    {
        self.world_mut().command::<A, S, M>(run);

        self
    }

    fn action<N>(&mut self, name: N) -> &mut Self
    where
        N: Into<CName>,
    {
        self.world_mut().action(name);
        self
    }

    fn alias<S, C>(&mut self, name: S, command: C) -> &mut Self
    where
        S: Into<CName>,
        C: Into<CName>,
    {
        self.world_mut().alias(name, command);
        self
    }

    fn cvar_on_set<N, I, S, C, M>(&mut self, name: N, value: C, on_set: S, usage: I) -> &mut Self
    where
        S: IntoSystem<In<Value>, (), M> + 'static,
        N: Into<CName>,
        C: Into<Cvar>,
        I: Into<CName>,
    {
        self.world_mut().cvar_on_set(name, value, on_set, usage);

        self
    }

    fn cvar<N, I, C>(&mut self, name: N, value: C, usage: I) -> &mut Self
    where
        N: Into<CName>,
        C: Into<Cvar>,
        I: Into<CName>,
    {
        self.world_mut().cvar(name, value, usage);

        self
    }
}

fn parse_args<A>(
    mut command: clap::Command,
) -> impl FnMut(In<Box<[String]>>) -> Result<A, clap::Error>
where
    A: FromArgMatches,
{
    move |In(args)| {
        let matches = command.try_get_matches_from_mut(args.iter());
        matches.and_then(|mut m| A::from_arg_matches_mut(&mut m))
    }
}

impl RegisterCmdExt for World {
    fn command<A, S, M>(&mut self, run: S) -> &mut Self
    where
        A: Parser + 'static,
        S: IntoSystem<In<A>, ExecResult, M> + 'static,
    {
        let mut command = A::command().no_binary_name(true);
        let command_name = Cow::from(command.get_name().to_owned());
        let usage = command.render_usage();
        let short_about = command.render_help();
        let about = command.render_long_help();
        let run_sys = self.register_system(run);
        let sys = self.register_system(parse_args::<A>(command).pipe(
            move |In(res): In<Result<A, clap::Error>>, world: &mut World| match res {
                Ok(val) => world.run_system_with(run_sys, val).unwrap(),
                Err(clap_err) => match clap_err.kind() {
                    clap::error::ErrorKind::DisplayHelp
                    | clap::error::ErrorKind::DisplayHelpOnMissingArgumentOrSubcommand => {
                        format!("{short_about}").into()
                    }
                    other => format!("{other}\n{usage}").into(),
                },
            },
        ));
        self.resource_mut::<Registry>().command(command_name, sys, format!("{about}"));

        self
    }

    fn action<N>(&mut self, name: N) -> &mut Self
    where
        N: Into<CName>,
    {
        self.resource_mut::<Registry>().insert(
            name,
            CommandImpl {
                kind: CmdKind::Action { system: None, state: Trigger::Negative },
                help: Default::default(),
            },
        );

        self
    }

    fn alias<S, C>(&mut self, name: S, command: C) -> &mut Self
    where
        S: Into<CName>,
        C: Into<CName>,
    {
        self.resource_mut::<Registry>().alias(name, command);

        self
    }

    fn cvar<N, I, C>(&mut self, name: N, value: C, usage: I) -> &mut Self
    where
        N: Into<CName>,
        C: Into<Cvar>,
        I: Into<CName>,
    {
        self.resource_mut::<Registry>().cvar(name, value, None, usage);

        self
    }

    fn cvar_on_set<N, I, S, C, M>(&mut self, name: N, value: C, on_set: S, usage: I) -> &mut Self
    where
        S: IntoSystem<In<Value>, (), M> + 'static,
        N: Into<CName>,
        C: Into<Cvar>,
        I: Into<CName>,
    {
        let sys = self.register_system(on_set);
        self.resource_mut::<Registry>().cvar(name, value, Some(sys), usage);

        self
    }
}

pub trait CmdExt {
    fn println<T: Into<CName>>(&self, text: T);
    fn println_alert<T: Into<CName>>(&self, text: T);
}

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq)]
pub enum OutputType {
    #[default]
    Console,
    Alert,
}

pub struct SetCvar(pub CName, pub Value);

pub struct ResetCvar(pub CName, pub Value);

impl Command for SetCvar {
    fn apply(self, world: &mut World) {
        if let Err(e) = world.resource_mut::<Registry>().set_cvar_raw(self.0, self.1) {
            warn!(target: "console", "{e}");
        }
    }
}

pub struct ExecResult {
    pub extra_commands: Box<dyn DoubleEndedIterator<Item = RunCmd<'static>>>,
    pub output: CName,
    pub output_ty: OutputType,
}

impl Default for ExecResult {
    fn default() -> Self {
        Self {
            extra_commands: Box::new(<[RunCmd; 0]>::into_iter([])),
            output: default(),
            output_ty: default(),
        }
    }
}

impl From<String> for ExecResult {
    fn from(value: String) -> Self {
        Self { output: value.into(), ..default() }
    }
}

impl From<&'static str> for ExecResult {
    fn from(value: &'static str) -> Self {
        Self { output: value.into(), ..default() }
    }
}

impl From<CName> for ExecResult {
    fn from(value: CName) -> Self {
        Self { output: value, ..default() }
    }
}

/// `SystemId` doesn't implement `Eq` for non-`Eq` types even though it should
#[derive(Copy, Clone, PartialEq, Hash)]
struct EqHack<T>(T);

impl<T> Eq for EqHack<T> where T: PartialEq {}

/// Stores console commands.
#[derive(Resource, Default, Clone)]
pub struct Registry {
    // We store a history so that we can remove functions and see the previously-defined ones
    // TODO: Implement a compression pass (e.g. after a removal)
    commands: HashMap<CName, (CommandImpl, Vec<CommandImpl>)>,
    changed_cvars: HashMap<EqHack<SystemId<In<Value>>>, Value>,
    names: BTreeSet<CName>,
}

impl Registry {
    pub fn new() -> Registry {
        Self::default()
    }

    pub fn alias<S, C>(&mut self, name: S, command: C)
    where
        S: Into<CName>,
        C: Into<CName>,
    {
        self.insert(
            name.into(),
            CommandImpl {
                kind: CmdKind::Alias(command.into()),
                // TODO: Implement help text for aliases?
                help: "".into(),
            },
        );
    }

    pub fn aliases(&self) -> impl Iterator<Item = AliasInfo<'_>> + '_ {
        self.all_names().filter_map(move |name| {
            let cmd = self.get(name).expect("Name in `names` but not in map");

            match &cmd.kind {
                CmdKind::Alias(target) => Some(AliasInfo { name, target, help: &cmd.help }),
                _ => None,
            }
        })
    }

    fn cvar<S, C, H>(&mut self, name: S, cvar: C, on_set: Option<SystemId<In<Value>>>, help: H)
    where
        S: Into<CName>,
        C: Into<Cvar>,
        H: Into<CName>,
    {
        let cvar = cvar.into();
        if let Some(sys) = on_set {
            self.changed_cvars.insert(EqHack(sys), cvar.default.clone());
        }
        self.insert(
            name.into(),
            CommandImpl { kind: CmdKind::Cvar { cvar, on_set }, help: help.into() },
        );
    }

    fn insert<N: Into<CName>>(&mut self, name: N, value: CommandImpl) {
        let name = name.into();

        match self.commands.entry(name.clone()) {
            Entry::Occupied(mut commands) => commands.get_mut().1.push(value),
            Entry::Vacant(entry) => {
                entry.insert((value, vec![]));
            }
        }

        self.names.insert(name);
    }

    /// Registers a new command with the given name.
    ///
    /// Returns an error if a command with the specified name already exists.
    fn command<N, H>(&mut self, name: N, cmd: SystemId<In<Box<[String]>>, ExecResult>, help: H)
    where
        N: Into<CName>,
        H: Into<CName>,
    {
        self.insert(name.into(), CommandImpl { kind: CmdKind::Builtin(cmd), help: help.into() });
    }

    /// Removes the command with the given name.
    ///
    /// Returns an error if there was no command with that name.
    // TODO: If we remove a builtin we should also remove the corresponding system from the world
    pub fn remove<S>(&mut self, name: S) -> Result<(), ConsoleError>
    where
        S: AsRef<str>,
    {
        let name = name.as_ref();
        // TODO: Use `HashMap::extract_if` when stabilised
        match self.commands.get_mut(name) {
            Some((_, overlays)) => {
                if overlays.pop().is_none() {
                    self.commands.remove(name);
                }

                Ok(())
            }
            None => Err(ConsoleError::NoSuchCommand { cmd: name.to_owned().into() }),
        }
    }

    /// Removes the alias with the given name.
    ///
    /// Returns an error if there was no command with that name.
    pub fn remove_alias<S>(&mut self, name: S) -> Result<(), ConsoleError>
    where
        S: AsRef<str>,
    {
        let name = name.as_ref();
        // TODO: Use `HashMap::extract_if` when stabilised
        match self.commands.get_mut(name) {
            Some((cmd, overlays)) => {
                let CommandImpl { kind: CmdKind::Alias(_), .. } = overlays.last().unwrap_or(cmd)
                else {
                    return Err(ConsoleError::NoSuchAlias { name: name.to_owned().into() });
                };
                if overlays.pop().is_none() {
                    self.commands.remove(name);
                }

                Ok(())
            }
            None => Err(ConsoleError::NoSuchAlias { name: name.to_owned().into() }),
        }
    }

    /// Get a command.
    ///
    /// Returns an error if no command with the specified name exists.
    pub fn get<S>(&self, name: S) -> Option<&CommandImpl>
    where
        S: AsRef<str>,
    {
        self.commands.get(name.as_ref()).map(|(first, rest)| rest.last().unwrap_or(first))
    }

    /// Get a command.
    ///
    /// Returns an error if no command with the specified name exists.
    pub fn get_mut<S>(&mut self, name: S) -> Option<&mut CommandImpl>
    where
        S: AsRef<str>,
    {
        self.commands.get_mut(name.as_ref()).map(|(first, rest)| rest.last_mut().unwrap_or(first))
    }

    pub fn contains<S>(&self, name: S) -> bool
    where
        S: AsRef<str>,
    {
        self.commands.contains_key(name.as_ref())
    }

    pub fn get_cvar<S: AsRef<str>>(&self, name: S) -> Option<&Cvar> {
        self.get(name).and_then(|info| match &info.kind {
            CmdKind::Cvar { cvar, .. } => Some(cvar),
            _ => None,
        })
    }

    fn get_cvar_mut<S: AsRef<str>>(
        &mut self,
        name: S,
    ) -> Option<(&mut Cvar, Option<SystemId<In<Value>>>)> {
        self.get_mut(name).and_then(|info| match &mut info.kind {
            CmdKind::Cvar { cvar, on_set } => Some((cvar, *on_set)),
            _ => None,
        })
    }

    pub fn is_pressed<S: AsRef<str>>(&self, name: S) -> bool {
        self.get(name).and_then(|info| match &info.kind {
            CmdKind::Action { state, .. } => Some(*state),
            _ => None,
        }) == Some(Trigger::Positive)
    }

    pub fn reset_cvar<N>(&mut self, name: N) -> Result<Value, ConsoleError>
    where
        N: AsRef<str>,
    {
        let (cvar, on_set) = self
            .get_cvar_mut(name.as_ref())
            .ok_or_else(|| ConsoleError::NoSuchCvar { name: name.as_ref().to_owned().into() })?;

        let to_insert = if let Some(sys) = on_set {
            if cvar.value.is_some() { Some((EqHack(sys), cvar.default.clone())) } else { None }
        } else {
            None
        };

        let out = Ok(cvar.value.take().unwrap_or(cvar.default.clone()));

        if let Some((sys, val)) = to_insert {
            self.changed_cvars.insert(sys, val);
        }

        out
    }

    pub fn set_cvar_raw<N>(&mut self, name: N, value: Value) -> Result<Value, ConsoleError>
    where
        N: AsRef<str>,
    {
        let (cvar, on_set) = self
            .get_cvar_mut(name.as_ref())
            .ok_or_else(|| ConsoleError::NoSuchCvar { name: name.as_ref().to_owned().into() })?;

        let to_insert = if let Some(sys) = on_set {
            if cvar.value.as_ref().unwrap_or(&cvar.default) != &value {
                let value = value.clone();
                Some((EqHack(sys), value))
            } else {
                None
            }
        } else {
            None
        };

        let out = Ok(cvar.value.replace(value).unwrap_or(cvar.default.clone()));

        if let Some((sys, val)) = to_insert {
            self.changed_cvars.insert(sys, val);
        }

        out
    }

    pub fn set_cvar<N, V>(&mut self, name: N, value: V) -> Result<Value, ConsoleError>
    where
        N: AsRef<str>,
        V: AsRef<str>,
    {
        let value = Value::from_str(value.as_ref())
            .map_err(|_| ConsoleError::CvarParseInvalid { backtrace: Backtrace::capture() })?;
        self.set_cvar_raw(name, value)
    }

    /// Deserialize a single value from cvars
    pub fn read_cvar<'a, V: serde::Deserialize<'a>>(
        &'a self,
        name: impl AsRef<str>,
    ) -> Result<V, ConsoleError> {
        let name = name.as_ref();
        let cvar = self
            .get_cvar(name)
            .ok_or_else(|| ConsoleError::NoSuchCvar { name: name.to_owned().into() })?;
        serde_lexpr::from_value::<V>(cvar.value()).map_err(|_| ConsoleError::CvarParseFailed {
            name: name.to_owned().into(),
            value: cvar.value().clone(),
        })
    }

    /// Deserialize a struct or similar from cvars
    pub fn read_cvars<'a, V: serde::Deserialize<'a>>(&'a self) -> Result<V, ConsoleError> {
        struct CvarDeserializer<'a> {
            inner: &'a Registry,
        }

        struct LexprArrayDeserializer<T, V> {
            values: T,
            cur: Option<V>,
        }

        impl<'a, T>
            LexprArrayDeserializer<
                T,
                (StrDeserializer<'a, ConsoleError>, serde_lexpr::value::de::Deserializer<'a>),
            >
        where
            T: Iterator<
                Item = (
                    StrDeserializer<'a, ConsoleError>,
                    serde_lexpr::value::de::Deserializer<'a>,
                ),
            >,
        {
            fn new(mut values: T) -> Self {
                let cur = values.next();

                Self { values, cur }
            }
        }

        impl<'a, T> MapAccess<'a>
            for LexprArrayDeserializer<
                T,
                (StrDeserializer<'a, ConsoleError>, serde_lexpr::value::de::Deserializer<'a>),
            >
        where
            T: Iterator<
                Item = (
                    StrDeserializer<'a, ConsoleError>,
                    serde_lexpr::value::de::Deserializer<'a>,
                ),
            >,
        {
            type Error = ConsoleError;

            fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
            where
                K: serde::de::DeserializeSeed<'a>,
            {
                match &mut self.cur {
                    Some((k, _)) => Ok(Some(seed.deserialize(*k)?)),
                    None => Ok(None),
                }
            }

            fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::DeserializeSeed<'a>,
            {
                match mem::replace(&mut self.cur, self.values.next()) {
                    Some((_, mut v)) => Ok(seed.deserialize(&mut v).map_err(|_| {
                        ConsoleError::CvarParseInvalid { backtrace: Backtrace::capture() }
                    })?),
                    None => Err(ConsoleError::CvarParseInvalid { backtrace: Backtrace::capture() }),
                }
            }
        }

        impl<'a> Deserializer<'a> for CvarDeserializer<'a> {
            type Error = ConsoleError;

            fn deserialize_struct<V>(
                self,
                _name: &'static str,
                fields: &'static [&'static str],
                visitor: V,
            ) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                let de = LexprArrayDeserializer::new(fields.iter().filter_map(|name| {
                    self.inner.get_cvar(name).map(|c| {
                        (
                            StrDeserializer::new(name),
                            serde_lexpr::value::de::Deserializer::from_value(c.value()),
                        )
                    })
                }));

                visitor.visit_map(de)
            }

            fn deserialize_any<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("any"), &"struct"))
            }

            fn deserialize_bool<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("bool"), &"struct"))
            }

            fn deserialize_i8<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("i8"), &"struct"))
            }

            fn deserialize_i16<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("i16"), &"struct"))
            }

            fn deserialize_i32<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("i32"), &"struct"))
            }

            fn deserialize_i64<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("i64"), &"struct"))
            }

            fn deserialize_u8<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("u8"), &"struct"))
            }

            fn deserialize_u16<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("u16"), &"struct"))
            }

            fn deserialize_u32<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("u32"), &"struct"))
            }

            fn deserialize_u64<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("u64"), &"struct"))
            }

            fn deserialize_f32<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("f32"), &"struct"))
            }

            fn deserialize_f64<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("f64"), &"struct"))
            }

            fn deserialize_char<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("char"), &"struct"))
            }

            fn deserialize_str<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("str"), &"struct"))
            }

            fn deserialize_string<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("string"), &"struct"))
            }

            fn deserialize_bytes<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("bytes"), &"struct"))
            }

            fn deserialize_byte_buf<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("byte_buf"), &"struct"))
            }

            fn deserialize_option<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("option"), &"struct"))
            }

            fn deserialize_unit<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("unit"), &"struct"))
            }

            fn deserialize_unit_struct<V>(
                self,
                _: &'static str,
                _: V,
            ) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("unit_struct"), &"struct"))
            }

            fn deserialize_newtype_struct<V>(
                self,
                _: &'static str,
                _: V,
            ) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("newtype_struct"), &"struct"))
            }

            fn deserialize_seq<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("seq"), &"struct"))
            }

            fn deserialize_tuple<V>(self, _: usize, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("tuple"), &"struct"))
            }

            fn deserialize_tuple_struct<V>(
                self,
                _: &'static str,
                _: usize,
                _: V,
            ) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("tuple_struct"), &"struct"))
            }

            fn deserialize_map<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("map"), &"struct"))
            }

            fn deserialize_enum<V>(
                self,
                _: &'static str,
                _: &'static [&'static str],
                _: V,
            ) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("enum"), &"struct"))
            }

            fn deserialize_identifier<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("identifier"), &"struct"))
            }

            fn deserialize_ignored_any<V>(self, _: V) -> Result<V::Value, Self::Error>
            where
                V: serde::de::Visitor<'a>,
            {
                Err(ConsoleError::invalid_type(Unexpected::Other("ignored_any"), &"struct"))
            }
        }

        V::deserialize(CvarDeserializer { inner: self })
            .or_else(|_| V::deserialize(CvarDeserializer { inner: self }))
    }

    pub fn cmd_names(&self) -> impl Iterator<Item = &str> + Clone + '_ {
        self.all_names().filter_map(move |name| {
            self.get(name).and_then(|CommandImpl { kind, .. }| match kind {
                CmdKind::Builtin(_) => Some(name),
                _ => None,
            })
        })
    }

    pub fn alias_names(&self) -> impl Iterator<Item = &str> + Clone + '_ {
        self.all_names().filter_map(move |name| {
            self.get(name).and_then(|CommandImpl { kind, .. }| match kind {
                CmdKind::Alias(_) => Some(name),
                _ => None,
            })
        })
    }

    pub fn cvar_names(&self) -> impl Iterator<Item = &str> + Clone + '_ {
        self.all_names().filter_map(move |name| {
            self.get(name).and_then(|CommandImpl { kind, .. }| match kind {
                CmdKind::Cvar { .. } => Some(name),
                _ => None,
            })
        })
    }

    pub fn all_names(&self) -> impl Iterator<Item = &str> + Clone + '_ {
        self.names.iter().map(AsRef::as_ref)
    }
}

/// A configuration variable.
///
/// Cvars are the primary method of configuring the game.
#[derive(Debug, Clone)]
pub struct Cvar {
    // Value of this variable
    pub value: Option<Value>,

    // If true, this variable should be archived in vars.rc
    pub archive: bool,

    // If true:
    // - If a server cvar, broadcast updates to clients
    // - If a client cvar, update userinfo
    pub notify: bool,

    // The default value of this variable
    pub default: Value,
}

impl Default for Cvar {
    fn default() -> Self {
        Self { value: default(), archive: default(), notify: default(), default: Value::Nil }
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

    pub fn archive(mut self) -> Self {
        self.archive = true;

        self
    }

    pub fn notify(mut self) -> Self {
        self.notify = true;

        self
    }

    pub fn value(&self) -> &Value {
        self.value.as_ref().unwrap_or(&self.default)
    }
}

/// The line of text currently being edited in the console.
#[derive(Default)]
pub struct ConsoleInputContext {
    pub input_buf: String,
    pub history: lined::History,
    pub key_bindings: KeyBindings,
    pub commands: Vec<RunCmd<'static>>,
    pub terminal: ConsoleInputTerminal,
    pub cmd_buf: String,
}

#[derive(Default)]
pub struct ConsoleInputTerminal {
    pub stdout: Vec<u8>,
}

impl io::Write for ConsoleInputTerminal {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.stdout.write(buf)
    }

    fn write_vectored(&mut self, bufs: &[io::IoSlice<'_>]) -> io::Result<usize> {
        self.stdout.write_vectored(bufs)
    }

    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        self.stdout.write_all(buf)
    }

    fn write_fmt(&mut self, fmt: fmt::Arguments<'_>) -> io::Result<()> {
        self.stdout.write_fmt(fmt)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.stdout.flush()
    }
}

impl Tty for ConsoleInputTerminal {
    fn next_key(&mut self) -> Option<std::io::Result<lined::Key>> {
        unreachable!("TODO: Remove `next_key` from `lined::Tty`")
    }

    fn width(&self) -> std::io::Result<usize> {
        Ok(80) // TODO: Make this actually read the console width
    }
}

impl fmt::Write for ConsoleInputContext {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.cmd_buf.push_str(s);

        Ok(())
    }
}

impl EditorContext for ConsoleInputContext {
    type Terminal = ConsoleInputTerminal;
    type WordDividerIter = <lined::Context as EditorContext>::WordDividerIter;

    fn history(&self) -> &lined::History {
        &self.history
    }

    fn history_mut(&mut self) -> &mut lined::History {
        &mut self.history
    }

    fn word_divider(&self, buf: &lined::Buffer) -> Self::WordDividerIter {
        DefaultWordDivider.divide(buf)
    }

    fn terminal(&self) -> &Self::Terminal {
        &self.terminal
    }

    fn terminal_mut(&mut self) -> &mut Self::Terminal {
        &mut self.terminal
    }

    fn key_bindings(&self) -> lined::KeyBindings {
        self.key_bindings
    }
}

pub fn to_terminal_key<'a>(
    key: &'a bevy::input::keyboard::Key,
    button_state: &bevy::input::ButtonInput<KeyCode>,
) -> impl Iterator<Item = Key> + 'a {
    use bevy::input::keyboard::Key::*;
    use itertools::Either;

    let make_char =
        if button_state.pressed(KeyCode::AltLeft) || button_state.pressed(KeyCode::AltRight) {
            Key::Alt
        } else if button_state.pressed(KeyCode::ControlLeft)
            || button_state.pressed(KeyCode::ControlRight)
        {
            Key::Ctrl
        } else {
            Key::Char
        };

    match key {
        Character(c) => Either::Left(c.chars().map(make_char)),
        Backspace => Either::Right(Some(Key::Backspace).into_iter()),
        Delete => Either::Right(Some(Key::Delete).into_iter()),
        Enter => Either::Right(Some(Key::Char('\n')).into_iter()),
        Tab => Either::Right(Some(Key::Char('\t')).into_iter()),
        Space => Either::Right(Some(Key::Char(' ')).into_iter()),
        ArrowUp => Either::Right(Some(Key::Up).into_iter()),
        ArrowDown => Either::Right(Some(Key::Down).into_iter()),
        ArrowLeft => Either::Right(Some(Key::Left).into_iter()),
        ArrowRight => Either::Right(Some(Key::Right).into_iter()),
        End => Either::Right(Some(Key::End).into_iter()),
        Home => Either::Right(Some(Key::Home).into_iter()),
        Fn | FnLock | ScrollLock | Symbol | SymbolLock | Meta | Hyper | Shift | Control | Alt
        | Super | Escape | CapsLock | Dead(_) => Either::Right(None.into_iter()),

        // TODO
        _ => Either::Right(None.into_iter()),
    }
}

// TODO: This can be a tree for much better completions but we don't have enough commands to make it
// necessary right now       The `lined` interface allocates a lot anyway, so micro-optimisation
// isn't necessary here.
struct IterCompleter<'a, I> {
    iter: I,
    _marker: PhantomData<&'a ()>,
}

impl<'iter, I, Item> lined::Completer for IterCompleter<'iter, I>
where
    Item: ?Sized + 'iter,
    I: Iterator<Item = &'iter Item> + Clone + 'iter,
    &'iter Item: Into<std::borrow::Cow<'iter, str>>,
{
    fn completions<'a>(
        &'a mut self,
        start: &'a str,
    ) -> impl Iterator<Item = std::borrow::Cow<'a, str>> + 'a {
        self.iter.clone().filter_map(move |candidate: &'iter Item| {
            let candidate = candidate.into();
            if candidate.starts_with(start) { Some(candidate) } else { None }
        })
    }
}

#[derive(Resource)]
pub struct ConsoleInput {
    editor: Option<Editor<ConsoleInputContext>>,
    keymap: Emacs,
    pub stuffcmds: Vec<RunCmd<'static>>,
}

#[derive(Resource, Default)]
pub struct RenderConsoleInput {
    pub cur_text: String,
}

impl Default for ConsoleInput {
    fn default() -> Self {
        Self::new(default()).unwrap()
    }
}

impl ConsoleInput {
    pub const PROMPT: &'static str = "] ";

    /// Constructs a new `ConsoleInput`.
    ///
    /// Initializes the text content to be empty and places the cursor at position 0.
    pub fn new(history: lined::History) -> io::Result<ConsoleInput> {
        let mut keymap = Emacs::new();

        let editor = match Editor::new(
            Prompt::from(Self::PROMPT.to_owned()),
            ConsoleInputContext { history, ..default() },
        ) {
            Ok(mut editor) => {
                keymap.init(&mut editor)?;
                Some(editor)
            }
            Err(e) => {
                error!("{e}");
                None
            }
        };

        Ok(ConsoleInput { editor, keymap: Emacs::new(), stuffcmds: default() })
    }

    /// Send characters to the inner editor
    pub fn update<'a, I, C, Item>(
        &'a mut self,
        keys: I,
        candidates: C,
    ) -> impl Iterator<Item = io::Result<String>> + 'a
    where
        I: IntoIterator<Item = Key>,
        I::IntoIter: 'a,
        C: Iterator<Item = &'a Item> + Clone + 'a,
        Item: ?Sized + 'a,
        &'a Item: Into<std::borrow::Cow<'a, str>>,
    {
        let mut completer = IterCompleter { iter: candidates, _marker: PhantomData };
        keys.into_iter().filter_map(move |key| {
            let editor = self.editor.as_mut()?;

            // TODO: Completion
            match self.keymap.handle_key(key, editor, &mut completer) {
                Ok(true) => {
                    let out = editor.take_exec_buffer();

                    if let Err(e) = editor.move_to_end_of_history() {
                        warn!(target: "console", "{e}");
                    }

                    Some(
                        editor
                            .context_mut()
                            .history
                            .push(out.clone().into())
                            .and_then(|()| editor.move_cursor_to_start_of_line())
                            .map(|()| out),
                    )
                }
                Ok(false) => None,
                Err(e) => Some(Err(e)),
            }
        })
    }

    /// Returns the text currently being edited
    pub fn get_text(&self) -> impl Iterator<Item = char> + '_ {
        Self::PROMPT.chars().chain(self.editor.iter().flat_map(|e| e.current_buffer().chars()))
    }
}

#[derive(Debug, Clone)]
pub struct ConsoleText {
    pub output_type: OutputType,
    pub text: QString,
}

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Timestamp {
    pub timestamp: i64,
    pub generation: u16,
}

impl Timestamp {
    pub fn new(timestamp: i64, generation: u16) -> Self {
        Self { timestamp, generation }
    }
}

#[derive(Resource, Default, Debug)]
pub struct ConsoleOutput {
    generation: u16,
    center_print: Option<(Timestamp, QString)>,
    buffer_ty: OutputType,
    buffer: QString,
    last_timestamp: i64,
    unwritten_chunks: Vec<(Timestamp, ConsoleText)>,
}

#[derive(Resource, Default)]
pub struct RenderConsoleOutput {
    pub text_chunks: BTreeMap<Timestamp, ConsoleText>,
    pub center_print: (Timestamp, QString),
}

fn elapsed_millis(time: &Time<impl Default>) -> i64 {
    time.elapsed().as_millis().try_into().expect("Elapsed overflowed i64")
}

impl ConsoleOutput {
    pub fn print<S: AsRef<[u8]>>(&mut self, s: S, timestamp: &Time<impl Default>) {
        self.push(s, elapsed_millis(timestamp), OutputType::Console);
    }

    pub fn print_alert<S: AsRef<[u8]>>(&mut self, s: S, timestamp: &Time<impl Default>) {
        self.push(s, elapsed_millis(timestamp), OutputType::Alert);
    }

    pub fn println<S: AsRef<[u8]>>(&mut self, s: S, timestamp: &Time<impl Default>) {
        self.push_line(s, elapsed_millis(timestamp), OutputType::Console);
    }

    pub fn println_alert<S: AsRef<[u8]>>(&mut self, s: S, timestamp: &Time<impl Default>) {
        self.push_line(s, elapsed_millis(timestamp), OutputType::Alert);
    }

    pub fn new() -> ConsoleOutput {
        ConsoleOutput::default()
    }

    fn push<S: AsRef<[u8]>>(&mut self, chars: S, timestamp: i64, ty: OutputType) {
        let chars = chars.as_ref();

        if chars.is_empty() {
            return;
        }

        self.last_timestamp = timestamp;

        // TODO: set maximum capacity and pop_back when we reach it
        if ty != self.buffer_ty {
            self.flush();
        }

        self.buffer_ty = ty;
        self.buffer.push_bytes(chars);

        self.try_flush();
    }

    fn try_flush(&mut self) {
        if let Some(last_newline) = self.buffer.raw.iter().rposition(|v| *v == b'\n') {
            let (to_flush, rest) = self.buffer.split_at(last_newline + 1);
            let new_buf = rest.to_owned();
            self.buffer.truncate(to_flush.len());
            let generation = self.generation();
            self.unwritten_chunks.push((
                Timestamp::new(self.last_timestamp, generation),
                ConsoleText {
                    text: mem::replace(&mut self.buffer, new_buf.into()),
                    output_type: self.buffer_ty,
                },
            ));
        }
    }

    fn flush(&mut self) {
        let generation = self.generation();
        self.unwritten_chunks.push((
            Timestamp::new(self.last_timestamp, generation),
            ConsoleText { text: mem::take(&mut self.buffer), output_type: self.buffer_ty },
        ));
    }

    fn push_line<S: AsRef<[u8]>>(&mut self, chars: S, timestamp: i64, ty: OutputType) {
        if let Ok(vals) = str::from_utf8(chars.as_ref()) {
            match ty {
                OutputType::Console => debug!(target: "console", "{vals}"),
                OutputType::Alert => warn!(target: "console", "{vals}"),
            }
        }
        self.push(chars, timestamp, ty);
        self.push("\n", timestamp, ty);
    }

    fn generation(&mut self) -> u16 {
        let out = self.generation;
        self.generation = self.generation.wrapping_add(1);
        out
    }

    pub fn set_center_print<S: Into<QString>>(&mut self, print: S, timestamp: &Time<impl Default>) {
        let generation = self.generation();
        self.center_print =
            Some((Timestamp::new(elapsed_millis(timestamp), generation), print.into()));
    }

    pub fn drain_center_print(&mut self) -> Option<(Timestamp, QString)> {
        self.center_print.take()
    }

    pub fn drain_unwritten(
        &mut self,
    ) -> impl ExactSizeIterator<Item = (Timestamp, ConsoleText)> + '_ {
        self.unwritten_chunks.drain(..)
    }
}

impl RenderConsoleOutput {
    pub fn text(&self) -> impl Iterator<Item = (i64, &ConsoleText)> + '_ {
        self.text_chunks.iter().map(|(Timestamp { timestamp: k, .. }, v)| (*k, v))
    }

    pub fn center_print(&self, since: Duration) -> Option<QStr<'_>> {
        if self.center_print.0.timestamp
            >= i64::try_from(since.as_millis()).expect("Time overflowed i64")
        {
            Some(self.center_print.1.reborrow())
        } else {
            None
        }
    }

    /// Return an iterator over lines that have been printed in the last
    /// `interval` of time.
    ///
    /// The iterator yields the oldest results first.
    ///
    /// `max_candidates` specifies the maximum number of lines to consider,
    /// while `max_results` specifies the maximum number of lines that should
    /// be returned.
    pub fn recent(&self, since: i64) -> impl Iterator<Item = (i64, &ConsoleText)> + '_ {
        self.text_chunks
            .range(Timestamp::new(since, 0)..)
            .map(|(Timestamp { timestamp: k, .. }, v)| (*k, v))
    }
}

#[derive(Component, Default)]
struct AlertOutput {
    last_timestamp: Option<i64>,
}

#[derive(Resource)]
pub struct ConsoleAlertSettings {
    timeout: Duration,
    max_lines: usize,
}

impl Default for ConsoleAlertSettings {
    fn default() -> Self {
        Self { timeout: Duration::from_secs(3), max_lines: 10 }
    }
}

#[derive(Component)]
struct ConsoleUi;

#[derive(Component)]
struct ConsoleTextOutputUi;

#[derive(Component)]
struct ConsoleTextCenterPrintUi;

#[derive(Component)]
struct ConsoleTextInputUi;

mod systems {
    use std::collections::VecDeque;

    use crate::{
        client::text::AtlasText,
        common::net::{ClientCmd, ClientMessage, MessageKind},
    };

    use super::*;

    pub mod startup {
        use crate::client::text::{AtlasText, Conchars};

        use super::*;

        pub fn init_alert_output(conchars: Res<Conchars>, mut commands: Commands) {
            const GLYPH_SIZE_MULTIPLIER: f32 = 1.5;

            commands.spawn((
                Node {
                    position_type: PositionType::Absolute,
                    width: Val::Percent(100.),
                    top: Val::Percent(30.),
                    justify_content: JustifyContent::Center,
                    flex_direction: FlexDirection::Column,
                    ..default()
                },
                GlobalZIndex(1),
                AtlasText {
                    text: "".into(),
                    image: conchars.image.clone().into(),
                    layout: conchars.layout.clone(),
                    glyph_size: conchars.glyph_size.map(|s| s * GLYPH_SIZE_MULTIPLIER).into(),
                    line_padding: UiRect { top: Val::Px(4.), ..default() },
                    justify: JustifyContent::Center,
                },
                ConsoleTextCenterPrintUi,
            ));

            let [half_w, half_h] = conchars.glyph_size.map(|s| s / 2.);

            commands.spawn((
                Node {
                    left: half_w,
                    top: half_h,
                    flex_direction: FlexDirection::Column,
                    ..default()
                },
                AtlasText {
                    text: "".into(),
                    image: conchars.image.clone().into(),
                    layout: conchars.layout.clone(),
                    line_padding: UiRect { top: Val::Px(4.), ..default() },
                    glyph_size: conchars.glyph_size.clone().into(),
                    justify: JustifyContent::Center,
                },
                AlertOutput::default(),
            ));
        }

        pub fn init_console(
            conchars: Res<Conchars>,
            mut commands: Commands,
            assets: Res<AssetServer>,
        ) {
            let image = assets.load("gfx/conback.lmp");

            commands.spawn((
                Node {
                    position_type: PositionType::Absolute,
                    width: Val::Percent(100.),
                    height: Val::Percent(30.),
                    overflow: Overflow::clip(),
                    flex_direction: FlexDirection::Column,
                    justify_content: JustifyContent::End,
                    ..default()
                },
                Visibility::Hidden,
                GlobalZIndex(2),
                ConsoleUi,
                children![
                    (
                        ImageNode { image, ..default() },
                        Node {
                            position_type: PositionType::Absolute,
                            width: Val::Vw(100.),
                            height: Val::Vh(100.),
                            ..default()
                        },
                        ZIndex(-1),
                    ),
                    (
                        Node {
                            flex_direction: FlexDirection::Column,
                            flex_wrap: FlexWrap::NoWrap,
                            justify_content: JustifyContent::End,
                            ..default()
                        },
                        children![
                            (
                                Node { flex_direction: FlexDirection::Column, ..default() },
                                AtlasText {
                                    text: "".into(),
                                    image: conchars.image.clone().into(),
                                    layout: conchars.layout.clone(),
                                    glyph_size: conchars.glyph_size.clone().into(),
                                    line_padding: UiRect { top: Val::Px(4.), ..default() },
                                    justify: JustifyContent::FlexStart,
                                },
                                ConsoleTextOutputUi,
                            ),
                            (
                                Node { flex_direction: FlexDirection::Column, ..default() },
                                AtlasText {
                                    text: "] ".into(),
                                    image: conchars.image.clone().into(),
                                    layout: conchars.layout.clone(),
                                    glyph_size: conchars.glyph_size.clone().into(),
                                    line_padding: UiRect { top: Val::Px(4.), ..default() },
                                    justify: JustifyContent::FlexStart,
                                },
                                ConsoleTextInputUi,
                            )
                        ]
                    )
                ],
            ));
        }
    }

    pub fn update_console_visibility(
        mut consoles: Query<&mut Visibility, With<ConsoleUi>>,
        focus: Res<State<InputFocus>>,
    ) {
        for mut vis in consoles.iter_mut() {
            match focus.get() {
                InputFocus::Console => {
                    *vis = Visibility::Visible;
                }
                InputFocus::Game | InputFocus::Menu => {
                    *vis = Visibility::Hidden;
                }
            }
        }
    }

    pub fn send_unhandled_commands_to_server(
        mut unhandled: MessageReader<UnhandledCmd>,
        mut to_server: MessageWriter<ClientMessage>,
    ) {
        to_server.write_batch(unhandled.read().map(|UnhandledCmd(e)| {
            let mut bytes = vec![];
            let _ = ClientCmd::StringCmd { cmd: e.to_string().into() }.serialize(&mut bytes);

            ClientMessage { client_id: 0, packet: bytes.into(), kind: MessageKind::Reliable }
        }));
    }

    pub fn update_render_console(
        mut console_out: ResMut<ConsoleOutput>,
        mut render_out: ResMut<RenderConsoleOutput>,
        console_in: Res<ConsoleInput>,
        mut render_in: ResMut<RenderConsoleInput>,
        time: Res<Time<Real>>,
        registry: Res<Registry>,
    ) {
        if let Some(center) = console_out.drain_center_print() {
            render_out.center_print = center;
        }

        let center_time = registry.read_cvar::<f32>("scr_centertime").unwrap_or(2.);
        if !render_out.center_print.1.is_empty()
            && i64::try_from(time.elapsed().as_millis()).expect("Time overflowed i64")
                > (render_out.center_print.0.timestamp + (center_time * 1000.) as i64)
        {
            render_out.center_print.1.clear();
        }

        let new_text = console_out.drain_unwritten();
        if new_text.len() > 0 {
            render_out.text_chunks.extend(new_text);
        }

        if !itertools::equal(render_in.cur_text.chars(), console_in.get_text()) {
            render_in.cur_text.clear();
            render_in.cur_text.extend(console_in.get_text());
        }
    }

    pub fn write_console_out(
        console_out: Res<RenderConsoleOutput>,
        mut out_ui: Query<&mut AtlasText, With<ConsoleTextOutputUi>>,
    ) {
        for mut text in out_ui.iter_mut() {
            // TODO: Write only extra lines
            if !text.text.is_empty() {
                text.text.clear();
            }

            for (_, line) in console_out.text_chunks.iter() {
                text.text.push_bytes(&*line.text);
            }
        }
    }

    pub fn write_center_print(
        console_out: Res<RenderConsoleOutput>,
        mut center_ui: Query<&mut AtlasText, With<ConsoleTextCenterPrintUi>>,
    ) {
        for mut text in center_ui.iter_mut() {
            // TODO: Write only extra lines
            if !text.text.is_empty() {
                text.text.clear();
            }

            if !console_out.center_print.1.is_empty() {
                text.text.push_bytes(&*console_out.center_print.1.raw);
            }
        }
    }

    pub fn write_console_in(
        console_in: Res<RenderConsoleInput>,
        mut in_ui: Query<&mut AtlasText, With<ConsoleTextInputUi>>,
    ) {
        for mut text in in_ui.iter_mut() {
            if console_in.cur_text.as_bytes() == &*text.text.raw {
                continue;
            }

            // TODO: Write only extra lines
            if !text.text.is_empty() {
                text.text.clear();
            }

            if !console_in.cur_text.is_empty() {
                text.text.push_str(&console_in.cur_text);
            }
        }
    }

    pub fn write_alert(
        settings: Res<ConsoleAlertSettings>,
        time: Res<Time<Real>>,
        console_out: Res<RenderConsoleOutput>,
        mut alert: Query<(&mut AtlasText, &mut AlertOutput)>,
    ) {
        for (mut text, mut alert) in alert.iter_mut() {
            let elapsed = i64::try_from(time.elapsed().as_millis()).expect("Time overflowed i64");
            let timeout =
                i64::try_from(settings.timeout.as_millis()).expect("Timeout overflowed i64");

            // Need to subtract then negate, as std time doesn't support negative durations.
            let since = elapsed - timeout;
            let mut lines = console_out
                .recent(since)
                .filter(|(_, line)| line.output_type == OutputType::Alert)
                .map(|(ts, line)| (ts, &line.text))
                .take(settings.max_lines);

            let first = lines.next();
            let last_timestamp = first.map(|(ts, _)| ts);

            if last_timestamp == alert.last_timestamp {
                continue;
            }

            alert.last_timestamp = last_timestamp;

            text.text.clear();

            let Some((_, first)) = first else {
                continue;
            };
            text.text.push_bytes(first);

            for (_, line) in lines {
                text.text.push_bytes(&*line.raw);
            }
        }
    }

    pub fn execute_console(world: &mut World) {
        let mut commands =
            world.resource_mut::<Messages<RunCmd>>().drain().collect::<VecDeque<_>>();

        let mut changed_cvars = Vec::new();

        let mut unhandled = Some(Vec::<UnhandledCmd>::new())
            .filter(|_| world.get_resource::<Messages<UnhandledCmd>>().is_some());

        while let Some(cmd) = commands.pop_front() {
            debug!(target: "console", "{cmd}");
            let RunCmd(CmdName { mut name, trigger }, args) = cmd;
            loop {
                let (output, output_ty) = match world.resource_mut::<Registry>().get_mut(&*name) {
                    Some(CommandImpl { kind, .. }) => {
                        match (trigger, kind) {
                            (None, CmdKind::Cvar { cvar, on_set }) => match args.split_first() {
                                None => (
                                    Cow::from(format!("\"{}\" is \"{}\"", name, cvar.value())),
                                    OutputType::Console,
                                ),
                                Some((new_value, [])) => {
                                    let new_value =
                                        Value::from_str(new_value).unwrap_or_else(|_| {
                                            Value::String(new_value.clone().into())
                                        });

                                    if cvar.value() != &new_value {
                                        if let Some(on_set) = on_set {
                                            changed_cvars
                                                .push((EqHack(*on_set), new_value.clone()));
                                        }

                                        cvar.value = Some(new_value);
                                    }

                                    break;
                                }
                                Some(_) => (
                                    Cow::from("Too many arguments, expected 1"),
                                    OutputType::Console,
                                ),
                            },
                            (Some(_), CmdKind::Cvar { .. }) => {
                                (Cow::from(format!("{name} is a cvar")), OutputType::Console)
                            }
                            // Currently this allows action aliases - do we want that?
                            (_, CmdKind::Alias(alias)) => {
                                name = alias.clone();
                                continue;
                            }
                            (None, CmdKind::Builtin(cmd)) => {
                                let args = args.clone();
                                let cmd = *cmd;

                                match world.run_system_with(cmd, args) {
                                    Err(_) => {
                                        error!(
                                            "Command handler was registered in console but not in world"
                                        );
                                        break;
                                    }

                                    Ok(ExecResult { extra_commands, output, output_ty }) => {
                                        for command in extra_commands.rev() {
                                            commands.push_front(command);
                                        }

                                        (output, output_ty)
                                    }
                                }
                            }
                            (Some(_), CmdKind::Builtin(_)) => (
                                Cow::from(format!(
                                    "{name} is a command, and cannot be invoked with +/-"
                                )),
                                OutputType::Console,
                            ),
                            (Some(trigger), CmdKind::Action { system, state }) => {
                                if *state == trigger {
                                    break;
                                }

                                let args = args.clone();
                                *state = trigger;

                                let Some(cmd) = system else {
                                    // No invocation handler, just mark the pressed/released state
                                    break;
                                };

                                let cmd = *cmd;

                                match world.run_system_with(cmd, (trigger, args)) {
                                    Err(_) => {
                                        error!(
                                            "Command handler was registered in console but not in world"
                                        );
                                        break;
                                    }

                                    Ok(()) => break,
                                }
                            }
                            (None, CmdKind::Action { .. }) => (
                                Cow::from(format!(
                                    "{name} is an action, and must be invoked with +/-",
                                )),
                                OutputType::Console,
                            ),
                        }
                    }
                    None => {
                        if let Some(unhandled) = &mut unhandled {
                            // TODO: Receive cmd output from server(?)
                            let output = Cow::from(format!("Sending \"{name}\" to server..."));
                            unhandled.push(UnhandledCmd(RunCmd(CmdName { name, trigger }, args)));
                            (output, OutputType::Console)
                        } else {
                            (
                                Cow::from(format!("Unrecognized comand \"{name}\"")),
                                OutputType::Console,
                            )
                        }
                    }
                };

                if !output.is_empty() {
                    let time = *world.resource::<Time<Real>>();

                    if let Some(mut console_out) = world.get_resource_mut::<ConsoleOutput>() {
                        match output_ty {
                            OutputType::Console => console_out.println(output.as_bytes(), &time),
                            OutputType::Alert => {
                                console_out.println_alert(output.as_bytes(), &time)
                            }
                        }
                    } else {
                        match output_ty {
                            OutputType::Console => {
                                info!("{output}");
                            }
                            OutputType::Alert => {
                                warn!("{output}");
                            }
                        }
                    }
                }

                break;
            }
        }

        world.resource_mut::<Registry>().changed_cvars.extend(changed_cvars);
    }

    pub fn update_cvars(mut commands: Commands, mut registry: ResMut<Registry>) {
        for (sys, val) in registry.changed_cvars.drain() {
            commands.run_system_with(sys.0, val);
        }
    }
}
