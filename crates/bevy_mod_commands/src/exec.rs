use std::{any::Any, collections::VecDeque, marker::PhantomData, ops::ControlFlow, str::FromStr};

use beef::Cow;
use bevy_ecs::{
    message::{Message, Messages},
    resource::Resource,
    system::{Command, Commands, In, IntoSystem, Local, ResMut, SystemId},
    world::World,
};
use bevy_log::{error, info, warn};
use serde_lexpr::Value;

use crate::{
    CName, CmdKind, CmdName, CommandImpl, Registry, RunCmd, UnhandledCmd, registry::EqHack,
};

pub trait FromWithWorld<Other> {
    fn from_with_world(other: Other, world: &mut World) -> Self;
}

pub trait IntoWithWorld<Other> {
    fn into_with_world(self, world: &mut World) -> Other;
}

impl<T, U> FromWithWorld<T> for U
where
    U: From<T>,
{
    fn from_with_world(other: T, _: &mut World) -> Self {
        Self::from(other)
    }
}

impl<T, U> IntoWithWorld<T> for U
where
    T: FromWithWorld<U>,
{
    fn into_with_world(self, world: &mut World) -> T {
        T::from_with_world(self, world)
    }
}

pub struct CommandIncomplete<T, S, O, M>
where
    T: ?Sized,
{
    pub context: Box<T>,
    pub system: S,
    pub marker: PhantomData<(O, M)>,
}

type ErasedCommandIncomplete = CommandIncomplete<
    dyn Any + Send,
    SystemId<In<Box<dyn Any + Send>>, ErasedBuiltinSystemReturn>,
    ErasedBuiltinSystemReturn,
    (),
>;

impl<T, S, O, M> FromWithWorld<CommandIncomplete<T, S, O, M>> for ErasedBuiltinSystemReturn
where
    T: Any + Send,
    S: IntoSystem<In<Box<T>>, O, M> + 'static,
    O: IntoWithWorld<Self> + 'static,
{
    fn from_with_world(other: CommandIncomplete<T, S, O, M>, world: &mut World) -> Self {
        let erased_system = |In(any): In<Box<dyn Any + Send>>| {
            any.downcast().expect("Invalid context passed to command continuation")
        };
        let erased_system_id = world.register_system_cached(
            erased_system
                .pipe(other.system)
                .pipe(|In(out): In<O>, world: &mut World| out.into_with_world(world)),
        );

        Self::ContinueWith(CommandIncomplete {
            context: other.context,
            system: erased_system_id,
            marker: PhantomData,
        })
    }
}

impl<Context, Out> From<ControlFlow<Out, Box<Context>>> for ErasedBuiltinSystemReturn
where
    Context: Any + Send,
    Out: Into<ExecResult> + Send,
{
    fn from(other: ControlFlow<Out, Box<Context>>) -> Self {
        match other {
            ControlFlow::Continue(context) => Self::Continue(context),
            ControlFlow::Break(exec_result) => Self::Done(exec_result.into()),
        }
    }
}

pub enum ErasedBuiltinSystemReturn {
    Continue(Box<dyn Any + Send>),
    ContinueWith(ErasedCommandIncomplete),
    Done(ExecResult),
}

impl<T> From<T> for ErasedBuiltinSystemReturn
where
    T: Into<ExecResult>,
{
    fn from(value: T) -> Self {
        Self::Done(value.into())
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

/// Output produced by executing a command.
///
/// Only emitted if the `capture_out` field of [`crate::CommandsPlugin`] is set,
/// otherwise command output is logged using `info!`/`warn!`.
#[derive(Message, Clone, Debug)]
pub struct CommandOutput {
    pub output: CName,
    pub output_ty: OutputType,
}

pub struct SetCvar(pub CName, pub Value);

pub struct ResetCvar(pub CName, pub Value);

impl Command for SetCvar {
    type Out = ();

    fn apply(self, world: &mut World) {
        if let Err(e) = world.resource_mut::<Registry>().set_cvar_raw(self.0, self.1) {
            warn!("{e}");
        }
    }
}

#[derive(Default)]
pub struct ExecResult {
    pub output: CName,
    pub output_ty: OutputType,
}

impl From<String> for ExecResult {
    fn from(value: String) -> Self {
        Self { output: value.into(), ..Default::default() }
    }
}

impl From<&'static str> for ExecResult {
    fn from(value: &'static str) -> Self {
        Self { output: value.into(), ..Default::default() }
    }
}

impl From<CName> for ExecResult {
    fn from(value: CName) -> Self {
        Self { output: value, ..Default::default() }
    }
}

impl From<()> for ExecResult {
    fn from(_: ()) -> Self {
        Default::default()
    }
}

/// Commands queued for execution, in order.
#[derive(Resource, Default)]
pub struct PendingCommands(pub VecDeque<RunCmd<'static>>);

pub(crate) fn populate_pending(
    mut messages: ResMut<Messages<RunCmd<'static>>>,
    mut pending: ResMut<PendingCommands>,
) {
    pending.0.extend(messages.drain());
}

pub(crate) fn execute_console(
    world: &mut World,
    mut currently_running_command: Local<Option<ErasedCommandIncomplete>>,
) {
    let mut changed_cvars = Vec::new();
    let mut outputs = Vec::<CommandOutput>::new();

    let mut unhandled_messages =
        world.contains_resource::<Messages<UnhandledCmd>>().then_some(Vec::<UnhandledCmd>::new());

    'execute: loop {
        let ExecResult { output, output_ty } = if let Some(cmd) = currently_running_command.take() {
            let Ok(result) = world.run_system_with(cmd.system, cmd.context) else {
                error!("Failure while running command continuation");
                continue;
            };

            match result {
                ErasedBuiltinSystemReturn::Continue(context) => {
                    *currently_running_command = Some(CommandIncomplete {
                        context,
                        system: cmd.system,
                        marker: PhantomData,
                    });
                    break;
                }
                ErasedBuiltinSystemReturn::ContinueWith(command_incomplete) => {
                    *currently_running_command = Some(command_incomplete);
                    break;
                }
                ErasedBuiltinSystemReturn::Done(result) => result,
            }
        } else {
            let Some(RunCmd(CmdName { mut name, trigger }, args)) =
                world.resource_mut::<PendingCommands>().0.pop_front()
            else {
                break;
            };

            loop {
                break match world.resource_mut::<Registry>().get_mut(&*name) {
                    Some(CommandImpl { kind, .. }) => {
                        match (trigger, kind) {
                            (None, CmdKind::Cvar { cvar, on_set }) => match args.split_first() {
                                None => Cow::from(format!("\"{}\" is \"{}\"", name, cvar.value()))
                                    .into(),
                                Some((new_value, [])) => {
                                    let new_value = Value::from_str(new_value)
                                        .unwrap_or_else(|_| Value::String(new_value.clone()));

                                    if cvar.value() != &new_value {
                                        if let Some(on_set) = on_set {
                                            changed_cvars
                                                .push((EqHack(*on_set), new_value.clone()));
                                        }

                                        cvar.value = Some(new_value);
                                    }

                                    break 'execute;
                                }
                                Some(_) => Cow::from("Too many arguments, expected 1").into(),
                            },
                            (Some(_), CmdKind::Cvar { .. }) => {
                                Cow::from(format!("{name} is a cvar")).into()
                            }
                            // Currently this allows action aliases - do we want that?
                            (_, CmdKind::Alias(alias)) => {
                                name = alias.clone();
                                continue;
                            }
                            (None, CmdKind::Builtin(cmd)) => {
                                let args = args.clone();
                                let cmd = *cmd;

                                let Ok(result) = world.run_system_with(cmd, args) else {
                                    error!("Failure while running command \"{name}\"");
                                    continue 'execute;
                                };

                                match result {
                                    ErasedBuiltinSystemReturn::Continue(_) => panic!(
                                        "Misconfigured command: returned `Continue` without a first continuation"
                                    ),
                                    ErasedBuiltinSystemReturn::ContinueWith(command_incomplete) => {
                                        *currently_running_command = Some(command_incomplete);
                                        break 'execute;
                                    }
                                    ErasedBuiltinSystemReturn::Done(result) => result,
                                }
                            }
                            (Some(_), CmdKind::Builtin(_)) => {
                                format!("{name} is a command, and cannot be invoked with +/-")
                                    .into()
                            }
                            (Some(trigger), CmdKind::Action { system, state }) => {
                                if *state == trigger {
                                    continue 'execute;
                                }

                                let args = args.clone();
                                *state = trigger;

                                let Some(cmd) = system else {
                                    // No invocation handler, just mark the pressed/released state
                                    continue 'execute;
                                };

                                let cmd = *cmd;

                                match world.run_system_with(cmd, (trigger, args)) {
                                    Err(_) => {
                                        error!(
                                            "Command handler was registered in the registry but not in the world"
                                        );
                                        continue 'execute;
                                    }

                                    Ok(()) => continue 'execute,
                                }
                            }
                            (None, CmdKind::Action { .. }) => {
                                format!("{name} is an action, and must be invoked with +/-").into()
                            }
                        }
                    }
                    None => {
                        if let Some(unhandled_messages) = &mut unhandled_messages {
                            unhandled_messages
                                .push(UnhandledCmd(RunCmd(CmdName { name, trigger }, args)));
                            ExecResult::default()
                        } else {
                            format!("Unrecognized command \"{name}\"").into()
                        }
                    }
                };
            }
        };

        if !output.is_empty() {
            outputs.push(CommandOutput { output, output_ty });
        }
    }

    if !outputs.is_empty() {
        if let Some(mut messages) = world.get_resource_mut::<Messages<CommandOutput>>() {
            messages.write_batch(outputs);
        } else {
            for CommandOutput { output, output_ty } in outputs {
                match output_ty {
                    OutputType::Console => info!("{output}"),
                    OutputType::Alert => warn!("{output}"),
                }
            }
        }
    }

    if let Some(unhandled_messages) = unhandled_messages {
        world.resource_mut::<Messages<UnhandledCmd>>().write_batch(unhandled_messages);
    }

    world.resource_mut::<Registry>().changed_cvars.extend(changed_cvars);
}

pub(crate) fn update_cvars(mut commands: Commands, mut registry: ResMut<Registry>) {
    for (sys, val) in registry.changed_cvars.drain() {
        commands.run_system_with(sys.0, val);
    }
}
