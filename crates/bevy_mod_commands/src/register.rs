use beef::Cow;
use bevy_app::{App, SubApp};
use bevy_ecs::{
    system::{In, IntoSystem},
    world::World,
};
use clap::{FromArgMatches, Parser};
use serde_lexpr::Value;

use crate::{
    CName, CmdKind, CommandImpl, Cvar, ErasedBuiltinSystemReturn, IntoWithWorld, Registry, Trigger,
};

/// Extension trait for registering commands, actions, aliases and cvars.
///
/// Requires [`CommandsPlugin`](crate::CommandsPlugin) to have been added first, since it
/// initializes the [`Registry`] resource.
pub trait RegisterCmdExt {
    fn command<A, O, S, M>(&mut self, run: S) -> &mut Self
    where
        A: Parser + 'static,
        S: IntoSystem<In<A>, O, M> + 'static,
        O: IntoWithWorld<ErasedBuiltinSystemReturn> + 'static;

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
    fn command<A, O, S, M>(&mut self, run: S) -> &mut Self
    where
        A: Parser + 'static,
        S: IntoSystem<In<A>, O, M> + 'static,
        O: IntoWithWorld<ErasedBuiltinSystemReturn> + 'static,
    {
        self.world_mut().command::<A, O, S, M>(run);

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
    fn command<A, O, S, M>(&mut self, run: S) -> &mut Self
    where
        A: Parser + 'static,
        S: IntoSystem<In<A>, O, M> + 'static,
        O: IntoWithWorld<ErasedBuiltinSystemReturn> + 'static,
    {
        self.world_mut().command::<A, O, S, M>(run);

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
    fn command<A, O, S, M>(&mut self, run: S) -> &mut Self
    where
        A: Parser + 'static,
        S: IntoSystem<In<A>, O, M> + 'static,
        O: IntoWithWorld<ErasedBuiltinSystemReturn> + 'static,
    {
        let mut command = A::command().no_binary_name(true);
        let command_name = Cow::from(command.get_name().to_owned());
        let usage = command.render_usage();
        let short_about = command.render_help();
        let about = command.render_long_help();
        let run_sys = self.register_system(run);
        let sys = self.register_system(parse_args::<A>(command).pipe(
            move |In(res): In<Result<A, clap::Error>>,
                  world: &mut World|
                  -> ErasedBuiltinSystemReturn {
                match res {
                    Ok(val) => {
                        let out_raw = world
                            .run_system_with(run_sys, val)
                            .expect("Tried to run unregistered command");

                        out_raw.into_with_world(world)
                    }
                    Err(clap_err) => match clap_err.kind() {
                        clap::error::ErrorKind::DisplayHelp
                        | clap::error::ErrorKind::DisplayHelpOnMissingArgumentOrSubcommand => {
                            format!("{short_about}").into()
                        }
                        other => format!("{other}\n{usage}").into(),
                    },
                }
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
