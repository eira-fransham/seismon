use std::{collections::BTreeSet, mem, str::FromStr};

use bevy_ecs::{
    resource::Resource,
    system::{In, SystemId},
};
use hashbrown::{HashMap, hash_map::Entry};
use serde::{
    Deserializer,
    de::{Error, MapAccess, Unexpected, value::StrDeserializer},
};
use serde_lexpr::Value;
use snafu::Backtrace;

use crate::{CName, ConsoleError, Cvar, ErasedBuiltinSystemReturn, Trigger};

// TODO: Use `On` so this acts like a regular observer, with the only difference being that
// it can optionally hold up execution of further commands. This is useful for commands like `wait`.
// This would also make the
pub(crate) type BuiltinSystem = SystemId<In<Box<[String]>>, ErasedBuiltinSystemReturn>;
pub(crate) type ActionSystem = SystemId<In<(Trigger, Box<[String]>)>, ()>;
pub(crate) type OnSetCvarSystem = SystemId<In<Value>>;

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

/// `SystemId` doesn't implement `Eq` for non-`Eq` types even though it should
#[derive(Copy, Clone, PartialEq, Hash)]
pub(crate) struct EqHack<T>(pub(crate) T);

impl<T> Eq for EqHack<T> where T: PartialEq {}

/// Stores registered commands, cvars, aliases and actions.
#[derive(Resource, Default, Clone)]
pub struct Registry {
    // We store a history so that we can remove functions and see the previously-defined ones
    // TODO: Implement a compression pass (e.g. after a removal)
    commands: HashMap<CName, (CommandImpl, Vec<CommandImpl>)>,
    pub(crate) changed_cvars: HashMap<EqHack<SystemId<In<Value>>>, Value>,
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

    pub(crate) fn cvar<S, C, H>(
        &mut self,
        name: S,
        cvar: C,
        on_set: Option<SystemId<In<Value>>>,
        help: H,
    ) where
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

    pub(crate) fn insert<N: Into<CName>>(&mut self, name: N, value: CommandImpl) {
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
    pub(crate) fn command<N, H>(&mut self, name: N, cmd: BuiltinSystem, help: H)
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

    pub(crate) fn get_cvar_mut<S: AsRef<str>>(
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
                (StrDeserializer<'a, ConsoleError>, serde_lexpr::value::Deserializer<'a>),
            >
        where
            T: Iterator<
                Item = (StrDeserializer<'a, ConsoleError>, serde_lexpr::value::Deserializer<'a>),
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
                (StrDeserializer<'a, ConsoleError>, serde_lexpr::value::Deserializer<'a>),
            >
        where
            T: Iterator<
                Item = (StrDeserializer<'a, ConsoleError>, serde_lexpr::value::Deserializer<'a>),
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
                            serde_lexpr::value::Deserializer::from_value(c.value()),
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
