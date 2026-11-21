//! Cvar and console builtins (`registercvar`, `cvar_string`, `print` etc.), as in FTEQW.

use bevy::prelude::*;
use bevy_mod_commands::{CmdKind, Registry};
use bevy_mod_scripting::bindings::{NamespaceBuilder, V};
use bevy_mod_scripting_qcvm::{QCString, builtin_argc};
use serde_lexpr::Value;

use super::{buffers::with_buffers, qc_string};
use crate::common::{
    console::ConsoleOutput,
    qc::{
        QC_CREATED_HELP,
        stub::{S, string},
        text::wildcmp,
        with_vm_ctx,
    },
};

/// `cvar_type`'s flags (`CVAR_TYPEFLAG_*`).
pub mod cvar_type {
    pub const EXISTS: u32 = 1 << 0;
    pub const SAVED: u32 = 1 << 1;
    pub const PRIVATE: u32 = 1 << 2;
    pub const ENGINE: u32 = 1 << 3;
    pub const HAS_DESCRIPTION: u32 = 1 << 4;
    pub const READONLY: u32 = 1 << 5;
}

/// A cvar's value as QuakeC sees it: strings without quotes, and numbers as they're written.
pub fn cvar_value_string(value: &Value) -> String {
    match value {
        Value::Nil | Value::Null => String::new(),
        Value::Bool(b) => if *b { "1" } else { "0" }.to_owned(),
        Value::Number(n) => n.to_string(),
        Value::String(s) | Value::Symbol(s) => s.to_string(),
        Value::Char(c) => c.to_string(),
        Value::Keyword(k) => format!("#:{k}"),
        other => other.to_string(),
    }
}

/// Inserts a cvar with the default `value` (which is quoted if it isn't a valid cvar value as
/// it is, e.g. it has spaces).
pub fn insert_cvar(registry: &mut Registry, name: &str, value: &str) -> anyhow::Result<()> {
    if registry.insert_cvar(name.to_owned(), value, QC_CREATED_HELP).is_ok() {
        return Ok(());
    }
    let quoted = format!("\"{}\"", value.replace('\\', "\\\\").replace('"', "\\\""));
    Ok(registry.insert_cvar(name.to_owned(), quoted, QC_CREATED_HELP)?)
}

/// `cvar_type` for the cvar `name`.
pub fn cvar_type_flags(registry: &Registry, name: &str) -> u32 {
    let Some(command) = registry.get(name) else {
        return 0;
    };
    let CmdKind::Cvar { cvar, .. } = &command.kind else {
        return 0;
    };

    let mut flags = cvar_type::EXISTS;
    if cvar.archive {
        flags |= cvar_type::SAVED;
    }
    if &*command.help != QC_CREATED_HELP {
        flags |= cvar_type::ENGINE;
    }
    if !command.help.is_empty() {
        flags |= cvar_type::HAS_DESCRIPTION;
    }
    flags
}

/// Whether `name` matches `buf_cvarlist`'s `pattern`: a wildcard pattern if it has `*` or `?`, or
/// otherwise a prefix. An empty pattern matches everything.
fn matches_pattern(name: &[u8], pattern: &[u8]) -> bool {
    if pattern.contains(&b'*') || pattern.contains(&b'?') {
        wildcmp(pattern, name, false)
    } else {
        name.starts_with(pattern)
    }
}

/// The names of the cvars for `buf_cvarlist`: those matching `pattern` but not `antipattern`
/// (where empty patterns are ignored), sorted.
pub fn cvar_list(registry: &Registry, pattern: &[u8], antipattern: &[u8]) -> Vec<String> {
    let mut names = registry
        .cvar_names()
        .filter(|name| pattern.is_empty() || matches_pattern(name.as_bytes(), pattern))
        .filter(|name| antipattern.is_empty() || !matches_pattern(name.as_bytes(), antipattern))
        .map(str::to_owned)
        .collect::<Vec<_>>();
    names.sort();
    names
}

/// `checkcommand`: 1 for a command, 2 for an alias, 3 for a cvar and 0 if `name` doesn't exist.
pub fn check_command(registry: &Registry, name: &str) -> f32 {
    match registry.get(name).map(|command| &command.kind) {
        Some(CmdKind::Builtin(_) | CmdKind::Action { .. }) => 1.,
        Some(CmdKind::Alias(_)) => 2.,
        Some(CmdKind::Cvar { .. }) => 3.,
        None => 0.,
    }
}

/// Prints to the console, like `print`.
pub fn console_print(world: &mut World, text: &[u8]) {
    info!("{}", String::from_utf8_lossy(text).trim_end());
    let time = world.get_resource::<Time<Real>>().cloned();
    if let (Some(mut console), Some(time)) = (world.get_resource_mut::<ConsoleOutput>(), time) {
        console.print(text, &time);
    }
}

fn lossy(s: &S) -> String {
    String::from_utf8_lossy(string(s)).into_owned()
}

pub fn register<N: 'static>(ns: &mut NamespaceBuilder<'_, N>) {
    ns.register("registercvar", |name: S, value: S, _flags: f32| {
        // FTEQW ignores the default value unless the flags are passed too.
        let value = if builtin_argc() > 2 { lossy(&value) } else { String::new() };
        with_vm_ctx(|ctx| {
            let mut registry = ctx.world.resource_mut::<Registry>();
            let name = lossy(&name);
            if registry.contains(&name) {
                return Ok(0.);
            }
            Ok(if insert_cvar(&mut registry, &name, &value).is_ok() { 1. } else { 0. })
        })
    })
    .register("cvar_string", |name: S| {
        with_vm_ctx(|ctx| {
            let registry = ctx.world.resource::<Registry>();
            Ok(match registry.get_cvar(lossy(&name)) {
                Some(cvar) => qc_string(cvar_value_string(cvar.value())),
                None => V(QCString::from("")),
            })
        })
    })
    .register("cvar_defstring", |name: S| {
        with_vm_ctx(|ctx| {
            let registry = ctx.world.resource::<Registry>();
            Ok(match registry.get_cvar(lossy(&name)) {
                Some(cvar) => qc_string(cvar_value_string(&cvar.default)),
                None => V(QCString::from("")),
            })
        })
    })
    .register("cvar_description", |name: S| {
        with_vm_ctx(|ctx| {
            let registry = ctx.world.resource::<Registry>();
            Ok(match registry.get(lossy(&name)) {
                Some(command) if matches!(command.kind, CmdKind::Cvar { .. }) => {
                    qc_string(command.help.as_bytes())
                }
                _ => V(QCString::from("")),
            })
        })
    })
    .register("cvar_type", |name: S| {
        with_vm_ctx(|ctx| {
            Ok(cvar_type_flags(ctx.world.resource::<Registry>(), &lossy(&name)) as f32)
        })
    })
    .register("checkcommand", |name: S| {
        with_vm_ctx(|ctx| Ok(check_command(ctx.world.resource::<Registry>(), &lossy(&name))))
    })
    .register("buf_cvarlist", |handle: f32, pattern: S, antipattern: S| {
        with_vm_ctx(|ctx| {
            let names =
                cvar_list(ctx.world.resource::<Registry>(), string(&pattern), string(&antipattern));
            with_buffers(ctx, |buffers| {
                if let Some(buffer) = buffers.get_mut(handle) {
                    buffer.strings =
                        names.into_iter().map(|name| Some(name.into_bytes())).collect();
                }
            });
            Ok(())
        })
    })
    .register("print", |a: S, b: S, c: S, d: S, e: S, f: S, g: S, h: S| {
        let args = [&a, &b, &c, &d, &e, &f, &g, &h];
        let count = builtin_argc().clamp(1, args.len());
        let text = args[..count].iter().flat_map(|s| string(s)).copied().collect::<Vec<_>>();
        with_vm_ctx(|ctx| {
            console_print(ctx.world, &text);
            Ok(())
        })
    });
}

#[cfg(test)]
mod tests {
    use std::assert_matches;

    use super::*;

    #[test]
    fn cvar_values_are_unquoted() {
        assert_eq!(cvar_value_string(&"1.5".parse().unwrap()), "1.5");
        assert_eq!(cvar_value_string(&"\"a b\"".parse().unwrap()), "a b");
        assert_eq!(cvar_value_string(&"player".parse().unwrap()), "player");
    }

    #[test]
    fn registry_queries() {
        let mut registry = Registry::new();
        assert_matches!(insert_cvar(&mut registry, "sv_gravity", "800"), Ok(()));
        assert_matches!(insert_cvar(&mut registry, "qc_thing", "a b"), Ok(()));
        assert_matches!(insert_cvar(&mut registry, "sv_maxspeed", "320"), Ok(()));

        assert_eq!(cvar_value_string(registry.get_cvar("qc_thing").unwrap().value()), "a b");
        assert_eq!(
            cvar_type_flags(&registry, "sv_gravity"),
            cvar_type::EXISTS | cvar_type::ENGINE | cvar_type::HAS_DESCRIPTION
        );
        assert_eq!(
            cvar_type_flags(&registry, "qc_thing"),
            cvar_type::EXISTS | cvar_type::HAS_DESCRIPTION
        );
        assert_eq!(cvar_type_flags(&registry, "nope"), 0);
        assert_eq!(check_command(&registry, "sv_gravity"), 3.);
        assert_eq!(check_command(&registry, "nope"), 0.);

        assert_eq!(cvar_list(&registry, b"sv_", b""), ["sv_gravity", "sv_maxspeed"]);
        assert_eq!(cvar_list(&registry, b"*", b"sv_g*"), ["qc_thing", "sv_maxspeed"]);
    }
}
