//! Argument types and helpers for builtins, and stubs for builtins that aren't implemented yet.
//!
//! Builtins receive arguments as converted by `bevy_mod_scripting`: floats are `f32`, vectors are
//! [`Vec3Arg`], entities are references to their `QCEntity` component ([`Ent`]), strings are
//! [`S`], field references are [`Fld`] and functions are [`Func`]. Types that QuakeC can't
//! represent natively (FTEQW's `int`, `__uint`, `__variant` etc.) are [`Any`] for now.

use bevy_mod_scripting::bindings::{ReflectReference, ScriptValue, V};
use bevy_mod_scripting_qcvm::{QCField, QCFunction, QCString};
use seismon_utils::QString;

/// A string argument. The null string is `None`.
pub type S = Option<V<QCString>>;
/// An entity argument.
pub type Ent = ReflectReference;
/// A vector argument.
pub type Vec3Arg = [f32; 3];
/// A field reference argument (`.float` etc.).
pub type Fld = V<QCField>;
/// A function argument.
pub type Func = V<QCFunction>;
/// An argument of a type that QuakeC can't represent natively (e.g. FTEQW's `int` or
/// `__variant`), which is passed as an untyped scalar.
pub type Any = ScriptValue;

/// The bytes of a string argument (empty for the null string).
pub fn string(s: &S) -> &[u8] {
    s.as_ref().map(|s| s.0.as_bytes()).unwrap_or_default()
}

/// Concatenates string arguments.
///
/// Equivalent to `PF_VarString`.
pub fn var_string(args: &[&S]) -> QString {
    let mut out = Vec::new();
    for arg in args {
        out.extend_from_slice(string(arg));
    }
    out.into()
}

/// A builtin that isn't implemented yet. In debug builds this panics, so that progs that need it
/// are noticed.
macro_rules! todo_builtin {
    ($name:literal) => {{
        const ERROR_MSG: &str = concat!("TODO: ", $name);

        if cfg!(debug_assertions) {
            panic!("{ERROR_MSG}")
        } else {
            bevy::log::error!("{ERROR_MSG}");
        }

        Ok::<(), bevy_mod_scripting::bindings::InteropError>(())
    }};
}
pub(crate) use todo_builtin;

/// The error for calling a builtin that has only been stubbed out. Unlike [`todo_builtin`], this
/// never panics: the VM that called it is expected to handle the error.
macro_rules! unimplemented_builtin {
    ($name:literal, $number:literal) => {
        Err(bevy_mod_scripting::bindings::InteropError::external_boxed(
            concat!("unimplemented builtin `", $name, "` (#", $number, ")").into(),
        ))
    };
}
#[allow(unused_imports)]
pub(crate) use unimplemented_builtin;
