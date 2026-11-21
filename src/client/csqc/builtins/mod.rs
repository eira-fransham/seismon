//! The CSQC builtins that depend on running on the client (and aren't shared with the server, as
//! [`core_builtins`](crate::common::qc::core_builtins) and
//! [`ext_builtins`](crate::common::qc::ext::ext_builtins) are).

pub mod client;
pub mod draw;
pub mod hostcache;
pub mod physics;

use bevy_mod_scripting::bindings::NamespaceBuilder;

/// Registers the client-only CSQC builtins.
pub fn register<N: 'static>(ns: &mut NamespaceBuilder<'_, N>) {
    client::register(ns);
    draw::register(ns);
    hostcache::register(ns);
    physics::register(ns);
}
