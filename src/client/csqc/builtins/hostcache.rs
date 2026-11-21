//! The server browser builtins (`gethostcachevalue` etc.). Seismon has no server browser yet, so
//! the host cache is always empty and finished refreshing. The keys, sort order and masks that
//! CSQC sets are kept, so that it sees them again, as in FTEQW's `pr_clcmd.c`.

use bevy::prelude::*;
use bevy_mod_scripting::bindings::{NamespaceBuilder, V};
use bevy_mod_scripting_qcvm::QCString;

use crate::common::qc::{
    stub::{S, string},
    with_vm_ctx,
};

/// FTEQW's `hostcacheglobal_t`.
mod value {
    pub const VIEW_COUNT: i32 = 0;
    pub const TOTAL_COUNT: i32 = 1;
    pub const SORT_FIELD: i32 = 6;
    pub const SORT_DESCENDING: i32 = 7;
}

/// FTEQW's keys (`hostcachekey_t`), by name. The index is the key number.
const KEYS: &[&[&str]] = &[
    &["ping"],
    &["map"],
    &["name", "hostname"],
    &["address", "cname"],
    &["numplayers"],
    &["maxplayers"],
    &["gamedir", "game", "*gamedir"],
    &["freeplayers", "freeslots"],
    &["basegame"],
    &["flags"],
    &["timelimit"],
    &["fraglimit"],
    &["mod"],
    &["protocol"],
    &["numbots"],
    &["numspectators"],
    &["numhumans"],
    &["qcstatus"],
    &["category"],
    &["isfavorite"],
    &["islocal"],
    &["isproxy"],
    &["serverinfo"],
];

/// The key number of `player0` (`SLKEY_PLAYER0`).
const PLAYER0_KEY: f32 = 24.;
/// The key number of the first custom key (`SLKEY_CUSTOM`).
const CUSTOM_KEY: f32 = PLAYER0_KEY + 255.;

/// The host cache's settings for a VM, on its worldspawn.
#[derive(Component, Debug, Default)]
pub struct HostCache {
    custom_keys: Vec<String>,
    sort_field: f32,
    sort_descending: bool,
}

impl HostCache {
    /// The key number for `name`, adding it as a custom key if it isn't known.
    pub fn key(&mut self, name: &str) -> f32 {
        if let Some(index) = KEYS.iter().position(|names| names.contains(&name)) {
            return index as f32;
        }
        if let Some(player) = name.strip_prefix("player") {
            return PLAYER0_KEY + player.parse::<f32>().unwrap_or(0.);
        }
        let index = match self.custom_keys.iter().position(|key| key == name) {
            Some(index) => index,
            None => {
                self.custom_keys.push(name.to_owned());
                self.custom_keys.len() - 1
            }
        };
        CUSTOM_KEY + index as f32
    }

    /// `gethostcachevalue`.
    pub fn value(&self, kind: f32) -> f32 {
        match kind as i32 {
            // There are no servers (and no queries), so the counts are 0.
            value::VIEW_COUNT | value::TOTAL_COUNT => 0.,
            value::SORT_FIELD => self.sort_field,
            value::SORT_DESCENDING if self.sort_descending => 1.,
            _ => 0.,
        }
    }
}

fn with_cache<O>(f: impl FnOnce(&mut HostCache) -> O) -> Result<O, bevy_mod_scripting::bindings::InteropError> {
    with_vm_ctx(|ctx| {
        let mut entity = ctx.world.entity_mut(ctx.root);
        let mut entry = entity.entry::<HostCache>().or_default();
        Ok(f(&mut entry.get_mut()))
    })
}

pub fn register<N: 'static>(ns: &mut NamespaceBuilder<'_, N>) {
    ns.register("gethostcachevalue", |kind: f32| with_cache(|cache| cache.value(kind)))
        // There are no hosts.
        .register("gethostcachestring", |_field: f32, _host: f32| V(QCString::null()))
        .register("gethostcachenumber", |_field: f32, _host: f32| 0.)
        .register("gethostcacheindexforkey", |key: S| {
            with_cache(|cache| cache.key(&String::from_utf8_lossy(string(&key))))
        })
        .register("addwantedhostcachekey", |key: S| {
            with_cache(|cache| {
                cache.key(&String::from_utf8_lossy(string(&key)));
            })
        })
        .register("sethostcachesort", |field: f32, descending: f32| {
            with_cache(|cache| {
                cache.sort_field = field;
                cache.sort_descending = descending != 0.;
            })
        })
        // Masks only filter servers, and there are none.
        .register("resethostcachemasks", || {})
        .register("sethostcachemaskstring", |_mask: f32, _field: f32, _s: S, _op: f32| {})
        .register("sethostcachemasknumber", |_mask: f32, _field: f32, _n: f32, _op: f32| {})
        .register("resorthostcache", || {})
        .register("refreshhostcache", |_purge: f32| {});
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keys_match_fteqw() {
        let mut cache = HostCache::default();
        assert_eq!(cache.key("ping"), 0.);
        assert_eq!(cache.key("hostname"), 2.);
        assert_eq!(cache.key("cname"), 3.);
        assert_eq!(cache.key("serverinfo"), 22.);
        assert_eq!(cache.key("player3"), 27.);
        assert_eq!(cache.key("custom"), CUSTOM_KEY);
        assert_eq!(cache.key("other"), CUSTOM_KEY + 1.);
        assert_eq!(cache.key("custom"), CUSTOM_KEY);
    }

    #[test]
    fn the_cache_is_empty() {
        let mut cache = HostCache::default();
        assert_eq!(cache.value(0.), 0.);
        assert_eq!(cache.value(1.), 0.);
        cache.sort_field = 2.;
        cache.sort_descending = true;
        assert_eq!(cache.value(6.), 2.);
        assert_eq!(cache.value(7.), 1.);
    }
}
