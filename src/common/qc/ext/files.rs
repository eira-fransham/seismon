//! File builtins (`fopen`, `search_begin`, `whichpack` etc.). Files are read-only: they are
//! read from the game's [`Vfs`] (paks and loose files), and opening a file for writing fails as
//! if writing weren't allowed.
//!
//! Like FTEQW, `fopen` looks for `data/<name>` first and then `<name>`. Each VM has its own open
//! files and searches, in a [`QcFiles`] on its worldspawn.

use bevy::prelude::*;
use bevy_mod_scripting::bindings::NamespaceBuilder;

use crate::common::qc::{
    qc_error,
    stub::{Any, Ent, S, string, unimplemented_builtin},
};

pub fn register<N: 'static>(ns: &mut NamespaceBuilder<'_, N>) {
    ns.register("fopen", |_name: S, _mode: f32, _mmap_min_size: f32| -> Result<(), _> {
        unimplemented_builtin!("fopen", 110)
    })
    .register("fclose", |_handle: f32| -> Result<(), _> { unimplemented_builtin!("fclose", 111) })
    .register("fgets", |_handle: f32| -> Result<(), _> { unimplemented_builtin!("fgets", 112) })
    .register(
        "fputs",
        |_handle: f32, _a: S, _b: S, _c: S, _d: S, _e: S, _f: S, _g: S| -> Result<(), _> {
            unimplemented_builtin!("fputs", 113)
        },
    )
    .register("fseek", |_handle: f32, _offset: Any| -> Result<(), _> {
        unimplemented_builtin!("fseek")
    })
    .register(
        "search_begin",
        |_pattern: S, _flags: f32, _quiet: f32, _package: S| -> Result<(), _> {
            unimplemented_builtin!("search_begin")
        },
    )
    .register("search_end", |_handle: f32| -> Result<(), _> {
        unimplemented_builtin!("search_end")
    })
    .register("search_getsize", |_handle: f32| -> Result<(), _> {
        unimplemented_builtin!("search_getsize")
    })
    .register("search_getfilename", |_handle: f32, _n: f32| -> Result<(), _> {
        unimplemented_builtin!("search_getfilename")
    })
    .register("search_getfilesize", |_handle: f32, _n: f32| -> Result<(), _> {
        unimplemented_builtin!("search_getfilesize")
    })
    .register("search_getfilemtime", |_handle: f32, _n: f32| -> Result<(), _> {
        unimplemented_builtin!("search_getfilemtime")
    })
    .register("search_getpackagename", |_handle: f32, _n: f32| -> Result<(), _> {
        unimplemented_builtin!("search_getpackagename")
    })
    .register("whichpack", |_name: S, _flags: f32| -> Result<(), _> {
        unimplemented_builtin!("whichpack", 503)
    })
    .register("buf_loadfile", |_name: S, _handle: f32| -> Result<(), _> {
        unimplemented_builtin!("buf_loadfile", 535)
    })
    .register("buf_writefile", |_file: f32, _handle: f32, _start: f32, _count: f32| {
        warn!("buf_writefile: files are read-only");
        0.
    })
    .register(
        "writetofile",
        |file: f32, _e: Ent| -> Result<(), bevy_mod_scripting::bindings::InteropError> {
            // No file can be open for writing.
            Err(qc_error(format!("writetofile: file {file} is not open for writing")))
        },
    )
    .register("uri_get", |uri: S, _id: f32, _mime: S, _data: S| {
        warn!(
            "uri_get: can't fetch {:?}, there's no HTTP client",
            String::from_utf8_lossy(string(&uri))
        );
        0.
    });
}
