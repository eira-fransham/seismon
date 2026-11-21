//! Tokenizing (`tokenize`, `argv` etc.), as in FTEQW. Each VM has its own tokens, in a
//! [`QcTokens`] on its worldspawn.

use bevy::prelude::*;
use bevy_mod_scripting::bindings::{NamespaceBuilder, V};
use bevy_mod_scripting_qcvm::{QCString, builtin_argc};

use super::qc_string;
use crate::common::qc::{
    QcVmCtx,
    stub::{S, string},
    text::{self, Token},
    with_vm_ctx,
};

/// The tokens from the last `tokenize` (or `tokenize_console` etc.) of a VM, on its worldspawn.
#[derive(Component, Debug, Default)]
pub struct QcTokens(pub Vec<Token>);

impl QcTokens {
    /// The token at `index`, where negative indices count from the end.
    pub fn get(&self, index: f32) -> Option<&Token> {
        let mut index = index as i64;
        if index < 0 {
            index += self.0.len() as i64;
        }
        self.0.get(usize::try_from(index).ok()?)
    }
}

/// Replaces the calling VM's tokens with `tokens`, returning how many there are.
fn set_tokens(ctx: &mut QcVmCtx<'_>, tokens: Vec<Token>) -> f32 {
    let count = tokens.len() as f32;
    ctx.world.entity_mut(ctx.root).insert(QcTokens(tokens));
    count
}

/// Runs `f` with the calling VM's token at `index`.
fn with_token<O>(index: f32, f: impl FnOnce(Option<&Token>) -> O) -> Result<O, bevy_mod_scripting::bindings::InteropError> {
    with_vm_ctx(|ctx| Ok(f(ctx.world.get::<QcTokens>(ctx.root).and_then(|tokens| tokens.get(index)))))
}

pub fn register<N: 'static>(ns: &mut NamespaceBuilder<'_, N>) {
    ns.register("tokenize", |s: S| {
        with_vm_ctx(|ctx| Ok(set_tokens(ctx, text::tokenize(string(&s), false))))
    })
    .register("tokenize_console", |s: S| {
        with_vm_ctx(|ctx| Ok(set_tokens(ctx, text::tokenize(string(&s), true))))
    })
    .register(
        "tokenizebyseparator",
        |s: S, a: S, b: S, c: S, d: S, e: S, f: S, g: S| {
            let separators = [&a, &b, &c, &d, &e, &f, &g];
            let count = builtin_argc().saturating_sub(1).min(separators.len());
            let separators = separators[..count].iter().map(|s| string(s)).collect::<Vec<_>>();
            with_vm_ctx(|ctx| {
                Ok(set_tokens(ctx, text::tokenize_by_separator(string(&s), &separators)))
            })
        },
    )
    .register("argv", |index: f32| {
        with_token(index, |token| match token {
            Some(token) => qc_string(&token.text),
            None => V(QCString::null()),
        })
    })
    .register("argv_start_index", |index: f32| {
        with_token(index, |token| token.map_or(-1., |token| token.start as f32))
    })
    .register("argv_end_index", |index: f32| {
        with_token(index, |token| token.map_or(-1., |token| token.end as f32))
    });
}
