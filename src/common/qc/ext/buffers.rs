//! String buffers (`DP_QC_STRINGBUFFERS`: `buf_create`, `bufstr_get` etc.), as in FTEQW.
//!
//! Each VM has its own buffers, in a [`QcStringBuffers`] on its worldspawn. Like FTEQW, handles
//! start at 1, and operations on invalid handles do nothing.

use bevy::prelude::*;
use bevy_mod_scripting::bindings::{NamespaceBuilder, V};
use bevy_mod_scripting_qcvm::QCString;

use super::qc_string;
use crate::common::qc::{QcVmCtx, stub::S, stub::string, with_vm_ctx};

/// FTEQW's bias for buffer handles, so that 0 is never valid.
const HANDLE_BASE: f32 = 1.;

/// The most strings that a buffer can have, to catch QuakeC bugs.
const MAX_STRINGS: usize = 1024 * 1024;

/// A string buffer: strings by index, some of which may be unset.
#[derive(Debug, Clone, Default, PartialEq)]
pub struct StringBuffer {
    pub strings: Vec<Option<Vec<u8>>>,
}

impl StringBuffer {
    /// Sets the string at `index`, growing the buffer if needed.
    pub fn set(&mut self, index: usize, s: Vec<u8>) {
        if index >= MAX_STRINGS {
            warn!("String buffer index {index} is outside the sanity range");
            return;
        }
        if self.strings.len() <= index {
            self.strings.resize(index + 1, None);
        }
        self.strings[index] = Some(s);
    }

    /// Adds `s` at the end if `append`, or otherwise in the first unset slot, returning its index.
    pub fn add(&mut self, s: Vec<u8>, append: bool) -> usize {
        let index = if append {
            self.strings.len()
        } else {
            self.strings.iter().position(Option::is_none).unwrap_or(self.strings.len())
        };
        self.set(index, s);
        index
    }

    /// Removes the unset strings and sorts the rest by their first `prefix_len` bytes (all of
    /// them if `prefix_len` is 0 or less), descending if `backward`.
    pub fn sort(&mut self, prefix_len: i64, backward: bool) {
        let prefix_len = if prefix_len <= 0 { usize::MAX } else { prefix_len as usize };
        let mut strings = self.strings.drain(..).flatten().collect::<Vec<_>>();
        let key = |s: &Vec<u8>| s[..s.len().min(prefix_len)].to_vec();
        strings.sort_by_key(key);
        if backward {
            strings.reverse();
        }
        self.strings = strings.into_iter().map(Some).collect();
    }

    /// The set strings, joined with `glue`.
    pub fn implode(&self, glue: &[u8]) -> Vec<u8> {
        let mut out = Vec::new();
        for (i, s) in self.strings.iter().flatten().enumerate() {
            if i > 0 {
                out.extend_from_slice(glue);
            }
            out.extend_from_slice(s);
        }
        out
    }
}

/// The string buffers of a VM, on its worldspawn.
#[derive(Component, Debug, Default)]
pub struct QcStringBuffers {
    buffers: Vec<Option<StringBuffer>>,
}

impl QcStringBuffers {
    /// Creates a buffer, returning its handle.
    pub fn create(&mut self) -> f32 {
        let index = match self.buffers.iter().position(Option::is_none) {
            Some(index) => index,
            None => {
                self.buffers.push(None);
                self.buffers.len() - 1
            }
        };
        self.buffers[index] = Some(StringBuffer::default());
        index as f32 + HANDLE_BASE
    }

    fn index(handle: f32) -> Option<usize> {
        let index = handle - HANDLE_BASE;
        (index >= 0.).then_some(index as usize)
    }

    /// The buffer with handle `handle`.
    pub fn get(&self, handle: f32) -> Option<&StringBuffer> {
        self.buffers.get(Self::index(handle)?)?.as_ref()
    }

    /// The buffer with handle `handle`.
    pub fn get_mut(&mut self, handle: f32) -> Option<&mut StringBuffer> {
        self.buffers.get_mut(Self::index(handle)?)?.as_mut()
    }

    /// Deletes the buffer with handle `handle`.
    pub fn delete(&mut self, handle: f32) {
        if let Some(index) = Self::index(handle)
            && let Some(slot) = self.buffers.get_mut(index)
        {
            *slot = None;
        }
    }
}

/// Runs `f` with the calling VM's string buffers.
pub fn with_buffers<O>(ctx: &mut QcVmCtx<'_>, f: impl FnOnce(&mut QcStringBuffers) -> O) -> O {
    let mut entity = ctx.world.entity_mut(ctx.root);
    let mut buffers = entity.entry::<QcStringBuffers>().or_default();
    f(&mut buffers.get_mut())
}

pub fn register<N: 'static>(ns: &mut NamespaceBuilder<'_, N>) {
    ns.register("buf_create", || with_vm_ctx(|ctx| Ok(with_buffers(ctx, |b| b.create()))))
        .register("buf_del", |handle: f32| {
            with_vm_ctx(|ctx| {
                with_buffers(ctx, |b| b.delete(handle));
                Ok(())
            })
        })
        .register("buf_getsize", |handle: f32| {
            with_vm_ctx(|ctx| {
                Ok(with_buffers(ctx, |b| b.get(handle).map_or(0., |b| b.strings.len() as f32)))
            })
        })
        .register("buf_copy", |from: f32, to: f32| {
            with_vm_ctx(|ctx| {
                with_buffers(ctx, |b| {
                    if from != to
                        && let Some(source) = b.get(from).cloned()
                        && let Some(dest) = b.get_mut(to)
                    {
                        *dest = source;
                    }
                });
                Ok(())
            })
        })
        .register("buf_sort", |handle: f32, prefix_len: f32, backward: f32| {
            with_vm_ctx(|ctx| {
                with_buffers(ctx, |b| {
                    if let Some(buffer) = b.get_mut(handle) {
                        buffer.sort(prefix_len as i64, backward != 0.);
                    }
                });
                Ok(())
            })
        })
        .register("buf_implode", |handle: f32, glue: S| {
            with_vm_ctx(|ctx| {
                Ok(with_buffers(ctx, |b| match b.get(handle) {
                    Some(buffer) => qc_string(buffer.implode(string(&glue))),
                    None => V(QCString::null()),
                }))
            })
        })
        .register("bufstr_get", |handle: f32, index: f32| {
            with_vm_ctx(|ctx| {
                Ok(with_buffers(ctx, |b| {
                    match b.get(handle).and_then(|b| b.strings.get(index as usize)?.as_ref()) {
                        Some(s) if index >= 0. => qc_string(s),
                        _ => V(QCString::null()),
                    }
                }))
            })
        })
        .register("bufstr_set", |handle: f32, index: f32, s: S| {
            with_vm_ctx(|ctx| {
                with_buffers(ctx, |b| {
                    if let Some(buffer) = b.get_mut(handle)
                        && index >= 0.
                    {
                        buffer.set(index as usize, string(&s).to_vec());
                    }
                });
                Ok(())
            })
        })
        .register("bufstr_add", |handle: f32, s: S, ordered: f32| {
            with_vm_ctx(|ctx| {
                Ok(with_buffers(ctx, |b| match b.get_mut(handle) {
                    Some(buffer) => buffer.add(string(&s).to_vec(), ordered != 0.) as f32,
                    None => 0.,
                }))
            })
        })
        .register("bufstr_free", |handle: f32, index: f32| {
            with_vm_ctx(|ctx| {
                with_buffers(ctx, |b| {
                    if let Some(buffer) = b.get_mut(handle)
                        && index >= 0.
                        && let Some(slot) = buffer.strings.get_mut(index as usize)
                    {
                        *slot = None;
                    }
                });
                Ok(())
            })
        });
}

#[cfg(test)]
mod tests {
    use super::*;

    fn strings(buffer: &StringBuffer) -> Vec<Option<&[u8]>> {
        buffer.strings.iter().map(|s| s.as_deref()).collect()
    }

    #[test]
    fn handles_start_at_one_and_are_reused() {
        let mut buffers = QcStringBuffers::default();
        assert_eq!(buffers.create(), 1.);
        assert_eq!(buffers.create(), 2.);
        buffers.delete(1.);
        assert!(buffers.get(1.).is_none());
        assert!(buffers.get(0.).is_none());
        assert_eq!(buffers.create(), 1.);
    }

    #[test]
    fn add_fills_holes_unless_appending() {
        let mut buffer = StringBuffer::default();
        assert_eq!(buffer.add(b"a".to_vec(), false), 0);
        buffer.set(2, b"c".to_vec());
        assert_eq!(strings(&buffer), [Some(&b"a"[..]), None, Some(b"c")]);
        assert_eq!(buffer.add(b"b".to_vec(), false), 1);
        assert_eq!(buffer.add(b"d".to_vec(), true), 3);
        assert_eq!(buffer.implode(b","), b"a,b,c,d");
    }

    #[test]
    fn sort_removes_unset_strings() {
        let mut buffer = StringBuffer::default();
        buffer.set(0, b"b2".to_vec());
        buffer.set(2, b"a".to_vec());
        buffer.set(3, b"b1".to_vec());
        buffer.sort(0, false);
        assert_eq!(strings(&buffer), [Some(&b"a"[..]), Some(b"b1"), Some(b"b2")]);
        buffer.sort(0, true);
        assert_eq!(strings(&buffer), [Some(&b"b2"[..]), Some(b"b1"), Some(b"a")]);
        // With a prefix, ties keep their order.
        buffer.sort(1, false);
        assert_eq!(strings(&buffer), [Some(&b"a"[..]), Some(b"b2"), Some(b"b1")]);
    }
}
