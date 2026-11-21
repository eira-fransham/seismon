//! Builtins about QuakeC functions and engine extensions (`isfunction`, `callfunction`,
//! `checkbuiltin`, `checkextension`).

use std::ffi::CString;

use bevy::prelude::*;
use bevy_mod_scripting::bindings::{InteropError, NamespaceBuilder};
use bevy_mod_scripting_qcvm::{builtin_argc, builtin_vector_arg, qcvm};

use super::AnyArg;
use crate::common::qc::{
    qc_error,
    stub::{Any, S, string},
    with_vm_ctx,
};

/// The extensions that both SSQC and CSQC support, see [`ext_builtins`](super::ext_builtins).
pub const COMMON_EXTENSIONS: &[&str] = &[
    "DP_QC_ASINACOSATANATAN2TAN",
    "DP_QC_COPYENTITY",
    "DP_QC_CRC16",
    "DP_QC_CVAR_DEFSTRING",
    "DP_QC_CVAR_DESCRIPTION",
    "DP_QC_CVAR_STRING",
    "DP_QC_CVAR_TYPE",
    "DP_QC_DIGEST",
    "DP_QC_EDICT_NUM",
    "DP_QC_ENTITYDATA",
    "DP_QC_ENTITYSTRING",
    "DP_QC_ETOS",
    "DP_QC_FINDCHAIN",
    "DP_QC_FINDCHAINFLAGS",
    "DP_QC_FINDCHAINFLOAT",
    "DP_QC_FINDCHAIN_TOFIELD",
    "DP_QC_FINDFLAGS",
    "DP_QC_FINDFLOAT",
    "DP_QC_FS_SEARCH",
    "DP_QC_LOG",
    "DP_QC_MINMAXBOUND",
    "DP_QC_RANDOMVEC",
    "DP_QC_SINCOSSQRTPOW",
    "DP_QC_SPRINTF",
    "DP_QC_STRFTIME",
    "DP_QC_STRINGBUFFERS",
    "DP_QC_STRINGBUFFERS_CVARLIST",
    "DP_QC_STRINGCOLORFUNCTIONS",
    "DP_QC_STRING_CASE_FUNCTIONS",
    "DP_QC_STRREPLACE",
    "DP_QC_TOKENIZEBYSEPARATOR",
    "DP_QC_TOKENIZE_CONSOLE",
    "DP_QC_UNLIMITEDTEMPSTRINGS",
    "DP_QC_URI_ESCAPE",
    "DP_QC_VECTORVECTORS",
    "DP_QC_WHICHPACK",
    "DP_REGISTERCVAR",
    "FRIK_FILE",
    "FTE_QC_CHECKCOMMAND",
    "FTE_STRINGS",
    "KRIMZON_SV_PARSECLIENTCOMMAND",
];

/// The extensions that a VM supports on top of [`COMMON_EXTENSIONS`], on its worldspawn.
#[derive(Component, Debug, Clone, Default)]
pub struct QcExtensions(pub &'static [&'static str]);

pub fn register<N: 'static>(ns: &mut NamespaceBuilder<'_, N>) {
    ns.register("isfunction", |name: S| {
        with_vm_ctx(|ctx| {
            let vm = ctx.vm()?;
            let defined = CString::new(string(&name))
                .is_ok_and(|name| vm.vm.functions().get_by_name(&name).is_ok());
            Ok(if defined { 1. } else { 0. })
        })
    })
    .register("callfunction", |a: Any, b: Any, c: Any, d: Any, e: Any, f: Any, g: Any, h: Any| {
        let argc = builtin_argc();
        if argc == 0 {
            return Err(qc_error("callfunction needs at least one argument"));
        }
        let raw = [a, b, c, d, e, f, g, h];
        let decoded = raw[..argc.min(raw.len())]
            .iter()
            .map(AnyArg::decode)
            .collect::<Result<Vec<_>, InteropError>>()?;
        let (name, args) = decoded.split_last().expect("argc > 0");
        let AnyArg::String(name) = name else {
            return Err(qc_error("callfunction: the last argument must be a function name"));
        };

        with_vm_ctx(|ctx| {
            let vm = ctx.vm()?;
            // FTEQW does nothing for undefined functions.
            if vm.vm.functions().get_by_name(&*name.0).is_err() {
                return Ok(());
            }

            // The arguments are passed through as they are. Their types aren't known, so
            // anything that isn't a string or entity is passed as a vector, which a float
            // parameter reads the first component of.
            let args = args
                .iter()
                .enumerate()
                .map(|(i, arg)| match arg {
                    AnyArg::Void => qcvm::Value::Void,
                    AnyArg::String(s) => qcvm::Value::String(s.0.clone()),
                    AnyArg::Entity(e) => vm.entity_value(*e),
                    AnyArg::Function(f) => {
                        qcvm::Value::Function(qcvm::Function::Index(qcvm::FunctionIndex(f.0)))
                    }
                    AnyArg::Field(f) => qcvm::Value::Field((*f).into()),
                    AnyArg::Float(x) => match builtin_vector_arg(i) {
                        Some(v) => qcvm::Value::Vector(v.to_array().into()),
                        None => qcvm::Value::Float(*x),
                    },
                })
                .collect::<Vec<_>>();

            vm.call_with_world::<()>(ctx.world, name.0.clone(), args)
        })
    })
    .register("checkbuiltin", |function: Any| {
        let function = AnyArg::decode(&function)?;
        with_vm_ctx(|ctx| {
            let AnyArg::Function(function) = function else {
                return Ok(0.);
            };
            let vm = ctx.vm()?;
            Ok(if vm.has_builtin(ctx.world, function.0) { 1. } else { 0. })
        })
    })
    .register("checkextension", |name: S| {
        with_vm_ctx(|ctx| {
            let name = String::from_utf8_lossy(string(&name));
            let extra = ctx.world.get::<QcExtensions>(ctx.root).map_or(&[][..], |e| e.0);
            let supported = COMMON_EXTENSIONS
                .iter()
                .chain(extra)
                .any(|extension| extension.eq_ignore_ascii_case(&name));
            Ok(if supported { 1. } else { 0. })
        })
    });
}
