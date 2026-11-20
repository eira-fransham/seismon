//! Client-side QuakeC (CSQC): mods' `csprogs.dat`, which draws the HUD or the whole view.
//!
//! See <https://quakewiki.org/wiki/EXT_CSQC>. Like FTEQW, the mode depends on which entry points
//! the `csprogs.dat` defines (see [`CsqcMode`]):
//!
//! - With `CSQC_UpdateView`, CSQC draws everything ("full" CSQC): the engine draws no world,
//!   entities, view model, particles or HUD, so if CSQC doesn't draw anything, nothing is drawn.
//! - Otherwise, with `CSQC_DrawHud` or `CSQC_DrawScores`, the engine draws the 3D view and CSQC
//!   draws the HUD and scoreboard instead of the engine ("simple" CSQC).
//!
//! The console, menus and loading screen are always drawn by the engine.
//!
//! `csprogs.dat` is loaded with each level (when the server info arrives), unless `cl_nocsqc` is
//! set. If CSQC fails at any point (including calling a builtin that hasn't been implemented yet),
//! it is shut down and the engine draws everything as usual.
//!
//! CSQC runs in its own VM, with its own builtin namespace ([`CsqcBuiltins`]) and storage (see
//! [`storage`]). Its entities belong to its worldspawn (see [`QCOwnedBy`]), so they're separate
//! from the server's even on a listen server.
//!
//! [`QCOwnedBy`]: bevy_mod_scripting_qcvm::QCOwnedBy

pub mod draw;
pub mod hooks;
pub mod storage;
pub mod stubs;

use std::ffi::CString;

use bevy::{prelude::*, window::PrimaryWindow};
use bevy_mod_commands::{RegisterCmdExt as _, Registry};
use bevy_mod_scripting::{
    asset::ScriptAsset,
    bindings::NamespaceBuilder,
    core::{
        commands::{AttachScript, DetachScript},
        script::ScriptContexts,
    },
    script::ScriptAttachment,
};
use bevy_mod_scripting_qcvm::{
    QCEntity, QCScriptingPlugin, QCVmConfig, QCVmContext, QCWorldspawn,
    qcvm::{self, FromReturnValue, MissingArguments, QCDefs as _, VmOptions},
};
use qcvm_defs_csqc::{CSQC_API_VERSION, entry_points};

use self::{
    draw::{CsqcDrawList, CsqcScene},
    hooks::{ConsumedInput, CsqcCommands, CsqcInputCursors},
};
use crate::{
    client::{ClientGameState, input::InputFocus, ui::UI_SCALE},
    common::qc::{CsqcBuiltins, QcStorage, QcValue, add_core_plugin, core_builtins::core_builtins},
};

/// What CSQC draws, which depends on the entry points that the `csprogs.dat` defines.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CsqcMode {
    /// CSQC draws the HUD and scoreboard (`CSQC_DrawHud` and `CSQC_DrawScores`), after the engine
    /// draws the 3D view.
    Simple,
    /// CSQC draws the whole view (`CSQC_UpdateView`).
    Full,
}

impl CsqcMode {
    /// The mode for a `csprogs.dat` that defines the functions that `defines` returns true for, or
    /// `None` if it doesn't define any of the entry points that draw anything (in which case CSQC
    /// isn't used, as in FTEQW).
    pub fn detect(defines: impl Fn(&str) -> bool) -> Option<Self> {
        if defines(entry_points::UPDATE_VIEW) {
            Some(Self::Full)
        } else if defines(entry_points::DRAW_HUD) || defines(entry_points::DRAW_SCORES) {
            Some(Self::Simple)
        } else {
            None
        }
    }
}

/// The loaded CSQC VM. CSQC is active while this exists.
#[derive(Resource, Clone, Debug)]
pub struct Csqc {
    pub vm: QCVmContext,
    pub worldspawn: Entity,
    pub attachment: ScriptAttachment,
    pub mode: CsqcMode,
}

/// Loading `csprogs.dat` for the current level.
#[derive(Resource, Debug)]
pub enum CsqcLoad {
    /// Waiting for the asset.
    Pending(Handle<ScriptAsset>),
    /// Waiting for the VM to load.
    Attaching {
        worldspawn: Entity,
        attachment: ScriptAttachment,
        /// The frames that have passed without the VM being found.
        frames_missing: u32,
    },
    /// Finished, whether or not CSQC loaded.
    Done,
}

/// How many frames to wait for the VM to appear after attaching `csprogs.dat`, before giving up.
/// Contexts that fail to load are never created.
const MAX_ATTACH_FRAMES: u32 = 60;

/// The configuration of the CSQC VM, for the worldspawn that `csprogs.dat` is attached to.
pub fn vm_config() -> QCVmConfig {
    QCVmConfig::new::<CsqcBuiltins>(qcvm_defs_csqc::Csqc).with_options(VmOptions {
        // As for the server's VM: QuakeC compilers allow calls with missing arguments.
        missing_arguments: MissingArguments::Stale,
    })
}

/// Whether CSQC is drawing the HUD (in either mode), so the engine shouldn't.
pub fn csqc_draws_hud(csqc: Option<Res<Csqc>>) -> bool {
    csqc.is_some()
}

/// Whether CSQC is drawing the whole view, so the engine shouldn't draw the 3D view either.
pub fn csqc_draws_scene(csqc: Option<Res<Csqc>>) -> bool {
    csqc.is_some_and(|csqc| csqc.mode == CsqcMode::Full)
}

/// Registers the CSQC builtins: the [`core_builtins`] shared with the server, and stubs for the
/// rest (see [`stubs`]).
pub fn register_builtins(world: &mut World) {
    let mut ns = NamespaceBuilder::<CsqcBuiltins>::new_unregistered(world);
    core_builtins(&mut ns);
    stubs::engine_builtins(&mut ns);
    stubs::extension_builtins(&mut ns);
    stubs::csqc_only_builtins(&mut ns);
}

/// Calls the CSQC entry point `name` if CSQC is loaded and defines it, returning what it returned.
///
/// If the call fails, CSQC is shut down (so the engine draws everything again) and this returns
/// `None`.
pub fn call_entry<R: FromReturnValue>(
    world: &mut World,
    name: &str,
    args: Vec<qcvm::Value>,
) -> Option<R> {
    let csqc = world.get_resource::<Csqc>()?.clone();
    let name_c = CString::new(name).ok()?;
    if csqc.vm.vm.functions().get_by_name(&name_c).is_err() {
        return None;
    }

    match csqc.vm.call_with_world::<R>(world, name_c, args) {
        Ok(ret) => Some(ret),
        Err(e) => {
            error!("CSQC error in {name}, disabling CSQC: {e}");
            shutdown(world, false);
            None
        }
    }
}

/// Unloads CSQC, if it is loaded or loading. If `call_qc`, `CSQC_Shutdown` is called first.
pub fn shutdown(world: &mut World, call_qc: bool) {
    if call_qc {
        call_entry::<()>(world, entry_points::SHUTDOWN, vec![]);
    }

    let unload = |world: &mut World, worldspawn: Entity, attachment: ScriptAttachment| {
        DetachScript::<QCScriptingPlugin>::new(attachment).apply(world);
        // This also despawns every entity that belongs to CSQC.
        if let Ok(entity) = world.get_entity_mut(worldspawn) {
            entity.despawn();
        }
    };

    if let Some(csqc) = world.remove_resource::<Csqc>() {
        info!("Unloading CSQC");
        unload(world, csqc.worldspawn, csqc.attachment);
    }
    if let Some(CsqcLoad::Attaching { worldspawn, attachment, .. }) =
        world.remove_resource::<CsqcLoad>()
    {
        unload(world, worldspawn, attachment);
    }

    if let Some(mut draw_list) = world.get_resource_mut::<CsqcDrawList>() {
        draw_list.0.clear();
    }
    if let Some(mut scene) = world.get_resource_mut::<CsqcScene>() {
        scene.clear();
    }
    if let Some(mut commands) = world.get_resource_mut::<CsqcCommands>() {
        commands.0.clear();
    }
}

/// Starts loading `csprogs.dat` for a new level, unloading the previous level's CSQC.
pub struct BeginLoad;

impl Command for BeginLoad {
    type Out = ();

    fn apply(self, world: &mut World) {
        shutdown(world, true);

        let disabled = world.resource::<Registry>().read_cvar::<f32>("cl_nocsqc").unwrap_or(0.);
        if disabled != 0. {
            return;
        }

        let handle = world.resource::<AssetServer>().load("csprogs.dat");
        world.insert_resource(CsqcLoad::Pending(handle));
    }
}

/// Unloads CSQC, e.g. when disconnecting.
pub struct Shutdown;

impl Command for Shutdown {
    type Out = ();

    fn apply(self, world: &mut World) {
        shutdown(world, true);
    }
}

/// Whether `wait_for_load` should wait for CSQC to load before continuing.
pub fn csqc_loading(load: Option<&CsqcLoad>) -> bool {
    matches!(load, Some(CsqcLoad::Pending(_) | CsqcLoad::Attaching { .. }))
}

/// The CRC in the header of a `progs.dat`.
fn progs_crc(bytes: &[u8]) -> Option<i32> {
    Some(i32::from_le_bytes(bytes.get(4..8)?.try_into().ok()?))
}

/// Advances [`CsqcLoad`]: waits for `csprogs.dat`, attaches it to a new worldspawn, waits for the
/// VM, and then starts CSQC.
pub fn advance_csqc_load(world: &mut World) {
    let Some(load) = world.get_resource::<CsqcLoad>() else {
        return;
    };

    match load {
        CsqcLoad::Done => {}
        CsqcLoad::Pending(handle) => {
            let handle = handle.clone();
            let asset_server = world.resource::<AssetServer>();
            if let Some(bevy::asset::LoadState::Failed(e)) = asset_server.get_load_state(&handle) {
                // Most games don't have a `csprogs.dat`.
                debug!("Not using CSQC: {e}");
                world.insert_resource(CsqcLoad::Done);
                return;
            }

            let Some(asset) = world.resource::<Assets<ScriptAsset>>().get(&handle) else {
                return;
            };

            // Check the CRC here so that the error is clear, as contexts that fail to load are
            // only logged.
            let expected = qcvm_defs_csqc::Csqc::CRC;
            match progs_crc(&asset.content) {
                Some(crc) if crc == expected => {}
                crc => {
                    warn!(
                        "Not using CSQC: csprogs.dat has CRC {crc:?}, but only simple CSQC \
                         (CRC {expected}) is supported"
                    );
                    world.insert_resource(CsqcLoad::Done);
                    return;
                }
            }

            let worldspawn = world
                .spawn((Name::new("CSQC worldspawn"), QCWorldspawn, QCEntity, vm_config()))
                .id();
            let attachment = ScriptAttachment::EntityScript(worldspawn, handle);
            AttachScript::<QCScriptingPlugin>::new(attachment.clone()).apply(world);
            world.insert_resource(CsqcLoad::Attaching {
                worldspawn,
                attachment,
                frames_missing: 0,
            });
        }
        CsqcLoad::Attaching { worldspawn, attachment, frames_missing } => {
            let (worldspawn, attachment, frames_missing) =
                (*worldspawn, attachment.clone(), *frames_missing);

            let context = world
                .resource::<ScriptContexts<QCScriptingPlugin>>()
                .read()
                .get_context(&attachment);
            let vm = match context {
                Some(context) => match context.as_loaded() {
                    Some(vm) => vm.lock().clone(),
                    // Still loading.
                    None => return,
                },
                None if frames_missing < MAX_ATTACH_FRAMES => {
                    world.insert_resource(CsqcLoad::Attaching {
                        worldspawn,
                        attachment,
                        frames_missing: frames_missing + 1,
                    });
                    return;
                }
                None => {
                    warn!("Not using CSQC: csprogs.dat failed to load");
                    shutdown(world, false);
                    world.insert_resource(CsqcLoad::Done);
                    return;
                }
            };

            start(world, vm, worldspawn, attachment);
        }
    }
}

/// Starts CSQC once its VM has loaded.
fn start(world: &mut World, vm: QCVmContext, worldspawn: Entity, attachment: ScriptAttachment) {
    world.insert_resource(CsqcLoad::Done);

    let defines = |name: &str| {
        CString::new(name).is_ok_and(|name| vm.vm.functions().get_by_name(&name).is_ok())
    };
    let Some(mode) = CsqcMode::detect(defines) else {
        warn!(
            "Not using CSQC: csprogs.dat defines none of {}, {} or {}",
            entry_points::UPDATE_VIEW,
            entry_points::DRAW_HUD,
            entry_points::DRAW_SCORES,
        );
        DetachScript::<QCScriptingPlugin>::new(attachment).apply(world);
        world.despawn(worldspawn);
        return;
    };

    info!("Using {mode:?} CSQC");
    vm.vm.reset_globals();
    storage::init_storage(world, &vm);
    world.insert_resource(Csqc { vm, worldspawn, attachment, mode });

    set_frame_globals(world);
    call_entry::<()>(
        world,
        entry_points::INIT,
        vec![
            qcvm::Value::Float(CSQC_API_VERSION),
            hooks::qc_string(b"Seismon"),
            qcvm::Value::Float(env!("CARGO_PKG_VERSION_MINOR").parse().unwrap_or(0.)),
        ],
    );
}

/// Sets the `time` and `frametime` globals before calling CSQC.
fn set_frame_globals(world: &mut World) {
    let Some(csqc) = world.get_resource::<Csqc>() else {
        return;
    };
    let worldspawn = csqc.worldspawn;
    let Some(QcStorage(storage)) = world.get::<QcStorage>(worldspawn).cloned() else {
        return;
    };

    let time = world.resource::<Time<Virtual>>();
    let (elapsed, delta) = (time.elapsed_secs(), time.delta_secs());
    for (name, value) in [("time", elapsed), ("frametime", delta)] {
        if let Err(e) = storage.set_global(world, name, QcValue::Float(value)) {
            error!("Failed to set CSQC global {name}: {e}");
        }
    }
}

/// The size of the screen in virtual (Quake) pixels, as passed to CSQC.
fn virtual_screen_size(world: &mut World) -> Vec2 {
    let mut windows = world.query_filtered::<&Window, With<PrimaryWindow>>();
    windows
        .iter(world)
        .next()
        .map(|window| window.size() / UI_SCALE)
        .unwrap_or(Vec2::new(320., 200.))
}

/// Calls CSQC to draw the frame.
pub fn run_csqc_frame(world: &mut World) {
    let Some(mode) = world.get_resource::<Csqc>().map(|csqc| csqc.mode) else {
        return;
    };

    world.resource_mut::<CsqcDrawList>().0.clear();
    world.resource_mut::<CsqcScene>().clear();
    set_frame_globals(world);

    let size = virtual_screen_size(world);
    let size_vec = qcvm::Value::Vector([size.x, size.y, 0.].into());

    match mode {
        CsqcMode::Simple => {
            // `sb_showscores | sb_showteamscores << 1`. There are no team scores yet.
            let show_scores = world.resource::<Registry>().is_pressed("showscores");
            let scores_shown = if show_scores { 1. } else { 0. };

            call_entry::<()>(
                world,
                entry_points::DRAW_HUD,
                vec![size_vec.clone(), qcvm::Value::Float(scores_shown)],
            );
            if show_scores {
                call_entry::<()>(
                    world,
                    entry_points::DRAW_SCORES,
                    vec![size_vec, qcvm::Value::Float(scores_shown)],
                );
            }
        }
        CsqcMode::Full => {
            let not_menu = *world.resource::<State<InputFocus>>().get() == InputFocus::Game;
            call_entry::<()>(
                world,
                entry_points::UPDATE_VIEW,
                vec![
                    qcvm::Value::Float(size.x),
                    qcvm::Value::Float(size.y),
                    qcvm::Value::Float(if not_menu { 1. } else { 0. }),
                ],
            );
        }
    }
}

/// Runs client-side QuakeC.
pub struct CsqcPlugin;

impl Plugin for CsqcPlugin {
    fn build(&self, app: &mut App) {
        add_core_plugin(app);
        register_builtins(app.world_mut());

        app.cvar("cl_nocsqc", "0", "Set to 1 to never use client-side QuakeC (csprogs.dat)")
            .init_resource::<CsqcDrawList>()
            .init_resource::<CsqcScene>()
            .init_resource::<CsqcCommands>()
            .init_resource::<ConsumedInput>()
            .init_resource::<CsqcInputCursors>()
            .add_systems(Startup, draw::init_csqc_hud)
            .add_systems(PreUpdate, advance_csqc_load.run_if(resource_exists::<CsqcLoad>))
            .add_systems(
                Update,
                hooks::csqc_input_events
                    .run_if(in_state(InputFocus::Game))
                    .before(crate::client::input::systems::game_input),
            )
            .add_systems(
                PostUpdate,
                (
                    run_csqc_frame.run_if(
                        resource_exists::<Csqc>.and_then(in_state(ClientGameState::InGame)),
                    ),
                    (
                        draw::render_csqc_2d.run_if(resource_changed::<CsqcDrawList>),
                        draw::apply_csqc_scene,
                        draw::hide_view_model.run_if(csqc_draws_scene),
                    ),
                )
                    .chain()
                    .after(crate::client::view::apply_view_effects)
                    .before(bevy::transform::TransformSystems::Propagate)
                    .before(crate::client::text::systems::update_atlas_text),
            );
    }
}

#[cfg(test)]
mod tests {
    use std::{any::TypeId, collections::HashSet};

    use bevy_mod_scripting::bindings::AppScriptFunctionRegistry;
    use bevy_mod_scripting_qcvm::qcvm::{BuiltinAddress as _, strum::VariantArray as _};

    use super::*;
    use crate::common::qc::{SsqcBuiltins, core_builtins::CORE_BUILTINS};

    #[test]
    fn mode_is_detected_from_entry_points() {
        let defines = |names: &'static [&'static str]| move |name: &str| names.contains(&name);

        assert_eq!(CsqcMode::detect(defines(&["CSQC_UpdateView"])), Some(CsqcMode::Full));
        // `CSQC_UpdateView` takes precedence, as in FTEQW.
        assert_eq!(
            CsqcMode::detect(defines(&["CSQC_UpdateView", "CSQC_DrawHud"])),
            Some(CsqcMode::Full)
        );
        assert_eq!(CsqcMode::detect(defines(&["CSQC_DrawHud"])), Some(CsqcMode::Simple));
        assert_eq!(CsqcMode::detect(defines(&["CSQC_DrawScores"])), Some(CsqcMode::Simple));
        assert_eq!(CsqcMode::detect(defines(&["CSQC_Init", "CSQC_InputEvent"])), None);
    }

    fn registered(world: &World, namespace: TypeId, name: &str) -> bool {
        world
            .resource::<AppScriptFunctionRegistry>()
            .read()
            .get_function(namespace, name.to_owned())
            .is_ok()
    }

    #[test]
    fn every_core_csqc_builtin_is_registered() {
        let mut world = World::new();
        register_builtins(&mut world);

        let names = qcvm_defs_csqc::BuiltinAddr::VARIANTS
            .iter()
            .map(|addr| addr.name())
            .chain(qcvm_defs_csqc::builtins::NAMED_BUILTINS.iter().copied());
        for name in names {
            assert!(registered(&world, TypeId::of::<CsqcBuiltins>(), name), "{name}");
        }
    }

    #[test]
    fn core_builtins_are_shared_and_not_stubbed() {
        let mut world = World::new();
        register_builtins(&mut world);
        crate::server::qc::builtins::register(&mut world);

        let stubs = stubs::STUB_BUILTINS.iter().copied().collect::<HashSet<_>>();
        assert_eq!(stubs.len(), stubs::STUB_BUILTINS.len(), "a builtin is stubbed twice");
        assert_eq!(stubs.len() + CORE_BUILTINS.len(), 289);

        for name in CORE_BUILTINS {
            assert!(!stubs.contains(name), "{name} is both a core builtin and a stub");
            assert!(registered(&world, TypeId::of::<SsqcBuiltins>(), name), "{name}");
            assert!(registered(&world, TypeId::of::<CsqcBuiltins>(), name), "{name}");
        }
    }
}
