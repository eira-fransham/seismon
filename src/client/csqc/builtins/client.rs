//! CSQC builtins about the client's state: stats, players, the server, key bindings, input, the
//! window and models. These follow FTEQW's `pr_csqc.c` and `pr_clcmd.c`.

use bevy::{prelude::*, window::PrimaryWindow};
use bevy_mod_scripting::bindings::{NamespaceBuilder, V};
use bevy_mod_scripting_qcvm::{QCString, builtin_argc};

use crate::{
    client::{
        Connection, InputHistory, SeismonGameSettings,
        commands::LocalServerActive,
        csqc::{
            hooks::{self, CsqcCommands, CsqcCursor, CsqcSensitivity},
            models, set_global_if_defined,
        },
        input::{InputFocus, game::GameInput},
        state::{ClientState, PlayerInfo, Worldspawn},
    },
    common::{
        console::ConsoleOutput,
        qc::{
            QcStorage, QcValue, QcVmCtx,
            ext::{qc_string, strings::atof},
            storage::ref_entity,
            stub::{Ent, S, string},
            text::com_parse,
            with_vm_ctx,
        },
    },
};

/// The state of the client's level, if there is one.
fn client_state(world: &World) -> Option<&ClientState> {
    world.get_resource::<Connection>()?.client_state()
}

/// A string to return, which is null if it's empty (as FTEQW returns for missing keys).
fn non_empty(s: impl AsRef<[u8]>) -> V<QCString> {
    let s = s.as_ref();
    if s.is_empty() { V(QCString::null()) } else { qc_string(s) }
}

/// Player number `number` for `getplayerkeyvalue`: negative numbers are positions on the
/// scoreboard, sorted by frags (`-1` is the leader).
fn player_slot(players: &[PlayerInfo], number: f32) -> Option<usize> {
    let number = number as i64;
    if number >= 0 {
        return Some(number as usize);
    }
    let mut sorted = players
        .iter()
        .enumerate()
        .filter(|(_, player)| !player.name.raw.is_empty())
        .collect::<Vec<_>>();
    sorted.sort_by_key(|(_, player)| std::cmp::Reverse(player.frags));
    sorted.get((-number - 1) as usize).map(|(slot, _)| *slot)
}

/// `getplayerkeyvalue`'s value for `key` of player slot `slot`, or empty if there isn't one.
pub fn player_key(players: &[PlayerInfo], slot: usize, key: &str) -> String {
    let Some(player) = players.get(slot) else {
        return String::new();
    };
    if key == "viewentity" {
        return (slot + 1).to_string();
    }
    if player.name.raw.is_empty() {
        return String::new();
    }
    match key {
        "name" => player.name.to_str().into_owned(),
        "frags" => player.frags.to_string(),
        "userid" => (slot + 1).to_string(),
        "topcolor" => player.colors.top().to_string(),
        "bottomcolor" => player.colors.bottom().to_string(),
        "colors" => player.colors.bits().to_string(),
        // Quake servers don't send pings or packet loss.
        "ping" | "pl" => "0".to_owned(),
        _ => String::new(),
    }
}

/// `serverkey`'s value for `key`, or empty if there isn't one.
pub fn server_key(world: &World, key: &str) -> String {
    let connection = world.get_resource::<Connection>();
    let state = connection.and_then(Connection::client_state);
    match key {
        "constate" => match connection {
            Some(conn) if conn.is_connected() => "active".to_owned(),
            Some(_) => "connecting".to_owned(),
            None => "disconnected".to_owned(),
        },
        "maxplayers" => state.map(|s| s.max_clients.to_string()).unwrap_or_default(),
        "deathmatch" => {
            state.map(|s| if s.deathmatch { "1" } else { "0" }.to_owned()).unwrap_or_default()
        }
        // The map's path is model 1.
        "map" => state
            .and_then(|s| s.models.get(1).and_then(|m| m.name.as_ref()))
            .map(|path| {
                let name = path.strip_prefix("maps/").unwrap_or(path);
                name.strip_suffix(".bsp").unwrap_or(name).to_owned()
            })
            .unwrap_or_default(),
        "pausestate" => "0".to_owned(),
        _ => String::new(),
    }
}

/// The binding of the input with key number `key`, as text.
fn key_binding(world: &World, key: f32) -> String {
    let Some(input) = hooks::number_input(key as u32) else {
        return String::new();
    };
    let Some(game_input) = world.get_resource::<GameInput>() else {
        return String::new();
    };
    game_input
        .bindings
        .get(&input)
        .map(|binding| {
            binding.commands.iter().map(ToString::to_string).collect::<Vec<_>>().join("; ")
        })
        .unwrap_or_default()
}

/// The key numbers of the keys bound to exactly `command`, in order.
fn keys_for_command(world: &World, command: &str) -> Vec<u32> {
    let Some(game_input) = world.get_resource::<GameInput>() else {
        return Vec::new();
    };
    let mut keys = game_input
        .bindings
        .iter()
        .filter(|(_, binding)| {
            binding.commands.iter().map(ToString::to_string).collect::<Vec<_>>().join("; ")
                == command
        })
        .filter_map(|(input, _)| hooks::input_number(input))
        .collect::<Vec<_>>();
    keys.sort_unstable();
    keys
}

/// `findkeysforcommand`'s result: the key numbers in quotes, with at least two (`-1` for none).
pub fn format_keys_for_command(keys: &[u32]) -> String {
    let mut out = String::new();
    for i in 0..keys.len().max(2) {
        let key = keys.get(i).map_or(-1, |&k| k as i64);
        out.push_str(&format!(" '{key}'"));
    }
    out
}

/// Sets the `input_*` globals to the move command `sequence`, like `getinputstate`.
fn get_input_state(ctx: &mut QcVmCtx<'_>, sequence: f32) -> f32 {
    let Some(frame) = ctx
        .world
        .get_resource::<InputHistory>()
        .and_then(|history| history.get(sequence.max(0.) as u32).copied())
    else {
        return 0.;
    };
    let storage = QcStorage(ctx.storage.clone());
    for (name, value) in [
        ("input_timelength", QcValue::Float(frame.timelength)),
        ("input_angles", QcValue::Vector(frame.angles)),
        ("input_movevalues", QcValue::Vector(frame.movevalues)),
        ("input_buttons", QcValue::Float(frame.buttons as f32)),
        ("input_impulse", QcValue::Float(f32::from(frame.impulse))),
    ] {
        set_global_if_defined(ctx.world, &storage, name, value);
    }
    1.
}

/// Where `getentitytoken` is in its string, on the CSQC worldspawn.
#[derive(Component, Debug, Default)]
pub struct EntityTokens {
    data: Option<Vec<u8>>,
    pos: usize,
}

/// The entity lump of the client's map.
fn map_entities(world: &World) -> Vec<u8> {
    let Some(state) = client_state(world) else {
        return Vec::new();
    };
    let Some(worldspawn) = world.get::<Worldspawn>(state.worldspawn) else {
        return Vec::new();
    };
    world
        .get_resource::<Assets<bevy_trenchbroom::bsp::Bsp>>()
        .and_then(|bsps| bsps.get(&worldspawn.bsp))
        .map(|bsp| bsp.data.entities.clone())
        .unwrap_or_default()
}

/// The game directories under the base directory, for `getgamedirinfo`: directories with
/// pakfiles, a `progs.dat` or maps.
fn game_dirs(world: &World) -> Vec<String> {
    let Some(settings) = world.get_resource::<SeismonGameSettings>() else {
        return Vec::new();
    };
    let Ok(entries) = std::fs::read_dir(&settings.base_dir) else {
        return Vec::new();
    };
    let mut dirs = entries
        .flatten()
        .filter(|entry| entry.file_type().is_ok_and(|kind| kind.is_dir()))
        .filter(|entry| {
            let path = entry.path();
            path.join("progs.dat").is_file()
                || path.join("maps").is_dir()
                || std::fs::read_dir(&path).is_ok_and(|files| {
                    files.flatten().any(|f| {
                        f.path().extension().is_some_and(|ext| ext.eq_ignore_ascii_case("pak"))
                    })
                })
        })
        .map(|entry| entry.file_name().to_string_lossy().into_owned())
        .collect::<Vec<_>>();
    dirs.sort();
    dirs
}

fn lossy(s: &S) -> String {
    String::from_utf8_lossy(string(s)).into_owned()
}

pub fn register<N: 'static>(ns: &mut NamespaceBuilder<'_, N>) {
    ns.register("getstati", |stat: f32| {
        with_vm_ctx(|ctx| {
            Ok(client_state(ctx.world)
                .and_then(|s| s.stats.get(stat.max(0.) as usize).copied())
                .unwrap_or(0) as f32)
        })
    })
    .register("getstatf", |stat: f32, first_bit: f32, bit_count: f32| {
        let argc = builtin_argc();
        with_vm_ctx(|ctx| {
            let value = client_state(ctx.world)
                .and_then(|s| s.stats.get(stat.max(0.) as usize).copied())
                .unwrap_or(0);
            if argc <= 1 {
                return Ok(value as f32);
            }
            let count = if argc > 2 { bit_count as u32 } else { 1 };
            let bits = (value as u32).checked_shr(first_bit as u32).unwrap_or(0);
            let mask = 1u32.checked_shl(count).map_or(u32::MAX, |bit| bit - 1);
            Ok((bits & mask) as f32)
        })
    })
    // Quake servers have no string stats.
    .register("getstats", |_stat: f32| V(QCString::null()))
    .register("getplayerkeyvalue", |player: f32, key: S| {
        with_vm_ctx(|ctx| {
            let players = client_state(ctx.world).map(|s| s.players.as_slice()).unwrap_or_default();
            Ok(match player_slot(players, player) {
                Some(slot) => non_empty(player_key(players, slot, &lossy(&key))),
                None => V(QCString::null()),
            })
        })
    })
    .register("getplayerkeyfloat", |player: f32, key: S, assume: f32| {
        let argc = builtin_argc();
        with_vm_ctx(|ctx| {
            let players = client_state(ctx.world).map(|s| s.players.as_slice()).unwrap_or_default();
            let value = player_slot(players, player)
                .map(|slot| player_key(players, slot, &lossy(&key)))
                .unwrap_or_default();
            Ok(if !value.is_empty() {
                atof(value.as_bytes())
            } else if argc > 2 {
                assume
            } else {
                0.
            })
        })
    })
    .register("serverkey", |key: S| {
        with_vm_ctx(|ctx| Ok(non_empty(server_key(ctx.world, &lossy(&key)))))
    })
    .register("serverkeyfloat", |key: S, assume: f32| {
        let argc = builtin_argc();
        with_vm_ctx(|ctx| {
            let value = server_key(ctx.world, &lossy(&key));
            Ok(if !value.is_empty() {
                atof(value.as_bytes())
            } else if argc > 1 {
                assume
            } else {
                0.
            })
        })
    })
    .register("isdemo", || {
        with_vm_ctx(|ctx| {
            let demo = ctx.world.get_resource::<Connection>().is_some_and(Connection::is_demo);
            Ok(if demo { 1. } else { 0. })
        })
    })
    .register("isserver", || {
        with_vm_ctx(|ctx| {
            let server = ctx
                .world
                .get_resource::<LocalServerActive>()
                .copied()
                .is_some_and(|LocalServerActive(active)| active(ctx.world));
            Ok(if server { 1. } else { 0. })
        })
    })
    .register("keynumtostring", |key: f32| {
        qc_string(hooks::key_name(key as u32).unwrap_or_else(|| "<UNKNOWN KEYNUM>".to_owned()))
    })
    .register("stringtokeynum", |name: S| {
        lossy(&name)
            .parse::<crate::client::input::game::AnyInput>()
            .ok()
            .and_then(|input| hooks::input_number(&input))
            .map_or(-1., |number| number as f32)
    })
    .register("getkeybind", |key: f32, _bindmap: f32, _modifier: f32| {
        with_vm_ctx(|ctx| Ok(qc_string(key_binding(ctx.world, key))))
    })
    .register("setkeybind", |key: f32, binding: S, bindmap: f32, _modifier: f32| {
        with_vm_ctx(|ctx| {
            // There's only one bind map.
            if bindmap != 0. {
                return Ok(0.);
            }
            let Some(input) = hooks::number_input(key as u32) else {
                return Ok(0.);
            };
            let Some(mut game_input) = ctx.world.get_resource_mut::<GameInput>() else {
                return Ok(0.);
            };
            let binding = lossy(&binding);
            if binding.is_empty() {
                game_input.bindings.remove(&input);
            } else if let Err(e) = game_input.bind(input, &binding) {
                warn!("setkeybind: {e}");
                return Ok(0.);
            }
            Ok(1.)
        })
    })
    .register("findkeysforcommand", |command: S, _bindmap: f32| {
        with_vm_ctx(|ctx| {
            Ok(qc_string(format_keys_for_command(&keys_for_command(ctx.world, &lossy(&command)))))
        })
    })
    .register("findkeysforcommandex", |command: S, _bindmap: f32| {
        with_vm_ctx(|ctx| {
            let names = keys_for_command(ctx.world, &lossy(&command))
                .into_iter()
                .filter_map(hooks::key_name)
                .collect::<Vec<_>>();
            Ok(qc_string(names.join(" ")))
        })
    })
    .register("getbindmaps", || [0., 0., 0.])
    .register(
        "setbindmaps",
        |maps: crate::common::qc::stub::Vec3Arg| {
            if maps[0] == 0. && maps[1] == 0. { 1. } else { 0. }
        },
    )
    .register("getinputstate", |sequence: f32| {
        with_vm_ctx(|ctx| Ok(get_input_state(ctx, sequence)))
    })
    .register(
        "setcursormode",
        |enabled: f32, _image: S, _hotspot: crate::common::qc::stub::Vec3Arg, _scale: f32| {
            with_vm_ctx(|ctx| {
                ctx.world.insert_resource(CsqcCursor { enabled: enabled != 0. });
                Ok(())
            })
        },
    )
    .register("getcursormode", |effective: f32| {
        with_vm_ctx(|ctx| {
            let enabled = ctx.world.get_resource::<CsqcCursor>().is_some_and(|c| c.enabled);
            let free = enabled
                || (effective != 0.
                    && ctx
                        .world
                        .get_resource::<State<InputFocus>>()
                        .is_some_and(|focus| *focus.get() != InputFocus::Game));
            Ok(if free { 1. } else { 0. })
        })
    })
    .register("setsensitivityscaler", |scale: f32| {
        with_vm_ctx(|ctx| {
            ctx.world.insert_resource(CsqcSensitivity(scale));
            Ok(())
        })
    })
    .register("getresolution", |mode: f32, _fullscreen: f32| {
        with_vm_ctx(|ctx| {
            // Only the current mode is known.
            if mode != -1. && mode != 0. {
                return Ok([0., 0., 0.]);
            }
            let mut windows = ctx.world.query_filtered::<&Window, With<PrimaryWindow>>();
            let size = windows.iter(ctx.world).next().map(|w| w.physical_size().as_vec2());
            Ok(match size {
                Some(size) if size.x > 0. && size.y > 0. => [size.x, size.y, 1.],
                _ => [0., 0., 0.],
            })
        })
    })
    .register("setwindowcaption", |caption: S| {
        with_vm_ctx(|ctx| {
            let caption = lossy(&caption);
            let caption = if caption.is_empty() { "Seismon".to_owned() } else { caption };
            let mut windows = ctx.world.query_filtered::<&mut Window, With<PrimaryWindow>>();
            for mut window in windows.iter_mut(ctx.world) {
                window.title = caption.clone();
            }
            Ok(())
        })
    })
    .register("getgamedirinfo", |n: f32, property: f32| {
        with_vm_ctx(|ctx| {
            let dirs = game_dirs(ctx.world);
            Ok(match dirs.get(n.max(0.) as usize) {
                // The name and the description (which is the name, as there's no `modinfo`).
                Some(dir) if property == 0. || property == 1. => qc_string(dir),
                Some(_) => qc_string(""),
                None => V(QCString::null()),
            })
        })
    })
    .register("gettimed", |kind: f32| {
        with_vm_ctx(|ctx| {
            // FTEQW's `GETTIME_*`: 5 is the simulation time, and the rest are real time.
            Ok(if kind == 5. {
                ctx.world.resource::<Time<Virtual>>().elapsed_secs_f64() as f32
            } else {
                ctx.world.resource::<Time<Real>>().elapsed_secs_f64() as f32
            })
        })
    })
    .register("registercommand", |name: S, _description: S| {
        with_vm_ctx(|ctx| {
            ctx.world.get_resource_or_init::<CsqcCommands>().0.insert(lossy(&name));
            Ok(())
        })
    })
    .register("cprint", |a: S, b: S, c: S, d: S, e: S, f: S, g: S, h: S| {
        let args = [&a, &b, &c, &d, &e, &f, &g, &h];
        let count = builtin_argc().clamp(1, args.len());
        let text = args[..count].iter().flat_map(|s| string(s)).copied().collect::<Vec<_>>();
        with_vm_ctx(|ctx| {
            let time = *ctx.world.resource::<Time<Real>>();
            if let Some(mut console) = ctx.world.get_resource_mut::<ConsoleOutput>() {
                console.set_center_print(text, &time);
            }
            Ok(())
        })
    })
    .register("getentitytoken", |reset: S| {
        let reset = (builtin_argc() > 0).then(|| string(&reset).to_vec());
        with_vm_ctx(|ctx| {
            let map = (reset.as_deref().is_none_or(<[u8]>::is_empty)
                && ctx.world.get::<EntityTokens>(ctx.root).is_none_or(|t| t.data.is_none()))
            .then(|| map_entities(ctx.world));

            let mut entity = ctx.world.entity_mut(ctx.root);
            let mut entry = entity.entry::<EntityTokens>().or_default();
            let mut tokens = entry.get_mut();
            if let Some(reset) = reset {
                // An empty string resets to the map's entities.
                tokens.data = if reset.is_empty() { map } else { Some(reset) };
                tokens.pos = 0;
                return Ok(V(QCString::null()));
            }
            if tokens.data.is_none() {
                tokens.data = map;
            }
            let data = tokens.data.as_deref().unwrap_or_default();
            Ok(match com_parse(data, tokens.pos, false) {
                Some(token) => {
                    tokens.pos = token.end;
                    qc_string(token.text)
                }
                None => V(QCString::null()),
            })
        })
    })
    .register("modelnameforindex", |index: f32| {
        with_vm_ctx(|ctx| {
            Ok(match models::model_name(ctx.world, ctx.root, index as i32) {
                Some(name) => qc_string(name),
                None => V(QCString::null()),
            })
        })
    })
    .register("setmodelindex", |e: Ent, index: f32| {
        with_vm_ctx(|ctx| {
            let entity = ref_entity(&e)?;
            let name = models::model_name(ctx.world, ctx.root, index as i32).unwrap_or_default();
            ctx.set_field(entity, "modelindex", QcValue::Float(index.trunc()))?;
            ctx.set_field(entity, "model", QcValue::String(QCString::from(&*name)))?;
            Ok(())
        })
    })
    .register("frameforname", |model: f32, name: S| {
        with_vm_ctx(|ctx| {
            let name = lossy(&name);
            Ok(models::mdl(ctx.world, ctx.root, model as i32)
                .and_then(|mdl| mdl.frames().iter().position(|frame| frame.name == name))
                .map_or(-1., |frame| frame as f32))
        })
    })
    .register("frametoname", |model: f32, frame: f32| {
        with_vm_ctx(|ctx| {
            Ok(
                match models::mdl(ctx.world, ctx.root, model as i32)
                    .and_then(|mdl| mdl.frames().get(frame.max(0.) as usize))
                {
                    Some(frame) => qc_string(&frame.name),
                    None => V(QCString::null()),
                },
            )
        })
    })
    .register("frameduration", |model: f32, frame: f32| {
        with_vm_ctx(|ctx| {
            Ok(models::mdl(ctx.world, ctx.root, model as i32)
                .and_then(|mdl| mdl.frames().get(frame.max(0.) as usize))
                .map_or(0., |frame| frame.duration_secs))
        })
    });
}

#[cfg(test)]
mod tests {
    use seismon_net::PlayerColor;

    use super::*;

    fn player(name: &str, frags: i32) -> PlayerInfo {
        PlayerInfo { name: name.to_owned().into(), frags, colors: PlayerColor::new(3, 4) }
    }

    #[test]
    fn negative_players_are_sorted_by_frags() {
        let players = [player("a", 1), PlayerInfo::default(), player("c", 5)];
        assert_eq!(player_slot(&players, 1.), Some(1));
        assert_eq!(player_slot(&players, -1.), Some(2));
        assert_eq!(player_slot(&players, -2.), Some(0));
        assert_eq!(player_slot(&players, -3.), None);
    }

    #[test]
    fn player_keys() {
        let players = [player("bob", 7), PlayerInfo::default()];
        assert_eq!(player_key(&players, 0, "name"), "bob");
        assert_eq!(player_key(&players, 0, "frags"), "7");
        assert_eq!(player_key(&players, 0, "topcolor"), "3");
        assert_eq!(player_key(&players, 0, "bottomcolor"), "4");
        assert_eq!(player_key(&players, 0, "viewentity"), "1");
        // Empty slots only have a view entity.
        assert_eq!(player_key(&players, 1, "name"), "");
        assert_eq!(player_key(&players, 1, "viewentity"), "2");
        assert_eq!(player_key(&players, 5, "name"), "");
    }

    #[test]
    fn keys_for_commands_are_padded() {
        assert_eq!(format_keys_for_command(&[]), " '-1' '-1'");
        assert_eq!(format_keys_for_command(&[32, 513, 9]), " '32' '513' '9'");
    }

    #[test]
    fn key_names_round_trip() {
        for number in [32, 9, 97, 135, 512] {
            let name = hooks::key_name(number).unwrap();
            let input: crate::client::input::game::AnyInput = name.parse().unwrap();
            assert_eq!(hooks::input_number(&input), Some(number), "{name}");
        }
    }
}
