#[cfg(feature = "dev-tools")]
mod dev;
mod menu;

use std::{path::PathBuf, process::ExitCode, time::Duration};

use bevy::{
    app::ScheduleRunnerPlugin,
    audio::AudioPlugin,
    camera::{Exposure, Hdr},
    core_pipeline::tonemapping::Tonemapping,
    post_process::bloom::Bloom,
    prelude::*,
    render::view::ColorGrading,
    window::{PresentMode, PrimaryWindow},
};
use bevy_mod_commands::{RegisterCmdExt as _, RunCmd};
use bevy_mod_mdl::MdlPlugin;
use bevy_mod_pakfile::PakfilePlugin;
use bevy_mod_spr::SprPlugin;
use bevy_seedling::spatial::SpatialListener3D;
use clap::Parser;
use seismon::{
    client::{
        SeismonClientPlugin, SeismonGameSettings, check_game_dirs, default_overlay_camera,
        default_world_camera,
    },
    common::{Template, console::StuffCmds, default_base_dir},
    listen::SeismonListenServerPlugin,
};
use serde_lexpr::Value;

#[derive(Parser, Debug)]
struct Opt {
    #[arg(long, short = 'd')]
    base_dir: Option<PathBuf>,

    #[arg(long)]
    game: Option<String>,

    /// Run the game in headless mode - i.e. do not spawn a window.
    ///
    /// Will run for the number of seconds specified and then quit.
    #[arg(long)]
    headless: Option<f32>,

    commands: Vec<String>,
}

const DEFAULT_EXPOSURE: Exposure = Exposure { ev100: 9. };

fn cmd_exposure(In(val): In<Value>, mut exposures: Query<(&mut Exposure, Has<Template>)>) {
    let new_exposure = match val.as_name() {
        Some("indoor") => Exposure::INDOOR,
        Some("blender") => Exposure::BLENDER,
        Some("sunlight") => Exposure::SUNLIGHT,
        Some("overcast") => Exposure::OVERCAST,
        Some("default") => DEFAULT_EXPOSURE,
        _ => match serde_lexpr::from_value(&val) {
            Ok(exposure) => Exposure { ev100: exposure },
            Err(_) => {
                // TODO: Error handling
                return;
            }
        },
    };

    for (mut exposure, _) in &mut exposures {
        *exposure = new_exposure;
    }
}

fn cmd_saturation(
    In(saturation): In<Value>,
    mut gradings: Query<(&mut ColorGrading, Has<Template>)>,
) {
    let saturation: f32 = match serde_lexpr::from_value(&saturation) {
        Ok(saturation) => saturation,
        Err(_) => {
            // TODO: Error handling
            return;
        }
    };

    for (mut grading, _) in &mut gradings {
        grading.highlights.saturation = saturation;
        grading.midtones.saturation = saturation;
        grading.shadows.saturation = saturation;
    }
}

fn cmd_postsaturation(
    In(saturation): In<Value>,
    mut gradings: Query<(&mut ColorGrading, Has<Template>)>,
) {
    let saturation: f32 = match serde_lexpr::from_value(&saturation) {
        Ok(saturation) => saturation,
        Err(_) => {
            // TODO: Error handling
            return;
        }
    };

    for (mut grading, _) in &mut gradings {
        grading.global.post_saturation = saturation;
    }
}

fn cmd_gamma(In(gamma): In<Value>, mut gradings: Query<(&mut ColorGrading, Has<Template>)>) {
    let gamma: f32 = match serde_lexpr::from_value(&gamma) {
        Ok(gamma) => gamma,
        Err(_) => {
            // TODO: Error handling
            return;
        }
    };

    for (mut grading, _) in &mut gradings {
        grading.highlights.gamma = gamma;
        grading.midtones.gamma = gamma;
        grading.shadows.gamma = gamma;
    }
}

fn cmd_tonemapping(In(new_tonemapping): In<Value>, mut tonemapping: Query<&mut Tonemapping>) {
    let new_tonemapping = match new_tonemapping.as_name().map(|s| s.to_lowercase()).as_deref() {
        Some("tmmf") => Tonemapping::TonyMcMapface,
        Some("agx") => Tonemapping::AgX,
        Some("aces") => Tonemapping::AcesFitted,
        Some("blender") => Tonemapping::BlenderFilmic,
        Some("sbdt") => Tonemapping::SomewhatBoringDisplayTransform,
        // TODO: Should we rename the setting to "linear" to match Bevy?
        Some("none") => Tonemapping::Linear,
        _ => {
            // TODO: Error handling
            return;
        }
    };

    for mut tonemapping in &mut tonemapping {
        *tonemapping = new_tonemapping;
    }
}

fn cmd_gametitle(In(new_name): In<Value>, mut window: Query<&mut Window, With<PrimaryWindow>>) {
    if let (Some(new_name), Ok(mut window)) = (new_name.as_name(), window.single_mut()) {
        window.title = new_name.to_owned();
    }
}

fn startup(
    opt: Opt,
) -> impl FnMut(Commands, ResMut<StuffCmds>, ResMut<SeismonGameSettings>, MessageWriter<RunCmd<'static>>)
{
    move |mut commands,
          mut stuffcmds: ResMut<StuffCmds>,
          mut game_settings: ResMut<SeismonGameSettings>,
          mut console_cmds| {
        let quake_camera_bundle =
            (Hdr, Tonemapping::TonyMcMapface, Exposure::BLENDER, Msaa::Sample2, Bloom::default());

        let world_camera_bundle =
            (default_world_camera(), SpatialListener3D, quake_camera_bundle.clone());
        let overlay_camera_bundle = (default_overlay_camera(), quake_camera_bundle);

        // main game camera
        let world_camera_template = commands.spawn(world_camera_bundle).id();
        // overlay camera for viewmodel/HUD
        let overlay_camera_template = commands.spawn(overlay_camera_bundle).id();

        game_settings.camera_templates.world = Some(world_camera_template);
        game_settings.camera_templates.overlay = Some(overlay_camera_template);

        console_cmds.write(RunCmd::parse("exec quake.rc").unwrap());

        let mut commands = opt.commands.iter();
        let mut next = commands.next();
        while let Some(cur) = next {
            if let Some(rest) = cur.strip_prefix("+") {
                let mut cmd = rest.to_string();
                loop {
                    next = commands.next();

                    if let Some(arg) = next {
                        if arg.starts_with("+") {
                            break;
                        }

                        cmd.push(' ');
                        cmd.push_str(arg);
                    } else {
                        break;
                    }
                }

                let runcmd = match RunCmd::parse(&cmd) {
                    Ok(cmd) => cmd.into_owned(),
                    Err(e) => {
                        warn!("Couldn't parse cmd {:?}: {}", cmd, e);
                        continue;
                    }
                };

                stuffcmds.0.push(runcmd);
            } else {
                warn!("Arg without command: {}", cur);
                next = commands.next();
            }
        }
    }
}

fn main() -> ExitCode {
    let opt = Opt::parse();

    let base_dir = opt.base_dir.clone().unwrap_or_else(default_base_dir);
    if let Err(e) = check_game_dirs(&base_dir, opt.game.as_deref()) {
        // Logging isn't set up until the app is built, so print directly.
        eprintln!("error: {e}");
        return ExitCode::FAILURE;
    }

    let mut app = App::new();
    let mut default_plugins = DefaultPlugins
        .set(WindowPlugin {
            primary_window: Some(bevy::window::Window {
                title: "Seismon".into(),
                name: Some("seismon-engine".into()),
                resolution: (1366, 768).into(),
                present_mode: PresentMode::AutoVsync,
                // Tells wasm not to override default event handling, like F5, Ctrl+R etc.
                prevent_default_event_handling: false,
                ..default()
            }),
            ..default()
        })
        .set(ImagePlugin::default_nearest());

    if let Some(secs) = &opt.headless {
        default_plugins =
            default_plugins.set(ScheduleRunnerPlugin::run_loop(Duration::from_secs_f32(*secs)));
    }

    let default_plugins =
        default_plugins.disable::<AudioPlugin>().add_group(bevy_seedling::SeedlingPlugins);

    let games = [Some("id1"), opt.game.as_deref()].into_iter().flatten().map(|game| {
        opt.base_dir
            .as_ref()
            .map(PathBuf::from)
            .unwrap_or_else(|| std::env::current_dir().unwrap())
            .join(game)
    });

    #[derive(Parser)]
    #[command(name = "screenshot", about = "Take a screenshot and save it to a file")]
    struct Screenshot {
        path: String,
    }

    app
        // TODO: Respect game dir.
        // TODO: Use `BEVY_ASSET_ROOT`
        .add_plugins(PakfilePlugin::from_paths(games))
        .add_plugins(default_plugins)
        // TODO: These shouldn't be here, we should have `SeismonCorePlugin`
        .add_plugins(MdlPlugin::default())
        .add_plugins(SprPlugin::default())
        .add_plugins(SeismonClientPlugin{
            base_dir: opt.base_dir.clone(),
            game: opt.game.clone(),
            main_menu: menu::build_main_menu,
        })
        .add_plugins(SeismonListenServerPlugin)
        .command(|In(Screenshot { path }), mut commands: Commands| {
            use bevy::render::view::window::screenshot::{Screenshot, save_to_disk};

            commands.spawn(Screenshot::primary_window())
               .observe(save_to_disk(path));
        })
        .cvar_on_set(
            "cl_title",
            "Quake",
            cmd_gametitle,
            "Set the title of the window",
        )
        .cvar_on_set(
            "r_exposure",
            "default",
            cmd_exposure,
            "Set the physically-based exposure of the screen: indoor, sunlight, overcast, blender, or a specific ev100 value",
        )
        .cvar_on_set(
            "gamma",
            "1",
            cmd_gamma,
            "Adjust the gamma of the screen",
        )
        .cvar_on_set(
            "r_saturation",
            "1",
            cmd_saturation,
            "Adjust the color saturation of the screen",
        )
        .cvar_on_set(
            "r_postsaturation",
            "1",
            cmd_postsaturation,
            "Adjust the color saturation of the screen (applied after tonemapping)",
        )
        .cvar_on_set(
            "r_tonemapping",
            "sbdt",
            cmd_tonemapping,
            "Set the tonemapping type - Tony McMapFace (tmmf), AgX, ACES, Blender Filmic (blender), Somewhat Boring Display Transform (sbdt), or none",
        )// .insert_resource(DefaultOpaqueRendererMethod::deferred())
        .add_systems(Startup, startup(opt));

    #[cfg(feature = "screen-record")]
    app.add_plugins(bevy_dev_tools::EasyScreenRecordPlugin {
        toggle: bevy::input::keyboard::KeyCode::F12,
        ..default()
    });

    #[cfg(feature = "dev-tools")]
    app.add_plugins(dev::DevtoolsPlugins);

    app.run();

    0.into()
}
