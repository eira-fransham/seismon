#[cfg(feature = "dev_tools")]
mod dev;
mod menu;

use std::{path::PathBuf, process::ExitCode};

use bevy::{
    audio::AudioPlugin,
    camera::Exposure,
    core_pipeline::tonemapping::Tonemapping,
    ecs::entity_disabling::Disabled,
    post_process::bloom::Bloom,
    prelude::*,
    render::view::{ColorGrading, Hdr},
    window::{PresentMode, PrimaryWindow},
};
#[cfg(feature = "capture")]
use bevy_capture::{
    Capture, CaptureBundle, CapturePlugin,
    encoder::{gif::GifEncoder, mp4_openh264::Mp4Openh264Encoder},
};
use bevy_mod_mdl::MdlPlugin;
use bevy_mod_pakfile::PakfilePlugin;
use bevy_seedling::spatial::SpatialListener3D;
use clap::Parser;
use seismon::{
    client::{SeismonClientPlugin, SeismonGameSettings},
    common::console::{ConsoleInput, RegisterCmdExt as _, RunCmd},
    server::SeismonListenServerPlugin,
};
use serde_lexpr::Value;

#[derive(Parser, Debug)]
struct Opt {
    #[arg(long)]
    base_dir: Option<PathBuf>,

    #[arg(long)]
    game: Option<String>,

    commands: Vec<String>,
}

fn cmd_exposure(In(val): In<Value>, mut exposures: Query<&mut Exposure>) {
    let new_exposure = match val.as_name() {
        Some("indoor") => Exposure::INDOOR,
        Some("blender") => Exposure::BLENDER,
        Some("sunlight") => Exposure::SUNLIGHT,
        Some("overcast") => Exposure::OVERCAST,
        _ => match serde_lexpr::from_value(&val) {
            Ok(exposure) => Exposure { ev100: exposure },
            Err(_) => {
                // TODO: Error handling
                return;
            }
        },
    };

    for mut exposure in &mut exposures {
        *exposure = new_exposure;
    }
}

fn cmd_saturation(In(saturation): In<Value>, mut gradings: Query<&mut ColorGrading>) {
    let saturation: f32 = match serde_lexpr::from_value(&saturation) {
        Ok(saturation) => saturation,
        Err(_) => {
            // TODO: Error handling
            return;
        }
    };

    for mut grading in &mut gradings {
        grading.highlights.saturation = saturation;
        grading.midtones.saturation = saturation;
        grading.shadows.saturation = saturation;
    }
}

fn cmd_postsaturation(In(saturation): In<Value>, mut gradings: Query<&mut ColorGrading>) {
    let saturation: f32 = match serde_lexpr::from_value(&saturation) {
        Ok(saturation) => saturation,
        Err(_) => {
            // TODO: Error handling
            return;
        }
    };

    for mut grading in &mut gradings {
        grading.global.post_saturation = saturation;
    }
}

fn cmd_gamma(In(gamma): In<Value>, mut gradings: Query<&mut ColorGrading>) {
    let gamma: f32 = match serde_lexpr::from_value(&gamma) {
        Ok(gamma) => gamma,
        Err(_) => {
            // TODO: Error handling
            return;
        }
    };

    for mut grading in &mut gradings {
        grading.highlights.gamma = 1. / gamma;
        grading.midtones.gamma = 1. / gamma;
        grading.shadows.gamma = 1. / gamma;
    }
}

fn cmd_tonemapping(In(new_tonemapping): In<Value>, mut tonemapping: Query<&mut Tonemapping>) {
    let new_tonemapping = match new_tonemapping.as_name() {
        Some("tmmf") => Tonemapping::TonyMcMapface,
        Some("aces") => Tonemapping::AcesFitted,
        Some("blender") => Tonemapping::BlenderFilmic,
        Some("sbdt") => Tonemapping::SomewhatBoringDisplayTransform,
        Some("none") => Tonemapping::None,
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
) -> impl FnMut(
    Commands,
    ResMut<ConsoleInput>,
    ResMut<SeismonGameSettings>,
    MessageWriter<RunCmd<'static>>,
) {
    move |mut commands,
          mut input: ResMut<ConsoleInput>,
          mut game_settings: ResMut<SeismonGameSettings>,
          mut console_cmds| {
        let camera_bundle = (
            Camera3d::default(),
            Camera::default(),
            Projection::Perspective(PerspectiveProjection {
                fov: 90_f32.to_radians(),
                ..default()
            }),
            Hdr,
            // TemporalAntiAliasing::default(),
            Transform::from_translation(Vec3::new(0.0, 22.0, 0.0)),
            Exposure::INDOOR,
            Msaa::Sample2,
            Bloom::default(),
            // DepthPrepass,
            // NormalPrepass,
            SpatialListener3D,
            #[cfg(feature = "dev_tools")]
            dev::DebugCamera,
            #[cfg(not(feature = "dev_tools"))]
            Disabled,
        );
        #[cfg(feature = "capture")]
        let camera_bundle = (camera_bundle, CaptureBundle::default());
        // main game camera
        let camera_template = commands.spawn(camera_bundle).id();

        #[cfg(not(feature = "dev_tools"))]
        {
            game_settings.camera_template = camera_template;
        }

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

                input.stuffcmds.push(runcmd);
            } else {
                warn!("Arg without command: {}", cur);
                next = commands.next();
            }
        }
    }
}

fn main() -> ExitCode {
    let opt = Opt::parse();

    let mut app = App::new();
    let default_plugins = DefaultPlugins
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

    #[cfg(feature = "capture")]
    let default_plugins =
        default_plugins.set(RenderPlugin { synchronous_pipeline_compilation: true, ..default() });

    let default_plugins =
        default_plugins.disable::<AudioPlugin>().add(bevy_seedling::SeedlingPlugin::default());

    app
        // TODO: Respect game dir.
        // TODO: Use `BEVY_ASSET_ROOT`
        .add_plugins(PakfilePlugin::from_paths([std::env::current_dir().unwrap().join("id1")]))
        .add_plugins(default_plugins)
        .add_plugins(MdlPlugin::default())
        .add_plugins(SeismonClientPlugin{
            base_dir: opt.base_dir.clone(),
            game: opt.game.clone(),
            main_menu: menu::build_main_menu,
        })
        .add_plugins(SeismonListenServerPlugin)
        .cvar_on_set(
            "cl_title",
            "Quake",
            cmd_gametitle,
            "Set the title of the window",
        )
        .cvar_on_set(
            "r_exposure",
            "indoor",
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
            "blender",
            cmd_tonemapping,
            "Set the tonemapping type - Tony McMapFace (TMMF), ACES, Blender Filmic, Somewhat Boring Display Transform (SBBT), or none",
        )// .insert_resource(DefaultOpaqueRendererMethod::deferred())
        .add_systems(Startup, startup(opt));

    #[cfg(feature = "dev_tools")]
    app.add_plugins(dev::DevtoolsPlugins);

    #[cfg(feature = "capture")]
    {
        app.add_plugins(CapturePlugin);

        #[derive(Parser)]
        #[command(name = "startcapture", about = "Starts a new capture.")]
        struct StartCapture {
            file: String,
        }

        #[derive(Parser)]
        #[command(name = "stopcapture", about = "Stops the current capture.")]
        struct StopCapture {}

        app.command(|In(StartCapture { file }), mut capture: Query<&mut Capture>| {
            let mut capture = capture.single_mut().unwrap();
            let mut filename = Path::new(&file);
            let filename = if filename.extension().is_none() {
                filename.with_extension("mp4")
            } else {
                filename.to_owned()
            };

            if !capture.is_capturing() {
                match filename.extension().and_then(OsStr::to_str) {
                    Some("mp4") => capture.start(
                        Mp4Openh264Encoder::new(File::create(filename).unwrap(), 1366, 768)
                            .unwrap(),
                    ),
                    Some("gif") => capture.start(GifEncoder::new(File::create(filename).unwrap())),
                    Some(other) => {
                        return format!("Unsupported file extension {other}",).into();
                    }
                    None => unreachable!(),
                }
            }

            default()
        })
        .command(|In(StopCapture {}), mut capture: Query<&mut Capture>| {
            let mut capture = capture.single_mut().unwrap();
            if capture.is_capturing() {
                capture.stop();
            }

            default()
        });
    }
    app.run();

    0.into()
}
