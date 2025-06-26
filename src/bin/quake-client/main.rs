// Copyright © 2018 Cormac O'Brien
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#![recursion_limit = "256"]

mod capture;
mod menu;

use std::{path::PathBuf, process::ExitCode};

use bevy::{
    core_pipeline::{
        bloom::Bloom,
        experimental::taa::TemporalAntiAliasing,
        prepass::{DepthPrepass, NormalPrepass},
        tonemapping::Tonemapping,
    },
    pbr::DefaultOpaqueRendererMethod,
    prelude::*,
    render::{
        camera::Exposure,
        view::{ColorGrading, ColorGradingGlobal},
    },
    window::{PresentMode, PrimaryWindow},
};
#[cfg(feature = "screenrecord")]
use capture::CapturePlugin;
use clap::Parser;
use seismon::{
    client::SeismonClientPlugin,
    common::console::{ConsoleInput, RegisterCmdExt as _, RunCmd},
    server::SeismonServerPlugin,
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

#[cfg(feature = "auto-exposure")]
fn cmd_autoexposure(
    In(autoexposure): In<Value>,
    mut commands: Commands,
    assets: Res<AssetServer>,
    mut cameras: Query<
        (
            Entity,
            Option<&mut bevy::core_pipeline::auto_exposure::AutoExposure>,
        ),
        With<Camera3d>,
    >,
) {
    use bevy::{
        core_pipeline::auto_exposure::{AutoExposure, AutoExposureCompensationCurve},
        math::cubic_splines::LinearSpline,
    };

    let enabled: bool = match autoexposure.as_name() {
        Some("on") => true,
        Some("off") => false,
        _ => match serde_lexpr::from_value(&autoexposure) {
            Ok(autoexposure) => autoexposure,
            Err(_) => {
                // TODO: Error handling
                return;
            }
        },
    };

    for (e, autoexposure) in &mut cameras {
        match (autoexposure, enabled) {
            (Some(mut autoexposure), false) => {
                autoexposure.range = 0f32..=0f32;
            }
            (None, true) => {
                commands.entity(e).insert(AutoExposure {
                    metering_mask: assets.load("autoexposure-mask.png"),
                    compensation_curve: assets.add(
                        AutoExposureCompensationCurve::from_curve(LinearSpline::new([
                            [-8.0, -8.0].into(),
                            [8.0, -8.0].into(),
                        ]))
                        .unwrap(),
                    ),
                    ..default()
                });
            }
            (Some(mut autoexposure), true) => {
                autoexposure.range = -8f32..=8f32;
            }
            _ => {}
        }
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
    if let (Some(new_name), Ok(mut window)) = (new_name.as_name(), window.get_single_mut()) {
        window.title = new_name.to_owned();
    }
}

fn startup(opt: Opt) -> impl FnMut(Commands, ResMut<ConsoleInput>, EventWriter<RunCmd<'static>>) {
    move |mut commands, mut input: ResMut<ConsoleInput>, mut console_cmds| {
        // main game camera
        commands.spawn((
            Camera3d::default(),
            Camera {
                hdr: true,
                ..default()
            },
            TemporalAntiAliasing::default(),
            Transform::from_translation(Vec3::new(0.0, 0.0, 5.0))
                .looking_at(Vec3::default(), Vec3::Y),
            Exposure::INDOOR,
            // In addition to the in-camera exposure, we add a post exposure grading
            // in order to adjust the brightness on the UI elements.
            ColorGrading {
                global: ColorGradingGlobal {
                    exposure: 2.,
                    ..default()
                },
                ..default()
            },
            Msaa::Off,
            Bloom::default(),
            DepthPrepass,
            NormalPrepass,
        ));

        console_cmds.send(RunCmd::parse("exec quake.rc").unwrap());

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

                let runcmd = match RunCmd::parse(&*cmd) {
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
                resolution: (1366., 768.).into(),
                present_mode: PresentMode::AutoVsync,
                // Tells wasm not to override default event handling, like F5, Ctrl+R etc.
                prevent_default_event_handling: false,
                ..default()
            }),
            ..default()
        })
        .set(ImagePlugin::default_nearest());

    let default_plugins = default_plugins
        .add(bevy_seedling::SeedlingPlugin::default());

    app
        .add_plugins(default_plugins)
        .add_plugins(SeismonClientPlugin{
            base_dir: opt.base_dir.clone(),
            game: opt.game.clone(),
            main_menu: menu::build_main_menu,
        })
        .add_plugins(SeismonServerPlugin)
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
        ).insert_resource(DefaultOpaqueRendererMethod::deferred())
        .add_systems(Startup, startup(opt));

    #[cfg(feature = "screenrecord")]
    app.add_plugins(CapturePlugin);

    #[cfg(feature = "auto-exposure")]
    app.add_plugins(bevy::core_pipeline::auto_exposure::AutoExposurePlugin)
        .cvar_on_set(
            "r_autoexposure",
            "on",
            cmd_autoexposure,
            "Enable/disable automatic exposure compensation",
        );

    app.run();

    0.into()
}
