#![cfg(feature = "screenrecord")]

use clap::Parser;
use crossbeam_channel::{Receiver, Sender};
use std::{
    collections::BTreeMap,
    path::PathBuf,
    sync::{Arc, atomic::AtomicBool},
    time::Duration,
};

use bevy::{prelude::*, window::PrimaryWindow};
use chrono::Utc;
use image::RgbImage;
use seismon::common::console::RegisterCmdExt as _;

pub struct CapturePlugin;

impl Plugin for CapturePlugin {
    fn build(&self, app: &mut App) {
        #[derive(Parser)]
        #[command(name = "screenshot", about = "Take a screenshot")]
        struct Screenshot {
            path: Option<PathBuf>,
        }

        #[derive(Parser)]
        #[command(name = "startvideo", about = "Start recording a video")]
        struct StartVideo {
            path: Option<PathBuf>,
            #[arg(long)]
            width: Option<u32>,
            #[arg(long)]
            height: Option<u32>,
        }

        #[derive(Parser)]
        #[command(name = "stopvideo", about = "Stop recording")]
        struct StopVideo;

        app.add_systems(
            Update,
            (
                systems::video_frame.run_if(resource_exists::<VideoCtx>),
                systems::recv_frame.run_if(resource_exists::<VideoCtxRecv>),
            ),
        )
        // .command(|In(Screenshot { path }), windows: Res<Entity, With<PrimaryWindow>>, mut commands: Commands| {
        //         let Ok(window) = window.get_single() else {
        //             return "Can't find primary window".to_owned().into();
        //         };
        //         let path = match path {
        //             // TODO: make default path configurable
        //             None => {
        //                 PathBuf::from(format!("richter-{}.png", Utc::now().format("%FT%H-%M-%S")))
        //             }
        //             Some(path) => path,
        //         };
        //         commands.spawn(Screenshot::window(window))
        //             .observe(save_to_disk(path));
        //         default()
        //     })
        .command(
            |In(StartVideo {
                 path,
                 width,
                 height,
             }),
             mut commands: Commands,
             window: Query<&Window, With<PrimaryWindow>>,
             ctx: Option<Res<VideoCtx>>| {
                fn ceil_to(x: u32, to: u32) -> u32 {
                    let x = x + (to - 1);
                    x - (x % to)
                }

                const LONGEST_SIDE: u32 = 800;
                const FPS: f64 = 30.;

                if ctx.is_some() {
                    return "Already recording video".into();
                }

                let mut path = match path {
                    // TODO: make default path configurable
                    None => {
                        PathBuf::from(format!("richter-{}.mp4", Utc::now().format("%FT%H-%M-%S")))
                    }
                    Some(path) => path,
                };
                if path.extension().is_none() {
                    path.set_extension("mp4");
                }

                let aspect_ratio = window
                    .get_single()
                    .map(|w| w.width() / w.height())
                    .unwrap_or(4. / 3.);
                let size = match (width, height) {
                    (Some(w), Some(h)) => [w, h],
                    (Some(w), None) => [w, (w as f32 / aspect_ratio) as u32],
                    (None, Some(h)) => [(h as f32 * aspect_ratio) as u32, h],
                    (None, None) => {
                        if aspect_ratio < 1. {
                            [(LONGEST_SIDE as f32 * aspect_ratio) as u32, LONGEST_SIDE]
                        } else {
                            [LONGEST_SIDE, (LONGEST_SIDE as f32 / aspect_ratio) as u32]
                        }
                    }
                };
                let [w, h] = size.map(|x| ceil_to(x, 10));

                let out = format!("Recording a video ({}x{}) to {}", w, h, path.display());

                let (sender, receiver) = crossbeam_channel::unbounded::<VideoFrame>();
                let frame_time = Duration::from_secs_f64(FPS.recip());

                let encoder = video_rs::Encoder::new(
                    &path.into(),
                    video_rs::EncoderSettings::for_h264_yuv420p(w as _, h as _, true),
                )
                .unwrap();

                commands.insert_resource(VideoCtx {
                    send_frame: sender,
                    size: (w, h),
                    frame_time,
                    last_time: None,
                    cur_frame: 0,
                    closed: Arc::new(false.into()),
                });

                commands.insert_resource(VideoCtxRecv {
                    recv_frame: Some(receiver),
                    frame_buf: default(),
                    encoder,
                    frame_time: video_rs::Time::from_nth_of_a_second(FPS as _),
                    cur_frame: 0,
                });

                out.into()
            },
        )
        .command(
            |In(StopVideo), mut commands: Commands, ctx: Option<Res<VideoCtx>>| {
                if ctx.is_some() {
                    commands.remove_resource::<VideoCtx>();
                    default()
                } else {
                    "Error: no video recording in progress".into()
                }
            },
        );
    }
}

struct VideoFrame {
    image: RgbImage,
    frame_id: usize,
}

#[derive(Resource)]
struct VideoCtx {
    send_frame: Sender<VideoFrame>,
    size: (u32, u32),
    last_time: Option<Duration>,
    frame_time: Duration,
    cur_frame: usize,
    closed: Arc<AtomicBool>,
}

#[derive(Resource)]
struct VideoCtxRecv {
    recv_frame: Option<Receiver<VideoFrame>>,
    frame_buf: BTreeMap<usize, RgbImage>,
    cur_frame: usize,
    frame_time: video_rs::Time,
    encoder: video_rs::Encoder,
}

mod systems {
    use crossbeam_channel::TryRecvError;

    use super::*;

    pub fn video_frame(
        commands: Commands,
        window: Query<Entity, With<PrimaryWindow>>,
        time: Res<Time>,
    ) {
        /*
            let Ok(window) = window.get_single() else {
                commands.remove_resource::<VideoCtx>();
                return;
            };

            if ctx.closed.load(Ordering::SeqCst) {
                commands.remove_resource::<VideoCtx>();
                return;
            }

            if ctx
                .last_time
                .map(|t| time.elapsed() >= (t + ctx.frame_time))
                .unwrap_or(true)
            {
                let sender = ctx.send_frame.clone();
                let frame_id = ctx.cur_frame;
                let size = ctx.size;
                let closed = ctx.closed.clone();

                ctx.last_time = Some(time.elapsed());

                if let Ok(_) = screenshot.take_screenshot(window, move |image| {
                    let image = image
                        .try_into_dynamic()
                        .unwrap()
                        .resize_to_fill(size.0, size.1, FilterType::Nearest)
                        .into_rgb8();

                    if let Err(_) = sender.send(VideoFrame { image, frame_id }) {
                        closed.store(true, Ordering::SeqCst);
                    }
                }) {
                    ctx.cur_frame += 1;
                }
            }

            // Handle new frames
        */
        todo!()
    }

    pub fn recv_frame(mut ctx: ResMut<VideoCtxRecv>, mut commands: Commands) {
        loop {
            let frame = match (ctx.frame_buf.first_key_value(), &ctx.recv_frame) {
                (Some((frame, _)), _) if *frame == ctx.cur_frame => {
                    let (_, frame) = ctx.frame_buf.pop_first().unwrap();
                    frame
                }
                (Some(_), None) => {
                    let (_, frame) = ctx.frame_buf.pop_first().unwrap();
                    frame
                }
                (_, Some(recv)) => {
                    match recv.try_recv() {
                        Ok(next) => {
                            ctx.frame_buf.insert(next.frame_id, next.image);
                        }
                        Err(TryRecvError::Empty) => break,
                        Err(TryRecvError::Disconnected) => ctx.recv_frame = None,
                    }

                    continue;
                }
                (None, None) => {
                    commands.remove_resource::<VideoCtxRecv>();
                    break;
                }
            };

            let frame = frame.into_flat_samples();
            let frame_array = ndarray::Array3::<u8>::from_shape_vec(
                (
                    frame.layout.height as usize,
                    frame.layout.width as usize,
                    frame.layout.channels as usize,
                ),
                frame.samples,
            )
            .unwrap();
            let time = video_rs::Time::new(
                Some(ctx.cur_frame as _),
                ctx.frame_time.clone().into_parts().1,
            );
            ctx.encoder.encode(&frame_array, &time).unwrap();
            ctx.cur_frame += 1;
        }
    }
}
