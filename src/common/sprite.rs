// Copyright © 2018 Cormac O'Brien.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this software
// and associated documentation files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or
// substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
// BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
// DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

use std::io::{BufReader, Read, Seek};

use crate::common::{engine, model::SyncType};

use bevy::prelude::*;
use byteorder::{LittleEndian, ReadBytesExt};
use chrono::Duration;
use num::FromPrimitive;
use num_derive::FromPrimitive;

const MAGIC: u32 = ('I' as u32) << 0 | ('D' as u32) << 8 | ('S' as u32) << 16 | ('P' as u32) << 24;
const VERSION: u32 = 1;

#[derive(Clone, Copy, Debug, Eq, FromPrimitive, PartialEq)]
pub enum SpriteKind {
    ViewPlaneParallelUpright = 0,
    Upright = 1,
    ViewPlaneParallel = 2,
    Oriented = 3,
    ViewPlaneParallelOriented = 4,
}

#[derive(Debug, Clone)]
pub struct SpriteModel {
    kind: SpriteKind,
    max_width: usize,
    max_height: usize,
    radius: f32,
    frames: Vec<SpriteFrame>,
}

impl SpriteModel {
    pub fn min(&self) -> Vec3 {
        Vec3::new(
            -(self.max_width as f32) / 2.0,
            -(self.max_width as f32) / 2.0,
            -(self.max_height as f32) / 2.0,
        )
    }

    pub fn max(&self) -> Vec3 {
        Vec3::new(
            self.max_width as f32 / 2.0,
            self.max_width as f32 / 2.0,
            self.max_height as f32 / 2.0,
        )
    }

    pub fn radius(&self) -> f32 {
        self.radius
    }

    pub fn kind(&self) -> SpriteKind {
        self.kind
    }

    pub fn frames(&self) -> impl Iterator<Item = &SpriteFrame> {
        self.frames.iter()
    }
}

#[derive(Debug, Clone)]
pub enum SpriteFrame {
    Static {
        frame: SpriteSubframe,
    },
    Animated {
        subframes: Vec<SpriteSubframe>,
        durations: Vec<Duration>,
    },
}

#[derive(Debug, Clone)]
pub struct SpriteSubframe {
    width: u32,
    height: u32,
    indexed: Box<[u8]>,
}

impl SpriteSubframe {
    pub fn width(&self) -> u32 {
        self.width
    }

    pub fn height(&self) -> u32 {
        self.height
    }

    pub fn indexed(&self) -> &[u8] {
        &self.indexed
    }
}

pub fn load<R>(data: R) -> SpriteModel
where
    R: Read + Seek,
{
    let mut reader = BufReader::new(data);

    let magic = reader.read_u32::<LittleEndian>().unwrap();
    if magic != MAGIC {
        panic!(
            "Bad magic number for sprite model (got {}, should be {})",
            magic, MAGIC
        );
    }

    let version = reader.read_u32::<LittleEndian>().unwrap();
    if version != VERSION {
        panic!(
            "Bad version number for sprite model (got {}, should be {})",
            version, VERSION
        );
    }

    // TODO: use an enum for this
    let kind = SpriteKind::from_i32(reader.read_i32::<LittleEndian>().unwrap()).unwrap();

    let radius = reader.read_f32::<LittleEndian>().unwrap();

    let max_width = match reader.read_i32::<LittleEndian>().unwrap() {
        w if w < 0 => panic!("Negative max width ({})", w),
        w => w as usize,
    };

    let max_height = match reader.read_i32::<LittleEndian>().unwrap() {
        h if h < 0 => panic!("Negative max height ({})", h),
        h => h as usize,
    };

    let frame_count = match reader.read_i32::<LittleEndian>().unwrap() {
        c if c < 1 => panic!("Invalid frame count ({}), must be at least 1", c),
        c => c as usize,
    };

    let _beam_len = match reader.read_i32::<LittleEndian>().unwrap() {
        l if l < 0 => panic!("Negative beam length ({})", l),
        l => l as usize,
    };

    debug!(
        "max_width = {} max_height = {} frame_count = {}",
        max_width, max_height, frame_count
    );

    let _sync_type = SyncType::from_i32(reader.read_i32::<LittleEndian>().unwrap()).unwrap();

    let frames = (0..frame_count)
        .map(|i| {
            let frame_kind_int = reader.read_i32::<LittleEndian>().unwrap();

            // TODO: substitute out this magic number
            if frame_kind_int == 0 {
                let _origin_x = reader.read_i32::<LittleEndian>().unwrap();
                let _origin_z = reader.read_i32::<LittleEndian>().unwrap();

                let width = match reader.read_i32::<LittleEndian>().unwrap() {
                    w if w < 0 => panic!("Negative frame width ({})", w),
                    w => w,
                };

                let height = match reader.read_i32::<LittleEndian>().unwrap() {
                    h if h < 0 => panic!("Negative frame height ({})", h),
                    h => h,
                };

                debug!("Frame {}: width = {} height = {}", i, width, height);

                let index_count = (width * height) as usize;
                let indices = (0..index_count as usize)
                    .map(|_| reader.read_u8().unwrap())
                    .collect();

                SpriteFrame::Static {
                    frame: SpriteSubframe {
                        width: width as u32,
                        height: height as u32,
                        indexed: indices,
                    },
                }
            } else {
                let subframe_count = match reader.read_i32::<LittleEndian>().unwrap() {
                    c if c < 0 => panic!("Negative subframe count ({}) in frame {}", c, i),
                    c => c as usize,
                };

                let durations = (0..subframe_count)
                    .map(|_| engine::duration_from_f32(reader.read_f32::<LittleEndian>().unwrap()))
                    .collect();

                let subframes = (0..subframe_count)
                    .map(|_| {
                        let _origin_x = reader.read_i32::<LittleEndian>().unwrap();
                        let _origin_z = reader.read_i32::<LittleEndian>().unwrap();

                        let width = match reader.read_i32::<LittleEndian>().unwrap() {
                            w if w < 0 => panic!("Negative subframe width ({}) in frame {}", w, i),
                            w => w,
                        };

                        let height = match reader.read_i32::<LittleEndian>().unwrap() {
                            h if h < 0 => panic!("Negative subframe height ({}) in frame {}", h, i),
                            h => h,
                        };

                        let index_count = (width * height) as usize;
                        let indices = (0..index_count as usize)
                            .map(|_| reader.read_u8().unwrap())
                            .collect();

                        SpriteSubframe {
                            width: width as u32,
                            height: height as u32,
                            indexed: indices,
                        }
                    })
                    .collect();

                SpriteFrame::Animated {
                    durations,
                    subframes,
                }
            }
        })
        .collect();

    SpriteModel {
        kind,
        max_width,
        max_height,
        radius,
        frames,
    }
}
