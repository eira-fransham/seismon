// Copyright © 2018 Cormac O'Brien
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

use std::{io, time::Duration};

use bevy_transform::components::Transform;
use futures::AsyncReadExt;
use futures_byteorder::{AsyncReadBytes, LittleEndian};
use num_traits::FromPrimitive as _;
use seismon_utils::{
    model::{ModelFlags, SyncType},
    read_f32_3_async,
};

use bevy_log as log;
use bevy_math::{UVec2, Vec2, Vec3};

use crate::MdlFileError;

pub const MAGIC: i32 = i32::from_le_bytes(*b"IDPO");
pub const VERSION: i32 = 6;

#[expect(
    dead_code,
    reason = "TODO: For error reporting, but we can't use this as Bevy asset readers don't implement seek"
)]
const HEADER_SIZE: u64 = 84;

#[derive(Clone, Debug)]
pub struct StaticTexture {
    indices: Vec<u8>,
}

impl StaticTexture {
    /// Returns the indexed colors of this texture.
    pub fn indices(&self) -> &[u8] {
        &self.indices
    }
}

#[derive(Clone, Debug)]
pub struct AnimatedTextureFrame {
    duration: Duration,
    indices: Vec<u8>,
}

impl AnimatedTextureFrame {
    /// Returns the duration of this frame.
    pub fn duration(&self) -> Duration {
        self.duration
    }

    /// Returns the indexed colors of this texture.
    pub fn indices(&self) -> &[u8] {
        &self.indices
    }
}

#[derive(Clone, Debug)]
pub struct AnimatedTexture {
    frames: Vec<AnimatedTextureFrame>,
}

impl AnimatedTexture {
    pub fn frames(&self) -> &[AnimatedTextureFrame] {
        &self.frames
    }
}

#[derive(Clone, Debug)]
pub enum Texture {
    Static(StaticTexture),
    Animated(AnimatedTexture),
}

#[derive(Clone, Debug)]
pub struct Texcoord {
    is_on_seam: bool,
    s: u32,
    t: u32,
}

impl Texcoord {
    pub fn is_on_seam(&self) -> bool {
        self.is_on_seam
    }

    pub fn s(&self) -> u32 {
        self.s
    }

    pub fn t(&self) -> u32 {
        self.t
    }

    pub fn to_bevy(&self, size: UVec2, faces_front: bool) -> Vec2 {
        let s = if !faces_front && self.is_on_seam() {
            (self.s() + size.x / 2) as f32 + 0.5
        } else {
            self.s() as f32 + 0.5
        } / size.x as f32;
        let t = (self.t() as f32 + 0.5) / size.y as f32;

        Vec2::new(s, t)
    }
}

#[derive(Clone, Debug)]
pub struct TriFace {
    faces_front: bool,
    indices: [u32; 3],
}

impl TriFace {
    pub fn faces_front(&self) -> bool {
        self.faces_front
    }

    pub fn indices(&self) -> &[u32; 3] {
        &self.indices
    }
}

#[derive(Clone, Debug)]
pub struct Mesh {
    name: String,
    min: Vec3,
    max: Vec3,
    vertices: Vec<Vec3>,
}

impl Mesh {
    /// Returns the name of this keyframe.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Returns the minimum extent of this keyframe relative to the model origin.
    pub fn min(&self) -> Vec3 {
        self.min
    }

    /// Returns the minimum extent of this keyframe relative to the model origin.
    pub fn max(&self) -> Vec3 {
        self.max
    }

    /// Returns the vertices defining this keyframe.
    pub fn vertices(&self) -> &[Vec3] {
        &self.vertices
    }
}

#[derive(Clone, Debug)]
pub struct AnimatedMeshFrame {
    name: String,
    min: Vec3,
    max: Vec3,
    duration: Duration,
    vertices: Vec<Vec3>,
}

impl AnimatedMeshFrame {
    /// Returns the name of this subframe.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Returns the minimum extent of this keyframe relative to the model origin.
    pub fn min(&self) -> Vec3 {
        self.min
    }

    /// Returns the minimum extent of this keyframe relative to the model origin.
    pub fn max(&self) -> Vec3 {
        self.max
    }

    /// Returns the duration of this subframe.
    pub fn duration(&self) -> Duration {
        self.duration
    }

    /// Returns the vertices defining this subframe.
    pub fn vertices(&self) -> &[Vec3] {
        &self.vertices
    }
}

#[derive(Clone, Debug)]
pub struct AnimatedMesh {
    min: Vec3,
    max: Vec3,
    frames: Vec<AnimatedMeshFrame>,
}

impl AnimatedMesh {
    /// Returns the minimum extent of all subframes in this keyframe relative to the model origin.
    pub fn min(&self) -> Vec3 {
        self.min
    }

    /// Returns the maximum extent of all subframes in this keyframe relative to the model origin.
    pub fn max(&self) -> Vec3 {
        self.max
    }

    /// Returns the subframes of this keyframe.
    pub fn frames(&self) -> &[AnimatedMeshFrame] {
        &self.frames
    }
}

#[derive(Clone, Debug)]
pub enum Animation {
    Static(Mesh),
    Animated(AnimatedMesh),
}

#[derive(Debug, Clone)]
pub struct RawMdl {
    radius: f32,
    scale: Vec3,
    scale_origin: Vec3,
    texture_width: u32,
    texture_height: u32,
    textures: Vec<Texture>,
    texcoords: Vec<Texcoord>,
    tris: Vec<TriFace>,
    animations: Vec<Animation>,
    flags: ModelFlags,
}

impl RawMdl {
    // See https://github.com/id-Software/Quake/blob/master/WinQuake/r_alias.c#L337-L407
    pub fn transform(&self) -> Transform {
        // TODO: For some reason this seems to rotate models 180 degrees instead of flipping them as we might
        // expect. Inverting X seems to do nothing, only inverting Y. This makes e.g. ogres left-handed instead
        // of right, but at least makes them face the correct direction.
        const MODEL_FUDGE_SCALE: Vec3 = Vec3::new(1., -1., 1.);

        Transform::from_translation(MODEL_FUDGE_SCALE * self.scale_origin)
            .with_scale(MODEL_FUDGE_SCALE * self.scale)
    }

    pub fn scale_origin(&self) -> Vec3 {
        self.scale_origin
    }

    pub fn radius(&self) -> f32 {
        self.radius
    }

    pub fn texture_width(&self) -> u32 {
        self.texture_width
    }

    pub fn texture_height(&self) -> u32 {
        self.texture_height
    }

    pub fn textures(&self) -> &[Texture] {
        &self.textures
    }

    pub fn texcoords(&self) -> &[Texcoord] {
        &self.texcoords
    }

    pub fn polygons(&self) -> &[TriFace] {
        &self.tris
    }

    pub fn animations(&self) -> &[Animation] {
        &self.animations
    }

    pub fn flags(&self) -> ModelFlags {
        self.flags
    }
}

#[derive(Default)]
pub struct MdlResult {
    pub value: Option<RawMdl>,
    pub errors: Vec<MdlFileError>,
}

impl MdlResult {
    pub fn into_result(self) -> Result<RawMdl, MdlFileError> {
        self.into()
    }
}

impl From<MdlResult> for Result<RawMdl, MdlFileError> {
    fn from(value: MdlResult) -> Self {
        value.value.ok_or(value.errors.into_iter().next_back().unwrap_or(MdlFileError::Unknown))
    }
}

pub async fn load<R>(mut reader: &mut R) -> MdlResult
where
    R: bevy_asset::io::Reader + ?Sized,
{
    let mut reader = AsyncReadBytes::new(&mut reader);
    let mut error_set = Vec::<MdlFileError>::new();

    macro_rules! nonfatal {
        ($errval:expr) => {
            nonfatal!(error_set, $errval);
        };
        ($errors:expr, $errval:expr) => {
            $errors.push($errval.into());
        };
    }

    macro_rules! fatal {
        ($errval:expr) => {{
            nonfatal!($errval);
            return MdlResult { value: None, errors: error_set };
        }};
    }

    macro_rules! try_ {
        ($val:expr) => {
            match $val {
                Ok(val) => val,
                Err(e) => {
                    fatal!(e);
                }
            }
        };
    }

    // struct MdlHeader {
    //     magic: i32
    //     version: i32
    //     scale: [f32; 3]
    //     scale_origin: [f32; 3]
    //     radius: f32
    //     eye_position: [f32; 3]
    //     texture_count: i32,
    //     texture_width: i32,
    //     texture_height: i32,
    //     vertex_count: i32,
    //     poly_count: i32,
    //     keyframe_count: i32,
    //     sync_type: i32,
    //     flags_bits: i32,
    // }

    let magic = try_!(reader.read_i32::<LittleEndian>().await);
    if magic != MAGIC {
        fatal!(MdlFileError::InvalidMagicNumber(magic));
    }

    let version = try_!(reader.read_i32::<LittleEndian>().await);
    if version != VERSION {
        fatal!(MdlFileError::UnrecognizedVersion(version));
    }

    let scale: Vec3 = try_!(read_f32_3_async(&mut reader).await).into();
    let scale_origin: Vec3 = try_!(read_f32_3_async(&mut reader).await).into();
    let radius = try_!(reader.read_f32::<LittleEndian>().await);
    // TODO: Expose this
    let _eye_position: Vec3 = try_!(read_f32_3_async(&mut reader).await).into();
    let texture_count = try_!(reader.read_i32::<LittleEndian>().await);
    let texture_width = try_!(reader.read_i32::<LittleEndian>().await);
    if texture_width <= 0 {
        fatal!(MdlFileError::InvalidTextureWidth(texture_width));
    }
    let texture_height = try_!(reader.read_i32::<LittleEndian>().await);
    if texture_height <= 0 {
        fatal!(MdlFileError::InvalidTextureHeight(texture_height));
    }
    let vertex_count = try_!(reader.read_i32::<LittleEndian>().await);
    if vertex_count <= 0 {
        fatal!(MdlFileError::InvalidVertexCount(vertex_count));
    }
    let poly_count = try_!(reader.read_i32::<LittleEndian>().await);
    if poly_count <= 0 {
        fatal!(MdlFileError::InvalidPolygonCount(poly_count));
    }
    let keyframe_count = try_!(reader.read_i32::<LittleEndian>().await);
    if keyframe_count <= 0 {
        fatal!(MdlFileError::InvalidKeyframeCount(keyframe_count));
    }

    let _sync_type = SyncType::from_i32(try_!(reader.read_i32::<LittleEndian>().await));

    let mut flags_bits = try_!(reader.read_i32::<LittleEndian>().await);
    if flags_bits < 0 || flags_bits > u8::MAX as i32 {
        nonfatal!(MdlFileError::InvalidFlags(flags_bits));
        flags_bits = flags_bits as u8 as i32;
    }
    let flags = match ModelFlags::from_bits(flags_bits as u8) {
        Some(flags) => flags,
        None => {
            nonfatal!(MdlFileError::InvalidFlags(flags_bits));
            ModelFlags::from_bits_truncate(flags_bits as u8)
        }
    };

    // unused
    let _size = try_!(reader.read_i32::<LittleEndian>().await);

    // Cannot seek with bevy reader
    // assert_eq!(
    //     try_!(reader.stream_position()),
    //     try_!(reader.seek(SeekFrom::Start(HEADER_SIZE))),
    //     "Misaligned read on MDL header"
    // );

    let textures = {
        let mut out = Vec::with_capacity(texture_count as usize);

        for _ in 0..texture_count {
            // TODO: add a TextureKind type
            let texture = match try_!(reader.read_i32::<LittleEndian>().await) {
                // Static
                0 => {
                    let mut indices: Vec<u8> =
                        Vec::with_capacity((texture_width * texture_height) as usize);
                    try_!(
                        (&mut *reader)
                            .take((texture_width * texture_height) as u64)
                            .read_to_end(&mut indices)
                            .await
                    );
                    Texture::Static(StaticTexture { indices })
                }

                // Animated
                1 => {
                    // TODO: sanity check this value
                    let texture_frame_count =
                        try_!(reader.read_i32::<LittleEndian>().await) as usize;

                    let mut durations = Vec::with_capacity(texture_frame_count);
                    for _ in 0..texture_frame_count {
                        durations.push(seismon_utils::duration_from_f32(try_!(
                            reader.read_f32::<LittleEndian>().await
                        )));
                    }

                    let mut frames = Vec::with_capacity(texture_frame_count);
                    for duration in durations {
                        let mut indices: Vec<u8> =
                            Vec::with_capacity((texture_width * texture_height) as usize);
                        try_!(
                            (&mut *reader)
                                .take((texture_width * texture_height) as u64)
                                .read_to_end(&mut indices)
                                .await
                        );
                        frames.push(AnimatedTextureFrame { duration, indices });
                    }

                    Texture::Animated(AnimatedTexture { frames })
                }

                k => {
                    fatal!(MdlFileError::InvalidTextureKind(k));
                }
            };

            out.push(texture);
        }

        out
    };

    let texcoords = {
        let mut out = Vec::with_capacity(vertex_count as usize);

        for _ in 0..vertex_count {
            let is_on_seam = match try_!(reader.read_i32::<LittleEndian>().await) {
                0 => false,
                0x20 => true,
                x => {
                    nonfatal!(MdlFileError::InvalidSeamFlag(x));
                    false
                }
            };

            let s = try_!(reader.read_i32::<LittleEndian>().await);
            let t = try_!(reader.read_i32::<LittleEndian>().await);
            let (s, t) = if s < 0 || t < 0 {
                nonfatal!(MdlFileError::InvalidTexcoord([s, t]));
                (0, 0)
            } else {
                (s as u32, t as u32)
            };

            out.push(Texcoord { is_on_seam, s, t });
        }

        out
    };

    let polygons = {
        let mut out = Vec::with_capacity(poly_count as usize);
        for _ in 0..poly_count {
            let faces_front = match try_!(reader.read_i32::<LittleEndian>().await) {
                0 => false,
                1 => true,
                // Some models in MALICE have this set to `9`, we report the error
                // and assume x != 0 means front-facing.
                x => {
                    nonfatal!(MdlFileError::InvalidFrontFacing(x));
                    true
                }
            };

            let indices = [
                try_!(reader.read_i32::<LittleEndian>().await),
                try_!(reader.read_i32::<LittleEndian>().await),
                try_!(reader.read_i32::<LittleEndian>().await),
            ]
            .map(|v| v as u32);

            out.push(TriFace { faces_front, indices });
        }

        out
    };

    let keyframes = {
        let mut out = Vec::with_capacity(keyframe_count as usize);

        for _ in 0..keyframe_count {
            match try_!(reader.read_i32::<LittleEndian>().await) {
                0 => {
                    let min = try_!(read_vertex(&mut reader).await);
                    try_!(reader.read_u8().await); // discard vertex normal
                    let max = try_!(read_vertex(&mut reader).await);
                    try_!(reader.read_u8().await); // discard vertex normal

                    let name = {
                        let mut bytes: [u8; 16] = [0; 16];
                        try_!(reader.read_exact(&mut bytes).await);
                        let len = try_!(
                            bytes
                                .iter()
                                .position(|b| *b == 0)
                                .ok_or(MdlFileError::KeyframeNameTooLong(bytes))
                        );
                        try_!(String::from_utf8(bytes[0..(len + 1)].to_vec()))
                    };

                    log::debug!("Keyframe name: {}", name);

                    let mut vertices: Vec<Vec3> = Vec::with_capacity(vertex_count as usize);
                    for _ in 0..vertex_count {
                        vertices.push(try_!(read_vertex(&mut reader).await));
                        try_!(reader.read_u8().await); // discard vertex normal
                    }

                    out.push(Animation::Static(Mesh { name, min, max, vertices }));
                }

                1 => {
                    let subframe_count = match try_!(reader.read_i32::<LittleEndian>().await) {
                        s if s <= 0 => panic!("Invalid subframe count: {s}"),
                        s => s,
                    };

                    let abs_min = try_!(read_vertex(&mut reader).await);
                    try_!(reader.read_u8().await); // discard vertex normal
                    let abs_max = try_!(read_vertex(&mut reader).await);
                    try_!(reader.read_u8().await); // discard vertex normal

                    let mut durations = Vec::new();
                    for _ in 0..subframe_count {
                        durations.push(seismon_utils::duration_from_f32(try_!(
                            reader.read_f32::<LittleEndian>().await
                        )));
                    }

                    let mut subframes = Vec::new();
                    for subframe_id in 0..subframe_count {
                        let min = try_!(read_vertex(&mut reader).await);
                        try_!(reader.read_u8().await); // discard vertex normal
                        let max = try_!(read_vertex(&mut reader).await);
                        try_!(reader.read_u8().await); // discard vertex normal

                        let name = {
                            let mut bytes: [u8; 16] = [0; 16];
                            try_!(reader.read_exact(&mut bytes).await);
                            let len = try_!(
                                bytes
                                    .iter()
                                    .position(|b| *b == 0)
                                    .ok_or(MdlFileError::KeyframeNameTooLong(bytes))
                            );
                            try_!(String::from_utf8(bytes[0..(len + 1)].to_vec()))
                        };

                        log::debug!("Frame name: {}", name);

                        let mut vertices: Vec<Vec3> = Vec::with_capacity(vertex_count as usize);
                        for _ in 0..vertex_count {
                            vertices.push(try_!(read_vertex(&mut reader).await));
                            try_!(reader.read_u8().await); // discard vertex normal
                        }

                        subframes.push(AnimatedMeshFrame {
                            min,
                            max,
                            name,
                            duration: durations[subframe_id as usize],
                            vertices,
                        })
                    }

                    out.push(Animation::Animated(AnimatedMesh {
                        min: abs_min,
                        max: abs_max,
                        frames: subframes,
                    }));
                }

                x => panic!("Bad frame kind value: {x}"),
            }
        }

        out
    };

    // TODO: Not possible to seek bevy asset data streams
    // if try_!(reader.stream_position()) != try_!(reader.seek(SeekFrom::End(0))) {
    //     nonfatal!(MdlFileError::Misaligned);
    // }

    MdlResult {
        value: Some(RawMdl {
            scale,
            scale_origin,
            radius,
            texture_width: texture_width as u32,
            texture_height: texture_height as u32,
            textures,
            texcoords,
            tris: polygons,
            animations: keyframes,
            flags,
        }),
        errors: error_set,
    }
}

async fn read_vertex<R>(reader: &mut AsyncReadBytes<'_, R>) -> Result<Vec3, io::Error>
where
    R: AsyncReadExt + Unpin,
{
    Ok(Vec3::new(
        reader.read_u8().await? as f32,
        reader.read_u8().await? as f32,
        reader.read_u8().await? as f32,
    ))
}
