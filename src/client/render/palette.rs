use std::io::BufReader;

use crate::{
    client::render::{DiffuseData, FullbrightData},
    common::vfs::Vfs,
};

use bevy::{
    prelude::*,
    render::render_resource::{AsBindGroup, ShaderRef},
    sprite::Material2d,
};
use byteorder::ReadBytesExt;

#[derive(Asset, TypePath, AsBindGroup, Debug, Clone)]
pub struct PalettedMaterial {
    /// The transparent color index. Set to a negative number to disable
    #[uniform(0)]
    pub transparent: i32,
    #[texture(1)]
    #[sampler(2)]
    pub greyscale: Option<Handle<Image>>,
    #[texture(3)]
    #[sampler(4)]
    pub palette: Option<Handle<Image>>,
    pub alpha_mode: AlphaMode,
}

impl Default for PalettedMaterial {
    fn default() -> Self {
        Self {
            transparent: 0xff,
            greyscale: None,
            palette: None,
            alpha_mode: AlphaMode::default(),
        }
    }
}

impl Material for PalettedMaterial {
    fn fragment_shader() -> ShaderRef {
        "shaders/paletted_image.wgsl".into()
    }
}

impl Material2d for PalettedMaterial {
    fn fragment_shader() -> ShaderRef {
        "shaders/paletted_image_2d.wgsl".into()
    }
}

impl UiMaterial for PalettedMaterial {
    fn fragment_shader() -> ShaderRef {
        "shaders/paletted_image_ui.wgsl".into()
    }
}

pub struct Palette {
    pub rgb: [[u8; 3]; 256],
}

impl Palette {
    pub fn new(data: &[u8]) -> Palette {
        if data.len() != 768 {
            panic!("Bad len for rgb data");
        }

        let mut rgb = [[0; 3]; 256];
        for color in 0..256 {
            for component in 0..3 {
                rgb[color][component] = data[color * 3 + component];
            }
        }

        Palette { rgb }
    }

    pub fn load<S>(vfs: &Vfs, path: S) -> Palette
    where
        S: AsRef<str>,
    {
        let mut data = BufReader::new(vfs.open(path).unwrap());

        let mut rgb = [[0u8; 3]; 256];

        for color in 0..256 {
            for component in 0..3 {
                rgb[color][component] = data.read_u8().unwrap();
            }
        }

        Palette { rgb }
    }

    // TODO: this will not render console characters correctly, as they use index 0 (black) to
    // indicate transparency.
    /// Translates a set of indices into a list of RGBA values and a list of fullbright values.
    pub fn translate(&self, indices: &[u8]) -> (DiffuseData, FullbrightData) {
        let mut rgba = Vec::with_capacity(indices.len() * 4);
        let mut fullbright = Vec::with_capacity(indices.len());

        for index in indices {
            match *index {
                0xFF => {
                    for _ in 0..4 {
                        rgba.push(0);
                        fullbright.push(0);
                    }
                }

                i => {
                    for component in 0..3 {
                        rgba.push(self.rgb[*index as usize][component]);
                    }
                    rgba.push(0xFF);
                    fullbright.push(if i > 223 { 0xFF } else { 0 });
                }
            }
        }

        (DiffuseData { rgba }, FullbrightData { fullbright })
    }
}
