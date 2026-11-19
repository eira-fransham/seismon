//! The color lookup table sampled by the particle shaders.
//!
//! Quake particles are colored by palette index, and some particle types (fire, explosions)
//! step through a fixed "ramp" of palette indices as they age. Both are resolved on the GPU
//! from a single small texture:
//!
//! - Row [`LutRow::Palette`] holds the 256 palette colors.
//! - Rows [`LutRow::Ramp1`], [`LutRow::Ramp2`] and [`LutRow::Ramp3`] hold the colors of
//!   `ramp1`, `ramp2` and `ramp3` from `r_part.c`.

use bevy::{
    asset::RenderAssetUsages,
    image::Image,
    render::render_resource::{Extent3d, TextureDimension, TextureFormat},
};

/// `ramp1` from `r_part.c`, used by `pt_explode`.
pub const RAMP1: [u8; 8] = [0x6f, 0x6d, 0x6b, 0x69, 0x67, 0x65, 0x63, 0x61];
/// `ramp2` from `r_part.c`, used by `pt_explode2`.
pub const RAMP2: [u8; 8] = [0x6f, 0x6e, 0x6d, 0x6c, 0x6b, 0x6a, 0x68, 0x66];
/// `ramp3` from `r_part.c`, used by `pt_fire`.
pub const RAMP3: [u8; 6] = [0x6d, 0x6b, 6, 5, 4, 3];

/// Width of the LUT: one texel per palette entry.
pub const LUT_WIDTH: u32 = 256;

/// The rows of the LUT texture.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(u32)]
pub enum LutRow {
    Palette = 0,
    Ramp1 = 1,
    Ramp2 = 2,
    Ramp3 = 3,
}

impl LutRow {
    pub const ALL: [LutRow; 4] = [LutRow::Palette, LutRow::Ramp1, LutRow::Ramp2, LutRow::Ramp3];

    /// The palette indices making up this row.
    fn indices(self) -> &'static [u8] {
        const IDENTITY: [u8; 256] = {
            let mut out = [0; 256];
            let mut i = 0;
            while i < 256 {
                out[i] = i as u8;
                i += 1;
            }
            out
        };

        match self {
            LutRow::Palette => &IDENTITY,
            LutRow::Ramp1 => &RAMP1,
            LutRow::Ramp2 => &RAMP2,
            LutRow::Ramp3 => &RAMP3,
        }
    }
}

/// Builds the RGBA8 texel data of the LUT from the 256-color palette.
///
/// Ramp rows are padded with their final color, so out-of-range lookups stay on the ramp.
pub fn lut_data(palette: &[[u8; 3]; 256]) -> Vec<u8> {
    LutRow::ALL
        .into_iter()
        .flat_map(|row| {
            let indices = row.indices();
            (0..LUT_WIDTH as usize).map(move |x| indices[x.min(indices.len() - 1)])
        })
        .flat_map(|index| {
            let [r, g, b] = palette[index as usize];
            [r, g, b, 0xff]
        })
        .collect()
}

/// Creates the LUT image. Pass `None` for a black placeholder before the palette has loaded.
pub fn lut_image(palette: Option<&[[u8; 3]; 256]>) -> Image {
    let data = match palette {
        Some(palette) => lut_data(palette),
        None => lut_data(&[[0; 3]; 256]),
    };

    Image::new(
        Extent3d { width: LUT_WIDTH, height: LutRow::ALL.len() as u32, depth_or_array_layers: 1 },
        TextureDimension::D2,
        data,
        // The palette is in sRGB, so loads return linear colors.
        TextureFormat::Rgba8UnormSrgb,
        RenderAssetUsages::RENDER_WORLD,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_palette() -> [[u8; 3]; 256] {
        std::array::from_fn(|i| [i as u8, 255 - i as u8, (i as u8).wrapping_mul(3)])
    }

    fn texel(data: &[u8], row: LutRow, x: usize) -> [u8; 4] {
        let offset = (row as usize * LUT_WIDTH as usize + x) * 4;
        data[offset..offset + 4].try_into().unwrap()
    }

    #[test]
    fn palette_row_is_identity() {
        let palette = test_palette();
        let data = lut_data(&palette);

        assert_eq!(data.len(), (LUT_WIDTH as usize) * LutRow::ALL.len() * 4);
        for (i, [r, g, b]) in palette.iter().copied().enumerate() {
            assert_eq!(texel(&data, LutRow::Palette, i), [r, g, b, 0xff]);
        }
    }

    #[test]
    fn ramp_rows_follow_ramp_tables_and_clamp() {
        let palette = test_palette();
        let data = lut_data(&palette);

        for (row, ramp) in
            [(LutRow::Ramp1, &RAMP1[..]), (LutRow::Ramp2, &RAMP2[..]), (LutRow::Ramp3, &RAMP3[..])]
        {
            for x in 0..LUT_WIDTH as usize {
                let index = ramp[x.min(ramp.len() - 1)] as usize;
                let [r, g, b] = palette[index];
                assert_eq!(texel(&data, row, x), [r, g, b, 0xff], "{row:?}[{x}]");
            }
        }
    }
}
