use std::sync::LazyLock;

use bevy::{
    prelude::*,
    sprite::{TextureAtlasBuilderError, TextureAtlasLayout, TextureAtlasSources},
};
use hashbrown::HashMap;
use itertools::Itertools as _;
use rand::distr::{Distribution as _, Uniform};
use uuid::Uuid;

pub struct CompiledTextureAtlas {
    pub layout: TextureAtlasLayout,
    pub id_map: TextureAtlasSources,
    pub image: Image,
}

impl Clone for CompiledTextureAtlas {
    fn clone(&self) -> Self {
        Self {
            layout: self.layout.clone(),
            id_map: TextureAtlasSources {
                texture_ids: self.id_map.texture_ids.clone(),
            },
            image: self.image.clone(),
        }
    }
}

impl CompiledTextureAtlas {
    pub fn translate(&self, id: AssetId<Image>, coords: Vec2) -> Vec2 {
        let lightmap_size = self.layout.size;
        let raw_coords = self.layout.textures[self.id_map.texture_ids[&id]];
        let real_coords = Rect {
            min: raw_coords.min.as_vec2() / lightmap_size.as_vec2(),
            max: raw_coords.max.as_vec2() / lightmap_size.as_vec2(),
        };

        coords * real_coords.size() + real_coords.min
    }
}

pub struct IncrementalTextureAtlas {
    format: wgpu::TextureFormat,
    atlas_id: u64,
    // The asset ID is uniquely generated from the hash of the image data, so this should
    // be a bijection.
    images: HashMap<AssetId<Image>, Image>,
}

impl IncrementalTextureAtlas {
    pub fn new(format: wgpu::TextureFormat) -> Self {
        static ID_DISTRIBUTION: LazyLock<Uniform<u64>> =
            LazyLock::new(|| Uniform::new_inclusive(0, u64::MAX).unwrap());

        IncrementalTextureAtlas {
            format,
            atlas_id: ID_DISTRIBUTION.sample(&mut rand::rng()),
            images: default(),
        }
    }

    pub fn build(&mut self) -> Result<CompiledTextureAtlas, TextureAtlasBuilderError> {
        let mut atlas = TextureAtlasBuilder::default();
        atlas.format(self.format);
        atlas.padding(UVec2 { x: 2, y: 2 });
        atlas.auto_format_conversion(true);

        for (id, lightmap) in &self.images {
            atlas.add_texture(Some(*id), lightmap);
        }

        let (layout, id_map, image) = atlas.build()?;

        Ok(CompiledTextureAtlas {
            layout,
            id_map,
            image,
        })
    }

    pub fn push(&mut self, image: Image) -> AssetId<Image> {
        // Many fullbright textures are all-black, so we deduplicate textures that are all one colour.
        let image = if let Ok(homogenous) = image.data.iter().all_equal_value() {
            Image::new(
                wgpu::Extent3d {
                    width: 1,
                    height: 1,
                    depth_or_array_layers: 1,
                },
                image.texture_descriptor.dimension,
                vec![*homogenous; self.format.components() as usize],
                image.texture_descriptor.format,
                image.asset_usage,
            )
        } else {
            image
        };

        let hash = hashers::fx_hash::fxhash64(&image.data);
        let id = AssetId::from(Uuid::from_u64_pair(self.atlas_id, hash));

        self.images.entry(id).or_insert(image);

        id
    }
}
