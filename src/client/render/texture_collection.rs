use std::{borrow::Borrow, sync::LazyLock};

use bevy::{
    asset::RenderAssetUsages,
    image::{TextureAtlasBuilderError, TextureFormatPixelInfo},
    prelude::*,
};
use hashbrown::HashMap;
use indexmap::IndexMap;
use itertools::Itertools as _;
use rand::distr::{Distribution as _, Uniform};
use uuid::Uuid;

fn resize_resampling_with_pixel_type<P: image::Pixel<Subpixel = u8> + 'static>(
    image: &mut Image,
    new_size: UVec2,
) {
    let new_buf = image::imageops::resize(
        &image::ImageBuffer::<P, _>::from_raw(
            image.width(),
            image.height(),
            image.data.as_deref().unwrap_or_default(),
        )
        .unwrap(),
        new_size.x,
        new_size.y,
        image::imageops::FilterType::CatmullRom,
    );

    image.data = Some(new_buf.into_vec());
    image.texture_descriptor.size = wgpu::Extent3d {
        width: new_size.x,
        height: new_size.y,
        depth_or_array_layers: 1,
    };
}

fn resize_image_resampling(image: &mut Image, new_size: UVec2) {
    match image.texture_descriptor.format.pixel_size() {
        1 => {
            resize_resampling_with_pixel_type::<image::Luma<u8>>(image, new_size);
        }
        2 => {
            resize_resampling_with_pixel_type::<image::LumaA<u8>>(image, new_size);
        }
        3 => {
            resize_resampling_with_pixel_type::<image::Rgb<u8>>(image, new_size);
        }
        4 => {
            resize_resampling_with_pixel_type::<image::Rgba<u8>>(image, new_size);
        }
        _ => todo!(),
    }
}

pub trait CollectTexture<P> {
    type Index: Copy;
    type Image;
    type Error;

    fn build<'a, I>(
        &self,
        vals: I,
    ) -> Result<CompiledTextureCollection<Self::Index, Self::Image>, Self::Error>
    where
        P: 'a,
        I: IntoIterator<Item = (AssetId<Image>, &'a P)>;

    fn transform(&self, image: Image) -> Image {
        image
    }
}

pub trait CollectTextureExt<P>: CollectTexture<P> {
    type CompiledTextureCollection;
}

impl<T, P> CollectTextureExt<P> for T
where
    T: CollectTexture<P>,
{
    type CompiledTextureCollection = CompiledTextureCollection<Self::Index, Self::Image>;
}

pub struct BucketedTextureSetConfig {
    pub format: wgpu::TextureFormat,
}

impl BucketedTextureSetConfig {
    pub fn new(format: wgpu::TextureFormat) -> Self {
        Self { format }
    }
}

impl From<wgpu::TextureFormat> for BucketedTextureSetConfig {
    fn from(value: wgpu::TextureFormat) -> Self {
        Self::new(value)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BucketedTextureSetIndex {
    pub bucket_index: usize,
    pub index_in_bucket: usize,
}

impl<P> CollectTexture<P> for BucketedTextureSetConfig
where
    P: Borrow<Image>,
{
    type Index = BucketedTextureSetIndex;
    type Image = Vec<Image>;
    type Error = !;

    fn build<'a, I>(
        &self,
        vals: I,
    ) -> Result<CompiledTextureCollection<Self::Index, Self::Image>, Self::Error>
    where
        P: 'a,
        I: IntoIterator<Item = (AssetId<Image>, &'a P)>,
    {
        let mut buckets = IndexMap::<UVec2, (Image, Vec<AssetId<Image>>)>::new();

        let layout = vals
            .into_iter()
            .map(|(id, img)| {
                let entry = buckets.entry(img.borrow().size());
                let bucket_index = entry.index();
                let (texture_array, bucket) = entry.or_insert_with(|| {
                    (
                        Image::new(
                            wgpu::Extent3d {
                                width: 0,
                                height: 0,
                                depth_or_array_layers: 1,
                            },
                            wgpu::TextureDimension::D2,
                            vec![],
                            self.format,
                            RenderAssetUsages::RENDER_WORLD,
                        ),
                        default(),
                    )
                });
                match (&mut texture_array.data, &img.borrow().data) {
                    (Some(dst), Some(src)) => dst.extend(src),
                    (dst @ None, Some(src)) => {
                        *dst = Some(src.clone());
                    }
                    (_, None) => {}
                }
                texture_array.texture_descriptor.size.depth_or_array_layers += 1;
                let index_in_bucket = bucket.len();
                bucket.push(id);

                (
                    id,
                    BucketedTextureSetIndex {
                        bucket_index,
                        index_in_bucket,
                    },
                )
            })
            .collect();

        Ok(CompiledTextureCollection {
            layout,
            image: buckets.into_iter().map(|(_, (img, _))| img).collect(),
        })
    }

    fn transform(&self, mut image: Image) -> Image {
        let pow_2_w = image.width().next_power_of_two();
        let pow_2_h = image.height().next_power_of_two();

        if image.width() != pow_2_w || image.height() != pow_2_h {
            resize_image_resampling(&mut image, [pow_2_w, pow_2_h].into());
        }

        image
    }
}

pub struct TextureAtlasConfig {
    pub format: wgpu::TextureFormat,
    pub padding: UVec2,
}

impl TextureAtlasConfig {
    pub fn new(format: wgpu::TextureFormat) -> Self {
        Self {
            format,
            padding: UVec2 { x: 1, y: 1 },
        }
    }
}

impl From<wgpu::TextureFormat> for TextureAtlasConfig {
    fn from(value: wgpu::TextureFormat) -> Self {
        Self::new(value)
    }
}

impl<P> CollectTexture<P> for TextureAtlasConfig
where
    P: Borrow<Image>,
{
    type Index = Rect;
    type Image = Image;
    type Error = TextureAtlasBuilderError;

    fn build<'a, I>(
        &self,
        vals: I,
    ) -> Result<CompiledTextureCollection<Self::Index, Self::Image>, Self::Error>
    where
        P: 'a,
        I: IntoIterator<Item = (AssetId<Image>, &'a P)>,
    {
        let mut atlas = TextureAtlasBuilder::default();
        atlas.format(self.format);
        atlas.auto_format_conversion(true);
        // TODO: We probably want to have multiple textures instead of one big texture,
        //       this is far easier to achieve with Bevy's render system than our
        //       hand-rolled one.
        atlas.max_size(UVec2::splat(4096));

        for (id, image) in vals {
            atlas.add_texture(Some(id), image.borrow());
        }

        let (layout, id_map, image) = atlas.build()?;

        let size = layout.size.as_vec2();

        Ok(CompiledTextureCollection {
            layout: id_map
                .texture_ids
                .into_iter()
                .map(|(id, index)| {
                    let rect = layout.textures[index];

                    (
                        id,
                        Rect {
                            min: (rect.min + self.padding).as_vec2() / size,
                            max: (rect.max - self.padding).as_vec2() / size,
                        },
                    )
                })
                .collect(),
            image,
        })
    }

    fn transform(&self, mut image: Image) -> Image {
        static ZERO: u8 = 0;

        if self.padding == UVec2::default() {
            return image;
        }

        fn pad_row(padding: UVec2, chunk: &[u8]) -> impl Iterator<Item = &'_ u8> + Clone {
            let first = chunk.first().unwrap_or(&ZERO);
            let last = chunk.last().unwrap_or(&ZERO);
            std::iter::repeat_n(first, padding.x as _)
                .chain(chunk)
                .chain(std::iter::repeat_n(last, padding.x as _))
        }

        let original_size = image.size();

        let final_row = pad_row(
            self.padding,
            image
                .data
                .as_deref()
                .map(|data| {
                    &data[data.len().saturating_sub(
                        image.width() as usize * image.texture_descriptor.format.pixel_size(),
                    )..]
                })
                .unwrap_or_default(),
        );
        let pixel_size = image.texture_descriptor.format.pixel_size();
        let mut extended_x = image
            .data
            .as_deref()
            .unwrap_or_default()
            .chunks(image.width() as usize * pixel_size)
            .map(|chunk| pad_row(self.padding, chunk))
            .peekable();
        let first_row = extended_x.peek().unwrap().clone();
        let new_data = std::iter::repeat_n(first_row, self.padding.y as _)
            .chain(extended_x)
            .chain(std::iter::repeat_n(final_row, self.padding.y as _))
            .flatten()
            .copied()
            .collect::<Vec<_>>();

        image.data = Some(new_data);
        image.texture_descriptor.size = wgpu::Extent3d {
            width: image.width() + self.padding.x * 2,
            height: image.height() + self.padding.y * 2,
            depth_or_array_layers: 1,
        };

        let new_size = image.size();

        trace!("Resized image from {original_size} to {new_size}");

        image
    }
}

pub struct TextureArrayConfig {
    pub format: wgpu::TextureFormat,
    pub min_size: UVec2,
}

impl From<wgpu::TextureFormat> for TextureArrayConfig {
    fn from(value: wgpu::TextureFormat) -> Self {
        Self {
            format: value,
            min_size: UVec2 { x: 1, y: 1 },
        }
    }
}

impl<P> CollectTexture<P> for TextureArrayConfig
where
    P: Borrow<Image>,
{
    type Index = (usize, Vec2);
    type Image = Image;
    type Error = !;

    fn build<'a, I>(
        &self,
        vals: I,
    ) -> Result<CompiledTextureCollection<Self::Index, Self::Image>, Self::Error>
    where
        P: 'a,
        I: IntoIterator<Item = (AssetId<Image>, &'a P)>,
    {
        let (layout, mut images): (Vec<_>, Vec<_>) = vals
            .into_iter()
            .enumerate()
            .map(|(i, (id, img))| ((id, i), img.borrow().clone()))
            .unzip();

        let mut max_width = 0;
        let mut max_height = 0;

        for image in &mut images {
            let pow_2_w = image.width().next_power_of_two();
            let pow_2_h = image.height().next_power_of_two();

            max_width = max_width.max(pow_2_w);
            max_height = max_height.max(pow_2_h);
        }

        let max_size: UVec2 = UVec2 {
            x: max_width,
            y: max_height,
        };

        let layout = layout
            .into_iter()
            .zip(&mut images)
            .map(|((id, index), image)| {
                let original_size = image.size();
                let mapped_size = original_size.as_vec2() / max_size.as_vec2();
                if image.texture_descriptor.format != self.format
                    && let Some(converted) = image.convert(self.format)
                {
                    *image = converted;
                }

                if image.width() != max_width || image.height() != max_height {
                    resize_image_resampling(image, [max_width, max_height].into());
                }

                (id, (index, mapped_size))
            })
            .collect();

        let combined_image = Image::new(
            wgpu::Extent3d {
                width: max_size.x,
                height: max_size.y,
                depth_or_array_layers: images.len() as _,
            },
            wgpu::TextureDimension::D3,
            images
                .into_iter()
                .flat_map(|image| image.data.unwrap_or_default().into_iter())
                .collect(),
            self.format,
            RenderAssetUsages::RENDER_WORLD,
        );

        Ok(CompiledTextureCollection {
            layout,
            image: combined_image,
        })
    }
}

pub type ImagePtr = Image;

#[derive(Default, Clone)]
pub struct CompiledTextureCollection<
    Index = <TextureAtlasConfig as CollectTexture<ImagePtr>>::Index,
    CompiledImage = <TextureAtlasConfig as CollectTexture<ImagePtr>>::Image,
> {
    pub layout: HashMap<AssetId<Image>, Index>,
    pub image: CompiledImage,
}

impl
    CompiledTextureCollection<
        <TextureAtlasConfig as CollectTexture<ImagePtr>>::Index,
        <TextureAtlasConfig as CollectTexture<ImagePtr>>::Image,
    >
{
    pub fn translate(&self, id: AssetId<Image>, coords: Vec2) -> Vec2 {
        let bounds = self.layout[&id];
        coords * bounds.size() + bounds.min
    }
}

impl
    CompiledTextureCollection<
        <TextureArrayConfig as CollectTexture<ImagePtr>>::Index,
        <TextureArrayConfig as CollectTexture<ImagePtr>>::Image,
    >
{
    pub fn translate(&self, id: AssetId<Image>, coords: Vec2) -> Vec2 {
        let (_, bounds) = self.layout[&id];
        coords * bounds
    }
}

pub struct IncrementalTextureCollection<Format = TextureAtlasConfig> {
    format: Format,
    atlas_id: u64,
    // The asset ID is uniquely generated from the hash of the image data, so this should
    // be a bijection.
    images: HashMap<AssetId<Image>, ImagePtr>,
}

impl<Format> IncrementalTextureCollection<Format>
where
    Format: CollectTexture<ImagePtr>,
{
    pub fn new<I>(format: I) -> Self
    where
        Format: From<I>,
    {
        static ID_DISTRIBUTION: LazyLock<Uniform<u64>> =
            LazyLock::new(|| Uniform::new_inclusive(0, u64::MAX).unwrap());

        IncrementalTextureCollection {
            format: format.into(),
            atlas_id: ID_DISTRIBUTION.sample(&mut rand::rng()),
            images: default(),
        }
    }

    pub fn format_mut(&mut self) -> &mut Format {
        &mut self.format
    }

    pub fn build(
        &mut self,
    ) -> Result<CompiledTextureCollection<Format::Index, Format::Image>, Format::Error> {
        self.format
            .build(self.images.iter().map(|(i, img)| (*i, img)))
    }

    pub fn push(&mut self, image: Image) -> AssetId<Image> {
        let pixel_components = image.texture_descriptor.format.components() as usize;

        let image = self.format.transform(image);

        // Many fullbright textures are all-black but different sizes, so we deduplicate textures
        // that are all one colour.
        let image = if let Some(Ok(homogenous)) = image
            .data
            .as_deref()
            .map(|v| v.chunks_exact(pixel_components).all_equal_value())
        {
            Image::new(
                wgpu::Extent3d {
                    width: 1,
                    height: 1,
                    depth_or_array_layers: 1,
                },
                image.texture_descriptor.dimension,
                homogenous.to_vec(),
                image.texture_descriptor.format,
                image.asset_usage,
            )
        } else {
            image
        };

        let hash = hashers::fx_hash::fxhash64(image.data.as_deref().unwrap_or_default());
        let id = AssetId::from(Uuid::from_u64_pair(self.atlas_id, hash));

        self.images.entry(id).or_insert(image);

        id
    }
}
