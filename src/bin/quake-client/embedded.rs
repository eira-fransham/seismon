use std::path::Path;

use bevy::{
    asset::io::{
        AssetReader, AssetReaderError, AssetReaderFuture, PathStream, Reader, SliceReader,
        VecReader,
    },
    tasks::{ConditionalSendFuture, futures_lite::stream::iter},
};

pub struct EmbeddedLibrequake;

impl AssetReader for EmbeddedLibrequake {
    fn read<'a>(&'a self, path: &'a Path) -> impl AssetReaderFuture<Value: Reader + 'a> {
        async move {
            if path == "pak0.pak" {
                Ok(SliceReader::new(seismon_embedded_librequake::PAK0_PAK))
            } else if path == "pak1.pak" {
                Ok(SliceReader::new(seismon_embedded_librequake::PAK1_PAK))
            } else {
                Err(AssetReaderError::NotFound(path.to_path_buf()))
            }
        }
    }

    fn read_meta<'a>(&'a self, path: &'a Path) -> impl AssetReaderFuture<Value: Reader + 'a> {
        async { Err::<VecReader, _>(AssetReaderError::NotFound(path.to_path_buf())) }
    }

    fn read_directory<'a>(
        &'a self,
        path: &'a Path,
    ) -> impl ConditionalSendFuture<Output = Result<Box<PathStream>, AssetReaderError>> {
        async move {
            if path != "" {
                Err(AssetReaderError::NotFound(path.to_path_buf()))
            } else {
                Ok(Box::new(iter(
                    [Path::new("pak0.pak"), Path::new("pak1.pak")]
                        .map(Path::to_path_buf)
                        .into_iter(),
                )) as Box<PathStream>)
            }
        }
    }

    fn is_directory<'a>(
        &'a self,
        path: &'a std::path::Path,
    ) -> impl bevy::tasks::ConditionalSendFuture<Output = Result<bool, bevy::asset::io::AssetReaderError>>
    {
        async move { Ok(path == "") }
    }
}
