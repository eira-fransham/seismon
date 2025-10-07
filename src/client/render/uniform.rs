use std::{
    marker::PhantomData,
    mem::{self, align_of, size_of},
    sync::Arc,
};

use bevy::{
    prelude::*,
    render::{
        render_resource::Buffer,
        renderer::{RenderDevice, RenderQueue},
    },
};
use bytemuck::{Pod, Zeroable};
use failure::{Error, bail};

// minimum limit is 16384:
// https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-maxUniformBufferRange
// but https://vulkan.gpuinfo.org/displaydevicelimit.php?name=maxUniformBufferRange&platform=windows
// indicates that a limit of 65536 or higher is more common
const DYNAMIC_UNIFORM_BUFFER_SIZE: wgpu::BufferAddress = 1 << 19;

// https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-minUniformBufferOffsetAlignment
pub const DYNAMIC_UNIFORM_BUFFER_ALIGNMENT: usize = 256;

#[repr(transparent)]
#[derive(Zeroable, Pod, Clone, Copy, Debug)]
pub struct UniformBool {
    value: u32,
}

impl UniformBool {
    pub fn new(value: bool) -> UniformBool {
        UniformBool {
            value: value as u32,
        }
    }
}

/// A handle to a dynamic uniform buffer on the GPU.
///
/// Allows allocation and updating of individual blocks of memory.
pub struct DynamicUniformBuffer<T>
where
    T: Pod,
{
    // keeps track of how many blocks are allocated so we know whether we can
    // clear the buffer or not
    _rc: Arc<()>,

    // represents the data in the buffer, which we don't actually own
    _phantom: PhantomData<T>,

    inner: Buffer,
    allocated: u64,
    update_buf: Vec<u8>,
}

impl<T> DynamicUniformBuffer<T>
where
    T: Pod,
{
    pub fn new(device: &RenderDevice) -> DynamicUniformBuffer<T> {
        // TODO: is this something we can enforce at compile time?
        assert!(align_of::<T>() % DYNAMIC_UNIFORM_BUFFER_ALIGNMENT == 0);

        let inner = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("dynamic uniform buffer"),
            size: DYNAMIC_UNIFORM_BUFFER_SIZE,
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        let update_buf = vec![0; DYNAMIC_UNIFORM_BUFFER_SIZE as usize];

        DynamicUniformBuffer {
            _rc: Arc::new(()),
            _phantom: PhantomData,
            inner,
            allocated: 0,
            update_buf,
        }
    }

    pub fn block_size(&self) -> wgpu::BufferSize {
        std::num::NonZeroU64::new(
            ((DYNAMIC_UNIFORM_BUFFER_ALIGNMENT / 8).max(size_of::<T>())) as u64,
        )
        .unwrap()
    }

    /// Allocates a block of memory in this dynamic uniform buffer with the
    /// specified initial value.
    #[must_use]
    pub fn allocate(&mut self, val: T) -> DynamicUniformBufferBlock<T> {
        let allocated = self.allocated;
        let size = self.block_size().get();
        trace!("Allocating dynamic uniform block (allocated: {allocated})");
        if allocated + size > DYNAMIC_UNIFORM_BUFFER_SIZE {
            panic!("Not enough space to allocate {size} bytes in dynamic uniform buffer");
        }

        let addr = allocated;
        self.allocated = allocated + size;

        let block = DynamicUniformBufferBlock {
            _rc: self._rc.clone(),
            _phantom: PhantomData,
            addr,
        };

        self.write_block(&block, val);
        block
    }

    pub fn write_block(&mut self, block: &DynamicUniformBufferBlock<T>, val: T) {
        let start = block.addr as usize;
        let end = start + self.block_size().get() as usize;
        let slice = &mut self.update_buf[start..end];
        slice.copy_from_slice(bytemuck::bytes_of(&val));
    }

    /// Removes all allocations from the underlying buffer.
    ///
    /// Returns an error if the buffer is currently mapped or there are
    /// outstanding allocated blocks.
    pub fn clear(&mut self) -> Result<(), Error> {
        let out = mem::take(&mut self._rc);
        match Arc::try_unwrap(out) {
            // no outstanding blocks
            Ok(()) => {
                self.allocated = 0;
                Ok(())
            }
            Err(rc) => {
                let _ = mem::replace(&mut self._rc, rc);
                bail!(
                    "Can't clear uniform buffer: there are outstanding references to allocated blocks."
                );
            }
        }
    }

    pub fn flush(&self, queue: &RenderQueue) {
        queue.write_buffer(&self.inner, 0, &self.update_buf);
    }

    pub fn buffer(&self) -> &wgpu::Buffer {
        &self.inner
    }
}

/// An address into a dynamic uniform buffer.
#[derive(Debug)]
pub struct DynamicUniformBufferBlock<T> {
    _rc: Arc<()>,
    _phantom: PhantomData<T>,

    addr: wgpu::BufferAddress,
}

impl<T> DynamicUniformBufferBlock<T> {
    pub fn offset(&self) -> wgpu::DynamicOffset {
        self.addr as wgpu::DynamicOffset
    }
}

pub fn clear_and_rewrite<T>(
    queue: &RenderQueue,
    buffer: &mut DynamicUniformBuffer<T>,
    blocks: &mut Vec<DynamicUniformBufferBlock<T>>,
    uniforms: &[T],
) where
    T: Pod,
{
    blocks.clear();
    buffer.clear().unwrap();
    for (uni_id, uni) in uniforms.iter().enumerate() {
        if uni_id >= blocks.len() {
            let block = buffer.allocate(*uni);
            blocks.push(block);
        } else {
            buffer.write_block(&blocks[uni_id], *uni);
        }
    }
    buffer.flush(queue);
}
