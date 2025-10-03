use core::f32;
use std::num::NonZeroU32;

use bevy::ecs::component::Component;
use firewheel::{
    SilenceMask, Volume,
    channel_config::{ChannelConfig, NonZeroChannelCount},
    diff::{Diff, Patch},
    event::NodeEventList,
    node::{
        AudioNode, AudioNodeInfo, AudioNodeProcessor, ConstructProcessorContext, ProcBuffers,
        ProcInfo, ProcessStatus,
    },
};
use fundsp::hacker::{AFollow, AudioNode as _, Maximum, ReduceBuffer};

#[derive(Debug, Clone, Component)]
pub struct LimiterConfig {
    /// The limiter lookahead - how much latency will be introduced in order to ensure that the
    /// limiter will reduce volum in time for high peaks to be reduced. By default, it will set
    /// the lookahead to the same as the `attack` of the limiter.
    pub lookahead: Option<f32>,
    pub headroom: Volume,
    pub channels: NonZeroChannelCount,
}

impl Default for LimiterConfig {
    fn default() -> Self {
        Self {
            lookahead: None,
            headroom: Volume::Decibels(1.),
            channels: NonZeroChannelCount::STEREO,
        }
    }
}

#[derive(Diff, Patch, Debug, Clone, Component)]
pub struct LimiterNode {
    pub attack: f32,
    pub release: f32,
}

impl LimiterNode {
    pub fn new(attack: f32, release: f32) -> Self {
        Self { attack, release }
    }
}

impl Default for LimiterNode {
    fn default() -> Self {
        Self::new(0.2, 0.05)
    }
}

/// Look-ahead limiter.
#[derive(Clone)]
struct Limiter {
    lookahead: f32,
    headroom: Volume,
    #[expect(dead_code)]
    release: f32,
    sample_rate: NonZeroU32,
    reducer: ReduceBuffer<f32, Maximum<f32>>,
    follower: AFollow<f32>,
    buffer: Box<[f32]>,
    num_channels: u32,
    max_buffer_length: NonZeroU32,
    index: usize,
}

const DEFAULT_MAX_BUFFER_LENGTH: NonZeroU32 = NonZeroU32::new(1024).unwrap();

impl AudioNode for LimiterNode {
    type Configuration = LimiterConfig;

    fn info(&self, config: &Self::Configuration) -> firewheel::node::AudioNodeInfo {
        AudioNodeInfo::new()
            .debug_name("limiter")
            .channel_config(ChannelConfig {
                num_inputs: config.channels.get(),
                num_outputs: config.channels.get(),
            })
    }

    fn construct_processor(
        &self,
        config: &Self::Configuration,
        _cx: ConstructProcessorContext,
    ) -> impl AudioNodeProcessor {
        Limiter::new(
            NonZeroU32::new(44100).unwrap(),
            config.lookahead.unwrap_or(self.attack),
            self.attack,
            self.release,
            config.headroom,
            config.channels.get().get(),
            DEFAULT_MAX_BUFFER_LENGTH,
        )
    }
}

impl Limiter {
    fn advance(&mut self) {
        self.index = (self.index + 1) % self.reducer.length();
    }

    fn reducer_buf_size(sample_rate: NonZeroU32, lookahead: f32) -> usize {
        (sample_rate.get() as f32 * lookahead).round().max(1.) as usize
    }

    fn new_reducer(sample_rate: NonZeroU32, lookahead: f32) -> ReduceBuffer<f32, Maximum<f32>> {
        ReduceBuffer::new(
            Self::reducer_buf_size(sample_rate, lookahead),
            Maximum::new(),
        )
    }

    fn new(
        sample_rate: NonZeroU32,
        lookahead: f32,
        attack: f32,
        release: f32,
        headroom: Volume,
        num_channels: u32,
        max_buffer_length: NonZeroU32,
    ) -> Self {
        let mut follower = AFollow::new(attack, release);
        follower.set_sample_rate(sample_rate.get() as _);
        let reducer = Self::new_reducer(sample_rate, lookahead);
        let buffer = vec![0.; reducer.length() * num_channels as usize].into();

        Limiter {
            // Updated when given a new stream
            sample_rate,
            buffer,
            num_channels,
            max_buffer_length,
            reducer,
            index: 0,

            // Static
            lookahead,
            headroom,
            release,
            follower,
        }
    }
}

impl AudioNodeProcessor for Limiter {
    fn process(
        &mut self,
        buffers: ProcBuffers,
        proc_info: &ProcInfo,
        _events: NodeEventList,
    ) -> ProcessStatus {
        if proc_info
            .in_silence_mask
            .all_channels_silent(buffers.inputs.len())
            && self.buffer.iter().all(|s| *s == 0.)
        {
            return ProcessStatus::ClearAllOutputs;
        }

        let frame_size = proc_info.frames;

        for i in 0..frame_size {
            let amplitude = buffers
                .inputs
                .iter()
                .map(|input| input[i])
                .filter(|x| x.is_finite())
                .fold(0f32, |amp, x| amp.max(x.abs()));

            self.reducer.set(self.index, amplitude);
            let total = self.reducer.total();

            // Leave some headroom.
            self.follower
                .filter_mono((total * self.headroom.amp()).max(1.));

            let limit = self.follower.value();

            for ((current_chan, out_chan), input_chan) in self
                .buffer
                .chunks_exact_mut(self.num_channels as usize)
                .nth(self.index)
                .unwrap()
                .iter_mut()
                .zip(&mut *buffers.outputs)
                .zip(buffers.inputs)
            {
                out_chan[i] = *current_chan / limit;
                *current_chan = input_chan[i];
            }

            self.advance();
        }

        ProcessStatus::OutputsModified {
            out_silence_mask: SilenceMask::NONE_SILENT,
        }
    }

    fn new_stream(&mut self, stream_info: &firewheel::StreamInfo) {
        self.index = 0;
        self.sample_rate = stream_info.sample_rate;
        self.num_channels = stream_info.num_stream_in_channels;
        self.max_buffer_length = stream_info.max_block_frames;

        self.reducer = Self::new_reducer(self.sample_rate, self.lookahead);

        self.follower.reset();
        self.follower
            .set_sample_rate(stream_info.sample_rate.get() as _);

        let new_buffer_size = self.reducer.length() * self.num_channels as usize;

        if self.buffer.len() == new_buffer_size {
            self.buffer.fill(0.);
        } else {
            self.buffer = vec![0.; new_buffer_size].into();
        }
    }
}
