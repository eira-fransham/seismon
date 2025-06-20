use fundsp::hacker::*;

pub fn limiter_stereo(lookahead: f64, attack_time: f32, release_time: f32) -> An<Limiter<U2>> {
    An(Limiter::new(
        DEFAULT_SR,
        lookahead,
        attack_time,
        release_time,
    ))
}

/// Look-ahead limiter.
#[derive(Clone)]
pub struct Limiter<N>
where
    N: Size<f32>,
{
    lookahead: f64,
    #[allow(dead_code)]
    release: f64,
    sample_rate: f64,
    reducer: ReduceBuffer<f32, Maximum<f32>>,
    follower: AFollow<f32>,
    buffer: Vec<Frame<f32, N>>,
    index: usize,
}

impl<N> Limiter<N>
where
    N: Size<f32>,
{
    #[inline]
    fn advance(&mut self) {
        self.index += 1;
        if self.index >= self.reducer.length() {
            self.index = 0;
        }
    }

    fn buffer_length(sample_rate: f64, lookahead: f64) -> usize {
        max(1, round(sample_rate * lookahead) as usize)
    }

    fn new_buffer(sample_rate: f64, lookahead: f64) -> ReduceBuffer<f32, Maximum<f32>> {
        ReduceBuffer::new(Self::buffer_length(sample_rate, lookahead), Maximum::new())
    }

    pub fn new(sample_rate: f64, lookahead: f64, attack_time: f32, release_time: f32) -> Self {
        let mut follower = AFollow::new(attack_time * 0.4, release_time * 0.4);
        follower.set_sample_rate(sample_rate);
        Limiter {
            lookahead,
            release: release_time as f64,
            sample_rate,
            follower,
            buffer: Vec::new(),
            reducer: Self::new_buffer(sample_rate, lookahead),
            index: 0,
        }
    }
}

impl<N> AudioNode for Limiter<N>
where
    N: Size<f32>,
{
    const ID: u64 = 25;
    type Inputs = N;
    type Outputs = N;

    fn reset(&mut self) {
        self.set_sample_rate(self.sample_rate);
    }

    fn set_sample_rate(&mut self, sample_rate: f64) {
        self.index = 0;
        self.sample_rate = sample_rate;
        let length = Self::buffer_length(sample_rate, self.lookahead);
        if length != self.reducer.length() {
            self.reducer = Self::new_buffer(sample_rate, self.lookahead);
        }
        self.follower.set_sample_rate(sample_rate);
        self.reducer.clear();
        self.buffer.clear();
    }

    #[inline]
    fn tick(&mut self, input: &Frame<f32, Self::Inputs>) -> Frame<f32, Self::Outputs> {
        let amplitude = input.iter().fold(0.0, |amp, &x| max(amp, abs(x)));
        self.reducer.set(self.index, amplitude);
        if self.buffer.len() < self.reducer.length() {
            // We are filling up the initial buffer.
            self.buffer.push(input.clone());
            if self.buffer.len() == self.reducer.length() {
                // When the buffer is full, start following from its total peak.
                self.follower.set_value(self.reducer.total());
            }
            self.advance();
            Frame::default()
        } else {
            let output = self.buffer[self.index].clone();
            self.buffer[self.index] = input.clone();
            // Leave some headroom.
            self.follower
                .filter_mono(max(1.0, self.reducer.total() * 1.10));
            self.advance();
            let limit = self.follower.value();
            output * Frame::splat(1.0 / limit)
        }
    }

    fn route(&mut self, input: &SignalFrame, _frequency: f64) -> SignalFrame {
        let mut output = SignalFrame::new(self.outputs());
        for i in 0..N::USIZE {
            // We pretend that the limiter does not alter the frequency response.
            output.set(i, input.at(i).delay(self.reducer.length() as f64));
        }
        output
    }

    fn allocate(&mut self) {
        if self.buffer.capacity() < self.reducer.length() {
            self.buffer
                .reserve(self.reducer.length() - self.buffer.capacity());
        }
    }
}
