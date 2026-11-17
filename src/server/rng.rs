//! The server's random number generator.
//!
//! All randomness that can affect the game state (QuakeC's `random`, monster chase directions
//! etc.) goes through [`ServerRng`], so that a level replays identically given the same `sv_seed`
//! and the same client input.

use bevy::ecs::resource::Resource;
use rand::{Rng as _, SeedableRng as _, rngs::StdRng};

/// Deterministic RNG for the server, seeded from the `sv_seed` cvar when a level starts.
#[derive(Resource, Debug, Clone)]
pub struct ServerRng(StdRng);

impl ServerRng {
    pub fn new(seed: u64) -> Self {
        Self(StdRng::seed_from_u64(seed))
    }

    /// A random float in `[0, 1]`, as QuakeC's `random` returns it.
    ///
    /// Like Quake's `PF_random`, this only has 15 bits of precision: `(rand() & 0x7fff) / 0x7fff`.
    pub fn random_qc(&mut self) -> f32 {
        (self.0.random::<u16>() & 0x7fff) as f32 / 0x7fff as f32
    }

    /// Returns `true` with probability `p`.
    pub fn random_bool(&mut self, p: f64) -> bool {
        self.0.random_bool(p)
    }

    /// Returns `true` with probability `numerator / denominator`.
    pub fn random_ratio(&mut self, numerator: u32, denominator: u32) -> bool {
        self.0.random_ratio(numerator, denominator)
    }
}
