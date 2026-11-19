//! Quake lightstyles, driving `bevy_trenchbroom`'s animated lightmaps.
//!
//! A lightstyle is a string of letters, each one a brightness: `'a'` is black, `'m'` is normal and
//! `'z'` is double brightness. The client steps through the string at 10Hz using client time with
//! no interpolation, like `R_AnimateLight`. The server can replace a style at any time, e.g. to
//! switch a light on or off.
//!
//! Styles that fit in a [`LightingAnimator`] are animated on the GPU. Its clock is the virtual
//! time, which the client keeps in step with the server, so nothing needs to happen per frame.
//! Longer styles are sampled here every frame instead, and only written when their value changes.

use bevy::{
    math::Vec3,
    prelude::*,
    time::{Time, Virtual},
};
use bevy_trenchbroom::{
    bsp::lighting::{LightingAnimator, LightingAnimators, MAX_LIGHTMAP_FRAMES},
    prelude::LightmapStyle,
};
use seismon_utils::QString;

use super::Connection;

/// How many characters of a lightstyle are stepped through per second.
pub const LIGHTSTYLE_HZ: f64 = 10.0;

/// The client's copy of one lightstyle, i.e. an entry of `cl_lightstyle` in Quake.
#[derive(Default, Debug, Clone)]
pub struct LightStyle {
    /// The string sent by the server.
    pub source: QString,
    /// The brightness multiplier for each character. Empty means full brightness, unanimated.
    pub values: Box<[f32]>,
}

/// Quake computes `(c - 'a') * 22`, with 256 being normal brightness. Characters before `'a'`
/// would make the light negative, so they're clamped to black.
fn char_brightness(c: u8) -> f32 {
    ((c as f32 - b'a' as f32) * 22.0 / 256.0).max(0.0)
}

impl LightStyle {
    pub fn parse(source: QString) -> Self {
        let bytes: &[u8] = &source;
        let bytes = bytes.strip_suffix(b"\0").unwrap_or(bytes);
        let values = bytes.iter().copied().map(char_brightness).collect();

        Self { source, values }
    }

    /// The brightness multiplier at `time` seconds of client time.
    pub fn sample(&self, time: f64) -> f32 {
        if self.values.is_empty() {
            return 1.0;
        }

        let frame = (time * LIGHTSTYLE_HZ) as usize;
        self.values[frame % self.values.len()]
    }

    /// Whether this style is too long to be animated by a [`LightingAnimator`], so has to be
    /// animated with [`Self::sample`] every frame.
    pub fn needs_cpu_animation(&self) -> bool {
        self.values.len() > MAX_LIGHTMAP_FRAMES
    }

    /// The GPU animator for this style, if it is animated and short enough to have one.
    pub fn gpu_animator(&self) -> Option<LightingAnimator> {
        if self.values.is_empty() || self.needs_cpu_animation() {
            return None;
        }

        let mut sequence = [Vec3::ZERO; MAX_LIGHTMAP_FRAMES];
        for (out, value) in sequence.iter_mut().zip(&self.values) {
            *out = Vec3::splat(*value);
        }

        Some(LightingAnimator {
            sequence,
            sequence_len: self.values.len() as u32,
            speed: LIGHTSTYLE_HZ as f32,
            interpolate: 0.0,
        })
    }
}

/// Keeps [`LightingAnimators`] in step with the client's lightstyles.
///
/// Changing [`LightingAnimators`] makes `bevy_trenchbroom` rebuild every animated lighting bind
/// group, so this only marks it as changed when something is actually different.
pub fn sync_lightstyles(
    mut conn: Option<ResMut<Connection>>,
    time: Res<Time<Virtual>>,
    mut animators: ResMut<LightingAnimators>,
    // Styles that are animated by `sync_lightstyles` rather than on the GPU.
    mut cpu_styles: Local<Vec<u8>>,
) {
    // Only `ClientState::lightstyles_changed` is written, which nothing observes via change detection.
    let Some(state) =
        conn.as_mut().and_then(|conn| conn.bypass_change_detection().client_state.as_mut())
    else {
        // Not connected: don't let the previous level's styles leak into whatever's next.
        cpu_styles.clear();
        if !animators.values.is_empty() {
            animators.values.clear();
        }
        return;
    };

    let now = time.elapsed_secs_f64();

    if state.lightstyles_changed {
        state.lightstyles_changed = false;
        cpu_styles.clear();

        let animators = animators.as_mut();
        animators.values.clear();
        // Style IDs are a `u8` in the BSP format, so styles past 255 can't be referenced.
        for (id, style) in (0..=u8::MAX).zip(&state.lightstyles) {
            let animator = if style.needs_cpu_animation() {
                cpu_styles.push(id);
                LightingAnimator::unanimated(Vec3::splat(style.sample(now)))
            } else if let Some(animator) = style.gpu_animator() {
                animator
            } else {
                // Empty: the default animator is full brightness.
                continue;
            };

            animators.values.insert(LightmapStyle(id), animator);
        }

        return;
    }

    let mut changed = false;
    let values = &mut animators.bypass_change_detection().values;
    for &id in cpu_styles.iter() {
        let Some(style) = state.lightstyles.get(id as usize) else {
            continue;
        };
        let value = style.sample(now);
        let animator = values.entry(LightmapStyle(id)).or_default();
        if animator.sequence[0].x != value {
            *animator = LightingAnimator::unanimated(Vec3::splat(value));
            changed = true;
        }
    }

    if changed {
        animators.set_changed();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(s: &str) -> LightStyle {
        LightStyle::parse(s.as_bytes().to_vec().into())
    }

    #[test]
    fn empty_is_full_brightness() {
        let style = parse("");
        assert!(style.values.is_empty());
        assert!(style.gpu_animator().is_none());
        assert!(!style.needs_cpu_animation());
        assert_eq!(style.sample(12.3), 1.0);
    }

    #[test]
    fn quake_brightness_scale() {
        assert_eq!(&*parse("m").values, &[264.0 / 256.0]);
        assert_eq!(&*parse("az").values, &[0.0, 550.0 / 256.0]);
        // Below 'a' would be negative.
        assert_eq!(&*parse("\x01").values, &[0.0]);
        // Trailing NUL is ignored.
        assert_eq!(parse("ab\0").values.len(), 2);
    }

    #[test]
    fn gpu_animator_matches_quake() {
        let style = parse("abcdefghijklmnopqrstuvwxyz");
        let animator = style.gpu_animator().unwrap();
        assert_eq!(animator.sequence_len, 26);
        assert_eq!(animator.speed, 10.0);
        assert_eq!(animator.interpolate, 0.0);

        for (t, idx) in [(0.0, 0), (0.1001, 1), (0.25, 2), (2.65, 0)] {
            assert_eq!(style.sample(t), style.values[idx], "t = {t}");
            assert_eq!(animator.sample(t as f32), Vec3::splat(style.values[idx]), "t = {t}");
        }
    }

    #[test]
    fn long_styles_are_not_truncated() {
        let source: String = (0..200).map(|i| (b'a' + (i % 26) as u8) as char).collect();
        let style = parse(&source);
        assert_eq!(style.values.len(), 200);
        assert!(style.needs_cpu_animation());
        assert!(style.gpu_animator().is_none());

        for i in 0..200 {
            let t = (i as f64 + 0.5) / LIGHTSTYLE_HZ;
            assert_eq!(style.sample(t), char_brightness(b'a' + (i % 26) as u8));
        }
        // Wraps after 20 seconds.
        assert_eq!(style.sample(20.05), style.values[0]);
    }
}
