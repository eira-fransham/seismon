//! Maths builtins (`sin`, `min`, `vectorvectors` etc.), as in FTEQW's `pr_bgcmd.c`.

use bevy::prelude::*;
use bevy_mod_scripting::bindings::NamespaceBuilder;
use bevy_mod_scripting_qcvm::builtin_argc;

use crate::common::qc::{QcValue, stub::Vec3Arg, with_vm_ctx};

/// The smallest (or with `max`, the largest) of the first `argc` of `args`, like `PF_min` and
/// `PF_max`. With fewer than 2 arguments, this is the first.
pub fn min_max(args: &[f32], argc: usize, max: bool) -> f32 {
    let args = &args[..argc.clamp(1, args.len())];
    args.iter().copied().reduce(|a, b| if (b > a) == max { b } else { a }).unwrap_or(0.)
}

/// `PF_bound`: `val` clamped to `minimum..=maximum`, preferring `maximum` if they're reversed.
pub fn bound(minimum: f32, val: f32, maximum: f32) -> f32 {
    if val > maximum {
        maximum
    } else if val < minimum {
        minimum
    } else {
        val
    }
}

/// `PF_mod`: the remainder of truncating division (0 when dividing by 0).
pub fn modulo(a: f32, n: f32) -> f32 {
    if n == 0. {
        warn!("mod by zero");
        0.
    } else {
        a - n * (a / n).trunc()
    }
}

/// `PF_bitshift`: shifts the integer part of `number` left (or right, for negative `quantity`).
pub fn bitshift(number: f32, quantity: f32) -> f32 {
    let (number, shift) = (number as i32, quantity as i32);
    let shifted = if shift < 0 {
        number.checked_shr(shift.unsigned_abs()).unwrap_or(if number < 0 { -1 } else { 0 })
    } else {
        number.checked_shl(shift as u32).unwrap_or(0)
    };
    shifted as f32
}

/// `PF_anglemod`: `value` wrapped to `0..360`.
pub fn anglemod(value: f32) -> f32 {
    let wrapped = value.rem_euclid(360.);
    // `rem_euclid` can round up to exactly 360 for tiny negative values.
    if wrapped >= 360. { 0. } else { wrapped }
}

/// The right and up vectors for `forward`, like FTEQW's `VectorVectors`. `forward` is normalized
/// first.
pub fn vector_vectors(forward: Vec3) -> (Vec3, Vec3, Vec3) {
    let forward = forward.normalize_or_zero();
    let right = if forward.x == 0. && forward.y == 0. {
        Vec3::new(0., if forward.z != 0. { -1. } else { 0. }, 0.)
    } else {
        Vec3::new(forward.y, -forward.x, 0.).normalize_or_zero()
    };
    let up = right.cross(forward);
    (forward, right, up)
}

pub fn register<N: 'static>(ns: &mut NamespaceBuilder<'_, N>) {
    ns.register("sin", f32::sin)
        .register("cos", f32::cos)
        .register("sqrt", f32::sqrt)
        .register("tan", f32::tan)
        .register("asin", f32::asin)
        .register("acos", f32::acos)
        .register("atan", f32::atan)
        .register("atan2", f32::atan2)
        .register("pow", f32::powf)
        .register("log", |v: f32, base: f32| {
            let r = f64::from(v).ln();
            (if builtin_argc() > 1 { r / f64::from(base).ln() } else { r }) as f32
        })
        .register(
            "min",
            |a: f32, b: f32, c: f32, d: f32, e: f32, f: f32, g: f32, h: f32| {
                min_max(&[a, b, c, d, e, f, g, h], builtin_argc(), false)
            },
        )
        .register(
            "max",
            |a: f32, b: f32, c: f32, d: f32, e: f32, f: f32, g: f32, h: f32| {
                min_max(&[a, b, c, d, e, f, g, h], builtin_argc(), true)
            },
        )
        .register("bound", bound)
        .register("mod", modulo)
        .register("bitshift", bitshift)
        .register("anglemod", anglemod)
        .register("crossproduct", |a: Vec3Arg, b: Vec3Arg| {
            Vec3::from(a).cross(Vec3::from(b)).to_array()
        })
        .register("randomvec", || {
            with_vm_ctx(|ctx| {
                // `PF_randomvector`: a random point in the unit sphere.
                loop {
                    let mut axis = || ctx.storage.random(ctx.world) * 2. - 1.;
                    let v = Vec3::new(axis(), axis(), axis());
                    if v.length_squared() < 1. {
                        break Ok(v.to_array());
                    }
                }
            })
        })
        .register("vectorvectors", |dir: Vec3Arg| {
            with_vm_ctx(|ctx| {
                let (forward, right, up) = vector_vectors(dir.into());
                ctx.set_global("v_forward", QcValue::Vector(forward))?;
                ctx.set_global("v_right", QcValue::Vector(right))?;
                ctx.set_global("v_up", QcValue::Vector(up))
            })
        });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn min_and_max_use_argc() {
        let args = [3., 1., 2., 0., 0., 0., 0., 0.];
        assert_eq!(min_max(&args, 2, false), 1.);
        assert_eq!(min_max(&args, 3, true), 3.);
        // The zeros past `argc` aren't arguments.
        assert_eq!(min_max(&args, 3, false), 1.);
        assert_eq!(min_max(&args, 0, false), 3.);
    }

    #[test]
    fn bound_prefers_maximum() {
        assert_eq!(bound(0., 5., 10.), 5.);
        assert_eq!(bound(0., -1., 10.), 0.);
        assert_eq!(bound(10., 5., 0.), 0.);
    }

    #[test]
    fn mod_truncates() {
        assert_eq!(modulo(7., 3.), 1.);
        assert_eq!(modulo(-7., 3.), -1.);
        assert_eq!(modulo(7.5, 2.), 1.5);
        assert_eq!(modulo(1., 0.), 0.);
    }

    #[test]
    fn bitshift_shifts_integers() {
        assert_eq!(bitshift(1., 4.), 16.);
        assert_eq!(bitshift(16.9, -2.), 4.);
        assert_eq!(bitshift(-16., -2.), -4.);
    }

    #[test]
    fn anglemod_wraps() {
        assert_eq!(anglemod(370.), 10.);
        assert_eq!(anglemod(-10.), 350.);
        assert_eq!(anglemod(360.), 0.);
    }

    #[test]
    fn vector_vectors_are_orthonormal() {
        let (forward, right, up) = vector_vectors(Vec3::new(2., 0., 0.));
        assert_eq!(forward, Vec3::X);
        assert_eq!(right, Vec3::new(0., -1., 0.));
        assert_eq!(up, Vec3::Z);
        let (_, right, up) = vector_vectors(Vec3::Z);
        assert_eq!(right, Vec3::new(0., -1., 0.));
        assert_eq!(up, Vec3::new(-1., 0., 0.));
    }
}
