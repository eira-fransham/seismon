//! Vector maths used by the server and QuakeC builtins.

use bevy::math::{EulerRot, Mat3, Vec3, Vec3Swizzles as _};

/// Calculates the rotation matrix for Quake `[pitch, yaw, roll]` angles, in degrees.
///
/// Quake's coordinate system has `x` forward, `y` left and `z` up. Yaw rotates about `z`, pitch
/// about `y` (positive pitch looks *down*) and roll about `x`, applied in that order. The columns
/// of the result are the forward, left and up vectors of the rotated frame. Note that QuakeC's
/// `v_right` is the *negation* of the second column; see [`angle_vectors`].
pub fn make_vectors<V: Into<Vec3>>(angles: V) -> Mat3 {
    let angles = angles.into();
    let pitch = angles[0].to_radians();
    let yaw = angles[1].to_radians();
    let roll = angles[2].to_radians();

    Mat3::from_euler(EulerRot::ZYX, yaw, pitch, roll)
}

/// The forward, right and up vectors for a set of angles.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct AngleVectors {
    pub forward: Vec3,
    pub right: Vec3,
    pub up: Vec3,
}

/// Calculates the forward, right and up vectors for Quake `[pitch, yaw, roll]` angles in degrees.
///
/// Equivalen to `AngleVectors` (`makevectors` in QuakeC).
pub fn angle_vectors<V: Into<Vec3>>(angles: V) -> AngleVectors {
    let rotation = make_vectors(angles);

    AngleVectors { forward: rotation.x_axis, right: -rotation.y_axis, up: rotation.z_axis }
}

/// Calculates `[pitch, yaw, 0]` angles (in degrees) pointing along `v`.
///
/// Angles are truncated to whole degrees and wrapped to `[0, 360)`.
///
/// Equivalent to `PF_vectoangles`.
pub fn vec_to_angles(v: Vec3) -> Vec3 {
    let (pitch, yaw);
    if v.x == 0. && v.y == 0. {
        yaw = 0.;
        pitch = if v.z > 0. { 90. } else { 270. };
    } else {
        let mut y = v.y.atan2(v.x).to_degrees().trunc();
        if y < 0. {
            y += 360.;
        }

        let forward = v.xy().length();
        let mut p = v.z.atan2(forward).to_degrees().trunc();
        if p < 0. {
            p += 360.;
        }

        yaw = y;
        pitch = p;
    }

    Vec3::new(pitch, yaw, 0.)
}

#[cfg(test)]
mod test {
    use super::*;

    /// Direct, literal port of `AngleVectors`, kept as close as possible to the original C.
    #[expect(clippy::neg_multiply, reason = "Direct port of `AngleVectors`")]
    fn quake_angle_vectors(angles: [f32; 3]) -> AngleVectors {
        let [pitch, yaw, roll] = angles.map(f32::to_radians);
        let (sy, cy) = yaw.sin_cos();
        let (sp, cp) = pitch.sin_cos();
        let (sr, cr) = roll.sin_cos();

        AngleVectors {
            forward: Vec3::new(cp * cy, cp * sy, -sp),
            right: Vec3::new(
                -1. * sr * sp * cy + -1. * cr * -sy,
                -1. * sr * sp * sy + -1. * cr * cy,
                -1. * sr * cp,
            ),
            up: Vec3::new(cr * sp * cy + -sr * -sy, cr * sp * sy + -sr * cy, cr * cp),
        }
    }

    fn assert_vec_approx_eq(a: Vec3, b: Vec3, what: &str) {
        assert!(a.abs_diff_eq(b, 1e-5), "{what}: {a} != {b}");
    }

    #[test]
    fn test_make_vectors_no_rotation() {
        let angles_zero = [0.0; 3];
        let result = make_vectors(angles_zero);
        assert_eq!(Mat3::IDENTITY, result);
    }

    #[test]
    fn test_angle_vectors_matches_quake() {
        let cases = [
            [0., 0., 0.],
            [90., 0., 0.],
            [-90., 0., 0.],
            [0., 90., 0.],
            [0., 0., 90.],
            [30., 45., 10.],
            [-20., 200., -35.],
            [10., -135., 5.],
            [80., 300., 170.],
        ];

        for angles in cases {
            let expected = quake_angle_vectors(angles);
            let actual = angle_vectors(angles);
            assert_vec_approx_eq(actual.forward, expected.forward, &format!("forward {angles:?}"));
            assert_vec_approx_eq(actual.right, expected.right, &format!("right {angles:?}"));
            assert_vec_approx_eq(actual.up, expected.up, &format!("up {angles:?}"));
        }
    }

    #[test]
    fn test_angle_vectors_conventions() {
        // Looking level at yaw 90 faces +Y, with right pointing along +X.
        let v = angle_vectors([0., 90., 0.]);
        assert_vec_approx_eq(v.forward, Vec3::Y, "forward");
        assert_vec_approx_eq(v.right, Vec3::X, "right");
        assert_vec_approx_eq(v.up, Vec3::Z, "up");

        // Positive pitch looks down.
        let v = angle_vectors([90., 0., 0.]);
        assert_vec_approx_eq(v.forward, -Vec3::Z, "forward");
    }

    #[test]
    fn test_vec_to_angles() {
        assert_eq!(vec_to_angles(Vec3::X), Vec3::new(0., 0., 0.));
        assert_eq!(vec_to_angles(Vec3::Y), Vec3::new(0., 90., 0.));
        assert_eq!(vec_to_angles(-Vec3::Y), Vec3::new(0., 270., 0.));
        assert_eq!(vec_to_angles(Vec3::Z), Vec3::new(90., 0., 0.));
        assert_eq!(vec_to_angles(-Vec3::Z), Vec3::new(270., 0., 0.));
        // Pitch is measured against the horizontal length, not just `y`.
        assert_eq!(vec_to_angles(Vec3::new(1., 0., 1.)), Vec3::new(45., 0., 0.));
        assert_eq!(vec_to_angles(Vec3::new(1., 0., -1.)), Vec3::new(315., 0., 0.));
        // Angles are truncated to whole degrees.
        assert_eq!(vec_to_angles(Vec3::new(1., 0.5, 0.)), Vec3::new(0., 26., 0.));
    }
}
