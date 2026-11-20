//! [`EffectAsset`] builders reproducing the particle effects of Quake's `r_part.c`.
//!
//! Each effect runs entirely on the GPU. The physics of Quake's particle types (`ptype_t`)
//! are expressed as hanabi update modifiers (see [`PType`]), and colors are looked up from the
//! palette/ramp LUT built in [`super::palette`], bound to texture slot 0.
//!
//! Particles are simulated in the emitter's local space ([`SimulationSpace::Local`]). Emitters
//! are children of `worldspawn` and never move, so this is Quake's coordinate space (Z up, Quake
//! units), offset to the emitter's origin. `worldspawn` itself converts to Bevy's space (Y up,
//! scaled down), so constants like [`GRAVITY`] and [`PARTICLE_SIZE`] would be wrong in global
//! space.
//!
//! Quake's `rand() & N` and `rand() % N` are expressed as `floor(rand() * K)`, which gives the
//! same uniform distribution over `0..K`.
//!
//! > ## NOTE: THIS MODULE WAS VIBE CODED! DO NOT TRUST THE IMPLEMENTATION HERE!

use bevy::math::Vec3;
use bevy_hanabi::{expr::TextureLoadExpr, prelude::*};

use super::palette::{LutRow, RAMP1, RAMP2, RAMP3};

/// Downward acceleration of particles affected by gravity, in units/s².
///
/// Quake subtracts `frametime * sv_gravity * 0.05` from the velocity each frame. A remote
/// client has no `sv_gravity`, so this assumes the default of 800.
pub const GRAVITY: f32 = 800. * 0.05;

/// Edge length of a particle's quad, in Quake units, at moderate distances.
///
/// Software Quake (`D_DrawParticle`) draws particles as squares of `0x8000 / depth >> 7` pixels
/// at 320x200, i.e. 1.6 units with its 90° horizontal field of view, clamped to between
/// [`PARTICLE_MIN_PIXELS`] and [`PARTICLE_MAX_PIXELS`]. The clamp stops nearby particles
/// filling the screen and keeps distant ones visible.
pub const PARTICLE_SIZE: f32 = 1.6;

/// Minimum particle size, in pixels at 320x200 (`d_pix_min`).
pub const PARTICLE_MIN_PIXELS: f32 = 1.;
/// Maximum particle size, in pixels at 320x200 (`d_pix_max`).
pub const PARTICLE_MAX_PIXELS: f32 = 4.;

/// Quake draws a particle at least once even if it dies on the frame it was spawned; hanabi
/// would not, so clamp lifetimes to roughly one frame.
const MIN_LIFETIME: f32 = 1. / 60.;

/// Number of particles in `R_ParticleExplosion`, `R_BlobExplosion` and `R_LavaSplash`.
pub const EXPLOSION_COUNT: u32 = 1024;

pub const PROP_COLOR_BASE: &str = "color_base";
pub const PROP_DIRECTION: &str = "direction";
pub const PROP_COLOR_START: &str = "color_start";
pub const PROP_COLOR_LEN: &str = "color_len";
pub const PROP_TRAIL_START: &str = "start";
pub const PROP_TRAIL_END: &str = "end";
pub const PROP_RAMP_BASE: &str = "ramp_base";
pub const PROP_TRAIL_SIDE: &str = "side";

/// Camera position, in the effect's local space. Set on every effect, every frame.
pub const PROP_VIEW_ORIGIN: &str = "view_origin";
/// Camera forward vector, normalized, in the effect's local space. Set on every effect, every
/// frame.
pub const PROP_VIEW_FORWARD: &str = "view_forward";
/// World size of one 320x200 pixel per unit of depth, i.e. `2 * tan(vfov / 2) / 200`. Set on
/// every effect, every frame.
pub const PROP_PIXEL_SCALE: &str = "pixel_scale";

/// Quake's `ptype_t`, which determines how a particle moves and changes color over time.
///
/// `pt_slowgrav` behaves identically to `pt_grav` in Quake 1 and is folded into [`PType::Grav`].
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PType {
    Static,
    Grav,
    Fire,
    Explode,
    Explode2,
    Blob,
    Blob2,
}

/// A palette ramp that a particle steps through as it ages, dying when it runs off the end.
struct Ramp {
    row: LutRow,
    len: usize,
    /// Ramp entries advanced per second.
    rate: f32,
}

impl PType {
    fn ramp(self) -> Option<Ramp> {
        match self {
            PType::Fire => Some(Ramp { row: LutRow::Ramp3, len: RAMP3.len(), rate: 5. }),
            PType::Explode => Some(Ramp { row: LutRow::Ramp1, len: RAMP1.len(), rate: 10. }),
            PType::Explode2 => Some(Ramp { row: LutRow::Ramp2, len: RAMP2.len(), rate: 15. }),
            _ => None,
        }
    }

    /// The per-frame velocity change from `R_DrawParticles`, as an update modifier.
    fn velocity_update(self, w: &ExprWriter) -> Option<SetAttributeModifier> {
        let v = || w.attr(Attribute::VELOCITY);
        let dt = || w.delta_time();
        let grav = || w.lit(Vec3::Z * GRAVITY) * dt();
        let dvel = || dt() * w.lit(4.);

        let new_velocity = match self {
            PType::Static => return None,
            PType::Grav => v() - grav(),
            PType::Fire => v() + grav(),
            PType::Explode | PType::Blob => v() + v() * dvel() - grav(),
            PType::Explode2 => v() - v() * dt() - grav(),
            PType::Blob2 => v() - v() * w.lit(Vec3::new(1., 1., 0.)) * dvel() - grav(),
        };

        Some(SetAttributeModifier::new(Attribute::VELOCITY, new_velocity.expr()))
    }
}

/// How a particle is colored.
enum ParticleColor {
    /// A fixed palette index.
    Palette(WriterExpr),
    /// Walks the [`PType`]'s ramp, starting at this ramp position.
    Ramp(WriterExpr),
}

/// The per-particle initial state of an effect.
struct ParticleInit {
    /// Initial position, relative to the emitter.
    position: WriterExpr,
    velocity: WriterExpr,
    color: ParticleColor,
    /// Time until the particle dies (`p->die - cl.time`), unless its ramp runs out first.
    die: WriterExpr,
}

/// A float uniformly distributed in `0..1`.
fn rand(w: &ExprWriter) -> WriterExpr {
    w.rand(ScalarType::Float)
}

/// An integer-valued float uniformly distributed in `0..n`.
fn rand_int(w: &ExprWriter, n: f32) -> WriterExpr {
    (rand(w) * w.lit(n)).floor()
}

/// A vector of independent integer-valued floats uniformly distributed in `0..n`.
fn rand_int3(w: &ExprWriter, n: f32) -> WriterExpr {
    (w.rand(VectorType::VEC3F) * w.lit(Vec3::splat(n))).floor()
}

/// `(rand() % n) - n/2` per component, e.g. Quake's `(rand() % 32) - 16`.
fn rand_offset3(w: &ExprWriter, n: f32) -> WriterExpr {
    rand_int3(w, n) - w.lit(Vec3::splat(n / 2.))
}

/// Normalizes `v`, returning zero for a zero vector like Quake's `VectorNormalize`.
fn safe_normalize(w: &ExprWriter, v: WriterExpr) -> WriterExpr {
    v.clone() / v.length().max(w.lit(1e-6))
}

/// Looks up the color at palette or ramp index `index` in `row` of the LUT.
fn lut_color(w: &ExprWriter, index: WriterExpr, row: LutRow) -> WriterExpr {
    let coordinates = index.cast(ScalarType::Uint).vec2(w.lit(row as u32)).expr();
    let mip_level = w.lit(0u32).expr();

    let load = TextureLoadExpr::new(0, SlotDimension::D2, coordinates, None, Some(mip_level))
        .expect("2D texture loads take no array index");

    w.push(Expr::TextureLoad(load))
}

/// Sets [`Attribute::SIZE`] each frame to [`PARTICLE_SIZE`], clamped to the on-screen size
/// limits of software Quake.
fn size_update(w: &ExprWriter) -> SetAttributeModifier {
    let view_origin = w.add_property(PROP_VIEW_ORIGIN, Vec3::ZERO.into());
    let view_forward = w.add_property(PROP_VIEW_FORWARD, Vec3::NEG_Z.into());
    let pixel_scale = w.add_property(PROP_PIXEL_SCALE, (2. / 200f32).into());

    let depth = (w.attr(Attribute::POSITION) - w.prop(view_origin))
        .dot(w.prop(view_forward))
        .max(w.lit(0.));
    let pixel = depth * w.prop(pixel_scale);
    let size = w
        .lit(PARTICLE_SIZE)
        .clamp(pixel.clone() * w.lit(PARTICLE_MIN_PIXELS), pixel * w.lit(PARTICLE_MAX_PIXELS));

    SetAttributeModifier::new(Attribute::SIZE, size.expr())
}

/// Assembles an effect from its Quake particle type and initial state.
fn build(
    w: ExprWriter,
    name: &str,
    capacity: u32,
    spawner: SpawnerSettings,
    ptype: PType,
    init: ParticleInit,
) -> EffectAsset {
    let ParticleInit { position, velocity, color, die } = init;

    let mut inits = vec![
        SetAttributeModifier::new(Attribute::AGE, w.lit(0.).expr()),
        SetAttributeModifier::new(Attribute::POSITION, position.expr()),
        SetAttributeModifier::new(Attribute::VELOCITY, velocity.expr()),
    ];
    let mut updates = Vec::new();

    let lifetime = match (color, ptype.ramp()) {
        (ParticleColor::Palette(index), None) => {
            let color = lut_color(&w, index, LutRow::Palette);
            inits.push(SetAttributeModifier::new(Attribute::HDR_COLOR, color.expr()));
            die
        }
        (ParticleColor::Ramp(ramp0), Some(Ramp { row, len, rate })) => {
            let last = w.lit((len - 1) as f32);
            inits.push(SetAttributeModifier::new(Attribute::F32_0, ramp0.clone().expr()));

            let ramp =
                (w.attr(Attribute::F32_0) + w.attr(Attribute::AGE) * w.lit(rate)).floor().min(last);
            let color = lut_color(&w, ramp, row);
            updates.push(SetAttributeModifier::new(Attribute::HDR_COLOR, color.expr()));

            // Quake kills ramped particles once the ramp reaches its end.
            die.min((w.lit(len as f32) - ramp0) / w.lit(rate))
        }
        (_, ramp) => unreachable!(
            "`{name}`: {ptype:?} {} a ramp color",
            if ramp.is_some() { "requires" } else { "can't use" }
        ),
    };
    inits.push(SetAttributeModifier::new(
        Attribute::LIFETIME,
        lifetime.max(w.lit(MIN_LIFETIME)).expr(),
    ));

    updates.extend(ptype.velocity_update(&w));
    updates.push(size_update(&w));

    let mut module = w.finish();
    module.add_texture_slot("lut", SlotDimension::D2);

    let mut asset = EffectAsset::new(capacity, spawner, module)
        .with_name(name)
        // In Quake units, Z up; see the module docs.
        .with_simulation_space(SimulationSpace::Local)
        // Quake simulates every particle, visible or not.
        .with_simulation_condition(SimulationCondition::Always)
        // Quake moves particles with their old velocity before updating it.
        .with_motion_integration(MotionIntegration::PreUpdate)
        // Particles are fully opaque, but `AlphaMode::Opaque` puts them in the retained binned
        // phase, where hanabi 0.20 leaves stale entries behind for despawned effects and panics
        // in `draw()`. The sorted transparent phase is rebuilt every frame.
        .with_alpha_mode(bevy_hanabi::AlphaMode::Blend);

    for init in inits {
        asset = asset.init(init);
    }
    for update in updates {
        asset = asset.update(update);
    }

    asset.render(OrientModifier::new(OrientMode::FaceCameraPosition))
}

/// A burst whose size is set per instance through its `EffectSpawner`.
fn burst() -> SpawnerSettings {
    SpawnerSettings::once(0.0.into())
}

/// A spawner which never spawns by itself, for trails whose spawn count is set every frame.
fn manual() -> SpawnerSettings {
    SpawnerSettings::rate(0.0.into())
}

/// `R_RunParticleEffect`, for counts other than 1024 (see [`explode`]).
///
/// Properties: [`PROP_DIRECTION`], and [`PROP_COLOR_BASE`] (the color with the low 3 bits
/// cleared). The particle count is set per instance.
pub fn run_effect() -> EffectAsset {
    let w = ExprWriter::new();
    let direction = w.add_property(PROP_DIRECTION, Vec3::ZERO.into());
    let color_base = w.add_property(PROP_COLOR_BASE, 0f32.into());

    let init = ParticleInit {
        position: rand_offset3(&w, 16.),
        velocity: w.prop(direction) * w.lit(15.),
        color: ParticleColor::Palette(w.prop(color_base) + rand_int(&w, 8.)),
        die: rand_int(&w, 5.) * w.lit(0.1),
    };

    // `count` is a byte, except for the 1024-particle case which is handled by `explode`.
    build(w, "run_effect", 255, burst(), PType::Grav, init)
}

/// One half of `R_ParticleExplosion`, which splits its particles between `pt_explode` and
/// `pt_explode2` (`ptype` must be one of those).
pub fn explode(ptype: PType) -> EffectAsset {
    assert!(matches!(ptype, PType::Explode | PType::Explode2));

    let w = ExprWriter::new();
    let init = ParticleInit {
        position: rand_offset3(&w, 32.),
        velocity: rand_offset3(&w, 512.),
        color: ParticleColor::Ramp(rand_int(&w, 4.)),
        die: w.lit(5.),
    };

    let name = if ptype == PType::Explode { "explode" } else { "explode2" };
    let count = EXPLOSION_COUNT / 2;
    build(w, name, count, SpawnerSettings::once((count as f32).into()), ptype, init)
}

/// `R_ParticleExplosion2` (`TE_EXPLOSION2`).
///
/// Properties: [`PROP_COLOR_START`] and [`PROP_COLOR_LEN`].
pub fn color_explosion() -> EffectAsset {
    let w = ExprWriter::new();
    let color_start = w.add_property(PROP_COLOR_START, 0f32.into());
    let color_len = w.add_property(PROP_COLOR_LEN, 1f32.into());

    let init = ParticleInit {
        position: rand_offset3(&w, 32.),
        velocity: rand_offset3(&w, 512.),
        color: ParticleColor::Palette(w.prop(color_start) + (rand(&w) * w.prop(color_len)).floor()),
        die: w.lit(0.3),
    };

    build(w, "color_explosion", 512, SpawnerSettings::once(512.0.into()), PType::Blob, init)
}

/// One half of `R_BlobExplosion` (`TE_TAREXPLOSION`), which splits its particles between
/// `pt_blob` and `pt_blob2` (`ptype` must be one of those).
pub fn blob_explosion(ptype: PType) -> EffectAsset {
    let base = match ptype {
        PType::Blob => 66.,
        PType::Blob2 => 150.,
        _ => panic!("blob explosions use `pt_blob` or `pt_blob2`, not {ptype:?}"),
    };

    let w = ExprWriter::new();
    let init = ParticleInit {
        position: rand_offset3(&w, 32.),
        velocity: rand_offset3(&w, 512.),
        color: ParticleColor::Palette(w.lit(base) + rand_int(&w, 6.)),
        // `1 + (rand() & 8) * 0.05`: either 1.0 or 1.4 seconds.
        die: w.lit(1.) + rand_int(&w, 2.) * w.lit(0.4),
    };

    let name = if ptype == PType::Blob { "blob" } else { "blob2" };
    let count = EXPLOSION_COUNT / 2;
    build(w, name, count, SpawnerSettings::once((count as f32).into()), ptype, init)
}

/// `R_LavaSplash`: a 32x32 grid of particles thrown upwards.
///
/// Grid cells are picked at random, since hanabi has no per-spawn particle index.
pub fn lava_splash() -> EffectAsset {
    let w = ExprWriter::new();

    let i = rand_int(&w, 32.) - w.lit(16.);
    let j = rand_int(&w, 32.) - w.lit(16.);
    let dir =
        (j * w.lit(8.) + rand_int(&w, 8.)).vec3(i * w.lit(8.) + rand_int(&w, 8.), w.lit(256.));

    let init = ParticleInit {
        position: dir.clone().x().vec3(dir.clone().y(), rand_int(&w, 64.)),
        velocity: safe_normalize(&w, dir) * (w.lit(50.) + rand_int(&w, 64.)),
        color: ParticleColor::Palette(w.lit(224.) + rand_int(&w, 8.)),
        die: w.lit(2.) + rand_int(&w, 32.) * w.lit(0.02),
    };

    let count = EXPLOSION_COUNT;
    build(w, "lava_splash", count, SpawnerSettings::once((count as f32).into()), PType::Grav, init)
}

/// Number of particles in `R_TeleportSplash`: an 8x8x14 grid.
pub const TELEPORT_COUNT: u32 = 8 * 8 * 14;

/// `R_TeleportSplash`: a box of particles bursting outwards.
///
/// Grid cells are picked at random, since hanabi has no per-spawn particle index.
pub fn teleport_splash() -> EffectAsset {
    let w = ExprWriter::new();

    // `for (i = -16; i < 16; i += 4)`, likewise `j`, and `for (k = -24; k < 32; k += 4)`.
    let i = rand_int(&w, 8.) * w.lit(4.) - w.lit(16.);
    let j = rand_int(&w, 8.) * w.lit(4.) - w.lit(16.);
    let k = rand_int(&w, 14.) * w.lit(4.) - w.lit(24.);

    // Quake swaps `i` and `j` between the direction and the origin.
    let dir = j.clone().vec3(i.clone(), k.clone()) * w.lit(8.);
    let position = i.vec3(j, k) + rand_int3(&w, 4.);

    let init = ParticleInit {
        position,
        velocity: safe_normalize(&w, dir) * (w.lit(50.) + rand_int(&w, 64.)),
        color: ParticleColor::Palette(w.lit(7.) + rand_int(&w, 8.)),
        die: w.lit(0.2) + rand_int(&w, 8.) * w.lit(0.02),
    };

    let count = TELEPORT_COUNT;
    build(
        w,
        "teleport_splash",
        count,
        SpawnerSettings::once((count as f32).into()),
        PType::Grav,
        init,
    )
}

/// Capacity of each trail emitter.
const TRAIL_CAPACITY: u32 = 1024;

/// Adds the [`PROP_TRAIL_START`] and [`PROP_TRAIL_END`] properties, returning a random point on
/// the segment between them.
fn trail_point(w: &ExprWriter) -> WriterExpr {
    let start = w.prop(w.add_property(PROP_TRAIL_START, Vec3::ZERO.into()));
    let end = w.prop(w.add_property(PROP_TRAIL_END, Vec3::ZERO.into()));
    start.mix(end, rand(w))
}

/// Rocket (type 0) and grenade smoke (type 1) trails.
///
/// Properties: the trail segment, and [`PROP_RAMP_BASE`] (0 for rockets, 2 for smoke).
pub fn trail_fire() -> EffectAsset {
    let w = ExprWriter::new();
    let point = trail_point(&w);
    let ramp_base = w.add_property(PROP_RAMP_BASE, 0f32.into());

    let init = ParticleInit {
        position: point + rand_offset3(&w, 6.),
        velocity: w.lit(Vec3::ZERO),
        color: ParticleColor::Ramp(w.prop(ramp_base) + rand_int(&w, 4.)),
        die: w.lit(2.),
    };

    build(w, "trail_fire", TRAIL_CAPACITY, manual(), PType::Fire, init)
}

/// Blood (type 2) and slight blood (type 4) trails.
pub fn trail_blood() -> EffectAsset {
    let w = ExprWriter::new();
    let point = trail_point(&w);

    let init = ParticleInit {
        position: point + rand_offset3(&w, 6.),
        velocity: w.lit(Vec3::ZERO),
        color: ParticleColor::Palette(w.lit(67.) + rand_int(&w, 4.)),
        die: w.lit(2.),
    };

    build(w, "trail_blood", TRAIL_CAPACITY, manual(), PType::Grav, init)
}

/// Scrag (type 3) and Death Knight (type 5) tracers, which drift sideways off the trail.
///
/// Properties: the trail segment, [`PROP_COLOR_BASE`] (52 or 230), and [`PROP_TRAIL_SIDE`]
/// (the sideways velocity, `30 * (dir.y, -dir.x, 0)` for the segment's direction `dir`).
///
/// The side is computed on the CPU because hanabi 0.20 doesn't parenthesize binary expressions
/// before swizzles, so e.g. `(end - start).x` miscompiles.
pub fn trail_tracer() -> EffectAsset {
    let w = ExprWriter::new();
    let point = trail_point(&w);
    let color_base = w.add_property(PROP_COLOR_BASE, 0f32.into());
    let side = w.add_property(PROP_TRAIL_SIDE, Vec3::ZERO.into());

    // Quake alternates the side and color deterministically; pick them at random instead.
    let side = w.prop(side) * (rand_int(&w, 2.) * w.lit(2.) - w.lit(1.));

    let init = ParticleInit {
        position: point,
        velocity: side,
        color: ParticleColor::Palette(w.prop(color_base) + rand_int(&w, 2.) * w.lit(8.)),
        die: w.lit(0.5),
    };

    build(w, "trail_tracer", TRAIL_CAPACITY, manual(), PType::Static, init)
}

/// Vore missile (type 6) trail.
pub fn trail_voor() -> EffectAsset {
    let w = ExprWriter::new();
    let point = trail_point(&w);

    let init = ParticleInit {
        position: point + rand_offset3(&w, 16.),
        velocity: w.lit(Vec3::ZERO),
        color: ParticleColor::Palette(w.lit(9. * 16. + 8.) + rand_int(&w, 4.)),
        die: w.lit(0.3),
    };

    build(w, "trail_voor", TRAIL_CAPACITY, manual(), PType::Static, init)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn effects_build() {
        let effects = [
            run_effect(),
            explode(PType::Explode),
            explode(PType::Explode2),
            color_explosion(),
            blob_explosion(PType::Blob),
            blob_explosion(PType::Blob2),
            lava_splash(),
            teleport_splash(),
            trail_fire(),
            trail_blood(),
            trail_tracer(),
            trail_voor(),
        ];

        for effect in effects {
            assert!(
                effect.texture_layout().get_slot_by_name("lut").is_some(),
                "{} is missing its LUT slot",
                effect.name
            );

            // Catches type errors in the expressions, which would otherwise only show up when
            // the effect is first used.
            if let Err(e) = bevy_hanabi::EffectShaderSources::generate(&effect, None, 0) {
                panic!("{} failed to generate its shaders: {e}", effect.name);
            }
        }
    }
}
