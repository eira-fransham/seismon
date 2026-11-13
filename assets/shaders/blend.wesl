fn blendAdd_comp(base: f32, blend: f32) -> f32 {
    return min(base+blend,1.0);
}
fn blendAdd(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return min(base+blend,vec3<f32>(1.0));
}
fn blendAdd_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendAdd(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendAverage(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return (base+blend)/2.0;
}
fn blendAverage_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendAverage(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendColorBurn_comp(base: f32, blend: f32) -> f32 {
    return select(blend,max((1.0-((1.0-base)/blend)),0.0), (blend==0.0));
}
fn blendColorBurn(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return vec3<f32>(blendColorBurn_comp(base.r,blend.r),blendColorBurn_comp(base.g,blend.g),blendColorBurn_comp(base.b,blend.b));
}
fn blendColorBurn_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendColorBurn(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendColorDodge_comp(base: f32, blend: f32) -> f32 {
    return select(blend,min(base/(1.0-blend),1.0), (blend==1.0));
}
fn blendColorDodge(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return vec3<f32>(blendColorDodge_comp(base.r,blend.r),blendColorDodge_comp(base.g,blend.g),blendColorDodge_comp(base.b,blend.b));
}
fn blendColorDodge_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendColorDodge(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendDarken_comp(base: f32, blend: f32) -> f32 {
    return min(blend,base);
}
fn blendDarken(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return vec3<f32>(blendDarken_comp(base.r,blend.r),blendDarken_comp(base.g,blend.g),blendDarken_comp(base.b,blend.b));
}
fn blendDarken_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendDarken(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendDifference(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return abs(base-blend);
}
fn blendDifference_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendDifference(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendExclusion(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return base+blend-2.0*base*blend;
}
fn blendExclusion_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendExclusion(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendGlow(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return blendReflect(blend,base);
}
fn blendGlow_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendGlow(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendHardLight(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return blendOverlay(blend,base);
}
fn blendHardLight_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendHardLight(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendHardMix_comp(base: f32, blend: f32) -> f32 {
    return select(0.0, 1.0, blendVividLight_comp(base,blend) < 0.5);
}
fn blendHardMix(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return vec3<f32>(blendHardMix_comp(base.r,blend.r),blendHardMix_comp(base.g,blend.g),blendHardMix_comp(base.b,blend.b));
}
fn blendHardMix_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendHardMix(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendLighten_comp(base: f32, blend: f32) -> f32 {
    return max(blend,base);
}
fn blendLighten(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return vec3<f32>(blendLighten_comp(base.r,blend.r),blendLighten_comp(base.g,blend.g),blendLighten_comp(base.b,blend.b));
}
fn blendLighten_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendLighten(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendLinearBurn_comp(base: f32, blend: f32) -> f32 {
    // Note : Same implementation as BlendSubtractf
    return max(base+blend-1.0,0.0);
}
fn blendLinearBurn(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    // Note : Same implementation as BlendSubtract
    return max(base+blend-vec3<f32>(1.0),vec3<f32>(0.0));
}
fn blendLinearBurn_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendLinearBurn(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendLinearDodge_comp(base: f32, blend: f32) -> f32 {
    // Note : Same implementation as BlendAddf
    return min(base+blend,1.0);
}
fn blendLinearDodge(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    // Note : Same implementation as BlendAdd
    return min(base+blend,vec3<f32>(1.0));
}
fn blendLinearDodge_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendLinearDodge(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendLinearLight_comp(base: f32, blend: f32) -> f32 {
    return select(blendLinearBurn_comp(base,(2.0*blend)), blendLinearDodge_comp(base,(2.0*(blend-0.5))), blend<0.5);
}
fn blendLinearLight(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return vec3<f32>(blendLinearLight_comp(base.r,blend.r),blendLinearLight_comp(base.g,blend.g),blendLinearLight_comp(base.b,blend.b));
}
fn blendLinearLight_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendLinearLight(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendMultiply(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return base*blend;
}
fn blendMultiply_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendMultiply(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendNegation(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return vec3<f32>(1.0)-abs(vec3<f32>(1.0)-base-blend);
}
fn blendNegation_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendNegation(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendNormal(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return blend;
}
fn blendNormal_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendNormal(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendOverlay_comp(base: f32, blend: f32) -> f32 {
    return select((2.0*base*blend),(1.0-2.0*(1.0-base)*(1.0-blend)), base<0.5);
}
fn blendOverlay(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return vec3<f32>(blendOverlay_comp(base.r,blend.r),blendOverlay_comp(base.g,blend.g),blendOverlay_comp(base.b,blend.b));
}
fn blendOverlay_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendOverlay(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendPhoenix(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return min(base,blend)-max(base,blend)+vec3<f32>(1.0);
}
fn blendPhoenix_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendPhoenix(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendPinLight_comp(base: f32, blend: f32) -> f32 {
    return select(blendDarken_comp(base,(2.0*blend)), blendLighten_comp(base,(2.0*(blend-0.5))), blend<0.5);
}
fn blendPinLight(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return vec3<f32>(blendPinLight_comp(base.r,blend.r),blendPinLight_comp(base.g,blend.g),blendPinLight_comp(base.b,blend.b));
}
fn blendPinLight_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendPinLight(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendReflect_comp(base: f32, blend: f32) -> f32 {
    return select(blend,min(base*base/(1.0-blend),1.0), (blend==1.0));
}
fn blendReflect(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return vec3<f32>(blendReflect_comp(base.r,blend.r),blendReflect_comp(base.g,blend.g),blendReflect_comp(base.b,blend.b));
}
fn blendReflect_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendReflect(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendScreen_comp(base: f32, blend: f32) -> f32 {
    return 1.0-((1.0-base)*(1.0-blend));
}
fn blendScreen(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return vec3<f32>(blendScreen_comp(base.r,blend.r),blendScreen_comp(base.g,blend.g),blendScreen_comp(base.b,blend.b));
}
fn blendScreen_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendScreen(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendSoftLight_comp(base: f32, blend: f32) -> f32 {
    return select((2.0*base*blend+base*base*(1.0-2.0*blend)),(sqrt(base)*(2.0*blend-1.0)+2.0*base*(1.0-blend)), (blend<0.5));
}
fn blendSoftLight(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return vec3<f32>(blendSoftLight_comp(base.r,blend.r),blendSoftLight_comp(base.g,blend.g),blendSoftLight_comp(base.b,blend.b));
}
fn blendSoftLight_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendSoftLight(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendSubstract_comp(base: f32, blend: f32) -> f32 {
    return max(base+blend-1.0,0.0);
}
fn blendSubstract(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return max(base+blend-vec3<f32>(1.0),vec3<f32>(0.0));
}
fn blendSubstract_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendSubstract(base, blend) * opacity + blend * (1.0 - opacity));
}
fn blendSubtract_comp(base: f32, blend: f32) -> f32 {
    return max(base+blend-1.0,0.0);
}
fn blendSubtract(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return max(base+blend-vec3<f32>(1.0),vec3<f32>(0.0));
}
fn blendSubtract_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendSubtract(base, blend) * opacity + base * (1.0 - opacity));
}
fn blendVividLight_comp(base: f32, blend: f32) -> f32 {
    return select(blendColorBurn_comp(base,(2.0*blend)), blendColorDodge_comp(base,(2.0*(blend-0.5))), blend<0.5);
}
fn blendVividLight(base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    return vec3<f32>(blendVividLight_comp(base.r,blend.r),blendVividLight_comp(base.g,blend.g),blendVividLight_comp(base.b,blend.b));
}
fn blendVividLight_opacity(base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    return (blendVividLight(base, blend) * opacity + base * (1.0 - opacity));
}
fn blend(mode: u32, base: vec3<f32>, blend: vec3<f32>) -> vec3<f32> {
    switch mode {
        case 1u: {
            return blendAdd(base, blend);
        }
        case 2u: {
            return blendAverage(base, blend);
        }
        case 3u: {
            return blendColorBurn(base, blend);
        }
        case 4u: {
            return blendColorDodge(base, blend);
        }
        case 5u: {
            return blendDarken(base, blend);
        }
        case 6u: {
            return blendDifference(base, blend);
        }
        case 7u: {
            return blendExclusion(base, blend);
        }
        case 8u: {
            return blendGlow(base, blend);
        }
        case 9u: {
            return blendHardLight(base, blend);
        }
        case 10u: {
            return blendHardMix(base, blend);
        }
        case 11u: {
            return blendLighten(base, blend);
        }
        case 12u: {
            return blendLinearBurn(base, blend);
        }
        case 13u: {
            return blendLinearDodge(base, blend);
        }
        case 14u: {
            return blendLinearLight(base, blend);
        }
        case 15u: {
            return blendMultiply(base, blend);
        }
        case 16u: {
            return blendNegation(base, blend);
        }
        case 17u: {
            return blendNormal(base, blend);
        }
        case 18u: {
            return blendOverlay(base, blend);
        }
        case 19u: {
            return blendPhoenix(base, blend);
        }
        case 20u: {
            return blendPinLight(base, blend);
        }
        case 21u: {
            return blendReflect(base, blend);
        }
        case 22u: {
            return blendScreen(base, blend);
        }
        case 23u: {
            return blendSoftLight(base, blend);
        }
        case 24u: {
            return blendSubtract(base, blend);
        }
        case 25u: {
            return blendVividLight(base, blend);
        }
        default: {
            return base + blend;
        }
    }
}
fn blend_opacity(mode: u32, base: vec3<f32>, blend: vec3<f32>, opacity: f32) -> vec3<f32> {
    switch mode {
        case 1u: {
            return blendAdd_opacity(base, blend, opacity);
        }
        case 2u: {
            return blendAverage_opacity(base, blend, opacity);
        }
        case 3u: {
            return blendColorBurn_opacity(base, blend, opacity);
        }
        case 4u: {
            return blendColorDodge_opacity(base, blend, opacity);
        }
        case 5u: {
            return blendDarken_opacity(base, blend, opacity);
        }
        case 6u: {
            return blendDifference_opacity(base, blend, opacity);
        }
        case 7u: {
            return blendExclusion_opacity(base, blend, opacity);
        }
        case 8u: {
            return blendGlow_opacity(base, blend, opacity);
        }
        case 9u: {
            return blendHardLight_opacity(base, blend, opacity);
        }
        case 10u: {
            return blendHardMix_opacity(base, blend, opacity);
        }
        case 11u: {
            return blendLighten_opacity(base, blend, opacity);
        }
        case 12u: {
            return blendLinearBurn_opacity(base, blend, opacity);
        }
        case 13u: {
            return blendLinearDodge_opacity(base, blend, opacity);
        }
        case 14u: {
            return blendLinearLight_opacity(base, blend, opacity);
        }
        case 15u: {
            return blendMultiply_opacity(base, blend, opacity);
        }
        case 16u: {
            return blendNegation_opacity(base, blend, opacity);
        }
        case 17u: {
            return blendNormal_opacity(base, blend, opacity);
        }
        case 18u: {
            return blendOverlay_opacity(base, blend, opacity);
        }
        case 19u: {
            return blendPhoenix_opacity(base, blend, opacity);
        }
        case 20u: {
            return blendPinLight_opacity(base, blend, opacity);
        }
        case 21u: {
            return blendReflect_opacity(base, blend, opacity);
        }
        case 22u: {
            return blendScreen_opacity(base, blend, opacity);
        }
        case 23u: {
            return blendSoftLight_opacity(base, blend, opacity);
        }
        case 24u: {
            return blendSubtract_opacity(base, blend, opacity);
        }
        case 25u: {
            return blendVividLight_opacity(base, blend, opacity);
        }
        default: {
            return blendNormal_opacity(base, blend, opacity);
        }
    }
}

