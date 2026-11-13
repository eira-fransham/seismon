
// Constants
const HCV_EPSILON: f32 = 1e-10;
const HSL_EPSILON: f32 = 1e-10;
const HCY_EPSILON: f32 = 1e-10;

const SRGB_GAMMA: f32 = 1.0 / 2.2;
const SRGB_INVERSE_GAMMA: f32 = 2.2;
const SRGB_ALPHA: f32 = 0.055;


// Used to convert from linear RGB to XYZ space
const RGB_2_XYZ = (mat3x3<f32>(
    0.4124564, 0.2126729, 0.0193339,
    0.3575761, 0.7151522, 0.1191920,
    0.1804375, 0.0721750, 0.9503041
));

// Used to convert from XYZ to linear RGB space
const XYZ_2_RGB = (mat3x3<f32>(
     3.2404542,-0.9692660, 0.0556434,
    -1.5371385, 1.8760108,-0.2040259,
    -0.4985314, 0.0415560, 1.0572252
));

const LUMA_COEFFS = vec3<f32>(0.2126, 0.7152, 0.0722);

const HCYwts = vec3<f32>(0.299, 0.587, 0.114);

// Returns the luminance of a !! linear !! rgb color
fn get_luminance(rgb: vec3<f32>) -> f32 {
    return dot(LUMA_COEFFS, rgb);
}

// Converts a linear rgb color to a srgb color (approximated, but fast)
fn rgb_to_srgb_approx(rgb: vec3<f32>) -> vec3<f32> {
    return pow(rgb, vec3<f32>(SRGB_GAMMA));
}

// Converts a srgb color to a rgb color (approximated, but fast)
fn srgb_to_rgb_approx(srgb: vec3<f32>) -> vec3<f32> {
    return pow(srgb, vec3<f32>(SRGB_INVERSE_GAMMA));
}

// Converts a single linear channel to srgb
fn linear_to_srgb(channel: f32) -> f32 {
    if channel <= 0.0031308 {
        return 12.92 * channel;
    } else {
        return (1.0 + SRGB_ALPHA) * pow(channel, 1.0/2.4) - SRGB_ALPHA;
    }
}

// Converts a single srgb channel to rgb
fn srgb_to_linear(channel: f32) -> f32 {
    if channel <= 0.04045 {
        return channel / 12.92;
    } else {
        return pow((channel + SRGB_ALPHA) / (1.0 + SRGB_ALPHA), 2.4);
    }
}

// Converts a linear rgb color to a srgb color (exact, not approximated)
fn rgb_to_srgb(rgb: vec3<f32>) -> vec3<f32> {
    return vec3<f32>(
        linear_to_srgb(rgb.r),
        linear_to_srgb(rgb.g),
        linear_to_srgb(rgb.b)
    );
}

// Converts a srgb color to a linear rgb color (exact, not approximated)
fn srgb_to_rgb(srgb: vec3<f32>) -> vec3<f32> {
    return vec3<f32>(
        srgb_to_linear(srgb.r),
        srgb_to_linear(srgb.g),
        srgb_to_linear(srgb.b)
    );
}

// Converts a color from linear RGB to XYZ space
fn rgb_to_xyz(rgb: vec3<f32>) -> vec3<f32> {
    return RGB_2_XYZ * rgb;
}

// Converts a color from XYZ to linear RGB space
fn xyz_to_rgb(xyz: vec3<f32>) -> vec3<f32> {
    return XYZ_2_RGB * xyz;
}

// Converts a color from XYZ to xyY space (Y is luminosity)
fn xyz_to_xyY(xyz: vec3<f32>) -> vec3<f32> {
    var Y = xyz.y;
    var x = xyz.x / (xyz.x + xyz.y + xyz.z);
    var y = xyz.y / (xyz.x + xyz.y + xyz.z);
    return vec3<f32>(x, y, Y);
}

// Converts a color from xyY space to XYZ space
fn xyY_to_xyz(xyY: vec3<f32>) -> vec3<f32> {
    var Y = xyY.z;
    var x = Y * xyY.x / xyY.y;
    var z = Y * (1.0 - xyY.x - xyY.y) / xyY.y;
    return vec3<f32>(x, Y, z);
}

// Converts a color from linear RGB to xyY space
fn rgb_to_xyY(rgb: vec3<f32>) -> vec3<f32> {
    var xyz = rgb_to_xyz(rgb);
    return xyz_to_xyY(xyz);
}

// Converts a color from xyY space to linear RGB
fn xyY_to_rgb(xyY: vec3<f32>) -> vec3<f32> {
    var xyz = xyY_to_xyz(xyY);
    return xyz_to_rgb(xyz);
}

// Converts a value from linear RGB to HCV (Hue, Chroma, Value)
fn rgb_to_hcv(rgb: vec3<f32>) -> vec3<f32> {
    // Based on work by Sam Hocevar and Emil Persson
    var P = select(vec4(rgb.bg, -1.0, 2.0/3.0), vec4(rgb.gb, 0.0, -1.0/3.0), rgb.g < rgb.b);
    var Q = select(vec4(P.xyw, rgb.r), vec4(rgb.r, P.yzx), rgb.r < P.x);
    var C = Q.x - min(Q.w, Q.y);
    var H = abs((Q.w - Q.y) / (6.0 * C + HCV_EPSILON) + Q.z);
    return vec3<f32>(H, C, Q.x);
}

// Converts from pure Hue to linear RGB
fn hue_to_rgb(hue: f32) -> vec3<f32> {
    var R = abs(hue * 6.0 - 3.0) - 1.0;
    var G = 2.0 - abs(hue * 6.0 - 2.0);
    var B = 2.0 - abs(hue * 6.0 - 4.0);
    return clamp(vec3<f32>(R,G,B), vec3<f32>(0.), vec3<f32>(1.));
}

// Converts from HSV to linear RGB
fn hsv_to_rgb(hsv: vec3<f32>) -> vec3<f32> {
    var rgb = hue_to_rgb(hsv.x);
    return ((rgb - 1.0) * hsv.y + 1.0) * hsv.z;
}

// Converts from HSL to linear RGB
fn hsl_to_rgb(hsl: vec3<f32>) -> vec3<f32> {
    var rgb = hue_to_rgb(hsl.x);
    var C = (1.0 - abs(2.0 * hsl.z - 1.0)) * hsl.y;
    return (rgb - 0.5) * C + hsl.z;
}

// Converts from HCY to linear RGB
fn hcy_to_rgb(hcy_in: vec3<f32>) -> vec3<f32> {
    var hcy = hcy_in;
    var RGB = hue_to_rgb(hcy.x);
    var Z = dot(RGB, HCYwts);
    if hcy.z < Z {
        hcy.y *= hcy.z / Z;
    } else if Z < 1.0 {
        hcy.y *= (1.0 - hcy.z) / (1.0 - Z);
    }
    return (RGB - Z) * hcy.y + hcy.z;
}


// Converts from linear RGB to HSV
fn rgb_to_hsv(rgb: vec3<f32>) -> vec3<f32> {
    var HCV = rgb_to_hcv(rgb);
    var S = HCV.y / (HCV.z + HCV_EPSILON);
    return vec3<f32>(HCV.x, S, HCV.z);
}

// Converts from linear rgb to HSL
fn rgb_to_hsl(rgb: vec3<f32>) -> vec3<f32> {
    var HCV = rgb_to_hcv(rgb);
    var L = HCV.z - HCV.y * 0.5;
    var S = HCV.y / (1.0 - abs(L * 2.0 - 1.0) + HSL_EPSILON);
    return vec3<f32>(HCV.x, S, L);
}

// Converts from rgb to hcy (Hue, Chroma, Luminance)
fn rgb_to_hcy(rgb: vec3<f32>) -> vec3<f32> {
    // Corrected by David Schaeffer
    var HCV = rgb_to_hcv(rgb);
    var Y = dot(rgb, HCYwts);
    var Z = dot(hue_to_rgb(HCV.x), HCYwts);
    if Y < Z {
      HCV.y *= Z / (HCY_EPSILON + Y);
    } else {
      HCV.y *= (1.0 - Z) / (HCY_EPSILON + 1.0 - Y);
    }
    return vec3<f32>(HCV.x, HCV.y, Y);
}

// RGB to YCbCr, ranges [0, 1]
fn rgb_to_ycbcr(rgb: vec3<f32>) -> vec3<f32> {
    var y = 0.299 * rgb.r + 0.587 * rgb.g + 0.114 * rgb.b;
    var cb = (rgb.b - y) * 0.565;
    var cr = (rgb.r - y) * 0.713;

    return vec3<f32>(y, cb, cr);
}

// YCbCr to RGB
fn ycbcr_to_rgb(yuv: vec3<f32>) -> vec3<f32> {
    return vec3<f32>(
        yuv.x + 1.403 * yuv.z,
        yuv.x - 0.344 * yuv.y - 0.714 * yuv.z,
        yuv.x + 1.770 * yuv.y
    );
}

// Additional conversions converting to rgb first and then to the desired
// color space.

// To srgb
fn xyz_to_srgb(xyz: vec3<f32>) -> vec3<f32>  { return rgb_to_srgb(xyz_to_rgb(xyz)); }
fn xyY_to_srgb(xyY: vec3<f32>) -> vec3<f32>  { return rgb_to_srgb(xyY_to_rgb(xyY)); }
fn hue_to_srgb(hue: f32) -> vec3<f32> { return rgb_to_srgb(hue_to_rgb(hue)); }
fn hsv_to_srgb(hsv: vec3<f32>) -> vec3<f32>  { return rgb_to_srgb(hsv_to_rgb(hsv)); }
fn hsl_to_srgb(hsl: vec3<f32>) -> vec3<f32>  { return rgb_to_srgb(hsl_to_rgb(hsl)); }
fn hcy_to_srgb(hcy: vec3<f32>) -> vec3<f32>  { return rgb_to_srgb(hcy_to_rgb(hcy)); }
fn ycbcr_to_srgb(yuv: vec3<f32>) -> vec3<f32>  { return rgb_to_srgb(ycbcr_to_rgb(yuv)); }

// To xyz
fn srgb_to_xyz(srgb: vec3<f32>) -> vec3<f32> { return rgb_to_xyz(srgb_to_rgb(srgb)); }
fn hue_to_xyz(hue: f32) -> vec3<f32>  { return rgb_to_xyz(hue_to_rgb(hue)); }
fn hsv_to_xyz(hsv: vec3<f32>) -> vec3<f32>   { return rgb_to_xyz(hsv_to_rgb(hsv)); }
fn hsl_to_xyz(hsl: vec3<f32>) -> vec3<f32>   { return rgb_to_xyz(hsl_to_rgb(hsl)); }
fn hcy_to_xyz(hcy: vec3<f32>) -> vec3<f32>   { return rgb_to_xyz(hcy_to_rgb(hcy)); }
fn ycbcr_to_xyz(yuv: vec3<f32>) -> vec3<f32> { return rgb_to_xyz(ycbcr_to_rgb(yuv)); }

// To xyY
fn srgb_to_xyY(srgb: vec3<f32>) -> vec3<f32> { return rgb_to_xyY(srgb_to_rgb(srgb)); }
fn hue_to_xyY(hue: f32) -> vec3<f32>  { return rgb_to_xyY(hue_to_rgb(hue)); }
fn hsv_to_xyY(hsv: vec3<f32>) -> vec3<f32>   { return rgb_to_xyY(hsv_to_rgb(hsv)); }
fn hsl_to_xyY(hsl: vec3<f32>) -> vec3<f32>   { return rgb_to_xyY(hsl_to_rgb(hsl)); }
fn hcy_to_xyY(hcy: vec3<f32>) -> vec3<f32>   { return rgb_to_xyY(hcy_to_rgb(hcy)); }
fn ycbcr_to_xyY(yuv: vec3<f32>) -> vec3<f32> { return rgb_to_xyY(ycbcr_to_rgb(yuv)); }

// To HCV
fn srgb_to_hcv(srgb: vec3<f32>) -> vec3<f32> { return rgb_to_hcv(srgb_to_rgb(srgb)); }
fn xyz_to_hcv(xyz: vec3<f32>) -> vec3<f32>   { return rgb_to_hcv(xyz_to_rgb(xyz)); }
fn xyY_to_hcv(xyY: vec3<f32>) -> vec3<f32>   { return rgb_to_hcv(xyY_to_rgb(xyY)); }
fn hue_to_hcv(hue: f32) -> vec3<f32>  { return rgb_to_hcv(hue_to_rgb(hue)); }
fn hsv_to_hcv(hsv: vec3<f32>) -> vec3<f32>   { return rgb_to_hcv(hsv_to_rgb(hsv)); }
fn hsl_to_hcv(hsl: vec3<f32>) -> vec3<f32>   { return rgb_to_hcv(hsl_to_rgb(hsl)); }
fn hcy_to_hcv(hcy: vec3<f32>) -> vec3<f32>   { return rgb_to_hcv(hcy_to_rgb(hcy)); }
fn ycbcr_to_hcv(yuv: vec3<f32>) -> vec3<f32>   { return rgb_to_hcy(ycbcr_to_rgb(yuv)); }

// To HSV
fn srgb_to_hsv(srgb: vec3<f32>) -> vec3<f32> { return rgb_to_hsv(srgb_to_rgb(srgb)); }
fn xyz_to_hsv(xyz: vec3<f32>) -> vec3<f32>   { return rgb_to_hsv(xyz_to_rgb(xyz)); }
fn xyY_to_hsv(xyY: vec3<f32>) -> vec3<f32>   { return rgb_to_hsv(xyY_to_rgb(xyY)); }
fn hue_to_hsv(hue: f32) -> vec3<f32>  { return rgb_to_hsv(hue_to_rgb(hue)); }
fn hsl_to_hsv(hsl: vec3<f32>) -> vec3<f32>   { return rgb_to_hsv(hsl_to_rgb(hsl)); }
fn hcy_to_hsv(hcy: vec3<f32>) -> vec3<f32>   { return rgb_to_hsv(hcy_to_rgb(hcy)); }
fn ycbcr_to_hsv(yuv: vec3<f32>) -> vec3<f32>   { return rgb_to_hsv(ycbcr_to_rgb(yuv)); }

// To HSL
fn srgb_to_hsl(srgb: vec3<f32>) -> vec3<f32> { return rgb_to_hsl(srgb_to_rgb(srgb)); }
fn xyz_to_hsl(xyz: vec3<f32>) -> vec3<f32>   { return rgb_to_hsl(xyz_to_rgb(xyz)); }
fn xyY_to_hsl(xyY: vec3<f32>) -> vec3<f32>   { return rgb_to_hsl(xyY_to_rgb(xyY)); }
fn hue_to_hsl(hue: f32) -> vec3<f32>  { return rgb_to_hsl(hue_to_rgb(hue)); }
fn hsv_to_hsl(hsv: vec3<f32>) -> vec3<f32>   { return rgb_to_hsl(hsv_to_rgb(hsv)); }
fn hcy_to_hsl(hcy: vec3<f32>) -> vec3<f32>   { return rgb_to_hsl(hcy_to_rgb(hcy)); }
fn ycbcr_to_hsl(yuv: vec3<f32>) -> vec3<f32>   { return rgb_to_hsl(ycbcr_to_rgb(yuv)); }

// To HCY
fn srgb_to_hcy(srgb: vec3<f32>) -> vec3<f32> { return rgb_to_hcy(srgb_to_rgb(srgb)); }
fn xyz_to_hcy(xyz: vec3<f32>) -> vec3<f32>   { return rgb_to_hcy(xyz_to_rgb(xyz)); }
fn xyY_to_hcy(xyY: vec3<f32>) -> vec3<f32>   { return rgb_to_hcy(xyY_to_rgb(xyY)); }
fn hue_to_hcy(hue: f32) -> vec3<f32>  { return rgb_to_hcy(hue_to_rgb(hue)); }
fn hsv_to_hcy(hsv: vec3<f32>) -> vec3<f32>   { return rgb_to_hcy(hsv_to_rgb(hsv)); }
fn hsl_to_hcy(hsl: vec3<f32>) -> vec3<f32>   { return rgb_to_hcy(hsl_to_rgb(hsl)); }
fn ycbcr_to_hcy(yuv: vec3<f32>) -> vec3<f32> { return rgb_to_hcy(ycbcr_to_rgb(yuv)); }

// YCbCr
fn srgb_to_ycbcr(srgb: vec3<f32>) -> vec3<f32> { return rgb_to_ycbcr(srgb_to_rgb(srgb)); }
fn xyz_to_ycbcr(xyz: vec3<f32>) -> vec3<f32>   { return rgb_to_ycbcr(xyz_to_rgb(xyz)); }
fn xyY_to_ycbcr(xyY: vec3<f32>) -> vec3<f32>   { return rgb_to_ycbcr(xyY_to_rgb(xyY)); }
fn hue_to_ycbcr(hue: f32) -> vec3<f32>  { return rgb_to_ycbcr(hue_to_rgb(hue)); }
fn hsv_to_ycbcr(hsv: vec3<f32>) -> vec3<f32>   { return rgb_to_ycbcr(hsv_to_rgb(hsv)); }
fn hsl_to_ycbcr(hsl: vec3<f32>) -> vec3<f32>   { return rgb_to_ycbcr(hsl_to_rgb(hsl)); }
fn hcy_to_ycbcr(hcy: vec3<f32>) -> vec3<f32>   { return rgb_to_ycbcr(hcy_to_rgb(hcy)); }

// https://bottosson.github.io/posts/oklab
const kCONEtoLMS = mat3x3<f32>(                
     0.4121656120,  0.2118591070,  0.0883097947,
     0.5362752080,  0.6807189584,  0.2818474174,
     0.0514575653,  0.1074065790,  0.6302613616);
const kLMStoCONE = mat3x3<f32>(
     4.0767245293, -1.2681437731, -0.0041119885,
    -3.3072168827,  2.6093323231, -0.7034763098,
     0.2307590544, -0.3411344290,  1.7068625689);
// OKlab
fn rgb_to_oklab(color: vec3<f32>) -> vec3<f32> { return pow(kCONEtoLMS*color, vec3<f32>(1.0/3.0)); }
fn oklab_to_rgb(color: vec3<f32>) -> vec3<f32> { return kLMStoCONE*(color*color*color); }

fn toColorSpace(colorspace: u32, val: vec3<f32>) -> vec3<f32> {
    switch colorspace {
        case 1u: {
            return rgb_to_xyz(val);
        }
        case 2u: {
            return rgb_to_xyY(val);
        }
        case 3u: {
            return rgb_to_hsl(val);
        }
        case 4u: {
            return rgb_to_hsv(val);
        }
        case 5u: {
            return rgb_to_srgb(val);
        }
        case 6u: {
            return rgb_to_hcy(val);
        }
        case 7u: {
            return rgb_to_ycbcr(val);
        }
        case 8u: {
            return rgb_to_oklab(val);
        }
        default: {
            return val;
        }
    }
}

fn fromColorSpace(colorspace: u32, val: vec3<f32>) -> vec3<f32> {
    switch colorspace {
        case 1u: {
            return xyz_to_rgb(val);
        }
        case 2u: {
            return xyY_to_rgb(val);
        }
        case 3u: {
            return hsl_to_rgb(val);
        }
        case 4u: {
            return hsv_to_rgb(val);
        }
        case 5u: {
            return srgb_to_rgb(val);
        }
        case 6u: {
            return hcy_to_rgb(val);
        }
        case 7u: {
            return ycbcr_to_rgb(val);
        }
        case 8u: {
            return oklab_to_rgb(val);
        }
        default: {
            return val;
        }
    }
}

