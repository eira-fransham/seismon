#import bevy_core_pipeline::fullscreen_vertex_shader::FullscreenVertexOutput
#import "shaders/colorspaces.wgsl"::{toColorSpace, fromColorSpace}
#import "shaders/blend.wgsl"::blend_opacity

@group(0) @binding(0) var screen_texture: texture_2d<f32>;
@group(0) @binding(1) var texture_sampler: sampler;
struct PostProcessUniforms {
    color_shift: array<vec4<f32>, 5>,
}
@group(0) @binding(2) var<uniform> postprocess_uniforms: PostProcessUniforms;

const COLOR_SPACE: u32 = #{COLORSPACE};
const BLEND_MODE: u32 = #{BLENDMODE};

@fragment
fn main(in: FullscreenVertexOutput) -> @location(0) vec4<f32> {
    var in_color: vec4<f32> = textureSample(screen_texture, texture_sampler, in.uv);

    var color_shifted: vec3<f32> = toColorSpace(COLOR_SPACE, in_color.rgb);
    for (var i = 0; i < 5; i++) {
        var color_shift: vec4<f32> = postprocess_uniforms.color_shift[i];
        var color_shift_rgb: vec3<f32> = toColorSpace(COLOR_SPACE, color_shift.rgb);

        color_shifted = blend_opacity(
            BLEND_MODE,
            color_shifted,
            color_shift_rgb,
            color_shift.a
        );
    }

    return vec4<f32>(fromColorSpace(COLOR_SPACE, color_shifted), in_color.a);
}
