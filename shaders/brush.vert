#version 450

const uint TEXTURE_KIND_NORMAL = 0;
const uint TEXTURE_KIND_WARP = 1;
const uint TEXTURE_KIND_SKY = 2;

layout(location = 0) in vec3 a_position;
layout(location = 1) in vec3 a_normal;
layout(location = 2) in vec2 a_diffuse;
layout(location = 3) in uvec4 a_lightmap_anim;
layout(location = 4) in vec2 a_lightmap_coord_0;
layout(location = 5) in vec2 a_lightmap_coord_1;
layout(location = 6) in vec2 a_lightmap_coord_2;
layout(location = 7) in vec2 a_lightmap_coord_3;

layout(push_constant) uniform PushConstants {
  mat4 transform;
  mat3 inv_view;
} push_constants;

layout(location = 0) out vec3 f_normal;
layout(location = 1) out vec3 f_diffuse;
flat layout(location = 2) out uvec4 f_lightmap_anim;
layout(location = 3) out vec2 f_lightmap_coord_0;
layout(location = 4) out vec2 f_lightmap_coord_1;
layout(location = 5) out vec2 f_lightmap_coord_2;
layout(location = 6) out vec2 f_lightmap_coord_3;

// set 0: per-frame
layout(set = 0, binding = 0) uniform FrameUniforms {
    vec4 light_anim_frames[16];
    vec4 camera_pos;
    float time;
    float sky_time;
} frame_uniforms;

// convert from Quake coordinates
vec3 convert(vec3 from) {
  return vec3(-from.y, from.z, -from.x);
}

void main() {
    if (INPUT_TEXTURE_KIND == TEXTURE_KIND_SKY) {
        f_diffuse = a_position;
    } else {
        f_diffuse = vec3(a_diffuse, 0.);
    }

    f_normal = push_constants.inv_view * convert(a_normal);

    f_lightmap_anim = a_lightmap_anim;

    f_lightmap_coord_0 = a_lightmap_coord_0;
    f_lightmap_coord_1 = a_lightmap_coord_1;
    f_lightmap_coord_2 = a_lightmap_coord_2;
    f_lightmap_coord_3 = a_lightmap_coord_3;

    gl_Position = push_constants.transform * vec4(convert(a_position), 1.0);

}
