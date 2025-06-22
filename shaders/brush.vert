#version 450

const uint TEXTURE_KIND_NORMAL = 0;
const uint TEXTURE_KIND_WARP = 1;
const uint TEXTURE_KIND_SKY = 2;

layout(location = 0) in vec3 a_position;
layout(location = 1) in vec3 a_normal;
layout(location = 2) in vec2 a_diffuse;
layout(location = 3) in uint a_tex_kind;
layout(location = 4) in uvec4 a_lightmap_anim;
layout(location = 5) in vec2 a_lightmap_coord_0;
layout(location = 6) in vec2 a_lightmap_coord_1;
layout(location = 7) in vec2 a_lightmap_coord_2;
layout(location = 8) in vec2 a_lightmap_coord_3;

layout(push_constant) uniform PushConstants {
  mat4 transform;
  mat3 model_view;
} push_constants;

layout(location = 0) out vec3 f_normal;
layout(location = 1) out vec3 f_diffuse;
flat layout(location = 2) out uint f_tex_kind;
flat layout(location = 3) out uvec4 f_lightmap_anim;
layout(location = 4) out vec2 f_lightmap_coord_0;
layout(location = 5) out vec2 f_lightmap_coord_1;
layout(location = 6) out vec2 f_lightmap_coord_2;
layout(location = 7) out vec2 f_lightmap_coord_3;

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

float det(mat2 matrix) {
    return matrix[0].x * matrix[1].y - matrix[0].y * matrix[1].x;
}

mat3 inv(mat3 matrix) {
    vec3 row0 = matrix[0];
    vec3 row1 = matrix[1];
    vec3 row2 = matrix[2];

    vec3 minors0 = vec3(
        det(mat2(row1.y, row1.z, row2.y, row2.z)),
        det(mat2(row1.z, row1.x, row2.z, row2.x)),
        det(mat2(row1.x, row1.y, row2.x, row2.y))
    );
    vec3 minors1 = vec3(
        det(mat2(row2.y, row2.z, row0.y, row0.z)),
        det(mat2(row2.z, row2.x, row0.z, row0.x)),
        det(mat2(row2.x, row2.y, row0.x, row0.y))
    );
    vec3 minors2 = vec3(
        det(mat2(row0.y, row0.z, row1.y, row1.z)),
        det(mat2(row0.z, row0.x, row1.z, row1.x)),
        det(mat2(row0.x, row0.y, row1.x, row1.y))
    );

    mat3 adj = transpose(mat3(minors0, minors1, minors2));

    return (1.0 / dot(row0, minors0)) * adj;
}

vec2 unpack(uint packed) {
    return vec2(
        float((packed & 0x0000FFFF)) / float(0xFFFF),
        float((packed & 0xFFFF0000) >> 16) / float(0xFFFF)
    );
}

void main() {
    if (a_tex_kind== TEXTURE_KIND_SKY) {
        f_diffuse = a_position;
    } else {
        f_diffuse = vec3(a_diffuse, 0.);
    }

    f_tex_kind = a_tex_kind;

    f_normal = transpose(inv(push_constants.model_view)) * convert(a_normal);

    f_lightmap_anim = a_lightmap_anim;

    f_lightmap_coord_0 = a_lightmap_coord_0;
    f_lightmap_coord_1 = a_lightmap_coord_1;
    f_lightmap_coord_2 = a_lightmap_coord_2;
    f_lightmap_coord_3 = a_lightmap_coord_3;

    gl_Position = push_constants.transform * vec4(convert(a_position), 1.0);

}
