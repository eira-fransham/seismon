#version 450

const uint TEXTURE_KIND_NORMAL = 0;
const uint TEXTURE_KIND_WARP = 1;
const uint TEXTURE_KIND_SKY = 2;

layout(location = 0) in vec3 a_position;
layout(location = 1) in vec3 a_normal;
layout(location = 2) in vec2 a_diffuse;
layout(location = 3) in vec2 a_lightmap;
layout(location = 4) in uvec4 a_lightmap_anim;

layout(push_constant) uniform PushConstants {
  mat4 transform;
  mat3 model_view;
  uint texture_kind;
} push_constants;

layout(location = 0) out vec3 f_normal;
layout(location = 1) out vec3 f_diffuse;
layout(location = 2) out vec2 f_lightmap;
layout(location = 3) out uvec4 f_lightmap_anim;

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

void main() {
    if (push_constants.texture_kind == TEXTURE_KIND_SKY) {
        f_diffuse = a_position;
    } else {
        f_diffuse = vec3(a_diffuse, 0.);
    }

    f_normal = transpose(inv(push_constants.model_view)) * convert(a_normal);
    f_lightmap = a_lightmap;
    f_lightmap_anim = a_lightmap_anim;
    gl_Position = push_constants.transform * vec4(convert(a_position), 1.0);

}
