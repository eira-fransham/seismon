#version 450

layout(location = 0) in vec3 a_position1;
// layout(location = 1) in vec3 a_position2;
layout(location = 2) in vec3 a_normal;
layout(location = 3) in vec2 a_diffuse;

layout(push_constant) uniform PushConstants {
  mat4 transform;
  mat3 model_view;
} push_constants;

layout(location = 0) out vec3 f_normal;
layout(location = 1) out vec2 f_diffuse;

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

// convert from Quake coordinates
vec3 convert(vec3 from) {
  return vec3(-from.y, from.z, -from.x);
}

void main() {
  f_normal = transpose(inv(push_constants.model_view)) * convert(a_normal);
  f_diffuse = a_diffuse;
  gl_Position = push_constants.transform * vec4(convert(a_position1), 1.0);
}
