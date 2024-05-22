#version 450 core

uniform mat4 transform;
out vec2 screen;
out vec3 origin, ray;

void main() {
  vec2 position[] = vec2[](vec2(-1.0, -1.0), vec2(-1.0, +1.0), vec2(+1.0, -1.0),
                           vec2(+1.0, +1.0));

  screen = position[gl_VertexID];

  vec4 tw = transform * vec4(screen, -1.0, 1.0);
  vec4 tr = transform * vec4(screen, +1.0, 1.0);

  origin = tw.xyz / tw.w;
  ray = normalize(tr.xyz / tr.w - origin);

  gl_Position = vec4(screen, 0.0, 1.0);
}
