#version 450 core

uniform float time;
uniform int display, scene;

in vec2 screen;
in vec3 origin, ray;

out vec4 result;

int evaluations = 0;

float dot2(vec2 v) { return dot(v, v); }

// 2D
// https://iquilezles.org/articles/distfunctions2d/

float sdCircle(float radius, vec2 point) { return length(point) - radius; }

float sdRect(vec2 b, vec2 p) {
  vec2 d = abs(p) - b;
  return length(max(d, 0.0)) + min(max(d.x, d.y), 0.0);
}

float sdRoundedBox(vec2 size, vec4 radius, vec2 point) {
  radius.xy = (point.x > 0.0) ? radius.xy : radius.zw;
  radius.x = (point.y > 0.0) ? radius.x : radius.y;
  vec2 q = abs(point) - size + radius.x;
  return min(max(q.x, q.y), 0.0) + length(max(q, 0.0)) - radius.x;
}

float sdTriangle(vec2 p0, vec2 p1, vec2 p2, vec2 point) {
  vec2 e0 = p1 - p0, e1 = p2 - p1, e2 = p0 - p2;
  vec2 v0 = point - p0, v1 = point - p1, v2 = point - p2;
  vec2 pq0 = v0 - e0 * clamp(dot(v0, e0) / dot(e0, e0), 0.0, 1.0);
  vec2 pq1 = v1 - e1 * clamp(dot(v1, e1) / dot(e1, e1), 0.0, 1.0);
  vec2 pq2 = v2 - e2 * clamp(dot(v2, e2) / dot(e2, e2), 0.0, 1.0);
  float s = sign(e0.x * e2.y - e0.y * e2.x);
  vec2 d = min(min(vec2(dot(pq0, pq0), s * (v0.x * e0.y - v0.y * e0.x)),
                   vec2(dot(pq1, pq1), s * (v1.x * e1.y - v1.y * e1.x))),
               vec2(dot(pq2, pq2), s * (v2.x * e2.y - v2.y * e2.x)));
  return -sqrt(d.x) * sign(d.y);
}

float sdCoolS(vec2 point) {
  float six = (point.y < 0.0) ? -point.x : point.x;
  point.x = abs(point.x);
  point.y = abs(point.y) - 0.2;

  float rex = point.x - min(round(point.x / 0.4), 0.4);
  float aby = abs(point.y - 0.2) - 0.6;

  float d = dot2(vec2(six, -point.y) - clamp(0.5 * (six - point.y), 0.0, 0.2));
  d = min(d,
          dot2(vec2(point.x, -aby) - clamp(0.5 * (point.x - aby), 0.0, 0.4)));
  d = min(d, dot2(vec2(rex, point.y - clamp(point.y, 0.0, 0.4))));

  float s = 2.0 * point.x + aby + abs(aby + 0.4) - 0.4;

  return sqrt(d) * sign(s) + 0.05;
}

// 3D
// https://iquilezles.org/articles/distfunctions/

float sdSphere(float radius, vec3 point) { return length(point) - radius; }

float sdBox(vec3 size, vec3 point) {
  vec3 q = abs(point) - size;
  return length(max(q, 0.0)) + min(max(q.x, max(q.y, q.z)), 0.0);
}

float sdCappedCylinder(float h, float r, vec3 point) {
  vec2 d = abs(vec2(length(point.xz), point.y)) - vec2(r, h);
  return min(max(d.x, d.y), 0.0) + length(max(d, 0.0));
}

float sdVerticalCapsule(float h, float r, vec3 point) {
  point.y -= clamp(point.y, 0.0, h);
  return length(point) - r;
}

float sdBoxFrame(vec3 b, float e, vec3 point) {
  vec3 p = abs(point) - b;
  vec3 q = abs(p + e) - e;
  return min(min(length(max(vec3(p.x, q.y, q.z), 0.0)) +
                     min(max(p.x, max(q.y, q.z)), 0.0),
                 length(max(vec3(q.x, p.y, q.z), 0.0)) +
                     min(max(q.x, max(p.y, q.z)), 0.0)),
             length(max(vec3(q.x, q.y, p.z), 0.0)) +
                 min(max(q.x, max(q.y, p.z)), 0.0));
}

float sdCappedCone(float h, float r1, float r2, vec3 p) {
  vec2 q = vec2(length(p.xz), p.y);
  vec2 k1 = vec2(r2, h);
  vec2 k2 = vec2(r2 - r1, 2.0 * h);
  vec2 ca = vec2(q.x - min(q.x, (q.y < 0.0) ? r1 : r2), abs(q.y) - h);
  vec2 cb = q - k1 + k2 * clamp(dot(k1 - q, k2) / dot2(k2), 0.0, 1.0);
  float s = (cb.x < 0.0 && ca.y < 0.0) ? -1.0 : 1.0;
  return s * sqrt(min(dot2(ca), dot2(cb)));
}

float sdCross(vec3 p) {
  float da = sdRect(vec2(1.0), p.xy);
  float db = sdRect(vec2(1.0), p.yz);
  float dc = sdRect(vec2(1.0), p.zx);
  return min(da, min(db, dc));
}

// Operators
// https://iquilezles.org/articles/distfunctions/

float opUnion(float d1, float d2) { return min(d2, d1); }
float opInter(float d1, float d2) { return max(d2, d1); }

float opSmoothUnion(float k, float a, float b) {
  float h = max(k - abs(a - b), 0.0);
  return min(a, b) - h * h * 0.25 / k;
}

float opSmoothInter(float k, float a, float b) {
  float h = max(k - abs(a - b), 0.0);
  return max(a, b) + h * h * 0.25 / k;
}

float opExtrusion(float height, vec3 point, float d) {
  vec2 w = vec2(d, abs(point.y) - height);
  return min(max(w.x, w.y), 0.0) + length(max(w, 0.0));
}

// https://iquilezles.org/articles/sdfrepetition/

vec3 opLimitedRepetition(float s, vec3 lmin, vec3 lmax, vec3 point) {
  return point - s * clamp(round(point / s), -lmin, lmax);
}

vec3 opCircleRepetition(float n, float s, vec3 point) {
  float a = atan(point.z, point.x);
  float i = round(a * n / 6.283185 - s) + s;
  float c = i * 6.283185 / n;
  return vec3(mat2(cos(c), -sin(c), sin(c), cos(c)) * point.xz, point.y).xzy;
}

// Noise
// https://iquilezles.org/articles/fbmsdf/

float hash(vec3 p) {
  p = 17.0 * fract(p * 0.3183099 + vec3(.11, .17, .13));
  return fract(p.x * p.y * p.z * (p.x + p.y + p.z));
}

float fbmSph(ivec3 i, vec3 f, ivec3 c) {
  // random radius at grid vertex i+c
  float rad = 0.5 * hash(i + c);
  // distance to sphere at grid vertex i+c
  return length(f - vec3(c)) - rad;
}

float sdFbmBase(vec3 p) {
  ivec3 i = ivec3(floor(p));
  vec3 f = fract(p);
  return min(
      min(min(fbmSph(i, f, ivec3(0, 0, 0)), fbmSph(i, f, ivec3(0, 0, 1))),
          min(fbmSph(i, f, ivec3(0, 1, 0)), fbmSph(i, f, ivec3(0, 1, 1)))),
      min(min(fbmSph(i, f, ivec3(1, 0, 0)), fbmSph(i, f, ivec3(1, 0, 1))),
          min(fbmSph(i, f, ivec3(1, 1, 0)), fbmSph(i, f, ivec3(1, 1, 1)))));
}

float opFbm(float d, vec3 p) {
  float s = 1.0;
  for (int i = 0; i < 4; i++) {
    float n = s * sdFbmBase(p);

    n = opSmoothInter(0.3 * s, n, d - 0.1 * s);
    d = opSmoothUnion(0.3 * s, n, d);

    p = mat3(0.00, 1.60, 1.20, -1.60, 0.72, -0.96, -1.20, -0.96, 1.28) * p;
    s = 0.5 * s;
  }
  return d;
}

// Compounds

float sdRing(float r, float d, float h, vec3 p) {
  return opExtrusion(h, p, abs(sdCircle(r, p.xz)) - d);
}

float sdTree(vec3 o, vec3 point, float d) {
  // https://iquilezles.org/articles/sdfbounding/
  if (sdSphere(2.0, point - o) > d)
    return d;
  d = sdCappedCone(1.25, 0.75, 0.1, point - o - vec3(0.0, 0.25, 0.0));
  return opFbm(d, point);
}

// Scenes

float sceneInstitute(vec3 point) {
  float d = 1e20;

  // Podłoże
  float ground = opSmoothUnion(
      20.0, point.y + 5.0,
      sdBox(vec3(11.0, 10.0, 11.0) - 2.0, point - vec3(0.0, -19.5, 1.0)) - 2.0);

  d = opUnion(d, ground);

  {
    // Symetria
    vec3 p = point - vec3(0.0, -2.0, -4.0);
    p.x = abs(p.x);

    float db = 1e20;

    // Sala wykładowa wielka
    db = opUnion(db, sdBox(vec3(1.0, 1.50, 2.0), p - vec3(4.0, 1.50, 0.0)));
    db = opUnion(db, sdBox(vec3(1.0, 1.25, 2.0), p - vec3(6.0, 1.25, 0.0)));
    db = opUnion(db, sdBox(vec3(1.0, 1.00, 2.0), p - vec3(8.0, 1.00, 0.0)));

    // Sala wykładowa kameralna
    db = opUnion(db, sdCappedCylinder(0.75, 3.5, p - vec3(0.0, 2.25, 0.0)));

    // Główna bryła
    db = opUnion(db, sdBox(vec3(9.0, 1.5, 5.0), p - vec3(0.0, 1.5, 7.0)));
    db = opUnion(db, sdBox(vec3(5.0, 1.0, 3.0), p - vec3(0.0, 4.0, 9.0)));

    // Restauracja plastyczna
    db = opUnion(db, opExtrusion(1.5, p - vec3(0.0, 1.5, 0.0),
                                 sdTriangle(vec2(7.0, 12.0), vec2(7.0, 14.0),
                                            vec2(0.0, 12.0), p.xz)));

    d = opUnion(d, db);

    // Wejście + daszek + kolumny
    if (sdSphere(4.5, p - vec3(0.0, 2.0, 0.0)) < d) {
      vec3 rp = opCircleRepetition(20.0, 0.5, p - vec3(0.0, 0.0, 0.0));
      d = opUnion(d, sdCappedCylinder(0.75, 0.2, rp - vec3(3.4, 0.75, 0.0)));
      d = opUnion(d, sdCappedCylinder(2.0, 2.0, p - vec3(0.0, 2.0, 0.0)));
      d = opUnion(d, sdBox(vec3(3.0, 1.5, 1.0), p - vec3(0.0, 1.0, 0.0)));
      d = opUnion(d, sdRing(3.5, 0.1, 0.1, p - vec3(0.0, 2.9, 0.0)));
      d = opUnion(d, sdRing(3.5, 0.1, 0.1, p - vec3(0.0, 1.4, 0.0)));
    }

    // Okrągłe schody + kolumny
    if (sdSphere(4.0, p - vec3(0.0, 2.25, 12.0)) < d) {
      vec3 rp = opCircleRepetition(20.0, 0.5, p - vec3(0.0, 0.0, 12.0));
      d = opUnion(d, sdCappedCylinder(0.75, 2.0, p - vec3(0.0, 0.75, 12.0)));
      d = opUnion(d, sdCappedCylinder(2.25, 1.0, p - vec3(0.0, 2.25, 12.0)));
      d = opUnion(d, sdCappedCylinder(0.75, 0.1, rp - vec3(3.0, 0.75, 0.0)));
      d = opUnion(d, sdRing(3.0, 0.1, 0.1, p - vec3(0.0, 1.5, 12.0)));
    }

    // Piętra 2 i 3 + kolumny
    float dr = opExtrusion(
        1.0, p - vec3(7.0, 4.0, 5.0),
        sdRoundedBox(vec2(2.0, 5.0), vec4(2.0), p.xz - vec2(5.0, 7.0)));

    if (dr < d + 2) {
      d = opUnion(d, dr);

      vec3 rp =
          opLimitedRepetition(2.0, vec3(1.0, 0.0, 2.0), vec3(1.0, 0.0, 3.0),
                              p - vec3(5.0, 4.0, 6.0));

      d = opUnion(d, sdCappedCylinder(1.0, 0.2, rp));

      float rb = sdRoundedBox(vec2(2.0, 5.0), vec4(0.0), p.xz - vec2(5.0, 7.0));
      d = opUnion(d, opExtrusion(0.1, p - vec3(7.0, 4.9, 5.0), abs(rb) - 0.2));
    }

    float rb = sdRoundedBox(vec2(3.0, 3.0), vec4(0.0), p.xz - vec2(0.0, 9.0));
    d = opUnion(d, opExtrusion(0.1, p - vec3(0.0, 4.9, 9.0), abs(rb) - 0.2));

    // Bluszcz
    float vine = sdSphere(3.0, point - vec3(8.0, -1.5, 8.0));
    if (vine < d)
      d = opUnion(d, opFbm(opInter(db, vine), point));
  }

  // Drzewa
  d = opUnion(d, sdTree(vec3(8.0, -1.0, -10.0), point, d));
  d = opUnion(d, sdTree(vec3(6.0, -1.0, -10.0), point, d));
  d = opUnion(d, sdTree(vec3(4.0, -1.0, -10.0), point, d));

  return d;
}

float sceneInstituteForever(vec3 p) {
  p *= 5.0;
  float s = 30.0;
  vec2 id = round(p.xz / s);
  vec2 o = sign(p.xz - s * id);
  vec3 r = p - vec3(s * id, 0.0).xzy;
  return sceneInstitute(r) / 5.0;
}

float sceneSimple(vec3 point) {
  float d = 1e20;

  d = opUnion(d, sdSphere(4.0, point - vec3(6.0, 0.0, -3.0)));
  d = opUnion(d, sdSphere(2.0, point - vec3(0.0, -1.5, +3.0)));
  d = opUnion(d, sdBox(vec3(5.0, 3.0, 2.0), point + vec3(1.0, 3.0, 7.0)));

  // Triangle
  {
    float tr = sdTriangle(vec2(-10.0, 5.0), vec2(-5.0, 5.0), vec2(-10.0, -5.0),
                          point.xz);
    d = opUnion(d, opExtrusion(4.0, point, abs(tr) - 1.0));
  }

  // Cool S
  {
    vec3 p = point - vec3(3.0, 5.0, 7.0);
    d = opUnion(d, opExtrusion(0.4, p, sdCoolS(p.xz / 5.0) * 5.0) - 0.1);
  }

  return d;
}

float sceneLavaLamp(vec3 point) {
  float d = 1e20;

  for (int x = -3; x <= 3; x += 3) {
    for (int y = -3; y <= 3; y += 3) {
      vec3 origin = vec3(x, 0, y);
      float r1 = mix(0.2, 0.8, hash(origin + vec3(0, 1, 0)));
      float r2 = mix(0.2, 0.8, hash(origin + vec3(0, 2, 0)));

      float blob = sdSphere(1.5 + 1.0 * sin(time * r1),
                            point - vec3(x, sin(time * r2) * 10.0, y));
      d = opSmoothUnion(4.0, d, blob);
    }
  }

  return d;
}

// https://iquilezles.org/articles/menger/
float sceneSponge(vec3 point) {
  vec3 p = point / 9.0;
  float d = sdBox(vec3(1.0), p);

  float s = 1.0;
  for (int m = 0; m < 6; m++) {
    vec3 a = mod(p * s, 2.0) - 1.0;
    s *= 3.0;
    vec3 r = 1.0 - 3.0 * abs(a);
    float c = sdCross(r) / s;
    d = max(d, c);
  }

  return d * 9;
}

// Map

float map(vec3 p) {
  evaluations += 1;
  if (scene == 0)
    return sceneSimple(p);
  if (scene == 1)
    return sceneLavaLamp(p);
  if (scene == 2)
    return sceneSponge(p);
  if (scene == 3)
    return sceneInstitute(p);
  if (scene == 4)
    return sceneInstituteForever(p);
}

// https://iquilezles.org/articles/nvscene2008/rwwtt.pdf
float raymarch(vec3 origin, vec3 ray) {
  float t = 0;
  float e = 0.0001;

  for (int i = 0; i < 100 && t < 100.0; i++) {
    float h = map(origin + t * ray);
    if (h < e)
      return t;
    t += h;
    e *= 1.1;
  }

  return t;
}

// https://iquilezles.org/articles/normalsSDF/
vec3 calcNormal(vec3 point) {
  const float h = 0.00001;
  const vec2 k = vec2(+1, -1);

  return normalize(
      k.xyy * map(point + k.xyy * h) + k.yyx * map(point + k.yyx * h) +
      k.yxy * map(point + k.yxy * h) + k.xxx * map(point + k.xxx * h));
}

// https://iquilezles.org/articles/rmshadows/
float softShadow(vec3 point, vec3 light, float tmin, float tmax, float k) {
  float res = 1.0;
  float t = tmin;
  float ph = 1e10;

  for (int i = 0; i < 32; i++) {
    float h = map(point + light * t);
    float y = i == 0 ? 0.0 : h * h / (2.0 * ph);
    // float y = h * h / (2.0 * ph);
    float d = sqrt(h * h - y * y);
    res = min(res, d / (k * max(0.0, t - y)));
    ph = h;
    t += h;
    if (res < 0.0001 || t > tmax)
      break;
  }

  res = clamp(res, 0.0, 1.0);
  return res * res * (3.0 - 2.0 * res);
}

// https://iquilezles.org/articles/nvscene2008/rwwtt.pdf
float ambientOcclusion(vec3 point, vec3 normal, float d, float k) {
  float res = 0.0;

  for (int i = 0; i < 5; i++) {
    res += (d * i - map(point + d * i * normal)) / pow(2, i);
  }

  return 1.0 - k * res;
}

void render(out vec3 color, out float depth, out vec3 position, out vec3 normal,
            out float diffuse, out float shadow, out float occlusion) {
  vec3 sun = normalize(vec3(sin(time / 10.0), 1.0, -cos(time / 10.0)));

  depth = raymarch(origin, normalize(ray));

  // Sky
  if (depth > 100.0) {
    color = mix(vec3(0.1, 0.5, 0.8), vec3(0.8, 0.9, 1.0), (screen.y + 1) / 2);
    diffuse = shadow = occlusion = 0.0;
    return;
  }

  position = origin + normalize(ray) * depth;
  normal = calcNormal(position);

  // Base color - gray checkerboard
  {
    ivec3 tile = ivec3(floor(position));
    float h = (tile.x + tile.y + tile.z) % 2;
    float d = mix(0.5, h, 1 / (1 + 0.2 * depth));
    color = mix(vec3(0.0), vec3(1.0), d);
  }

  // Sun & shadow
  {
    diffuse = clamp(dot(normal, sun), 0.0, 1.0);
    shadow = softShadow(position, sun, 0.01, 50.0, 0.1);
    occlusion = ambientOcclusion(position, normal, 0.5, 0.5);
    color *= 0.1 + 0.9 * (diffuse * shadow * 0.8 + 0.2) * occlusion;
  }
}

void main() {
  vec3 color, position, normal;
  float depth, diffuse, shadow, occlusion;

  render(color, depth, position, normal, diffuse, shadow, occlusion);

  // Vertical intersection through scene
  vec3 slice;
  {
    float d = map(vec3(screen.xy * 10.0, sin(time / 10.0) * 10.0));
    slice = (d > 0.0) ? vec3(0.9, 0.6, 0.3) : vec3(0.65, 0.85, 1.0);
    slice *= 1.0 - exp(-6.0 * abs(d));
    slice *= 0.8 + 0.2 * cos(31.416 * d);
    slice = mix(slice, vec3(1.0), 1.0 - smoothstep(0.0, 0.035, abs(d)));
  }

  result.a = 1.0;
  // clang-format off
  result.rgb = vec3[](
    pow(color, vec3(0.4545)),
    slice,
    vec3(1.0 - (depth / 50.0)),
    (position + 50.0) / 100.0,
    (normal + 1.0) / 2.0,
    vec3(diffuse),
    vec3(shadow),
    vec3(occlusion),
    mix(vec3(0.3, 0.8, 0.4), vec3(1.0, 0.4, 0.4), (evaluations - 10) / 100.0)
  )[display];
  // clang-format on
}
