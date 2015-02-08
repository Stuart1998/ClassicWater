
uniform mat4 WorldTransform;
uniform mat4 ViewProjTransform;

attribute vec3 a_Position;
attribute vec4 a_Color;
attribute vec3 a_Normal;

varying vec4 v_Color;
varying vec3 v_Position;
varying vec3 v_Normal;
varying vec2 v_TexCoord0;
varying vec2 v_TexCoord1;
varying vec2 v_TexCoord2;
varying vec3 v_TexCoordBlend;

void main() {
    v_Position = (WorldTransform * vec4(a_Position,1)).xyz;
    gl_Position = ViewProjTransform * vec4(v_Position, 1);
    v_Color = a_Color;
    v_Normal = (WorldTransform * vec4(a_Normal,0)).xyz;

    v_TexCoord0 = vec2(a_Position.z, -a_Position.y);
    v_TexCoord1 = vec2(a_Position.x, -a_Position.z);
    v_TexCoord2 = vec2(a_Position.x, -a_Position.y);

    vec3 normal = normalize(a_Normal);

    vec3 flip = sign(normal);
    v_TexCoord0.x *= flip.x;
    v_TexCoord1.x *= flip.y;
    v_TexCoord2.x *= -flip.z;

    // determine major axis
    vec3 absNormal = abs(normal);
    // Z is major axis
    if (absNormal.z > absNormal.x && absNormal.z > absNormal.y)
    {
        vec2 st = vec2(absNormal.x, absNormal.y) / absNormal.z;
        v_TexCoordBlend = clamp(vec3(
            st.s - 0.5,
            st.t - 0.5,
            1.5 - max(st.s, st.t)
            ), 0.0, 1.0);
    }
    // Y is major axis
    else if (absNormal.y > absNormal.x) // && absNormal.y > absNormal.z
    {
        vec2 st = vec2(absNormal.x, absNormal.z) / absNormal.y;
        v_TexCoordBlend = clamp(vec3(
            st.s - 0.5,
            1.5 - max(st.s, st.t),
            st.t - 0.5
            ), 0.0, 1.0);
    }
    // X is major axis
    else // absNormal.x > absNormal.y && absNormal.x > absNormal.z
    {
        vec2 st = vec2(absNormal.z, absNormal.y) / absNormal.x;
        v_TexCoordBlend = clamp(vec3(
            1.5 - max(st.s, st.t),
            st.t - 0.5,
            st.s - 0.5
            ), 0.0, 1.0);
    }

    // "renormalize" component sum == 1.0
    float component_total = (v_TexCoordBlend.x + v_TexCoordBlend.y + v_TexCoordBlend.z);
    v_TexCoordBlend /= component_total;
}