#version 120

uniform vec3 g_LightAmbientDir;
uniform vec3 g_LightAmbientCenter;
uniform sampler2D g_LightAmbientLookup;
uniform vec4 g_LightAmbientLookup_size;

uniform vec3 light_dir;
uniform vec3 light_diffuse;
uniform vec3 light_specular_color;

uniform vec4 light_fog_color;
uniform vec4 light_fog_density;
uniform vec4 light_fog_center_radius;

uniform sampler2D GBufferDepth;
uniform vec4 GBufferDepth_size;
uniform vec4 GBufferDepth_range;

uniform vec3 cameraPos;
uniform vec4 Time;

uniform sampler2D DiffuseTexture;
uniform sampler2D NormalTexture;
uniform sampler2D DepthColorTexture;
uniform sampler2D NoiseTexture;
varying vec3 v_Normal;
varying vec4 v_Color;
varying vec3 v_Position;

varying vec2 v_TexCoord0;
varying vec2 v_TexCoord1;
varying vec2 v_TexCoord2;
varying vec3 v_TexCoordBlend;

//#define callCalculateSpecular CalculateSpecularWhite
#define callCalculateSpecular CalculateSpecularNormal
vec3 CalculateSpecularNormal(vec3 reflectionVector, vec3 cameraVector, vec3 speccolor) {
   vec3 specular = pow( max(dot(reflectionVector, cameraVector),0.0), 60.0) * speccolor;
   return specular;
}
vec3 CalculateSpecularWhite(vec3 reflectionVector, vec3 cameraVector, vec3 speccolor) {
   return vec3(1,1,1);
}

vec3 calcAmbient(vec3 normal, vec3 pos)
{
    vec3 pos_norm = normalize(g_LightAmbientCenter - pos);
    float ndota = dot(g_LightAmbientDir, pos_norm) * 0.5 + 0.5;
    float ndotc = dot(normal, pos_norm) * 0.5 + 0.5;
    vec2 lookup = vec2(ndota, ndotc) * (1.0 - g_LightAmbientLookup_size.zw) + g_LightAmbientLookup_size.zw * 0.5;
    return pow(max(texture2D(g_LightAmbientLookup, lookup).rgb, 0.0), vec3(2.2, 2.2, 2.2));
}

vec4 fogAt(vec3 forward, float solidDistance, out bool miss)
{
    miss = false;

    vec3 center = light_fog_center_radius.xyz;
    float radius = light_fog_center_radius.w;
    float radius2 = radius * radius;
    float density = light_fog_density.x;

    // Sphere intersection
    vec3 offset = center - cameraPos;
    float ofs_proj = dot(forward, offset);
    if (ofs_proj < 0.0)
    {
        // Ray pointing away from sphere
        miss = true;
        return vec4(0.0, 0.0, 0.0, 0.0);
    }

    float d2 = dot(offset, offset) - ofs_proj * ofs_proj;
    if (d2 > radius2)
    {
        // Ray misses the sphere
        miss = true;
        return vec4(0.0, 0.0, 0.0, 0.0);
    }
    float proj_distance = sqrt(radius2 - d2);
    float t0 = ofs_proj - proj_distance;
    if (solidDistance < t0)
    {
        // Solid geometry is outside the sphere along this ray
        miss = true;
        return vec4(0.0, 0.0, 0.0, 0.0);
    }
    float t1 = ofs_proj + proj_distance;

    float thickness = min(t1, solidDistance) - t0;

    float alpha = pow(clamp(thickness / density, 0.0, 1.0), 2.0);

    return vec4(light_fog_color.xyz, light_fog_color.w * alpha);
}

float smoothstep(float edge0, float edge1, float x)
{
    // Scale, bias and saturate x to 0..1 range
    x = clamp((x - edge0)/(edge1 - edge0), 0.0, 1.0);
    // Evaluate polynomial
    return x*x*(3.0 - 2.0*x);
}

float calcSpecular(vec3 camera_dir, vec3 normal, float ndotl, float ndotv, vec3 light_dir, float spec_pow, float spec)
{
    float norm_term = ( spec_pow + 2.0 ) / 8.0;
    vec3 half_dir = normalize( -light_dir + camera_dir );

    float ndoth = clamp( dot( normal, half_dir ), 0.0, 1.0 );
    float blinn_phong = pow( ndoth, spec_pow );
    float spec_term = norm_term * blinn_phong;

    float base = 1.0 - dot( half_dir, -light_dir );
    float exponent = pow( base, 5.0 );
    float fresnel_term = spec + ( 1.0 - spec ) * exponent;

    float alpha = 1.0 / ( sqrt( 0.785375 * spec + 1.57075 ) );
    float vis_term = ( ndotl * ( 1.0 - alpha ) + alpha ) * ( ndotv * ( 1.0 - alpha ) + alpha );
    vis_term = 1.0 / vis_term;

    return spec_term * ndotl * fresnel_term * vis_term;
    // vec3 specular = spec_term * ndotl * fresnel_term * vis_term * light_diffuse;
}

void main() {
    // gl_FragColor = vec4(v_TexCoordBlend, 1.0);
    // return;

    vec4 texel = vec4(52.0/255.0, 105.0/255.0, 150.0/255.0, 0.2);
    texel.rgb = pow(texel.rgb, vec3(2.2, 2.2, 2.2));
    vec4 waveColor = vec4(0.45, 0.45, 0.55, 0.5);

    vec2 screenCoord = gl_FragCoord.xy * GBufferDepth_size.zw;

    // Figure out the world position
    float gdepth = texture2D(GBufferDepth, screenCoord).x * GBufferDepth_range.y + GBufferDepth_range.x;

    vec3 forward = v_Position - cameraPos;
    float depth = length(forward);
    float depth_opacity = gdepth - depth;

    float flowTexScale = 800.0;

    vec3 flow = vec3(0.0, 0.0, 0.0);
    if( v_TexCoordBlend.x > 0.0 )
    {
        vec3 flowA = texture2D(NoiseTexture, v_TexCoord0 / flowTexScale).rgb;
        flow += flowA * v_TexCoordBlend.x;
    }
    if( v_TexCoordBlend.y > 0.0 )
    {
        vec3 flowB = texture2D(NoiseTexture, v_TexCoord1 / flowTexScale + 0.33).rgb;
        flow += flowB * v_TexCoordBlend.y;
    }
    if( v_TexCoordBlend.z > 0.0 )
    {
        vec3 flowC = texture2D(NoiseTexture, v_TexCoord2 / flowTexScale + 0.67).rgb;
        flow += flowC * v_TexCoordBlend.z;
    }

    flow.xy = flow.xy * 2.0 - 1.0;
    float flowStrength = smoothstep(0.0, 1.0, sqrt(flow.x * flow.x + flow.y * flow.y));

    float flowSpeed = 0.05;
    float flowDistortionScale = 0.125;
    float flowOffsetTime = Time.x * flowSpeed + flow.z;
    float flowOffsetA = (fract(flowOffsetTime) * 2.0 - 1.0) * flowDistortionScale;
    float flowOffsetB = (fract(flowOffsetTime + 0.5) * 2.0 - 1.0) * flowDistortionScale;
    float flowAlpha = smoothstep(0.0, 1.0, abs(fract(flowOffsetTime) - 0.5) * 2.0);

    float texScale = 120.0;
    vec3 scaledPos = v_Position / texScale;
    vec4 diffuseA = vec4(0.0, 0.0, 0.0, 0.0);
    vec4 diffuseB = vec4(0.0, 0.0, 0.0, 0.0);
    vec3 normal_combine = vec3(0.0, 0.0, 0.0);

    if( v_TexCoordBlend.x > 0.0 )
    {
        vec2 uvXA = v_TexCoord0 / texScale + flow.xy * flowOffsetA;
        vec2 uvXB = v_TexCoord0 / texScale + flow.xy * flowOffsetB;

        mat3 tbn_X;
        tbn_X[0] = normalize(vec3(-v_Normal.z, 0.0, -v_Normal.x));
        tbn_X[1] = cross(tbn_X[0], v_Normal);
        tbn_X[2] = v_Normal;

        vec3 normalXA = 2.0 * texture2D(NormalTexture, uvXA).rgb - 1.0;
        vec3 normalXB = 2.0 * texture2D(NormalTexture, uvXB).rgb - 1.0;
        vec3 normal_X = tbn_X * mix(normalXA, normalXB, flowAlpha);

        normal_combine += normal_X * v_TexCoordBlend.x;

        vec4 diffuseXA = texture2D(DiffuseTexture, uvXA);
        vec4 diffuseXB = texture2D(DiffuseTexture, uvXB);

        diffuseA += diffuseXA * v_TexCoordBlend.x;
        diffuseB += diffuseXB * v_TexCoordBlend.x;
    }
    if( v_TexCoordBlend.y > 0.0 )
    {
        vec2 uvYA = v_TexCoord1 / texScale + flow.xy * flowOffsetA + 0.33;
        vec2 uvYB = v_TexCoord1 / texScale + flow.xy * flowOffsetB + 0.33;

        mat3 tbn_Y;
        tbn_Y[0] = normalize(vec3(-v_Normal.y, 0.0, v_Normal.z));
        tbn_Y[1] = cross(tbn_Y[0], v_Normal);
        tbn_Y[2] = v_Normal;

        vec3 normalYA = 2.0 * texture2D(NormalTexture, uvYA).rgb - 1.0;
        vec3 normalYB = 2.0 * texture2D(NormalTexture, uvYB).rgb - 1.0;
        vec3 normal_Y = tbn_Y * mix(normalYA, normalYB, flowAlpha);

        normal_combine += normal_Y * v_TexCoordBlend.y;

        vec4 diffuseYA = texture2D(DiffuseTexture, uvYA);
        vec4 diffuseYB = texture2D(DiffuseTexture, uvYB);

        diffuseA += diffuseYA * v_TexCoordBlend.y;
        diffuseB += diffuseYB * v_TexCoordBlend.y;
    }
    if( v_TexCoordBlend.z > 0.0 )
    {
        vec2 uvZA = v_TexCoord2 / texScale + flow.xy * flowOffsetA + 0.67;
        vec2 uvZB = v_TexCoord2 / texScale + flow.xy * flowOffsetB + 0.67;

        mat3 tbn_Z;
        tbn_Z[0] = normalize(vec3(v_Normal.z, -v_Normal.x, 0.0));
        tbn_Z[1] = cross(tbn_Z[0], v_Normal);
        tbn_Z[2] = v_Normal;

        vec3 normalZA = 2.0 * texture2D(NormalTexture, uvZA).rgb - 1.0;
        vec3 normalZB = 2.0 * texture2D(NormalTexture, uvZB).rgb - 1.0;
        vec3 normal_Z = tbn_Z * mix(normalZA, normalZB, flowAlpha);

        normal_combine += normal_Z * v_TexCoordBlend.z;

        vec4 diffuseZA = texture2D(DiffuseTexture, uvZA);
        vec4 diffuseZB = texture2D(DiffuseTexture, uvZB);

        diffuseA += diffuseZA * v_TexCoordBlend.z;
        diffuseB += diffuseZB * v_TexCoordBlend.z;
    }

    vec3 normal = normalize(
            mix(
                v_Normal,
                normal_combine,
                0.5 * (0.25 + flowStrength * 0.75) * clamp(depth_opacity / 8.0, 0.0, 1.0)
            )
        );

    vec4 diffuseNoise = mix(
            diffuseA,
            diffuseB,
            flowAlpha
        );
    vec4 diffuseColor = mix(vec4(texel.rgb, 0.0), diffuseNoise, 0.25 + flowStrength * 0.75);

    float ndotl = clamp( dot(-light_dir, normal), 0.0, 1.0 );

    vec3 camera_dir = normalize( cameraPos - v_Position );
    float ndotv = clamp( dot( camera_dir, normal ), 0.0, 1.0 );

    // hacky water stuff
    float F0 = texel.a;
    float fresnel = F0 + (1.0 - F0) * pow(1.0 - ndotv, 5.0);

    vec3 ambientIllum = calcAmbient(normal, v_Position);

    vec3 specular = vec3(0.0, 0.0, 0.0);
    float spec_pow = 100.0;
    float spec = 0.01;

    if (ndotl > 0.0)
    {
        specular = calcSpecular(camera_dir, normal, ndotl, ndotv, light_dir, spec_pow, spec) * light_diffuse;
    }
    else
    {
        float ambient_ndotl = clamp( dot(light_dir, normal), 0.0, 1.0 );
        specular = calcSpecular(camera_dir, normal, ambient_ndotl, ndotv, -light_dir, spec_pow * 0.25, spec) * ambientIllum;
    }

    vec3 illum = ambientIllum + ndotl * light_diffuse;

    //depth_opacity -= (flowAlpha - diffuseNoise.a * 0.5 + 0.25);
    // float unmod_depth_opacity = depth_opacity;
    float shore_depth = v_Color.r * 8.0 - flowAlpha;
    depth_opacity -= flowAlpha;

    // fog
    bool miss;
    vec4 fog = fogAt(forward / depth, depth, miss);

    vec2 depth_color_uv = vec2(clamp(depth_opacity / 32.0, 0.008, 0.9), 0.0);
    vec4 depth_tex = texture2D(DepthColorTexture, depth_color_uv);

    float depth_alpha = depth_tex.a; //1.0 - pow(1.0 - clamp(depth_opacity / 16.0, 0.0, 1.0), 2.0);
    float shore_alpha = clamp(depth_opacity * 2.0 - (1.0 - diffuseNoise.a) + 0.5, 0.0, 1.0);
    float wave_alpha = clamp(diffuseNoise.a - depth_opacity + 1.0, 0.0, 1.0);

    vec4 depth_color = vec4(depth_tex.rgb, mix(depth_alpha, waveColor.a, wave_alpha));

    vec3 color = mix(
        mix(
            depth_color.rgb,
            diffuseColor.rgb,
            clamp((diffuseNoise.a - (1.0 - flowAlpha * flowAlpha) * 0.25), 0.0, 1.0)
            ),
        waveColor.rgb,
        wave_alpha
        );

    color *= illum;
    color += specular;
    float ambientDirDot = dot(reflect(-camera_dir, normal), normalize(g_LightAmbientCenter - v_Position)) * 0.5 + 0.5;
    color = mix( color, fog.rgb * ambientDirDot, fresnel );
    color = mix( color, fog.rgb, fog.a );

    float alpha = mix(max(depth_color.a, fog.a), 1.0, fresnel) * shore_alpha;

    gl_FragColor = vec4(color, alpha);
}