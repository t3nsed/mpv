// Copyright (c) 2015-2016, bacondither
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer
//    in this position and unchanged.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
// OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
// IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
// NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
// THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

// Adaptive sharpen - version 2016-07-10 - (requires ps >= 3.0)
// Tuned for use post resize

//!HOOK SCALED
//!BIND HOOKED
//!COMPONENTS 4

//#define Gamma(x)    ( mix(x * vec3(12.92), vec3(1.055) * pow(max(x, 0.0), vec3(1.0/2.4)) - vec3(0.055), step(vec3(0.0031308), x)) )
#define Gamma(x) ( pow(max(x, 0.0), vec3(1.0/2.0)) )

vec4 hook() {
	vec4 c = HOOKED_tex(HOOKED_pos);
	c.rgb = Gamma(c.rgb);
	return c;
}

//!HOOK SCALED
//!BIND HOOKED

//---------------------------------------------------------------------------------
#define w_offset 2.0         // Edge channel offset, must be the same in all passes
//---------------------------------------------------------------------------------

// Get destination pixel values
#define get(x, y) ( clamp(HOOKED_texOff(vec2(x, y)*0.6).rgb, 0.0, 1.0) )

// Compute diff
#define b_diff(pix) ( abs(blur-c[pix]) )

vec4 hook() {

    // Get points and clip out of range values (BTB & WTW)
    // [                c9                ]
    // [           c1,  c2,  c3           ]
    // [      c10, c4,  c0,  c5, c11      ]
    // [           c6,  c7,  c8           ]
    // [                c12               ]
    vec3 c[13] = vec3[](get( 0, 0), get(-1,-1), get( 0,-1), get( 1,-1), get(-1, 0),
                        get( 1, 0), get(-1, 1), get( 0, 1), get( 1, 1), get( 0,-2),
                        get(-2, 0), get( 2, 0), get( 0, 2));

    // Blur, gauss 3x3
    vec3  blur   = (vec3(2)*(c[2]+c[4]+c[5]+c[7]) + (c[1]+c[3]+c[6]+c[8]) + vec3(4)*c[0])/vec3(16);
    float blur_Y = dot(blur, vec3(1.0/3.0));

    // Contrast compression, center = 0.5, scaled to 1/3
    float c_comp = clamp(0.266666681f + 0.9*exp2(-7.4*blur_Y), 0.0, 1.0);

    // Edge detection
    // Matrix weights
    // [          1,         ]
    // [      4,  5,  4      ]
    // [  1,  5,  6,  5,  1  ]
    // [      4,  5,  4      ]
    // [          1          ]
    float edge = length( 1.38*b_diff(0)
                       + 1.15*(b_diff(2) + b_diff(4) + b_diff(5) + b_diff(7))
                       + 0.92*(b_diff(1) + b_diff(3) + b_diff(6) + b_diff(8))
                       + 0.23*(b_diff(9) + b_diff(10) + b_diff(11) + b_diff(12)) );

    return vec4( (HOOKED_tex(HOOKED_pos).rgb), (edge*c_comp + w_offset) );
}

//!HOOK SCALED
//!BIND HOOKED

//--------------------------------------- Settings ------------------------------------------------

#define curve_height    1.0                  // Main control of sharpening strength, [>0]
                                             // 0.3 <-> 2.0 is a reasonable range of values

#define video_level_out false                // True to preserve BTB & WTW (minor summation error)
                                             // Normally it should be set to false

//-------------------------------------------------------------------------------------------------
// Defined values under this row are "optimal" DO NOT CHANGE IF YOU DO NOT KNOW WHAT YOU ARE DOING!

#define curveslope      0.5                  // Sharpening curve slope, high edge values

#define L_overshoot     0.003                // Max light overshoot before max compression [>0.001]
#define L_compr_low     0.169                // Light compression, default (0.169=~9x)
#define L_compr_high    0.337                // Light compression, surrounded by edges (0.337=~4x)

#define D_overshoot     0.009                // Max dark overshoot before max compression [>0.001]
#define D_compr_low     0.253                // Dark compression, default (0.253=~6x)
#define D_compr_high    0.504                // Dark compression, surrounded by edges (0.504=~2.5x)

#define max_scale_lim   0.1                  // Abs max change before compression (0.1=+-10%)

#define dW_lothr        0.3                  // Start interpolating between W1 and W2
#define dW_hithr        0.8                  // When dW is equal to W2

#define lowthr_mxw      0.12                 // Edge value for max lowthr weight [>0.01]

#define pm_p            0.75                 // Power mean p-value [>0 - 1.0] (0.5=sqrt)

#define alpha_out       1.0                  // MPDN requires the alpha channel output to be 1

//-------------------------------------------------------------------------------------------------
#define w_offset        2.0                  // Edge channel offset, must be the same in all passes
//-------------------------------------------------------------------------------------------------

// Saturation loss reduction
#define minim_satloss  ( (c[0].rgb*max((c0_Y + sharpdiff)/c0_Y, 0.0) + (c[0].rgb + sharpdiff))/2.0 )

// Soft if, fast
#define soft_if(a,b,c) ( saturate((a + b + c - 3.0*w_offset)/(saturate(maxedge) + 0.0067) - 0.85) )

// Soft limit, modified tanh
#define soft_lim(v,s)  ( ((exp(2.0*min(abs(v), s*16.0)/s) - 1.0)/(exp(2.0*min(abs(v), s*16.0)/s) + 1.0))*s )

// Weighted power mean
#define wpmean(a,b,c)  ( pow((c*pow(abs(a), pm_p) + (1.0-c)*pow(b, pm_p)), (1.0/pm_p)) )

// Get destination pixel values
#define get(x,y)       ( HOOKED_texOff(vec2(x, y)*0.6) )
#define sat(input)     ( vec4(saturate((input).xyz), (input).w) )

// Maximum of four values
#define max4(a,b,c,d)  ( max(max(a,b), max(c,d)) )

// Colour to luma, fast approx gamma, avg of rec. 709 & 601 luma coeffs
#define CtL(RGB)       ( dot(vec3(0.2558, 0.6511, 0.0931), GammaInv(RGB.rgb)) )

// Center pixel diff
#define mdiff(a,b,c,d,e,f,g) ( abs(luma[g]-luma[a]) + abs(luma[g]-luma[b])           \
                             + abs(luma[g]-luma[c]) + abs(luma[g]-luma[d])           \
                             + 0.5*(abs(luma[g]-luma[e]) + abs(luma[g]-luma[f])) )

#define saturate(x) clamp((x), 0.0, 1.0)

//#define GammaInv(x) ( mix(pow((x + vec3(0.055))/vec3(1.055), vec3(2.4)), x / vec3(12.92), step(x, vec3(0.04045))) )
#define GammaInv(x) ( pow((x), vec3(2.0)) )

vec4 hook() {

    vec4 orig = HOOKED_tex(HOOKED_pos);
    float c_edge = orig.w - w_offset;

    // Displays a green screen if the edge data is not inside a valid range in the .w channel
    //if (c_edge > 24.0 || c_edge < -0.5) { return vec4(0, 1, 0, alpha_out); }

    // Get points, clip out of range colour data in c[0]
    // [                c22               ]
    // [           c24, c9,  c23          ]
    // [      c21, c1,  c2,  c3, c18      ]
    // [ c19, c10, c4,  c0,  c5, c11, c16 ]
    // [      c20, c6,  c7,  c8, c17      ]
    // [           c15, c12, c14          ]
    // [                c13               ]
    vec4 c[25] = vec4[](sat( orig), get(-1,-1), get( 0,-1), get( 1,-1), get(-1, 0),
                        get( 1, 0), get(-1, 1), get( 0, 1), get( 1, 1), get( 0,-2),
                        get(-2, 0), get( 2, 0), get( 0, 2), get( 0, 3), get( 1, 2),
                        get(-1, 2), get( 3, 0), get( 2, 1), get( 2,-1), get(-3, 0),
                        get(-2, 1), get(-2,-1), get( 0,-3), get( 1,-2), get(-1,-2));

    // Allow for higher overshoot if the current edge pixel is surrounded by similar edge pixels
    float maxedge = max4( max4(c[1].w,c[2].w,c[3].w,c[4].w), max4(c[5].w,c[6].w,c[7].w,c[8].w),
                          max4(c[9].w,c[10].w,c[11].w,c[12].w), c[0].w ) - w_offset;

    // [          x          ]
    // [       z, x, w       ]
    // [    z, z, x, w, w    ]
    // [ y, y, y, 0, y, y, y ]
    // [    w, w, x, z, z    ]
    // [       w, x, z       ]
    // [          x          ]
    float sbe = soft_if(c[2].w,c[9].w,c[22].w) *soft_if(c[7].w,c[12].w,c[13].w)  // x dir
              + soft_if(c[4].w,c[10].w,c[19].w)*soft_if(c[5].w,c[11].w,c[16].w)  // y dir
              + soft_if(c[1].w,c[24].w,c[21].w)*soft_if(c[8].w,c[14].w,c[17].w)  // z dir
              + soft_if(c[3].w,c[23].w,c[18].w)*soft_if(c[6].w,c[20].w,c[15].w); // w dir

    float s[2] = float[]( mix( L_compr_low, L_compr_high, saturate(smoothstep(2.0, 3.1, sbe)) ),
                          mix( D_compr_low, D_compr_high, saturate(smoothstep(2.0, 3.1, sbe)) ) );

    // RGB to luma
    float c0_Y = CtL(c[0]);

    float luma[25] = float[](c0_Y, CtL(c[1]), CtL(c[2]), CtL(c[3]), CtL(c[4]), CtL(c[5]), CtL(c[6]),
                             CtL(c[7]),  CtL(c[8]),  CtL(c[9]),  CtL(c[10]), CtL(c[11]), CtL(c[12]),
                             CtL(c[13]), CtL(c[14]), CtL(c[15]), CtL(c[16]), CtL(c[17]), CtL(c[18]),
                             CtL(c[19]), CtL(c[20]), CtL(c[21]), CtL(c[22]), CtL(c[23]), CtL(c[24]));

    // Precalculated default squared kernel weights
    const vec3 w1 = vec3(0.5,           1.0, 1.41421356237); // 0.25, 1.0, 2.0
    const vec3 w2 = vec3(0.86602540378, 1.0, 0.5477225575);  // 0.75, 1.0, 0.3

    // Transition to a concave kernel if the center edge val is above thr
    vec3 dW = pow(mix( w1, w2, smoothstep(dW_lothr, dW_hithr, c_edge)), vec3(2.0));

    float mdiff_c0  = 0.02 + 3.0*( abs(luma[0]-luma[2]) + abs(luma[0]-luma[4])
                                 + abs(luma[0]-luma[5]) + abs(luma[0]-luma[7])
                                 + 0.25*(abs(luma[0]-luma[1]) + abs(luma[0]-luma[3])
                                        +abs(luma[0]-luma[6]) + abs(luma[0]-luma[8])) );

    // Use lower weights for pixels in a more active area relative to center pixel area.
    float weights[12]  = float[](( min((mdiff_c0/mdiff(24, 21, 2,  4,  9,  10, 1)),  dW.y) ),
                                 ( dW.x ),
                                 ( min((mdiff_c0/mdiff(23, 18, 5,  2,  9,  11, 3)),  dW.y) ),
                                 ( dW.x ),
                                 ( dW.x ),
                                 ( min((mdiff_c0/mdiff(4,  20, 15, 7,  10, 12, 6)),  dW.y) ),
                                 ( dW.x ),
                                 ( min((mdiff_c0/mdiff(5,  7,  17, 14, 12, 11, 8)),  dW.y) ),
                                 ( min((mdiff_c0/mdiff(2,  24, 23, 22, 1,  3,  9)),  dW.z) ),
                                 ( min((mdiff_c0/mdiff(20, 19, 21, 4,  1,  6,  10)), dW.z) ),
                                 ( min((mdiff_c0/mdiff(17, 5,  18, 16, 3,  8,  11)), dW.z) ),
                                 ( min((mdiff_c0/mdiff(13, 15, 7,  14, 6,  8,  12)), dW.z) ));

    weights[0] = (max(max((weights[8]  + weights[9])/4.0,  weights[0]), 0.25) + weights[0])/2.0;
    weights[2] = (max(max((weights[8]  + weights[10])/4.0, weights[2]), 0.25) + weights[2])/2.0;
    weights[5] = (max(max((weights[9]  + weights[11])/4.0, weights[5]), 0.25) + weights[5])/2.0;
    weights[7] = (max(max((weights[10] + weights[11])/4.0, weights[7]), 0.25) + weights[7])/2.0;

    // Calculate the negative part of the laplace kernel	
    float lowthrsum   = 0.0;
    float weightsum   = 0.0;
    float neg_laplace = 0.0;

    for (int pix = 0; pix < 12; ++pix)
    {
        float x       = saturate((c[pix+1].w - w_offset - 0.01)/(lowthr_mxw - 0.01));
        float lowthr  = x*x*(2.97 - 1.98*x) + 0.01;

        neg_laplace  += luma[pix+1]*(weights[pix]*lowthr);
        weightsum    += weights[pix]*lowthr;
        lowthrsum    += lowthr/12.0;
    }

    neg_laplace = abs(neg_laplace/weightsum);

    // Compute sharpening magnitude function
    float sharpen_val = (curve_height/(curve_height*curveslope*pow(abs(c_edge), 3.5) + 0.5));

    // Calculate sharpening diff and scale
    float sharpdiff = (c0_Y - neg_laplace)*(lowthrsum*sharpen_val*0.8 + 0.01);

    // Calculate local near min & max, partial sort
    float temp;
    
    for (int i1 = 0; i1 < 24; i1 += 2)
    {
        temp = luma[i1];
        luma[i1]   = min(luma[i1], luma[i1+1]);
        luma[i1+1] = max(temp, luma[i1+1]);
    }
    
    for (int i2 = 24; i2 > 0; i2 -= 2)
    {
        temp = luma[0];
        luma[0]    = min(luma[0], luma[i2]);
        luma[i2]   = max(temp, luma[i2]);

        temp = luma[24];
        luma[24] = max(luma[24], luma[i2-1]);
        luma[i2-1] = min(temp, luma[i2-1]);
    }
    
    for (int i1 = 1; i1 < 24-1; i1 += 2)
    {
        temp = luma[i1];
        luma[i1]   = min(luma[i1], luma[i1+1]);
        luma[i1+1] = max(temp, luma[i1+1]);
    }
    
    for (int i2 = 24-1; i2 > 1; i2 -= 2)
    {
        temp = luma[1];
        luma[1]    = min(luma[1], luma[i2]);
        luma[i2]   = max(temp, luma[i2]);

        temp = luma[24-1];
        luma[24-1] = max(luma[24-1], luma[i2-1]);
        luma[i2-1] = min(temp, luma[i2-1]);
    }
    
    for (int i1 = 2; i1 < 24-2; i1 += 2)
    {
        temp = luma[i1];
        luma[i1]   = min(luma[i1], luma[i1+1]);
        luma[i1+1] = max(temp, luma[i1+1]);
    }
    
    for (int i2 = 24-2; i2 > 2; i2 -= 2)
    {
        temp = luma[2];
        luma[2]    = min(luma[2], luma[i2]);
        luma[i2]   = max(temp, luma[i2]);

        temp = luma[24-2];
        luma[24-2] = max(luma[24-2], luma[i2-1]);
        luma[i2-1] = min(temp, luma[i2-1]);
    }

    float nmax = (max(luma[22] + luma[23]*2.0, c0_Y*3.0) + luma[24])/4.0;
    float nmin = (min(luma[2]  + luma[1]*2.0,  c0_Y*3.0) + luma[0])/4.0;

    // Calculate tanh scale factor, pos/neg
    float nmax_scale = min((abs(nmax - c0_Y) + L_overshoot), max_scale_lim);
    float nmin_scale = min((abs(c0_Y - nmin) + D_overshoot), max_scale_lim);

    // Soft limited antiringing with tanh, wpmean to control maximum compression slope
    sharpdiff = wpmean(max(sharpdiff, 0.0), soft_lim(max(sharpdiff, 0.0), nmax_scale ), s[0] )
              - wpmean(min(sharpdiff, 0.0), soft_lim(min(sharpdiff, 0.0), nmin_scale ), s[1] );

    if (video_level_out){
        return vec4(GammaInv(mix(orig.rgb + (minim_satloss - c[0].rgb), (orig.rgb + sharpdiff), step(sharpdiff, 0.0))), alpha_out);
    }

    else // Normal path
    {
        return vec4(GammaInv(mix(minim_satloss, (c[0].rgb + sharpdiff), step(sharpdiff, 0.0))), alpha_out);
    }
}

