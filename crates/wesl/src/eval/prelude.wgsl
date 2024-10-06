alias vec2i = vec2<i32>;
alias vec3i = vec3<i32>;
alias vec4i = vec4<i32>;
alias vec2u = vec2<u32>;
alias vec3u = vec3<u32>;
alias vec4u = vec4<u32>;
alias vec2f = vec2<f32>;
alias vec3f = vec3<f32>;
alias vec4f = vec4<f32>;
// TODO: these are only enabled with the f16 extension
alias vec2h = vec2<f16>;
alias vec3h = vec3<f16>;
alias vec4h = vec4<f16>;
alias mat2x2f = mat2x2<f32>;
alias mat2x3f = mat2x3<f32>;
alias mat2x4f = mat2x4<f32>;
alias mat3x2f = mat3x2<f32>;
alias mat3x3f = mat3x3<f32>;
alias mat3x4f = mat3x4<f32>;
alias mat4x2f = mat4x2<f32>;
alias mat4x3f = mat4x3<f32>;
alias mat4x4f = mat4x4<f32>;
// TODO: these are only enabled with the f16 extension
alias mat2x2h = mat2x2<f16>;
alias mat2x3h = mat2x3<f16>;
alias mat2x4h = mat2x4<f16>;
alias mat3x2h = mat3x2<f16>;
alias mat3x3h = mat3x3<f16>;
alias mat3x4h = mat3x4<f16>;
alias mat4x2h = mat4x2<f16>;
alias mat4x3h = mat4x3<f16>;
alias mat4x4h = mat4x4<f16>;

// @internal declarations become prefixed with __
@internal struct frexp_result_f32 { fract: f32, exp: i32 }
@internal struct frexp_result_f16 { fract: f16, exp: i32 }
@internal struct frexp_result_abstract { fract: AbstractFloat, exp: AbstractInt }
@internal struct frexp_result_vec2_f32 { fract: vec2<f32>, exp: vec2<i32> }
@internal struct frexp_result_vec3_f32 { fract: vec3<f32>, exp: vec3<i32> }
@internal struct frexp_result_vec4_f32 { fract: vec4<f32>, exp: vec4<i32> }
@internal struct frexp_result_vec2_f16 { fract: vec2<f16>, exp: vec2<i32> }
@internal struct frexp_result_vec3_f16 { fract: vec3<f16>, exp: vec3<i32> }
@internal struct frexp_result_vec4_f16 { fract: vec4<f16>, exp: vec4<i32> }
@internal struct frexp_result_vec2_abstract { fract: vec2<AbstractFloat>, exp: vec2<AbstractInt> }
@internal struct frexp_result_vec3_abstract { fract: vec3<AbstractFloat>, exp: vec3<AbstractInt> }
@internal struct frexp_result_vec4_abstract { fract: vec4<AbstractFloat>, exp: vec4<AbstractInt> }
@internal struct modf_result_f32 { fract: f32, whole: f32 }
@internal struct modf_result_f16 { fract: f16, whole: f16 }
@internal struct modf_result_abstract { fract: AbstractFloat, whole: AbstractFloat }
@internal struct modf_result_vec2_f32 { fract: vec2<f32>, whole: vec2<f32> }
@internal struct modf_result_vec3_f32 { fract: vec3<f32>, whole: vec3<f32> }
@internal struct modf_result_vec4_f32 { fract: vec4<f32>, whole: vec4<f32> }
@internal struct modf_result_vec2_f16 { fract: vec2<f16>, whole: vec2<f16> }
@internal struct modf_result_vec3_f16 { fract: vec3<f16>, whole: vec3<f16> }
@internal struct modf_result_vec4_f16 { fract: vec4<f16>, whole: vec4<f16> }
@internal struct modf_result_vec2_abstract { fract: vec2<AbstractFloat>, whole: vec2<AbstractFloat> }
@internal struct modf_result_vec3_abstract { fract: vec3<AbstractFloat>, whole: vec3<AbstractFloat> }
@internal struct modf_result_vec4_abstract { fract: vec4<AbstractFloat>, whole: vec4<AbstractFloat> }
@internal @generic(T) struct atomic_compare_exchange_result { old_value: T, exchanged: bool }

// constructors
@const fn array() @builtin {}
@const fn bool() @builtin {}
@const fn i32() @builtin {}
@const fn u32() @builtin {}
@const fn f32() @builtin {}
@const fn f16() @builtin {}
@const fn mat2x2() @builtin {}
@const fn mat2x3() @builtin {}
@const fn mat2x4() @builtin {}
@const fn mat3x2() @builtin {}
@const fn mat3x3() @builtin {}
@const fn mat3x4() @builtin {}
@const fn mat4x2() @builtin {}
@const fn mat4x3() @builtin {}
@const fn mat4x4() @builtin {}
@const fn vec2() @builtin {}
@const fn vec3() @builtin {}
@const fn vec4() @builtin {}

// bitcast
@const fn bitcast() @builtin {}

// logical
@const fn all() @builtin {}
@const fn any() @builtin {}
@const fn select() @builtin {}

// array
@const fn arrayLength() @builtin {}

// numeric
@const fn abs() @builtin {}
@const fn acos() @builtin {}
@const fn acosh() @builtin {}
@const fn asin() @builtin {}
@const fn asinh() @builtin {}
@const fn atan() @builtin {}
@const fn atanh() @builtin {}
@const fn atan2() @builtin {}
@const fn ceil() @builtin {}
@const fn clamp() @builtin {}
@const fn cos() @builtin {}
@const fn cosh() @builtin {}
@const fn countLeadingZeros() @builtin {}
@const fn countOneBits() @builtin {}
@const fn countTrailingZeros() @builtin {}
@const fn cross() @builtin {}
@const fn degrees() @builtin {}
@const fn determinant() @builtin {}
@const fn distance() @builtin {}
@const fn dot() @builtin {}
@const fn dot4U8Packed() @builtin {}
@const fn dot4I8Packed() @builtin {}
@const fn exp() @builtin {}
@const fn exp2() @builtin {}
@const fn extractBits() @builtin {}
@const fn faceForward() @builtin {}
@const fn firstLeadingBit() @builtin {}
@const fn firstTrailingBit() @builtin {}
@const fn floor() @builtin {}
@const fn fma() @builtin {}
@const fn fract() @builtin {}
@const fn frexp() @builtin {}
@const fn insertBits() @builtin {}
@const fn inverseSqrt() @builtin {}
@const fn ldexp() @builtin {}
@const fn length() @builtin {}
@const fn log() @builtin {}
@const fn log2() @builtin {}
@const fn max() @builtin {}
@const fn min() @builtin {}
@const fn mix() @builtin {}
@const fn modf() @builtin {}
@const fn normalize() @builtin {}
@const fn pow() @builtin {}
@const fn quantizeToF16() @builtin {}
@const fn radians() @builtin {}
@const fn reflect() @builtin {}
@const fn refract() @builtin {}
@const fn reverseBits() @builtin {}
@const fn round() @builtin {}
@const fn saturate() @builtin {}
@const fn sign() @builtin {}
@const fn sin() @builtin {}
@const fn sinh() @builtin {}
@const fn smoothstep() @builtin {}
@const fn sqrt() @builtin {}
@const fn step() @builtin {}
@const fn tan() @builtin {}
@const fn tanh() @builtin {}
@const fn transpose() @builtin {}
@const fn trunc() @builtin {}

// packing
@const fn pack4x8snorm() @builtin {}
@const fn pack4x8unorm() @builtin {}
@const fn pack4xI8() @builtin {}
@const fn pack4xU8() @builtin {}
@const fn pack4xI8Clamp() @builtin {}
@const fn pack4xU8Clamp() @builtin {}
@const fn pack2x16snorm() @builtin {}
@const fn pack2x16unorm() @builtin {}
@const fn pack2x16float() @builtin {}
@const fn unpack4x8snorm() @builtin {}
@const fn unpack4x8unorm() @builtin {}
@const fn unpack4xI8() @builtin {}
@const fn unpack4xU8() @builtin {}
@const fn unpack2x16snorm() @builtin {}
@const fn unpack2x16unorm() @builtin {}
@const fn unpack2x16float() @builtin {}
