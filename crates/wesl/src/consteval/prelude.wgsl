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
fn array() @builtin {}
fn bool() @builtin {}
fn i32() @builtin {}
fn u32() @builtin {}
fn f32() @builtin {}
fn f16() @builtin {}
fn mat2x2() @builtin {}
fn mat2x3() @builtin {}
fn mat2x4() @builtin {}
fn mat3x2() @builtin {}
fn mat3x3() @builtin {}
fn mat3x4() @builtin {}
fn mat4x2() @builtin {}
fn mat4x3() @builtin {}
fn mat4x4() @builtin {}
fn vec2() @builtin {}
fn vec3() @builtin {}
fn vec4() @builtin {}

// bitcast
fn bitcast() @builtin {}

// logical
fn all() @builtin {}
fn any() @builtin {}
fn select() @builtin {}

// array
fn arrayLength() @builtin {}

// numeric
fn abs() @builtin {}
fn acos() @builtin {}
fn acosh() @builtin {}
fn asin() @builtin {}
fn asinh() @builtin {}
fn atan() @builtin {}
fn atanh() @builtin {}
fn atan2() @builtin {}
fn ceil() @builtin {}
fn clamp() @builtin {}
fn cos() @builtin {}
fn cosh() @builtin {}
fn countLeadingZeros() @builtin {}
fn countOneBits() @builtin {}
fn countTrailingZeros() @builtin {}
fn cross() @builtin {}
fn degrees() @builtin {}
fn determinant() @builtin {}
fn distance() @builtin {}
fn dot() @builtin {}
fn dot4U8Packed() @builtin {}
fn dot4I8Packed() @builtin {}
fn exp() @builtin {}
fn exp2() @builtin {}
fn extractBits() @builtin {}
fn faceForward() @builtin {}
fn firstLeadingBit() @builtin {}
fn firstTrailingBit() @builtin {}
fn floor() @builtin {}
fn fma() @builtin {}
fn fract() @builtin {}
fn frexp() @builtin {}
fn insertBits() @builtin {}
fn inverseSqrt() @builtin {}
fn ldexp() @builtin {}
fn length() @builtin {}
fn log() @builtin {}
fn log2() @builtin {}
fn max() @builtin {}
fn min() @builtin {}
fn mix() @builtin {}
fn modf() @builtin {}
fn normalize() @builtin {}
fn pow() @builtin {}
fn quantizeToF16() @builtin {}
fn radians() @builtin {}
fn reflect() @builtin {}
fn refract() @builtin {}
fn reverseBits() @builtin {}
fn round() @builtin {}
fn saturate() @builtin {}
fn sign() @builtin {}
fn sin() @builtin {}
fn sinh() @builtin {}
fn smoothstep() @builtin {}
fn sqrt() @builtin {}
fn step() @builtin {}
fn tan() @builtin {}
fn tanh() @builtin {}
fn transpose() @builtin {}
fn trunc() @builtin {}

// packing
fn pack4x8snorm() @builtin {}
fn pack4x8unorm() @builtin {}
fn pack4xI8() @builtin {}
fn pack4xU8() @builtin {}
fn pack4xI8Clamp() @builtin {}
fn pack4xU8Clamp() @builtin {}
fn pack2x16snorm() @builtin {}
fn pack2x16unorm() @builtin {}
fn pack2x16float() @builtin {}
fn unpack4x8snorm() @builtin {}
fn unpack4x8unorm() @builtin {}
fn unpack4xI8() @builtin {}
fn unpack4xU8() @builtin {}
fn unpack2x16snorm() @builtin {}
fn unpack2x16unorm() @builtin {}
fn unpack2x16float() @builtin {}
