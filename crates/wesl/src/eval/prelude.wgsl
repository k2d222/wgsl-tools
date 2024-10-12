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
@const fn array() @intrinsic {}
@const fn bool() @intrinsic {}
@const fn i32() @intrinsic {}
@const fn u32() @intrinsic {}
@const fn f32() @intrinsic {}
@const fn f16() @intrinsic {}
@const fn mat2x2() @intrinsic {}
@const fn mat2x3() @intrinsic {}
@const fn mat2x4() @intrinsic {}
@const fn mat3x2() @intrinsic {}
@const fn mat3x3() @intrinsic {}
@const fn mat3x4() @intrinsic {}
@const fn mat4x2() @intrinsic {}
@const fn mat4x3() @intrinsic {}
@const fn mat4x4() @intrinsic {}
@const fn vec2() @intrinsic {}
@const fn vec3() @intrinsic {}
@const fn vec4() @intrinsic {}

// bitcast
@const fn bitcast() @intrinsic {}

// logical
@const fn all() @intrinsic {}
@const fn any() @intrinsic {}
@const fn select() @intrinsic {}

// array
@const fn arrayLength() @intrinsic {}

// numeric
@const fn abs() @intrinsic {}
@const fn acos() @intrinsic {}
@const fn acosh() @intrinsic {}
@const fn asin() @intrinsic {}
@const fn asinh() @intrinsic {}
@const fn atan() @intrinsic {}
@const fn atanh() @intrinsic {}
@const fn atan2() @intrinsic {}
@const fn ceil() @intrinsic {}
@const fn clamp() @intrinsic {}
@const fn cos() @intrinsic {}
@const fn cosh() @intrinsic {}
@const fn countLeadingZeros() @intrinsic {}
@const fn countOneBits() @intrinsic {}
@const fn countTrailingZeros() @intrinsic {}
@const fn cross() @intrinsic {}
@const fn degrees() @intrinsic {}
@const fn determinant() @intrinsic {}
@const fn distance() @intrinsic {}
@const fn dot() @intrinsic {}
@const fn dot4U8Packed() @intrinsic {}
@const fn dot4I8Packed() @intrinsic {}
@const fn exp() @intrinsic {}
@const fn exp2() @intrinsic {}
@const fn extractBits() @intrinsic {}
@const fn faceForward() @intrinsic {}
@const fn firstLeadingBit() @intrinsic {}
@const fn firstTrailingBit() @intrinsic {}
@const fn floor() @intrinsic {}
@const fn fma() @intrinsic {}
@const fn fract() @intrinsic {}
@const fn frexp() @intrinsic {}
@const fn insertBits() @intrinsic {}
@const fn inverseSqrt() @intrinsic {}
@const fn ldexp() @intrinsic {}
@const fn length() @intrinsic {}
@const fn log() @intrinsic {}
@const fn log2() @intrinsic {}
@const fn max() @intrinsic {}
@const fn min() @intrinsic {}
@const fn mix() @intrinsic {}
@const fn modf() @intrinsic {}
@const fn normalize() @intrinsic {}
@const fn pow() @intrinsic {}
@const fn quantizeToF16() @intrinsic {}
@const fn radians() @intrinsic {}
@const fn reflect() @intrinsic {}
@const fn refract() @intrinsic {}
@const fn reverseBits() @intrinsic {}
@const fn round() @intrinsic {}
@const fn saturate() @intrinsic {}
@const fn sign() @intrinsic {}
@const fn sin() @intrinsic {}
@const fn sinh() @intrinsic {}
@const fn smoothstep() @intrinsic {}
@const fn sqrt() @intrinsic {}
@const fn step() @intrinsic {}
@const fn tan() @intrinsic {}
@const fn tanh() @intrinsic {}
@const fn transpose() @intrinsic {}
@const fn trunc() @intrinsic {}

// packing
@const fn pack4x8snorm() @intrinsic {}
@const fn pack4x8unorm() @intrinsic {}
@const fn pack4xI8() @intrinsic {}
@const fn pack4xU8() @intrinsic {}
@const fn pack4xI8Clamp() @intrinsic {}
@const fn pack4xU8Clamp() @intrinsic {}
@const fn pack2x16snorm() @intrinsic {}
@const fn pack2x16unorm() @intrinsic {}
@const fn pack2x16float() @intrinsic {}
@const fn unpack4x8snorm() @intrinsic {}
@const fn unpack4x8unorm() @intrinsic {}
@const fn unpack4xI8() @intrinsic {}
@const fn unpack4xU8() @intrinsic {}
@const fn unpack2x16snorm() @intrinsic {}
@const fn unpack2x16unorm() @intrinsic {}
@const fn unpack2x16float() @intrinsic {}
