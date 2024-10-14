/// The prelude contains all pre-declared aliases, built-in structs and functions in WGSL.
/// the @internal attribute renames declaration with a two-underscores prefix.
/// the @intrinsic attribute indicates that a function definition is defined by the compiler.
/// it means that it is not representable with user code: has generics, or variadics.

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
@const @must_use fn array() @intrinsic {}
@const @must_use fn bool() @intrinsic {}
@const @must_use fn i32() @intrinsic {}
@const @must_use fn u32() @intrinsic {}
@const @must_use fn f32() @intrinsic {}
@const @must_use fn f16() @intrinsic {}
@const @must_use fn mat2x2() @intrinsic {}
@const @must_use fn mat2x3() @intrinsic {}
@const @must_use fn mat2x4() @intrinsic {}
@const @must_use fn mat3x2() @intrinsic {}
@const @must_use fn mat3x3() @intrinsic {}
@const @must_use fn mat3x4() @intrinsic {}
@const @must_use fn mat4x2() @intrinsic {}
@const @must_use fn mat4x3() @intrinsic {}
@const @must_use fn mat4x4() @intrinsic {}
@const @must_use fn vec2() @intrinsic {}
@const @must_use fn vec3() @intrinsic {}
@const @must_use fn vec4() @intrinsic {}

// bitcast
@const @must_use fn bitcast() @intrinsic {}

// logical
@const @must_use fn all() @intrinsic {}
@const @must_use fn any() @intrinsic {}
@const @must_use fn select() @intrinsic {}

// array
@const @must_use fn arrayLength() @intrinsic {}

// numeric
@const @must_use fn abs() @intrinsic {}
@const @must_use fn acos() @intrinsic {}
@const @must_use fn acosh() @intrinsic {}
@const @must_use fn asin() @intrinsic {}
@const @must_use fn asinh() @intrinsic {}
@const @must_use fn atan() @intrinsic {}
@const @must_use fn atanh() @intrinsic {}
@const @must_use fn atan2() @intrinsic {}
@const @must_use fn ceil() @intrinsic {}
@const @must_use fn clamp() @intrinsic {}
@const @must_use fn cos() @intrinsic {}
@const @must_use fn cosh() @intrinsic {}
@const @must_use fn countLeadingZeros() @intrinsic {}
@const @must_use fn countOneBits() @intrinsic {}
@const @must_use fn countTrailingZeros() @intrinsic {}
@const @must_use fn cross() @intrinsic {}
@const @must_use fn degrees() @intrinsic {}
@const @must_use fn determinant() @intrinsic {}
@const @must_use fn distance() @intrinsic {}
@const @must_use fn dot() @intrinsic {}
@const @must_use fn dot4U8Packed() @intrinsic {}
@const @must_use fn dot4I8Packed() @intrinsic {}
@const @must_use fn exp() @intrinsic {}
@const @must_use fn exp2() @intrinsic {}
@const @must_use fn extractBits() @intrinsic {}
@const @must_use fn faceForward() @intrinsic {}
@const @must_use fn firstLeadingBit() @intrinsic {}
@const @must_use fn firstTrailingBit() @intrinsic {}
@const @must_use fn floor() @intrinsic {}
@const @must_use fn fma() @intrinsic {}
@const @must_use fn fract() @intrinsic {}
@const @must_use fn frexp() @intrinsic {}
@const @must_use fn insertBits() @intrinsic {}
@const @must_use fn inverseSqrt() @intrinsic {}
@const @must_use fn ldexp() @intrinsic {}
@const @must_use fn length() @intrinsic {}
@const @must_use fn log() @intrinsic {}
@const @must_use fn log2() @intrinsic {}
@const @must_use fn max() @intrinsic {}
@const @must_use fn min() @intrinsic {}
@const @must_use fn mix() @intrinsic {}
@const @must_use fn modf() @intrinsic {}
@const @must_use fn normalize() @intrinsic {}
@const @must_use fn pow() @intrinsic {}
@const @must_use fn quantizeToF16() @intrinsic {}
@const @must_use fn radians() @intrinsic {}
@const @must_use fn reflect() @intrinsic {}
@const @must_use fn refract() @intrinsic {}
@const @must_use fn reverseBits() @intrinsic {}
@const @must_use fn round() @intrinsic {}
@const @must_use fn saturate() @intrinsic {}
@const @must_use fn sign() @intrinsic {}
@const @must_use fn sin() @intrinsic {}
@const @must_use fn sinh() @intrinsic {}
@const @must_use fn smoothstep() @intrinsic {}
@const @must_use fn sqrt() @intrinsic {}
@const @must_use fn step() @intrinsic {}
@const @must_use fn tan() @intrinsic {}
@const @must_use fn tanh() @intrinsic {}
@const @must_use fn transpose() @intrinsic {}
@const @must_use fn trunc() @intrinsic {}

// packing
@const @must_use fn pack4x8snorm() @intrinsic { }
@const @must_use fn pack4x8unorm() @intrinsic {}
@const @must_use fn pack4xI8() @intrinsic {}
@const @must_use fn pack4xU8() @intrinsic {}
@const @must_use fn pack4xI8Clamp() @intrinsic {}
@const @must_use fn pack4xU8Clamp() @intrinsic {}
@const @must_use fn pack2x16snorm() @intrinsic {}
@const @must_use fn pack2x16unorm() @intrinsic {}
@const @must_use fn pack2x16float() @intrinsic {}
@const @must_use fn unpack4x8snorm() @intrinsic {}
@const @must_use fn unpack4x8unorm() @intrinsic {}
@const @must_use fn unpack4xI8() @intrinsic {}
@const @must_use fn unpack4xU8() @intrinsic {}
@const @must_use fn unpack2x16snorm() @intrinsic {}
@const @must_use fn unpack2x16unorm() @intrinsic {}
@const @must_use fn unpack2x16float() @intrinsic {}
