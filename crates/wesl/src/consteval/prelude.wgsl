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

@generic(T, N) @const @must_use fn array() -> array<T, N> @builtin { }
@generic(T) @const @must_use fn bool(e : T) -> bool { var inst: bool; return inst; }

@generic(T) @const @must_use fn vec2() -> vec2<T> @builtin { }
@generic(T) @const @must_use fn vec3() -> vec3<T> @builtin { }
@generic(T) @const @must_use fn vec4() -> vec4<T> @builtin { }

