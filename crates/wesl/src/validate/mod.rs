use wgsl_parse::{
    syntax::{Ident, TranslationUnit},
    visit::Visit,
};

use crate::{Diagnostic, Error};

#[derive(Clone, Debug, thiserror::Error)]
pub enum ValidateError {
    #[error("cannot find declaration of `{0}`")]
    UndefinedSymbol(Ident),
}

type E = ValidateError;

const BUILTIN_NAMES: &[&'static str] = &[
    // https://www.w3.org/TR/WGSL/#predeclared-types
    // types
    "bool",
    "f16",
    "f32",
    "i32",
    "sampler",
    "sampler_comparison",
    "texture_depth_2d",
    "texture_depth_2d_array",
    "texture_depth_cube",
    "texture_depth_cube_array",
    "texture_depth_multisampled_2d",
    "texture_external",
    "u32",
    // type-generators
    "array",
    "atomic",
    "mat2x2",
    "mat2x3",
    "mat2x4",
    "mat3x2",
    "mat3x3",
    "mat3x4",
    "mat4x2",
    "mat4x3",
    "mat4x4",
    "ptr",
    "texture_1d",
    "texture_2d",
    "texture_2d_array",
    "texture_3d",
    "texture_cube",
    "texture_cube_array",
    "texture_multisampled_2d",
    "texture_storage_1d",
    "texture_storage_2d",
    "texture_storage_2d_array",
    "texture_storage_3d",
    "vec2",
    "vec3",
    "vec4 ",
    // predeclared aliases
    "vec2i",
    "vec3i",
    "vec4i",
    "vec2u",
    "vec3u",
    "vec4u",
    "vec2f",
    "vec3f",
    "vec4f",
    "vec2h",
    "vec3h",
    "vec4h",
    "mat2x2f",
    "mat2x3f",
    "mat2x4f",
    "mat3x2f",
    "mat3x3f",
    "mat3x4f",
    "mat4x2f",
    "mat4x3f",
    "mat4x4f",
    "mat2x2h",
    "mat2x3h",
    "mat2x4h",
    "mat3x2h",
    "mat3x3h",
    "mat3x4h",
    "mat4x2h",
    "mat4x3h",
    "mat4x4h",
    // built-in functions
    "bitcast",
    "all",
    "any",
    "select",
    "arrayLength",
    "abs",
    "acos",
    "acosh",
    "asin",
    "asinh",
    "atan",
    "atanh",
    "atan2",
    "ceil",
    "clamp",
    "cos",
    "cosh",
    "countLeadingZeros",
    "countOneBits",
    "countTrailingZeros",
    "cross",
    "degrees",
    "determinant",
    "distance",
    "dot",
    "dot4U8Packed",
    "exp",
    "exp2",
    "extractBits",
    "faceForward",
    "firstLeadingBit",
    "firstTrailingBit",
    "floor",
    "fma",
    "fract",
    "frexp",
    "insertBits",
    "inverseSqrt",
    "ldexp",
    "length",
    "log",
    "log2",
    "max",
    "min",
    "mix",
    "modf",
    "normalize",
    "pow",
    "quantizeToF16",
    "radians",
    "reflect",
    "refract",
    "reverseBits",
    "round",
    "saturate",
    "sign",
    "sin",
    "sinh",
    "smoothstep",
    "sqrt",
    "step",
    "tan",
    "tanh",
    "transpose",
    "trunc",
    "pack4x8snorm",
    "pack4x8unorm",
    "pack4xI8",
    "pack4xU8",
    "pack4xI8Clamp",
    "pack4xU8Clamp",
    "pack2x16snorm",
    "pack2x16unorm",
    "pack2x16float",
    "unpack4x8snorm",
    "unpack4x8unorm",
    "unpack4xI8",
    "unpack4xU8",
    "unpack2x16snorm",
    "unpack2x16unorm",
    "unpack2x16float",
];

fn check_defined_symbols(wesl: &TranslationUnit) -> Result<(), Error> {
    for decl in &wesl.global_declarations {
        let decl_name = decl.ident().map(|ident| ident.name().to_string());
        for ty in decl.visit() {
            if ty.ident.use_count() == 1
                && !BUILTIN_NAMES.contains(&ty.ident.name().as_str())
                && !wesl
                    .global_declarations
                    .iter()
                    .filter_map(|decl| decl.ident())
                    .any(|ident| ident.name().as_str() == ty.ident.name().as_str())
            {
                let mut err = Diagnostic::from(E::UndefinedSymbol(ty.ident.clone()));
                err.declaration = decl_name;
                return Err(err.into());
            }
        }
    }
    Ok(())
}

pub fn validate(wesl: &TranslationUnit) -> Result<(), Error> {
    check_defined_symbols(wesl)
}
