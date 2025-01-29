use wesl_macros::query;
use wgsl_parse::syntax::{
    Expression, ExpressionNode, GlobalDeclaration, TranslationUnit, TypeExpression,
};

use crate::visit::Visit;
use crate::{Diagnostic, Error};

#[derive(Clone, Debug, thiserror::Error)]
pub enum ValidateError {
    #[error("cannot find declaration of `{0}`")]
    UndefinedSymbol(String),
    #[error("incorrect number of arguments to `{0}`, expected `{1}`, got `{2}`")]
    ParamCount(String, usize, usize),
    #[error("`{0}` is not callable")]
    UnknownFunction(String),
}

type E = ValidateError;

const BUILTIN_NAMES: &[&str] = &[
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
    "vec4",
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
    // : bitcast
    "bitcast",
    // : logical
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
    // : derivative
    "dpdx",
    "dpdxCoarse",
    "dpdxFine",
    "dpdy",
    "dpdyCoarse",
    "dpdyFine",
    "fwidth",
    "fwidthCoarse",
    "fwidthFine",
    // : texture
    "textureDimensions",
    "textureGather",
    "textureGatherCompare",
    "textureLoad",
    "textureNumLayers",
    "textureNumLevels",
    "textureNumSamples",
    "textureSample",
    "textureSampleBias",
    "textureSampleCompare",
    "textureSampleCompareLevel",
    "textureSampleGrad",
    "textureSampleLevel",
    "textureSampleBaseClampToEdge",
    "textureStore",
    // : atomic
    "atomicLoad",
    "atomicStore",
    "atomicAdd",
    "atomicSub",
    "atomicMax",
    "atomicMin",
    "atomicAnd",
    "atomicOr",
    "atomicXor",
    "atomicExchange",
    "atomicCompareExchangeWeak",
    // : packing
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
    // : synchronization
    "storageBarrier",
    "textureBarrier",
    "workgroupBarrier",
    "workgroupUniformLoad",
    // : subgroup
    "subgroupAdd",
    "subgroupExclusiveAdd",
    "subgroupInclusiveAdd",
    "subgroupAll",
    "subgroupAnd",
    "subgroupAny",
    "subgroupBallot",
    "subgroupBroadcast",
    "subgroupBroadcastFirst",
    "subgroupElect",
    "subgroupMax",
    "subgroupMin",
    "subgroupMul",
    "subgroupExclusiveMul",
    "subgroupInclusiveMul",
    "subgroupOr",
    "subgroupShuffle",
    "subgroupShuffleDown",
    "subgroupShuffleUp",
    "subgroupShuffleXor",
    "subgroupXor",
    // : quad
    "quadBroadcast",
    "quadSwapDiagonal",
    "quadSwapX",
    "quadSwapY",
    // : predeclared enumerants
    "read",
    "write",
    "read_write",
    "function",
    "private",
    "workgroup",
    "uniform",
    "storage",
    "rgba8unorm",
    "rgba8snorm",
    "rgba8uint",
    "rgba8sint",
    "rgba16uint",
    "rgba16sint",
    "rgba16float",
    "r32uint",
    "r32sint",
    "r32float",
    "rg32uint",
    "rg32sint",
    "rg32float",
    "rgba32uint",
    "rgba32sint",
    "rgba32float",
    "bgra8unorm",
];

const BUILTIN_FUNCTIONS: &[&str] = &[
    // constructor built-in functions
    "bool",
    "f16",
    "f32",
    "i32",
    "u32",
    // type-generators
    "array",
    "mat2x2",
    "mat2x3",
    "mat2x4",
    "mat3x2",
    "mat3x3",
    "mat3x4",
    "mat4x2",
    "mat4x3",
    "mat4x4",
    "vec2",
    "vec3",
    "vec4",
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
    // : bitcast
    "bitcast",
    // : logical
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
    // : derivative
    "dpdx",
    "dpdxCoarse",
    "dpdxFine",
    "dpdy",
    "dpdyCoarse",
    "dpdyFine",
    "fwidth",
    "fwidthCoarse",
    "fwidthFine",
    // : texture
    "textureDimensions",
    "textureGather",
    "textureGatherCompare",
    "textureLoad",
    "textureNumLayers",
    "textureNumLevels",
    "textureNumSamples",
    "textureSample",
    "textureSampleBias",
    "textureSampleCompare",
    "textureSampleCompareLevel",
    "textureSampleGrad",
    "textureSampleLevel",
    "textureSampleBaseClampToEdge",
    "textureStore",
    // : atomic
    "atomicLoad",
    "atomicStore",
    "atomicAdd",
    "atomicSub",
    "atomicMax",
    "atomicMin",
    "atomicAnd",
    "atomicOr",
    "atomicXor",
    "atomicExchange",
    "atomicCompareExchangeWeak",
    // : packing
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
    // : synchronization
    "storageBarrier",
    "textureBarrier",
    "workgroupBarrier",
    "workgroupUniformLoad",
    // : subgroup
    "subgroupAdd",
    "subgroupExclusiveAdd",
    "subgroupInclusiveAdd",
    "subgroupAll",
    "subgroupAnd",
    "subgroupAny",
    "subgroupBallot",
    "subgroupBroadcast",
    "subgroupBroadcastFirst",
    "subgroupElect",
    "subgroupMax",
    "subgroupMin",
    "subgroupMul",
    "subgroupExclusiveMul",
    "subgroupInclusiveMul",
    "subgroupOr",
    "subgroupShuffle",
    "subgroupShuffleDown",
    "subgroupShuffleUp",
    "subgroupShuffleXor",
    "subgroupXor",
    // : quad
    "quadBroadcast",
    "quadSwapDiagonal",
    "quadSwapX",
    "quadSwapY",
];

// note that this function could be simplified if we didn't care about the diagnostics metadata (declaration and expression)
fn check_defined_symbols(wesl: &TranslationUnit) -> Result<(), Diagnostic<Error>> {
    fn check_ty(ty: &TypeExpression) -> Result<(), Diagnostic<Error>> {
        if ty.ident.use_count() == 1 && !BUILTIN_NAMES.contains(&ty.ident.name().as_str()) {
            Err(E::UndefinedSymbol(ty.ident.to_string()).into())
        } else {
            for arg in ty.template_args.iter().flatten() {
                check_expr(&arg.expression)?;
            }
            Ok(())
        }
    }
    fn check_expr(expr: &ExpressionNode) -> Result<(), Diagnostic<Error>> {
        if let Expression::TypeOrIdentifier(ty) = expr.node() {
            check_ty(ty).map_err(|d| d.with_span(expr.span().clone()))
        } else if let Expression::FunctionCall(call) = expr.node() {
            check_ty(&call.ty).map_err(|d| d.with_span(expr.span().clone()))
        } else {
            for expr in Visit::<ExpressionNode>::visit(expr.node()) {
                check_expr(expr)?;
            }
            Ok(())
        }
    }
    fn check_decl(decl: &GlobalDeclaration) -> Result<(), Diagnostic<Error>> {
        let decl_name = decl.ident().map(|ident| ident.name().to_string());
        for expr in Visit::<ExpressionNode>::visit(decl) {
            check_expr(expr).map_err(|mut d| {
                d.declaration = decl_name.clone();
                d
            })?;
        }

        // those are the attributes that don't have an expression as parent.
        // unfortunately the diagnostic won't have a span :(
        for ty in query!(decl.{
            GlobalDeclaration::Declaration.ty.[],
            GlobalDeclaration::TypeAlias.ty,
            GlobalDeclaration::Struct.members.[].ty,
            GlobalDeclaration::Function.{ parameters.[].ty, return_type.[] }
        }) {
            check_ty(ty).map_err(|mut d| {
                d.declaration = decl_name.clone();
                d
            })?;
        }
        Ok(())
    }

    for decl in &wesl.global_declarations {
        check_decl(decl)?;
    }
    Ok(())
}

fn check_function_calls(wesl: &TranslationUnit) -> Result<(), Diagnostic<Error>> {
    fn check_expr(expr: &Expression, wesl: &TranslationUnit) -> Result<(), E> {
        if let Expression::FunctionCall(call) = expr {
            let decl = wesl
                .global_declarations
                .iter()
                .find(|decl| decl.ident().is_some_and(|id| id == &call.ty.ident));

            match decl {
                Some(GlobalDeclaration::Function(decl)) => {
                    if call.arguments.len() != decl.parameters.len() {
                        return Err(E::ParamCount(
                            call.ty.ident.to_string(),
                            decl.parameters.len(),
                            call.arguments.len(),
                        ));
                    }
                }
                Some(GlobalDeclaration::Struct(decl)) => {
                    if call.arguments.len() != decl.members.len() && !call.arguments.is_empty() {
                        return Err(E::ParamCount(
                            call.ty.ident.to_string(),
                            decl.members.len(),
                            call.arguments.len(),
                        ));
                    }
                }
                Some(GlobalDeclaration::TypeAlias(_)) => {
                    // TODO: resolve type-alias
                }
                Some(_) => return Err(E::UnknownFunction(call.ty.ident.to_string())),
                None => {
                    if BUILTIN_FUNCTIONS
                        .iter()
                        .any(|name| name == &*call.ty.ident.name())
                    {
                        // TODO: check num args for builtin functions
                    } else {
                        return Err(E::UnknownFunction(call.ty.ident.to_string()));
                    }
                }
            };
        }
        Ok(())
    }
    for decl in &wesl.global_declarations {
        for expr in Visit::<ExpressionNode>::visit(decl) {
            check_expr(expr, wesl).map_err(|e| {
                let mut err = Diagnostic::from(e);
                err.span = Some(expr.span().clone());
                err.declaration = decl.ident().map(|id| id.name().to_string());
                err
            })?;
        }
    }
    Ok(())
}

pub fn validate(wesl: &TranslationUnit) -> Result<(), Diagnostic<Error>> {
    check_defined_symbols(wesl)?;
    check_function_calls(wesl)?;
    Ok(())
}
