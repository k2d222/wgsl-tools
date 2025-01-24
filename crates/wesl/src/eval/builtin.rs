use std::sync::LazyLock;

use half::prelude::*;
use num_traits::{real::Real, FromPrimitive, One, ToBytes, ToPrimitive, Zero};

use itertools::{chain, izip, Itertools};
use lazy_static::lazy_static;
use wgsl_parse::syntax::{
    AccessMode, AddressSpace, Attribute, CustomAttribute, Expression, GlobalDeclaration, Ident,
    LiteralExpression, TemplateArg, TranslationUnit, TypeExpression,
};

use crate::eval::{Context, Eval};

use super::{
    conv::{convert_all, Convert},
    convert, convert_all_inner_to, convert_all_ty,
    ops::Compwise,
    ArrayInstance, EvalError, EvalStage, EvalTy, Instance, LiteralInstance, MatInstance,
    RefInstance, StructInstance, SyntaxUtil, Ty, Type, VecInstance,
};

type E = EvalError;

// TODO: when we have the wgsl! macro, we can refactor the consts.

pub static EXPR_TRUE: Expression = Expression::Literal(LiteralExpression::Bool(true));
pub static EXPR_FALSE: Expression = Expression::Literal(LiteralExpression::Bool(false));

pub static IDENT_BOOL: LazyLock<Ident> = LazyLock::new(|| Ident::new("bool".to_string()));
pub static IDENT_I32: LazyLock<Ident> = LazyLock::new(|| Ident::new("i32".to_string()));
pub static IDENT_U32: LazyLock<Ident> = LazyLock::new(|| Ident::new("u32".to_string()));
pub static IDENT_F32: LazyLock<Ident> = LazyLock::new(|| Ident::new("f32".to_string()));
pub static IDENT_F16: LazyLock<Ident> = LazyLock::new(|| Ident::new("f32".to_string()));
pub static IDENT_ARRAY: LazyLock<Ident> = LazyLock::new(|| Ident::new("array".to_string()));
pub static IDENT_ATOMIC: LazyLock<Ident> = LazyLock::new(|| Ident::new("atomic".to_string()));
pub static IDENT_PTR: LazyLock<Ident> = LazyLock::new(|| Ident::new("ptr".to_string()));
pub static IDENT_VEC2: LazyLock<Ident> = LazyLock::new(|| Ident::new("vec2".to_string()));
pub static IDENT_VEC3: LazyLock<Ident> = LazyLock::new(|| Ident::new("vec3".to_string()));
pub static IDENT_VEC4: LazyLock<Ident> = LazyLock::new(|| Ident::new("vec4".to_string()));
pub static IDENT_MAT2X2: LazyLock<Ident> = LazyLock::new(|| Ident::new("mat2x2".to_string()));
pub static IDENT_MAT3X2: LazyLock<Ident> = LazyLock::new(|| Ident::new("mat3x2".to_string()));
pub static IDENT_MAT4X2: LazyLock<Ident> = LazyLock::new(|| Ident::new("mat4x2".to_string()));
pub static IDENT_MAT2X3: LazyLock<Ident> = LazyLock::new(|| Ident::new("mat2x3".to_string()));
pub static IDENT_MAT3X3: LazyLock<Ident> = LazyLock::new(|| Ident::new("mat3x3".to_string()));
pub static IDENT_MAT4X3: LazyLock<Ident> = LazyLock::new(|| Ident::new("mat4x3".to_string()));
pub static IDENT_MAT2X4: LazyLock<Ident> = LazyLock::new(|| Ident::new("mat2x4".to_string()));
pub static IDENT_MAT3X4: LazyLock<Ident> = LazyLock::new(|| Ident::new("mat3x4".to_string()));
pub static IDENT_MAT4X4: LazyLock<Ident> = LazyLock::new(|| Ident::new("mat4x4".to_string()));

pub fn name_to_builtin_ident(name: &str) -> Option<Ident> {
    match name {
        "bool" => Some(IDENT_BOOL.clone()),
        "i32" => Some(IDENT_BOOL.clone()),
        "u32" => Some(IDENT_BOOL.clone()),
        "f32" => Some(IDENT_BOOL.clone()),
        "array" => Some(IDENT_BOOL.clone()),
        "atomic" => Some(IDENT_BOOL.clone()),
        "ptr" => Some(IDENT_BOOL.clone()),
        "vec2" => Some(IDENT_BOOL.clone()),
        "vec3" => Some(IDENT_BOOL.clone()),
        "vec4" => Some(IDENT_BOOL.clone()),
        "mat2x2" => Some(IDENT_MAT2X2.clone()),
        "mat3x2" => Some(IDENT_MAT3X2.clone()),
        "mat4x2" => Some(IDENT_MAT4X2.clone()),
        "mat2x3" => Some(IDENT_MAT2X3.clone()),
        "mat3x3" => Some(IDENT_MAT3X3.clone()),
        "mat4x3" => Some(IDENT_MAT4X3.clone()),
        "mat2x4" => Some(IDENT_MAT2X4.clone()),
        "mat3x4" => Some(IDENT_MAT3X4.clone()),
        "mat4x4" => Some(IDENT_MAT4X4.clone()),
        _ => None,
    }
}

lazy_static! {
    pub static ref ATTR_INTRINSIC: Attribute = Attribute::Custom(CustomAttribute {
        name: "__intrinsic".to_string(),
        arguments: None
    });
    pub static ref PRELUDE: TranslationUnit = {
        let mut prelude = include_str!("prelude.wgsl")
            .parse::<TranslationUnit>()
            .inspect_err(|e| eprintln!("{e}"))
            .unwrap();

        let attr_internal = Attribute::Custom(CustomAttribute {
            name: "internal".to_string(),
            arguments: None,
        });
        let attr_intrinsic = Attribute::Custom(CustomAttribute {
            name: "intrinsic".to_string(),
            arguments: None,
        });

        for decl in &mut prelude.global_declarations {
            match decl {
                GlobalDeclaration::Struct(s) => {
                    if s.attributes.contains(&attr_internal) {
                        s.ident = Ident::new(format!("__{}", s.ident));
                    }
                }
                GlobalDeclaration::Function(f) => {
                    if let Some(attr) = f
                        .body
                        .attributes
                        .iter_mut()
                        .find(|attr| **attr == attr_intrinsic)
                    {
                        *attr = ATTR_INTRINSIC.clone();
                    }
                }
                _ => (),
            }
        }

        prelude
    };
}

pub fn call_builtin(
    ty: &TypeExpression,
    args: Vec<Instance>,
    ctx: &mut Context,
) -> Result<Instance, E> {
    match (
        ty.ident.name().as_str(),
        ty.template_args.as_deref(),
        args.as_slice(),
    ) {
        // constructors
        ("array", Some(t), []) => Instance::zero_value(&ArrayTemplate::parse(t, ctx)?.ty(), ctx),
        ("array", Some(t), a) => call_array_t(ArrayTemplate::parse(t, ctx)?, a),
        ("array", None, a) => call_array(a),
        ("bool", None, []) => Instance::zero_value(&Type::Bool, ctx),
        ("bool", None, [a1]) => call_bool_1(a1),
        ("i32", None, []) => Instance::zero_value(&Type::I32, ctx),
        ("i32", None, [a1]) => call_i32_1(a1),
        ("u32", None, []) => Instance::zero_value(&Type::U32, ctx),
        ("u32", None, [a1]) => call_u32_1(a1),
        ("f32", None, []) => Instance::zero_value(&Type::F32, ctx),
        ("f32", None, [a1]) => call_f32_1(a1, ctx.stage),
        ("f16", None, []) => Instance::zero_value(&Type::F16, ctx),
        ("f16", None, [a1]) => call_f16_1(a1, ctx.stage),
        ("mat2x2", Some(t), []) => Instance::zero_value(&MatTemplate::parse(t, ctx)?.ty(2, 2), ctx),
        ("mat2x2", Some(t), a) => call_mat_t(2, 2, MatTemplate::parse(t, ctx)?, a, ctx.stage),
        ("mat2x2", None, a) => call_mat(2, 2, a),
        ("mat2x3", Some(t), []) => Instance::zero_value(&MatTemplate::parse(t, ctx)?.ty(2, 3), ctx),
        ("mat2x3", Some(t), a) => call_mat_t(2, 3, MatTemplate::parse(t, ctx)?, a, ctx.stage),
        ("mat2x3", None, a) => call_mat(2, 3, a),
        ("mat2x4", Some(t), []) => Instance::zero_value(&MatTemplate::parse(t, ctx)?.ty(2, 4), ctx),
        ("mat2x4", Some(t), a) => call_mat_t(2, 4, MatTemplate::parse(t, ctx)?, a, ctx.stage),
        ("mat2x4", None, a) => call_mat(2, 4, a),
        ("mat3x2", Some(t), []) => Instance::zero_value(&MatTemplate::parse(t, ctx)?.ty(3, 2), ctx),
        ("mat3x2", Some(t), a) => call_mat_t(3, 2, MatTemplate::parse(t, ctx)?, a, ctx.stage),
        ("mat3x2", None, a) => call_mat(3, 2, a),
        ("mat3x3", Some(t), []) => Instance::zero_value(&MatTemplate::parse(t, ctx)?.ty(3, 3), ctx),
        ("mat3x3", Some(t), a) => call_mat_t(3, 3, MatTemplate::parse(t, ctx)?, a, ctx.stage),
        ("mat3x3", None, a) => call_mat(3, 3, a),
        ("mat3x4", Some(t), []) => Instance::zero_value(&MatTemplate::parse(t, ctx)?.ty(3, 4), ctx),
        ("mat3x4", Some(t), a) => call_mat_t(3, 4, MatTemplate::parse(t, ctx)?, a, ctx.stage),
        ("mat3x4", None, a) => call_mat(3, 4, a),
        ("mat4x2", Some(t), []) => Instance::zero_value(&MatTemplate::parse(t, ctx)?.ty(4, 2), ctx),
        ("mat4x2", Some(t), a) => call_mat_t(4, 2, MatTemplate::parse(t, ctx)?, a, ctx.stage),
        ("mat4x2", None, a) => call_mat(4, 2, a),
        ("mat4x3", Some(t), []) => Instance::zero_value(&MatTemplate::parse(t, ctx)?.ty(4, 3), ctx),
        ("mat4x3", Some(t), a) => call_mat_t(4, 3, MatTemplate::parse(t, ctx)?, a, ctx.stage),
        ("mat4x3", None, a) => call_mat(4, 3, a),
        ("mat4x4", Some(t), []) => Instance::zero_value(&MatTemplate::parse(t, ctx)?.ty(4, 4), ctx),
        ("mat4x4", Some(t), a) => call_mat_t(4, 4, MatTemplate::parse(t, ctx)?, a, ctx.stage),
        ("mat4x4", None, a) => call_mat(4, 4, a),
        ("vec2", Some(t), []) => Instance::zero_value(&VecTemplate::parse(t, ctx)?.ty(2), ctx),
        ("vec2", Some(t), a) => call_vec_t(2, VecTemplate::parse(t, ctx)?, a, ctx.stage),
        ("vec2", None, a) => call_vec(2, a),
        ("vec3", Some(t), []) => Instance::zero_value(&VecTemplate::parse(t, ctx)?.ty(3), ctx),
        ("vec3", Some(t), a) => call_vec_t(3, VecTemplate::parse(t, ctx)?, a, ctx.stage),
        ("vec3", None, a) => call_vec(3, a),
        ("vec4", Some(t), []) => Instance::zero_value(&VecTemplate::parse(t, ctx)?.ty(4), ctx),
        ("vec4", Some(t), a) => call_vec_t(4, VecTemplate::parse(t, ctx)?, a, ctx.stage),
        ("vec4", None, a) => call_vec(4, a),
        // bitcast
        ("bitcast", Some(t), [a1]) => call_bitcast_t(BitcastTemplate::parse(t, ctx)?, a1),
        // logical
        ("all", None, [a]) => call_all(a),
        ("any", None, [a]) => call_any(a),
        ("select", None, [a1, a2, a3]) => call_select(a1, a2, a3),
        // array
        ("arrayLength", None, [a]) => call_arraylength(a),
        // numeric
        ("abs", None, [a]) => call_abs(a),
        ("acos", None, [a]) => call_acos(a),
        ("acosh", None, [a]) => call_acosh(a),
        ("asin", None, [a]) => call_asin(a),
        ("asinh", None, [a]) => call_asinh(a),
        ("atan", None, [a]) => call_atan(a),
        ("atanh", None, [a]) => call_atanh(a),
        ("atan2", None, [a1, a2]) => call_atan2(a1, a2),
        ("ceil", None, [a]) => call_ceil(a),
        ("clamp", None, [a1, a2, a3]) => call_clamp(a1, a2, a3),
        ("cos", None, [a]) => call_cos(a),
        ("cosh", None, [a]) => call_cosh(a),
        ("countLeadingZeros", None, [a]) => call_countleadingzeros(a),
        ("countOneBits", None, [a]) => call_countonebits(a),
        ("countTrailingZeros", None, [a]) => call_counttrailingzeros(a),
        ("cross", None, [a1, a2]) => call_cross(a1, a2, ctx.stage),
        ("degrees", None, [a]) => call_degrees(a),
        ("determinant", None, [a]) => call_determinant(a),
        ("distance", None, [a1, a2]) => call_distance(a1, a2, ctx.stage),
        ("dot", None, [a1, a2]) => call_dot(a1, a2, ctx.stage),
        ("dot4U8Packed", None, [a1, a2]) => call_dot4u8packed(a1, a2),
        ("dot4I8Packed", None, [a1, a2]) => call_dot4i8packed(a1, a2),
        ("exp", None, [a]) => call_exp(a),
        ("exp2", None, [a]) => call_exp2(a),
        ("extractBits", None, [a1, a2, a3]) => call_extractbits(a1, a2, a3),
        ("faceForward", None, [a1, a2, a3]) => call_faceforward(a1, a2, a3),
        ("firstLeadingBit", None, [a]) => call_firstleadingbit(a),
        ("firstTrailingBit", None, [a]) => call_firsttrailingbit(a),
        ("floor", None, [a]) => call_floor(a),
        ("fma", None, [a1, a2, a3]) => call_fma(a1, a2, a3),
        ("fract", None, [a]) => call_fract(a, ctx.stage),
        ("frexp", None, [a]) => call_frexp(a),
        ("insertBits", None, [a1, a2, a3, a4]) => call_insertbits(a1, a2, a3, a4),
        ("inverseSqrt", None, [a]) => call_inversesqrt(a),
        ("ldexp", None, [a1, a2]) => call_ldexp(a1, a2),
        ("length", None, [a]) => call_length(a),
        ("log", None, [a]) => call_log(a),
        ("log2", None, [a]) => call_log2(a),
        ("max", None, [a1, a2]) => call_max(a1, a2),
        ("min", None, [a1, a2]) => call_min(a1, a2),
        ("mix", None, [a1, a2, a3]) => call_mix(a1, a2, a3),
        ("modf", None, [a]) => call_modf(a),
        ("normalize", None, [a]) => call_normalize(a),
        ("pow", None, [a1, a2]) => call_pow(a1, a2),
        ("quantizeToF16", None, [a]) => call_quantizetof16(a),
        ("radians", None, [a]) => call_radians(a),
        ("reflect", None, [a1, a2]) => call_reflect(a1, a2),
        ("refract", None, [a1, a2, a3]) => call_refract(a1, a2, a3),
        ("reverseBits", None, [a]) => call_reversebits(a),
        ("round", None, [a]) => call_round(a),
        ("saturate", None, [a]) => call_saturate(a),
        ("sign", None, [a]) => call_sign(a),
        ("sin", None, [a]) => call_sin(a),
        ("sinh", None, [a]) => call_sinh(a),
        ("smoothstep", None, [a1, a2, a3]) => call_smoothstep(a1, a2, a3),
        ("sqrt", None, [a]) => call_sqrt(a),
        ("step", None, [a1, a2]) => call_step(a1, a2),
        ("tan", None, [a]) => call_tan(a),
        ("tanh", None, [a]) => call_tanh(a),
        ("transpose", None, [a]) => call_transpose(a),
        ("trunc", None, [a]) => call_trunc(a),
        // packing
        ("pack4x8snorm", None, [a]) => call_pack4x8snorm(a),
        ("pack4x8unorm", None, [a]) => call_pack4x8unorm(a),
        ("pack4xI8", None, [a]) => call_pack4xi8(a),
        ("pack4xU8", None, [a]) => call_pack4xu8(a),
        ("pack4xI8Clamp", None, [a]) => call_pack4xi8clamp(a),
        ("pack4xU8Clamp", None, [a]) => call_pack4xu8clamp(a),
        ("pack2x16snorm", None, [a]) => call_pack2x16snorm(a),
        ("pack2x16unorm", None, [a]) => call_pack2x16unorm(a),
        ("pack2x16float", None, [a]) => call_pack2x16float(a),
        ("unpack4x8snorm", None, [a]) => call_unpack4x8snorm(a),
        ("unpack4x8unorm", None, [a]) => call_unpack4x8unorm(a),
        ("unpack4xI8", None, [a]) => call_unpack4xi8(a),
        ("unpack4xU8", None, [a]) => call_unpack4xu8(a),
        ("unpack2x16snorm", None, [a]) => call_unpack2x16snorm(a),
        ("unpack2x16unorm", None, [a]) => call_unpack2x16unorm(a),
        ("unpack2x16float", None, [a]) => call_unpack2x16float(a),

        _ => Err(E::Signature(ty.clone(), args)),
    }
}

// -----------
// ZERO VALUES
// -----------
// reference: <https://www.w3.org/TR/WGSL/#zero-value>

impl Instance {
    /// zero-value initialize an instance of a given type.
    pub fn zero_value(ty: &Type, ctx: &mut Context) -> Result<Self, E> {
        match ty {
            Type::Bool => Ok(LiteralInstance::Bool(false).into()),
            Type::AbstractInt => Ok(LiteralInstance::AbstractInt(0).into()),
            Type::AbstractFloat => Ok(LiteralInstance::AbstractFloat(0.0).into()),
            Type::I32 => Ok(LiteralInstance::I32(0).into()),
            Type::U32 => Ok(LiteralInstance::U32(0).into()),
            Type::F32 => Ok(LiteralInstance::F32(0.0).into()),
            Type::F16 => Ok(LiteralInstance::F16(f16::zero()).into()),
            Type::Struct(ident) => StructInstance::zero_value(ident.clone(), ctx).map(Into::into),
            Type::Array(Some(n), a_ty) => ArrayInstance::zero_value(*n, a_ty, ctx).map(Into::into),
            Type::Array(None, _) => Err(E::NotConstructible(ty.clone())),
            Type::Vec(n, v_ty) => VecInstance::zero_value(*n, v_ty).map(Into::into),
            Type::Mat(c, r, m_ty) => MatInstance::zero_value(*c, *r, m_ty).map(Into::into),
            Type::Atomic(_) => Err(E::NotConstructible(ty.clone())),
            Type::Ptr(_, _) => Err(E::NotConstructible(ty.clone())),
            Type::Void => Ok(Instance::Void),
        }
    }
}

impl LiteralInstance {
    pub fn zero_value(ty: &Type) -> Result<Self, E> {
        match ty {
            Type::Bool => Ok(LiteralInstance::Bool(false)),
            Type::AbstractInt => Ok(LiteralInstance::AbstractInt(0)),
            Type::AbstractFloat => Ok(LiteralInstance::AbstractFloat(0.0)),
            Type::I32 => Ok(LiteralInstance::I32(0)),
            Type::U32 => Ok(LiteralInstance::U32(0)),
            Type::F32 => Ok(LiteralInstance::F32(0.0)),
            Type::F16 => Ok(LiteralInstance::F16(f16::zero())),
            _ => Err(E::NotScalar(ty.clone())),
        }
    }
}

impl StructInstance {
    /// zero-value initialize a struct instance.
    pub fn zero_value(ident: Ident, ctx: &mut Context) -> Result<Self, E> {
        let decl = ctx
            .source
            .decl_struct(&ident)
            .expect("struct declaration not found");

        let members = decl
            .members
            .iter()
            .map(|m| {
                let ty = m.ty.eval_ty(ctx)?;
                let val = Instance::zero_value(&ty, ctx)?;
                Ok((m.ident.clone(), val))
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(StructInstance::new(ident, members))
    }
}

impl ArrayInstance {
    /// zero-value initialize an array instance.
    pub fn zero_value(n: usize, ty: &Type, ctx: &mut Context) -> Result<Self, E> {
        let zero = Instance::zero_value(ty, ctx)?;
        let comps = (0..n).map(|_| zero.clone()).collect_vec();
        Ok(ArrayInstance::new(comps, false))
    }
}

impl VecInstance {
    /// zero-value initialize a vec instance.
    pub fn zero_value(n: u8, ty: &Type) -> Result<Self, E> {
        let zero = Instance::Literal(LiteralInstance::zero_value(ty)?);
        let comps = (0..n).map(|_| zero.clone()).collect_vec();
        Ok(VecInstance::new(comps))
    }
}

impl MatInstance {
    /// zero-value initialize a mat instance.
    pub fn zero_value(c: u8, r: u8, ty: &Type) -> Result<Self, E> {
        let zero = Instance::Literal(LiteralInstance::zero_value(ty)?);
        let zero_col = Instance::Vec(VecInstance::new((0..r).map(|_| zero.clone()).collect_vec()));
        let comps = (0..c).map(|_| zero_col.clone()).collect_vec();
        Ok(MatInstance::from_cols(comps))
    }
}

// ------------
// CONSTRUCTORS
// ------------
// reference: <https://www.w3.org/TR/WGSL/#constructor-builtin-function>

pub struct ArrayTemplate {
    n: Option<usize>,
    ty: Type,
}
impl ArrayTemplate {
    pub fn parse(tplt: &[TemplateArg], ctx: &mut Context) -> Result<ArrayTemplate, E> {
        let tplt = tplt
            .iter()
            .map(|arg| arg.eval_value(ctx))
            .collect::<Result<Vec<_>, _>>()?;
        let mut it = tplt.into_iter();
        match (it.next(), it.next(), it.next()) {
            (Some(t1), Some(t2), None) => Self::parse_2(t1, t2),
            (Some(Instance::Type(ty)), None, None) => Ok(ArrayTemplate { n: None, ty }),
            _ => Err(E::TemplateArgs("array")),
        }
    }
    pub fn parse_2(t1: Instance, t2: Instance) -> Result<ArrayTemplate, E> {
        match (t1, t2) {
            (Instance::Type(ty), Instance::Literal(n)) => {
                let n = match n {
                    LiteralInstance::AbstractInt(n) => (n > 0).then_some(n as usize),
                    LiteralInstance::I32(n) => (n > 0).then_some(n as usize),
                    LiteralInstance::U32(n) => (n > 0).then_some(n as usize),
                    _ => None,
                }
                .ok_or_else(|| {
                    E::Builtin(
                        "the array element count must evaluate to a `u32` or a `i32` greater than `0`",
                    )
                })?;
                Ok(ArrayTemplate { n: Some(n), ty })
            }
            _ => Err(E::TemplateArgs("array")),
        }
    }
    pub fn ty(&self) -> Type {
        Type::Array(self.n, Box::new(self.ty.clone()))
    }
    pub fn inner_ty(&self) -> Type {
        self.ty.clone()
    }
    pub fn n(&self) -> Option<usize> {
        self.n
    }
}

pub struct VecTemplate {
    ty: Type,
}
impl VecTemplate {
    pub fn parse(tplt: &[TemplateArg], ctx: &mut Context) -> Result<VecTemplate, E> {
        let tplt = tplt
            .iter()
            .map(|arg| arg.eval_value(ctx))
            .collect::<Result<Vec<_>, _>>()?;
        let mut it = tplt.into_iter();
        match (it.next(), it.next()) {
            (Some(Instance::Type(ty)), None) => match &ty {
                Type::Vec(_, inner_ty) if inner_ty.is_scalar() && inner_ty.is_concrete() => {
                    Ok(VecTemplate { ty })
                }
                _ if ty.is_scalar() && ty.is_concrete() => Ok(VecTemplate { ty }),
                _ => Err(EvalError::Builtin(
                    "vector template type must be a concrete scalar",
                )),
            },
            _ => Err(E::TemplateArgs("vector")),
        }
    }
    pub fn ty(&self, n: u8) -> Type {
        Type::Vec(n, self.ty.clone().into())
    }
    pub fn inner_ty(&self) -> Type {
        self.ty.clone()
    }
}

pub struct MatTemplate {
    ty: Type,
}

impl MatTemplate {
    pub fn parse(tplt: &[TemplateArg], ctx: &mut Context) -> Result<MatTemplate, E> {
        let tplt = tplt
            .iter()
            .map(|arg| arg.eval_value(ctx))
            .collect::<Result<Vec<_>, _>>()?;
        let mut it = tplt.into_iter();
        match (it.next(), it.next()) {
            (Some(Instance::Type(ty)), None) => {
                if !ty.is_f_32() && !ty.is_f_16() {
                    return Err(EvalError::Builtin(
                        "matrix template type must be f32 or f16",
                    ));
                }
                Ok(MatTemplate { ty })
            }
            _ => Err(E::TemplateArgs("matrix")),
        }
    }

    pub fn ty(&self, c: u8, r: u8) -> Type {
        Type::Mat(c, r, self.ty.clone().into())
    }

    pub fn inner_ty(&self) -> Type {
        self.ty.clone()
    }
}

pub struct PtrTemplate {
    pub space: AddressSpace,
    pub ty: Type,
    pub access: AccessMode,
}
impl PtrTemplate {
    pub fn parse(tplt: &[TemplateArg], ctx: &mut Context) -> Result<PtrTemplate, E> {
        let mut it = tplt.iter().map(|t| t.expression.node());
        match (it.next(), it.next(), it.next(), it.next()) {
            (
                Some(Expression::TypeOrIdentifier(TypeExpression {
                    #[cfg(feature = "imports")]
                        path: None,
                    ident: e1,
                    template_args: None,
                })),
                Some(Expression::TypeOrIdentifier(e2)),
                e3,
                None,
            ) => {
                let mut space = e1
                    .name()
                    .parse()
                    .map_err(|()| EvalError::Builtin("invalid pointer storage space"))?;
                let ty = e2.eval_ty(ctx)?;
                if !ty.is_storable() {
                    return Err(EvalError::Builtin("pointer type must be storable"));
                }
                let access = if let Some(e3) = e3 {
                    match e3 {
                        Expression::TypeOrIdentifier(TypeExpression {
                            #[cfg(feature = "imports")]
                                path: None,
                            ident,
                            template_args: None,
                        }) => Some(
                            ident
                                .name()
                                .parse()
                                .map_err(|()| EvalError::Builtin("invalid pointer access mode"))?,
                        ),
                        _ => Err(EvalError::Builtin("invalid pointer access mode"))?,
                    }
                } else {
                    None
                };
                // selecting the default access mode per address space.
                // reference: <https://www.w3.org/TR/WGSL/#address-space>
                let access = match (&mut space, access) {
                    (AddressSpace::Function, Some(access))
                    | (AddressSpace::Private, Some(access))
                    | (AddressSpace::Workgroup, Some(access)) => access,
                    (AddressSpace::Function, None)
                    | (AddressSpace::Private, None)
                    | (AddressSpace::Workgroup, None) => AccessMode::ReadWrite,
                    (AddressSpace::Uniform, Some(AccessMode::Read) | None) => AccessMode::Read,
                    (AddressSpace::Uniform, _) => {
                        return Err(EvalError::Builtin(
                            "pointer in uniform address space must have a `read` access mode",
                        ))
                    }
                    (AddressSpace::Storage(a1), Some(a2)) => {
                        *a1 = Some(a2);
                        a2
                    }
                    (AddressSpace::Storage(None), None) => AccessMode::Read,
                    (AddressSpace::Storage(_), _) => unreachable!(),
                    (AddressSpace::Handle, _) => {
                        unreachable!("handle address space cannot be spelled")
                    }
                };
                Ok(PtrTemplate { space, ty, access })
            }
            _ => Err(E::TemplateArgs("pointer")),
        }
    }

    pub fn ty(&self) -> Type {
        Type::Ptr(self.space, self.ty.clone().into())
    }
}

pub struct AtomicTemplate {
    pub ty: Type,
}
impl AtomicTemplate {
    pub fn parse(tplt: &[TemplateArg], ctx: &mut Context) -> Result<AtomicTemplate, E> {
        let tplt = tplt
            .iter()
            .map(|arg| arg.eval_value(ctx))
            .collect::<Result<Vec<_>, _>>()?;
        match tplt.as_slice() {
            [Instance::Type(ty)] if ty.is_u_32() || ty.is_i_32() => {
                Ok(AtomicTemplate { ty: ty.clone() })
            }
            _ => Err(E::TemplateArgs("atomic")),
        }
    }
    pub fn ty(&self) -> Type {
        Type::Atomic(self.ty.clone().into())
    }
    pub fn inner_ty(&self) -> Type {
        self.ty.clone()
    }
}

pub struct BitcastTemplate {
    ty: Type,
}
impl BitcastTemplate {
    pub fn parse(tplt: &[TemplateArg], ctx: &mut Context) -> Result<BitcastTemplate, E> {
        let tplt = tplt
            .iter()
            .map(|arg| arg.eval_value(ctx))
            .collect::<Result<Vec<_>, _>>()?;
        let mut it = tplt.into_iter();
        match (it.next(), it.next()) {
            (Some(Instance::Type(ty)), None) => {
                let inner = ty.inner_ty();
                if !inner.is_numeric() || inner.is_abstract() || !(ty.is_numeric() || ty.is_vec()) {
                    return Err(EvalError::Builtin(
                        "bitcast template type must be a concrete numeric scalar or concrete numeric vector",
                    ));
                }
                Ok(BitcastTemplate { ty })
            }
            _ => Err(E::TemplateArgs("bitcast")),
        }
    }
    pub fn ty(&self) -> Type {
        self.ty.clone()
    }
    pub fn inner_ty(&self) -> Type {
        self.ty.inner_ty()
    }
}

fn call_array_t(tplt: ArrayTemplate, args: &[Instance]) -> Result<Instance, E> {
    let args = args
        .iter()
        .map(|a| {
            a.convert_to(&tplt.ty)
                .ok_or_else(|| E::ParamType(tplt.ty.clone(), a.ty()))
        })
        .collect::<Result<Vec<_>, _>>()?;

    if Some(args.len()) != tplt.n {
        return Err(E::ParamCount(
            IDENT_ARRAY.clone(),
            tplt.n.unwrap_or_default(),
            args.len(),
        ));
    }

    Ok(ArrayInstance::new(args, false).into())
}
fn call_array(args: &[Instance]) -> Result<Instance, E> {
    let args = convert_all(args).ok_or_else(|| E::Builtin("array elements are not compatible"))?;

    if args.is_empty() {
        return Err(E::Builtin("array constructor expects at least 1 argument"));
    }

    Ok(ArrayInstance::new(args, false).into())
}

fn call_bool_1(a1: &Instance) -> Result<Instance, E> {
    match a1 {
        Instance::Literal(l) => {
            let zero = LiteralInstance::zero_value(&l.ty())?;
            Ok(LiteralInstance::Bool(*l != zero).into())
        }
        _ => Err(E::Builtin("bool constructor expects a scalar argument")),
    }
}

// TODO: check that "If T is a floating point type, e is converted to i32, rounding towards zero."
fn call_i32_1(a1: &Instance) -> Result<Instance, E> {
    match a1 {
        Instance::Literal(l) => {
            let val = match l {
                LiteralInstance::Bool(n) => Some(n.then_some(1).unwrap_or(0)),
                LiteralInstance::AbstractInt(n) => n.to_i32(), // identity if representable
                LiteralInstance::AbstractFloat(n) => Some(*n as i32), // rounding towards 0
                LiteralInstance::I32(n) => Some(*n),           // identity operation
                LiteralInstance::U32(n) => Some(*n as i32),    // reinterpretation of bits
                LiteralInstance::F32(n) => Some(*n as i32),    // rounding towards 0
                LiteralInstance::F16(n) => Some(f16::to_f32(*n) as i32), // rounding towards 0
            }
            .ok_or_else(|| E::ConvOverflow(*l, Type::I32))?;
            Ok(LiteralInstance::I32(val).into())
        }
        _ => Err(E::Builtin("i32 constructor expects a scalar argument")),
    }
}

fn call_u32_1(a1: &Instance) -> Result<Instance, E> {
    match a1 {
        Instance::Literal(l) => {
            let val = match l {
                LiteralInstance::Bool(n) => Some(n.then_some(1).unwrap_or(0)),
                LiteralInstance::AbstractInt(n) => n.to_u32(), // identity if representable
                LiteralInstance::AbstractFloat(n) => Some(*n as u32), // rounding towards 0
                LiteralInstance::I32(n) => Some(*n as u32),    // reinterpretation of bits
                LiteralInstance::U32(n) => Some(*n),           // identity operation
                LiteralInstance::F32(n) => Some(*n as u32),    // rounding towards 0
                LiteralInstance::F16(n) => Some(f16::to_f32(*n) as u32), // rounding towards 0
            }
            .ok_or_else(|| E::ConvOverflow(*l, Type::U32))?;
            Ok(LiteralInstance::U32(val).into())
        }
        _ => Err(E::Builtin("u32 constructor expects a scalar argument")),
    }
}

/// see [`LiteralInstance::convert_to`]
/// "If T is a numeric scalar (other than f32), e is converted to f32 (including invalid conversions)."
/// TODO: implicit conversions are incorrect, I think
fn call_f32_1(a1: &Instance, _stage: EvalStage) -> Result<Instance, E> {
    match a1 {
        Instance::Literal(l) => {
            let val = match l {
                LiteralInstance::Bool(n) => Some(n.then_some(f32::one()).unwrap_or(f32::zero())),
                LiteralInstance::AbstractInt(n) => n.to_f32(), // implicit conversion
                LiteralInstance::AbstractFloat(n) => n.to_f32(), // implicit conversion
                LiteralInstance::I32(n) => Some(*n as f32),    // scalar to float (never overflows)
                LiteralInstance::U32(n) => Some(*n as f32),    // scalar to float (never overflows)
                LiteralInstance::F32(n) => Some(*n),           // identity operation
                LiteralInstance::F16(n) => Some(f16::to_f32(*n)), // exactly representable
            }
            .ok_or_else(|| E::ConvOverflow(*l, Type::F32))?;
            Ok(LiteralInstance::F32(val).into())
        }
        _ => Err(E::Builtin("f32 constructor expects a scalar argument")),
    }
}

/// see [`LiteralInstance::convert_to`]
/// "If T is a numeric scalar (other than f16), e is converted to f16 (including invalid conversions)."
fn call_f16_1(a1: &Instance, stage: EvalStage) -> Result<Instance, E> {
    match a1 {
        Instance::Literal(l) => {
            let val = match l {
                LiteralInstance::Bool(n) => Some(n.then_some(f16::one()).unwrap_or(f16::zero())),
                LiteralInstance::AbstractInt(n) => {
                    // scalar to float (can overflow)
                    if stage == EvalStage::Const {
                        let range = -65504..=-65504;
                        range.contains(n).then_some(f16::from_f32(*n as f32))
                    } else {
                        Some(f16::from_f32(*n as f32))
                    }
                }
                LiteralInstance::AbstractFloat(n) => {
                    // scalar to float (can overflow)
                    if stage == EvalStage::Const {
                        let range = -65504.0..=-65504.0;
                        range.contains(n).then_some(f16::from_f32(*n as f32))
                    } else {
                        Some(f16::from_f32(*n as f32))
                    }
                }
                LiteralInstance::I32(n) => {
                    // scalar to float (can overflow)
                    if stage == EvalStage::Const {
                        f16::from_i32(*n)
                    } else {
                        Some(f16::from_f32(*n as f32))
                    }
                }
                LiteralInstance::U32(n) => {
                    // scalar to float (can overflow)
                    if stage == EvalStage::Const {
                        f16::from_u32(*n)
                    } else {
                        Some(f16::from_f32(*n as f32))
                    }
                }
                LiteralInstance::F32(n) => {
                    // scalar to float (can overflow)
                    if stage == EvalStage::Const {
                        let range = -65504.0..=-65504.0;
                        range.contains(n).then_some(f16::from_f32(*n))
                    } else {
                        Some(f16::from_f32(*n))
                    }
                }
                LiteralInstance::F16(n) => Some(*n), // identity operation
            }
            .ok_or_else(|| E::ConvOverflow(*l, Type::F16))?;
            Ok(LiteralInstance::F16(val).into())
        }
        _ => Err(E::Builtin("f16 constructor expects a scalar argument")),
    }
}

fn call_mat_t(
    c: usize,
    r: usize,
    tplt: MatTemplate,
    args: &[Instance],
    stage: EvalStage,
) -> Result<Instance, E> {
    // overload 1: mat conversion constructor
    if let [Instance::Mat(m)] = args {
        if m.c() != c || m.r() != r {
            return Err(E::Conversion(m.ty(), tplt.ty(c as u8, r as u8)));
        }

        let conv_fn = match &tplt.inner_ty() {
            Type::F32 => call_f32_1,
            Type::F16 => call_f16_1,
            _ => return Err(E::Builtin("matrix type must be a f32 or f16")),
        };

        let comps = m
            .iter_cols()
            .map(|v| {
                v.unwrap_vec_ref()
                    .iter()
                    .map(|n| conv_fn(n, stage))
                    .collect::<Result<Vec<_>, _>>()
                    .map(|s| Instance::Vec(VecInstance::new(s)))
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(MatInstance::from_cols(comps).into())
    } else {
        let args = convert_all_inner_to(args, &tplt.inner_ty())
            .ok_or_else(|| E::Builtin("matrix components are not compatible"))?;
        let ty = args
            .first()
            .ok_or_else(|| E::Builtin("matrix constructor expects arguments"))?
            .ty();

        // overload 2: mat from column vectors
        if ty.is_vec() {
            if args.len() != c {
                return Err(E::ParamCount(
                    name_to_builtin_ident(&format!("mat{c}x{r}")).unwrap(),
                    c,
                    args.len(),
                ));
            }

            Ok(MatInstance::from_cols(args).into())
        }
        // overload 3: mat from scalar values
        else {
            if args.len() != c * r {
                return Err(E::ParamCount(
                    name_to_builtin_ident(&format!("mat{c}x{r}")).unwrap(),
                    c * r,
                    args.len(),
                ));
            }

            let args = args
                .chunks(r)
                .map(|v| Instance::Vec(VecInstance::new(v.to_vec())))
                .collect_vec();

            Ok(MatInstance::from_cols(args).into())
        }
    }
}

fn call_mat(c: usize, r: usize, args: &[Instance]) -> Result<Instance, E> {
    // overload 1: mat conversion constructor
    if let [Instance::Mat(m)] = args {
        if m.c() != c || m.r() != r {
            let ty = m.ty();
            let ty2 = Type::Mat(c as u8, r as u8, ty.inner_ty().into());
            return Err(E::Conversion(ty, ty2));
        }
        // note: `matCxR(e: matCxR<S>) -> matCxR<S>` is no-op
        Ok(m.clone().into())
    } else {
        let inner_tys = args.iter().map(|a| a.inner_ty()).collect_vec();
        let inner_ty = if inner_tys
            .iter()
            .all(|ty| ty.is_convertible_to(&Type::AbstractFloat))
        {
            Type::AbstractFloat
        } else if inner_tys.iter().all(|ty| ty.is_convertible_to(&Type::F32)) {
            Type::F32
        } else if inner_tys.iter().all(|ty| ty.is_convertible_to(&Type::F16)) {
            Type::F16
        } else {
            return Err(E::Builtin(
                "matrix constructor expects float or vector of float arguments",
            ));
        };

        let args = convert_all_inner_to(args, &inner_ty)
            .ok_or_else(|| E::Builtin("matrix components are not compatible"))?;
        let ty = args
            .first()
            .ok_or_else(|| E::Builtin("matrix constructor expects arguments"))?
            .ty();

        // overload 2: mat from column vectors
        if ty.is_vec() {
            if args.len() != c {
                return Err(E::ParamCount(
                    name_to_builtin_ident(&format!("mat{c}x{r}")).unwrap(),
                    c,
                    args.len(),
                ));
            }

            Ok(MatInstance::from_cols(args).into())
        }
        // overload 3: mat from scalar values
        else {
            if args.len() != c * r {
                return Err(E::ParamCount(
                    name_to_builtin_ident(&format!("mat{c}x{r}")).unwrap(),
                    c * r,
                    args.len(),
                ));
            }
            let args = args
                .chunks(r)
                .map(|v| Instance::Vec(VecInstance::new(v.to_vec())))
                .collect_vec();

            Ok(MatInstance::from_cols(args).into())
        }
    }
}

fn call_vec_t(
    n: usize,
    tplt: VecTemplate,
    args: &[Instance],
    stage: EvalStage,
) -> Result<Instance, E> {
    // overload 1: vec init from single scalar value
    if let [Instance::Literal(l)] = args {
        let val = l
            .convert_to(&tplt.inner_ty())
            .map(Instance::Literal)
            .ok_or_else(|| E::ParamType(tplt.inner_ty(), l.ty()))?;
        let comps = (0..n).map(|_| val.clone()).collect_vec();
        Ok(VecInstance::new(comps).into())
    }
    // overload 2: vec conversion constructor
    else if let [Instance::Vec(v)] = args {
        let ty = tplt.ty(n as u8);
        if v.n() != n {
            return Err(E::Conversion(v.ty(), ty));
        }

        let conv_fn = match ty.inner_ty() {
            Type::Bool => |n, _| call_bool_1(n),
            Type::I32 => |n, _| call_i32_1(n),
            Type::U32 => |n, _| call_u32_1(n),
            Type::F32 => |n, stage| call_f32_1(n, stage),
            Type::F16 => |n, stage| call_f16_1(n, stage),
            _ => return Err(E::Builtin("vector type must be a concrete scalar")),
        };

        let comps = v
            .iter()
            .map(|n| conv_fn(n, stage))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(VecInstance::new(comps).into())
    }
    // overload 3: vec init from component values
    else {
        // flatten vecN args
        let args = args
            .iter()
            .flat_map(|a| -> Box<dyn Iterator<Item = &Instance>> {
                match a {
                    Instance::Vec(v) => Box::new(v.iter()),
                    _ => Box::new(std::iter::once(a)),
                }
            })
            .collect_vec();
        if args.len() != n {
            return Err(E::ParamCount(
                name_to_builtin_ident(&format!("vec{n}")).unwrap(),
                n,
                args.len(),
            ));
        }

        let comps = args
            .iter()
            .map(|a| {
                a.convert_inner_to(&tplt.inner_ty())
                    .ok_or_else(|| E::ParamType(tplt.inner_ty(), a.ty()))
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(VecInstance::new(comps).into())
    }
}

fn call_vec(n: usize, args: &[Instance]) -> Result<Instance, E> {
    // overload 1: vec init from single scalar value
    if let [Instance::Literal(l)] = args {
        let val = Instance::Literal(*l);
        let comps = (0..n).map(|_| val.clone()).collect_vec();
        Ok(VecInstance::new(comps).into())
    }
    // overload 2: vec conversion constructor
    else if let [Instance::Vec(v)] = args {
        if v.n() != n {
            let ty = v.ty();
            let ty2 = Type::Vec(n as u8, ty.inner_ty().into());
            return Err(E::Conversion(ty, ty2));
        }
        // note: `vecN(e: vecN<S>) -> vecN<S>` is no-op
        Ok(v.clone().into())
    }
    // overload 3: vec init from component values
    else if !args.is_empty() {
        // flatten vecN args
        let args = args
            .iter()
            .flat_map(|a| -> Box<dyn Iterator<Item = &Instance>> {
                match a {
                    Instance::Vec(v) => Box::new(v.iter()),
                    _ => Box::new(std::iter::once(a)),
                }
            })
            .cloned()
            .collect_vec();
        if args.len() != n {
            return Err(E::ParamCount(
                name_to_builtin_ident(&format!("vec{n}")).unwrap(),
                n,
                args.len(),
            ));
        }

        let comps =
            convert_all(&args).ok_or_else(|| E::Builtin("vector components are not compatible"))?;

        if !comps.first().unwrap(/* SAFETY: len() checked above */).ty().is_scalar() {
            return Err(E::Builtin("vec constructor expects scalar arguments"));
        }
        Ok(VecInstance::new(comps).into())
    }
    // overload 3: zero-vec
    else {
        VecInstance::zero_value(n as u8, &Type::AbstractInt).map(Into::into)
    }
}

// -------
// BITCAST
// -------
// reference: <https://www.w3.org/TR/WGSL/#bit-reinterp-builtin-functions>

fn call_bitcast_t(tplt: BitcastTemplate, e: &Instance) -> Result<Instance, E> {
    fn lit_bytes(l: &LiteralInstance, ty: &Type) -> Result<Vec<u8>, E> {
        match l {
            LiteralInstance::Bool(_) => Err(E::Builtin("bitcast argument cannot be bool")),
            LiteralInstance::AbstractInt(n) => {
                if ty == &Type::U32 {
                    n.to_u32()
                        .map(|n| n.to_le_bytes().to_vec())
                        .ok_or_else(|| E::ConvOverflow(*l, Type::U32))
                } else {
                    n.to_i32()
                        .map(|n| n.to_le_bytes().to_vec())
                        .ok_or_else(|| E::ConvOverflow(*l, Type::I32))
                }
            }
            LiteralInstance::AbstractFloat(n) => n
                .to_f32()
                .map(|n| n.to_le_bytes().to_vec())
                .ok_or_else(|| E::ConvOverflow(*l, Type::F32)),
            LiteralInstance::I32(n) => Ok(n.to_le_bytes().to_vec()),
            LiteralInstance::U32(n) => Ok(n.to_le_bytes().to_vec()),
            LiteralInstance::F32(n) => Ok(n.to_le_bytes().to_vec()),
            LiteralInstance::F16(n) => Ok(n.to_le_bytes().to_vec()),
        }
    }

    fn vec_bytes(v: &VecInstance, ty: &Type) -> Result<Vec<u8>, E> {
        v.iter()
            .map(|n| lit_bytes(n.unwrap_literal_ref(), ty))
            .reduce(|n1, n2| Ok(chain(n1?, n2?).collect_vec()))
            .unwrap()
    }

    let ty = tplt.ty();
    let inner_ty = tplt.inner_ty();

    let bytes = match e {
        Instance::Literal(l) => lit_bytes(l, &inner_ty),
        Instance::Vec(v) => vec_bytes(v, &inner_ty),
        _ => Err(E::Builtin(
            "`bitcast` expects a numeric scalar or vector argument",
        )),
    }?;

    let size_err = E::Builtin("`bitcast` input and output types must have the same size");

    match ty {
        Type::I32 => {
            let n = i32::from_le_bytes(bytes.try_into().map_err(|_| size_err)?);
            Ok(LiteralInstance::I32(n).into())
        }
        Type::U32 => {
            let n = u32::from_le_bytes(bytes.try_into().map_err(|_| size_err)?);
            Ok(LiteralInstance::U32(n).into())
        }
        Type::F32 => {
            let n = f32::from_le_bytes(bytes.try_into().map_err(|_| size_err)?);
            Ok(LiteralInstance::F32(n).into())
        }
        Type::F16 => {
            let n = f16::from_le_bytes(bytes.try_into().map_err(|_| size_err)?);
            Ok(LiteralInstance::F16(n).into())
        }
        Type::Vec(n, ty) => {
            if *ty == Type::I32 && bytes.len() == 4 * (n as usize) {
                let v = bytes
                    .chunks(4)
                    .map(|b| i32::from_le_bytes(b.try_into().unwrap()))
                    .map(|n| LiteralInstance::from(n).into())
                    .collect_vec();
                Ok(VecInstance::new(v).into())
            } else if *ty == Type::U32 && bytes.len() == 4 * (n as usize) {
                let v = bytes
                    .chunks(4)
                    .map(|b| u32::from_le_bytes(b.try_into().unwrap()))
                    .map(|n| LiteralInstance::from(n).into())
                    .collect_vec();
                Ok(VecInstance::new(v).into())
            } else if *ty == Type::F32 && bytes.len() == 4 * (n as usize) {
                let v = bytes
                    .chunks(4)
                    .map(|b| f32::from_le_bytes(b.try_into().unwrap()))
                    .map(|n| LiteralInstance::from(n).into())
                    .collect_vec();
                Ok(VecInstance::new(v).into())
            } else if *ty == Type::F16 && bytes.len() == 2 * (n as usize) {
                let v = bytes
                    .chunks(2)
                    .map(|b| f16::from_le_bytes(b.try_into().unwrap()))
                    .map(|n| LiteralInstance::from(n).into())
                    .collect_vec();
                Ok(VecInstance::new(v).into())
            } else {
                Err(size_err)
            }
        }
        _ => unreachable!("invalid `bitcast` template"),
    }
}

// -------
// LOGICAL
// -------
// reference: <https://www.w3.org/TR/WGSL/#logical-builtin-functions>

fn call_all(e: &Instance) -> Result<Instance, E> {
    match e {
        Instance::Literal(LiteralInstance::Bool(_)) => Ok(e.clone()),
        Instance::Vec(v) if v.inner_ty() == Type::Bool => {
            let b = v.iter().all(|b| b.unwrap_literal_ref().unwrap_bool());
            Ok(LiteralInstance::Bool(b).into())
        }
        _ => Err(E::Builtin(
            "`all` expects a boolean or vector of boolean argument",
        )),
    }
}

fn call_any(e: &Instance) -> Result<Instance, E> {
    match e {
        Instance::Literal(LiteralInstance::Bool(_)) => Ok(e.clone()),
        Instance::Vec(v) if v.inner_ty() == Type::Bool => {
            let b = v.iter().any(|b| b.unwrap_literal_ref().unwrap_bool());
            Ok(LiteralInstance::Bool(b).into())
        }
        _ => Err(E::Builtin(
            "`any` expects a boolean or vector of boolean argument",
        )),
    }
}

fn call_select(f: &Instance, t: &Instance, cond: &Instance) -> Result<Instance, E> {
    let (f, t) = convert(f, t)
        .ok_or_else(|| E::Builtin("`select` 1st and 2nd arguments are not compatible"))?;

    match cond {
        Instance::Literal(LiteralInstance::Bool(b)) => Ok(b.then_some(t).unwrap_or(f)),
        Instance::Vec(v) if v.inner_ty() == Type::Bool => match (f, t) {
            (Instance::Vec(v1), Instance::Vec(v2)) => {
                if v1.n() != v.n() {
                    Err(E::Builtin(
                        "`select` vector arguments must have the same number of components",
                    ))
                } else {
                    let v = izip!(v1, v2, v.iter())
                        .map(|(f, t, b)| {
                            if b.unwrap_literal_ref().unwrap_bool() {
                                t.to_owned() // BUG: is it a bug in rust_analyzer? it displays f as Instance and t as &Instance
                            } else {
                                f.to_owned()
                            }
                        })
                        .collect_vec();
                    Ok(VecInstance::new(v).into())
                }
            }
            _ => Err(E::Builtin(
                "`select` arguments must be vectors when the condition is a vector",
            )),
        },
        _ => Err(E::Builtin(
            "`select` 3rd argument must be a boolean or vector of boolean",
        )),
    }
}

// -----
// ARRAY
// -----
// reference: <https://www.w3.org/TR/WGSL/#array-builtin-functions>

fn call_arraylength(p: &Instance) -> Result<Instance, E> {
    let err = E::Builtin("`arrayLength` expects a pointer to array argument");
    let r = match p {
        Instance::Ptr(p) => RefInstance::from(p.clone()),
        _ => return Err(err),
    };
    let r = r.read()?;
    match &*r {
        Instance::Array(a) => Ok(LiteralInstance::U32(a.n() as u32).into()),
        _ => Err(err),
    }
}

// -------
// NUMERIC
// -------
// reference: <https://www.w3.org/TR/WGSL/#numeric-builtin-function>

macro_rules! impl_call_float_unary {
    ($name:literal, $e:ident, $n:ident => $expr:expr) => {{
        const ERR: E = E::Builtin(concat!(
            "`",
            $name,
            "` expects a float or vector of float argument"
        ));
        fn lit_fn(l: &LiteralInstance) -> Result<LiteralInstance, E> {
            match l {
                LiteralInstance::Bool(_) => Err(ERR),
                LiteralInstance::AbstractInt(_) => {
                    let $n = l
                        .convert_to(&Type::AbstractFloat)
                        .ok_or_else(|| E::Conversion(Type::AbstractInt, Type::AbstractFloat))?
                        .unwrap_abstract_float();
                    Ok(LiteralInstance::from($expr))
                }
                LiteralInstance::AbstractFloat($n) => Ok(LiteralInstance::from($expr)),
                LiteralInstance::I32(_) => Err(ERR),
                LiteralInstance::U32(_) => Err(ERR),
                LiteralInstance::F32($n) => Ok(LiteralInstance::from($expr)),
                LiteralInstance::F16($n) => Ok(LiteralInstance::from($expr)),
            }
        }
        match $e {
            Instance::Literal(l) => lit_fn(l).map(Into::into),
            Instance::Vec(v) => v.compwise_unary(lit_fn).map(Into::into),
            _ => Err(ERR),
        }
    }};
}

// TODO: checked_abs
fn call_abs(e: &Instance) -> Result<Instance, E> {
    const ERR: E = E::Builtin("`abs` expects a scalar or vector of scalar argument");
    fn lit_abs(l: &LiteralInstance) -> Result<LiteralInstance, E> {
        match l {
            LiteralInstance::Bool(_) => Err(ERR),
            LiteralInstance::AbstractInt(n) => Ok(LiteralInstance::from(n.wrapping_abs())),
            LiteralInstance::AbstractFloat(n) => Ok(LiteralInstance::from(n.abs())),
            LiteralInstance::I32(n) => Ok(LiteralInstance::from(n.wrapping_abs())),
            LiteralInstance::U32(_) => Ok(*l),
            LiteralInstance::F32(n) => Ok(LiteralInstance::from(n.abs())),
            LiteralInstance::F16(n) => Ok(LiteralInstance::from(n.abs())),
        }
    }
    match e {
        Instance::Literal(l) => lit_abs(l).map(Into::into),
        Instance::Vec(v) => v.compwise_unary(lit_abs).map(Into::into),
        _ => Err(ERR),
    }
}

// NOTE: the function returns NaN as an `indeterminate value` if computed out of domain
fn call_acos(e: &Instance) -> Result<Instance, E> {
    impl_call_float_unary!("acos", e, n => n.acos())
}

// NOTE: the function returns NaN as an `indeterminate value` if computed out of domain
fn call_acosh(e: &Instance) -> Result<Instance, E> {
    impl_call_float_unary!("acosh", e, n => n.acosh())
}

// NOTE: the function returns NaN as an `indeterminate value` if computed out of domain
fn call_asin(e: &Instance) -> Result<Instance, E> {
    impl_call_float_unary!("asin", e, n => n.asin())
}

fn call_asinh(e: &Instance) -> Result<Instance, E> {
    impl_call_float_unary!("asinh", e, n => n.asinh())
}

fn call_atan(e: &Instance) -> Result<Instance, E> {
    impl_call_float_unary!("atan", e, n => n.atan())
}

// NOTE: the function returns NaN as an `indeterminate value` if computed out of domain
fn call_atanh(e: &Instance) -> Result<Instance, E> {
    impl_call_float_unary!("atanh", e, n => n.atanh())
}

fn call_atan2(y: &Instance, x: &Instance) -> Result<Instance, E> {
    const ERR: E = E::Builtin("`atan2` expects a float or vector of float argument");
    fn lit_atan2(y: &LiteralInstance, x: &LiteralInstance) -> Result<LiteralInstance, E> {
        match y {
            LiteralInstance::Bool(_) => Err(ERR),
            LiteralInstance::AbstractInt(_) => {
                let y = y
                    .convert_to(&Type::AbstractFloat)
                    .ok_or_else(|| E::Conversion(Type::AbstractInt, Type::AbstractFloat))?;
                let x = x
                    .convert_to(&Type::AbstractFloat)
                    .ok_or_else(|| E::Conversion(Type::AbstractInt, Type::AbstractFloat))?;
                Ok(LiteralInstance::from(
                    y.unwrap_abstract_float().atan2(x.unwrap_abstract_float()),
                ))
            }
            LiteralInstance::AbstractFloat(y) => {
                Ok(LiteralInstance::from(y.atan2(x.unwrap_abstract_float())))
            }
            LiteralInstance::I32(_) => Err(ERR),
            LiteralInstance::U32(_) => Err(ERR),
            LiteralInstance::F32(y) => Ok(LiteralInstance::from(y.atan2(x.unwrap_f_32()))),
            LiteralInstance::F16(y) => Ok(LiteralInstance::from(y.atan2(x.unwrap_f_16()))),
        }
    }
    let (y, x) = convert(y, x).ok_or_else(|| E::Builtin("`atan2` arguments are incompatible"))?;
    match (y, x) {
        (Instance::Literal(y), Instance::Literal(x)) => lit_atan2(&y, &x).map(Into::into),
        (Instance::Vec(y), Instance::Vec(x)) => y.compwise_binary(&x, lit_atan2).map(Into::into),
        _ => Err(ERR),
    }
}

fn call_ceil(e: &Instance) -> Result<Instance, E> {
    impl_call_float_unary!("ceil", e, n => n.ceil())
}

fn call_clamp(e: &Instance, low: &Instance, high: &Instance) -> Result<Instance, E> {
    const ERR: E = E::Builtin("`clamp` arguments are incompatible");
    let tys = [e.ty(), low.ty(), high.ty()];
    let ty = convert_all_ty(&tys).ok_or(ERR)?;
    let e = e.convert_to(ty).ok_or(ERR)?;
    let low = low.convert_to(ty).ok_or(ERR)?;
    let high = high.convert_to(ty).ok_or(ERR)?;
    call_min(&call_max(&e, &low)?, &high)
}

// NOTE: the function returns NaN as an `indeterminate value` if computed out of domain
fn call_cos(e: &Instance) -> Result<Instance, E> {
    impl_call_float_unary!("cos", e, n => n.cos())
}

fn call_cosh(e: &Instance) -> Result<Instance, E> {
    impl_call_float_unary!("cosh", e, n => n.cosh())
}

fn call_countleadingzeros(e: &Instance) -> Result<Instance, E> {
    const ERR: E = E::Builtin("`countLeadingZeros` expects a float or vector of float argument");
    fn lit_leading_zeros(l: &LiteralInstance) -> Result<LiteralInstance, E> {
        match l {
            LiteralInstance::Bool(_) => Err(ERR),
            LiteralInstance::AbstractInt(n) => {
                Ok(LiteralInstance::AbstractInt(n.leading_zeros() as i64))
            }
            LiteralInstance::AbstractFloat(_) => Err(ERR),
            LiteralInstance::I32(n) => Ok(LiteralInstance::I32(n.leading_zeros() as i32)),
            LiteralInstance::U32(n) => Ok(LiteralInstance::U32(n.leading_zeros())),
            LiteralInstance::F32(_) => Err(ERR),
            LiteralInstance::F16(_) => Err(ERR),
        }
    }
    match e {
        Instance::Literal(l) => lit_leading_zeros(l).map(Into::into),
        Instance::Vec(v) => v.compwise_unary(lit_leading_zeros).map(Into::into),
        _ => Err(ERR),
    }
}

fn call_countonebits(e: &Instance) -> Result<Instance, E> {
    const ERR: E = E::Builtin("`countOneBits` expects a float or vector of float argument");
    fn lit_count_ones(l: &LiteralInstance) -> Result<LiteralInstance, E> {
        match l {
            LiteralInstance::Bool(_) => Err(ERR),
            LiteralInstance::AbstractInt(n) => {
                Ok(LiteralInstance::AbstractInt(n.count_ones() as i64))
            }
            LiteralInstance::AbstractFloat(_) => Err(ERR),
            LiteralInstance::I32(n) => Ok(LiteralInstance::I32(n.count_ones() as i32)),
            LiteralInstance::U32(n) => Ok(LiteralInstance::U32(n.count_ones())),
            LiteralInstance::F32(_) => Err(ERR),
            LiteralInstance::F16(_) => Err(ERR),
        }
    }
    match e {
        Instance::Literal(l) => lit_count_ones(l).map(Into::into),
        Instance::Vec(v) => v.compwise_unary(lit_count_ones).map(Into::into),
        _ => Err(ERR),
    }
}

fn call_counttrailingzeros(e: &Instance) -> Result<Instance, E> {
    const ERR: E = E::Builtin("`countTrailingZeros` expects a float or vector of float argument");
    fn lit_trailing_zeros(l: &LiteralInstance) -> Result<LiteralInstance, E> {
        match l {
            LiteralInstance::Bool(_) => Err(ERR),
            LiteralInstance::AbstractInt(n) => {
                Ok(LiteralInstance::AbstractInt(n.trailing_zeros() as i64))
            }
            LiteralInstance::AbstractFloat(_) => Err(ERR),
            LiteralInstance::I32(n) => Ok(LiteralInstance::I32(n.trailing_zeros() as i32)),
            LiteralInstance::U32(n) => Ok(LiteralInstance::U32(n.trailing_zeros())),
            LiteralInstance::F32(_) => Err(ERR),
            LiteralInstance::F16(_) => Err(ERR),
        }
    }
    match e {
        Instance::Literal(l) => lit_trailing_zeros(l).map(Into::into),
        Instance::Vec(v) => v.compwise_unary(lit_trailing_zeros).map(Into::into),
        _ => Err(ERR),
    }
}

fn call_cross(a: &Instance, b: &Instance, stage: EvalStage) -> Result<Instance, E> {
    let (a, b) = convert(a, b).ok_or(E::Builtin("`cross` arguments are incompatible"))?;
    match (a, b) {
        (Instance::Vec(a), Instance::Vec(b)) if a.n() == 3 => {
            let s1 = a[1]
                .op_mul(&b[2], stage)?
                .op_sub(&a[2].op_mul(&b[1], stage)?, stage)?;
            let s2 = a[2]
                .op_mul(&b[0], stage)?
                .op_sub(&a[0].op_mul(&b[2], stage)?, stage)?;
            let s3 = a[0]
                .op_mul(&b[1], stage)?
                .op_sub(&a[1].op_mul(&b[0], stage)?, stage)?;
            Ok(VecInstance::new(vec![s1, s2, s3]).into())
        }
        _ => Err(E::Builtin(
            "`cross` expects a 3-component vector of float arguments",
        )),
    }
}

fn call_degrees(e: &Instance) -> Result<Instance, E> {
    impl_call_float_unary!("degrees", e, n => n.to_degrees())
}

fn call_determinant(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("determinant".to_string()))
}

// NOTE: the function returns an error if computed out of domain
fn call_distance(e1: &Instance, e2: &Instance, stage: EvalStage) -> Result<Instance, E> {
    call_length(&e1.op_sub(e2, stage)?)
}

fn call_dot(e1: &Instance, e2: &Instance, stage: EvalStage) -> Result<Instance, E> {
    let (e1, e2) = convert(e1, e2).ok_or(E::Builtin("`dot` arguments are incompatible"))?;
    match (e1, e2) {
        (Instance::Vec(e1), Instance::Vec(e2)) => e1.dot(&e2, stage).map(Into::into),
        _ => Err(E::Builtin("`dot` expects vector arguments")),
    }
}

fn call_dot4u8packed(_a1: &Instance, _a2: &Instance) -> Result<Instance, E> {
    Err(E::Todo("dot4U8Packed".to_string()))
}

fn call_dot4i8packed(_a1: &Instance, _a2: &Instance) -> Result<Instance, E> {
    Err(E::Todo("dot4I8Packed".to_string()))
}

fn call_exp(e: &Instance) -> Result<Instance, E> {
    impl_call_float_unary!("exp", e, n => n.exp())
}

fn call_exp2(e: &Instance) -> Result<Instance, E> {
    impl_call_float_unary!("exp2", e, n => n.exp2())
}

fn call_extractbits(_a1: &Instance, _a2: &Instance, _a3: &Instance) -> Result<Instance, E> {
    Err(E::Todo("extractBits".to_string()))
}

fn call_faceforward(_a1: &Instance, _a2: &Instance, _a3: &Instance) -> Result<Instance, E> {
    Err(E::Todo("faceForward".to_string()))
}

fn call_firstleadingbit(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("firstLeadingBit".to_string()))
}

fn call_firsttrailingbit(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("firstTrailingBit".to_string()))
}

fn call_floor(e: &Instance) -> Result<Instance, E> {
    impl_call_float_unary!("floor", e, n => n.floor())
}

fn call_fma(_a1: &Instance, _a2: &Instance, _a3: &Instance) -> Result<Instance, E> {
    Err(E::Todo("fma".to_string()))
}

fn call_fract(e: &Instance, stage: EvalStage) -> Result<Instance, E> {
    e.op_sub(&call_floor(e)?, stage)
    // impl_call_float_unary!("fract", e, n => n.fract())
}

fn call_frexp(e: &Instance) -> Result<Instance, E> {
    const ERR: E = E::Builtin("`frexp` expects a float or vector of float argument");
    fn make_frexp_inst(ty: &'static str, fract: Instance, exp: Instance) -> Instance {
        Instance::Struct(StructInstance::new(
            Ident::new(format!("__frexp_result_{ty}")),
            vec![
                (Ident::new("fract".to_string()), fract),
                (Ident::new("exp".to_string()), exp),
            ],
        ))
    }
    // from: https://docs.rs/libm/latest/src/libm/math/frexp.rs.html#1-20
    fn frexp(x: f64) -> (f64, i32) {
        let mut y = x.to_bits();
        let ee = ((y >> 52) & 0x7ff) as i32;

        if ee == 0 {
            if x != 0.0 {
                let x1p64 = f64::from_bits(0x43f0000000000000);
                let (x, e) = frexp(x * x1p64);
                return (x, e - 64);
            }
            return (x, 0);
        } else if ee == 0x7ff {
            return (x, 0);
        }

        let e = ee - 0x3fe;
        y &= 0x800fffffffffffff;
        y |= 0x3fe0000000000000;
        (f64::from_bits(y), e)
    }
    match e {
        Instance::Literal(l) => match l {
            LiteralInstance::Bool(_) => todo!(),
            LiteralInstance::AbstractInt(_) => todo!(),
            LiteralInstance::AbstractFloat(n) => {
                let (fract, exp) = frexp(*n);
                Ok(make_frexp_inst(
                    "abstract",
                    LiteralInstance::AbstractFloat(fract).into(),
                    LiteralInstance::AbstractInt(exp as i64).into(),
                ))
            }
            LiteralInstance::I32(_) => todo!(),
            LiteralInstance::U32(_) => todo!(),
            LiteralInstance::F32(n) => {
                let (fract, exp) = frexp(*n as f64);
                Ok(make_frexp_inst(
                    "f32",
                    LiteralInstance::F32(fract as f32).into(),
                    LiteralInstance::I32(exp).into(),
                ))
            }
            LiteralInstance::F16(n) => {
                let (fract, exp) = frexp(n.to_f64().unwrap(/* SAFETY: f16 to f64 is lossless */));
                Ok(make_frexp_inst(
                    "f16",
                    LiteralInstance::F16(f16::from_f64(fract)).into(),
                    LiteralInstance::I32(exp).into(),
                ))
            }
        },
        Instance::Vec(v) => {
            let ty = v.inner_ty();
            let name = match ty {
                Type::AbstractFloat => "vecN_abstract",
                Type::F32 => "vecN_f32",
                Type::F16 => "vecN_f16",
                _ => return Err(ERR),
            };
            let (fracts, exps): (Vec<_>, Vec<_>) = v
                .iter()
                .map(|l| match l.unwrap_literal_ref() {
                    LiteralInstance::AbstractFloat(n) => Ok(*n),
                    LiteralInstance::F32(n) => Ok(*n as f64),
                    LiteralInstance::F16(n) => {
                        Ok(n.to_f64().unwrap(/* SAFETY: f16 to f64 is lossless */))
                    }
                    _ => Err(ERR),
                })
                .collect::<Result<Vec<_>, _>>()?
                .into_iter()
                .map(frexp)
                .unzip();
            let fracts = fracts
                .into_iter()
                .map(|n| match ty {
                    Type::AbstractFloat => LiteralInstance::AbstractFloat(n).into(),
                    Type::F32 => LiteralInstance::F32(n as f32).into(),
                    Type::F16 => LiteralInstance::F16(f16::from_f64(n)).into(),
                    _ => unreachable!("case handled above"),
                })
                .collect_vec();
            let exps = exps
                .into_iter()
                .map(|n| match ty {
                    Type::AbstractFloat => LiteralInstance::AbstractInt(n as i64).into(),
                    Type::F32 => LiteralInstance::I32(n).into(),
                    Type::F16 => LiteralInstance::I32(n).into(),
                    _ => unreachable!("case handled above"),
                })
                .collect_vec();
            let fract = VecInstance::new(fracts).into();
            let exp = VecInstance::new(exps).into();
            Ok(make_frexp_inst(name, fract, exp))
        }
        _ => Err(ERR),
    }
}

fn call_insertbits(
    _a1: &Instance,
    _a2: &Instance,
    _a3: &Instance,
    _a4: &Instance,
) -> Result<Instance, E> {
    Err(E::Todo("insertBits".to_string()))
}

// NOTE: the function returns NaN as an `indeterminate value` if computed out of domain
fn call_inversesqrt(e: &Instance) -> Result<Instance, E> {
    const ERR: E = E::Builtin("`inverseSqrt` expects a float or vector of float argument");
    fn lit_isqrt(l: &LiteralInstance) -> Result<LiteralInstance, E> {
        match l {
            LiteralInstance::Bool(_) => Err(ERR),
            LiteralInstance::AbstractInt(_) => l
                .convert_to(&Type::AbstractFloat)
                .ok_or_else(|| E::Conversion(Type::AbstractInt, Type::AbstractFloat))
                .map(|n| LiteralInstance::from(1.0 / n.unwrap_abstract_float().sqrt())),
            LiteralInstance::AbstractFloat(n) => Ok(LiteralInstance::from(1.0 / n.sqrt())),
            LiteralInstance::I32(_) => Err(ERR),
            LiteralInstance::U32(_) => Err(ERR),
            LiteralInstance::F32(n) => Ok(LiteralInstance::from(1.0 / n.sqrt())),
            LiteralInstance::F16(n) => Ok(LiteralInstance::from(f16::one() / n.sqrt())),
        }
    }
    match e {
        Instance::Literal(l) => lit_isqrt(l).map(Into::into),
        Instance::Vec(v) => v.compwise_unary(lit_isqrt).map(Into::into),
        _ => Err(ERR),
    }
}

fn call_ldexp(e1: &Instance, e2: &Instance) -> Result<Instance, E> {
    // from: https://docs.rs/libm/latest/src/libm/math/scalbn.rs.html#3-34
    fn scalbn(x: f64, mut n: i32) -> f64 {
        let x1p1023 = f64::from_bits(0x7fe0000000000000); // 0x1p1023 === 2 ^ 1023
        let x1p53 = f64::from_bits(0x4340000000000000); // 0x1p53 === 2 ^ 53
        let x1p_1022 = f64::from_bits(0x0010000000000000); // 0x1p-1022 === 2 ^ (-1022)

        let mut y = x;

        if n > 1023 {
            y *= x1p1023;
            n -= 1023;
            if n > 1023 {
                y *= x1p1023;
                n -= 1023;
                if n > 1023 {
                    n = 1023;
                }
            }
        } else if n < -1022 {
            /* make sure final n < -53 to avoid double
            rounding in the subnormal range */
            y *= x1p_1022 * x1p53;
            n += 1022 - 53;
            if n < -1022 {
                y *= x1p_1022 * x1p53;
                n += 1022 - 53;
                if n < -1022 {
                    n = -1022;
                }
            }
        }
        y * f64::from_bits(((0x3ff + n) as u64) << 52)
    }
    fn ldexp_lit(l1: &LiteralInstance, l2: &LiteralInstance) -> Result<LiteralInstance, E> {
        match (l1, l2) {
            (LiteralInstance::AbstractInt(n1), LiteralInstance::AbstractInt(n2)) => Ok(
                LiteralInstance::AbstractFloat(scalbn(n1.to_f64().unwrap(), n2.to_i32().unwrap())),
            ),
            (LiteralInstance::AbstractFloat(n1), LiteralInstance::AbstractInt(n2)) => Ok(
                LiteralInstance::AbstractFloat(scalbn(*n1, n2.to_i32().unwrap())),
            ),
            (LiteralInstance::AbstractInt(n1), LiteralInstance::I32(n2)) => Ok(
                LiteralInstance::F32(scalbn(n1.to_f64().unwrap(), *n2) as f32),
            ),
            (LiteralInstance::AbstractFloat(n1), LiteralInstance::I32(n2)) => Ok(
                LiteralInstance::F32(scalbn(*n1, n2.to_i32().unwrap()) as f32),
            ),
            (LiteralInstance::F32(n1), LiteralInstance::AbstractInt(n2)) => Ok(
                LiteralInstance::F32(scalbn(n1.to_f64().unwrap(), n2.to_i32().unwrap()) as f32),
            ),
            (LiteralInstance::F32(n1), LiteralInstance::I32(n2)) => Ok(LiteralInstance::F32(
                scalbn(n1.to_f64().unwrap(), n2.to_i32().unwrap()) as f32,
            )),
            (LiteralInstance::F16(n1), LiteralInstance::AbstractInt(n2)) => {
                Ok(LiteralInstance::F16(f16::from_f64(scalbn(
                    n1.to_f64().unwrap(),
                    n2.to_i32().unwrap(),
                ))))
            }
            (LiteralInstance::F16(n1), LiteralInstance::I32(n2)) => Ok(LiteralInstance::F16(
                f16::from_f64(scalbn(n1.to_f64().unwrap(), *n2)),
            )),
            _ => Err(E::Builtin(
                "`ldexp` with scalar arguments expects a float and a i32 arguments",
            )),
        }
    }

    // TODO conversion errors
    match (e1, e2) {
        (Instance::Literal(l1), Instance::Literal(l2)) => ldexp_lit(l1, l2).map(Into::into),
        (Instance::Vec(v1), Instance::Vec(v2)) => v1.compwise_binary(v2, ldexp_lit).map(Into::into),
        _ => Err(E::Builtin(
            "`ldexp` expects two scalar or two vector arguments",
        )),
    }
}

fn call_length(e: &Instance) -> Result<Instance, E> {
    const ERR: E = E::Builtin("`length` expects a float or vector of float argument");
    match e {
        Instance::Literal(_) => call_abs(e),
        Instance::Vec(v) => call_sqrt(
            &v.op_mul(v, EvalStage::Exec)?
                .into_iter()
                .map(Ok)
                .reduce(|a, b| a?.op_add(&b?, EvalStage::Exec))
                .unwrap()?,
        ),
        _ => Err(ERR),
    }
}

fn call_log(e: &Instance) -> Result<Instance, E> {
    impl_call_float_unary!("log", e, n => n.ln())
}

fn call_log2(e: &Instance) -> Result<Instance, E> {
    impl_call_float_unary!("log2", e, n => n.log2())
}

fn call_max(e1: &Instance, e2: &Instance) -> Result<Instance, E> {
    const ERR: E = E::Builtin("`max` expects a scalar or vector of scalar argument");
    fn lit_max(e1: &LiteralInstance, e2: &LiteralInstance) -> Result<LiteralInstance, E> {
        match e1 {
            LiteralInstance::Bool(_) => Err(ERR),
            LiteralInstance::AbstractInt(e1) => {
                Ok(LiteralInstance::from(*e1.max(&e2.unwrap_abstract_int())))
            }
            LiteralInstance::AbstractFloat(e1) => {
                Ok(LiteralInstance::from(e1.max(e2.unwrap_abstract_float())))
            }
            LiteralInstance::I32(e1) => Ok(LiteralInstance::from(*e1.max(&e2.unwrap_i_32()))),
            LiteralInstance::U32(e1) => Ok(LiteralInstance::from(*e1.max(&e2.unwrap_u_32()))),
            LiteralInstance::F32(e1) => Ok(LiteralInstance::from(e1.max(e2.unwrap_f_32()))),
            LiteralInstance::F16(e1) => Ok(LiteralInstance::from(e1.max(e2.unwrap_f_16()))),
        }
    }
    let (e1, e2) = convert(e1, e2).ok_or_else(|| E::Builtin("`max` arguments are incompatible"))?;
    match (e1, e2) {
        (Instance::Literal(e1), Instance::Literal(e2)) => lit_max(&e1, &e2).map(Into::into),
        (Instance::Vec(e1), Instance::Vec(e2)) => e1.compwise_binary(&e2, lit_max).map(Into::into),
        _ => Err(ERR),
    }
}

fn call_min(e1: &Instance, e2: &Instance) -> Result<Instance, E> {
    const ERR: E = E::Builtin("`min` expects a scalar or vector of scalar argument");
    fn lit_min(e1: &LiteralInstance, e2: &LiteralInstance) -> Result<LiteralInstance, E> {
        match e1 {
            LiteralInstance::Bool(_) => Err(ERR),
            LiteralInstance::AbstractInt(e1) => {
                Ok(LiteralInstance::from(*e1.min(&e2.unwrap_abstract_int())))
            }
            LiteralInstance::AbstractFloat(e1) => {
                Ok(LiteralInstance::from(e1.min(e2.unwrap_abstract_float())))
            }
            LiteralInstance::I32(e1) => Ok(LiteralInstance::from(*e1.min(&e2.unwrap_i_32()))),
            LiteralInstance::U32(e1) => Ok(LiteralInstance::from(*e1.min(&e2.unwrap_u_32()))),
            LiteralInstance::F32(e1) => Ok(LiteralInstance::from(e1.min(e2.unwrap_f_32()))),
            LiteralInstance::F16(e1) => Ok(LiteralInstance::from(e1.min(e2.unwrap_f_16()))),
        }
    }
    let (e1, e2) = convert(e1, e2).ok_or_else(|| E::Builtin("`min` arguments are incompatible"))?;
    match (e1, e2) {
        (Instance::Literal(e1), Instance::Literal(e2)) => lit_min(&e1, &e2).map(Into::into),
        (Instance::Vec(e1), Instance::Vec(e2)) => e1.compwise_binary(&e2, lit_min).map(Into::into),
        _ => Err(ERR),
    }
}

fn call_mix(_a1: &Instance, _a2: &Instance, _a3: &Instance) -> Result<Instance, E> {
    Err(E::Todo("mix".to_string()))
}

fn call_modf(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("modf".to_string()))
}

fn call_normalize(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("normalize".to_string()))
}

fn call_pow(e1: &Instance, e2: &Instance) -> Result<Instance, E> {
    const ERR: E = E::Builtin("`pow` expects a scalar or vector of scalar argument");
    fn lit_powf(e1: &LiteralInstance, e2: &LiteralInstance) -> Result<LiteralInstance, E> {
        match e1 {
            LiteralInstance::Bool(_) => Err(ERR),
            LiteralInstance::AbstractInt(_) => {
                let e1 = e1
                    .convert_to(&Type::AbstractFloat)
                    .ok_or_else(|| E::Conversion(Type::AbstractInt, Type::AbstractFloat))?
                    .unwrap_abstract_float();
                let e2 = e2
                    .convert_to(&Type::AbstractFloat)
                    .ok_or_else(|| E::Conversion(Type::AbstractInt, Type::AbstractFloat))?
                    .unwrap_abstract_float();
                Ok(LiteralInstance::from(e1.powf(e2)))
            }
            LiteralInstance::AbstractFloat(e1) => {
                Ok(LiteralInstance::from(e1.powf(e2.unwrap_abstract_float())))
            }
            LiteralInstance::I32(_) => Err(ERR),
            LiteralInstance::U32(_) => Err(ERR),
            LiteralInstance::F32(e1) => Ok(LiteralInstance::from(e1.powf(e2.unwrap_f_32()))),
            LiteralInstance::F16(e1) => Ok(LiteralInstance::from(e1.powf(e2.unwrap_f_16()))),
        }
    }
    let (e1, e2) = convert(e1, e2).ok_or_else(|| E::Builtin("`pow` arguments are incompatible"))?;
    match (e1, e2) {
        (Instance::Literal(e1), Instance::Literal(e2)) => lit_powf(&e1, &e2).map(Into::into),
        (Instance::Vec(e1), Instance::Vec(e2)) => e1.compwise_binary(&e2, lit_powf).map(Into::into),
        _ => Err(ERR),
    }
}

fn call_quantizetof16(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("quantizeToF16".to_string()))
}

fn call_radians(e: &Instance) -> Result<Instance, E> {
    impl_call_float_unary!("radians", e, n => n.to_radians())
}

fn call_reflect(_a1: &Instance, _a2: &Instance) -> Result<Instance, E> {
    Err(E::Todo("reflect".to_string()))
}

fn call_refract(_a1: &Instance, _a2: &Instance, _a3: &Instance) -> Result<Instance, E> {
    Err(E::Todo("refract".to_string()))
}

fn call_reversebits(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("reverseBits".to_string()))
}

fn call_round(e: &Instance) -> Result<Instance, E> {
    const ERR: E = E::Builtin("`round` expects a float or vector of float argument");
    fn lit_fn(l: &LiteralInstance) -> Result<LiteralInstance, E> {
        match l {
            LiteralInstance::Bool(_) => Err(ERR),
            LiteralInstance::AbstractInt(_) => {
                let n = l
                    .convert_to(&Type::AbstractFloat)
                    .ok_or_else(|| E::Conversion(Type::AbstractInt, Type::AbstractFloat))?
                    .unwrap_abstract_float();
                Ok(LiteralInstance::from(n.round_ties_even()))
            }
            LiteralInstance::AbstractFloat(n) => Ok(LiteralInstance::from(n.round_ties_even())),
            LiteralInstance::I32(_) => Err(ERR),
            LiteralInstance::U32(_) => Err(ERR),
            LiteralInstance::F32(n) => Ok(LiteralInstance::from(n.round_ties_even())),
            LiteralInstance::F16(n) => Ok(LiteralInstance::from(f16::from_f32(
                f16::to_f32(*n).round_ties_even(),
            ))),
        }
    }
    match e {
        Instance::Literal(l) => lit_fn(l).map(Into::into),
        Instance::Vec(v) => v.compwise_unary(lit_fn).map(Into::into),
        _ => Err(ERR),
    }
}

fn call_saturate(e: &Instance) -> Result<Instance, E> {
    match e {
        Instance::Literal(_) => {
            let zero = LiteralInstance::AbstractFloat(0.0);
            let one = LiteralInstance::AbstractFloat(1.0);
            call_clamp(e, &zero.into(), &one.into())
        }
        Instance::Vec(v) => {
            let n = v.n();
            let zero = Instance::from(LiteralInstance::AbstractFloat(0.0));
            let one = Instance::from(LiteralInstance::AbstractFloat(1.0));
            let zero = VecInstance::new((0..n).map(|_| zero.clone()).collect_vec());
            let one = VecInstance::new((0..n).map(|_| one.clone()).collect_vec());
            call_clamp(e, &zero.into(), &one.into())
        }
        _ => Err(E::Builtin(
            "`saturate` expects a float or vector of float argument",
        )),
    }
}

fn call_sign(e: &Instance) -> Result<Instance, E> {
    const ERR: E = E::Builtin(concat!(
        "`",
        "sign",
        "` expects a float or vector of float argument"
    ));
    fn lit_fn(l: &LiteralInstance) -> Result<LiteralInstance, E> {
        match l {
            LiteralInstance::Bool(_) => Err(ERR),
            LiteralInstance::AbstractInt(n) => Ok(LiteralInstance::from(n.signum())),
            LiteralInstance::AbstractFloat(n) => Ok(LiteralInstance::from(if n.is_zero() {
                *n
            } else {
                n.signum()
            })),
            LiteralInstance::I32(n) => Ok(LiteralInstance::from(n.signum())),
            LiteralInstance::U32(n) => Ok(LiteralInstance::from(if n.is_zero() { *n } else { 1 })),
            LiteralInstance::F32(n) => Ok(LiteralInstance::from(if n.is_zero() {
                *n
            } else {
                n.signum()
            })),
            LiteralInstance::F16(n) => Ok(LiteralInstance::from(if n.is_zero() {
                *n
            } else {
                n.signum()
            })),
        }
    }
    match e {
        Instance::Literal(l) => lit_fn(l).map(Into::into),
        Instance::Vec(v) => v.compwise_unary(lit_fn).map(Into::into),
        _ => Err(ERR),
    }
}

fn call_sin(e: &Instance) -> Result<Instance, E> {
    impl_call_float_unary!("sin", e, n => n.sin())
}

fn call_sinh(e: &Instance) -> Result<Instance, E> {
    impl_call_float_unary!("sinh", e, n => n.sinh())
}

fn call_smoothstep(_low: &Instance, _high: &Instance, _x: &Instance) -> Result<Instance, E> {
    Err(E::Todo("smoothstep".to_string()))
}

fn call_sqrt(e: &Instance) -> Result<Instance, E> {
    impl_call_float_unary!("sqrt", e, n => n.sqrt())
}

fn call_step(edge: &Instance, x: &Instance) -> Result<Instance, E> {
    const ERR: E = E::Builtin("`step` expects a float or vector of float argument");
    fn lit_step(edge: &LiteralInstance, x: &LiteralInstance) -> Result<LiteralInstance, E> {
        match edge {
            LiteralInstance::Bool(_) => Err(ERR),
            LiteralInstance::AbstractInt(_) => {
                let edge = edge
                    .convert_to(&Type::AbstractFloat)
                    .ok_or_else(|| E::Conversion(Type::AbstractInt, Type::AbstractFloat))?
                    .unwrap_abstract_float();
                let x = x
                    .convert_to(&Type::AbstractFloat)
                    .ok_or_else(|| E::Conversion(Type::AbstractInt, Type::AbstractFloat))?
                    .unwrap_abstract_float();
                Ok(LiteralInstance::from(if edge <= x { 1.0 } else { 0.0 }))
            }
            LiteralInstance::AbstractFloat(edge) => Ok(LiteralInstance::from(
                if *edge <= x.unwrap_abstract_float() {
                    1.0
                } else {
                    0.0
                },
            )),
            LiteralInstance::I32(_) => Err(ERR),
            LiteralInstance::U32(_) => Err(ERR),
            LiteralInstance::F32(edge) => Ok(LiteralInstance::from(if *edge <= x.unwrap_f_32() {
                1.0
            } else {
                0.0
            })),
            LiteralInstance::F16(edge) => Ok(LiteralInstance::from(if *edge <= x.unwrap_f_16() {
                1.0
            } else {
                0.0
            })),
        }
    }
    let (edge, x) = convert(edge, x).ok_or(E::Builtin("`step` arguments are incompatible"))?;
    match (edge, x) {
        (Instance::Literal(edge), Instance::Literal(x)) => lit_step(&edge, &x).map(Into::into),
        (Instance::Vec(edge), Instance::Vec(x)) => {
            edge.compwise_binary(&x, lit_step).map(Into::into)
        }
        _ => Err(ERR),
    }
}

fn call_tan(e: &Instance) -> Result<Instance, E> {
    impl_call_float_unary!("tan", e, n => n.tan())
}

fn call_tanh(e: &Instance) -> Result<Instance, E> {
    impl_call_float_unary!("tanh", e, n => n.tanh())
}

fn call_transpose(e: &Instance) -> Result<Instance, E> {
    match e {
        Instance::Mat(e) => Ok(e.transpose().into()),
        _ => Err(E::Builtin("`transpose` expects a matrix argument")),
    }
}

fn call_trunc(e: &Instance) -> Result<Instance, E> {
    impl_call_float_unary!("trunc", e, n => n.trunc())
}

// ------------
// DATA PACKING
// ------------
// reference: <https://www.w3.org/TR/WGSL/#pack-builtin-functions>

fn call_pack4x8snorm(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("pack4x8snorm".to_string()))
}

fn call_pack4x8unorm(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("pack4x8unorm".to_string()))
}

fn call_pack4xi8(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("pack4xI8".to_string()))
}

fn call_pack4xu8(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("pack4xU8".to_string()))
}

fn call_pack4xi8clamp(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("pack4xI8Clamp".to_string()))
}

fn call_pack4xu8clamp(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("pack4xU8Clamp".to_string()))
}

fn call_pack2x16snorm(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("pack2x16snorm".to_string()))
}

fn call_pack2x16unorm(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("pack2x16unorm".to_string()))
}

fn call_pack2x16float(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("pack2x16float".to_string()))
}

fn call_unpack4x8snorm(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("unpack4x8snorm".to_string()))
}

fn call_unpack4x8unorm(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("unpack4x8unorm".to_string()))
}

fn call_unpack4xi8(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("unpack4xI8".to_string()))
}

fn call_unpack4xu8(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("unpack4xU8".to_string()))
}

fn call_unpack2x16snorm(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("unpack2x16snorm".to_string()))
}

fn call_unpack2x16unorm(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("unpack2x16unorm".to_string()))
}

fn call_unpack2x16float(_a1: &Instance) -> Result<Instance, E> {
    Err(E::Todo("unpack2x16float".to_string()))
}

impl VecInstance {
    /// warning, this function does not check operand types
    pub fn dot(&self, rhs: &VecInstance, stage: EvalStage) -> Result<LiteralInstance, E> {
        self.compwise_binary(rhs, |a, b| a.op_mul(b, stage))?
            .into_iter()
            .map(|c| Ok(c.unwrap_literal()))
            .reduce(|a, b| a?.op_add(&b?, stage))
            .unwrap()
    }
}

impl MatInstance {
    /// warning, this function does not check operand types
    pub fn transpose(&self) -> MatInstance {
        let components = (0..self.r())
            .map(|j| {
                VecInstance::new(
                    (0..self.c())
                        .map(|i| self.get(i, j).unwrap().clone())
                        .collect_vec(),
                )
                .into()
            })
            .collect_vec();
        MatInstance::from_cols(components)
    }
}
