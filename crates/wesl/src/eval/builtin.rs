use half::prelude::*;
use num_traits::{FromPrimitive, One, ToBytes, ToPrimitive, Zero};
use std::collections::HashMap;

use itertools::{chain, izip, Itertools};
use lazy_static::lazy_static;
use wgsl_parse::syntax::{
    AccessMode, AddressSpace, Attribute, CustomAttribute, Expression, GlobalDeclaration,
    LiteralExpression, TemplateArg, TranslationUnit, TypeExpression,
};

use crate::{Context, Eval};

use super::{
    conv::{convert_all, Convert},
    convert, convert_all_inner_to,
    ops::Compwise,
    ArrayInstance, EvalError, EvalTy, Instance, LiteralInstance, MatInstance, StructInstance,
    SyntaxUtil, Ty, Type, VecInstance,
};

type E = EvalError;

// TODO: when we have the wgsl! macro, we can refactor the consts.

pub const EXPR_TRUE: Expression = Expression::Literal(LiteralExpression::Bool(true));
pub const EXPR_FALSE: Expression = Expression::Literal(LiteralExpression::Bool(false));

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
                        s.name = format!("__{}", s.name);
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
            Type::Struct(name) => StructInstance::zero_value(name, ctx).map(Into::into),
            Type::Array(n, a_ty) => ArrayInstance::zero_value(*n, a_ty, ctx).map(Into::into),
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
    pub fn zero_value(name: &str, ctx: &mut Context) -> Result<Self, E> {
        let decl = ctx
            .source
            .decl_struct(name)
            .expect("struct declaration not found");

        let members = decl
            .members
            .iter()
            .map(|m| {
                let ty = m.ty.eval_ty(ctx)?;
                let val = Instance::zero_value(&ty, ctx)?;
                Ok((m.name.clone(), val))
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(StructInstance {
            name: name.to_string(),
            members: HashMap::from_iter(members),
        })
    }
}

impl ArrayInstance {
    /// zero-value initialize an array instance.
    pub fn zero_value(n: usize, ty: &Type, ctx: &mut Context) -> Result<Self, E> {
        let zero = Instance::zero_value(ty, ctx)?;
        let comps = (0..n).map(|_| zero.clone()).collect_vec();
        Ok(ArrayInstance::new(comps))
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
        Ok(MatInstance::new(comps))
    }
}

// ------------
// CONSTRUCTORS
// ------------
// reference: <https://www.w3.org/TR/WGSL/#constructor-builtin-function>

pub fn call_builtin(
    ty: &TypeExpression,
    args: Vec<Instance>,
    ctx: &mut Context,
) -> Result<Instance, E> {
    match (
        ty.name.as_str(),
        ty.template_args.as_ref().map(|tplt| tplt.as_slice()),
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
        ("f32", None, [a1]) => call_f32_1(a1),
        ("f16", None, []) => Instance::zero_value(&Type::F16, ctx),
        ("f16", None, [a1]) => call_f16_1(a1),
        ("mat2x2", Some(t), []) => Instance::zero_value(&MatTemplate::parse(t, ctx)?.ty(2, 2), ctx),
        ("mat2x2", Some(t), a) => call_mat_t(2, 2, MatTemplate::parse(t, ctx)?, a),
        ("mat2x2", None, a) => call_mat(2, 2, a),
        ("mat2x3", Some(t), []) => Instance::zero_value(&MatTemplate::parse(t, ctx)?.ty(2, 3), ctx),
        ("mat2x3", Some(t), a) => call_mat_t(2, 3, MatTemplate::parse(t, ctx)?, a),
        ("mat2x3", None, a) => call_mat(2, 3, a),
        ("mat2x4", Some(t), []) => Instance::zero_value(&MatTemplate::parse(t, ctx)?.ty(2, 4), ctx),
        ("mat2x4", Some(t), a) => call_mat_t(2, 4, MatTemplate::parse(t, ctx)?, a),
        ("mat2x4", None, a) => call_mat(2, 4, a),
        ("mat3x2", Some(t), []) => Instance::zero_value(&MatTemplate::parse(t, ctx)?.ty(3, 2), ctx),
        ("mat3x2", Some(t), a) => call_mat_t(3, 2, MatTemplate::parse(t, ctx)?, a),
        ("mat3x2", None, a) => call_mat(3, 2, a),
        ("mat3x3", Some(t), []) => Instance::zero_value(&MatTemplate::parse(t, ctx)?.ty(3, 3), ctx),
        ("mat3x3", Some(t), a) => call_mat_t(3, 3, MatTemplate::parse(t, ctx)?, a),
        ("mat3x3", None, a) => call_mat(3, 3, a),
        ("mat3x4", Some(t), []) => Instance::zero_value(&MatTemplate::parse(t, ctx)?.ty(3, 4), ctx),
        ("mat3x4", Some(t), a) => call_mat_t(3, 4, MatTemplate::parse(t, ctx)?, a),
        ("mat3x4", None, a) => call_mat(3, 4, a),
        ("mat4x2", Some(t), []) => Instance::zero_value(&MatTemplate::parse(t, ctx)?.ty(4, 2), ctx),
        ("mat4x2", Some(t), a) => call_mat_t(4, 2, MatTemplate::parse(t, ctx)?, a),
        ("mat4x2", None, a) => call_mat(4, 2, a),
        ("mat4x3", Some(t), []) => Instance::zero_value(&MatTemplate::parse(t, ctx)?.ty(4, 3), ctx),
        ("mat4x3", Some(t), a) => call_mat_t(4, 3, MatTemplate::parse(t, ctx)?, a),
        ("mat4x3", None, a) => call_mat(4, 3, a),
        ("mat4x4", Some(t), []) => Instance::zero_value(&MatTemplate::parse(t, ctx)?.ty(4, 4), ctx),
        ("mat4x4", Some(t), a) => call_mat_t(4, 4, MatTemplate::parse(t, ctx)?, a),
        ("mat4x4", None, a) => call_mat(4, 4, a),
        ("vec2", Some(t), []) => Instance::zero_value(&VecTemplate::parse(t, ctx)?.ty(2), ctx),
        ("vec2", Some(t), a) => call_vec_t(2, VecTemplate::parse(t, ctx)?, a),
        ("vec2", None, a) => call_vec(2, a),
        ("vec3", Some(t), []) => Instance::zero_value(&VecTemplate::parse(t, ctx)?.ty(3), ctx),
        ("vec3", Some(t), a) => call_vec_t(3, VecTemplate::parse(t, ctx)?, a),
        ("vec3", None, a) => call_vec(3, a),
        ("vec4", Some(t), []) => Instance::zero_value(&VecTemplate::parse(t, ctx)?.ty(4), ctx),
        ("vec4", Some(t), a) => call_vec_t(4, VecTemplate::parse(t, ctx)?, a),
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
        ("cross", None, [a1, a2]) => call_cross(a1, a2),
        ("degrees", None, [a]) => call_degrees(a),
        ("determinant", None, [a]) => call_determinant(a),
        ("distance", None, [a1, a2]) => call_distance(a1, a2),
        ("dot", None, [a1, a2]) => call_dot(a1, a2),
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
        ("fract", None, [a]) => call_fract(a),
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
        ("pow", None, [a]) => call_pow(a),
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

pub struct ArrayTemplate {
    n: usize,
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
            (Some(_), _, _) => Err(E::Builtin(
                "runtime-sized arrays are not supported in const contexts",
            )),
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
                Ok(ArrayTemplate { n, ty })
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
    pub fn n(&self) -> usize {
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
                    name: e1,
                    template_args: None,
                })),
                Some(Expression::TypeOrIdentifier(e2)),
                e3,
                None,
            ) => {
                let mut space = e1
                    .parse()
                    .map_err(|()| EvalError::Builtin("invalid pointer storage space"))?;
                let ty = e2.eval_ty(ctx)?;
                if !ty.is_storable() {
                    return Err(EvalError::Builtin("pointer type must be storable"));
                }
                let access = if let Some(e3) = e3 {
                    match e3 {
                        Expression::TypeOrIdentifier(TypeExpression {
                            name,
                            template_args: None,
                        }) => Some(
                            name.parse()
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
            a.convert_inner_to(&tplt.ty)
                .ok_or_else(|| E::ParamType(tplt.ty.clone(), a.ty()))
        })
        .collect::<Result<Vec<_>, _>>()?;

    if args.len() != tplt.n {
        return Err(E::ParamCount("array".to_string(), tplt.n, args.len()));
    }

    Ok(ArrayInstance::new(args).into())
}
fn call_array(args: &[Instance]) -> Result<Instance, E> {
    let args = convert_all(args).ok_or_else(|| E::Builtin("array elements are not compatible"))?;

    if args.len() < 1 {
        return Err(E::Builtin("array constructor expects at least 1 argument"));
    }

    Ok(ArrayInstance::new(args).into())
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
                LiteralInstance::AbstractInt(n) => n.to_i32(),
                LiteralInstance::AbstractFloat(n) => n.to_i32(),
                LiteralInstance::I32(n) => n.to_i32(),
                LiteralInstance::U32(n) => n.to_i32(),
                LiteralInstance::F32(n) => n.to_i32(),
                LiteralInstance::F16(n) => n.to_i32(),
            }
            .ok_or_else(|| E::ConvOverflow(*l, Type::I32))?;
            Ok(LiteralInstance::I32(val).into())
        }
        _ => Err(E::Builtin("i32 constructor expects a scalar argument")),
    }
}

// TODO: check that "If T is a floating point type, e is converted to u32, rounding towards zero."
fn call_u32_1(a1: &Instance) -> Result<Instance, E> {
    match a1 {
        Instance::Literal(l) => {
            let val = match l {
                LiteralInstance::Bool(n) => Some(n.then_some(1).unwrap_or(0)),
                LiteralInstance::AbstractInt(n) => n.to_u32(),
                LiteralInstance::AbstractFloat(n) => n.to_u32(),
                LiteralInstance::I32(n) => n.to_u32(),
                LiteralInstance::U32(n) => n.to_u32(),
                LiteralInstance::F32(n) => n.to_u32(),
                LiteralInstance::F16(n) => n.to_u32(),
            }
            .ok_or_else(|| E::ConvOverflow(*l, Type::U32))?;
            Ok(LiteralInstance::U32(val).into())
        }
        _ => Err(E::Builtin("u32 constructor expects a scalar argument")),
    }
}

/// see [`LiteralInstance::convert_to`]
/// "If T is a numeric scalar (other than f32), e is converted to f32 (including invalid conversions)."
fn call_f32_1(a1: &Instance) -> Result<Instance, E> {
    match a1 {
        Instance::Literal(l) => {
            let val = match l {
                LiteralInstance::Bool(n) => Some(n.then_some(f32::one()).unwrap_or(f32::zero())),
                LiteralInstance::AbstractInt(n) => n.to_f32(),
                LiteralInstance::AbstractFloat(n) => n.to_f32(),
                LiteralInstance::I32(n) => n.to_f32(),
                LiteralInstance::U32(n) => n.to_f32(),
                LiteralInstance::F32(n) => n.to_f32(),
                LiteralInstance::F16(n) => n.to_f32(),
            }
            .and_then(|n| n.is_finite().then_some(n))
            .ok_or_else(|| E::ConvOverflow(*l, Type::F32))?;
            Ok(LiteralInstance::F32(val).into())
        }
        _ => Err(E::Builtin("f32 constructor expects a scalar argument")),
    }
}

/// see [`LiteralInstance::convert_to`]
/// "If T is a numeric scalar (other than f16), e is converted to f16 (including invalid conversions)."
fn call_f16_1(a1: &Instance) -> Result<Instance, E> {
    match a1 {
        Instance::Literal(l) => {
            let val = match l {
                LiteralInstance::Bool(n) => Some(n.then_some(f16::one()).unwrap_or(f16::zero())),
                LiteralInstance::AbstractInt(n) => f16::from_i64(*n),
                LiteralInstance::AbstractFloat(n) => Some(f16::from_f64(*n)),
                LiteralInstance::I32(n) => f16::from_i32(*n),
                LiteralInstance::U32(n) => f16::from_u32(*n),
                LiteralInstance::F32(n) => Some(f16::from_f32(*n)),
                LiteralInstance::F16(n) => Some(*n),
            }
            .and_then(|n| n.is_finite().then_some(n))
            .ok_or_else(|| E::ConvOverflow(*l, Type::F16))?;
            Ok(LiteralInstance::F16(val).into())
        }
        _ => Err(E::Builtin("f16 constructor expects a scalar argument")),
    }
}

fn call_mat_t(c: usize, r: usize, tplt: MatTemplate, args: &[Instance]) -> Result<Instance, E> {
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
            .iter()
            .map(|v| {
                v.unwrap_vec_ref()
                    .iter()
                    .map(conv_fn)
                    .collect::<Result<Vec<_>, _>>()
                    .map(|s| Instance::Vec(VecInstance::new(s)))
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(MatInstance::new(comps).into())
    } else {
        let args = convert_all_inner_to(&args, &tplt.inner_ty())
            .ok_or_else(|| E::Builtin("matrix components are not compatible"))?;
        let ty = args
            .first()
            .ok_or_else(|| E::Builtin("matrix constructor expects arguments"))?
            .ty();

        // overload 2: mat from column vectors
        if ty.is_vec() {
            if args.len() != c {
                return Err(E::ParamCount(format!("mat{c}x{r}"), c, args.len()));
            }

            Ok(MatInstance::new(args).into())
        }
        // overload 3: mat from scalar values
        else {
            if args.len() != c {
                return Err(E::ParamCount(format!("mat{c}x{r}"), c * r, args.len()));
            }

            let args = args
                .chunks(c)
                .map(|v| Instance::Vec(VecInstance::new(v.to_vec())))
                .collect_vec();

            Ok(MatInstance::new(args).into())
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
                "matrix constructor expects AbstractFloat, f32, or f16 arguments",
            ));
        };

        let args = convert_all_inner_to(&args, &inner_ty)
            .ok_or_else(|| E::Builtin("matrix components are not compatible"))?;
        let ty = args
            .first()
            .ok_or_else(|| E::Builtin("matrix constructor expects arguments"))?
            .ty();

        // overload 2: mat from column vectors
        if ty.is_vec() {
            if args.len() != c {
                return Err(E::ParamCount(format!("mat{c}x{r}"), c, args.len()));
            }

            Ok(MatInstance::new(args).into())
        }
        // overload 3: mat from scalar values
        else {
            if args.len() != c {
                return Err(E::ParamCount(format!("mat{c}x{r}"), c * r, args.len()));
            }
            let args = args
                .chunks(c)
                .map(|v| Instance::Vec(VecInstance::new(v.to_vec())))
                .collect_vec();

            Ok(MatInstance::new(args).into())
        }
    }
}

fn call_vec_t(n: usize, tplt: VecTemplate, args: &[Instance]) -> Result<Instance, E> {
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
            Type::Bool => call_bool_1,
            Type::I32 => call_i32_1,
            Type::U32 => call_u32_1,
            Type::F32 => call_f32_1,
            Type::F16 => call_f16_1,
            _ => return Err(E::Builtin("vector type must be a concrete scalar")),
        };

        let comps = v.iter().map(conv_fn).collect::<Result<Vec<_>, _>>()?;

        Ok(VecInstance::new(comps).into())
    }
    // overload 3: vec init from component values
    else {
        if args.len() != n {
            return Err(E::ParamCount(format!("vec{n}"), n, args.len()));
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
        let val = Instance::Literal(l.clone());
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
    else {
        if args.len() != n {
            return Err(E::ParamCount(format!("vec{n}"), n, args.len()));
        }

        let comps =
            convert_all(&args).ok_or_else(|| E::Builtin("vector components are not compatible"))?;

        if !comps.first().unwrap(/* SAFETY: len() checked above */).ty().is_scalar() {
            return Err(E::Builtin("vec constructor expects scalar arguments"));
        }
        Ok(VecInstance::new(comps).into())
    }
}

// -------
// BITCAST
// -------
// reference: <https://www.w3.org/TR/WGSL/#bit-reinterp-builtin-functions>

fn call_bitcast_t(tplt: BitcastTemplate, a1: &Instance) -> Result<Instance, E> {
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

    let bytes = match a1 {
        Instance::Literal(l) => lit_bytes(l, &ty),
        Instance::Vec(v) => vec_bytes(v, &ty),
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
                            b.unwrap_literal_ref()
                                .unwrap_bool()
                                .then_some(t)
                                .unwrap_or(f)
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

fn call_arraylength(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("arrayLength".to_string()))
}

// -------
// NUMERIC
// -------
// reference: <https://www.w3.org/TR/WGSL/#numeric-builtin-function>

fn call_abs(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("abs".to_string()))
}

fn call_acos(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("acos".to_string()))
}

fn call_acosh(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("acosh".to_string()))
}

fn call_asin(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("asin".to_string()))
}

fn call_asinh(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("asinh".to_string()))
}

fn call_atan(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("atan".to_string()))
}

fn call_atanh(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("atanh".to_string()))
}

fn call_atan2(_a1: &Instance, _a2: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("atan2".to_string()))
}

fn call_ceil(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("ceil".to_string()))
}

fn call_clamp(_a1: &Instance, _a2: &Instance, _a3: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("clamp".to_string()))
}

fn call_cos(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("cos".to_string()))
}

fn call_cosh(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("cosh".to_string()))
}

fn call_countleadingzeros(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("countLeadingZeros".to_string()))
}

fn call_countonebits(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("countOneBits".to_string()))
}

fn call_counttrailingzeros(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("countTrailingZeros".to_string()))
}

fn call_cross(_a1: &Instance, _a2: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("cross".to_string()))
}

fn call_degrees(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("degrees".to_string()))
}

fn call_determinant(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("determinant".to_string()))
}

fn call_distance(_a1: &Instance, _a2: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("distance".to_string()))
}

fn call_dot(_a1: &Instance, _a2: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("dot".to_string()))
}

fn call_dot4u8packed(_a1: &Instance, _a2: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("dot4U8Packed".to_string()))
}

fn call_dot4i8packed(_a1: &Instance, _a2: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("dot4I8Packed".to_string()))
}

fn call_exp(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("exp".to_string()))
}

fn call_exp2(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("exp2".to_string()))
}

fn call_extractbits(_a1: &Instance, _a2: &Instance, _a3: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("extractBits".to_string()))
}

fn call_faceforward(_a1: &Instance, _a2: &Instance, _a3: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("faceForward".to_string()))
}

fn call_firstleadingbit(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("firstLeadingBit".to_string()))
}

fn call_firsttrailingbit(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("firstTrailingBit".to_string()))
}

fn call_floor(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("floor".to_string()))
}

fn call_fma(_a1: &Instance, _a2: &Instance, _a3: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("fma".to_string()))
}

fn call_fract(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("fract".to_string()))
}

fn call_frexp(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("frexp".to_string()))
}

fn call_insertbits(
    _a1: &Instance,
    _a2: &Instance,
    _a3: &Instance,
    _a4: &Instance,
) -> Result<Instance, E> {
    Err(E::NotImpl("insertBits".to_string()))
}

fn call_inversesqrt(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("inverseSqrt".to_string()))
}

fn call_ldexp(_a1: &Instance, _a2: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("ldexp".to_string()))
}

fn call_length(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("length".to_string()))
}

fn call_log(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("log".to_string()))
}

fn call_log2(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("log2".to_string()))
}

fn call_max(_a1: &Instance, _a2: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("max".to_string()))
}

fn call_min(_a1: &Instance, _a2: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("min".to_string()))
}

fn call_mix(_a1: &Instance, _a2: &Instance, _a3: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("mix".to_string()))
}

fn call_modf(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("modf".to_string()))
}

fn call_normalize(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("normalize".to_string()))
}

fn call_pow(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("pow".to_string()))
}

fn call_quantizetof16(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("quantizeToF16".to_string()))
}

fn call_radians(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("radians".to_string()))
}

fn call_reflect(_a1: &Instance, _a2: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("reflect".to_string()))
}

fn call_refract(_a1: &Instance, _a2: &Instance, _a3: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("refract".to_string()))
}

fn call_reversebits(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("reverseBits".to_string()))
}

fn call_round(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("round".to_string()))
}

fn call_saturate(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("saturate".to_string()))
}

fn call_sign(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("sign".to_string()))
}

fn call_sin(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("sin".to_string()))
}

fn call_sinh(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("sinh".to_string()))
}

fn call_smoothstep(_a1: &Instance, _a2: &Instance, _a3: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("smoothstep".to_string()))
}

fn call_sqrt(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("sqrt".to_string()))
}

fn call_step(_a1: &Instance, _a2: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("step".to_string()))
}

fn call_tan(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("tan".to_string()))
}

fn call_tanh(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("tanh".to_string()))
}

fn call_transpose(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("transpose".to_string()))
}

fn call_trunc(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("trunc".to_string()))
}

// ------------
// DATA PACKING
// ------------
// reference: <https://www.w3.org/TR/WGSL/#pack-builtin-functions>

fn call_pack4x8snorm(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("pack4x8snorm".to_string()))
}

fn call_pack4x8unorm(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("pack4x8unorm".to_string()))
}

fn call_pack4xi8(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("pack4xI8".to_string()))
}

fn call_pack4xu8(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("pack4xU8".to_string()))
}

fn call_pack4xi8clamp(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("pack4xI8Clamp".to_string()))
}

fn call_pack4xu8clamp(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("pack4xU8Clamp".to_string()))
}

fn call_pack2x16snorm(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("pack2x16snorm".to_string()))
}

fn call_pack2x16unorm(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("pack2x16unorm".to_string()))
}

fn call_pack2x16float(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("pack2x16float".to_string()))
}

fn call_unpack4x8snorm(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("unpack4x8snorm".to_string()))
}

fn call_unpack4x8unorm(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("unpack4x8unorm".to_string()))
}

fn call_unpack4xi8(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("unpack4xI8".to_string()))
}

fn call_unpack4xu8(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("unpack4xU8".to_string()))
}

fn call_unpack2x16snorm(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("unpack2x16snorm".to_string()))
}

fn call_unpack2x16unorm(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("unpack2x16unorm".to_string()))
}

fn call_unpack2x16float(_a1: &Instance) -> Result<Instance, E> {
    Err(E::NotImpl("unpack2x16float".to_string()))
}

impl VecInstance {
    pub fn dot(&self, rhs: &VecInstance) -> Result<LiteralInstance, E> {
        self.compwise_binary(rhs, |a, b| a.op_mul(b))?
            .into_iter()
            .map(|c| Ok(c.unwrap_literal()))
            .reduce(|a, b| a?.op_add(&b?))
            .unwrap()
    }
}

impl MatInstance {
    pub fn transpose(&self) -> MatInstance {
        let components = (0..self.r())
            .map(|j| {
                Instance::Vec(VecInstance::new(
                    (0..self.c())
                        .map(|i| self.get(i, j).unwrap().clone())
                        .collect_vec(),
                ))
            })
            .collect_vec();
        MatInstance::new(components)
    }
}
