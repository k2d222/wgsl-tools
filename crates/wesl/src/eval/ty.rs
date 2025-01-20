use super::{
    ArrayInstance, ArrayTemplate, AtomicInstance, AtomicTemplate, Context, Eval, EvalError,
    Instance, LiteralInstance, MatInstance, MatTemplate, PtrInstance, PtrTemplate, RefInstance,
    StructInstance, SyntaxUtil, VecInstance, VecTemplate,
};

use derive_more::derive::{IsVariant, Unwrap};
use wgsl_parse::syntax::*;

#[derive(Clone, Debug, PartialEq, Eq, IsVariant, Unwrap)]
pub enum Type {
    Bool,
    AbstractInt,
    AbstractFloat,
    I32,
    U32,
    F32,
    F16,
    Struct(Ident),
    // TODO: swap these two members
    Array(Option<usize>, Box<Type>),
    Vec(u8, Box<Type>),
    Mat(u8, u8, Box<Type>),
    Atomic(Box<Type>),
    Ptr(AddressSpace, Box<Type>),
    Void,
}

impl Type {
    /// reference: <https://www.w3.org/TR/WGSL/#scalar>
    pub fn is_scalar(&self) -> bool {
        matches!(
            self,
            Type::Bool
                | Type::AbstractInt
                | Type::AbstractFloat
                | Type::I32
                | Type::U32
                | Type::F32
                | Type::F16
        )
    }

    /// reference: <https://www.w3.org/TR/WGSL/#numeric-scalar>
    pub fn is_numeric(&self) -> bool {
        matches!(
            self,
            Type::AbstractInt | Type::AbstractFloat | Type::I32 | Type::U32 | Type::F32 | Type::F16
        )
    }

    /// reference: <https://www.w3.org/TR/WGSL/#integer-scalar>
    pub fn is_integer(&self) -> bool {
        matches!(self, Type::AbstractInt | Type::I32 | Type::U32)
    }

    /// reference: <https://www.w3.org/TR/WGSL/#floating-point-types>
    pub fn is_float(&self) -> bool {
        matches!(self, Type::AbstractFloat | Type::F32 | Type::F16)
    }

    /// reference: <https://www.w3.org/TR/WGSL/#abstract-types>
    pub fn is_abstract(&self) -> bool {
        match self {
            Type::AbstractInt => true,
            Type::AbstractFloat => true,
            Type::Array(_, ty) | Type::Vec(_, ty) | Type::Mat(_, _, ty) => ty.is_abstract(),
            _ => false,
        }
    }

    pub fn is_concrete(&self) -> bool {
        !self.is_abstract()
    }

    /// reference: <https://www.w3.org/TR/WGSL/#storable-types>
    pub fn is_storable(&self) -> bool {
        self.is_concrete()
            && matches!(
                self,
                Type::Bool
                    | Type::I32
                    | Type::U32
                    | Type::F32
                    | Type::F16
                    | Type::Struct(_)
                    | Type::Array(_, _)
                    | Type::Vec(_, _)
                    | Type::Mat(_, _, _)
                    | Type::Atomic(_)
            )
    }
}

pub trait Ty {
    /// get the type of an instance.
    fn ty(&self) -> Type;

    /// get the inner type of an instance (not recursive).
    ///
    /// e.g. the inner type of `array<vec3<u32>>` is `vec3<u32>`.
    fn inner_ty(&self) -> Type {
        self.ty()
    }
}

impl Ty for Type {
    fn ty(&self) -> Type {
        self.clone()
    }

    fn inner_ty(&self) -> Type {
        match self {
            Type::Bool => self.clone(),
            Type::AbstractInt => self.clone(),
            Type::AbstractFloat => self.clone(),
            Type::I32 => self.clone(),
            Type::U32 => self.clone(),
            Type::F32 => self.clone(),
            Type::F16 => self.clone(),
            Type::Struct(_) => self.clone(),
            Type::Array(_, ty) => ty.ty(),
            Type::Vec(_, ty) => ty.ty(),
            Type::Mat(_, _, ty) => ty.inner_ty(),
            Type::Atomic(ty) => ty.ty(),
            Type::Ptr(_, ty) => ty.ty(),
            Type::Void => self.clone(),
        }
    }
}

impl Ty for Instance {
    fn ty(&self) -> Type {
        match self {
            Instance::Literal(l) => l.ty(),
            Instance::Struct(s) => s.ty(),
            Instance::Array(a) => a.ty(),
            Instance::Vec(v) => v.ty(),
            Instance::Mat(m) => m.ty(),
            Instance::Ptr(p) => p.ty(),
            Instance::Ref(r) => r.ty(),
            Instance::Atomic(a) => a.ty(),
            Instance::Type(t) => t.ty(),
            Instance::Void => Type::Void,
        }
    }
    fn inner_ty(&self) -> Type {
        match self {
            Instance::Literal(l) => l.inner_ty(),
            Instance::Struct(s) => s.inner_ty(),
            Instance::Array(a) => a.inner_ty(),
            Instance::Vec(v) => v.inner_ty(),
            Instance::Mat(m) => m.inner_ty(),
            Instance::Ptr(p) => p.inner_ty(),
            Instance::Ref(r) => r.inner_ty(),
            Instance::Atomic(a) => a.inner_ty(),
            Instance::Type(t) => t.inner_ty(),
            Instance::Void => Type::Void,
        }
    }
}

impl Ty for LiteralInstance {
    fn ty(&self) -> Type {
        match self {
            LiteralInstance::Bool(_) => Type::Bool,
            LiteralInstance::AbstractInt(_) => Type::AbstractInt,
            LiteralInstance::AbstractFloat(_) => Type::AbstractFloat,
            LiteralInstance::I32(_) => Type::I32,
            LiteralInstance::U32(_) => Type::U32,
            LiteralInstance::F32(_) => Type::F32,
            LiteralInstance::F16(_) => Type::F16,
        }
    }
}
impl Ty for StructInstance {
    fn ty(&self) -> Type {
        Type::Struct(self.ident().clone())
    }
}

impl Ty for ArrayInstance {
    fn ty(&self) -> Type {
        Type::Array(
            (!self.runtime_sized).then_some(self.n()),
            Box::new(self.inner_ty().clone()),
        )
    }
    fn inner_ty(&self) -> Type {
        self.get(0).unwrap().ty()
    }
}

impl Ty for VecInstance {
    fn ty(&self) -> Type {
        Type::Vec(self.n() as u8, Box::new(self.inner_ty()))
    }
    fn inner_ty(&self) -> Type {
        self.get(0).unwrap().ty()
    }
}

impl Ty for MatInstance {
    fn ty(&self) -> Type {
        Type::Mat(self.c() as u8, self.r() as u8, Box::new(self.inner_ty()))
    }
    fn inner_ty(&self) -> Type {
        self.get(0, 0).unwrap().ty()
    }
}

impl Ty for PtrInstance {
    fn ty(&self) -> Type {
        Type::Ptr(self.ptr.space, Box::new(self.ptr.ty.clone()))
    }
}

impl Ty for RefInstance {
    fn ty(&self) -> Type {
        self.ty.clone()
    }
}

impl Ty for AtomicInstance {
    fn ty(&self) -> Type {
        Type::Atomic(self.inner_ty().into())
    }
    fn inner_ty(&self) -> Type {
        self.inner().ty()
    }
}

pub trait EvalTy {
    fn eval_ty(&self, ctx: &mut Context) -> Result<Type, EvalError>;
}

impl<T: Ty> EvalTy for T {
    fn eval_ty(&self, _ctx: &mut Context) -> Result<Type, EvalError> {
        Ok(self.ty())
    }
}

impl EvalTy for Ident {
    fn eval_ty(&self, ctx: &mut Context) -> Result<Type, EvalError> {
        match self.name().as_str() {
            "bool" => Ok(Type::Bool),
            "i32" => Ok(Type::I32),
            "u32" => Ok(Type::U32),
            "f32" => Ok(Type::F32),
            "f16" => Ok(Type::F16),
            _ => {
                if let Some(ty) = ctx.source.resolve_alias(self) {
                    ty.eval_ty(ctx)
                } else if ctx.source.decl_struct(self).is_some() {
                    let ty = Type::Struct(self.clone());
                    Ok(ty)
                } else {
                    Err(EvalError::UnknownType(self.to_string()))
                }
            }
        }
    }
}

impl EvalTy for TypeExpression {
    fn eval_ty(&self, ctx: &mut Context) -> Result<Type, EvalError> {
        if let Some(tplt) = &self.template_args {
            match self.ident.name().as_str() {
                "array" => {
                    let tplt = ArrayTemplate::parse(tplt, ctx)?;
                    Ok(tplt.ty())
                }
                "vec2" | "vec3" | "vec4" => {
                    let tplt = VecTemplate::parse(tplt, ctx)?;
                    let n = self
                        .ident
                        .name()
                        .chars()
                        .nth(3)
                        .unwrap()
                        .to_digit(10)
                        .unwrap() as u8;
                    Ok(tplt.ty(n))
                }
                "mat2x2" | "mat2x3" | "mat2x4" | "mat3x2" | "mat3x3" | "mat3x4" | "mat4x2"
                | "mat4x3" | "mat4x4" => {
                    let tplt = MatTemplate::parse(tplt, ctx)?;
                    let c = self
                        .ident
                        .name()
                        .chars()
                        .nth(3)
                        .unwrap()
                        .to_digit(10)
                        .unwrap() as u8;
                    let r = self
                        .ident
                        .name()
                        .chars()
                        .nth(5)
                        .unwrap()
                        .to_digit(10)
                        .unwrap() as u8;
                    Ok(tplt.ty(c, r))
                }
                "ptr" => {
                    let tplt = PtrTemplate::parse(tplt, ctx)?;
                    Ok(tplt.ty())
                }
                "atomic" => {
                    let tplt = AtomicTemplate::parse(tplt, ctx)?;
                    Ok(tplt.ty())
                }
                _ => {
                    if let Some(ty) = ctx.source.resolve_alias(&self.ident) {
                        ty.eval_ty(ctx)
                    } else {
                        Err(EvalError::UnexpectedTemplate(self.ident.clone()))
                    }
                }
            }
        } else {
            self.ident.eval_ty(ctx)
        }
    }
}

impl EvalTy for Expression {
    fn eval_ty(&self, ctx: &mut Context) -> Result<Type, EvalError> {
        self.eval_value(ctx).and_then(|v| match v {
            Instance::Type(ty) => Ok(ty),
            _ => Err(EvalError::NotType(v)),
        })
    }
}
