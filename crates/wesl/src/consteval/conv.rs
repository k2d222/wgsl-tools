use super::{
    ops::Compwise, ConstEvalError, Instance, LiteralInstance, MatInner, MatInstance, Ty, Type,
    VecInner, VecInstance, BUILTIN_STRUCTURES,
};
use wgsl_parse::syntax::*;

pub trait Convert: Sized {
    // reference: https://www.w3.org/TR/WGSL/#conversion-rank
    // TODO: check that the `as` cast conversions are correct.
    fn convert_to(&self, ty: &Type) -> Result<Self, ConstEvalError>;
    fn convert_inner_to(&self, ty: &Type) -> Result<Self, ConstEvalError> {
        self.convert_to(ty)
    }
}

impl Convert for LiteralInstance {
    fn convert_to(&self, ty: &Type) -> Result<LiteralInstance, ConstEvalError> {
        if ty == &self.ty() {
            return Ok(self.clone());
        }
        match (self, ty) {
            (LiteralInstance::AbstractInt(n), Type::AbstractFloat) => {
                Ok(LiteralInstance::AbstractFloat(*n as f64))
            }
            (LiteralInstance::AbstractInt(n), Type::I32) => Ok(LiteralInstance::I32(*n as i32)),
            (LiteralInstance::AbstractInt(n), Type::U32) => Ok(LiteralInstance::U32(*n as u32)),
            (LiteralInstance::AbstractInt(n), Type::F32) => Ok(LiteralInstance::F32(*n as f32)),
            (LiteralInstance::AbstractInt(n), Type::F16) => Ok(LiteralInstance::F16(*n as f32)),
            (LiteralInstance::AbstractFloat(n), Type::F32) => Ok(LiteralInstance::F32(*n as f32)),
            (LiteralInstance::AbstractFloat(n), Type::F16) => Ok(LiteralInstance::F16(*n as f32)),
            _ => Err(ConstEvalError::ConversionFailure(self.ty(), ty.clone())),
        }
    }
}

impl<const N: usize> Convert for VecInner<N> {
    fn convert_to(&self, ty: &Type) -> Result<Self, ConstEvalError> {
        if let Type::Vec(n, c_ty) = ty {
            if *n == N as u8 {
                self.compwise_unary(|c| c.convert_to(c_ty))
            } else {
                Err(ConstEvalError::ConversionFailure(self.ty(), ty.clone()))
            }
        } else {
            Err(ConstEvalError::ConversionFailure(self.ty(), ty.clone()))
        }
    }
    fn convert_inner_to(&self, ty: &Type) -> Result<Self, ConstEvalError> {
        let ty = Type::Vec(N as u8, ty.clone().into());
        self.convert_to(&ty)
    }
}

impl Convert for VecInstance {
    fn convert_to(&self, ty: &Type) -> Result<VecInstance, ConstEvalError> {
        match self {
            Self::Vec2(v) => Ok(Self::Vec2(v.convert_to(ty)?)),
            Self::Vec3(v) => Ok(Self::Vec3(v.convert_to(ty)?)),
            Self::Vec4(v) => Ok(Self::Vec4(v.convert_to(ty)?)),
        }
    }
    fn convert_inner_to(&self, ty: &Type) -> Result<Self, ConstEvalError> {
        match self {
            Self::Vec2(v) => Ok(Self::Vec2(v.convert_inner_to(ty)?)),
            Self::Vec3(v) => Ok(Self::Vec3(v.convert_inner_to(ty)?)),
            Self::Vec4(v) => Ok(Self::Vec4(v.convert_inner_to(ty)?)),
        }
    }
}

impl<const C: usize, const R: usize> Convert for MatInner<C, R> {
    fn convert_to(&self, ty: &Type) -> Result<Self, ConstEvalError> {
        if let Type::Mat(c, r, c_ty) = ty {
            if *c == C as u8 && *r == R as u8 {
                self.compwise_unary(|c| c.convert_to(c_ty))
            } else {
                Err(ConstEvalError::ConversionFailure(self.ty(), ty.clone()))
            }
        } else {
            Err(ConstEvalError::ConversionFailure(self.ty(), ty.clone()))
        }
    }
    fn convert_inner_to(&self, ty: &Type) -> Result<Self, ConstEvalError> {
        let ty = Type::Mat(C as u8, R as u8, ty.clone().into());
        self.convert_to(&ty)
    }
}

impl Convert for MatInstance {
    fn convert_to(&self, ty: &Type) -> Result<MatInstance, ConstEvalError> {
        match self {
            Self::Mat2x2(m) => Ok(Self::Mat2x2(m.convert_to(ty)?)),
            Self::Mat2x3(m) => Ok(Self::Mat2x3(m.convert_to(ty)?)),
            Self::Mat2x4(m) => Ok(Self::Mat2x4(m.convert_to(ty)?)),
            Self::Mat3x2(m) => Ok(Self::Mat3x2(m.convert_to(ty)?)),
            Self::Mat3x3(m) => Ok(Self::Mat3x3(m.convert_to(ty)?)),
            Self::Mat3x4(m) => Ok(Self::Mat3x4(m.convert_to(ty)?)),
            Self::Mat4x2(m) => Ok(Self::Mat4x2(m.convert_to(ty)?)),
            Self::Mat4x3(m) => Ok(Self::Mat4x3(m.convert_to(ty)?)),
            Self::Mat4x4(m) => Ok(Self::Mat4x4(m.convert_to(ty)?)),
        }
    }
    fn convert_inner_to(&self, ty: &Type) -> Result<MatInstance, ConstEvalError> {
        match self {
            Self::Mat2x2(m) => Ok(Self::Mat2x2(m.convert_inner_to(ty)?)),
            Self::Mat2x3(m) => Ok(Self::Mat2x3(m.convert_inner_to(ty)?)),
            Self::Mat2x4(m) => Ok(Self::Mat2x4(m.convert_inner_to(ty)?)),
            Self::Mat3x2(m) => Ok(Self::Mat3x2(m.convert_inner_to(ty)?)),
            Self::Mat3x3(m) => Ok(Self::Mat3x3(m.convert_inner_to(ty)?)),
            Self::Mat3x4(m) => Ok(Self::Mat3x4(m.convert_inner_to(ty)?)),
            Self::Mat4x2(m) => Ok(Self::Mat4x2(m.convert_inner_to(ty)?)),
            Self::Mat4x3(m) => Ok(Self::Mat4x3(m.convert_inner_to(ty)?)),
            Self::Mat4x4(m) => Ok(Self::Mat4x4(m.convert_inner_to(ty)?)),
        }
    }
}

impl Convert for Instance {
    fn convert_to(&self, ty: &Type) -> Result<Instance, ConstEvalError> {
        if &self.ty() == ty {
            return Ok(self.clone());
        }
        match self {
            Self::Literal(l) => Ok(Self::Literal(l.convert_to(ty)?)),
            Self::Struct(_) => todo!(),
            Self::Array(_) => todo!(),
            Self::Vec(_) => todo!(),
            Self::Mat(_) => todo!(),
            Self::Ref(_) => todo!(),
            Self::Ptr(_) => todo!(),
        }
    }
}

pub fn conversion_rank(ty1: &Type, ty2: &Type) -> Option<u32> {
    // reference: https://www.w3.org/TR/WGSL/#conversion-rank
    match (ty1, ty2) {
        (_, _) if ty1 == ty2 => Some(0),
        (Type::AbstractInt, Type::AbstractFloat) => Some(5),
        (Type::AbstractInt, Type::I32) => Some(3),
        (Type::AbstractInt, Type::U32) => Some(4),
        (Type::AbstractInt, Type::F32) => Some(6),
        (Type::AbstractInt, Type::F16) => Some(7),
        (Type::AbstractFloat, Type::F32) => Some(1),
        (Type::AbstractFloat, Type::F16) => Some(2),
        (Type::Struct(s1), Type::Struct(s2)) => {
            if BUILTIN_STRUCTURES.contains(s1.as_str())
                && BUILTIN_STRUCTURES.contains(s2.as_str())
                && s1.ends_with("abstract")
            {
                if s2.ends_with("f32") {
                    Some(1)
                } else if s2.ends_with("f16") {
                    Some(2)
                } else {
                    None
                }
            } else {
                None
            }
        }
        (Type::Array(ty1), Type::Array(ty2)) => conversion_rank(ty1, ty2),
        (Type::Vec(n1, ty1), Type::Vec(n2, ty2)) if n1 == n2 => conversion_rank(ty1, ty2),
        (Type::Mat(c1, r1, ty1), Type::Mat(c2, r2, ty2)) if c1 == c2 && r1 == r2 => {
            conversion_rank(ty1, ty2)
        }
        _ => None,
    }
}

// performs overload resolution when two instances of T are involved (which is the most common).
// it just makes sure that the two types are the same.
// this is sufficient in most cases.
// TODO: check that it is sufficient
// TODO: find a better fn name
pub fn apply_conversion<T: Convert + Ty + Clone>(i1: &T, i2: &T) -> Result<(T, T), ConstEvalError> {
    i1.convert_to(&i2.ty())
        .map(|i1| (i1, i2.clone()))
        .or_else(|_| i2.convert_to(&i1.ty()).map(|i2| (i1.clone(), i2)))
}

// performs overload resolution when two instances of T are involved (which is the most common).
// it just makes sure that the two types are the same.
// this is sufficient in most cases.
// TODO: check that it is sufficient
// TODO: find a better fn name
pub fn get_conversion(ty1: &Type, ty2: &Type) -> Option<Type> {
    conversion_rank(ty1, ty2)
        .map(|_rank| ty2.clone())
        .or_else(|| conversion_rank(ty2, ty1).map(|_rank| ty1.clone()))
}
