use std::iter::zip;

use crate::EvalError;

use super::{
    convert, convert_inner, Instance, LiteralInstance, MatInner, MatInstance, Ty, VecInner,
    VecInstance,
};

use wgsl_parse::syntax::*;

type E = EvalError;

pub trait Compwise: Clone + Sized {
    fn compwise_unary_mut<F>(&mut self, f: F) -> Result<(), E>
    where
        F: Fn(&LiteralInstance) -> Result<LiteralInstance, E>;
    fn compwise_binary_mut<F>(&mut self, rhs: &Self, f: F) -> Result<(), E>
    where
        F: Fn(&LiteralInstance, &LiteralInstance) -> Result<LiteralInstance, E>;
    fn compwise_unary<F>(&self, f: F) -> Result<Self, E>
    where
        F: Fn(&LiteralInstance) -> Result<LiteralInstance, E>,
    {
        let mut res = self.clone();
        res.compwise_unary_mut(f)?;
        Ok(res)
    }

    fn compwise_binary<F>(&self, rhs: &Self, f: F) -> Result<Self, E>
    where
        F: Fn(&LiteralInstance, &LiteralInstance) -> Result<LiteralInstance, E>,
    {
        let mut res = self.clone();
        res.compwise_binary_mut(rhs, f)?;
        Ok(res)
    }
}

impl<const N: usize> Compwise for VecInner<N> {
    fn compwise_unary_mut<F>(&mut self, f: F) -> Result<(), E>
    where
        F: Fn(&LiteralInstance) -> Result<LiteralInstance, E>,
    {
        self.iter_mut()
            .map(|c| {
                *c = f(c)?;
                Ok(())
            })
            .collect::<Result<_, _>>()
    }

    fn compwise_binary_mut<F>(&mut self, rhs: &Self, f: F) -> Result<(), E>
    where
        F: Fn(&LiteralInstance, &LiteralInstance) -> Result<LiteralInstance, E>,
    {
        zip(self.iter_mut(), rhs.iter())
            .map(|(a, b)| {
                *a = f(a, b)?;
                Ok(())
            })
            .collect::<Result<_, _>>()
    }
}

impl Compwise for VecInstance {
    fn compwise_unary_mut<F>(&mut self, f: F) -> Result<(), E>
    where
        F: Fn(&LiteralInstance) -> Result<LiteralInstance, E>,
    {
        match self {
            VecInstance::Vec2(v) => v.compwise_unary_mut(f),
            VecInstance::Vec3(v) => v.compwise_unary_mut(f),
            VecInstance::Vec4(v) => v.compwise_unary_mut(f),
        }
    }

    fn compwise_binary_mut<F>(&mut self, rhs: &Self, f: F) -> Result<(), E>
    where
        F: Fn(&LiteralInstance, &LiteralInstance) -> Result<LiteralInstance, E>,
    {
        match (self, rhs) {
            (VecInstance::Vec2(lhs), VecInstance::Vec2(rhs)) => lhs.compwise_binary_mut(rhs, f),
            (VecInstance::Vec3(lhs), VecInstance::Vec3(rhs)) => lhs.compwise_binary_mut(rhs, f),
            (VecInstance::Vec4(lhs), VecInstance::Vec4(rhs)) => lhs.compwise_binary_mut(rhs, f),
            (lhs @ _, _) => Err(E::CompwiseBinary(lhs.ty(), rhs.ty())),
        }
    }
}

impl<const C: usize, const R: usize> Compwise for MatInner<C, R> {
    fn compwise_unary_mut<F>(&mut self, f: F) -> Result<(), E>
    where
        F: Fn(&LiteralInstance) -> Result<LiteralInstance, E>,
    {
        self.iter_mut()
            .map(|c| {
                *c = f(c)?;
                Ok(())
            })
            .collect::<Result<_, _>>()
    }

    fn compwise_binary_mut<F>(&mut self, rhs: &Self, f: F) -> Result<(), E>
    where
        F: Fn(&LiteralInstance, &LiteralInstance) -> Result<LiteralInstance, E>,
    {
        zip(self.iter_mut(), rhs.iter())
            .map(|(a, b)| {
                *a = f(a, b)?;
                Ok(())
            })
            .collect::<Result<_, _>>()
    }
}

impl Compwise for MatInstance {
    fn compwise_unary_mut<F>(&mut self, f: F) -> Result<(), E>
    where
        F: Fn(&LiteralInstance) -> Result<LiteralInstance, E>,
    {
        match self {
            MatInstance::Mat2x2(m) => m.compwise_unary_mut(f),
            MatInstance::Mat2x3(m) => m.compwise_unary_mut(f),
            MatInstance::Mat2x4(m) => m.compwise_unary_mut(f),
            MatInstance::Mat3x2(m) => m.compwise_unary_mut(f),
            MatInstance::Mat3x3(m) => m.compwise_unary_mut(f),
            MatInstance::Mat3x4(m) => m.compwise_unary_mut(f),
            MatInstance::Mat4x2(m) => m.compwise_unary_mut(f),
            MatInstance::Mat4x3(m) => m.compwise_unary_mut(f),
            MatInstance::Mat4x4(m) => m.compwise_unary_mut(f),
        }
    }

    fn compwise_binary_mut<F>(&mut self, rhs: &Self, f: F) -> Result<(), E>
    where
        F: Fn(&LiteralInstance, &LiteralInstance) -> Result<LiteralInstance, E>,
    {
        match (self, rhs) {
            (MatInstance::Mat2x2(lhs), MatInstance::Mat2x2(rhs)) => lhs.compwise_binary_mut(rhs, f),
            (MatInstance::Mat2x3(lhs), MatInstance::Mat2x3(rhs)) => lhs.compwise_binary_mut(rhs, f),
            (MatInstance::Mat2x4(lhs), MatInstance::Mat2x4(rhs)) => lhs.compwise_binary_mut(rhs, f),
            (MatInstance::Mat3x2(lhs), MatInstance::Mat3x2(rhs)) => lhs.compwise_binary_mut(rhs, f),
            (MatInstance::Mat3x3(lhs), MatInstance::Mat3x3(rhs)) => lhs.compwise_binary_mut(rhs, f),
            (MatInstance::Mat3x4(lhs), MatInstance::Mat3x4(rhs)) => lhs.compwise_binary_mut(rhs, f),
            (MatInstance::Mat4x2(lhs), MatInstance::Mat4x2(rhs)) => lhs.compwise_binary_mut(rhs, f),
            (MatInstance::Mat4x3(lhs), MatInstance::Mat4x3(rhs)) => lhs.compwise_binary_mut(rhs, f),
            (MatInstance::Mat4x4(lhs), MatInstance::Mat4x4(rhs)) => lhs.compwise_binary_mut(rhs, f),
            (lhs @ _, _) => Err(E::CompwiseBinary(lhs.ty(), rhs.ty())),
        }
    }
}

macro_rules! both {
    ($enum:ident::$var:ident, $lhs:ident, $rhs:ident) => {
        ($enum::$var($lhs), $enum::$var($rhs))
    };
    ($enum1:ident::$var1:ident, $enum2:ident::$var2:ident, $lhs:ident, $rhs:ident) => {
        ($enum1::$var1($lhs), $enum2::$var2($rhs)) | ($enum2::$var2($rhs), $enum1::$var1($lhs))
    };
}

// -------------------
// LOGICAL EXPRESSIONS
// -------------------
// reference: https://www.w3.org/TR/WGSL/#logical-expr

// logical and/or are part of bitwise and/or.
// short circuiting and/or is implemented in eval() because it needs context.

impl LiteralInstance {
    pub fn op_not(&self) -> Result<Self, E> {
        match self {
            Self::Bool(b) => Ok(Self::Bool(!b)),
            _ => Err(E::Unary(UnaryOperator::LogicalNegation, self.ty())),
        }
    }
}

impl Instance {
    pub fn op_not(&self) -> Result<Self, E> {
        match self {
            Self::Literal(lit) => lit.op_not().map(Into::into),
            _ => Err(E::Unary(UnaryOperator::LogicalNegation, self.ty())),
        }
    }
}

// ----------------------
// ARITHMETIC EXPRESSIONS
// ----------------------
// reference: https://www.w3.org/TR/WGSL/#arithmetic-expr

impl LiteralInstance {
    pub fn op_neg(&self) -> Result<Self, E> {
        match self {
            Self::AbstractInt(lhs) => lhs.checked_neg().ok_or(E::NegOverflow).map(Into::into),
            Self::AbstractFloat(lhs) => Ok((-lhs).into()),
            Self::I32(lhs) => lhs.checked_neg().ok_or(E::NegOverflow).map(Into::into),
            Self::F32(lhs) => Ok((-lhs).into()),
            Self::F16(lhs) => Ok((-lhs).into()),
            _ => Err(E::Unary(UnaryOperator::Negation, self.ty())),
        }
    }
    pub fn op_add(&self, rhs: &Self) -> Result<LiteralInstance, E> {
        let err = || E::Binary(BinaryOperator::Addition, self.ty(), rhs.ty());
        match convert(self, rhs).ok_or_else(err)? {
            both!(Self::AbstractInt, lhs, rhs) => {
                lhs.checked_add(rhs).ok_or(E::AddOverflow).map(Into::into)
            }
            both!(Self::AbstractFloat, lhs, rhs) => {
                let res = lhs + rhs;
                res.is_finite()
                    .then_some(res)
                    .ok_or(E::AddOverflow)
                    .map(Into::into)
            }
            both!(Self::I32, lhs, rhs) => {
                lhs.checked_add(rhs).ok_or(E::AddOverflow).map(Into::into)
            }
            both!(Self::U32, lhs, rhs) => {
                lhs.checked_add(rhs).ok_or(E::AddOverflow).map(Into::into)
            }
            both!(Self::F32, lhs, rhs) => {
                let res = lhs + rhs;
                res.is_finite()
                    .then_some(res)
                    .ok_or(E::AddOverflow)
                    .map(Into::into)
            }
            both!(Self::F16, lhs, rhs) => {
                let res = lhs + rhs;
                res.is_finite()
                    .then_some(res)
                    .ok_or(E::AddOverflow)
                    .map(Into::into)
            }
            _ => Err(err()),
        }
    }
    pub fn op_sub(&self, rhs: &Self) -> Result<LiteralInstance, E> {
        let err = || E::Binary(BinaryOperator::Subtraction, self.ty(), rhs.ty());
        match convert(self, rhs).ok_or_else(err)? {
            both!(Self::AbstractInt, lhs, rhs) => {
                lhs.checked_sub(rhs).ok_or(E::SubOverflow).map(Into::into)
            }
            both!(Self::AbstractFloat, lhs, rhs) => {
                let res = lhs - rhs;
                res.is_finite()
                    .then_some(res)
                    .ok_or(E::SubOverflow)
                    .map(Into::into)
            }
            both!(Self::I32, lhs, rhs) => {
                lhs.checked_sub(rhs).ok_or(E::SubOverflow).map(Into::into)
            }
            both!(Self::U32, lhs, rhs) => {
                lhs.checked_sub(rhs).ok_or(E::SubOverflow).map(Into::into)
            }
            both!(Self::F32, lhs, rhs) => {
                let res = lhs - rhs;
                res.is_finite()
                    .then_some(res)
                    .ok_or(E::SubOverflow)
                    .map(Into::into)
            }
            both!(Self::F16, lhs, rhs) => {
                let res = lhs - rhs;
                res.is_finite()
                    .then_some(res)
                    .ok_or(E::SubOverflow)
                    .map(Into::into)
            }
            _ => Err(err()),
        }
    }
    pub fn op_mul(&self, rhs: &Self) -> Result<LiteralInstance, E> {
        let err = || E::Binary(BinaryOperator::Multiplication, self.ty(), rhs.ty());
        match convert(self, rhs).ok_or_else(err)? {
            both!(Self::AbstractInt, lhs, rhs) => {
                lhs.checked_mul(rhs).ok_or(E::MulOverflow).map(Into::into)
            }
            both!(Self::AbstractFloat, lhs, rhs) => {
                let res = lhs * rhs;
                res.is_finite()
                    .then_some(res)
                    .ok_or(E::MulOverflow)
                    .map(Into::into)
            }
            both!(Self::I32, lhs, rhs) => {
                lhs.checked_mul(rhs).ok_or(E::MulOverflow).map(Into::into)
            }
            both!(Self::U32, lhs, rhs) => {
                lhs.checked_mul(rhs).ok_or(E::MulOverflow).map(Into::into)
            }
            both!(Self::F32, lhs, rhs) => {
                let res = lhs * rhs;
                res.is_finite()
                    .then_some(res)
                    .ok_or(E::MulOverflow)
                    .map(Into::into)
            }
            both!(Self::F16, lhs, rhs) => {
                let res = lhs * rhs;
                res.is_finite()
                    .then_some(res)
                    .ok_or(E::MulOverflow)
                    .map(Into::into)
            }
            _ => Err(err()),
        }
    }
    pub fn op_div(&self, rhs: &Self) -> Result<LiteralInstance, E> {
        let err = || E::Binary(BinaryOperator::Division, self.ty(), rhs.ty());
        match convert(self, rhs).ok_or_else(err)? {
            both!(Self::AbstractInt, lhs, rhs) => {
                lhs.checked_div(rhs).ok_or(E::DivByZero).map(Into::into)
            }
            both!(Self::AbstractFloat, lhs, rhs) => {
                let res = lhs / rhs;
                res.is_finite()
                    .then_some(res)
                    .ok_or(E::DivByZero)
                    .map(Into::into)
            }
            both!(Self::I32, lhs, rhs) => lhs.checked_div(rhs).ok_or(E::DivByZero).map(Into::into),
            both!(Self::U32, lhs, rhs) => lhs.checked_div(rhs).ok_or(E::DivByZero).map(Into::into),
            both!(Self::F32, lhs, rhs) => {
                let res = lhs / rhs;
                res.is_finite()
                    .then_some(res)
                    .ok_or(E::DivByZero)
                    .map(Into::into)
            }
            both!(Self::F16, lhs, rhs) => {
                let res = lhs / rhs;
                res.is_finite()
                    .then_some(res)
                    .ok_or(E::DivByZero)
                    .map(Into::into)
            }
            _ => Err(err()),
        }
    }
    pub fn op_rem(&self, rhs: &Self) -> Result<LiteralInstance, E> {
        let err = || E::Binary(BinaryOperator::Remainder, self.ty(), rhs.ty());
        match convert(self, rhs).ok_or_else(err)? {
            both!(Self::AbstractInt, lhs, rhs) => {
                lhs.checked_rem(rhs).ok_or(E::RemZeroDiv).map(Into::into)
            }
            both!(Self::AbstractFloat, lhs, rhs) => {
                let res = lhs % rhs;
                res.is_finite()
                    .then_some(res)
                    .ok_or(E::RemZeroDiv)
                    .map(Into::into)
            }
            both!(Self::I32, lhs, rhs) => lhs.checked_rem(rhs).ok_or(E::RemZeroDiv).map(Into::into),
            both!(Self::U32, lhs, rhs) => lhs.checked_rem(rhs).ok_or(E::RemZeroDiv).map(Into::into),
            both!(Self::F32, lhs, rhs) => {
                let res = lhs % rhs;
                res.is_finite()
                    .then_some(res)
                    .ok_or(E::RemZeroDiv)
                    .map(Into::into)
            }
            both!(Self::F16, lhs, rhs) => {
                let res = lhs % rhs;
                res.is_finite()
                    .then_some(res)
                    .ok_or(E::RemZeroDiv)
                    .map(Into::into)
            }
            _ => Err(err()),
        }
    }
    pub fn op_add_vec(&self, rhs: &VecInstance) -> Result<VecInstance, E> {
        rhs.op_add_sca(self)
    }
    pub fn op_sub_vec(&self, rhs: &VecInstance) -> Result<VecInstance, E> {
        let (lhs, rhs) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Subtraction, self.ty(), rhs.ty()))?;
        rhs.compwise_unary(|r| lhs.op_sub(r)).map(Into::into)
    }
    pub fn op_mul_vec(&self, rhs: &VecInstance) -> Result<VecInstance, E> {
        rhs.op_mul_sca(self)
    }
    pub fn op_div_vec(&self, rhs: &VecInstance) -> Result<VecInstance, E> {
        let (lhs, rhs) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Division, self.ty(), rhs.ty()))?;
        rhs.compwise_unary(|r| lhs.op_div(r)).map(Into::into)
    }
    pub fn op_rem_vec(&self, rhs: &VecInstance) -> Result<VecInstance, E> {
        let (lhs, rhs) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Remainder, self.ty(), rhs.ty()))?;
        rhs.compwise_unary(|r| lhs.op_rem(r)).map(Into::into)
    }
    pub fn op_mul_mat(&self, rhs: &MatInstance) -> Result<MatInstance, E> {
        rhs.op_mul_sca(self)
    }
}

impl<const N: usize> VecInner<N> {
    fn op_add(&self, rhs: &Self) -> Result<Self, E> {
        let (lhs, rhs) = convert(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Addition, self.ty(), rhs.ty()))?;
        lhs.compwise_binary(&rhs, |l, r| l.op_add(&r))
    }
    fn op_mul_sca(&self, rhs: &LiteralInstance) -> Result<VecInner<N>, E> {
        let (vec, sca) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Multiplication, self.ty(), rhs.ty()))?;
        vec.compwise_unary(|k| k.op_mul(&sca))
    }
    fn op_mul_mat<const C: usize>(&self, rhs: &MatInner<C, N>) -> Result<VecInner<C>, E> {
        // TODO must be float
        let (vec, mat) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Multiplication, self.ty(), rhs.ty()))?;
        let mat = mat.transpose();

        Ok(zip(&vec.components, &mat.components)
            .map(|(s, v)| v.op_mul_sca(s))
            .reduce(|a, b| a?.op_add(&b?))
            .unwrap()?)
    }
}

impl VecInstance {
    pub fn op_neg(&self) -> Result<Self, E> {
        self.compwise_unary(|c| c.op_neg())
    }
    pub fn op_add(&self, rhs: &Self) -> Result<Self, E> {
        let (lhs, rhs) = convert(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Addition, self.ty(), rhs.ty()))?;
        lhs.compwise_binary(&rhs, |l, r| l.op_add(&r))
    }
    pub fn op_sub(&self, rhs: &Self) -> Result<Self, E> {
        let (lhs, rhs) = convert(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Addition, self.ty(), rhs.ty()))?;
        lhs.compwise_binary(&rhs, |l, r| l.op_sub(&r))
    }
    pub fn op_mul(&self, rhs: &Self) -> Result<Self, E> {
        let (lhs, rhs) = convert(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Addition, self.ty(), rhs.ty()))?;
        lhs.compwise_binary(&rhs, |l, r| l.op_mul(&r))
    }
    pub fn op_div(&self, rhs: &Self) -> Result<Self, E> {
        let (lhs, rhs) = convert(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Addition, self.ty(), rhs.ty()))?;
        lhs.compwise_binary(&rhs, |l, r| l.op_div(&r))
    }
    pub fn op_rem(&self, rhs: &Self) -> Result<Self, E> {
        let (lhs, rhs) = convert(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Addition, self.ty(), rhs.ty()))?;
        lhs.compwise_binary(&rhs, |l, r| l.op_rem(&r))
    }
    pub fn op_add_sca(&self, rhs: &LiteralInstance) -> Result<Self, E> {
        let (lhs, rhs) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Addition, self.ty(), rhs.ty()))?;
        lhs.compwise_unary(|l| l.op_add(&rhs)).map(Into::into)
    }
    pub fn op_sub_sca(&self, rhs: &LiteralInstance) -> Result<Self, E> {
        let (lhs, rhs) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Subtraction, self.ty(), rhs.ty()))?;
        lhs.compwise_unary(|l| l.op_sub(&rhs)).map(Into::into)
    }
    pub fn op_mul_sca(&self, rhs: &LiteralInstance) -> Result<Self, E> {
        let (lhs, rhs) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Multiplication, self.ty(), rhs.ty()))?;
        lhs.compwise_unary(|l| l.op_mul(&rhs)).map(Into::into)
    }
    pub fn op_div_sca(&self, rhs: &LiteralInstance) -> Result<Self, E> {
        let (lhs, rhs) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Division, self.ty(), rhs.ty()))?;
        lhs.compwise_unary(|l| l.op_div(&rhs)).map(Into::into)
    }
    pub fn op_rem_sca(&self, rhs: &LiteralInstance) -> Result<Self, E> {
        let (lhs, rhs) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Division, self.ty(), rhs.ty()))?;
        lhs.compwise_unary(|l| l.op_rem(&rhs)).map(Into::into)
    }
    pub fn op_mul_mat(&self, rhs: &MatInstance) -> Result<Self, E> {
        match (self, rhs) {
            (Self::Vec2(lhs), MatInstance::Mat2x2(rhs)) => lhs.op_mul_mat(rhs).map(Self::Vec2),
            (Self::Vec2(lhs), MatInstance::Mat3x2(rhs)) => lhs.op_mul_mat(rhs).map(Self::Vec3),
            (Self::Vec2(lhs), MatInstance::Mat4x2(rhs)) => lhs.op_mul_mat(rhs).map(Self::Vec4),
            (Self::Vec3(lhs), MatInstance::Mat2x3(rhs)) => lhs.op_mul_mat(rhs).map(Self::Vec2),
            (Self::Vec3(lhs), MatInstance::Mat3x3(rhs)) => lhs.op_mul_mat(rhs).map(Self::Vec3),
            (Self::Vec3(lhs), MatInstance::Mat4x3(rhs)) => lhs.op_mul_mat(rhs).map(Self::Vec4),
            (Self::Vec4(lhs), MatInstance::Mat2x4(rhs)) => lhs.op_mul_mat(rhs).map(Self::Vec2),
            (Self::Vec4(lhs), MatInstance::Mat3x4(rhs)) => lhs.op_mul_mat(rhs).map(Self::Vec3),
            (Self::Vec4(lhs), MatInstance::Mat4x4(rhs)) => lhs.op_mul_mat(rhs).map(Self::Vec4),
            _ => Err(E::Binary(
                BinaryOperator::Multiplication,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
}

impl<const C: usize, const R: usize> MatInner<C, R> {
    pub fn op_add_mat(&self, rhs: &Self) -> Result<Self, E> {
        let (lhs, rhs) = convert(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Addition, self.ty(), rhs.ty()))?;
        lhs.compwise_binary(&rhs, |l, r| l.op_add(&r))
    }

    pub fn op_sub_mat(&self, rhs: &Self) -> Result<Self, E> {
        let (lhs, rhs) = convert(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Subtraction, self.ty(), rhs.ty()))?;
        lhs.compwise_binary(&rhs, |l, r| l.op_sub(&r))
    }

    pub fn op_mul_sca(&self, rhs: &LiteralInstance) -> Result<MatInner<C, R>, E> {
        // TODO must be float
        let (lhs, rhs) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Multiplication, self.ty(), rhs.ty()))?;
        lhs.compwise_unary(|l| l.op_mul(&rhs))
    }

    pub fn op_mul_vec(&self, rhs: &VecInner<C>) -> Result<VecInner<R>, E> {
        // TODO must be float
        let (lhs, rhs) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Multiplication, self.ty(), rhs.ty()))?;

        Ok(zip(&lhs.components, &rhs.components)
            .map(|(l, r)| l.op_mul_sca(r))
            .reduce(|l, r| l?.op_add(&r?))
            .unwrap()?)
    }

    pub fn op_mul_mat<const K: usize>(&self, rhs: &MatInner<K, C>) -> Result<MatInner<K, R>, E> {
        // TODO must be float
        let (lhs, rhs) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Multiplication, self.ty(), rhs.ty()))?;
        let lhs = lhs.transpose();

        Ok(MatInner::new(
            rhs.components
                .iter()
                .map(|l| {
                    Ok(VecInner::new(
                        lhs.components
                            .iter()
                            .map(|r| l.dot(r))
                            .collect::<Result<_, _>>()?,
                    ))
                })
                .collect::<Result<_, _>>()?,
        ))
    }
}

impl MatInstance {
    pub fn op_add(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Mat2x2, lhs, rhs) => lhs.op_add_mat(rhs).map(Self::Mat2x2),
            both!(Self::Mat2x3, lhs, rhs) => lhs.op_add_mat(rhs).map(Self::Mat2x3),
            both!(Self::Mat2x4, lhs, rhs) => lhs.op_add_mat(rhs).map(Self::Mat2x4),
            both!(Self::Mat3x2, lhs, rhs) => lhs.op_add_mat(rhs).map(Self::Mat3x2),
            both!(Self::Mat3x3, lhs, rhs) => lhs.op_add_mat(rhs).map(Self::Mat3x3),
            both!(Self::Mat3x4, lhs, rhs) => lhs.op_add_mat(rhs).map(Self::Mat3x4),
            both!(Self::Mat4x2, lhs, rhs) => lhs.op_add_mat(rhs).map(Self::Mat4x2),
            both!(Self::Mat4x3, lhs, rhs) => lhs.op_add_mat(rhs).map(Self::Mat4x3),
            both!(Self::Mat4x4, lhs, rhs) => lhs.op_add_mat(rhs).map(Self::Mat4x4),
            (lhs @ _, rhs @ _) => Err(E::Binary(BinaryOperator::Addition, lhs.ty(), rhs.ty())),
        }
    }
    pub fn op_sub_mat(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Mat2x2, lhs, rhs) => lhs.op_sub_mat(rhs).map(Self::Mat2x2),
            both!(Self::Mat2x3, lhs, rhs) => lhs.op_sub_mat(rhs).map(Self::Mat2x3),
            both!(Self::Mat2x4, lhs, rhs) => lhs.op_sub_mat(rhs).map(Self::Mat2x4),
            both!(Self::Mat3x2, lhs, rhs) => lhs.op_sub_mat(rhs).map(Self::Mat3x2),
            both!(Self::Mat3x3, lhs, rhs) => lhs.op_sub_mat(rhs).map(Self::Mat3x3),
            both!(Self::Mat3x4, lhs, rhs) => lhs.op_sub_mat(rhs).map(Self::Mat3x4),
            both!(Self::Mat4x2, lhs, rhs) => lhs.op_sub_mat(rhs).map(Self::Mat4x2),
            both!(Self::Mat4x3, lhs, rhs) => lhs.op_sub_mat(rhs).map(Self::Mat4x3),
            both!(Self::Mat4x4, lhs, rhs) => lhs.op_sub_mat(rhs).map(Self::Mat4x4),
            (lhs @ _, rhs @ _) => Err(E::Binary(BinaryOperator::Subtraction, lhs.ty(), rhs.ty())),
        }
    }
    pub fn op_mul_sca(&self, rhs: &LiteralInstance) -> Result<MatInstance, E> {
        match self {
            Self::Mat2x2(lhs) => lhs.op_mul_sca(rhs).map(Self::Mat2x2),
            Self::Mat2x3(lhs) => lhs.op_mul_sca(rhs).map(Self::Mat2x3),
            Self::Mat2x4(lhs) => lhs.op_mul_sca(rhs).map(Self::Mat2x4),
            Self::Mat3x2(lhs) => lhs.op_mul_sca(rhs).map(Self::Mat3x2),
            Self::Mat3x3(lhs) => lhs.op_mul_sca(rhs).map(Self::Mat3x3),
            Self::Mat3x4(lhs) => lhs.op_mul_sca(rhs).map(Self::Mat3x4),
            Self::Mat4x2(lhs) => lhs.op_mul_sca(rhs).map(Self::Mat4x2),
            Self::Mat4x3(lhs) => lhs.op_mul_sca(rhs).map(Self::Mat4x3),
            Self::Mat4x4(lhs) => lhs.op_mul_sca(rhs).map(Self::Mat4x4),
        }
    }
    pub fn op_mul_vec(&self, rhs: &VecInstance) -> Result<VecInstance, E> {
        match (self, rhs) {
            (Self::Mat2x2(lhs), VecInstance::Vec2(rhs)) => {
                lhs.op_mul_vec(rhs).map(VecInstance::Vec2)
            }
            (Self::Mat2x3(lhs), VecInstance::Vec2(rhs)) => {
                lhs.op_mul_vec(rhs).map(VecInstance::Vec3)
            }
            (Self::Mat2x4(lhs), VecInstance::Vec2(rhs)) => {
                lhs.op_mul_vec(rhs).map(VecInstance::Vec4)
            }

            (Self::Mat3x2(lhs), VecInstance::Vec3(rhs)) => {
                lhs.op_mul_vec(rhs).map(VecInstance::Vec2)
            }
            (Self::Mat3x3(lhs), VecInstance::Vec3(rhs)) => {
                lhs.op_mul_vec(rhs).map(VecInstance::Vec3)
            }
            (Self::Mat3x4(lhs), VecInstance::Vec3(rhs)) => {
                lhs.op_mul_vec(rhs).map(VecInstance::Vec4)
            }

            (Self::Mat4x2(lhs), VecInstance::Vec4(rhs)) => {
                lhs.op_mul_vec(rhs).map(VecInstance::Vec2)
            }
            (Self::Mat4x3(lhs), VecInstance::Vec4(rhs)) => {
                lhs.op_mul_vec(rhs).map(VecInstance::Vec3)
            }
            (Self::Mat4x4(lhs), VecInstance::Vec4(rhs)) => {
                lhs.op_mul_vec(rhs).map(VecInstance::Vec4)
            }

            (lhs @ _, rhs @ _) => Err(E::Binary(
                BinaryOperator::Multiplication,
                lhs.ty(),
                rhs.ty(),
            )),
        }
    }
    pub fn op_mul(&self, rhs: &Self) -> Result<MatInstance, E> {
        match (self, rhs) {
            (Self::Mat2x2(lhs), Self::Mat2x2(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat2x2),
            (Self::Mat2x2(lhs), Self::Mat3x2(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat3x2),
            (Self::Mat2x2(lhs), Self::Mat4x2(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat4x2),
            (Self::Mat2x3(lhs), Self::Mat2x2(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat2x3),
            (Self::Mat2x3(lhs), Self::Mat3x2(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat3x3),
            (Self::Mat2x3(lhs), Self::Mat4x2(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat4x3),
            (Self::Mat2x4(lhs), Self::Mat2x2(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat2x4),
            (Self::Mat2x4(lhs), Self::Mat3x2(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat3x4),
            (Self::Mat2x4(lhs), Self::Mat4x2(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat4x4),

            (Self::Mat3x2(lhs), Self::Mat2x3(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat2x2),
            (Self::Mat3x2(lhs), Self::Mat3x3(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat3x2),
            (Self::Mat3x2(lhs), Self::Mat4x3(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat4x2),
            (Self::Mat3x3(lhs), Self::Mat2x3(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat2x3),
            (Self::Mat3x3(lhs), Self::Mat3x3(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat3x3),
            (Self::Mat3x3(lhs), Self::Mat4x3(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat4x3),
            (Self::Mat3x4(lhs), Self::Mat2x3(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat2x4),
            (Self::Mat3x4(lhs), Self::Mat3x3(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat3x4),
            (Self::Mat3x4(lhs), Self::Mat4x3(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat4x4),

            (Self::Mat4x2(lhs), Self::Mat2x4(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat2x2),
            (Self::Mat4x2(lhs), Self::Mat3x4(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat3x2),
            (Self::Mat4x2(lhs), Self::Mat4x4(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat4x2),
            (Self::Mat4x3(lhs), Self::Mat2x4(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat2x3),
            (Self::Mat4x3(lhs), Self::Mat3x4(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat3x3),
            (Self::Mat4x3(lhs), Self::Mat4x4(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat4x3),
            (Self::Mat4x4(lhs), Self::Mat2x4(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat2x4),
            (Self::Mat4x4(lhs), Self::Mat3x4(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat3x4),
            (Self::Mat4x4(lhs), Self::Mat4x4(rhs)) => lhs.op_mul_mat(rhs).map(Self::Mat4x4),

            (lhs @ _, rhs @ _) => Err(E::Binary(
                BinaryOperator::Multiplication,
                lhs.ty(),
                rhs.ty(),
            )),
        }
    }
}

impl Instance {
    pub fn op_neg(&self) -> Result<Self, E> {
        match self {
            Self::Literal(lhs) => lhs.op_neg().map(Into::into),
            Self::Vec(lhs) => lhs.op_neg().map(Into::into),
            _ => Err(E::Unary(UnaryOperator::Negation, self.ty())),
        }
    }
    pub fn op_add(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => (lhs.op_add(rhs)).map(Into::into),
            (Self::Vec(lhs), Self::Literal(rhs)) => lhs.op_add_sca(rhs).map(Into::into),
            (Self::Literal(lhs), Self::Vec(rhs)) => lhs.op_add_vec(rhs).map(Into::into),
            both!(Self::Vec, lhs, rhs) => lhs.op_add(rhs).map(Into::into),
            both!(Self::Mat, lhs, rhs) => lhs.op_add(rhs).map(Into::into),
            _ => Err(E::Binary(BinaryOperator::Addition, self.ty(), rhs.ty())),
        }
    }
    pub fn op_sub(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs.op_sub(&rhs).map(Into::into),
            (Self::Vec(lhs), Self::Literal(rhs)) => lhs.op_sub_sca(rhs).map(Into::into),
            (Self::Literal(lhs), Self::Vec(rhs)) => lhs.op_sub_vec(rhs).map(Into::into),
            both!(Self::Vec, lhs, rhs) => lhs.op_sub(rhs).map(Into::into),
            both!(Self::Mat, lhs, rhs) => lhs.op_sub_mat(rhs).map(Into::into),
            _ => Err(E::Binary(BinaryOperator::Subtraction, self.ty(), rhs.ty())),
        }
    }
    pub fn op_mul(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs.op_mul(&rhs).map(Into::into),
            (Self::Vec(lhs), Self::Literal(rhs)) => lhs.op_mul_sca(rhs).map(Into::into),
            (Self::Literal(lhs), Self::Vec(rhs)) => lhs.op_mul_vec(rhs).map(Into::into),
            both!(Self::Vec, lhs, rhs) => lhs.op_mul(rhs).map(Into::into),
            (Self::Mat(lhs), Self::Literal(rhs)) => lhs.op_mul_sca(rhs).map(Into::into),
            (Self::Literal(lhs), Self::Mat(rhs)) => lhs.op_mul_mat(rhs).map(Into::into),
            (Self::Mat(lhs), Self::Vec(rhs)) => lhs.op_mul_vec(rhs).map(Into::into),
            (Self::Vec(lhs), Self::Mat(rhs)) => lhs.op_mul_mat(rhs).map(Into::into),
            both!(Self::Mat, lhs, rhs) => lhs.op_mul(rhs).map(Into::into),
            _ => Err(E::Binary(BinaryOperator::Subtraction, self.ty(), rhs.ty())),
        }
    }
    pub fn op_div(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs.op_div(&rhs).map(Into::into),
            (Self::Literal(s), Self::Vec(v)) => v.compwise_unary(|k| s.op_div(&k)).map(Into::into),
            (Self::Vec(v), Self::Literal(s)) => v.compwise_unary(|k| k.op_div(s)).map(Into::into),
            both!(Self::Vec, lhs, rhs) => lhs.op_div(rhs).map(Into::into),
            _ => Err(E::Binary(BinaryOperator::Subtraction, self.ty(), rhs.ty())),
        }
    }
    pub fn op_rem(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs.op_rem(rhs).map(Into::into),
            (Self::Literal(s), Self::Vec(v)) => v.compwise_unary(|k| s.op_rem(k)).map(Into::into),
            (Self::Vec(v), Self::Literal(s)) => v.compwise_unary(|k| k.op_rem(s)).map(Into::into),
            both!(Self::Vec, lhs, rhs) => lhs.op_rem(rhs).map(Into::into),
            _ => Err(E::Binary(BinaryOperator::Subtraction, self.ty(), rhs.ty())),
        }
    }
}

// ----------------------
// COMPARISON EXPRESSIONS
// ----------------------
// reference: https://www.w3.org/TR/WGSL/#comparison-expr

impl LiteralInstance {
    pub fn op_eq(&self, rhs: &Self) -> Result<bool, E> {
        let err = || E::Binary(BinaryOperator::Equality, self.ty(), rhs.ty());
        match convert(self, rhs).ok_or_else(err)? {
            both!(Self::Bool, lhs, rhs) => Ok(lhs == rhs),
            both!(Self::AbstractInt, lhs, rhs) => Ok(lhs == rhs),
            both!(Self::AbstractFloat, lhs, rhs) => Ok(lhs == rhs),
            both!(Self::I32, lhs, rhs) => Ok(lhs == rhs),
            both!(Self::U32, lhs, rhs) => Ok(lhs == rhs),
            both!(Self::F32, lhs, rhs) => Ok(lhs == rhs),
            both!(Self::F16, lhs, rhs) => Ok(lhs == rhs),
            _ => Err(err()),
        }
    }
    pub fn op_ne(&self, rhs: &Self) -> Result<bool, E> {
        let err = || E::Binary(BinaryOperator::Inequality, self.ty(), rhs.ty());
        match convert(self, rhs).ok_or_else(err)? {
            both!(Self::Bool, lhs, rhs) => Ok(lhs != rhs),
            both!(Self::AbstractInt, lhs, rhs) => Ok(lhs != rhs),
            both!(Self::AbstractFloat, lhs, rhs) => Ok(lhs != rhs),
            both!(Self::I32, lhs, rhs) => Ok(lhs != rhs),
            both!(Self::U32, lhs, rhs) => Ok(lhs != rhs),
            both!(Self::F32, lhs, rhs) => Ok(lhs != rhs),
            both!(Self::F16, lhs, rhs) => Ok(lhs != rhs),
            _ => Err(err()),
        }
    }
    pub fn op_lt(&self, rhs: &Self) -> Result<bool, E> {
        let err = || E::Binary(BinaryOperator::LessThan, self.ty(), rhs.ty());
        match convert(self, rhs).ok_or_else(err)? {
            both!(Self::Bool, lhs, rhs) => Ok(lhs < rhs),
            both!(Self::AbstractInt, lhs, rhs) => Ok(lhs < rhs),
            both!(Self::AbstractFloat, lhs, rhs) => Ok(lhs < rhs),
            both!(Self::I32, lhs, rhs) => Ok(lhs < rhs),
            both!(Self::U32, lhs, rhs) => Ok(lhs < rhs),
            both!(Self::F32, lhs, rhs) => Ok(lhs < rhs),
            both!(Self::F16, lhs, rhs) => Ok(lhs < rhs),
            _ => Err(err()),
        }
    }
    pub fn op_le(&self, rhs: &Self) -> Result<bool, E> {
        let err = || E::Binary(BinaryOperator::LessThanEqual, self.ty(), rhs.ty());
        match convert(self, rhs).ok_or_else(err)? {
            both!(Self::Bool, lhs, rhs) => Ok(lhs <= rhs),
            both!(Self::AbstractInt, lhs, rhs) => Ok(lhs <= rhs),
            both!(Self::AbstractFloat, lhs, rhs) => Ok(lhs <= rhs),
            both!(Self::I32, lhs, rhs) => Ok(lhs <= rhs),
            both!(Self::U32, lhs, rhs) => Ok(lhs <= rhs),
            both!(Self::F32, lhs, rhs) => Ok(lhs <= rhs),
            both!(Self::F16, lhs, rhs) => Ok(lhs <= rhs),
            _ => Err(err()),
        }
    }
    pub fn op_gt(&self, rhs: &Self) -> Result<bool, E> {
        let err = || E::Binary(BinaryOperator::GreaterThan, self.ty(), rhs.ty());
        match convert(self, rhs).ok_or_else(err)? {
            both!(Self::Bool, lhs, rhs) => Ok(lhs > rhs),
            both!(Self::AbstractInt, lhs, rhs) => Ok(lhs > rhs),
            both!(Self::AbstractFloat, lhs, rhs) => Ok(lhs > rhs),
            both!(Self::I32, lhs, rhs) => Ok(lhs > rhs),
            both!(Self::U32, lhs, rhs) => Ok(lhs > rhs),
            both!(Self::F32, lhs, rhs) => Ok(lhs > rhs),
            both!(Self::F16, lhs, rhs) => Ok(lhs > rhs),
            _ => Err(err()),
        }
    }
    pub fn op_ge(&self, rhs: &Self) -> Result<bool, E> {
        let err = || E::Binary(BinaryOperator::GreaterThanEqual, self.ty(), rhs.ty());
        match convert(self, rhs).ok_or_else(err)? {
            both!(Self::Bool, lhs, rhs) => Ok(lhs >= rhs),
            both!(Self::AbstractInt, lhs, rhs) => Ok(lhs >= rhs),
            both!(Self::AbstractFloat, lhs, rhs) => Ok(lhs >= rhs),
            both!(Self::I32, lhs, rhs) => Ok(lhs >= rhs),
            both!(Self::U32, lhs, rhs) => Ok(lhs >= rhs),
            both!(Self::F32, lhs, rhs) => Ok(lhs >= rhs),
            both!(Self::F16, lhs, rhs) => Ok(lhs >= rhs),
            _ => Err(err()),
        }
    }
}

impl VecInstance {
    pub fn op_eq(&self, rhs: &Self) -> Result<VecInstance, E> {
        let (lhs, rhs) = convert(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Equality, self.ty(), rhs.ty()))?;
        lhs.compwise_binary(&rhs, |l, r| l.op_eq(&r).map(Into::into))
    }
    pub fn op_ne(&self, rhs: &Self) -> Result<VecInstance, E> {
        let (lhs, rhs) = convert(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Inequality, self.ty(), rhs.ty()))?;
        lhs.compwise_binary(&rhs, |l, r| l.op_ne(&r).map(Into::into))
    }
    pub fn op_lt(&self, rhs: &Self) -> Result<VecInstance, E> {
        let (lhs, rhs) = convert(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::LessThan, self.ty(), rhs.ty()))?;
        lhs.compwise_binary(&rhs, |l, r| l.op_lt(&r).map(Into::into))
    }
    pub fn op_le(&self, rhs: &Self) -> Result<VecInstance, E> {
        let (lhs, rhs) = convert(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::LessThanEqual, self.ty(), rhs.ty()))?;
        lhs.compwise_binary(&rhs, |l, r| l.op_le(&r).map(Into::into))
    }
    pub fn op_gt(&self, rhs: &Self) -> Result<VecInstance, E> {
        let (lhs, rhs) = convert(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::GreaterThan, self.ty(), rhs.ty()))?;
        lhs.compwise_binary(&rhs, |l, r| l.op_gt(&r).map(Into::into))
    }
    pub fn op_ge(&self, rhs: &Self) -> Result<VecInstance, E> {
        let (lhs, rhs) = convert(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::GreaterThanEqual, self.ty(), rhs.ty()))?;
        lhs.compwise_binary(&rhs, |l, r| l.op_ge(&r).map(Into::into))
    }
}

impl Instance {
    pub fn op_eq(&self, rhs: &Self) -> Result<Instance, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs
                .op_eq(rhs)
                .map(|b| Self::Literal(LiteralInstance::Bool(b))),
            both!(Self::Vec, lhs, rhs) => lhs.op_eq(rhs).map(Into::into),
            _ => Err(E::Binary(BinaryOperator::Equality, self.ty(), rhs.ty())),
        }
    }
    pub fn op_ne(&self, rhs: &Self) -> Result<Instance, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs
                .op_ne(rhs)
                .map(|b| Self::Literal(LiteralInstance::Bool(b))),
            both!(Self::Vec, lhs, rhs) => lhs.op_ne(rhs).map(Into::into),
            _ => Err(E::Binary(BinaryOperator::Inequality, self.ty(), rhs.ty())),
        }
    }
    pub fn op_lt(&self, rhs: &Self) -> Result<Instance, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs
                .op_lt(rhs)
                .map(|b| Self::Literal(LiteralInstance::Bool(b))),
            both!(Self::Vec, lhs, rhs) => lhs.op_lt(rhs).map(Into::into),
            _ => Err(E::Binary(BinaryOperator::LessThan, self.ty(), rhs.ty())),
        }
    }
    pub fn op_le(&self, rhs: &Self) -> Result<Instance, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs
                .op_le(rhs)
                .map(|b| Self::Literal(LiteralInstance::Bool(b))),
            both!(Self::Vec, lhs, rhs) => lhs.op_le(rhs).map(Into::into),
            _ => Err(E::Binary(
                BinaryOperator::LessThanEqual,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
    pub fn op_gt(&self, rhs: &Self) -> Result<Instance, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs
                .op_gt(rhs)
                .map(|b| Self::Literal(LiteralInstance::Bool(b))),
            both!(Self::Vec, lhs, rhs) => lhs.op_gt(rhs).map(Into::into),
            _ => Err(E::Binary(BinaryOperator::GreaterThan, self.ty(), rhs.ty())),
        }
    }
    pub fn op_ge(&self, rhs: &Self) -> Result<Instance, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs
                .op_ge(rhs)
                .map(|b| Self::Literal(LiteralInstance::Bool(b))),
            both!(Self::Vec, lhs, rhs) => lhs.op_ge(rhs).map(Into::into),
            _ => Err(E::Binary(
                BinaryOperator::GreaterThanEqual,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
}

// ---------------
// BIT EXPRESSIONS
// ---------------
// reference: https://www.w3.org/TR/WGSL/#bit-expr

impl LiteralInstance {
    pub fn op_bitnot(&self) -> Result<Self, E> {
        match self {
            Self::AbstractInt(n) => Ok(Self::AbstractInt(!n)),
            Self::I32(n) => Ok(Self::I32(!n)),
            Self::U32(n) => Ok(Self::U32(!n)),
            _ => Err(E::Unary(UnaryOperator::BitwiseComplement, self.ty())),
        }
    }
    pub fn op_bitor(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Bool, rhs, lhs) => Ok(Self::Bool(lhs | rhs)),
            both!(Self::AbstractInt, rhs, lhs) => Ok(Self::AbstractInt(lhs | rhs)),
            both!(Self::I32, rhs, lhs) => Ok(Self::I32(lhs | rhs)),
            both!(Self::U32, rhs, lhs) => Ok(Self::U32(lhs | rhs)),
            _ => Err(E::Binary(BinaryOperator::BitwiseOr, self.ty(), rhs.ty())),
        }
    }
    pub fn op_bitand(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Bool, rhs, lhs) => Ok(Self::Bool(lhs & rhs)),
            both!(Self::AbstractInt, rhs, lhs) => Ok(Self::AbstractInt(lhs & rhs)),
            both!(Self::I32, rhs, lhs) => Ok(Self::I32(lhs & rhs)),
            both!(Self::U32, rhs, lhs) => Ok(Self::U32(lhs & rhs)),
            _ => Err(E::Binary(BinaryOperator::BitwiseAnd, self.ty(), rhs.ty())),
        }
    }
    pub fn op_bitxor(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::AbstractInt, rhs, lhs) => Ok(Self::AbstractInt(lhs ^ rhs)),
            both!(Self::I32, rhs, lhs) => Ok(Self::I32(lhs ^ rhs)),
            both!(Self::U32, rhs, lhs) => Ok(Self::U32(lhs ^ rhs)),
            _ => Err(E::Binary(BinaryOperator::BitwiseAnd, self.ty(), rhs.ty())),
        }
    }
    pub fn op_shl(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            (Self::I32(lhs), Self::U32(rhs)) => lhs
                .checked_shl(*rhs)
                .map(Into::into)
                .ok_or(E::ShlOverflow(*rhs)),
            (Self::U32(lhs), Self::U32(rhs)) => lhs
                .checked_shl(*rhs)
                .map(Into::into)
                .ok_or(E::ShlOverflow(*rhs)),
            _ => Err(E::Binary(BinaryOperator::BitwiseAnd, self.ty(), rhs.ty())),
        }
    }
    pub fn op_shr(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            (Self::I32(lhs), Self::U32(rhs)) => lhs
                .checked_shr(*rhs)
                .map(Into::into)
                .ok_or(E::ShlOverflow(*rhs)),
            (Self::U32(lhs), Self::U32(rhs)) => lhs
                .checked_shr(*rhs)
                .map(Into::into)
                .ok_or(E::ShlOverflow(*rhs)),
            _ => Err(E::Binary(BinaryOperator::BitwiseAnd, self.ty(), rhs.ty())),
        }
    }
}

impl VecInstance {
    pub fn op_bitnot(&self) -> Result<Self, E> {
        self.compwise_unary(|c| LiteralInstance::op_bitnot(&c))
    }
    pub fn op_bitor(&self, rhs: &Self) -> Result<Self, E> {
        self.compwise_binary(rhs, |l, r| l.op_bitor(&r))
    }
    pub fn op_bitand(&self, rhs: &Self) -> Result<Self, E> {
        self.compwise_binary(rhs, |l, r| l.op_bitand(&r))
    }
    pub fn op_bitxor(&self, rhs: &Self) -> Result<Self, E> {
        self.compwise_binary(rhs, |l, r| l.op_bitxor(&r))
    }
    pub fn op_shl(&self, rhs: &Self) -> Result<Self, E> {
        self.compwise_binary(rhs, |l, r| l.op_shl(&r))
    }
    pub fn op_shr(&self, rhs: &Self) -> Result<Self, E> {
        self.compwise_binary(rhs, |l, r| l.op_shr(&r))
    }
}

impl Instance {
    pub fn op_bitnot(&self) -> Result<Self, E> {
        match self {
            Instance::Literal(l) => l.op_bitnot().map(Into::into),
            Instance::Vec(v) => v.op_bitnot().map(Into::into),
            _ => Err(E::Unary(UnaryOperator::BitwiseComplement, self.ty())),
        }
    }
    pub fn op_bitor(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs.op_bitor(rhs).map(Into::into),
            both!(Self::Vec, lhs, rhs) => lhs.op_bitor(rhs).map(Into::into),
            _ => Err(E::Unary(UnaryOperator::LogicalNegation, self.ty())),
        }
    }
    pub fn op_bitand(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs.op_bitand(rhs).map(Into::into),
            both!(Self::Vec, lhs, rhs) => lhs.op_bitand(rhs).map(Into::into),
            _ => Err(E::Unary(UnaryOperator::LogicalNegation, self.ty())),
        }
    }
    pub fn op_bitxor(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs.op_bitxor(rhs).map(Into::into),
            both!(Self::Vec, lhs, rhs) => lhs.op_bitxor(rhs).map(Into::into),
            _ => Err(E::Unary(UnaryOperator::LogicalNegation, self.ty())),
        }
    }
    pub fn op_shl(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs.op_shl(rhs).map(Into::into),
            both!(Self::Vec, lhs, rhs) => lhs.op_shl(rhs).map(Into::into),
            _ => Err(E::Unary(UnaryOperator::LogicalNegation, self.ty())),
        }
    }
    pub fn op_shr(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs.op_shr(rhs).map(Into::into),
            both!(Self::Vec, lhs, rhs) => lhs.op_shr(rhs).map(Into::into),
            _ => Err(E::Unary(UnaryOperator::LogicalNegation, self.ty())),
        }
    }
}
