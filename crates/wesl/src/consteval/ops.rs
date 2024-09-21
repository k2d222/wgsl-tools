use std::{
    iter::zip,
    ops::{Add, Div, Mul, Neg, Rem, Sub},
};

use crate::{consteval::conv::get_conversion, ConstEvalError};

use super::{
    apply_conversion, conv::Convert, Instance, LiteralInstance, MatInner, MatInstance, Ty,
    VecInner, VecInstance,
};

use wgsl_parse::syntax::*;

type E = ConstEvalError;

pub trait Compwise: Clone + Sized {
    fn compwise_unary_mut<F>(&mut self, f: F) -> Result<(), E>
    where
        F: Fn(LiteralInstance) -> Result<LiteralInstance, E>;
    fn compwise_binary_mut<F>(&mut self, rhs: &Self, f: F) -> Result<(), E>
    where
        F: Fn(LiteralInstance, LiteralInstance) -> Result<LiteralInstance, E>;
    fn compwise_unary<F>(&self, f: F) -> Result<Self, E>
    where
        F: Fn(LiteralInstance) -> Result<LiteralInstance, E>,
    {
        let mut res = self.clone();
        res.compwise_unary_mut(f)?;
        Ok(res)
    }

    fn compwise_binary<F>(&self, rhs: &Self, f: F) -> Result<Self, E>
    where
        F: Fn(LiteralInstance, LiteralInstance) -> Result<LiteralInstance, E>,
    {
        let mut res = self.clone();
        res.compwise_binary_mut(rhs, f)?;
        Ok(res)
    }
}

impl<const N: usize> Compwise for VecInner<N> {
    fn compwise_unary_mut<F>(&mut self, f: F) -> Result<(), E>
    where
        F: Fn(LiteralInstance) -> Result<LiteralInstance, E>,
    {
        self.iter_mut()
            .map(|c| {
                *c = f(*c)?;
                Ok(())
            })
            .collect::<Result<_, _>>()
    }

    fn compwise_binary_mut<F>(&mut self, rhs: &Self, f: F) -> Result<(), E>
    where
        F: Fn(LiteralInstance, LiteralInstance) -> Result<LiteralInstance, E>,
    {
        zip(self.iter_mut(), rhs.iter())
            .map(|(a, b)| {
                *a = f(*a, *b)?;
                Ok(())
            })
            .collect::<Result<_, _>>()
    }
}

impl Compwise for VecInstance {
    fn compwise_unary_mut<F>(&mut self, f: F) -> Result<(), E>
    where
        F: Fn(LiteralInstance) -> Result<LiteralInstance, E>,
    {
        match self {
            VecInstance::Vec2(v) => v.compwise_unary_mut(f),
            VecInstance::Vec3(v) => v.compwise_unary_mut(f),
            VecInstance::Vec4(v) => v.compwise_unary_mut(f),
        }
    }

    fn compwise_binary_mut<F>(&mut self, rhs: &Self, f: F) -> Result<(), E>
    where
        F: Fn(LiteralInstance, LiteralInstance) -> Result<LiteralInstance, E>,
    {
        match (self, rhs) {
            (VecInstance::Vec2(lhs), VecInstance::Vec2(rhs)) => lhs.compwise_binary_mut(rhs, f),
            (VecInstance::Vec3(lhs), VecInstance::Vec3(rhs)) => lhs.compwise_binary_mut(rhs, f),
            (VecInstance::Vec4(lhs), VecInstance::Vec4(rhs)) => lhs.compwise_binary_mut(rhs, f),
            (lhs @ _, _) => Err(E::InvalidCompwiseBinary(lhs.ty(), rhs.ty())),
        }
    }
}

impl<const C: usize, const R: usize> Compwise for MatInner<C, R> {
    fn compwise_unary_mut<F>(&mut self, f: F) -> Result<(), E>
    where
        F: Fn(LiteralInstance) -> Result<LiteralInstance, E>,
    {
        self.iter_mut()
            .map(|c| {
                *c = f(*c)?;
                Ok(())
            })
            .collect::<Result<_, _>>()
    }

    fn compwise_binary_mut<F>(&mut self, rhs: &Self, f: F) -> Result<(), E>
    where
        F: Fn(LiteralInstance, LiteralInstance) -> Result<LiteralInstance, E>,
    {
        zip(self.iter_mut(), rhs.iter())
            .map(|(a, b)| {
                *a = f(*a, *b)?;
                Ok(())
            })
            .collect::<Result<_, _>>()
    }
}

impl Compwise for MatInstance {
    fn compwise_unary_mut<F>(&mut self, f: F) -> Result<(), E>
    where
        F: Fn(LiteralInstance) -> Result<LiteralInstance, E>,
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
        F: Fn(LiteralInstance, LiteralInstance) -> Result<LiteralInstance, E>,
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
            (lhs @ _, _) => Err(E::InvalidCompwiseBinary(lhs.ty(), rhs.ty())),
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

// ----------------------
// ARITHMETIC EXPRESSIONS
// ----------------------
// reference: https://www.w3.org/TR/WGSL/#arithmetic-expr

impl LiteralInstance {
    fn op_neg(&self) -> Result<LiteralInstance, E> {
        match self {
            Self::AbstractInt(lhs) => lhs.checked_neg().ok_or(E::NegOverflow).map(Into::into),
            Self::AbstractFloat(lhs) => Ok((-lhs).into()),
            Self::I32(lhs) => lhs.checked_neg().ok_or(E::NegOverflow).map(Into::into),
            Self::F32(lhs) => Ok((-lhs).into()),
            Self::F16(lhs) => Ok((-lhs).into()),
            _ => Err(E::InvalidUnary(UnaryOperator::Negation, self.ty())),
        }
    }
    fn op_add(&self, rhs: &Self) -> Result<LiteralInstance, E> {
        match apply_conversion(self, rhs)? {
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
            _ => Err(E::InvalidBinary(
                BinaryOperator::Addition,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
    fn op_sub(&self, rhs: &Self) -> Result<LiteralInstance, E> {
        match apply_conversion(self, rhs)? {
            both!(Self::AbstractInt, lhs, rhs) => {
                lhs.checked_sub(rhs).ok_or(E::SubUnderflow).map(Into::into)
            }
            both!(Self::AbstractFloat, lhs, rhs) => {
                let res = lhs - rhs;
                res.is_finite()
                    .then_some(res)
                    .ok_or(E::SubUnderflow)
                    .map(Into::into)
            }
            both!(Self::I32, lhs, rhs) => {
                lhs.checked_sub(rhs).ok_or(E::SubUnderflow).map(Into::into)
            }
            both!(Self::U32, lhs, rhs) => {
                lhs.checked_sub(rhs).ok_or(E::SubUnderflow).map(Into::into)
            }
            both!(Self::F32, lhs, rhs) => {
                let res = lhs - rhs;
                res.is_finite()
                    .then_some(res)
                    .ok_or(E::SubUnderflow)
                    .map(Into::into)
            }
            both!(Self::F16, lhs, rhs) => {
                let res = lhs - rhs;
                res.is_finite()
                    .then_some(res)
                    .ok_or(E::SubUnderflow)
                    .map(Into::into)
            }
            _ => Err(E::InvalidBinary(
                BinaryOperator::Subtraction,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
    fn op_mul(&self, rhs: &Self) -> Result<LiteralInstance, E> {
        match apply_conversion(self, rhs)? {
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
            _ => Err(E::InvalidBinary(
                BinaryOperator::Multiplication,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
    fn op_div(&self, rhs: &Self) -> Result<LiteralInstance, E> {
        match apply_conversion(self, rhs)? {
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
            _ => Err(E::InvalidBinary(
                BinaryOperator::Division,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
    fn op_rem(&self, rhs: &Self) -> Result<LiteralInstance, E> {
        match apply_conversion(self, rhs)? {
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
            _ => Err(E::InvalidBinary(
                BinaryOperator::Remainder,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
}

impl Neg for LiteralInstance {
    type Output = Result<LiteralInstance, E>;
    fn neg(self) -> Result<LiteralInstance, E> {
        self.op_neg()
    }
}
impl Add for LiteralInstance {
    type Output = Result<LiteralInstance, E>;
    fn add(self, rhs: Self) -> Self::Output {
        self.op_add(&rhs)
    }
}
impl Sub for LiteralInstance {
    type Output = Result<LiteralInstance, E>;
    fn sub(self, rhs: Self) -> Self::Output {
        self.op_sub(&rhs)
    }
}
impl Mul for LiteralInstance {
    type Output = Result<LiteralInstance, E>;
    fn mul(self, rhs: Self) -> Self::Output {
        self.op_mul(&rhs)
    }
}
impl Div for LiteralInstance {
    type Output = Result<LiteralInstance, E>;
    fn div(self, rhs: Self) -> Self::Output {
        self.op_div(&rhs)
    }
}
impl Rem for LiteralInstance {
    type Output = Result<LiteralInstance, E>;
    fn rem(self, rhs: Self) -> Self::Output {
        self.op_rem(&rhs)
    }
}

impl<const N: usize> VecInner<N> {
    fn op_add_vec(&self, rhs: &Self) -> Result<Self, E> {
        let (lhs, rhs) = apply_conversion(self, rhs)?;
        lhs.compwise_binary(&rhs, |l, r| l + r)
    }
    fn op_sub_vec(&self, rhs: &Self) -> Result<Self, E> {
        let (lhs, rhs) = apply_conversion(self, rhs)?;
        lhs.compwise_binary(&rhs, |l, r| l - r)
    }
    fn op_mul_vec(&self, rhs: &Self) -> Result<Self, E> {
        let (lhs, rhs) = apply_conversion(self, rhs)?;
        lhs.compwise_binary(&rhs, |l, r| l * r)
    }
    fn op_div_vec(&self, rhs: &Self) -> Result<Self, E> {
        let (lhs, rhs) = apply_conversion(self, rhs)?;
        lhs.compwise_binary(&rhs, |l, r| l / r)
    }
    fn op_rem_vec(&self, rhs: &Self) -> Result<Self, E> {
        let (lhs, rhs) = apply_conversion(self, rhs)?;
        lhs.compwise_binary(&rhs, |l, r| l % r)
    }
    fn op_mul_scalar(&self, scalar: &LiteralInstance) -> Result<VecInner<N>, E> {
        let ty = get_conversion(&self.inner_ty(), &scalar.inner_ty()).ok_or_else(|| {
            E::InvalidBinary(BinaryOperator::Multiplication, self.ty(), scalar.ty())
        })?;
        let (vec, scalar) = (self.convert_inner_to(&ty)?, scalar.convert_to(&ty)?);

        vec.compwise_unary(|k| k * scalar)
    }
    fn op_mul_mat<const C: usize>(&self, rhs: &MatInner<C, N>) -> Result<VecInner<C>, E> {
        // TODO must be float
        let ty = get_conversion(&self.inner_ty(), &rhs.inner_ty())
            .ok_or_else(|| E::InvalidBinary(BinaryOperator::Multiplication, self.ty(), rhs.ty()))?;

        let (vec, mat) = (self.convert_inner_to(&ty)?, rhs.convert_inner_to(&ty)?);
        let mat = mat.transpose();

        Ok(zip(&vec.components, &mat.components)
            .map(|(s, v)| v.op_mul_scalar(s))
            .reduce(|a, b| a?.op_add_vec(&b?))
            .unwrap()?)
    }
}

impl VecInstance {
    fn op_add_vec(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Vec2, lhs, rhs) => lhs.op_add_vec(rhs).map(Self::Vec2),
            both!(Self::Vec3, lhs, rhs) => lhs.op_add_vec(rhs).map(Self::Vec3),
            both!(Self::Vec4, lhs, rhs) => lhs.op_add_vec(rhs).map(Self::Vec4),
            _ => Err(E::InvalidBinary(
                BinaryOperator::Multiplication,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
    fn op_sub_vec(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Vec2, lhs, rhs) => lhs.op_sub_vec(rhs).map(Self::Vec2),
            both!(Self::Vec3, lhs, rhs) => lhs.op_sub_vec(rhs).map(Self::Vec3),
            both!(Self::Vec4, lhs, rhs) => lhs.op_sub_vec(rhs).map(Self::Vec4),
            _ => Err(E::InvalidBinary(
                BinaryOperator::Multiplication,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
    fn op_mul_vec(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Vec2, lhs, rhs) => lhs.op_mul_vec(rhs).map(Self::Vec2),
            both!(Self::Vec3, lhs, rhs) => lhs.op_mul_vec(rhs).map(Self::Vec3),
            both!(Self::Vec4, lhs, rhs) => lhs.op_mul_vec(rhs).map(Self::Vec4),
            _ => Err(E::InvalidBinary(
                BinaryOperator::Multiplication,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
    fn op_div_vec(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Vec2, lhs, rhs) => lhs.op_div_vec(rhs).map(Self::Vec2),
            both!(Self::Vec3, lhs, rhs) => lhs.op_div_vec(rhs).map(Self::Vec3),
            both!(Self::Vec4, lhs, rhs) => lhs.op_div_vec(rhs).map(Self::Vec4),
            _ => Err(E::InvalidBinary(
                BinaryOperator::Multiplication,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
    fn op_rem_vec(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Vec2, lhs, rhs) => lhs.op_rem_vec(rhs).map(Self::Vec2),
            both!(Self::Vec3, lhs, rhs) => lhs.op_rem_vec(rhs).map(Self::Vec3),
            both!(Self::Vec4, lhs, rhs) => lhs.op_rem_vec(rhs).map(Self::Vec4),
            _ => Err(E::InvalidBinary(
                BinaryOperator::Multiplication,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
    fn op_mul_scalar(&self, rhs: &LiteralInstance) -> Result<Self, E> {
        match self {
            Self::Vec2(lhs) => lhs.op_mul_scalar(rhs).map(Self::Vec2),
            Self::Vec3(lhs) => lhs.op_mul_scalar(rhs).map(Self::Vec3),
            Self::Vec4(lhs) => lhs.op_mul_scalar(rhs).map(Self::Vec4),
        }
    }
    fn op_mul_mat(&self, rhs: &MatInstance) -> Result<Self, E> {
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
            _ => Err(E::InvalidBinary(
                BinaryOperator::Multiplication,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
}

impl Neg for VecInstance {
    type Output = Result<VecInstance, E>;
    fn neg(self) -> Self::Output {
        self.compwise_unary(Neg::neg)
    }
}
impl Add for &VecInstance {
    type Output = Result<VecInstance, E>;
    fn add(self, rhs: Self) -> Self::Output {
        self.op_add_vec(rhs)
    }
}
impl Add<&LiteralInstance> for &VecInstance {
    type Output = Result<VecInstance, E>;
    fn add(self, rhs: &LiteralInstance) -> Self::Output {
        self.compwise_unary(|k| *rhs + k).map(Into::into)
    }
}
impl Add<&VecInstance> for &LiteralInstance {
    type Output = Result<VecInstance, E>;
    fn add(self, rhs: &VecInstance) -> Self::Output {
        rhs.compwise_unary(|k| k + *self).map(Into::into)
    }
}
impl Sub for &VecInstance {
    type Output = Result<VecInstance, E>;
    fn sub(self, rhs: Self) -> Self::Output {
        self.op_sub_vec(rhs)
    }
}
impl Sub<&LiteralInstance> for &VecInstance {
    type Output = Result<VecInstance, E>;
    fn sub(self, rhs: &LiteralInstance) -> Self::Output {
        self.compwise_unary(|k| *rhs - k).map(Into::into)
    }
}
impl Sub<&VecInstance> for &LiteralInstance {
    type Output = Result<VecInstance, E>;
    fn sub(self, rhs: &VecInstance) -> Self::Output {
        rhs.compwise_unary(|k| k - *self).map(Into::into)
    }
}
impl Mul for &VecInstance {
    type Output = Result<VecInstance, E>;
    fn mul(self, rhs: Self) -> Self::Output {
        self.op_mul_vec(rhs)
    }
}
impl Mul<&LiteralInstance> for &VecInstance {
    type Output = Result<VecInstance, E>;
    fn mul(self, rhs: &LiteralInstance) -> Self::Output {
        self.op_mul_scalar(rhs)
    }
}
impl Mul<&VecInstance> for &LiteralInstance {
    type Output = Result<VecInstance, E>;
    fn mul(self, rhs: &VecInstance) -> Self::Output {
        rhs.op_mul_scalar(self)
    }
}
impl Div for &VecInstance {
    type Output = Result<VecInstance, E>;
    fn div(self, rhs: Self) -> Self::Output {
        self.op_div_vec(rhs)
    }
}
impl Rem for &VecInstance {
    type Output = Result<VecInstance, E>;
    fn rem(self, rhs: Self) -> Self::Output {
        self.op_rem_vec(rhs)
    }
}

impl<const C: usize, const R: usize> MatInner<C, R> {
    fn op_add_mat(&self, rhs: &Self) -> Result<Self, E> {
        let (lhs, rhs) = apply_conversion(self, rhs)?;
        lhs.compwise_binary(&rhs, |l, r| l + r)
    }

    fn op_sub_mat(&self, rhs: &Self) -> Result<Self, E> {
        let (lhs, rhs) = apply_conversion(self, rhs)?;
        lhs.compwise_binary(&rhs, |l, r| l - r)
    }

    fn op_mul_scalar(&self, rhs: &LiteralInstance) -> Result<MatInner<C, R>, E> {
        // TODO must be float
        let ty = get_conversion(&self.inner_ty(), &rhs.inner_ty())
            .ok_or_else(|| E::InvalidBinary(BinaryOperator::Multiplication, self.ty(), rhs.ty()))?;
        let (mat, scalar) = (self.convert_inner_to(&ty)?, rhs.convert_to(&ty)?);

        mat.compwise_unary(|k| k * scalar)
    }

    fn op_mul_vec(&self, rhs: &VecInner<C>) -> Result<VecInner<R>, E> {
        // TODO must be float
        let ty = get_conversion(&self.inner_ty(), &rhs.inner_ty())
            .ok_or_else(|| E::InvalidBinary(BinaryOperator::Multiplication, self.ty(), rhs.ty()))?;

        let (mat, vec) = (self.convert_inner_to(&ty)?, rhs.convert_inner_to(&ty)?);

        Ok(zip(&mat.components, &vec.components)
            .map(|(v, s)| v.op_mul_scalar(s))
            .reduce(|a, b| a?.op_add_vec(&b?))
            .unwrap()?)
    }

    fn op_mul_mat<const K: usize>(&self, rhs: &MatInner<K, C>) -> Result<MatInner<K, R>, E> {
        // TODO must be float
        let ty = get_conversion(&self.inner_ty(), &rhs.inner_ty())
            .ok_or_else(|| E::InvalidBinary(BinaryOperator::Multiplication, self.ty(), rhs.ty()))?;
        let (m1, m2) = (self.convert_inner_to(&ty)?, rhs.convert_inner_to(&ty)?);
        let m1 = m1.transpose();

        Ok(MatInner::new(
            m2.components
                .iter()
                .map(|c1| {
                    Ok(VecInner::new(
                        m1.components
                            .iter()
                            .map(|c2| c1.dot(c2))
                            .collect::<Result<_, _>>()?,
                    ))
                })
                .collect::<Result<_, _>>()?,
        ))
    }
}

impl MatInstance {
    fn op_add_mat(&self, rhs: &Self) -> Result<Self, E> {
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
            (lhs @ _, rhs @ _) => Err(E::InvalidBinary(
                BinaryOperator::Addition,
                lhs.ty(),
                rhs.ty(),
            )),
        }
    }
    fn op_sub_mat(&self, rhs: &Self) -> Result<Self, E> {
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
            (lhs @ _, rhs @ _) => Err(E::InvalidBinary(
                BinaryOperator::Subtraction,
                lhs.ty(),
                rhs.ty(),
            )),
        }
    }
    fn op_mul_scalar(&self, rhs: &LiteralInstance) -> Result<MatInstance, E> {
        match self {
            Self::Mat2x2(lhs) => lhs.op_mul_scalar(rhs).map(Self::Mat2x2),
            Self::Mat2x3(lhs) => lhs.op_mul_scalar(rhs).map(Self::Mat2x3),
            Self::Mat2x4(lhs) => lhs.op_mul_scalar(rhs).map(Self::Mat2x4),
            Self::Mat3x2(lhs) => lhs.op_mul_scalar(rhs).map(Self::Mat3x2),
            Self::Mat3x3(lhs) => lhs.op_mul_scalar(rhs).map(Self::Mat3x3),
            Self::Mat3x4(lhs) => lhs.op_mul_scalar(rhs).map(Self::Mat3x4),
            Self::Mat4x2(lhs) => lhs.op_mul_scalar(rhs).map(Self::Mat4x2),
            Self::Mat4x3(lhs) => lhs.op_mul_scalar(rhs).map(Self::Mat4x3),
            Self::Mat4x4(lhs) => lhs.op_mul_scalar(rhs).map(Self::Mat4x4),
        }
    }
    fn op_mul_vec(&self, rhs: &VecInstance) -> Result<VecInstance, E> {
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

            (lhs @ _, rhs @ _) => Err(E::InvalidBinary(
                BinaryOperator::Multiplication,
                lhs.ty(),
                rhs.ty(),
            )),
        }
    }
    pub fn op_mul_mat(&self, rhs: &Self) -> Result<MatInstance, E> {
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

            (lhs @ _, rhs @ _) => Err(E::InvalidBinary(
                BinaryOperator::Multiplication,
                lhs.ty(),
                rhs.ty(),
            )),
        }
    }
}

impl Add for &MatInstance {
    type Output = Result<MatInstance, E>;
    fn add(self, rhs: Self) -> Self::Output {
        self.op_add_mat(rhs)
    }
}
impl Sub for &MatInstance {
    type Output = Result<MatInstance, E>;
    fn sub(self, rhs: Self) -> Self::Output {
        self.op_sub_mat(rhs)
    }
}
impl Mul for &MatInstance {
    type Output = Result<MatInstance, E>;
    fn mul(self, rhs: Self) -> Self::Output {
        self.op_mul_mat(rhs)
    }
}
impl Mul<&LiteralInstance> for &MatInstance {
    type Output = Result<MatInstance, E>;
    fn mul(self, rhs: &LiteralInstance) -> Self::Output {
        self.op_mul_scalar(rhs)
    }
}
impl Mul<&MatInstance> for &LiteralInstance {
    type Output = Result<MatInstance, E>;
    fn mul(self, rhs: &MatInstance) -> Self::Output {
        rhs.op_mul_scalar(self)
    }
}
impl Mul<&VecInstance> for &MatInstance {
    type Output = Result<VecInstance, E>;
    fn mul(self, rhs: &VecInstance) -> Self::Output {
        self.op_mul_vec(rhs)
    }
}
impl Mul<&MatInstance> for &VecInstance {
    type Output = Result<VecInstance, E>;
    fn mul(self, rhs: &MatInstance) -> Self::Output {
        self.op_mul_mat(rhs)
    }
}

impl Neg for Instance {
    type Output = Result<Instance, E>;

    fn neg(self) -> Self::Output {
        match self {
            Instance::Literal(lhs) => Ok((-lhs)?.into()),
            Instance::Vec(lhs) => Ok((-lhs)?.into()),
            _ => Err(E::InvalidUnary(UnaryOperator::Negation, self.ty())),
        }
    }
}
impl Add for &Instance {
    type Output = Result<Instance, E>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            both!(Instance::Literal, lhs, rhs) => (*lhs + *rhs).map(Into::into),
            (Instance::Vec(lhs), Instance::Literal(rhs)) => (rhs + lhs).map(Into::into),
            (Instance::Literal(lhs), Instance::Vec(rhs)) => (rhs + lhs).map(Into::into),
            both!(Instance::Vec, lhs, rhs) => (lhs + rhs).map(Into::into),
            both!(Instance::Mat, lhs, rhs) => (lhs + rhs).map(Into::into),
            _ => Err(E::InvalidBinary(
                BinaryOperator::Addition,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
}
impl Sub for &Instance {
    type Output = Result<Instance, E>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            both!(Instance::Literal, lhs, rhs) => (*lhs - *rhs).map(Into::into),
            (Instance::Literal(lhs), Instance::Vec(rhs)) => (lhs - rhs).map(Into::into),
            (Instance::Vec(lhs), Instance::Literal(rhs)) => (lhs - rhs).map(Into::into),
            both!(Instance::Vec, lhs, rhs) => (lhs - rhs).map(Into::into),
            both!(Instance::Mat, lhs, rhs) => (lhs - rhs).map(Into::into),
            _ => Err(E::InvalidBinary(
                BinaryOperator::Subtraction,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
}
impl Mul for &Instance {
    type Output = Result<Instance, E>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            both!(Instance::Literal, lhs, rhs) => (*lhs * *rhs).map(Into::into),
            (Instance::Vec(lhs), Instance::Literal(rhs)) => (lhs * rhs).map(Into::into),
            (Instance::Literal(lhs), Instance::Vec(rhs)) => (lhs * rhs).map(Into::into),
            both!(Instance::Vec, lhs, rhs) => (lhs * rhs).map(Into::into),
            (Instance::Mat(lhs), Instance::Literal(rhs)) => (lhs * rhs).map(Into::into),
            (Instance::Literal(lhs), Instance::Mat(rhs)) => (lhs * rhs).map(Into::into),
            (Instance::Mat(lhs), Instance::Vec(rhs)) => (lhs * rhs).map(Into::into),
            (Instance::Vec(lhs), Instance::Mat(rhs)) => (lhs * rhs).map(Into::into),
            both!(Instance::Mat, lhs, rhs) => (lhs * rhs).map(Into::into),
            _ => Err(E::InvalidBinary(
                BinaryOperator::Subtraction,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
}
impl Div for &Instance {
    type Output = Result<Instance, E>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            both!(Instance::Literal, lhs, rhs) => (*lhs / *rhs).map(Into::into),
            (Instance::Literal(s), Instance::Vec(v)) => {
                v.compwise_unary(|k| *s / k).map(Into::into)
            }
            (Instance::Vec(v), Instance::Literal(s)) => {
                v.compwise_unary(|k| k / *s).map(Into::into)
            }
            both!(Instance::Vec, lhs, rhs) => (lhs / rhs).map(Into::into),
            _ => Err(E::InvalidBinary(
                BinaryOperator::Subtraction,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
}
impl Rem for &Instance {
    type Output = Result<Instance, E>;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            both!(Instance::Literal, lhs, rhs) => (*lhs % *rhs).map(Into::into),
            (Instance::Literal(s), Instance::Vec(v)) => Ok(v.compwise_unary(|k| *s % k)?.into()),
            (Instance::Vec(v), Instance::Literal(s)) => Ok(v.compwise_unary(|k| k % *s)?.into()),
            both!(Instance::Vec, lhs, rhs) => Ok((lhs % rhs)?.into()),
            _ => Err(E::InvalidBinary(
                BinaryOperator::Subtraction,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
}

// ----------------------
// COMPARISON EXPRESSIONS
// ----------------------
// reference: https://www.w3.org/TR/WGSL/#comparison-expr

impl LiteralInstance {
    pub fn op_eq(&self, rhs: &Self) -> Result<bool, E> {
        match apply_conversion(self, rhs)? {
            both!(Self::Bool, lhs, rhs) => Ok(lhs == rhs),
            both!(Self::AbstractInt, lhs, rhs) => Ok(lhs == rhs),
            both!(Self::AbstractFloat, lhs, rhs) => Ok(lhs == rhs),
            both!(Self::I32, lhs, rhs) => Ok(lhs == rhs),
            both!(Self::U32, lhs, rhs) => Ok(lhs == rhs),
            both!(Self::F32, lhs, rhs) => Ok(lhs == rhs),
            both!(Self::F16, lhs, rhs) => Ok(lhs == rhs),
            _ => Err(E::InvalidBinary(
                BinaryOperator::Equality,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
    pub fn op_ne(&self, rhs: &Self) -> Result<bool, E> {
        match (self, rhs) {
            both!(Self::Bool, lhs, rhs) => Ok(lhs != rhs),
            both!(Self::AbstractInt, lhs, rhs) => Ok(lhs != rhs),
            both!(Self::AbstractFloat, lhs, rhs) => Ok(lhs != rhs),
            both!(Self::I32, lhs, rhs) => Ok(lhs != rhs),
            both!(Self::U32, lhs, rhs) => Ok(lhs != rhs),
            both!(Self::F32, lhs, rhs) => Ok(lhs != rhs),
            both!(Self::F16, lhs, rhs) => Ok(lhs != rhs),
            _ => Err(E::InvalidBinary(
                BinaryOperator::Inequality,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
    pub fn op_lt(&self, rhs: &Self) -> Result<bool, E> {
        match (self, rhs) {
            both!(Self::Bool, lhs, rhs) => Ok(lhs < rhs),
            both!(Self::AbstractInt, lhs, rhs) => Ok(lhs < rhs),
            both!(Self::AbstractFloat, lhs, rhs) => Ok(lhs < rhs),
            both!(Self::I32, lhs, rhs) => Ok(lhs < rhs),
            both!(Self::U32, lhs, rhs) => Ok(lhs < rhs),
            both!(Self::F32, lhs, rhs) => Ok(lhs < rhs),
            both!(Self::F16, lhs, rhs) => Ok(lhs < rhs),
            _ => Err(E::InvalidBinary(
                BinaryOperator::LessThan,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
    pub fn op_le(&self, rhs: &Self) -> Result<bool, E> {
        match (self, rhs) {
            both!(Self::Bool, lhs, rhs) => Ok(lhs <= rhs),
            both!(Self::AbstractInt, lhs, rhs) => Ok(lhs <= rhs),
            both!(Self::AbstractFloat, lhs, rhs) => Ok(lhs <= rhs),
            both!(Self::I32, lhs, rhs) => Ok(lhs <= rhs),
            both!(Self::U32, lhs, rhs) => Ok(lhs <= rhs),
            both!(Self::F32, lhs, rhs) => Ok(lhs <= rhs),
            both!(Self::F16, lhs, rhs) => Ok(lhs <= rhs),
            _ => Err(E::InvalidBinary(
                BinaryOperator::LessThanEqual,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
    pub fn op_gt(&self, rhs: &Self) -> Result<bool, E> {
        match (self, rhs) {
            both!(Self::Bool, lhs, rhs) => Ok(lhs > rhs),
            both!(Self::AbstractInt, lhs, rhs) => Ok(lhs > rhs),
            both!(Self::AbstractFloat, lhs, rhs) => Ok(lhs > rhs),
            both!(Self::I32, lhs, rhs) => Ok(lhs > rhs),
            both!(Self::U32, lhs, rhs) => Ok(lhs > rhs),
            both!(Self::F32, lhs, rhs) => Ok(lhs > rhs),
            both!(Self::F16, lhs, rhs) => Ok(lhs > rhs),
            _ => Err(E::InvalidBinary(
                BinaryOperator::GreaterThan,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
    pub fn op_ge(&self, rhs: &Self) -> Result<bool, E> {
        match (self, rhs) {
            both!(Self::Bool, lhs, rhs) => Ok(lhs >= rhs),
            both!(Self::AbstractInt, lhs, rhs) => Ok(lhs >= rhs),
            both!(Self::AbstractFloat, lhs, rhs) => Ok(lhs >= rhs),
            both!(Self::I32, lhs, rhs) => Ok(lhs >= rhs),
            both!(Self::U32, lhs, rhs) => Ok(lhs >= rhs),
            both!(Self::F32, lhs, rhs) => Ok(lhs >= rhs),
            both!(Self::F16, lhs, rhs) => Ok(lhs >= rhs),
            _ => Err(E::InvalidBinary(
                BinaryOperator::GreaterThanEqual,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
}

impl VecInstance {
    pub fn op_eq(&self, rhs: &Self) -> Result<VecInstance, E> {
        let (lhs, rhs) = apply_conversion(self, rhs)?;
        lhs.compwise_binary(&rhs, |a, b| a.op_eq(&b).map(Into::into))
    }
    pub fn op_ne(&self, rhs: &Self) -> Result<VecInstance, E> {
        let (lhs, rhs) = apply_conversion(self, rhs)?;
        lhs.compwise_binary(&rhs, |a, b| a.op_ne(&b).map(Into::into))
    }
    pub fn op_lt(&self, rhs: &Self) -> Result<VecInstance, E> {
        let (lhs, rhs) = apply_conversion(self, rhs)?;
        lhs.compwise_binary(&rhs, |a, b| a.op_lt(&b).map(Into::into))
    }
    pub fn op_le(&self, rhs: &Self) -> Result<VecInstance, E> {
        let (lhs, rhs) = apply_conversion(self, rhs)?;
        lhs.compwise_binary(&rhs, |a, b| a.op_le(&b).map(Into::into))
    }
    pub fn op_gt(&self, rhs: &Self) -> Result<VecInstance, E> {
        let (lhs, rhs) = apply_conversion(self, rhs)?;
        lhs.compwise_binary(&rhs, |a, b| a.op_gt(&b).map(Into::into))
    }
    pub fn op_ge(&self, rhs: &Self) -> Result<VecInstance, E> {
        let (lhs, rhs) = apply_conversion(self, rhs)?;
        lhs.compwise_binary(&rhs, |a, b| a.op_ge(&b).map(Into::into))
    }
}

impl Instance {
    pub fn op_eq(&self, rhs: &Self) -> Result<Instance, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs
                .op_eq(rhs)
                .map(|b| Self::Literal(LiteralInstance::Bool(b))),
            both!(Self::Vec, lhs, rhs) => lhs.op_eq(rhs).map(Into::into),
            _ => Err(E::InvalidBinary(
                BinaryOperator::Equality,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
    pub fn op_ne(&self, rhs: &Self) -> Result<Instance, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs
                .op_ne(rhs)
                .map(|b| Self::Literal(LiteralInstance::Bool(b))),
            both!(Self::Vec, lhs, rhs) => lhs.op_ne(rhs).map(Into::into),
            _ => Err(E::InvalidBinary(
                BinaryOperator::Inequality,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
    pub fn op_lt(&self, rhs: &Self) -> Result<Instance, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs
                .op_lt(rhs)
                .map(|b| Self::Literal(LiteralInstance::Bool(b))),
            both!(Self::Vec, lhs, rhs) => lhs.op_lt(rhs).map(Into::into),
            _ => Err(E::InvalidBinary(
                BinaryOperator::LessThan,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
    pub fn op_le(&self, rhs: &Self) -> Result<Instance, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs
                .op_le(rhs)
                .map(|b| Self::Literal(LiteralInstance::Bool(b))),
            both!(Self::Vec, lhs, rhs) => lhs.op_le(rhs).map(Into::into),
            _ => Err(E::InvalidBinary(
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
            _ => Err(E::InvalidBinary(
                BinaryOperator::GreaterThan,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
    pub fn op_ge(&self, rhs: &Self) -> Result<Instance, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs
                .op_ge(rhs)
                .map(|b| Self::Literal(LiteralInstance::Bool(b))),
            both!(Self::Vec, lhs, rhs) => lhs.op_ge(rhs).map(Into::into),
            _ => Err(E::InvalidBinary(
                BinaryOperator::GreaterThanEqual,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
}
