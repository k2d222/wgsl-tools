use std::iter::zip;

use crate::EvalError;

use super::{
    convert, convert_inner, Convert, EvalStage, Instance, LiteralInstance, MatInstance, Ty, Type,
    VecInstance,
};

use num_traits::{ToPrimitive, WrappingNeg, WrappingShl};
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

impl Compwise for VecInstance {
    fn compwise_unary_mut<F>(&mut self, f: F) -> Result<(), E>
    where
        F: Fn(&LiteralInstance) -> Result<LiteralInstance, E>,
    {
        self.iter_mut()
            .map(|c| {
                match c {
                    Instance::Literal(c) => *c = f(c)?,
                    _ => unreachable!("vec must contain literal instances"),
                };
                Ok(())
            })
            .collect::<Result<_, _>>()
    }

    fn compwise_binary_mut<F>(&mut self, rhs: &Self, f: F) -> Result<(), E>
    where
        F: Fn(&LiteralInstance, &LiteralInstance) -> Result<LiteralInstance, E>,
    {
        if self.n() != rhs.n() {
            return Err(E::CompwiseBinary(self.ty(), rhs.ty()));
        }
        zip(self.iter_mut(), rhs.iter())
            .map(|(a, b)| {
                match (a, b) {
                    (Instance::Literal(a), Instance::Literal(b)) => *a = f(a, b)?,
                    _ => unreachable!("vec must contain literal instances"),
                };
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
        self.iter_cols_mut()
            .flat_map(|col| col.unwrap_vec_mut().iter_mut())
            .map(|c| {
                match c {
                    Instance::Literal(c) => *c = f(c)?,
                    _ => unreachable!("mat must contain literal instances"),
                };
                Ok(())
            })
            .collect::<Result<_, _>>()
    }

    fn compwise_binary_mut<F>(&mut self, rhs: &Self, f: F) -> Result<(), E>
    where
        F: Fn(&LiteralInstance, &LiteralInstance) -> Result<LiteralInstance, E>,
    {
        if self.c() != rhs.c() || self.r() != rhs.r() {
            return Err(E::CompwiseBinary(self.ty(), rhs.ty()));
        }
        zip(
            self.iter_cols_mut()
                .flat_map(|col| col.unwrap_vec_mut().iter_mut()),
            rhs.iter_cols().flat_map(|col| col.unwrap_vec_ref().iter()),
        )
        .map(|(a, b)| {
            match (a, b) {
                (Instance::Literal(a), Instance::Literal(b)) => *a = f(a, b)?,
                _ => unreachable!("mat must contain literal instances"),
            };
            Ok(())
        })
        .collect::<Result<_, _>>()
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

impl VecInstance {
    pub fn op_not(&self) -> Result<Self, E> {
        self.compwise_unary(|c| c.op_not())
    }
}

impl Instance {
    pub fn op_not(&self) -> Result<Self, E> {
        match self {
            Self::Literal(lit) => lit.op_not().map(Into::into),
            Self::Vec(v) => v.op_not().map(Into::into),
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
            Self::AbstractInt(lhs) => Ok(lhs.wrapping_neg().into()),
            Self::AbstractFloat(lhs) => Ok((-lhs).into()),
            Self::I32(lhs) => Ok(lhs.wrapping_neg().into()),
            Self::F32(lhs) => Ok((-lhs).into()),
            Self::F16(lhs) => Ok((-lhs).into()),
            _ => Err(E::Unary(UnaryOperator::Negation, self.ty())),
        }
    }
    pub fn op_add(&self, rhs: &Self, stage: EvalStage) -> Result<LiteralInstance, E> {
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
            both!(Self::I32, lhs, rhs) => Ok(lhs.wrapping_add(rhs).into()),
            both!(Self::U32, lhs, rhs) => Ok(lhs.wrapping_add(rhs).into()),
            both!(Self::F32, lhs, rhs) => {
                let res = lhs + rhs;
                if stage == EvalStage::Const {
                    res.is_finite()
                        .then_some(res)
                        .ok_or(E::AddOverflow)
                        .map(Into::into)
                } else {
                    Ok(res.into())
                }
            }
            both!(Self::F16, lhs, rhs) => {
                let res = lhs + rhs;
                if stage == EvalStage::Const {
                    res.is_finite()
                        .then_some(res)
                        .ok_or(E::AddOverflow)
                        .map(Into::into)
                } else {
                    Ok(res.into())
                }
            }
            _ => Err(err()),
        }
    }
    pub fn op_sub(&self, rhs: &Self, stage: EvalStage) -> Result<LiteralInstance, E> {
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
            both!(Self::I32, lhs, rhs) => Ok(lhs.wrapping_sub(rhs).into()),
            both!(Self::U32, lhs, rhs) => Ok(lhs.wrapping_sub(rhs).into()),
            both!(Self::F32, lhs, rhs) => {
                let res = lhs - rhs;
                if stage == EvalStage::Const {
                    res.is_finite()
                        .then_some(res)
                        .ok_or(E::SubOverflow)
                        .map(Into::into)
                } else {
                    Ok(res.into())
                }
            }
            both!(Self::F16, lhs, rhs) => {
                let res = lhs - rhs;
                if stage == EvalStage::Const {
                    res.is_finite()
                        .then_some(res)
                        .ok_or(E::SubOverflow)
                        .map(Into::into)
                } else {
                    Ok(res.into())
                }
            }
            _ => Err(err()),
        }
    }
    pub fn op_mul(&self, rhs: &Self, stage: EvalStage) -> Result<LiteralInstance, E> {
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
            both!(Self::I32, lhs, rhs) => Ok(lhs.wrapping_mul(rhs).into()),
            both!(Self::U32, lhs, rhs) => Ok(lhs.wrapping_mul(rhs).into()),
            both!(Self::F32, lhs, rhs) => {
                let res = lhs * rhs;
                if stage == EvalStage::Const {
                    res.is_finite()
                        .then_some(res)
                        .ok_or(E::MulOverflow)
                        .map(Into::into)
                } else {
                    Ok(res.into())
                }
            }
            both!(Self::F16, lhs, rhs) => {
                let res = lhs * rhs;
                if stage == EvalStage::Const {
                    res.is_finite()
                        .then_some(res)
                        .ok_or(E::MulOverflow)
                        .map(Into::into)
                } else {
                    Ok(res.into())
                }
            }
            _ => Err(err()),
        }
    }
    pub fn op_div(&self, rhs: &Self, stage: EvalStage) -> Result<LiteralInstance, E> {
        let err = || E::Binary(BinaryOperator::Division, self.ty(), rhs.ty());
        let res = match convert(self, rhs).ok_or_else(err)? {
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
        };
        if stage == EvalStage::Exec {
            // runtime expressions return lhs when operation fails
            Ok(res.unwrap_or_else(|_| self.clone()))
        } else {
            res
        }
    }
    pub fn op_rem(&self, rhs: &Self, stage: EvalStage) -> Result<LiteralInstance, E> {
        let err = || E::Binary(BinaryOperator::Remainder, self.ty(), rhs.ty());
        match convert(self, rhs).ok_or_else(err)? {
            both!(Self::AbstractInt, lhs, rhs) => {
                if stage == EvalStage::Const {
                    lhs.checked_rem(rhs).ok_or(E::RemZeroDiv).map(Into::into)
                } else {
                    Ok(lhs.checked_rem(rhs).unwrap_or(0).into())
                }
            }
            both!(Self::AbstractFloat, lhs, rhs) => {
                let res = lhs % rhs;
                res.is_finite()
                    .then_some(res)
                    .ok_or(E::RemZeroDiv)
                    .map(Into::into)
            }
            both!(Self::I32, lhs, rhs) => {
                if stage == EvalStage::Const {
                    lhs.checked_rem(rhs).ok_or(E::RemZeroDiv).map(Into::into)
                } else {
                    Ok(lhs.checked_rem(rhs).unwrap_or(0).into())
                }
            }
            both!(Self::U32, lhs, rhs) => {
                if stage == EvalStage::Const {
                    lhs.checked_rem(rhs).ok_or(E::RemZeroDiv).map(Into::into)
                } else {
                    Ok(lhs.checked_rem(rhs).unwrap_or(0).into())
                }
            }
            both!(Self::F32, lhs, rhs) => {
                let res = lhs % rhs;
                if stage == EvalStage::Const {
                    res.is_finite()
                        .then_some(res)
                        .ok_or(E::RemZeroDiv)
                        .map(Into::into)
                } else {
                    Ok(res.into())
                }
            }
            both!(Self::F16, lhs, rhs) => {
                let res = lhs % rhs;
                if stage == EvalStage::Const {
                    res.is_finite()
                        .then_some(res)
                        .ok_or(E::RemZeroDiv)
                        .map(Into::into)
                } else {
                    Ok(res.into())
                }
            }
            _ => Err(err()),
        }
    }
    pub fn op_add_vec(&self, rhs: &VecInstance, stage: EvalStage) -> Result<VecInstance, E> {
        rhs.op_add_sca(self, stage)
    }
    pub fn op_sub_vec(&self, rhs: &VecInstance, stage: EvalStage) -> Result<VecInstance, E> {
        let (lhs, rhs) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Subtraction, self.ty(), rhs.ty()))?;
        rhs.compwise_unary(|r| lhs.op_sub(r, stage)).map(Into::into)
    }
    pub fn op_mul_vec(&self, rhs: &VecInstance, stage: EvalStage) -> Result<VecInstance, E> {
        rhs.op_mul_sca(self, stage)
    }
    pub fn op_div_vec(&self, rhs: &VecInstance, stage: EvalStage) -> Result<VecInstance, E> {
        let (lhs, rhs) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Division, self.ty(), rhs.ty()))?;
        rhs.compwise_unary(|r| lhs.op_div(r, stage)).map(Into::into)
    }
    pub fn op_rem_vec(&self, rhs: &VecInstance, stage: EvalStage) -> Result<VecInstance, E> {
        let (lhs, rhs) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Remainder, self.ty(), rhs.ty()))?;
        rhs.compwise_unary(|r| lhs.op_rem(r, stage)).map(Into::into)
    }
    pub fn op_mul_mat(&self, rhs: &MatInstance, stage: EvalStage) -> Result<MatInstance, E> {
        rhs.op_mul_sca(self, stage)
    }
}

impl VecInstance {
    pub fn op_neg(&self) -> Result<Self, E> {
        self.compwise_unary(|c| c.op_neg())
    }
    pub fn op_add(&self, rhs: &Self, stage: EvalStage) -> Result<Self, E> {
        let (lhs, rhs) = convert(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Addition, self.ty(), rhs.ty()))?;
        lhs.compwise_binary(&rhs, |l, r| l.op_add(&r, stage))
    }
    pub fn op_sub(&self, rhs: &Self, stage: EvalStage) -> Result<Self, E> {
        let (lhs, rhs) = convert(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Subtraction, self.ty(), rhs.ty()))?;
        lhs.compwise_binary(&rhs, |l, r| l.op_sub(&r, stage))
    }
    pub fn op_mul(&self, rhs: &Self, stage: EvalStage) -> Result<Self, E> {
        let (lhs, rhs) = convert(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Multiplication, self.ty(), rhs.ty()))?;
        lhs.compwise_binary(&rhs, |l, r| l.op_mul(&r, stage))
    }
    pub fn op_div(&self, rhs: &Self, stage: EvalStage) -> Result<Self, E> {
        let (lhs, rhs) = convert(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Division, self.ty(), rhs.ty()))?;
        lhs.compwise_binary(&rhs, |l, r| l.op_div(&r, stage))
    }
    pub fn op_rem(&self, rhs: &Self, stage: EvalStage) -> Result<Self, E> {
        let (lhs, rhs) = convert(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Remainder, self.ty(), rhs.ty()))?;
        lhs.compwise_binary(&rhs, |l, r| l.op_rem(&r, stage))
    }
    pub fn op_add_sca(&self, rhs: &LiteralInstance, stage: EvalStage) -> Result<Self, E> {
        let (lhs, rhs) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Addition, self.ty(), rhs.ty()))?;
        lhs.compwise_unary(|l| l.op_add(&rhs, stage))
            .map(Into::into)
    }
    pub fn op_sub_sca(&self, rhs: &LiteralInstance, stage: EvalStage) -> Result<Self, E> {
        let (lhs, rhs) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Subtraction, self.ty(), rhs.ty()))?;
        lhs.compwise_unary(|l| l.op_sub(&rhs, stage))
            .map(Into::into)
    }
    pub fn op_mul_sca(&self, rhs: &LiteralInstance, stage: EvalStage) -> Result<Self, E> {
        let (lhs, rhs) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Multiplication, self.ty(), rhs.ty()))?;
        lhs.compwise_unary(|l| l.op_mul(&rhs, stage))
            .map(Into::into)
    }
    pub fn op_div_sca(&self, rhs: &LiteralInstance, stage: EvalStage) -> Result<Self, E> {
        let (lhs, rhs) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Division, self.ty(), rhs.ty()))?;
        lhs.compwise_unary(|l| l.op_div(&rhs, stage))
            .map(Into::into)
    }
    pub fn op_rem_sca(&self, rhs: &LiteralInstance, stage: EvalStage) -> Result<Self, E> {
        let (lhs, rhs) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Remainder, self.ty(), rhs.ty()))?;
        lhs.compwise_unary(|l| l.op_rem(&rhs, stage))
            .map(Into::into)
    }
    pub fn op_mul_mat(&self, rhs: &MatInstance, stage: EvalStage) -> Result<Self, E> {
        // TODO must be float
        let (vec, mat) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Multiplication, self.ty(), rhs.ty()))?;
        let mat = mat.transpose();

        Ok(zip(vec.iter(), mat.iter_cols())
            .map(|(s, v)| v.unwrap_vec_ref().op_mul_sca(s.unwrap_literal_ref(), stage))
            .reduce(|a, b| a?.op_add(&b?, stage))
            .unwrap()?)
    }
}

impl MatInstance {
    pub fn op_add(&self, rhs: &Self, stage: EvalStage) -> Result<Self, E> {
        let (lhs, rhs) = convert(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Addition, self.ty(), rhs.ty()))?;
        lhs.compwise_binary(&rhs, |l, r| l.op_add(&r, stage))
    }

    pub fn op_sub(&self, rhs: &Self, stage: EvalStage) -> Result<Self, E> {
        let (lhs, rhs) = convert(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Subtraction, self.ty(), rhs.ty()))?;
        lhs.compwise_binary(&rhs, |l, r| l.op_sub(&r, stage))
    }

    pub fn op_mul_sca(&self, rhs: &LiteralInstance, stage: EvalStage) -> Result<MatInstance, E> {
        // TODO must be float
        let (lhs, rhs) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Multiplication, self.ty(), rhs.ty()))?;
        lhs.compwise_unary(|l| l.op_mul(&rhs, stage))
    }

    pub fn op_mul_vec(&self, rhs: &VecInstance, stage: EvalStage) -> Result<VecInstance, E> {
        // TODO must be float
        let (lhs, rhs) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Multiplication, self.ty(), rhs.ty()))?;

        Ok(zip(lhs.iter_cols(), rhs.iter())
            .map(|(l, r)| l.unwrap_vec_ref().op_mul_sca(r.unwrap_literal_ref(), stage))
            .reduce(|l, r| l?.op_add(&r?, stage))
            .unwrap()?)
    }

    pub fn op_mul(&self, rhs: &Self, stage: EvalStage) -> Result<MatInstance, E> {
        // TODO must be float
        let (lhs, rhs) = convert_inner(self, rhs)
            .ok_or_else(|| E::Binary(BinaryOperator::Multiplication, self.ty(), rhs.ty()))?;
        let lhs = lhs.transpose();

        Ok(MatInstance::from_cols(
            rhs.iter_cols()
                .map(|col| {
                    Ok(VecInstance::new(
                        lhs.iter_cols()
                            .map(|r| {
                                col.unwrap_vec_ref()
                                    .dot(r.unwrap_vec_ref(), stage)
                                    .map(Into::into)
                            })
                            .collect::<Result<_, _>>()?,
                    )
                    .into())
                })
                .collect::<Result<_, _>>()?,
        ))
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
    pub fn op_add(&self, rhs: &Self, stage: EvalStage) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => (lhs.op_add(rhs, stage)).map(Into::into),
            (Self::Vec(lhs), Self::Literal(rhs)) => lhs.op_add_sca(rhs, stage).map(Into::into),
            (Self::Literal(lhs), Self::Vec(rhs)) => lhs.op_add_vec(rhs, stage).map(Into::into),
            both!(Self::Vec, lhs, rhs) => lhs.op_add(rhs, stage).map(Into::into),
            both!(Self::Mat, lhs, rhs) => lhs.op_add(rhs, stage).map(Into::into),
            _ => Err(E::Binary(BinaryOperator::Addition, self.ty(), rhs.ty())),
        }
    }
    pub fn op_sub(&self, rhs: &Self, stage: EvalStage) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs.op_sub(&rhs, stage).map(Into::into),
            (Self::Vec(lhs), Self::Literal(rhs)) => lhs.op_sub_sca(rhs, stage).map(Into::into),
            (Self::Literal(lhs), Self::Vec(rhs)) => lhs.op_sub_vec(rhs, stage).map(Into::into),
            both!(Self::Vec, lhs, rhs) => lhs.op_sub(rhs, stage).map(Into::into),
            both!(Self::Mat, lhs, rhs) => lhs.op_sub(rhs, stage).map(Into::into),
            _ => Err(E::Binary(BinaryOperator::Subtraction, self.ty(), rhs.ty())),
        }
    }
    pub fn op_mul(&self, rhs: &Self, stage: EvalStage) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs.op_mul(&rhs, stage).map(Into::into),
            (Self::Vec(lhs), Self::Literal(rhs)) => lhs.op_mul_sca(rhs, stage).map(Into::into),
            (Self::Literal(lhs), Self::Vec(rhs)) => lhs.op_mul_vec(rhs, stage).map(Into::into),
            both!(Self::Vec, lhs, rhs) => lhs.op_mul(rhs, stage).map(Into::into),
            (Self::Mat(lhs), Self::Literal(rhs)) => lhs.op_mul_sca(rhs, stage).map(Into::into),
            (Self::Literal(lhs), Self::Mat(rhs)) => lhs.op_mul_mat(rhs, stage).map(Into::into),
            (Self::Mat(lhs), Self::Vec(rhs)) => lhs.op_mul_vec(rhs, stage).map(Into::into),
            (Self::Vec(lhs), Self::Mat(rhs)) => lhs.op_mul_mat(rhs, stage).map(Into::into),
            both!(Self::Mat, lhs, rhs) => lhs.op_mul(rhs, stage).map(Into::into),
            _ => Err(E::Binary(
                BinaryOperator::Multiplication,
                self.ty(),
                rhs.ty(),
            )),
        }
    }
    pub fn op_div(&self, rhs: &Self, stage: EvalStage) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs.op_div(&rhs, stage).map(Into::into),
            (Self::Literal(s), Self::Vec(v)) => {
                v.compwise_unary(|k| s.op_div(&k, stage)).map(Into::into)
            }
            (Self::Vec(v), Self::Literal(s)) => {
                v.compwise_unary(|k| k.op_div(s, stage)).map(Into::into)
            }
            both!(Self::Vec, lhs, rhs) => lhs.op_div(rhs, stage).map(Into::into),
            _ => Err(E::Binary(BinaryOperator::Division, self.ty(), rhs.ty())),
        }
    }
    pub fn op_rem(&self, rhs: &Self, stage: EvalStage) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs.op_rem(rhs, stage).map(Into::into),
            (Self::Literal(s), Self::Vec(v)) => {
                v.compwise_unary(|k| s.op_rem(k, stage)).map(Into::into)
            }
            (Self::Vec(v), Self::Literal(s)) => {
                v.compwise_unary(|k| k.op_rem(s, stage)).map(Into::into)
            }
            both!(Self::Vec, lhs, rhs) => lhs.op_rem(rhs, stage).map(Into::into),
            _ => Err(E::Binary(BinaryOperator::Remainder, self.ty(), rhs.ty())),
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
        let err = || E::Binary(BinaryOperator::BitwiseOr, self.ty(), rhs.ty());
        match convert(self, rhs).ok_or_else(err)? {
            both!(Self::Bool, rhs, lhs) => Ok(Self::Bool(lhs | rhs)),
            both!(Self::AbstractInt, rhs, lhs) => Ok(Self::AbstractInt(lhs | rhs)),
            both!(Self::I32, rhs, lhs) => Ok(Self::I32(lhs | rhs)),
            both!(Self::U32, rhs, lhs) => Ok(Self::U32(lhs | rhs)),
            _ => Err(err()),
        }
    }
    pub fn op_bitand(&self, rhs: &Self) -> Result<Self, E> {
        let err = || E::Binary(BinaryOperator::BitwiseAnd, self.ty(), rhs.ty());
        match convert(self, rhs).ok_or_else(err)? {
            both!(Self::Bool, rhs, lhs) => Ok(Self::Bool(lhs & rhs)),
            both!(Self::AbstractInt, rhs, lhs) => Ok(Self::AbstractInt(lhs & rhs)),
            both!(Self::I32, rhs, lhs) => Ok(Self::I32(lhs & rhs)),
            both!(Self::U32, rhs, lhs) => Ok(Self::U32(lhs & rhs)),
            _ => Err(err()),
        }
    }
    pub fn op_bitxor(&self, rhs: &Self) -> Result<Self, E> {
        let err = || E::Binary(BinaryOperator::BitwiseXor, self.ty(), rhs.ty());
        match convert(self, rhs).ok_or_else(err)? {
            both!(Self::AbstractInt, rhs, lhs) => Ok(Self::AbstractInt(lhs ^ rhs)),
            both!(Self::I32, rhs, lhs) => Ok(Self::I32(lhs ^ rhs)),
            both!(Self::U32, rhs, lhs) => Ok(Self::U32(lhs ^ rhs)),
            _ => Err(err()),
        }
    }
    pub fn op_shl(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs.convert_to(&Type::U32)) {
            (Self::I32(lhs), Some(Self::U32(rhs))) => Ok(lhs.wrapping_shl(rhs).into()),
            (Self::U32(lhs), Some(Self::U32(rhs))) => Ok(lhs.wrapping_shl(rhs).into()),
            (Self::AbstractInt(lhs), Some(Self::U32(rhs))) => lhs
                .checked_shl(rhs)
                .map(Into::into)
                .ok_or(E::ShlOverflow(rhs)),
            (Self::AbstractFloat(lhs), Some(Self::U32(rhs))) => lhs
                .to_i64()
                .and_then(|lhs| lhs.checked_shl(rhs))
                .map(Into::into)
                .ok_or(E::ShlOverflow(rhs)),
            _ => Err(E::Binary(BinaryOperator::ShiftLeft, self.ty(), rhs.ty())),
        }
    }
    pub fn op_shr(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs.convert_to(&Type::U32)) {
            (Self::I32(lhs), Some(Self::U32(rhs))) => lhs
                .checked_shr(rhs)
                .map(Into::into)
                .ok_or(E::ShrOverflow(rhs)),
            (Self::U32(lhs), Some(Self::U32(rhs))) => lhs
                .checked_shr(rhs)
                .map(Into::into)
                .ok_or(E::ShrOverflow(rhs)),
            (Self::AbstractInt(lhs), Some(Self::U32(rhs))) => lhs
                .checked_shr(rhs)
                .map(Into::into)
                .ok_or(E::ShrOverflow(rhs)),
            (Self::AbstractFloat(lhs), Some(Self::U32(rhs))) => lhs
                .to_i64()
                .and_then(|lhs| lhs.checked_shr(rhs))
                .map(Into::into)
                .ok_or(E::ShrOverflow(rhs)),
            _ => Err(E::Binary(BinaryOperator::ShiftRight, self.ty(), rhs.ty())),
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
            _ => Err(E::Binary(BinaryOperator::BitwiseOr, self.ty(), rhs.ty())),
        }
    }
    pub fn op_bitand(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs.op_bitand(rhs).map(Into::into),
            both!(Self::Vec, lhs, rhs) => lhs.op_bitand(rhs).map(Into::into),
            _ => Err(E::Binary(BinaryOperator::BitwiseAnd, self.ty(), rhs.ty())),
        }
    }
    pub fn op_bitxor(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs.op_bitxor(rhs).map(Into::into),
            both!(Self::Vec, lhs, rhs) => lhs.op_bitxor(rhs).map(Into::into),
            _ => Err(E::Binary(BinaryOperator::BitwiseXor, self.ty(), rhs.ty())),
        }
    }
    pub fn op_shl(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs.op_shl(rhs).map(Into::into),
            both!(Self::Vec, lhs, rhs) => lhs.op_shl(rhs).map(Into::into),
            _ => Err(E::Binary(BinaryOperator::ShiftLeft, self.ty(), rhs.ty())),
        }
    }
    pub fn op_shr(&self, rhs: &Self) -> Result<Self, E> {
        match (self, rhs) {
            both!(Self::Literal, lhs, rhs) => lhs.op_shr(rhs).map(Into::into),
            both!(Self::Vec, lhs, rhs) => lhs.op_shr(rhs).map(Into::into),
            _ => Err(E::Binary(BinaryOperator::ShiftRight, self.ty(), rhs.ty())),
        }
    }
}
