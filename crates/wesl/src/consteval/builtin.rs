use std::iter::zip;

use itertools::Itertools;

use crate::ConstEvalError;

use super::{LiteralInstance, MatInner, MatInstance, Ty, VecInner, VecInstance};

impl<const N: usize> VecInner<N> {
    pub fn dot(&self, rhs: &VecInner<N>) -> Result<LiteralInstance, ConstEvalError> {
        zip(self.iter(), rhs.iter())
            .map(|(a, b)| a.op_mul(b))
            .reduce(|a, b| a?.op_add(&b?))
            .unwrap()
    }
}

impl VecInstance {
    pub fn dot(&self, rhs: &VecInstance) -> Result<LiteralInstance, ConstEvalError> {
        match (self, rhs) {
            (VecInstance::Vec2(lhs), VecInstance::Vec2(rhs)) => lhs.dot(rhs),
            (VecInstance::Vec3(lhs), VecInstance::Vec3(rhs)) => lhs.dot(rhs),
            (VecInstance::Vec4(lhs), VecInstance::Vec4(rhs)) => lhs.dot(rhs),
            (lhs @ _, _) => Err(ConstEvalError::InvalidCompwiseBinary(lhs.ty(), rhs.ty())),
        }
    }
}

impl<const C: usize, const R: usize> MatInner<C, R> {
    pub fn transpose(&self) -> MatInner<R, C> {
        let components = (0..self.components.len())
            .map(|i| {
                self.components
                    .iter()
                    .map(|inner| inner.components[i].clone())
                    .collect_vec()
                    .into()
            })
            .collect_vec();
        MatInner { components }
    }
}

impl MatInstance {
    pub fn transpose(&self) -> MatInstance {
        match self {
            Self::Mat2x2(m) => Self::Mat2x2(m.transpose()),
            Self::Mat2x3(m) => Self::Mat3x2(m.transpose()),
            Self::Mat2x4(m) => Self::Mat4x2(m.transpose()),
            Self::Mat3x2(m) => Self::Mat2x3(m.transpose()),
            Self::Mat3x3(m) => Self::Mat3x3(m.transpose()),
            Self::Mat3x4(m) => Self::Mat4x3(m.transpose()),
            Self::Mat4x2(m) => Self::Mat2x4(m.transpose()),
            Self::Mat4x3(m) => Self::Mat3x4(m.transpose()),
            Self::Mat4x4(m) => Self::Mat4x4(m.transpose()),
        }
    }
}
