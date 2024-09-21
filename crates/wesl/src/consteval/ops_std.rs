use super::{
    apply_conversion, conv::Convert, Instance, LiteralInstance, MatInner, MatInstance, Ty,
    VecInner, VecInstance,
};
use std::{
    iter::zip,
    ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Not, Rem, Shl, Shr, Sub},
};

// OPS LITERAL

impl Not for LiteralInstance {
    type Output = Result<Self, E>;
    fn not(self) -> Self::Output {
        self.op_not()
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
impl Add<&VecInstance> for &LiteralInstance {
    type Output = Result<VecInstance, E>;
    fn add(self, rhs: &VecInstance) -> Self::Output {
        rhs.compwise_unary(|k| k + *self).map(Into::into)
    }
}
impl Sub for LiteralInstance {
    type Output = Result<LiteralInstance, E>;
    fn sub(self, rhs: Self) -> Self::Output {
        self.op_sub(&rhs)
    }
}
impl Sub<&VecInstance> for &LiteralInstance {
    type Output = Result<VecInstance, E>;
    fn sub(self, rhs: &VecInstance) -> Self::Output {
        rhs.compwise_unary(|k| k - *self).map(Into::into)
    }
}
impl Mul for LiteralInstance {
    type Output = Result<LiteralInstance, E>;
    fn mul(self, rhs: Self) -> Self::Output {
        self.op_mul(&rhs)
    }
}
impl Mul<&VecInstance> for &LiteralInstance {
    type Output = Result<VecInstance, E>;
    fn mul(self, rhs: &VecInstance) -> Self::Output {
        rhs.op_mul_scalar(self)
    }
}
impl Mul<&MatInstance> for &LiteralInstance {
    type Output = Result<MatInstance, E>;
    fn mul(self, rhs: &MatInstance) -> Self::Output {
        rhs.op_mul_scalar(self)
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
impl BitOr for LiteralInstance {
    type Output = Result<Self, E>;
    fn bitor(self, rhs: Self) -> Self::Output {
        self.op_bitor(&rhs)
    }
}
impl BitAnd for LiteralInstance {
    type Output = Result<Self, E>;
    fn bitand(self, rhs: Self) -> Self::Output {
        self.op_bitand(&rhs)
    }
}
impl BitXor for LiteralInstance {
    type Output = Result<Self, E>;
    fn bitxor(self, rhs: Self) -> Self::Output {
        self.op_bitxor(&rhs)
    }
}
impl Shl for LiteralInstance {
    type Output = Result<Self, E>;
    fn shl(self, rhs: Self) -> Self::Output {
        self.op_shl(&rhs)
    }
}
impl Shr for LiteralInstance {
    type Output = Result<Self, E>;
    fn shr(self, rhs: Self) -> Self::Output {
        self.op_shr(&rhs)
    }
}

// OPS VEC

impl Neg for &VecInstance {
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
impl Mul<&MatInstance> for &VecInstance {
    type Output = Result<VecInstance, E>;
    fn mul(self, rhs: &MatInstance) -> Self::Output {
        self.op_mul_mat(rhs)
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
impl BitOr for &VecInstance {
    type Output = Result<VecInstance, E>;
    fn bitor(self, rhs: Self) -> Self::Output {
        self.op_bitor(rhs)
    }
}
impl BitAnd for &VecInstance {
    type Output = Result<VecInstance, E>;
    fn bitand(self, rhs: Self) -> Self::Output {
        self.op_bitand(rhs)
    }
}
impl BitXor for &VecInstance {
    type Output = Result<VecInstance, E>;
    fn bitxor(self, rhs: Self) -> Self::Output {
        self.op_bitxor(rhs)
    }
}

// OPS MAT

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
impl Mul<&VecInstance> for &MatInstance {
    type Output = Result<VecInstance, E>;
    fn mul(self, rhs: &VecInstance) -> Self::Output {
        self.op_mul_vec(rhs)
    }
}

// OPS INSTANCE

impl Not for &Instance {
    type Output = Result<Instance, E>;
    fn not(self) -> Self::Output {
        self.op_not()
    }
}
impl Neg for &Instance {
    type Output = Result<Instance, E>;
    fn neg(self) -> Self::Output {
        self.op_neg()
    }
}
impl Add for &Instance {
    type Output = Result<Instance, E>;
    fn add(self, rhs: Self) -> Self::Output {
        self.op_add(rhs)
    }
}
impl Sub for &Instance {
    type Output = Result<Instance, E>;
    fn sub(self, rhs: Self) -> Self::Output {
        self.op_sub(rhs)
    }
}
impl Mul for &Instance {
    type Output = Result<Instance, E>;
    fn mul(self, rhs: Self) -> Self::Output {
        self.op_mul(rhs)
    }
}
impl Div for &Instance {
    type Output = Result<Instance, E>;
    fn div(self, rhs: Self) -> Self::Output {
        self.op_div(rhs)
    }
}
impl Rem for &Instance {
    type Output = Result<Instance, E>;
    fn rem(self, rhs: Self) -> Self::Output {
        self.op_rem(rhs)
    }
}
impl BitOr for &Instance {
    type Output = Result<Instance, E>;
    fn bitor(self, rhs: Self) -> Self::Output {
        self.op_bitor(rhs)
    }
}
impl BitAnd for &Instance {
    type Output = Result<Instance, E>;
    fn bitand(self, rhs: Self) -> Self::Output {
        self.op_bitand(rhs)
    }
}
