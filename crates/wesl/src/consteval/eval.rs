use super::{
    Address, ConstEvalError, Context, Instance, LiteralInstance, RefInstance, Ty, Type, VecInstance,
};

use itertools::Itertools;
use wgsl_parse::syntax::*;

pub trait Eval {
    fn eval(&self, ctx: &Context) -> Result<Instance, ConstEvalError>;

    fn eval_value(&self, ctx: &Context) -> Result<Instance, ConstEvalError> {
        let inst = self.eval(ctx)?;
        if let Instance::Ref(r) = inst {
            r.value(ctx).cloned()
        } else {
            Ok(inst)
        }
    }
}

impl Eval for Expression {
    fn eval(&self, ctx: &Context) -> Result<Instance, ConstEvalError> {
        match self {
            Expression::Literal(e) => e.eval(ctx),
            Expression::Parenthesized(e) => e.eval(ctx),
            Expression::NamedComponent(e) => e.eval(ctx),
            Expression::Indexing(e) => e.eval(ctx),
            Expression::Unary(e) => e.eval(ctx),
            Expression::Binary(e) => e.eval(ctx),
            Expression::FunctionCall(e) => e.eval(ctx),
            Expression::Identifier(e) => e.eval(ctx),
            Expression::Type(e) => e.eval(ctx),
        }
    }
}

impl Eval for LiteralExpression {
    fn eval(&self, _ctx: &Context) -> Result<Instance, ConstEvalError> {
        match self {
            LiteralExpression::Bool(l) => Ok(LiteralInstance::Bool(*l).into()),
            LiteralExpression::AbstractInt(l) => Ok(LiteralInstance::AbstractInt(*l).into()),
            LiteralExpression::AbstractFloat(l) => Ok(LiteralInstance::AbstractFloat(*l).into()),
            LiteralExpression::I32(l) => Ok(LiteralInstance::I32(*l).into()),
            LiteralExpression::U32(l) => Ok(LiteralInstance::U32(*l).into()),
            LiteralExpression::F32(l) => Ok(LiteralInstance::F32(*l).into()),
            LiteralExpression::F16(l) => Ok(LiteralInstance::F16(*l).into()),
        }
    }
}

impl Eval for ParenthesizedExpression {
    fn eval(&self, ctx: &Context) -> Result<Instance, ConstEvalError> {
        self.expression.eval(ctx)
    }
}

impl Eval for NamedComponentExpression {
    fn eval(&self, ctx: &Context) -> Result<Instance, ConstEvalError> {
        fn vec_eval(
            v: &VecInstance,
            comp: &String,
            addr: Option<Address>,
        ) -> Result<Instance, ConstEvalError> {
            if !check_swizzle(comp) {
                return Err(ConstEvalError::InvalidSwizzle(comp.clone()));
            }
            let indices = comp
                .chars()
                .map(|c| match c {
                    'x' | 'r' => 0usize,
                    'y' | 'g' => 1usize,
                    'z' | 'b' => 2usize,
                    'w' | 'a' => 3usize,
                    _ => unreachable!(), // SAFETY: check_swizzle above checks it.
                })
                .collect_vec();
            match indices.as_slice() {
                [i] => v
                    .get(*i)
                    .map(|e| {
                        if let Some(mut address) = addr {
                            address.view.append_index(*i);
                            Instance::Ref(RefInstance {
                                ty: e.ty(),
                                address,
                            })
                            .into()
                        } else {
                            e.clone().into()
                        }
                    })
                    .ok_or_else(|| ConstEvalError::OutOfBounds(*i, v.ty(), v.n() as usize)),
                _ => {
                    let components = indices
                        .iter()
                        .map(|i| {
                            v.get(*i).cloned().ok_or_else(|| {
                                ConstEvalError::OutOfBounds(*i, v.ty(), v.n() as usize)
                            })
                        })
                        .collect::<Result<_, _>>()?;
                    Ok(VecInstance::new(components).into())
                }
            }
        }

        fn eval_inst(
            base: Instance,
            comp: &String,
            ctx: &Context,
        ) -> Result<Instance, ConstEvalError> {
            match &base {
                Instance::Struct(s) => {
                    let val = s.components.get(comp).ok_or_else(|| {
                        ConstEvalError::NoComponent(Type::Struct(s.name.clone()), comp.clone())
                    })?;
                    Ok(val.clone())
                }
                Instance::Vec(v) => vec_eval(v, comp, None),
                Instance::Ref(r) => {
                    let val = r.value(ctx)?;
                    if r.ty != val.ty() {
                        Err(ConstEvalError::InvalidRefType(val.ty(), r.ty.clone()))
                    } else {
                        match val {
                            Instance::Struct(s) => {
                                let comp_val = s.components.get(comp).ok_or_else(|| {
                                    ConstEvalError::NoComponent(
                                        Type::Struct(s.name.clone()),
                                        comp.clone(),
                                    )
                                })?;
                                let mut address = r.address.clone();
                                address.view.append_member(comp.clone());
                                Ok(Instance::Ref(RefInstance {
                                    ty: comp_val.ty(),
                                    address,
                                }))
                            }
                            Instance::Vec(v) => vec_eval(v, comp, Some(r.address.clone())),
                            _ => Err(ConstEvalError::NoComponent(base.ty(), comp.clone())),
                        }
                    }
                }
                _ => Err(ConstEvalError::NoComponent(base.ty(), comp.clone())),
            }
        }

        let base = self.base.eval(ctx)?;
        eval_inst(base, &self.component, ctx)
    }
}

impl Eval for IndexingExpression {
    fn eval(&self, ctx: &Context) -> Result<Instance, ConstEvalError> {
        fn eval_inst(
            base: Instance,
            index: usize,
            ctx: &Context,
        ) -> Result<Instance, ConstEvalError> {
            match base {
                Instance::Vec(v) => v
                    .get(index)
                    .map(|e| Instance::Literal(e.clone()))
                    .ok_or_else(|| ConstEvalError::OutOfBounds(index, v.ty(), v.n() as usize)),
                Instance::Mat(m) => m
                    .col(index)
                    .map(Into::into)
                    .ok_or_else(|| ConstEvalError::OutOfBounds(index, m.ty(), m.c() as usize)),
                Instance::Array(a) => a
                    .get(index)
                    .cloned()
                    .ok_or_else(|| ConstEvalError::OutOfBounds(index, a.ty(), a.n())),
                Instance::Ref(r) => {
                    let val = r.value(ctx)?;
                    if r.ty != val.ty() {
                        return Err(ConstEvalError::InvalidRefType(val.ty(), r.ty.clone()));
                    }
                    match val {
                        Instance::Vec(v) => v
                            .get(index)
                            .map(|_| {
                                let mut address = r.address.clone();
                                address.view.append_index(index);
                                Instance::Ref(RefInstance {
                                    ty: v.ty(),
                                    address,
                                })
                            })
                            .ok_or_else(|| {
                                ConstEvalError::OutOfBounds(index, v.ty(), v.n() as usize)
                            }),
                        Instance::Mat(m) => m
                            .col(index)
                            .map(|v| {
                                let mut address = r.address.clone();
                                address.view.append_index(index);
                                Instance::Ref(RefInstance {
                                    ty: v.ty(),
                                    address,
                                })
                            })
                            .ok_or_else(|| {
                                ConstEvalError::OutOfBounds(index, m.ty(), m.c() as usize)
                            }),
                        Instance::Array(s) => s
                            .get(index)
                            .map(|e| {
                                let mut address = r.address.clone();
                                address.view.append_index(index);
                                Ok(Instance::Ref(RefInstance {
                                    ty: e.ty(),
                                    address,
                                }))
                            })
                            .ok_or_else(|| {
                                ConstEvalError::OutOfBounds(index, s.ty(), s.n() as usize)
                            })?,
                        _ => Err(ConstEvalError::NotIndexable(r.ty.clone())),
                    }
                }
                Instance::Literal(_)
                | Instance::Struct(_)
                | Instance::Ptr(_)
                | Instance::Type(_)
                | Instance::Void => Err(ConstEvalError::NotIndexable(base.ty())),
            }
        }

        let base = self.base.eval(ctx)?;
        let index = self.index.eval_value(ctx)?;
        let index = match index {
            Instance::Literal(LiteralInstance::AbstractInt(i)) => Ok(i as usize),
            Instance::Literal(LiteralInstance::I32(i)) => Ok(i as usize),
            Instance::Literal(LiteralInstance::U32(i)) => Ok(i as usize),
            _ => Err(ConstEvalError::InvalidIndex(index.ty())),
        }?;

        eval_inst(base, index, ctx)
    }
}

impl Eval for UnaryExpression {
    fn eval(&self, ctx: &Context) -> Result<Instance, ConstEvalError> {
        if self.operator == UnaryOperator::AddressOf {
            let operand = self.operand.eval(ctx)?;
            match operand {
                Instance::Ref(r) => Ok(Instance::Ref(RefInstance {
                    ty: r.ty.clone(),
                    address: r.address.clone(),
                })),
                operand @ _ => Err(ConstEvalError::InvalidUnary(self.operator, operand.ty())),
            }
        } else {
            let operand = self.operand.eval_value(ctx)?;
            match self.operator {
                UnaryOperator::LogicalNegation => operand.op_not(),
                UnaryOperator::Negation => operand.op_neg(),
                UnaryOperator::BitwiseComplement => operand.op_bitnot(),
                UnaryOperator::AddressOf => unreachable!(),
                UnaryOperator::Indirection => match operand {
                    Instance::Ptr(p) => Ok(Instance::Ref(RefInstance {
                        ty: p.ty.clone(),
                        address: p.address.clone(),
                    })),
                    operand @ _ => Err(ConstEvalError::InvalidUnary(self.operator, operand.ty())),
                },
            }
        }
    }
}

impl Eval for BinaryExpression {
    fn eval(&self, ctx: &Context) -> Result<Instance, ConstEvalError> {
        let lhs = self.left.eval_value(ctx)?;

        if self.operator == BinaryOperator::ShortCircuitOr {
            match lhs {
                Instance::Literal(LiteralInstance::Bool(true)) => Ok(lhs),
                Instance::Literal(LiteralInstance::Bool(false)) => {
                    let rhs = self.right.eval_value(ctx)?;
                    match rhs {
                        Instance::Literal(LiteralInstance::Bool(true)) => Ok(rhs),
                        Instance::Literal(LiteralInstance::Bool(false)) => Ok(rhs),
                        _ => Err(ConstEvalError::InvalidBinary(
                            self.operator,
                            lhs.ty(),
                            rhs.ty(),
                        )),
                    }
                }
                _ => Err(ConstEvalError::InvalidBinary(
                    self.operator,
                    lhs.ty(),
                    Type::Bool,
                )),
            }
        } else if self.operator == BinaryOperator::ShortCircuitAnd {
            match lhs {
                Instance::Literal(LiteralInstance::Bool(true)) => {
                    let rhs = self.right.eval_value(ctx)?;
                    match rhs {
                        Instance::Literal(LiteralInstance::Bool(true)) => Ok(rhs),
                        Instance::Literal(LiteralInstance::Bool(false)) => Ok(rhs),
                        _ => Err(ConstEvalError::InvalidBinary(
                            self.operator,
                            lhs.ty(),
                            rhs.ty(),
                        )),
                    }
                }
                Instance::Literal(LiteralInstance::Bool(false)) => Ok(lhs),
                _ => Err(ConstEvalError::InvalidBinary(
                    self.operator,
                    lhs.ty(),
                    Type::Bool,
                )),
            }
        } else {
            let rhs = self.right.eval_value(ctx)?;
            match self.operator {
                BinaryOperator::ShortCircuitOr | BinaryOperator::ShortCircuitAnd => unreachable!(),
                BinaryOperator::Addition => lhs.op_add(&rhs),
                BinaryOperator::Subtraction => lhs.op_sub(&rhs),
                BinaryOperator::Multiplication => lhs.op_mul(&rhs),
                BinaryOperator::Division => lhs.op_div(&rhs),
                BinaryOperator::Remainder => lhs.op_rem(&rhs),
                BinaryOperator::Equality => lhs.op_eq(&rhs),
                BinaryOperator::Inequality => lhs.op_ne(&rhs),
                BinaryOperator::LessThan => lhs.op_lt(&rhs),
                BinaryOperator::LessThanEqual => lhs.op_le(&rhs),
                BinaryOperator::GreaterThan => lhs.op_gt(&rhs),
                BinaryOperator::GreaterThanEqual => lhs.op_ge(&rhs),
                BinaryOperator::BitwiseOr => lhs.op_bitor(&rhs),
                BinaryOperator::BitwiseAnd => lhs.op_bitand(&rhs),
                BinaryOperator::BitwiseXor => lhs.op_bitxor(&rhs),
                BinaryOperator::ShiftLeft => lhs.op_shl(&rhs),
                BinaryOperator::ShiftRight => lhs.op_shr(&rhs),
            }
        }
    }
}

impl Eval for FunctionCallExpression {
    fn eval(&self, ctx: &Context) -> Result<Instance, ConstEvalError> {
        todo!()
    }
}

impl Eval for IdentifierExpression {
    fn eval(&self, ctx: &Context) -> Result<Instance, ConstEvalError> {
        let ptr = *ctx
            .variables
            .get(&self.name)
            .ok_or_else(|| ConstEvalError::NoDecl(self.name.clone()))?;
        let address = Address::new(ptr);
        let inst = ctx
            .memory
            .get(ptr)
            .ok_or_else(|| ConstEvalError::InvalidRef(address.clone()))?;
        Ok(RefInstance {
            ty: inst.ty(),
            address,
        }
        .into())
    }
}

impl Eval for TypeExpression {
    fn eval(&self, ctx: &Context) -> Result<Instance, ConstEvalError> {
        todo!()
    }
}

fn check_swizzle(swizzle: &str) -> bool {
    // reference: https://www.w3.org/TR/WGSL/#swizzle-names
    (1..=4).contains(&swizzle.len())
        && (swizzle.chars().all(|c| "xyzw".contains(c))
            || swizzle.chars().all(|c| "rgba".contains(c)))
}
