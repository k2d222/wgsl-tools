use super::{
    ConstEvalError, Context, Instance, LiteralInstance, RefInstance, Ty, Type, VecInstance,
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
    fn eval(&self, ctx: &Context) -> Result<Instance, ConstEvalError> {
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
        fn vec_eval(v: &VecInstance, comp: &String) -> Result<Instance, ConstEvalError> {
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
                    _ => unreachable!(), // SAFETY: we checked with the regex above.
                })
                .collect_vec();
            match indices.as_slice() {
                [i] => v
                    .get(*i)
                    .map(|e| Instance::Literal(e.clone()))
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
                Instance::Vec(v) => vec_eval(v, comp),
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
                            Instance::Vec(v) => vec_eval(v, comp),
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
                            .map(|e| {
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
                Instance::Literal(_) | Instance::Struct(_) | Instance::Ptr(_) => {
                    Err(ConstEvalError::NotIndexable(base.ty()))
                }
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
        let operand = self.operand.eval_value(ctx)?;
        match self.operator {
            UnaryOperator::LogicalNegation => todo!(),
            UnaryOperator::Negation => -operand,
            UnaryOperator::BitwiseComplement => todo!(),
            UnaryOperator::AddressOf => todo!(),
            UnaryOperator::Indirection => todo!(),
        }
        //     match self.operator {
        //         UnaryOperator::LogicalNegation => match &self.operand.eval_value(ctx)? {
        //             Instance::Literal(LiteralInstance::Bool(b)) => Ok(LiteralInstance::Bool(!b).into()),
        //             operand @ Instance::Vec(v) => {
        //                 let components = v
        //                     .iter()
        //                     .map(|e| match e {
        //                         LiteralInstance::True => Ok(LiteralInstance::False),
        //                         LiteralInstance::False => Ok(LiteralInstance::True),
        //                         _ => Err(ConstEvalError::InvalidUnary(self.operator, operand.ty())),
        //                     })
        //                     .collect::<Result<_, _>>()?;
        //                 Ok(Instance::Vec(VecInstance {
        //                     ty: Type::Bool,
        //                     components,
        //                 }))
        //             }
        //             operand @ _ => Err(ConstEvalError::InvalidUnary(self.operator, operand.ty())),
        //         },
        //         UnaryOperator::Negation => match &self.operand.eval_value(ctx)? {
        //             Instance::Literal(LiteralInstance::AbstractInt(n)) => {
        //                 Ok(Instance::Literal(LiteralInstance::AbstractInt(-n)))
        //             }
        //             Instance::Literal(LiteralInstance::AbstractFloat(n)) => {
        //                 Ok(Instance::Literal(LiteralInstance::AbstractFloat(-n)))
        //             }
        //             Instance::Literal(LiteralInstance::I32(n)) => {
        //                 Ok(Instance::Literal(LiteralInstance::I32(-n)))
        //             }
        //             Instance::Literal(LiteralInstance::F32(n)) => {
        //                 Ok(Instance::Literal(LiteralInstance::F32(-n)))
        //             }
        //             Instance::Literal(LiteralInstance::F16(n)) => {
        //                 Ok(Instance::Literal(LiteralInstance::F16(-n)))
        //             }
        //             operand @ Instance::Vec(v) => {
        //                 let components = v
        //                     .components
        //                     .iter()
        //                     .map(|e| match e {
        //                         LiteralInstance::AbstractInt(n) => Ok(LiteralInstance::AbstractInt(-n)),
        //                         LiteralInstance::AbstractFloat(n) => {
        //                             Ok(LiteralInstance::AbstractFloat(-n))
        //                         }
        //                         LiteralInstance::I32(n) => Ok(LiteralInstance::I32(-n)),
        //                         LiteralInstance::F32(n) => Ok(LiteralInstance::F32(-n)),
        //                         LiteralInstance::F16(n) => Ok(LiteralInstance::F16(-n)),
        //                         _ => Err(ConstEvalError::InvalidUnary(self.operator, operand.ty())),
        //                     })
        //                     .collect::<Result<_, _>>()?;
        //                 Ok(Instance::Vec(VecInstance {
        //                     ty: v.ty.clone(),
        //                     components,
        //                 }))
        //             }
        //             operand @ _ => Err(ConstEvalError::InvalidUnary(self.operator, operand.ty())),
        //         },

        //         UnaryOperator::BitwiseComplement => match &self.operand.eval_value(ctx)? {
        //             Instance::Literal(LiteralInstance::AbstractInt(n)) => {
        //                 Ok(Instance::Literal(LiteralInstance::AbstractInt(!n)))
        //             }
        //             Instance::Literal(LiteralInstance::I32(n)) => {
        //                 Ok(Instance::Literal(LiteralInstance::I32(!n)))
        //             }
        //             Instance::Literal(LiteralInstance::U32(n)) => {
        //                 Ok(Instance::Literal(LiteralInstance::U32(!n)))
        //             }
        //             operand @ Instance::Vec(v) => {
        //                 let components = v
        //                     .components
        //                     .iter()
        //                     .map(|e| match e {
        //                         LiteralInstance::AbstractInt(n) => Ok(LiteralInstance::AbstractInt(-n)),
        //                         LiteralInstance::I32(n) => Ok(LiteralInstance::I32(!n)),
        //                         LiteralInstance::U32(n) => Ok(LiteralInstance::U32(!n)),
        //                         _ => Err(ConstEvalError::InvalidUnary(self.operator, operand.ty())),
        //                     })
        //                     .collect::<Result<_, _>>()?;
        //                 Ok(Instance::Vec(VecInstance {
        //                     ty: v.ty.clone(),
        //                     components,
        //                 }))
        //             }
        //             operand @ _ => Err(ConstEvalError::InvalidUnary(self.operator, operand.ty())),
        //         },

        //         UnaryOperator::AddressOf => match self.operand.eval(ctx)? {
        //             Instance::Ref(r) => Ok(Instance::Ref(RefInstance {
        //                 ty: r.ty.clone(),
        //                 address: r.address.clone(),
        //             })),
        //             operand @ _ => Err(ConstEvalError::InvalidUnary(self.operator, operand.ty())),
        //         },
        //         UnaryOperator::Indirection => match self.operand.eval_value(ctx)? {
        //             Instance::Ptr(p) => Ok(Instance::Ref(RefInstance {
        //                 ty: p.ty.clone(),
        //                 address: p.address.clone(),
        //             })),
        //             operand @ _ => Err(ConstEvalError::InvalidUnary(self.operator, operand.ty())),
        //         },
        //     }
        // }
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
                BinaryOperator::Addition => &lhs + &rhs,
                BinaryOperator::Subtraction => &lhs - &rhs,
                BinaryOperator::Multiplication => &lhs * &rhs,
                BinaryOperator::Division => &lhs / &rhs,
                BinaryOperator::Remainder => &lhs % &rhs,
                BinaryOperator::Equality => lhs.op_eq(&rhs),
                BinaryOperator::Inequality => lhs.op_ne(&rhs),
                BinaryOperator::LessThan => lhs.op_lt(&rhs),
                BinaryOperator::LessThanEqual => lhs.op_le(&rhs),
                BinaryOperator::GreaterThan => lhs.op_gt(&rhs),
                BinaryOperator::GreaterThanEqual => lhs.op_ge(&rhs),
                BinaryOperator::BitwiseOr => todo!(),
                BinaryOperator::BitwiseAnd => todo!(),
                BinaryOperator::BitwiseXor => todo!(),
                BinaryOperator::ShiftLeft => todo!(),
                BinaryOperator::ShiftRight => todo!(),
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
        todo!()
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
