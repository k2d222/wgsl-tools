use std::iter::zip;

use super::{
    call_builtin, get_builtin_fn, Address, ConstEvalError, Context, Convert, EvalTy, Exec, Flow,
    Instance, LiteralInstance, RefInstance, SyntaxUtil, Ty, Type, VecInstance, ATTR_BUILTIN,
};

use itertools::Itertools;
use wgsl_parse::syntax::*;

type E = ConstEvalError;

pub trait Eval {
    fn eval(&self, ctx: &mut Context) -> Result<Instance, E>;

    fn eval_value(&self, ctx: &mut Context) -> Result<Instance, E> {
        let inst = self.eval(ctx)?;
        if let Instance::Ref(r) = inst {
            r.value(ctx).cloned()
        } else {
            Ok(inst)
        }
    }
}

impl Eval for Expression {
    fn eval(&self, ctx: &mut Context) -> Result<Instance, E> {
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
    fn eval(&self, _ctx: &mut Context) -> Result<Instance, E> {
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
    fn eval(&self, ctx: &mut Context) -> Result<Instance, E> {
        self.expression.eval(ctx)
    }
}

impl Eval for NamedComponentExpression {
    fn eval(&self, ctx: &mut Context) -> Result<Instance, E> {
        fn vec_eval(v: &VecInstance, comp: &String, addr: Option<Address>) -> Result<Instance, E> {
            if !check_swizzle(comp) {
                return Err(E::Swizzle(comp.clone()));
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
                    .ok_or_else(|| E::OutOfBounds(*i, v.ty(), v.n() as usize)),
                _ => {
                    let components = indices
                        .iter()
                        .map(|i| {
                            v.get(*i)
                                .cloned()
                                .ok_or_else(|| E::OutOfBounds(*i, v.ty(), v.n() as usize))
                        })
                        .collect::<Result<_, _>>()?;
                    Ok(VecInstance::new(components).into())
                }
            }
        }

        fn eval_inst(base: Instance, comp: &String, ctx: &mut Context) -> Result<Instance, E> {
            match &base {
                Instance::Struct(s) => {
                    let val = s
                        .components
                        .get(comp)
                        .ok_or_else(|| E::Component(Type::Struct(s.name.clone()), comp.clone()))?;
                    Ok(val.clone())
                }
                Instance::Vec(v) => vec_eval(v, comp, None),
                Instance::Ref(r) => {
                    let val = r.value(ctx)?;
                    if r.ty != val.ty() {
                        Err(E::RefType(val.ty(), r.ty.clone()))
                    } else {
                        match val {
                            Instance::Struct(s) => {
                                let comp_val = s.components.get(comp).ok_or_else(|| {
                                    E::Component(Type::Struct(s.name.clone()), comp.clone())
                                })?;
                                let mut address = r.address.clone();
                                address.view.append_member(comp.clone());
                                Ok(Instance::Ref(RefInstance {
                                    ty: comp_val.ty(),
                                    address,
                                }))
                            }
                            Instance::Vec(v) => vec_eval(v, comp, Some(r.address.clone())),
                            _ => Err(E::Component(base.ty(), comp.clone())),
                        }
                    }
                }
                _ => Err(E::Component(base.ty(), comp.clone())),
            }
        }

        let base = self.base.eval(ctx)?;
        eval_inst(base, &self.component, ctx)
    }
}

impl Eval for IndexingExpression {
    fn eval(&self, ctx: &mut Context) -> Result<Instance, E> {
        fn eval_inst(base: Instance, index: usize, ctx: &mut Context) -> Result<Instance, E> {
            match base {
                Instance::Vec(v) => v
                    .get(index)
                    .map(|e| Instance::Literal(e.clone()))
                    .ok_or_else(|| E::OutOfBounds(index, v.ty(), v.n() as usize)),
                Instance::Mat(m) => m
                    .col(index)
                    .map(Into::into)
                    .ok_or_else(|| E::OutOfBounds(index, m.ty(), m.c() as usize)),
                Instance::Array(a) => a
                    .get(index)
                    .cloned()
                    .ok_or_else(|| E::OutOfBounds(index, a.ty(), a.n())),
                Instance::Ref(r) => {
                    let val = r.value(ctx)?;
                    if r.ty != val.ty() {
                        return Err(E::RefType(val.ty(), r.ty.clone()));
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
                            .ok_or_else(|| E::OutOfBounds(index, v.ty(), v.n() as usize)),
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
                            .ok_or_else(|| E::OutOfBounds(index, m.ty(), m.c() as usize)),
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
                            .ok_or_else(|| E::OutOfBounds(index, s.ty(), s.n() as usize))?,
                        _ => Err(E::NotIndexable(r.ty.clone())),
                    }
                }
                Instance::Literal(_)
                | Instance::Struct(_)
                | Instance::Ptr(_)
                | Instance::Type(_)
                | Instance::Void => Err(E::NotIndexable(base.ty())),
            }
        }

        let base = self.base.eval(ctx)?;
        let index = self.index.eval_value(ctx)?;
        let index = match index {
            Instance::Literal(LiteralInstance::AbstractInt(i)) => Ok(i as usize),
            Instance::Literal(LiteralInstance::I32(i)) => Ok(i as usize),
            Instance::Literal(LiteralInstance::U32(i)) => Ok(i as usize),
            _ => Err(E::Index(index.ty())),
        }?;

        eval_inst(base, index, ctx)
    }
}

impl Eval for UnaryExpression {
    fn eval(&self, ctx: &mut Context) -> Result<Instance, E> {
        if self.operator == UnaryOperator::AddressOf {
            let operand = self.operand.eval(ctx)?;
            match operand {
                Instance::Ref(r) => Ok(Instance::Ref(RefInstance {
                    ty: r.ty.clone(),
                    address: r.address.clone(),
                })),
                operand @ _ => Err(E::Unary(self.operator, operand.ty())),
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
                    operand @ _ => Err(E::Unary(self.operator, operand.ty())),
                },
            }
        }
    }
}

impl Eval for BinaryExpression {
    fn eval(&self, ctx: &mut Context) -> Result<Instance, E> {
        let lhs = self.left.eval_value(ctx)?;

        if self.operator == BinaryOperator::ShortCircuitOr {
            match lhs {
                Instance::Literal(LiteralInstance::Bool(true)) => Ok(lhs),
                Instance::Literal(LiteralInstance::Bool(false)) => {
                    let rhs = self.right.eval_value(ctx)?;
                    match rhs {
                        Instance::Literal(LiteralInstance::Bool(true)) => Ok(rhs),
                        Instance::Literal(LiteralInstance::Bool(false)) => Ok(rhs),
                        _ => Err(E::Binary(self.operator, lhs.ty(), rhs.ty())),
                    }
                }
                _ => Err(E::Binary(self.operator, lhs.ty(), Type::Bool)),
            }
        } else if self.operator == BinaryOperator::ShortCircuitAnd {
            match lhs {
                Instance::Literal(LiteralInstance::Bool(true)) => {
                    let rhs = self.right.eval_value(ctx)?;
                    match rhs {
                        Instance::Literal(LiteralInstance::Bool(true)) => Ok(rhs),
                        Instance::Literal(LiteralInstance::Bool(false)) => Ok(rhs),
                        _ => Err(E::Binary(self.operator, lhs.ty(), rhs.ty())),
                    }
                }
                Instance::Literal(LiteralInstance::Bool(false)) => Ok(lhs),
                _ => Err(E::Binary(self.operator, lhs.ty(), Type::Bool)),
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

impl Eval for FunctionCall {
    fn eval(&self, ctx: &mut Context) -> Result<Instance, E> {
        let (name, tplt) = self
            .template_args
            .is_none()
            .then(|| {
                ctx.source
                    .resolve_alias(&self.name)
                    .map(|ty| (ty.name, ty.template_args))
                    .unwrap_or((self.name.clone(), None))
            })
            .unwrap_or((self.name.clone(), self.template_args.clone()));

        let args = self
            .arguments
            .iter()
            .map(|a| a.eval_value(ctx))
            .collect::<Result<Vec<_>, _>>()?;

        let tplt = tplt.map_or(Ok(None), |tplt| {
            tplt.iter()
                .map(|e| e.eval_value(ctx))
                .collect::<Result<Vec<_>, _>>()
                .map(Some)
        })?;

        let decl = ctx
            .source
            .decl_function(&name)
            .ok_or_else(|| E::UnknownFunction(name.clone()))?;

        if decl.body.attributes.contains(&ATTR_BUILTIN) {
            return call_builtin(&name, tplt, args);
        }

        if self.arguments.len() != decl.parameters.len() {
            return Err(E::ParamCount(decl.parameters.len(), self.arguments.len()));
        }

        let ret_ty = decl
            .return_type
            .as_ref()
            .map(|e| e.eval_ty(ctx))
            .unwrap_or(Ok(Type::Void))?;

        ctx.scope.push();

        for (a, p) in zip(&args, &decl.parameters) {
            let p_ty = p.ty.eval_ty(ctx)?;
            let a_ty = a.ty();

            if &a_ty != &p_ty {
                return Err(E::ParamType(a_ty, p_ty));
            }
        }

        for (a, p) in zip(args, &decl.parameters) {
            ctx.scope.add(p.name.clone(), ctx.memory.len());
            ctx.memory.push(a);
        }

        // TODO: ensure has const attribute

        let flow = decl.body.exec(ctx)?;
        ctx.scope.pop();

        match flow {
            Flow::Next => Ok(Instance::Void),
            Flow::Break | Flow::Continue => Err(E::FlowInFunction(flow)),
            Flow::Return(inst) => inst
                .convert_to(&ret_ty)
                .ok_or(E::ReturnType(ret_ty, inst.ty())),
        }
    }
}

impl Eval for IdentifierExpression {
    fn eval(&self, ctx: &mut Context) -> Result<Instance, E> {
        let ptr = ctx
            .scope
            .get(&self.name)
            .ok_or_else(|| E::NoDecl(self.name.clone()))?;
        let address = Address::new(ptr);
        let inst = ctx.memory.get(ptr).ok_or_else(|| E::Ref(address.clone()))?;
        Ok(RefInstance {
            ty: inst.ty(),
            address,
        }
        .into())
    }
}

impl Eval for TypeExpression {
    fn eval(&self, ctx: &mut Context) -> Result<Instance, E> {
        todo!()
    }
}

fn check_swizzle(swizzle: &str) -> bool {
    // reference: https://www.w3.org/TR/WGSL/#swizzle-names
    (1..=4).contains(&swizzle.len())
        && (swizzle.chars().all(|c| "xyzw".contains(c))
            || swizzle.chars().all(|c| "rgba".contains(c)))
}
