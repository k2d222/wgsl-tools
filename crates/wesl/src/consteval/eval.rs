use std::iter::zip;

use crate::consteval::MatInstance;

use super::{
    call_builtin, AccessMode, Context, Convert, EvalError, EvalTy, Exec, Flow, Instance,
    LiteralInstance, PtrInstance, RefInstance, SyntaxUtil, Ty, Type, VecInstance, ATTR_BUILTIN,
};

use half::f16;
use itertools::Itertools;
use wgsl_parse::{span::Spanned, syntax::*};

type E = EvalError;

pub trait Eval {
    fn eval(&self, ctx: &mut Context) -> Result<Instance, E>;

    fn eval_value(&self, ctx: &mut Context) -> Result<Instance, E> {
        let inst = self.eval(ctx)?;
        if let Instance::Ref(r) = inst {
            let r = r.read()?;
            Ok((*r).clone())
        } else {
            Ok(inst)
        }
    }
}

impl Eval for Spanned<Expression> {
    fn eval(&self, ctx: &mut Context) -> Result<Instance, E> {
        self.node().eval(ctx).inspect_err(|_| {
            if ctx.err_span.is_none() {
                ctx.err_span = Some(self.span().clone());
            }
        })
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
            LiteralExpression::F16(l) => Ok(LiteralInstance::F16(f16::from_f32(*l)).into()), // TODO: check infinity
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
        fn vec_comp(
            v: &VecInstance,
            comp: &String,
            r: Option<&RefInstance>,
        ) -> Result<Instance, E> {
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
                [i] => {
                    if let Some(r) = r {
                        r.view_index(*i).map(Into::into)
                    } else {
                        v.get(*i)
                            .cloned()
                            .map(Into::into)
                            .ok_or_else(|| E::OutOfBounds(*i, v.ty(), v.n() as usize))
                    }
                }
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

        fn inst_comp(base: Instance, comp: &String, ctx: &mut Context) -> Result<Instance, E> {
            match &base {
                Instance::Struct(s) => {
                    let val = s
                        .members
                        .get(comp)
                        .ok_or_else(|| E::Component(Type::Struct(s.name.clone()), comp.clone()))?;
                    Ok(val.clone())
                }
                Instance::Vec(v) => vec_comp(v, comp, None),
                Instance::Ref(r) => match &*r.read()? {
                    Instance::Struct(s) => r.view_member(comp.clone()).map(Into::into),
                    Instance::Vec(v) => vec_comp(v, comp, Some(r)),
                    _ => Err(E::Component(base.ty(), comp.clone())),
                },
                _ => Err(E::Component(base.ty(), comp.clone())),
            }
        }

        let base = self.base.eval(ctx)?;
        inst_comp(base, &self.component, ctx)
    }
}

impl Eval for IndexingExpression {
    fn eval(&self, ctx: &mut Context) -> Result<Instance, E> {
        fn vec_index(v: &VecInstance, index: usize) -> Result<Instance, E> {
            v.get(index)
                .map(|e| Instance::Literal(e.clone()))
                .ok_or_else(|| E::OutOfBounds(index, v.ty(), v.n() as usize))
        }
        fn mat_index(m: &MatInstance, index: usize) -> Result<Instance, E> {
            m.col(index)
                .map(Into::into)
                .ok_or_else(|| E::OutOfBounds(index, m.ty(), m.c() as usize))
        }
        fn index_inst(base: &Instance, index: usize) -> Result<Instance, E> {
            match base {
                Instance::Vec(v) => vec_index(v, index),
                Instance::Mat(m) => mat_index(m, index),
                Instance::Array(a) => a
                    .get(index)
                    .cloned()
                    .ok_or_else(|| E::OutOfBounds(index, a.ty(), a.n())),
                Instance::Ref(r) => match &*r.read()? {
                    Instance::Vec(v) => vec_index(v, index),
                    Instance::Mat(m) => mat_index(m, index),
                    _ => Err(E::NotIndexable(r.ty())),
                },
                _ => Err(E::NotIndexable(base.ty())),
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

        index_inst(&base, index)
    }
}

impl Eval for UnaryExpression {
    fn eval(&self, ctx: &mut Context) -> Result<Instance, E> {
        if self.operator == UnaryOperator::AddressOf {
            let operand = self.operand.eval(ctx)?;
            match operand {
                Instance::Ref(r) => Ok(PtrInstance::from(r).into()),
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
                    Instance::Ptr(p) => Ok(RefInstance::from(p).into()),
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
            return call_builtin(&name, tplt, args, ctx);
        }

        // TODO: ensure has const attribute

        if self.arguments.len() != decl.parameters.len() {
            return Err(E::ParamCount(
                decl.name.clone(),
                decl.parameters.len(),
                self.arguments.len(),
            ));
        }

        let ret_ty = decl
            .return_type
            .as_ref()
            .map(|e| e.eval_ty(ctx))
            .unwrap_or(Ok(Type::Void))?;

        ctx.scope.push();
        let flow = {
            for (a, p) in zip(&args, &decl.parameters) {
                let p_ty = p.ty.eval_ty(ctx)?;
                let a_ty = a.ty();

                if &a_ty != &p_ty {
                    return Err(E::ParamType(a_ty, p_ty));
                }
            }

            for (a, p) in zip(args, &decl.parameters) {
                ctx.scope.add(p.name.clone(), a, AccessMode::Read);
            }

            ctx.err_decl = Some(decl.name.to_string());
            let flow = decl.body.exec(ctx)?;
            ctx.err_decl = None;
            flow
        };
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
        if let Some(r) = ctx.scope.get(&self.name) {
            Ok(r.clone().into())
        } else {
            let ty = self.name.as_str().eval_ty(ctx)?;
            Ok(ty.into())
        }
    }
}

impl Eval for TypeExpression {
    fn eval(&self, ctx: &mut Context) -> Result<Instance, E> {
        todo!()
    }
}

impl Eval for TemplateArg {
    fn eval(&self, ctx: &mut Context) -> Result<Instance, E> {
        self.expression.eval(ctx)
    }
}

fn check_swizzle(swizzle: &str) -> bool {
    // reference: https://www.w3.org/TR/WGSL/#swizzle-names
    (1..=4).contains(&swizzle.len())
        && (swizzle.chars().all(|c| "xyzw".contains(c))
            || swizzle.chars().all(|c| "rgba".contains(c)))
}
