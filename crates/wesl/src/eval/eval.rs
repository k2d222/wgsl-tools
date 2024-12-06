use std::iter::zip;

use super::{
    call_builtin, Context, Convert, EvalError, EvalStage, EvalTy, Exec, Flow, Instance,
    LiteralInstance, PtrInstance, RefInstance, StructInstance, SyntaxUtil, Ty, Type, VecInstance,
    ATTR_INTRINSIC,
};

use half::f16;
use itertools::Itertools;
use wgsl_parse::{span::Spanned, syntax::*};

type E = EvalError;

pub trait Eval {
    fn eval(&self, ctx: &mut Context) -> Result<Instance, E>;

    fn eval_value(&self, ctx: &mut Context) -> Result<Instance, E> {
        let mut inst = self.eval(ctx)?;
        while let Instance::Ref(r) = inst {
            let r = r.read()?;
            inst = (*r).clone();
        }
        Ok(inst)
    }
}

// this impl exists purely for eval_value()
impl Eval for Instance {
    fn eval(&self, _ctx: &mut Context) -> Result<Instance, E> {
        Ok(self.clone())
    }
}

impl Eval for Spanned<Expression> {
    fn eval(&self, ctx: &mut Context) -> Result<Instance, E> {
        self.node()
            .eval(ctx)
            .inspect_err(|_| ctx.set_err_expr_ctx(self.span()))
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
            Expression::TypeOrIdentifier(e) => e.eval(ctx),
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
            LiteralExpression::F16(l) => {
                let l = f16::from_f32(*l);
                if l.is_infinite() {
                    // this is not supposed to happen.
                    Err(E::Builtin("invalid `f16` literal value (overflow)"))
                } else {
                    Ok(LiteralInstance::F16(l).into()) // TODO: check infinity
                }
            }
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
            if let [i] = indices.as_slice() {
                if let Some(r) = r {
                    r.view_index(*i).map(Instance::Ref)
                } else {
                    v.get(*i)
                        .cloned()
                        .ok_or_else(|| E::OutOfBounds(*i, v.ty(), v.n() as usize))
                }
            } else {
                let components = indices
                    .into_iter()
                    .map(|i| {
                        v.get(i)
                            .cloned()
                            .ok_or_else(|| E::OutOfBounds(i, v.ty(), v.n() as usize))
                    })
                    .collect::<Result<_, _>>()?;
                Ok(VecInstance::new(components).into())
            }
        }

        fn inst_comp(base: Instance, comp: &String) -> Result<Instance, E> {
            match &base {
                Instance::Struct(s) => {
                    let val = s.member(comp).ok_or_else(|| {
                        E::Component(Type::Struct(s.name().to_string()), comp.clone())
                    })?;
                    Ok(val.clone())
                }
                Instance::Vec(v) => vec_comp(v, comp, None),
                Instance::Ref(r) => match &*r.read()? {
                    Instance::Struct(_) => r.view_member(comp.clone()).map(Into::into),
                    Instance::Vec(v) => vec_comp(v, comp, Some(r)),
                    _ => Err(E::Component(base.ty(), comp.clone())),
                },
                _ => Err(E::Component(base.ty(), comp.clone())),
            }
        }

        let base = self.base.eval(ctx)?;
        inst_comp(base, &self.component)
    }
}

impl Eval for IndexingExpression {
    fn eval(&self, ctx: &mut Context) -> Result<Instance, E> {
        fn index_inst(base: &Instance, index: usize) -> Result<Instance, E> {
            match base {
                Instance::Vec(v) => v
                    .get(index)
                    .cloned()
                    .ok_or_else(|| E::OutOfBounds(index, v.ty(), v.n() as usize)),
                Instance::Mat(m) => m
                    .col(index)
                    .cloned()
                    .ok_or_else(|| E::OutOfBounds(index, m.ty(), m.c() as usize)),
                Instance::Array(a) => a
                    .get(index)
                    .cloned()
                    .ok_or_else(|| E::OutOfBounds(index, a.ty(), a.n())),
                Instance::Ref(r) => r.view_index(index).map(Instance::Ref),
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
                UnaryOperator::AddressOf => unreachable!("handled above"),
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
                BinaryOperator::Addition => lhs.op_add(&rhs, ctx.stage),
                BinaryOperator::Subtraction => lhs.op_sub(&rhs, ctx.stage),
                BinaryOperator::Multiplication => lhs.op_mul(&rhs, ctx.stage),
                BinaryOperator::Division => lhs.op_div(&rhs, ctx.stage),
                BinaryOperator::Remainder => lhs.op_rem(&rhs, ctx.stage),
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
        let ty = self
            .ty
            .template_args
            .is_none()
            .then(|| {
                ctx.source
                    .resolve_alias(&self.ty.name)
                    .unwrap_or(self.ty.clone())
            })
            .unwrap_or(self.ty.clone());

        let args = self
            .arguments
            .iter()
            .map(|a| a.eval_value(ctx))
            .collect::<Result<Vec<_>, _>>()?;

        // struct constructor
        if let Some(decl) = ctx.source.decl_struct(&ty.name) {
            if args.len() == decl.members.len() {
                let members = decl
                    .members
                    .iter()
                    .zip(args)
                    .map(|(member, inst)| {
                        let ty = member.ty.eval_ty(ctx)?;
                        let inst = inst
                            .convert_to(&ty)
                            .ok_or_else(|| E::ParamType(ty, inst.ty()))?;
                        Ok((member.name.clone(), inst))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(StructInstance::new(ty.name.to_string(), members).into())
            } else if args.is_empty() {
                StructInstance::zero_value(&decl.name, ctx).map(Into::into)
            } else {
                Err(E::ParamCount(
                    decl.name.clone(),
                    decl.members.len(),
                    args.len(),
                ))
            }
        }
        // function call
        else if let Some(decl) = ctx.source.decl_function(&ty.name) {
            if !decl.attributes.contains(&Attribute::Const) && ctx.stage == EvalStage::Const {
                return Err(E::NotConst(decl.name.clone()));
            }

            if decl.body.attributes.contains(&ATTR_INTRINSIC) {
                return call_builtin(&ty, args, ctx);
            }

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
                .unwrap_or(Ok(Type::Void))
                .inspect_err(|_| ctx.set_err_decl_ctx(&decl.name))?;

            ctx.scope.push();
            let flow = {
                let args = args
                    .iter()
                    .zip(&decl.parameters)
                    .map(|(arg, param)| {
                        let param_ty = param.ty.eval_ty(ctx)?;
                        arg.convert_to(&param_ty)
                            .ok_or_else(|| E::ParamType(param_ty.clone(), arg.ty()))
                    })
                    .collect::<Result<Vec<_>, _>>()
                    .inspect_err(|_| ctx.set_err_decl_ctx(&decl.name))?;

                for (a, p) in zip(args, &decl.parameters) {
                    ctx.scope.add_val(p.name.clone(), a);
                }

                let flow = decl
                    .body
                    .exec(ctx)
                    .inspect_err(|_| ctx.set_err_decl_ctx(&decl.name))?;
                flow
            };
            ctx.scope.pop();

            let inst = match flow {
                Flow::Next => {
                    if ret_ty == Type::Void {
                        Ok(Instance::Void)
                    } else {
                        Err(E::ReturnType(Type::Void, ret_ty))
                    }
                }
                Flow::Break | Flow::Continue => Err(E::FlowInFunction(flow)),
                Flow::Return(inst) => inst
                    .convert_to(&ret_ty)
                    .ok_or(E::ReturnType(inst.ty(), ret_ty))
                    .inspect_err(|_| ctx.set_err_decl_ctx(&decl.name)),
            };
            inst
        }
        // not struct constructor and not function
        else {
            Err(E::UnknownFunction(ty.name.clone()))
        }
    }
}

impl Eval for TypeExpression {
    fn eval(&self, ctx: &mut Context) -> Result<Instance, E> {
        if self.template_args.is_none() {
            if let Some(r) = ctx.scope.get(&self.name) {
                Ok(r.clone().into())
            } else {
                let ty = self.name.as_str().eval_ty(ctx)?;
                Ok(ty.into())
            }
        } else {
            Ok(self.eval_ty(ctx)?.into())
        }
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
