use std::fmt::Display;

use crate::consteval::conv::Convert;

use super::{
    AccessMode, Context, Eval, EvalError, EvalStage, EvalTy, Instance, LiteralInstance, ScopeKind,
    Ty, Type,
};

use wgsl_parse::syntax::*;

type E = EvalError;

// reference: https://www.w3.org/TR/WGSL/#behaviors
#[derive(Clone, Debug, PartialEq)]
pub enum Flow {
    Next,
    Break,
    Continue,
    Return(Instance),
}

impl Display for Flow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Flow::Next => write!(f, "void"),
            Flow::Break => write!(f, "break"),
            Flow::Continue => write!(f, "continue"),
            Flow::Return(_) => write!(f, "return"),
        }
    }
}

pub trait Exec {
    fn exec(&self, ctx: &mut Context) -> Result<Flow, E>;
}

impl Exec for TranslationUnit {
    fn exec(&self, ctx: &mut Context) -> Result<Flow, E> {
        fn inner(ctx: &mut Context) -> Result<Flow, E> {
            for decl in &ctx.source.global_declarations {
                let flow = decl.exec(ctx)?;
                match flow {
                    Flow::Next => (),
                    Flow::Break | Flow::Continue | Flow::Return(_) => {
                        return Err(E::FlowInModule(flow))
                    }
                }
            }

            Ok(Flow::Next)
        }

        let kind = ctx.kind;
        ctx.kind = ScopeKind::Module;
        let res = inner(ctx);
        ctx.kind = kind;
        res
    }
}

impl Exec for GlobalDeclaration {
    fn exec(&self, ctx: &mut Context) -> Result<Flow, E> {
        match self {
            GlobalDeclaration::Declaration(decl) => decl.exec(ctx),
            GlobalDeclaration::ConstAssert(decl) => decl.exec(ctx),
            _ => Ok(Flow::Next),
        }
    }
}

impl Exec for Statement {
    fn exec(&self, ctx: &mut Context) -> Result<Flow, E> {
        match self {
            Statement::Void => Ok(Flow::Next),
            Statement::Compound(s) => s.exec(ctx),
            Statement::Assignment(s) => s.exec(ctx),
            Statement::Increment(s) => s.exec(ctx),
            Statement::Decrement(s) => s.exec(ctx),
            Statement::If(s) => s.exec(ctx),
            Statement::Switch(s) => s.exec(ctx),
            Statement::Loop(s) => s.exec(ctx),
            Statement::For(s) => s.exec(ctx),
            Statement::While(s) => s.exec(ctx),
            Statement::Break(s) => s.exec(ctx),
            Statement::Continue(s) => s.exec(ctx),
            Statement::Return(s) => s.exec(ctx),
            Statement::Discard(s) => s.exec(ctx),
            Statement::FunctionCall(s) => s.exec(ctx),
            Statement::ConstAssert(s) => s.exec(ctx),
            Statement::Declaration(s) => s.exec(ctx),
        }
    }
}

impl Exec for CompoundStatement {
    fn exec(&self, ctx: &mut Context) -> Result<Flow, E> {
        ctx.scope.push();
        for stat in &self.statements {
            let flow = stat.exec(ctx)?;
            match flow {
                Flow::Next => (),
                Flow::Break | Flow::Continue | Flow::Return(_) => {
                    ctx.scope.pop();
                    return Ok(flow);
                }
            }
        }

        ctx.scope.pop();
        Ok(Flow::Next)
    }
}

impl Exec for AssignmentStatement {
    fn exec(&self, ctx: &mut Context) -> Result<Flow, E> {
        let lhs = self.lhs.eval(ctx)?;

        if let Instance::Ref(mut r) = lhs {
            let rhs = self.rhs.eval_value(ctx)?;
            match self.operator {
                AssignmentOperator::Equal => {
                    let _ = r.write(rhs)?;
                }
                AssignmentOperator::PlusEqual => {
                    let val = r.read()?.op_add(&rhs)?;
                    let _ = r.write(val)?;
                }
                AssignmentOperator::MinusEqual => {
                    let val = r.read()?.op_sub(&rhs)?;
                    let _ = r.write(val)?;
                }
                AssignmentOperator::TimesEqual => {
                    let val = r.read()?.op_mul(&rhs)?;
                    let _ = r.write(val)?;
                }
                AssignmentOperator::DivisionEqual => {
                    let val = r.read()?.op_div(&rhs)?;
                    let _ = r.write(val)?;
                }
                AssignmentOperator::ModuloEqual => {
                    let val = r.read()?.op_rem(&rhs)?;
                    let _ = r.write(val)?;
                }
                AssignmentOperator::AndEqual => {
                    let val = r.read()?.op_bitand(&rhs)?;
                    let _ = r.write(val)?;
                }
                AssignmentOperator::OrEqual => {
                    let val = r.read()?.op_bitor(&rhs)?;
                    let _ = r.write(val)?;
                }
                AssignmentOperator::XorEqual => {
                    let val = r.read()?.op_bitxor(&rhs)?;
                    let _ = r.write(val)?;
                }
                AssignmentOperator::ShiftRightAssign => {
                    let val = r.read()?.op_shl(&rhs)?;
                    let _ = r.write(val)?;
                }
                AssignmentOperator::ShiftLeftAssign => {
                    let val = r.read()?.op_shr(&rhs)?;
                    let _ = r.write(val)?;
                }
            }
            Ok(Flow::Next)
        } else {
            Err(E::NotRef(lhs))
        }
    }
}

impl Exec for IncrementStatement {
    fn exec(&self, ctx: &mut Context) -> Result<Flow, E> {
        let expr = self.expression.eval(ctx)?;
        if let Instance::Ref(mut r) = expr {
            let mut r = r.read_write()?;
            match &*r {
                Instance::Literal(LiteralInstance::I32(n)) => {
                    let val = n.checked_add(1).ok_or(E::IncrOverflow)?;
                    let _ = r.write(LiteralInstance::I32(val).into());
                    Ok(Flow::Next)
                }
                Instance::Literal(LiteralInstance::U32(n)) => {
                    let val = n.checked_add(1).ok_or(E::IncrOverflow)?;
                    let _ = r.write(LiteralInstance::U32(val).into());
                    Ok(Flow::Next)
                }
                i => Err(E::IncrType(i.ty())),
            }
        } else {
            Err(E::NotRef(expr))
        }
    }
}

impl Exec for DecrementStatement {
    fn exec(&self, ctx: &mut Context) -> Result<Flow, E> {
        let expr = self.expression.eval(ctx)?;
        if let Instance::Ref(mut r) = expr {
            let mut r = r.read_write()?;
            match &*r {
                Instance::Literal(LiteralInstance::I32(n)) => {
                    let val = n.checked_sub(1).ok_or(E::DecrOverflow)?;
                    let _ = r.write(LiteralInstance::I32(val).into());
                    Ok(Flow::Next)
                }
                Instance::Literal(LiteralInstance::U32(n)) => {
                    let val = n.checked_sub(1).ok_or(E::DecrOverflow)?;
                    let _ = r.write(LiteralInstance::U32(val).into());
                    Ok(Flow::Next)
                }
                r => Err(E::DecrType(r.ty())),
            }
        } else {
            Err(E::NotRef(expr))
        }
    }
}

impl Exec for IfStatement {
    fn exec(&self, ctx: &mut Context) -> Result<Flow, E> {
        {
            let expr = self.if_clause.expression.eval_value(ctx)?;
            let cond = match expr {
                Instance::Literal(LiteralInstance::Bool(b)) => Ok(b),
                _ => Err(E::Type(Type::Bool, expr.ty())),
            }?;

            if cond {
                let flow = self.if_clause.body.exec(ctx)?;
                return Ok(flow);
            }
        }

        for elif in &self.else_if_clauses {
            let expr = elif.expression.eval_value(ctx)?;
            let cond = match expr {
                Instance::Literal(LiteralInstance::Bool(b)) => Ok(b),
                _ => Err(E::Type(Type::Bool, expr.ty())),
            }?;
            if cond {
                let flow = elif.body.exec(ctx)?;
                return Ok(flow);
            }
        }

        if let Some(el) = &self.else_clause {
            let flow = el.body.exec(ctx)?;
            return Ok(flow);
        }

        Ok(Flow::Next)
    }
}

impl Exec for SwitchStatement {
    fn exec(&self, ctx: &mut Context) -> Result<Flow, E> {
        let expr = self.expression.eval_value(ctx)?;

        for clause in &self.clauses {}

        // this one looks quite complicated and ambiguous
        // * should I evaluate case selectors first?
        // * concrete integer scalar type
        // * ..
        todo!()

        // Ok(Flow::Void)
    }
}

impl Exec for LoopStatement {
    fn exec(&self, ctx: &mut Context) -> Result<Flow, E> {
        loop {
            let flow = self.body.exec(ctx)?;

            match flow {
                Flow::Next | Flow::Continue => {
                    if let Some(cont) = &self.continuing {
                        let flow = cont.exec(ctx)?;

                        match flow {
                            Flow::Next => (),
                            Flow::Break => return Ok(Flow::Next), // This must be a break-if, see impl Exec for ContinuingStatement
                            Flow::Continue => unreachable!("no continue in continuing"),
                            Flow::Return(_) => unreachable!("no return in continuing"),
                        }
                    }
                }
                Flow::Break => {
                    return Ok(Flow::Next);
                }
                Flow::Return(_) => {
                    return Ok(flow);
                }
            }
        }
    }
}

impl Exec for ContinuingStatement {
    fn exec(&self, ctx: &mut Context) -> Result<Flow, E> {
        let flow = self.body.exec(ctx)?;
        match flow {
            Flow::Next => {
                if let Some(break_if) = &self.break_if {
                    let expr = break_if.expression.eval_value(ctx)?;
                    let cond = match expr {
                        Instance::Literal(LiteralInstance::Bool(b)) => Ok(b),
                        _ => Err(E::Type(Type::Bool, expr.ty())),
                    }?;
                    if cond {
                        Ok(Flow::Break)
                    } else {
                        Ok(Flow::Next)
                    }
                } else {
                    Ok(Flow::Next)
                }
            }
            Flow::Break | Flow::Continue | Flow::Return(_) => Err(E::FlowInContinuing(flow)),
        }
    }
}

impl Exec for ForStatement {
    fn exec(&self, ctx: &mut Context) -> Result<Flow, E> {
        if let Some(init) = &self.initializer {
            ctx.scope.push();
            let flow = init.exec(ctx)?;
            if flow != Flow::Next {
                ctx.scope.pop();
                return Ok(flow);
            }
        }

        loop {
            let cond = self
                .condition
                .as_ref()
                .map(|expr| {
                    let expr = expr.eval_value(ctx)?;
                    match expr {
                        Instance::Literal(LiteralInstance::Bool(b)) => Ok(b),
                        _ => Err(E::Type(Type::Bool, expr.ty())),
                    }
                })
                .unwrap_or(Ok(false))?;

            if !cond {
                break;
            }

            let flow = self.body.exec(ctx)?;

            match flow {
                Flow::Next | Flow::Continue => {
                    if let Some(updt) = &self.update {
                        updt.exec(ctx)?;
                    }
                }
                Flow::Break => {
                    if let Some(_) = &self.initializer {
                        ctx.scope.pop();
                    }
                    return Ok(Flow::Next);
                }
                Flow::Return(_) => {
                    if let Some(_) = &self.initializer {
                        ctx.scope.pop();
                    }
                    return Ok(flow);
                }
            }
        }

        if let Some(_) = &self.initializer {
            ctx.scope.pop();
        }

        Ok(Flow::Next)
    }
}

impl Exec for WhileStatement {
    fn exec(&self, ctx: &mut Context) -> Result<Flow, E> {
        loop {
            let expr = self.condition.eval_value(ctx)?;
            let cond = match expr {
                Instance::Literal(LiteralInstance::Bool(b)) => Ok(b),
                _ => Err(E::Type(Type::Bool, expr.ty())),
            }?;

            if cond {
                let flow = self.body.exec(ctx)?;
                match flow {
                    Flow::Next | Flow::Continue => (),
                    Flow::Break => return Ok(Flow::Next),
                    Flow::Return(_) => return Ok(flow),
                }
            } else {
                return Ok(Flow::Next);
            }
        }
    }
}

impl Exec for BreakStatement {
    fn exec(&self, _ctx: &mut Context) -> Result<Flow, E> {
        Ok(Flow::Break)
    }
}

impl Exec for ContinueStatement {
    fn exec(&self, _ctx: &mut Context) -> Result<Flow, E> {
        Ok(Flow::Continue)
    }
}

impl Exec for ReturnStatement {
    fn exec(&self, ctx: &mut Context) -> Result<Flow, E> {
        let inst = if let Some(e) = &self.expression {
            e.eval_value(ctx)?
        } else {
            Instance::Void
        };
        Ok(Flow::Return(inst))
    }
}

impl Exec for DiscardStatement {
    fn exec(&self, _ctx: &mut Context) -> Result<Flow, E> {
        Err(E::DiscardInConst)
    }
}

impl Exec for FunctionCallStatement {
    fn exec(&self, ctx: &mut Context) -> Result<Flow, E> {
        let _ = self.call.eval(ctx)?;
        Ok(Flow::Next)
    }
}

impl Exec for ConstAssertStatement {
    fn exec(&self, ctx: &mut Context) -> Result<Flow, E> {
        let expr = self.expression.eval_value(ctx)?;
        let cond = match expr {
            Instance::Literal(LiteralInstance::Bool(b)) => Ok(b),
            _ => Err(E::Type(Type::Bool, expr.ty())),
        }?;

        if cond {
            Ok(Flow::Next)
        } else {
            Err(E::ConstAssertFailure(self.expression.clone()))
        }
    }
}

// TODO: implement address space
// TODO: implement access mode / const-ness
// TODO: implement evaluating module scope
impl Exec for Declaration {
    fn exec(&self, ctx: &mut Context) -> Result<Flow, E> {
        if ctx.scope.has(&self.name) {
            return Err(E::DuplicateDecl(self.name.clone()));
        }

        match (self.kind, ctx.kind) {
            (DeclarationKind::Const, _) => {
                let mut inst = self
                    .initializer
                    .as_ref()
                    .ok_or_else(|| E::UninitConst(self.name.clone()))?
                    .eval(ctx)?;

                if let Some(ty) = &self.ty {
                    let ty = ty.eval_ty(ctx)?;
                    inst = inst
                        .convert_to(&ty)
                        .ok_or_else(|| E::ConversionFailure(inst.ty(), ty))?;
                }

                ctx.scope.add(self.name.clone(), inst, AccessMode::Read);
                Ok(Flow::Next)
            }
            (DeclarationKind::Override, ScopeKind::Function) => Err(E::OverrideInFn),
            (DeclarationKind::Let, ScopeKind::Function) => {
                let inst = self
                    .initializer
                    .as_ref()
                    .ok_or_else(|| E::UninitLet(self.name.clone()))?
                    .eval(ctx)?;

                let inst = if let Some(ty) = &self.ty {
                    let ty = ty.eval_ty(ctx)?;
                    inst.convert_to(&ty)
                        .ok_or_else(|| E::ConversionFailure(inst.ty(), ty))?
                } else {
                    inst.concretize()
                        .ok_or_else(|| E::ConversionFailure(inst.ty(), inst.ty().concretize()))?
                };

                ctx.scope.add(self.name.clone(), inst, AccessMode::Read);
                Ok(Flow::Next)
            }
            (DeclarationKind::Var, ScopeKind::Function) => {
                let inst = match (&self.ty, &self.initializer) {
                    (None, None) => Err(E::UntypedDecl),
                    (None, Some(init)) => {
                        let inst = init.eval(ctx)?;
                        inst.concretize()
                            .ok_or_else(|| E::ConversionFailure(inst.ty(), inst.ty().concretize()))
                    }
                    (Some(ty), None) => {
                        let ty = ty.eval_ty(ctx)?;
                        Instance::zero_value(&ty, ctx)
                    }
                    (Some(ty), Some(init)) => {
                        let inst = init.eval(ctx)?;
                        let ty = ty.eval_ty(ctx)?;
                        inst.convert_to(&ty)
                            .ok_or_else(|| E::ConversionFailure(inst.ty(), ty))
                    }
                }?;

                ctx.scope
                    .add(self.name.clone(), inst, AccessMode::ReadWrite);
                Ok(Flow::Next)
            }
            (DeclarationKind::Override, ScopeKind::Module) => {
                if ctx.stage == EvalStage::Const {
                    // in const contexts we just ignore var declarations since they cannot be
                    // used in const eval contexts. But we could at least store the ident to
                    // provide a better err msg if one is used.
                    Ok(Flow::Next)
                } else {
                    todo!("evaluate module scope")
                }
            }
            (DeclarationKind::Let, ScopeKind::Module) => Err(E::LetInMod),
            (DeclarationKind::Var, ScopeKind::Module) => {
                if ctx.stage == EvalStage::Const {
                    // in const contexts we just ignore var declarations since they cannot be
                    // used in const eval contexts. But we could at least store the ident to
                    // provide a better err msg if one is used.
                    Ok(Flow::Next)
                } else {
                    todo!("evaluate module scope")
                }
            }
        }
    }
}
