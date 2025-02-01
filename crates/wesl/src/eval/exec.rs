use std::fmt::Display;

use crate::eval::conv::Convert;

use super::{
    attrs::EvalAttrs, AccessMode, Context, Eval, EvalError, EvalStage, EvalTy, Instance,
    LiteralInstance, RefInstance, ScopeKind, Ty, Type,
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

/// careful: do not return in with_stage body!
macro_rules! with_stage {
    ($ctx:expr, $stage:expr, $body:tt) => {{
        let stage = $ctx.stage;
        $ctx.stage = $stage;
        let body = $body;
        $ctx.stage = stage;
        body
    }};
}

macro_rules! with_scope {
    ($ctx:expr, $body:tt) => {{
        $ctx.scope.push();
        let body = (|| $body)();
        $ctx.scope.pop();
        body
    }};
}
pub(super) use with_scope;
pub(super) use with_stage;

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

impl Exec for StatementNode {
    fn exec(&self, ctx: &mut Context) -> Result<Flow, E> {
        self.node().exec(ctx).inspect_err(|_| {
            if ctx.err_expr.is_none() {
                ctx.err_expr = Some(self.span().clone());
            }
        })
    }
}

impl Exec for TranslationUnit {
    fn exec(&self, ctx: &mut Context) -> Result<Flow, E> {
        fn inner(ctx: &mut Context) -> Result<Flow, E> {
            for decl in &ctx.source.global_declarations {
                let flow = decl.exec(ctx).inspect_err(|_| {
                    decl.ident()
                        .inspect(|&ident| ctx.set_err_decl_ctx(ident.to_string()));
                })?;
                match flow {
                    Flow::Next => (),
                    Flow::Break | Flow::Continue | Flow::Return(_) => {
                        decl.ident()
                            .inspect(|&ident| ctx.set_err_decl_ctx(ident.to_string()));
                        return Err(E::FlowInModule(flow));
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
        with_scope!(ctx, {
            for stat in &self.statements {
                let flow = stat.exec(ctx)?;
                match flow {
                    Flow::Next => (),
                    Flow::Break | Flow::Continue | Flow::Return(_) => {
                        return Ok(flow);
                    }
                }
            }

            Ok(Flow::Next)
        })
    }
}

// because some places in the grammar requires that no scope is created when executing the
// CompoundStatement, such as for loops with initializer or function invocations.
pub(crate) fn compound_exec_no_scope(
    stat: &CompoundStatement,
    ctx: &mut Context,
) -> Result<Flow, E> {
    for stat in &stat.statements {
        let flow = stat.exec(ctx)?;
        match flow {
            Flow::Next => (),
            Flow::Break | Flow::Continue | Flow::Return(_) => {
                return Ok(flow);
            }
        }
    }
    Ok(Flow::Next)
}

impl Exec for AssignmentStatement {
    fn exec(&self, ctx: &mut Context) -> Result<Flow, E> {
        let lhs = self.lhs.eval(ctx)?;
        let ty = lhs.ty().concretize();

        if let Instance::Ref(mut r) = lhs {
            let rhs = self.rhs.eval_value(ctx)?;
            match self.operator {
                AssignmentOperator::Equal => {
                    let rhs = rhs
                        .convert_to(&ty)
                        .ok_or_else(|| E::AssignType(rhs.ty(), ty))?;
                    r.write(rhs)?;
                }
                AssignmentOperator::PlusEqual => {
                    let val = r.read()?.op_add(&rhs, ctx.stage)?;
                    r.write(val)?;
                }
                AssignmentOperator::MinusEqual => {
                    let val = r.read()?.op_sub(&rhs, ctx.stage)?;
                    r.write(val)?;
                }
                AssignmentOperator::TimesEqual => {
                    let val = r.read()?.op_mul(&rhs, ctx.stage)?;
                    r.write(val)?;
                }
                AssignmentOperator::DivisionEqual => {
                    let val = r.read()?.op_div(&rhs, ctx.stage)?;
                    r.write(val)?;
                }
                AssignmentOperator::ModuloEqual => {
                    let val = r.read()?.op_rem(&rhs, ctx.stage)?;
                    r.write(val)?;
                }
                AssignmentOperator::AndEqual => {
                    let val = r.read()?.op_bitand(&rhs)?;
                    r.write(val)?;
                }
                AssignmentOperator::OrEqual => {
                    let val = r.read()?.op_bitor(&rhs)?;
                    r.write(val)?;
                }
                AssignmentOperator::XorEqual => {
                    let val = r.read()?.op_bitxor(&rhs)?;
                    r.write(val)?;
                }
                AssignmentOperator::ShiftRightAssign => {
                    let val = r.read()?.op_shl(&rhs)?;
                    r.write(val)?;
                }
                AssignmentOperator::ShiftLeftAssign => {
                    let val = r.read()?.op_shr(&rhs)?;
                    r.write(val)?;
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
        let ty = expr.ty();

        for clause in &self.clauses {
            for selector in &clause.case_selectors {
                match selector {
                    CaseSelector::Default => return clause.body.exec(ctx),
                    CaseSelector::Expression(e) => {
                        let e = with_stage!(ctx, EvalStage::Const, { e.eval_value(ctx) })?;
                        let e = e
                            .convert_to(&ty)
                            .ok_or_else(|| E::Conversion(e.ty(), ty.clone()))?;
                        if e == expr {
                            return clause.body.exec(ctx);
                        }
                    }
                }
            }
        }

        Ok(Flow::Next)
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
        // the initializer is in the same scope as the body.
        // https://github.com/gpuweb/gpuweb/issues/5024
        with_scope!(ctx, {
            if let Some(init) = &self.initializer {
                let flow = init.exec(ctx)?;
                if flow != Flow::Next {
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

                // the body has to run in the same scope as the initializer.
                let flow = compound_exec_no_scope(&self.body, ctx)?;

                match flow {
                    Flow::Next | Flow::Continue => {
                        if let Some(updt) = &self.update {
                            updt.exec(ctx)?;
                        }
                    }
                    Flow::Break => {
                        break;
                    }
                    Flow::Return(_) => {
                        return Ok(flow);
                    }
                }
            }

            Ok(Flow::Next)
        })
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
        with_stage!(ctx, EvalStage::Const, {
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
        })
    }
}

// TODO: implement address space
impl Exec for Declaration {
    fn exec(&self, ctx: &mut Context) -> Result<Flow, E> {
        if ctx.scope.contains_current(&*self.ident.name()) {
            return Err(E::DuplicateDecl(self.ident.to_string()));
        }

        match (self.kind, ctx.kind) {
            (DeclarationKind::Const, _scope) => {
                let mut inst = self
                    .initializer
                    .as_ref()
                    .ok_or_else(|| E::UninitConst(self.ident.to_string()))?
                    .eval(ctx)?;

                if let Some(ty) = &self.ty {
                    let ty = ty.eval_ty(ctx)?;
                    inst = inst
                        .convert_to(&ty)
                        .ok_or_else(|| E::Conversion(inst.ty(), ty))?;
                }

                ctx.scope.add_val(self.ident.to_string(), inst);
                Ok(Flow::Next)
            }
            (DeclarationKind::Override, ScopeKind::Function) => Err(E::OverrideInFn),
            (DeclarationKind::Let, ScopeKind::Function) => {
                let inst = self
                    .initializer
                    .as_ref()
                    .ok_or_else(|| E::UninitLet(self.ident.to_string()))?
                    .eval(ctx)?;

                let inst = if let Some(ty) = &self.ty {
                    let ty = ty.eval_ty(ctx)?;
                    inst.convert_to(&ty)
                        .ok_or_else(|| E::Conversion(inst.ty(), ty))?
                } else {
                    inst.concretize()
                        .ok_or_else(|| E::Conversion(inst.ty(), inst.ty().concretize()))?
                };

                ctx.scope.add_val(self.ident.to_string(), inst);
                Ok(Flow::Next)
            }
            (DeclarationKind::Var(space), ScopeKind::Function) => {
                match space {
                    Some(AddressSpace::Function) | None => (),
                    _ => return Err(E::ForbiddenDecl(self.kind, ctx.kind)),
                }
                let inst = match (&self.ty, &self.initializer) {
                    (None, None) => Err(E::UntypedDecl),
                    (None, Some(init)) => {
                        let inst = init.eval(ctx)?;
                        inst.concretize()
                            .ok_or_else(|| E::Conversion(inst.ty(), inst.ty().concretize()))
                    }
                    (Some(ty), None) => {
                        let ty = ty.eval_ty(ctx)?;
                        Instance::zero_value(&ty, ctx)
                    }
                    (Some(ty), Some(init)) => {
                        let inst = init.eval(ctx)?;
                        let ty = ty.eval_ty(ctx)?;
                        inst.convert_to(&ty)
                            .ok_or_else(|| E::Conversion(inst.ty(), ty))
                    }
                }?;

                ctx.scope.add_var(
                    self.ident.to_string(),
                    RefInstance::from_instance(inst, AddressSpace::Function, AccessMode::ReadWrite),
                );
                Ok(Flow::Next)
            }
            (DeclarationKind::Override, ScopeKind::Module) => {
                if ctx.stage == EvalStage::Const {
                    // in const contexts we just ignore var declarations since they cannot be
                    // used in const eval contexts. But we could at least store the ident to
                    // provide a better err msg if one is used.
                    Ok(Flow::Next)
                } else {
                    let inst = ctx
                        .overridable(&self.ident.name())
                        .cloned()
                        .ok_or_else(|| E::UninitOverride(self.ident.to_string()))
                        .or_else(|e| self.initializer.as_ref().ok_or(e)?.eval(ctx))?;

                    let inst = if let Some(ty) = &self.ty {
                        let ty = ty.eval_ty(ctx)?;
                        inst.convert_to(&ty)
                            .ok_or_else(|| E::Conversion(inst.ty(), ty))?
                    } else {
                        inst.concretize()
                            .ok_or_else(|| E::Conversion(inst.ty(), inst.ty().concretize()))?
                    };

                    ctx.scope.add_val(self.ident.to_string(), inst);
                    Ok(Flow::Next)
                }
            }
            (DeclarationKind::Let, ScopeKind::Module) => Err(E::LetInMod),
            (DeclarationKind::Var(addr_space), ScopeKind::Module) => {
                // TODO: implement  address space
                if ctx.stage == EvalStage::Const {
                    // in const contexts we just ignore var declarations since they cannot be
                    // used in const eval contexts. But we could at least store the ident to
                    // provide a better err msg if one is used.
                    Ok(Flow::Next)
                } else {
                    let addr_space = addr_space.unwrap_or(AddressSpace::Handle);

                    match addr_space {
                        AddressSpace::Function => {
                            return Err(E::ForbiddenDecl(self.kind, ctx.kind))
                        }
                        AddressSpace::Private => {
                            let inst = match (&self.ty, &self.initializer) {
                                (None, None) => Err(E::UntypedDecl),
                                (None, Some(init)) => {
                                    let inst =
                                        with_stage!(ctx, EvalStage::Const, { init.eval(ctx) })?;
                                    inst.concretize().ok_or_else(|| {
                                        E::Conversion(inst.ty(), inst.ty().concretize())
                                    })
                                }
                                (Some(ty), None) => {
                                    let ty = ty.eval_ty(ctx)?;
                                    Instance::zero_value(&ty, ctx)
                                }
                                (Some(ty), Some(init)) => {
                                    let inst =
                                        with_stage!(ctx, EvalStage::Const, { init.eval(ctx) })?;
                                    let ty = ty.eval_ty(ctx)?;
                                    inst.convert_to(&ty)
                                        .ok_or_else(|| E::Conversion(inst.ty(), ty))
                                }
                            }?;

                            ctx.scope.add_var(
                                self.ident.to_string(),
                                RefInstance::from_instance(
                                    inst,
                                    AddressSpace::Private,
                                    AccessMode::ReadWrite,
                                ),
                            );
                        }
                        AddressSpace::Workgroup => {
                            // let Some(ty) = &self.ty else {
                            //     return Err(E::UntypedDecl);
                            // };
                            todo!("workgroup address space")
                        }
                        AddressSpace::Uniform => {
                            let Some(ty) = &self.ty else {
                                return Err(E::UntypedDecl);
                            };
                            let ty = ty.eval_ty(ctx)?;
                            let (group, binding) = self.eval_group_binding(ctx)?;
                            let inst = ctx
                                .resource(group, binding)
                                .ok_or_else(|| E::MissingResource(group, binding))?;
                            if ty != inst.ty() {
                                return Err(E::Type(ty, inst.ty()));
                            }
                            if !inst.space.is_uniform() {
                                return Err(E::AddressSpace(addr_space, inst.space));
                            }
                            if inst.access != AccessMode::Read {
                                return Err(E::AccessMode(AccessMode::Read, inst.access));
                            }
                            ctx.scope.add_var(self.ident.to_string(), inst.clone())
                        }
                        AddressSpace::Storage(access_mode) => {
                            let Some(ty) = &self.ty else {
                                return Err(E::UntypedDecl);
                            };
                            let ty = ty.eval_ty(ctx)?;
                            let (group, binding) = self.eval_group_binding(ctx)?;
                            let inst = ctx
                                .resource(group, binding)
                                .ok_or_else(|| E::MissingResource(group, binding))?;
                            if ty != inst.ty() {
                                return Err(E::Type(ty, inst.ty()));
                            }
                            if !inst.space.is_storage() {
                                return Err(E::AddressSpace(addr_space, inst.space));
                            }
                            let access_mode = access_mode.unwrap_or(AccessMode::Read);
                            if inst.access != access_mode {
                                return Err(E::AccessMode(access_mode, inst.access));
                            }
                            ctx.scope.add_var(self.ident.to_string(), inst.clone())
                        }
                        AddressSpace::Handle => todo!("handle address space"),
                    }
                    Ok(Flow::Next)
                }
            }
        }
    }
}
