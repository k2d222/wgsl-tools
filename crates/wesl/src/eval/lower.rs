use std::iter::zip;

use crate::{
    eval::{Context, Eval, EvalError, Exec, Ty, Type},
    visit::Visit,
};
use wesl_macros::query_mut;
use wgsl_parse::{span::Spanned, syntax::*};

use super::{to_expr::ToExpr, EvalTy, SyntaxUtil, EXPR_FALSE, EXPR_TRUE};

type E = EvalError;

// TODO: I am aware that it is not correct to make all implicit conversions explicit.
// I should fix that at some point, but meanwhile it fixes Naga not supporting automatic conversions.
pub fn make_explicit_conversions(wesl: &mut TranslationUnit, ctx: &mut Context) -> Result<(), E> {
    fn explicit_call(call: &mut FunctionCall, ctx: &mut Context) -> Result<(), E> {
        let decl = ctx.source.decl_function(&*call.ty.ident.name());
        if let Some(decl) = decl {
            for (arg, param) in zip(&mut call.arguments, &decl.parameters) {
                let ty = param.ty.eval_ty(ctx)?;
                if ty.inner_ty().is_scalar() {
                    let ty = ty.to_expr(ctx)?.unwrap_type_or_identifier();
                    *arg.node_mut() = Expression::FunctionCall(FunctionCall {
                        ty,
                        arguments: vec![arg.clone()],
                    })
                }
            }
        }
        Ok(())
    }
    fn explicit_expr(expr: &mut Expression, ctx: &mut Context) -> Result<(), E> {
        if let Expression::FunctionCall(call) = expr {
            explicit_call(call, ctx)?;
        }
        for expr in Visit::<ExpressionNode>::visit_mut(expr) {
            explicit_expr(expr, ctx)?;
        }
        Ok(())
    }
    for expr in Visit::<ExpressionNode>::visit_mut(wesl) {
        explicit_expr(expr, ctx)?;
    }

    fn explicit_stat(stat: &mut Statement, ret: &Type, ctx: &mut Context) -> Result<(), E> {
        if let Statement::Return(stat) = stat {
            if let Some(expr) = &mut stat.expression {
                let ty = ret.to_expr(ctx)?.unwrap_type_or_identifier();
                *expr.node_mut() = Expression::FunctionCall(FunctionCall {
                    ty,
                    arguments: vec![expr.clone()],
                })
            }
        } else if let Statement::FunctionCall(stat) = stat {
            explicit_call(&mut stat.call, ctx)?;
        }
        for stat in Visit::<StatementNode>::visit_mut(stat) {
            explicit_stat(stat, ret, ctx)?;
        }
        Ok(())
    }
    for decl in query_mut!(wesl.global_declarations.[].GlobalDeclaration::Function) {
        if let Some(ret) = &decl.return_type {
            let ty = ret.eval_ty(ctx)?;
            if ty.inner_ty().is_scalar() {
                for stat in &mut decl.body.statements {
                    explicit_stat(stat, &ty, ctx)?;
                }
            }
        }
    }
    Ok(())
}

pub trait Lower {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E>;
}

impl<T: Lower> Lower for Option<T> {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        if let Some(x) = self {
            x.lower(ctx)?;
        }
        Ok(())
    }
}

impl<T: Lower> Lower for Spanned<T> {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        self.node_mut()
            .lower(ctx)
            .inspect_err(|_| ctx.set_err_expr_ctx(self.span()))?;
        Ok(())
    }
}

impl Lower for Expression {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        match self.eval_value(ctx) {
            Ok(inst) => *self = inst.to_expr(ctx)?,
            Err(_) => match self {
                Expression::Literal(_) => (),
                Expression::Parenthesized(expr) => expr.expression.lower(ctx)?,
                Expression::NamedComponent(expr) => expr.base.lower(ctx)?,
                Expression::Indexing(expr) => {
                    expr.base.lower(ctx)?;
                    expr.index.lower(ctx)?;
                }
                Expression::Unary(expr) => expr.operand.lower(ctx)?,
                Expression::Binary(expr) => {
                    expr.left.lower(ctx)?;
                    expr.right.lower(ctx)?;
                }
                Expression::FunctionCall(expr) => expr.lower(ctx)?,
                Expression::TypeOrIdentifier(_) => {
                    if let Ok(expr) = self.eval_value(ctx).and_then(|inst| inst.to_expr(ctx)) {
                        *self = expr;
                    }
                }
            },
        }
        Ok(())
    }
}

impl Lower for FunctionCall {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        self.ty = ctx.source.resolve_ty(&self.ty).clone();
        for arg in &mut self.arguments {
            arg.lower(ctx)?;
        }
        Ok(())
    }
}

impl Lower for TemplateArgs {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        if let Some(tplts) = self {
            for tplt in tplts {
                tplt.expression.lower(ctx)?;
            }
        }
        Ok(())
    }
}

impl Lower for TypeExpression {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        // types must be const-expressions
        let expr = self.eval_ty(ctx)?.to_expr(ctx)?;
        *self = match expr {
            Expression::TypeOrIdentifier(ty) => ty,
            _ => unreachable!("eval_ty must return Literal"),
        };
        Ok(())
    }
}

impl Lower for Attributes {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        for attr in self {
            match attr {
                Attribute::Align(expr)
                | Attribute::Binding(expr)
                | Attribute::BlendSrc(expr)
                | Attribute::Group(expr)
                | Attribute::Id(expr)
                | Attribute::Location(expr)
                | Attribute::Size(expr) => {
                    expr.lower(ctx)?;
                }
                #[cfg(feature = "condcomp")]
                Attribute::If(expr) => {
                    expr.lower(ctx)?;
                }
                Attribute::WorkgroupSize(attr) => {
                    attr.x.lower(ctx)?;
                    attr.y.lower(ctx)?;
                    attr.z.lower(ctx)?;
                }
                Attribute::Custom(_) => {
                    // we ignore unknown attributes for now. We don't know how they are implemented.
                }
                _ => (),
            }
        }
        Ok(())
    }
}

impl Lower for Declaration {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        self.attributes.lower(ctx)?;
        self.ty.lower(ctx)?;
        self.initializer.lower(ctx)?;
        Ok(())
    }
}

impl Lower for TypeAlias {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        self.ty.lower(ctx)?;
        Ok(())
    }
}

impl Lower for Struct {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        for m in &mut self.members {
            m.attributes.lower(ctx)?;
            m.ty.lower(ctx)?;
        }
        Ok(())
    }
}

impl Lower for Function {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        if self.attributes.contains(&Attribute::Const) && self.return_type.is_none() {
            self.body.statements.clear();
        }
        self.attributes.lower(ctx)?;
        for p in &mut self.parameters {
            p.attributes.lower(ctx)?;
            p.ty.lower(ctx)?;
        }
        self.return_attributes.lower(ctx)?;
        self.return_type.lower(ctx)?;
        self.body.lower(ctx)?;
        Ok(())
    }
}

impl Lower for ConstAssert {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        self.exec(ctx).map(|_| ())
    }
}

impl Lower for Statement {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        match self {
            Statement::Void => (),
            Statement::Compound(stat) => {
                stat.lower(ctx)?;
                if stat.statements.is_empty() {
                    *self = Statement::Void;
                } else if let [stat] = stat.statements.as_slice() {
                    *self = stat.node().clone();
                }
            }
            Statement::Assignment(stat) => stat.lower(ctx)?,
            Statement::Increment(stat) => stat.lower(ctx)?,
            Statement::Decrement(stat) => stat.lower(ctx)?,
            Statement::If(stat) => {
                stat.lower(ctx)?;

                // remove clauses evaluating to false
                stat.else_if_clauses
                    .retain(|clause| *clause.expression != EXPR_FALSE);

                // remove subsequent clauses after a true
                if let Some(i) = stat
                    .else_if_clauses
                    .iter()
                    .position(|clause| *clause.expression == EXPR_TRUE)
                {
                    stat.else_if_clauses.resize_with(i + 1, || unreachable!());
                    stat.else_clause = None;
                }

                macro_rules! assign_clause {
                    ($stat:ident, $body:expr) => {
                        if $body.statements.is_empty() {
                            *$stat = Statement::Void;
                        } else if let [s1] = $body.statements.as_slice() {
                            *$stat = s1.node().clone();
                        } else {
                            *$stat = Statement::Compound($body.clone())
                        }
                    };
                }

                // remove the whole statement if the first clause is true
                if *stat.if_clause.expression == EXPR_TRUE {
                    assign_clause!(self, stat.if_clause.body);
                } else if *stat.if_clause.expression == EXPR_FALSE {
                    if let Some(clause) = stat.else_if_clauses.first() {
                        if *clause.expression == EXPR_TRUE {
                            assign_clause!(self, clause.body);
                        }
                    } else if let Some(clause) = &stat.else_clause {
                        assign_clause!(self, clause.body);
                    }
                }
            }
            Statement::Switch(stat) => stat.lower(ctx)?,
            Statement::Loop(stat) => stat.lower(ctx)?,
            Statement::For(stat) => {
                stat.lower(ctx)?;
                if stat
                    .condition
                    .as_ref()
                    .is_some_and(|cond| **cond == EXPR_FALSE)
                {
                    *self = Statement::Void;
                }
            }
            Statement::While(stat) => {
                stat.lower(ctx)?;
                if *stat.condition == EXPR_FALSE {
                    *self = Statement::Void;
                }
            }
            Statement::Break(_) => (),
            Statement::Continue(_) => (),
            Statement::Return(stat) => stat.lower(ctx)?,
            Statement::Discard(_) => (),
            Statement::FunctionCall(stat) => {
                let decl = ctx.source.decl_function(&*stat.call.ty.ident.name());
                if let Some(decl) = decl {
                    if decl.attributes.contains(&Attribute::Const) {
                        *self = Statement::Void; // a void const function does nothing
                    } else {
                        stat.lower(ctx)?
                    }
                } else {
                    stat.lower(ctx)?
                }
            }
            Statement::ConstAssert(stat) => stat.exec(ctx).map(|_| ())?,
            Statement::Declaration(stat) => {
                if stat.kind == DeclarationKind::Const {
                    // eval and add it to the scope
                    stat.exec(ctx)?;
                    *self = Statement::Void;
                } else {
                    stat.lower(ctx)?;
                }
            }
        }
        Ok(())
    }
}

impl Lower for CompoundStatement {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        self.attributes.lower(ctx)?;
        for stat in &mut self.statements {
            stat.lower(ctx)?;
        }
        self.statements.retain(|stat| match stat.node() {
            Statement::Void => false,
            Statement::Compound(_) => true,
            Statement::Assignment(_) => true,
            Statement::Increment(_) => true,
            Statement::Decrement(_) => true,
            Statement::If(_) => true,
            Statement::Switch(_) => true,
            Statement::Loop(_) => true,
            Statement::For(_) => true,
            Statement::While(_) => true,
            Statement::Break(_) => true,
            Statement::Continue(_) => true,
            Statement::Return(_) => true,
            Statement::Discard(_) => true,
            Statement::FunctionCall(_) => true,
            Statement::ConstAssert(_) => false,
            Statement::Declaration(_) => true,
        });
        Ok(())
    }
}

impl Lower for AssignmentStatement {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        self.lhs.lower(ctx)?;
        self.rhs.lower(ctx)?;
        Ok(())
    }
}

impl Lower for IncrementStatement {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        self.expression.lower(ctx)?;
        Ok(())
    }
}

impl Lower for DecrementStatement {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        self.expression.lower(ctx)?;
        Ok(())
    }
}

impl Lower for IfStatement {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        self.attributes.lower(ctx)?;
        self.if_clause.expression.lower(ctx)?;
        self.if_clause.body.lower(ctx)?;
        for clause in &mut self.else_if_clauses {
            clause.expression.lower(ctx)?;
            clause.body.lower(ctx)?;
        }
        if let Some(clause) = &mut self.else_clause {
            clause.body.lower(ctx)?;
        }
        Ok(())
    }
}

impl Lower for SwitchStatement {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        self.attributes.lower(ctx)?;
        self.expression.lower(ctx)?;
        self.body_attributes.lower(ctx)?;
        for clause in &mut self.clauses {
            for sel in &mut clause.case_selectors {
                if let CaseSelector::Expression(expr) = sel {
                    expr.lower(ctx)?;
                }
            }
            clause.body.lower(ctx)?;
        }
        Ok(())
    }
}

impl Lower for LoopStatement {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        self.body.lower(ctx)?;
        if let Some(cont) = &mut self.continuing {
            cont.body.lower(ctx)?;
            if let Some(break_if) = &mut cont.break_if {
                break_if.expression.lower(ctx)?;
            }
        }
        Ok(())
    }
}

impl Lower for ForStatement {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        self.initializer.lower(ctx)?;
        self.condition.lower(ctx)?;
        self.body.lower(ctx)?;
        Ok(())
    }
}

impl Lower for WhileStatement {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        self.condition.lower(ctx)?;
        self.body.lower(ctx)?;
        Ok(())
    }
}

impl Lower for ReturnStatement {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        self.expression.lower(ctx)?;
        Ok(())
    }
}

impl Lower for FunctionCallStatement {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        self.call.lower(ctx)
    }
}

impl Lower for TranslationUnit {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        self.exec(ctx)?; // add const-decls to the scope and eval const_asserts
        for decl in &mut self.global_declarations {
            match decl {
                GlobalDeclaration::Void => Ok(()),
                GlobalDeclaration::Declaration(decl) => decl.lower(ctx),
                GlobalDeclaration::TypeAlias(decl) => decl.lower(ctx),
                GlobalDeclaration::Struct(decl) => decl.lower(ctx),
                GlobalDeclaration::Function(decl) => decl.lower(ctx),
                GlobalDeclaration::ConstAssert(decl) => decl.lower(ctx),
            }
            .inspect_err(|_| {
                decl.ident()
                    .inspect(|&ident| ctx.set_err_decl_ctx(ident.to_string()));
            })?;
        }
        self.global_declarations.retain(|decl| match decl {
            GlobalDeclaration::Void => false,
            GlobalDeclaration::Declaration(decl) => decl.kind != DeclarationKind::Const,
            GlobalDeclaration::TypeAlias(_) => false,
            GlobalDeclaration::Struct(_) => true,
            GlobalDeclaration::Function(_) => true,
            GlobalDeclaration::ConstAssert(_) => false,
        });
        Ok(())
    }
}
