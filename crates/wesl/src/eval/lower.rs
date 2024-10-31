use crate::{with_stage, Context, Eval, EvalError, Exec};
use wgsl_parse::{span::Spanned, syntax::*};

use super::{to_expr::ToExpr, EvalStage, EXPR_TRUE};

type E = EvalError;

pub trait Lower {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E>;
}

impl Lower for Spanned<Expression> {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        self.node_mut().lower(ctx)?;
        Ok(())
    }
}

impl Lower for Expression {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        match self.eval_value(ctx) {
            Ok(inst) => *self = inst.to_expr(ctx)?,
            Err(_) => match self {
                Expression::Literal(_) => *self = self.eval_value(ctx)?.to_expr(ctx)?,
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
                Expression::TypeOrIdentifier(_) => *self = self.eval_value(ctx)?.to_expr(ctx)?,
            },
        }
        Ok(())
    }
}

impl Lower for FunctionCall {
    fn lower(&mut self, _ctx: &mut Context) -> Result<(), E> {
        // todo!()
        Ok(())
    }
}

impl Lower for TemplateArgs {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        with_stage!(ctx, EvalStage::Const, {
            if let Some(tplts) = self {
                for tplt in tplts {
                    tplt.expression.lower(ctx)?;
                }
            }
            Ok(())
        })
    }
}

impl Lower for TypeExpression {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        self.template_args.lower(ctx)?;
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
                    if let Some(y) = &mut attr.y {
                        y.lower(ctx)?
                    }
                    if let Some(z) = &mut attr.z {
                        z.lower(ctx)?
                    }
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
        if let Some(ty) = &mut self.ty {
            ty.lower(ctx)?;
        }
        if let Some(init) = &mut self.initializer {
            init.lower(ctx)?;
        }
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
        if self.attributes.contains(&Attribute::Const) {
            return Ok(());
        }
        self.attributes.lower(ctx)?;
        for p in &mut self.parameters {
            p.attributes.lower(ctx)?;
            p.ty.lower(ctx)?;
        }
        self.return_attributes.lower(ctx)?;
        if let Some(ret) = &mut self.return_type {
            ret.lower(ctx)?;
        }
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
        self.exec(ctx)?;
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
                if stat.if_clause.expression.node() == &EXPR_TRUE {
                    if stat.if_clause.body.statements.is_empty() {
                        *self = Statement::Void;
                    } else if let [stat] = stat.if_clause.body.statements.as_slice() {
                        *self = stat.node().clone();
                    } else {
                        *self = Statement::Compound(stat.if_clause.body.clone())
                    }
                }
            }
            Statement::Switch(stat) => stat.lower(ctx)?,
            Statement::Loop(stat) => stat.lower(ctx)?,
            Statement::For(stat) => stat.lower(ctx)?,
            Statement::While(stat) => stat.lower(ctx)?,
            Statement::Break(stat) => stat.lower(ctx)?,
            Statement::Continue(stat) => stat.lower(ctx)?,
            Statement::Return(stat) => stat.lower(ctx)?,
            Statement::Discard(stat) => stat.lower(ctx)?,
            Statement::FunctionCall(stat) => stat.lower(ctx)?,
            Statement::ConstAssert(stat) => stat.exec(ctx).map(|_| ())?,
            Statement::Declaration(stat) => stat.lower(ctx)?,
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
    fn lower(&mut self, _ctx: &mut Context) -> Result<(), E> {
        Ok(())
    }
}

impl Lower for ForStatement {
    fn lower(&mut self, _ctx: &mut Context) -> Result<(), E> {
        Ok(())
    }
}

impl Lower for WhileStatement {
    fn lower(&mut self, _ctx: &mut Context) -> Result<(), E> {
        Ok(())
    }
}

impl Lower for BreakStatement {
    fn lower(&mut self, _ctx: &mut Context) -> Result<(), E> {
        Ok(())
    }
}

impl Lower for ContinueStatement {
    fn lower(&mut self, _ctx: &mut Context) -> Result<(), E> {
        Ok(())
    }
}

impl Lower for ReturnStatement {
    fn lower(&mut self, _ctx: &mut Context) -> Result<(), E> {
        Ok(())
    }
}

impl Lower for DiscardStatement {
    fn lower(&mut self, _ctx: &mut Context) -> Result<(), E> {
        Ok(())
    }
}

impl Lower for FunctionCallStatement {
    fn lower(&mut self, _ctx: &mut Context) -> Result<(), E> {
        Ok(())
    }
}

impl Lower for TranslationUnit {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        self.exec(ctx)?;
        for decl in &mut self.global_declarations {
            match decl {
                GlobalDeclaration::Void => Ok(()),
                GlobalDeclaration::Declaration(decl) => decl.lower(ctx),
                GlobalDeclaration::TypeAlias(decl) => decl.lower(ctx),
                GlobalDeclaration::Struct(decl) => decl.lower(ctx),
                GlobalDeclaration::Function(decl) => decl.lower(ctx),
                GlobalDeclaration::ConstAssert(decl) => decl.lower(ctx),
            }
            .inspect_err(|_| ctx.set_err_decl_ctx(decl.name().unwrap_or_default()))?;
        }
        self.global_declarations.retain(|decl| match decl {
            GlobalDeclaration::Void => false,
            GlobalDeclaration::Declaration(_) => true,
            GlobalDeclaration::TypeAlias(_) => false,
            GlobalDeclaration::Struct(_) => true,
            GlobalDeclaration::Function(_) => true,
            GlobalDeclaration::ConstAssert(_) => false,
        });
        Ok(())
    }
}
