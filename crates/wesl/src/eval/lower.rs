use crate::{with_stage, Context, Eval, EvalError, Exec, Instance};
use wgsl_parse::{span::Spanned, syntax::*};

use super::{
    ArrayInstance, EvalStage, LiteralInstance, MatInstance, StructInstance, SyntaxUtil, Ty, Type,
    VecInstance,
};

type E = EvalError;

const EXPR_TRUE: Expression = Expression::Literal(LiteralExpression::Bool(true));
const EXPR_FALSE: Expression = Expression::Literal(LiteralExpression::Bool(false));

pub trait ToExpr {
    fn to_expr(&self, ctx: &Context) -> Result<Expression, E>;
}

impl ToExpr for Instance {
    fn to_expr(&self, ctx: &Context) -> Result<Expression, E> {
        match self {
            Instance::Literal(inst) => inst.to_expr(ctx),
            Instance::Struct(inst) => inst.to_expr(ctx),
            Instance::Array(inst) => inst.to_expr(ctx),
            Instance::Vec(inst) => inst.to_expr(ctx),
            Instance::Mat(inst) => inst.to_expr(ctx),
            Instance::Type(inst) => inst.to_expr(ctx),
            Instance::Ptr(_) | Instance::Ref(_) | Instance::Void => {
                Err(E::NotConstructible(self.ty()))
            }
        }
    }
}

impl ToExpr for LiteralInstance {
    fn to_expr(&self, _ctx: &Context) -> Result<Expression, E> {
        Ok(match self {
            LiteralInstance::Bool(lit) => LiteralExpression::Bool(*lit),
            LiteralInstance::AbstractInt(lit) => LiteralExpression::AbstractInt(*lit),
            LiteralInstance::AbstractFloat(lit) => LiteralExpression::AbstractFloat(*lit),
            LiteralInstance::I32(lit) => LiteralExpression::I32(*lit),
            LiteralInstance::U32(lit) => LiteralExpression::U32(*lit),
            LiteralInstance::F32(lit) => LiteralExpression::F32(*lit),
            LiteralInstance::F16(lit) => LiteralExpression::F16(lit.to_f32()),
        }
        .into())
    }
}

impl ToExpr for StructInstance {
    fn to_expr(&self, ctx: &Context) -> Result<Expression, E> {
        let decl = ctx
            .source
            .decl_struct(&self.name)
            .expect("struct declaration not found");
        Ok(Expression::FunctionCall(FunctionCall {
            ty: TypeExpression {
                name: self.name.clone(),
                template_args: None,
            },
            arguments: decl
                .members
                .iter()
                .map(|m| {
                    self.members
                        .get(&m.name)
                        .expect("struct member not found")
                        .to_expr(ctx)
                        .map(Spanned::from)
                })
                .collect::<Result<Vec<_>, _>>()?,
        }))
    }
}

impl ToExpr for ArrayInstance {
    fn to_expr(&self, ctx: &Context) -> Result<Expression, E> {
        Ok(Expression::FunctionCall(FunctionCall {
            ty: TypeExpression {
                name: format!("array"),
                template_args: None,
            },
            arguments: self
                .components
                .iter()
                .map(|c| c.to_expr(ctx).map(Spanned::from))
                .collect::<Result<Vec<_>, _>>()?,
        }))
    }
}

impl ToExpr for VecInstance {
    fn to_expr(&self, ctx: &Context) -> Result<Expression, E> {
        Ok(Expression::FunctionCall(FunctionCall {
            ty: TypeExpression {
                name: format!("vec{}", self.n()),
                template_args: None,
            },
            arguments: self
                .iter()
                .map(|c| c.to_expr(ctx).map(Spanned::from))
                .collect::<Result<Vec<_>, _>>()?,
        }))
    }
}

impl ToExpr for MatInstance {
    fn to_expr(&self, ctx: &Context) -> Result<Expression, E> {
        Ok(Expression::FunctionCall(FunctionCall {
            ty: TypeExpression {
                name: format!("mat{}x{}", self.c(), self.r()),
                template_args: None,
            },
            arguments: self
                .iter()
                .map(|c| c.to_expr(ctx).map(Spanned::from))
                .collect::<Result<Vec<_>, _>>()?,
        }))
    }
}

impl ToExpr for Type {
    fn to_expr(&self, ctx: &Context) -> Result<Expression, E> {
        match self {
            Type::Bool => Ok(TypeExpression {
                name: format!("bool"),
                template_args: None,
            }),
            Type::AbstractInt => Err(E::NotConstructible(Type::AbstractInt)),
            Type::AbstractFloat => Err(E::NotConstructible(Type::AbstractFloat)),
            Type::I32 => Ok(TypeExpression {
                name: format!("i32"),
                template_args: None,
            }),
            Type::U32 => Ok(TypeExpression {
                name: format!("u32"),
                template_args: None,
            }),
            Type::F32 => Ok(TypeExpression {
                name: format!("f32"),
                template_args: None,
            }),
            Type::F16 => Ok(TypeExpression {
                name: format!("f16"),
                template_args: None,
            }),
            Type::Struct(s) => Ok(TypeExpression {
                name: format!("{s}"),
                template_args: None,
            }),
            Type::Array(n, ty) => Ok(TypeExpression {
                name: format!("array"),
                template_args: Some(vec![
                    TemplateArg {
                        expression: Expression::Identifier(IdentifierExpression {
                            name: n.to_string(),
                        })
                        .into(),
                    },
                    TemplateArg {
                        expression: ty.to_expr(ctx)?.into(),
                    },
                ]),
            }),
            Type::Vec(n, ty) => Ok(TypeExpression {
                name: format!("vec{n}"),
                template_args: Some(vec![TemplateArg {
                    expression: ty.to_expr(ctx)?.into(),
                }]),
            }),
            Type::Mat(c, r, ty) => Ok(TypeExpression {
                name: format!("mat{c}x{r}"),
                template_args: Some(vec![TemplateArg {
                    expression: ty.to_expr(ctx)?.into(),
                }]),
            }),
            Type::Atomic(a) => Ok(TypeExpression {
                name: format!("atomic"),
                template_args: Some(vec![TemplateArg {
                    expression: a.to_expr(ctx)?.into(),
                }]),
            }),
            Type::Ptr(p) => Ok(TypeExpression {
                name: format!("ptr"),
                template_args: Some(vec![TemplateArg {
                    expression: p.to_expr(ctx)?.into(),
                }]),
            }),
            Type::Void => Err(E::NotConstructible(Type::Void)),
        }
        .map(Into::into)
    }
}

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
                Expression::Identifier(_) => (),
                Expression::Type(_) => *self = self.eval_value(ctx)?.to_expr(ctx)?,
            },
        }
        Ok(())
    }
}

impl Lower for FunctionCall {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
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
            match attr.name.as_str() {
                // those are the build-in attributes that take const-expressions
                "align" | "binding" | "blend_src" | "group" | "id" | "location" | "size"
                | "workgroup_size" => {
                    if let Some(args) = &mut attr.arguments {
                        for arg in args {
                            arg.lower(ctx)?;
                        }
                    }
                }
                // we ignore the unknown attributes and those that do not take const-expressions
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
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        Ok(())
    }
}

impl Lower for ForStatement {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        Ok(())
    }
}

impl Lower for WhileStatement {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        Ok(())
    }
}

impl Lower for BreakStatement {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        Ok(())
    }
}

impl Lower for ContinueStatement {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        Ok(())
    }
}

impl Lower for ReturnStatement {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        Ok(())
    }
}

impl Lower for DiscardStatement {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        Ok(())
    }
}

impl Lower for FunctionCallStatement {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        Ok(())
    }
}

impl Lower for ConstAssertStatement {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        Ok(())
    }
}

impl Lower for TranslationUnit {
    fn lower(&mut self, ctx: &mut Context) -> Result<(), E> {
        // self.global_directives.retain_mut(|directive| true);
        for decl in &mut self.global_declarations {
            match decl {
                GlobalDeclaration::Void => Ok(()),
                GlobalDeclaration::Declaration(decl) => decl.lower(ctx),
                GlobalDeclaration::TypeAlias(decl) => decl.lower(ctx),
                GlobalDeclaration::Struct(decl) => decl.lower(ctx),
                GlobalDeclaration::Function(decl) => decl.lower(ctx),
                GlobalDeclaration::ConstAssert(decl) => decl.exec(ctx).map(|_| ()),
            }
            .inspect_err(|_| ctx.set_err_decl_ctx(decl.name()))?;
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
