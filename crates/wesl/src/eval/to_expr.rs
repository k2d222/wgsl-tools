use super::{
    name_to_builtin_ident, ArrayInstance, LiteralInstance, MatInstance, StructInstance, SyntaxUtil,
    Ty, Type, VecInstance, IDENT_ARRAY, IDENT_ATOMIC, IDENT_BOOL, IDENT_F16, IDENT_F32, IDENT_I32,
    IDENT_PTR, IDENT_U32,
};
use crate::eval::{Context, EvalError, Instance};
use wgsl_parse::{span::Spanned, syntax::*};

type E = EvalError;

/// Convert and instance to an Expression.
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
            Instance::Atomic(_) | Instance::Ptr(_) | Instance::Ref(_) | Instance::Void => {
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
            .decl_struct(self.ident())
            .expect("struct declaration not found");
        Ok(Expression::FunctionCall(FunctionCall {
            ty: TypeExpression {
                ident: self.ident().clone(),
                template_args: None,
            },
            arguments: decl
                .members
                .iter()
                .map(|m| {
                    self.member(&m.ident)
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
                ident: Ident::new("array".to_string()),
                template_args: None,
            },
            arguments: self
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
                ident: Ident::new(format!("vec{}", self.n())),
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
                ident: Ident::new(format!("mat{}x{}", self.c(), self.r())),
                template_args: None,
            },
            arguments: self
                .iter_cols()
                .map(|c| c.to_expr(ctx).map(Spanned::from))
                .collect::<Result<Vec<_>, _>>()?,
        }))
    }
}

impl ToExpr for Type {
    fn to_expr(&self, ctx: &Context) -> Result<Expression, E> {
        match self {
            Type::Bool => Ok(TypeExpression {
                ident: IDENT_BOOL.clone(),
                template_args: None,
            }),
            Type::AbstractInt => Err(E::NotConstructible(Type::AbstractInt)),
            Type::AbstractFloat => Err(E::NotConstructible(Type::AbstractFloat)),
            Type::I32 => Ok(TypeExpression {
                ident: IDENT_I32.clone(),
                template_args: None,
            }),
            Type::U32 => Ok(TypeExpression {
                ident: IDENT_U32.clone(),
                template_args: None,
            }),
            Type::F32 => Ok(TypeExpression {
                ident: IDENT_F32.clone(),
                template_args: None,
            }),
            Type::F16 => Ok(TypeExpression {
                ident: IDENT_F16.clone(),
                template_args: None,
            }),
            Type::Struct(s) => Ok(TypeExpression {
                ident: s.clone(),
                template_args: None,
            }),
            Type::Array(Some(n), ty) => Ok(TypeExpression {
                ident: IDENT_ARRAY.clone(),
                template_args: Some(vec![
                    TemplateArg {
                        expression: ty.to_expr(ctx)?.into(),
                    },
                    TemplateArg {
                        expression: Expression::TypeOrIdentifier(Ident::new(n.to_string()).into())
                            .into(),
                    },
                ]),
            }),
            Type::Array(None, ty) => Ok(TypeExpression {
                ident: IDENT_ARRAY.clone(),
                template_args: Some(vec![TemplateArg {
                    expression: ty.to_expr(ctx)?.into(),
                }]),
            }),
            Type::Vec(n, ty) => Ok(TypeExpression {
                ident: name_to_builtin_ident(&format!("vec{n}")).unwrap(),
                template_args: Some(vec![TemplateArg {
                    expression: ty.to_expr(ctx)?.into(),
                }]),
            }),
            Type::Mat(c, r, ty) => Ok(TypeExpression {
                ident: name_to_builtin_ident(&format!("mat{c}x{r}")).unwrap(),
                template_args: Some(vec![TemplateArg {
                    expression: ty.to_expr(ctx)?.into(),
                }]),
            }),
            Type::Atomic(a) => Ok(TypeExpression {
                ident: IDENT_ATOMIC.clone(),
                template_args: Some(vec![TemplateArg {
                    expression: a.to_expr(ctx)?.into(),
                }]),
            }),
            Type::Ptr(space, p) => Ok(TypeExpression {
                ident: IDENT_PTR.clone(),
                template_args: Some(vec![
                    TemplateArg {
                        expression: space.to_expr(ctx)?.into(),
                    },
                    TemplateArg {
                        expression: p.to_expr(ctx)?.into(),
                    },
                ]),
            }),
            Type::Void => Err(E::NotConstructible(Type::Void)),
        }
        .map(Into::into)
    }
}

impl ToExpr for AddressSpace {
    fn to_expr(&self, _ctx: &Context) -> Result<Expression, E> {
        Ok(Expression::TypeOrIdentifier(TypeExpression::from(
            Ident::new(self.to_string()),
        )))
    }
}
