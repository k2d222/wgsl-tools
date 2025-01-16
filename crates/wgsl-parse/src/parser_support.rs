//! support functions to be injected in the lalrpop parser.

use std::str::FromStr;

use itertools::Itertools;

use crate::{
    error::CustomLalrError,
    span::{Span, Spanned},
    syntax::*,
};

type E = CustomLalrError;

pub(crate) enum Component {
    Named(Ident),
    Index(ExpressionNode),
}

pub(crate) fn apply_components(
    expr: Expression,
    span: Span,
    components: Vec<Spanned<Component>>,
) -> Expression {
    components.into_iter().fold(expr, |base, comp| {
        let span = span.extend(comp.span());
        let base = Spanned::new(base, span);
        match comp.into_inner() {
            Component::Named(component) => {
                Expression::NamedComponent(NamedComponentExpression { base, component })
            }
            Component::Index(index) => Expression::Indexing(IndexingExpression { base, index }),
        }
    })
}

impl FromStr for DeclarationKind {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "const" => Ok(Self::Const),
            "override" => Ok(Self::Override),
            "let" => Ok(Self::Let),
            "var" => Ok(Self::Var(None)),
            _ => Err(()),
        }
    }
}

impl FromStr for AddressSpace {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "function" => Ok(Self::Function),
            "private" => Ok(Self::Private),
            "workgroup" => Ok(Self::Workgroup),
            "uniform" => Ok(Self::Uniform),
            "storage" => Ok(Self::Storage(None)),
            // "WGSL predeclares an enumerant for each address space, except for the handle address space."
            // "handle" => Ok(Self::Handle),
            _ => Err(()),
        }
    }
}

impl FromStr for AccessMode {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "read" => Ok(Self::Read),
            "write" => Ok(Self::Write),
            "read_write" => Ok(Self::ReadWrite),
            _ => Err(()),
        }
    }
}

impl FromStr for DiagnosticSeverity {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "error" => Ok(Self::Error),
            "warning" => Ok(Self::Warning),
            "info" => Ok(Self::Info),
            "off" => Ok(Self::Off),
            _ => Err(()),
        }
    }
}

impl FromStr for BuiltinValue {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "vertex_index" => Ok(Self::VertexIndex),
            "instance_index" => Ok(Self::InstanceIndex),
            "position" => Ok(Self::Position),
            "front_facing" => Ok(Self::FrontFacing),
            "frag_depth" => Ok(Self::FragDepth),
            "sample_index" => Ok(Self::SampleIndex),
            "sample_mask" => Ok(Self::SampleMask),
            "local_invocation_id" => Ok(Self::LocalInvocationId),
            "local_invocation_index" => Ok(Self::LocalInvocationIndex),
            "global_invocation_id" => Ok(Self::GlobalInvocationId),
            "workgroup_id" => Ok(Self::WorkgroupId),
            "num_workgroups" => Ok(Self::NumWorkgroups),
            _ => Err(()),
        }
    }
}

impl FromStr for InterpolationType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "perspective" => Ok(Self::Perspective),
            "linear" => Ok(Self::Linear),
            "flat" => Ok(Self::Flat),
            _ => Err(()),
        }
    }
}

impl FromStr for InterpolationSampling {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "center" => Ok(Self::Center),
            "centroid" => Ok(Self::Centroid),
            "sample" => Ok(Self::Sample),
            "first" => Ok(Self::First),
            "either" => Ok(Self::Either),
            _ => Err(()),
        }
    }
}

fn one_arg(arguments: Option<Vec<ExpressionNode>>) -> Option<ExpressionNode> {
    match arguments {
        Some(mut args) => (args.len() == 1).then(|| args.pop().unwrap()),
        None => None,
    }
}
fn two_args(arguments: Option<Vec<ExpressionNode>>) -> Option<(ExpressionNode, ExpressionNode)> {
    match arguments {
        Some(args) => (args.len() == 2).then(|| args.into_iter().collect_tuple().unwrap()),
        None => None,
    }
}
fn zero_args(arguments: Option<Vec<ExpressionNode>>) -> bool {
    match arguments {
        Some(_) => false,
        None => true,
    }
}
fn ident(expr: ExpressionNode) -> Option<Ident> {
    match expr.into_inner() {
        Expression::TypeOrIdentifier(TypeExpression {
            ident: name,
            template_args: None,
        }) => Some(name),
        _ => None,
    }
}

pub(crate) fn parse_attribute(
    name: String,
    args: Option<Vec<ExpressionNode>>,
) -> Result<Attribute, E> {
    match name.as_str() {
        "align" => match one_arg(args) {
            Some(expr) => Ok(Attribute::Align(expr)),
            _ => Err(E::Attribute("align", "expected 1 argument")),
        },
        "binding" => match one_arg(args) {
            Some(expr) => Ok(Attribute::Binding(expr)),
            _ => Err(E::Attribute("binding", "expected 1 argument")),
        },
        "blend_src" => match one_arg(args) {
            Some(expr) => Ok(Attribute::BlendSrc(expr)),
            _ => Err(E::Attribute("blend_src", "expected 1 argument")),
        },
        "builtin" => match one_arg(args) {
            Some(expr) => match ident(expr).and_then(|name| name.name().parse().ok()) {
                Some(b) => Ok(Attribute::Builtin(b)),
                _ => Err(E::Attribute(
                    "builtin",
                    "the argument is not a valid built-in value name",
                )),
            },
            _ => Err(E::Attribute("builtin", "expected 1 argument")),
        },
        "const" => match zero_args(args) {
            true => Ok(Attribute::Const),
            false => Err(E::Attribute("const", "expected 0 arguments")),
        },
        "diagnostic" => match two_args(args) {
            Some((e1, e2)) => {
                let severity = ident(e1).and_then(|name| name.name().parse().ok());
                let rule = match e2.into_inner() {
                    Expression::TypeOrIdentifier(TypeExpression {
                        ident: name,
                        template_args: None,
                    }) => Some(name.name().to_string()),
                    Expression::NamedComponent(e) => {
                        ident(e.base).map(|name| format!("{}.{}", name.name(), e.component))
                    }
                    _ => None,
                };
                match (severity, rule) {
                    (Some(severity), Some(rule)) => {
                        Ok(Attribute::Diagnostic(DiagnosticAttribute {
                            severity,
                            rule,
                        }))
                    }
                    _ => Err(E::Attribute("diagnostic", "invalid arguments")),
                }
            }
            _ => Err(E::Attribute("diagnostic", "expected 1 argument")),
        },
        "group" => match one_arg(args) {
            Some(expr) => Ok(Attribute::Group(expr)),
            _ => Err(E::Attribute("group", "expected 1 argument")),
        },
        "id" => match one_arg(args) {
            Some(expr) => Ok(Attribute::Id(expr)),
            _ => Err(E::Attribute("id", "expected 1 argument")),
        },
        "interpolate" => match two_args(args) {
            Some((e1, e2)) => {
                let ty = ident(e1).and_then(|name| name.name().parse().ok());
                let sampling = ident(e2).and_then(|name| name.name().parse().ok());
                match (ty, sampling) {
                    (Some(ty), Some(sampling)) => {
                        Ok(Attribute::Interpolate(InterpolateAttribute {
                            ty,
                            sampling,
                        }))
                    }
                    _ => Err(E::Attribute("interpolate", "invalid arguments")),
                }
            }
            _ => Err(E::Attribute("interpolate", "invalid arguments")),
        },
        "invariant" => match zero_args(args) {
            true => Ok(Attribute::Invariant),
            false => Err(E::Attribute("invariant", "expected 0 arguments")),
        },
        "location" => match one_arg(args) {
            Some(expr) => Ok(Attribute::Location(expr)),
            _ => Err(E::Attribute("location", "expected 1 argument")),
        },
        "must_use" => match zero_args(args) {
            true => Ok(Attribute::MustUse),
            false => Err(E::Attribute("must_use", "expected 0 arguments")),
        },
        "size" => match one_arg(args) {
            Some(expr) => Ok(Attribute::Size(expr)),
            _ => Err(E::Attribute("size", "expected 1 argument")),
        },
        "workgroup_size" => match args {
            Some(args) => {
                let mut it = args.into_iter();
                match (it.next(), it.next(), it.next(), it.next()) {
                    (Some(x), y, z, None) => {
                        Ok(Attribute::WorkgroupSize(WorkgroupSizeAttribute { x, y, z }))
                    }
                    _ => Err(E::Attribute("workgroup_size", "expected 1-3 arguments")),
                }
            }
            _ => Err(E::Attribute("workgroup_size", "expected 1-3 arguments")),
        },
        "vertex" => match zero_args(args) {
            true => Ok(Attribute::Vertex),
            false => Err(E::Attribute("vertex", "expected 0 arguments")),
        },
        "fragment" => match zero_args(args) {
            true => Ok(Attribute::Fragment),
            false => Err(E::Attribute("fragment", "expected 0 arguments")),
        },
        "compute" => match zero_args(args) {
            true => Ok(Attribute::Compute),
            false => Err(E::Attribute("compute", "expected 0 arguments")),
        },
        #[cfg(feature = "condcomp")]
        "if" => match one_arg(args) {
            Some(expr) => Ok(Attribute::If(expr)),
            None => Err(E::Attribute("if", "expected 1 argument")),
        },
        #[cfg(feature = "generics")]
        "type" => parse_attr_type(args).map(Attribute::Type),
        _ => Ok(Attribute::Custom(CustomAttribute {
            name,
            arguments: args,
        })),
    }
}

// format: @type(T, foo | bar | baz)
fn parse_attr_type(arguments: Option<Vec<ExpressionNode>>) -> Result<TypeConstraint, E> {
    fn parse_rec(expr: Expression) -> Result<Vec<TypeExpression>, E> {
        match expr {
            Expression::TypeOrIdentifier(ty) => Ok(vec![ty]),
            Expression::Binary(BinaryExpression {
                operator: BinaryOperator::BitwiseOr,
                left,
                right,
            }) => {
                let ty = match right.into_inner() {
                    Expression::TypeOrIdentifier(ty) => Ok(ty),
                    _ => Err(E::Attribute(
                        "type",
                        "invalid second argument (type constraint)",
                    )),
                }?;
                let mut v = parse_rec(left.into_inner())?;
                v.push(ty);
                Ok(v)
            }
            _ => Err(E::Attribute(
                "type",
                "invalid second argument (type constraint)",
            )),
        }
    }
    match two_args(arguments) {
        Some((e1, e2)) => match e1.into_inner() {
            Expression::TypeOrIdentifier(TypeExpression {
                ident: name,
                template_args: None,
            }) => parse_rec(e2.into_inner()).map(|variants| TypeConstraint {
                ident: name,
                variants,
            }),
            _ => Err(E::Attribute("type", "invalid first argument (type name)")),
        },
        None => Err(E::Attribute("type", "expected 2 arguments")),
    }
}

pub(crate) fn parse_var_template(template_args: TemplateArgs) -> Result<Option<AddressSpace>, E> {
    fn ident(expr: ExpressionNode) -> Option<String> {
        match expr.into_inner() {
            Expression::TypeOrIdentifier(TypeExpression {
                ident: name,
                template_args: None,
            }) => Some(name.name().to_string()),
            _ => None,
        }
    }
    match template_args {
        Some(tplt) => {
            let mut it = tplt.into_iter();
            match (it.next(), it.next(), it.next()) {
                (Some(e1), e2, None) => {
                    let mut addr_space = ident(e1.expression)
                        .and_then(|name| name.parse().ok())
                        .ok_or(E::VarTemplate("invalid address space"))?;
                    if let Some(e2) = e2 {
                        if let AddressSpace::Storage(access_mode) = &mut addr_space {
                            *access_mode = Some(
                                ident(e2.expression)
                                    .and_then(|name| name.parse().ok())
                                    .ok_or(E::VarTemplate("invalid access mode"))?,
                            );
                        } else {
                            return Err(E::VarTemplate("only variables with `storage` address space can have an access mode"));
                        }
                    }
                    Ok(Some(addr_space))
                }
                _ => Err(E::VarTemplate("template is empty")),
            }
        }
        None => Ok(None),
    }
}
