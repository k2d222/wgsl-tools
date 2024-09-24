use std::{
    collections::{HashMap, HashSet},
    iter::zip,
};

use itertools::Itertools;
use lazy_static::lazy_static;
use wgsl_parse::syntax::{
    self, Attribute, Expression, FunctionCall, GlobalDeclaration, LiteralExpression, TemplateArg,
    TemplateArgs, TranslationUnit,
};

use super::{
    conv::{convert_all, Convert},
    ArrayInstance, ConstEvalError, Flow, Instance, LiteralInstance, MatInner, MatInstance, Ty,
    Type, VecInner, VecInstance,
};

type E = ConstEvalError;

// TODO: when we have the wgsl! macro, we can refactor this.
lazy_static! {
    pub static ref ATTR_BUILTIN: Attribute = Attribute {
        name: "__builtin".to_string(),
        arguments: None
    };
    pub static ref PRELUDE: TranslationUnit = {
        let mut prelude = include_str!("prelude.wgsl")
            .parse::<TranslationUnit>()
            .inspect_err(|e| eprintln!("{e}"))
            .unwrap();

        let attr_internal = Attribute {
            name: "internal".to_string(),
            arguments: None,
        };
        let attr_builtin = Attribute {
            name: "builtin".to_string(),
            arguments: None,
        };

        for decl in &mut prelude.global_declarations {
            match decl {
                GlobalDeclaration::Struct(s) => {
                    if s.attributes.contains(&attr_internal) {
                        s.name = format!("__{}", s.name);
                    }
                }
                GlobalDeclaration::Function(f) => {
                    if let Some(attr) = f
                        .body
                        .attributes
                        .iter_mut()
                        .find(|attr| **attr == attr_builtin)
                    {
                        attr.name = "__builtin".to_string();
                    }
                }
                _ => (),
            }
        }

        prelude
    };
}

// ------------
// CONSTRUCTORS
// ------------
// reference: https://www.w3.org/TR/WGSL/#constructor-builtin-function

pub fn call_builtin(
    name: &str,
    tplt: Option<Vec<Instance>>,
    args: Vec<Instance>,
) -> Result<Instance, E> {
    let f = get_builtin_fn(name).ok_or_else(|| E::NotImpl(name.to_string()))?;
    f(tplt, args)
}

type BuiltinFn = dyn Fn(Option<Vec<Instance>>, Vec<Instance>) -> Result<Instance, E>;
pub fn get_builtin_fn(name: &str) -> Option<&BuiltinFn> {
    match name {
        "array" => Some(&call_array),
        "vec2" => Some(&|tplt, args| call_vecN(2, tplt, args)),
        "vec3" => Some(&|tplt, args| call_vecN(3, tplt, args)),
        "vec4" => Some(&|tplt, args| call_vecN(4, tplt, args)),
        _ => None,
    }
}

struct ArrayTemplate {
    count: usize,
    ty: Type,
}
impl ArrayTemplate {
    fn parse(tplt: Vec<Instance>) -> Result<ArrayTemplate, E> {
        match tplt.as_slice() {
            [Instance::Type(ty), Instance::Literal(n)] => {
                let count = match n {
                    LiteralInstance::AbstractInt(n) => (*n > 0).then_some(*n as usize),
                    LiteralInstance::I32(n) => (*n > 0).then_some(*n as usize),
                    LiteralInstance::U32(n) => (*n > 0).then_some(*n as usize),
                    _ => None,
                }
                .ok_or_else(|| {
                    E::Builtin(
                        "the array element count must evaluate to a `u32` or a `i32` greater than `0`",
                    )
                })?;
                Ok(ArrayTemplate {
                    count,
                    ty: ty.clone(),
                })
            }
            _ => Err(E::TemplateArgs("array")),
        }
    }
}

fn call_array(tplt: Option<Vec<Instance>>, args: Vec<Instance>) -> Result<Instance, E> {
    match tplt {
        Some(tplt) => {
            let tplt = ArrayTemplate::parse(tplt)?;

            let args = args
                .iter()
                .map(|a| {
                    a.convert_inner_to(&tplt.ty)
                        .ok_or_else(|| E::ParamType(tplt.ty.clone(), a.ty()))
                })
                .collect::<Result<Vec<_>, _>>()?;

            if args.len() != tplt.count {
                return Err(E::ParamCount(tplt.count, args.len()));
            }

            Ok(ArrayInstance::new(args).into())
        }
        None => {
            let args = convert_all(&args)
                .ok_or_else(|| E::Builtin("array elements are not compatible"))?;

            if args.len() < 1 {
                return Err(E::Builtin("array constructor expects at least 1 argument"));
            }

            Ok(ArrayInstance::new(args).into())
        }
    }
}

struct VecTemplate {
    ty: Type,
}
impl VecTemplate {
    fn parse(tplt: Vec<Instance>) -> Result<VecTemplate, E> {
        match tplt.as_slice() {
            [Instance::Type(ty)] => {
                if !ty.is_scalar() || ty.is_abstract() {
                    return Err(ConstEvalError::Builtin(
                        "vector template type must be a concrete scalar type",
                    ));
                }
                Ok(VecTemplate { ty: ty.clone() })
            }
            _ => Err(E::TemplateArgs("vector")),
        }
    }
}

fn call_vecN(n: usize, tplt: Option<Vec<Instance>>, args: Vec<Instance>) -> Result<Instance, E> {
    if args.len() != n {
        return Err(E::ParamCount(n, args.len()));
    }

    match tplt {
        Some(tplt) => {
            let tplt = VecTemplate::parse(tplt)?;

            let args = args
                .iter()
                .map(|a| {
                    a.convert_inner_to(&tplt.ty)
                        .ok_or_else(|| E::ParamType(tplt.ty.clone(), a.ty()))
                        .and_then(|a| match a {
                            Instance::Literal(l) => Ok(l),
                            _ => unreachable!("vec type should be concrete scalar"),
                        })
                })
                .collect::<Result<Vec<_>, _>>()?;

            Ok(VecInstance::new(args).into())
        }
        None => {
            let args = convert_all(&args)
                .ok_or_else(|| E::Builtin("vector components are not compatible"))?
                .into_iter()
                .map(|a| match a {
                    Instance::Literal(l) => Ok(l),
                    _ => unreachable!("vec type should be concrete scalar"),
                })
                .collect::<Result<Vec<_>, _>>()?;

            Ok(VecInstance::new(args).into())
        }
    }
}

// -------
// NUMERIC
// -------
// reference: https://www.w3.org/TR/WGSL/#numeric-builtin-function

impl<const N: usize> VecInner<N> {
    pub fn dot(&self, rhs: &VecInner<N>) -> Result<LiteralInstance, E> {
        zip(self.iter(), rhs.iter())
            .map(|(a, b)| a.op_mul(b))
            .reduce(|a, b| a?.op_add(&b?))
            .unwrap()
    }
}

impl VecInstance {
    pub fn dot(&self, rhs: &VecInstance) -> Result<LiteralInstance, E> {
        match (self, rhs) {
            (VecInstance::Vec2(lhs), VecInstance::Vec2(rhs)) => lhs.dot(rhs),
            (VecInstance::Vec3(lhs), VecInstance::Vec3(rhs)) => lhs.dot(rhs),
            (VecInstance::Vec4(lhs), VecInstance::Vec4(rhs)) => lhs.dot(rhs),
            (lhs @ _, _) => Err(E::CompwiseBinary(lhs.ty(), rhs.ty())),
        }
    }
}

impl<const C: usize, const R: usize> MatInner<C, R> {
    pub fn transpose(&self) -> MatInner<R, C> {
        let components = (0..self.components.len())
            .map(|i| {
                self.components
                    .iter()
                    .map(|inner| inner.components[i].clone())
                    .collect_vec()
                    .into()
            })
            .collect_vec();
        MatInner { components }
    }
}

impl MatInstance {
    pub fn transpose(&self) -> MatInstance {
        match self {
            Self::Mat2x2(m) => Self::Mat2x2(m.transpose()),
            Self::Mat2x3(m) => Self::Mat3x2(m.transpose()),
            Self::Mat2x4(m) => Self::Mat4x2(m.transpose()),
            Self::Mat3x2(m) => Self::Mat2x3(m.transpose()),
            Self::Mat3x3(m) => Self::Mat3x3(m.transpose()),
            Self::Mat3x4(m) => Self::Mat4x3(m.transpose()),
            Self::Mat4x2(m) => Self::Mat2x4(m.transpose()),
            Self::Mat4x3(m) => Self::Mat3x4(m.transpose()),
            Self::Mat4x4(m) => Self::Mat4x4(m.transpose()),
        }
    }
}
