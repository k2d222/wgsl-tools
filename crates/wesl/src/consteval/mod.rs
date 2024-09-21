mod builtin;
mod conv;
mod display;
mod eval;
mod instance;
mod ops;
mod ty;

use conv::apply_conversion;

pub use eval::Eval;
pub use instance::*;
pub use ty::*;

use core::panic;
use derive_more::From;
use std::collections::{HashMap, HashSet};

use lazy_static::lazy_static;
use thiserror::Error;
use wgsl_parse::syntax::*;

use crate::syntax_util::struct_decl;

#[derive(Clone, Debug, Error)]
pub enum ConstEvalError {
    #[error("`{0}` has no component `{1}`")]
    NoComponent(Type, String),
    #[error("`{0}` cannot be indexed")]
    NotIndexable(Type),
    #[error("invalid vector component or swizzle `{0}`")]
    InvalidSwizzle(String),
    #[error("index `{0}` is out-of-bounds for `{1}` of `{2}` components")]
    OutOfBounds(usize, Type, usize),
    #[error("type `{0}` is not indexable")]
    InvalidIndex(Type),
    #[error("cannot use unary operator `{0}` on type `{1}`")]
    InvalidUnary(UnaryOperator, Type),
    #[error("cannot use binary operator `{0}` with operands `{1}` and `{2}`")]
    InvalidBinary(BinaryOperator, Type, Type),
    #[error("cannot apply component-wise binary operation on operands `{0}` and `{1}`")]
    InvalidCompwiseBinary(Type, Type),
    #[error("invalid reference to memory location `{0}`")]
    InvalidRef(Address),
    #[error("type `{0}` does not take any generic parameters")]
    UnexpectedGeneric(Type),
    #[error("missing generic parameters for type `{0}`")]
    MissingGeneric(&'static str),
    #[error("invalid generic parameters for type `{0}`")]
    InvalidGeneric(&'static str),
    #[error("unknown type `{0}`")]
    UnknownType(TypeExpression),
    #[error("invalid reference to `{0}`, expected `{1}`")]
    InvalidRefType(Type, Type),
    #[error("cannot convert from `{0}` to `{1}`")]
    ConversionFailure(Type, Type),
    #[error("attempt to negate with overflow")]
    NegOverflow,
    #[error("attempt to add with overflow")]
    AddOverflow,
    #[error("attempt to subtract with underflow")]
    SubUnderflow,
    #[error("attempt to multiply with overflow")]
    MulOverflow,
    #[error("attempt to divide by zero")]
    DivByZero,
    #[error("attempt to calculate the remainder with a divisor of zero")]
    RemZeroDiv,
}

lazy_static! {
    static ref PREDECLARED_ALIASES: HashMap<&'static str, Type> = HashMap::from_iter([
        // reference: https://www.w3.org/TR/WGSL/#vector-types
        ("vec2i", Type::Vec(2, Box::new(Type::I32))),
        ("vec3i", Type::Vec(3, Box::new(Type::I32))),
        ("vec4i", Type::Vec(4, Box::new(Type::I32))),
        ("vec2u", Type::Vec(2, Box::new(Type::U32))),
        ("vec3u", Type::Vec(3, Box::new(Type::U32))),
        ("vec4u", Type::Vec(4, Box::new(Type::U32))),
        ("vec2f", Type::Vec(2, Box::new(Type::F32))),
        ("vec3f", Type::Vec(3, Box::new(Type::F32))),
        ("vec4f", Type::Vec(4, Box::new(Type::F32))),
        ("vec2h", Type::Vec(2, Box::new(Type::F16))),
        ("vec3h", Type::Vec(3, Box::new(Type::F16))),
        ("vec4h", Type::Vec(4, Box::new(Type::F16))),
        // reference: https://www.w3.org/TR/WGSL/#matrix-types
        ("mat2x2f", Type::Mat(2, 2, Box::new(Type::F32))),
        ("mat2x3f", Type::Mat(2, 3, Box::new(Type::F32))),
        ("mat2x4f", Type::Mat(2, 4, Box::new(Type::F32))),
        ("mat3x2f", Type::Mat(3, 2, Box::new(Type::F32))),
        ("mat3x3f", Type::Mat(3, 3, Box::new(Type::F32))),
        ("mat3x4f", Type::Mat(3, 4, Box::new(Type::F32))),
        ("mat4x2f", Type::Mat(4, 2, Box::new(Type::F32))),
        ("mat4x3f", Type::Mat(4, 3, Box::new(Type::F32))),
        ("mat4x4f", Type::Mat(4, 4, Box::new(Type::F32))),
        ("mat2x2h", Type::Mat(2, 2, Box::new(Type::F16))),
        ("mat2x3h", Type::Mat(2, 3, Box::new(Type::F16))),
        ("mat2x4h", Type::Mat(2, 4, Box::new(Type::F16))),
        ("mat3x2h", Type::Mat(3, 2, Box::new(Type::F16))),
        ("mat3x3h", Type::Mat(3, 3, Box::new(Type::F16))),
        ("mat3x4h", Type::Mat(3, 4, Box::new(Type::F16))),
        ("mat4x2h", Type::Mat(4, 2, Box::new(Type::F16))),
        ("mat4x3h", Type::Mat(4, 3, Box::new(Type::F16))),
        ("mat4x4h", Type::Mat(4, 4, Box::new(Type::F16))),
    ]);

    static ref BUILTIN_STRUCTURES: HashSet<&'static str> = HashSet::from_iter([
        "__frexp_result_f32",
        "__frexp_result_f16",
        "__frexp_result_abstract",
        "__frexp_result_vec2_f32",
        "__frexp_result_vec3_f32",
        "__frexp_result_vec4_f32",
        "__frexp_result_vec2_f16",
        "__frexp_result_vec3_f16",
        "__frexp_result_vec4_f16",
        "__frexp_result_vec2_abstract",
        "__frexp_result_vec3_abstract",
        "__frexp_result_vec4_abstract",
        "__modf_result_f32",
        "__modf_result_f16",
        "__modf_result_abstract",
        "__modf_result_vec2_f32",
        "__modf_result_vec3_f32",
        "__modf_result_vec4_f32",
        "__modf_result_vec2_f16",
        "__modf_result_vec3_f16",
        "__modf_result_vec4_f16",
        "__modf_result_vec2_abstract",
        "__modf_result_vec3_abstract",
        "__modf_result_vec4_abstract",
        "__atomic_compare_exchange_result",
    ]);
}

pub struct Context<'s> {
    source: &'s TranslationUnit,
    variables: HashMap<String, usize>,
    memory: Vec<Instance>,
}

impl<'s> Context<'s> {
    pub fn new(source: &'s TranslationUnit) -> Self {
        Self {
            source,
            variables: Default::default(),
            memory: Default::default(),
        }
    }
}
