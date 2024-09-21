mod builtin;
mod conv;
mod display;
mod eval;
mod ops;
mod ty;

use conv::apply_conversion;

pub use eval::Eval;
pub use ty::{EvalTy, Ty};

use core::panic;
use derive_more::From;
use std::{
    collections::{HashMap, HashSet},
    iter::zip,
    ops::{Add, Div, Mul, RangeBounds, Rem, Sub},
    path::Components,
    ptr::write,
};

use itertools::Itertools;
use lazy_static::lazy_static;
use thiserror::Error;
use wgsl_parse::syntax::*;

use crate::syntax_util::{decl_name, struct_decl};

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
    #[error("division by zero")]
    DivByZero,
    #[error("number overflow")]
    Overflow,
    #[error("number underflow")]
    Unverflow,
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

#[derive(Clone, Debug, From, PartialEq)]
pub enum Instance {
    Literal(LiteralInstance),
    Struct(StructInstance),
    Array(ArrayInstance),
    Vec(VecInstance),
    Mat(MatInstance),
    Ptr(PtrInstance),
    Ref(RefInstance),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Bool,
    AbstractInt,
    AbstractFloat,
    I32,
    U32,
    F32,
    F16,
    Struct(String),
    Array(Box<Type>),
    Vec(u8, Box<Type>),
    Mat(u8, u8, Box<Type>),
    Ptr(Box<Type>),
}

#[derive(Clone, Copy, Debug, PartialEq, From)]
pub enum LiteralInstance {
    Bool(bool),
    AbstractInt(i64),
    AbstractFloat(f64),
    I32(i32),
    U32(u32),
    F32(f32),
    #[from(skip)]
    F16(f32),
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructInstance {
    name: String,
    components: HashMap<String, Instance>,
}

impl StructInstance {
    fn declaration<'s>(&self, wesl: &'s TranslationUnit) -> Option<&'s Struct> {
        struct_decl(&self.name, wesl)
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct ArrayInstance {
    components: Vec<Instance>,
}

impl ArrayInstance {
    pub fn new(components: Vec<Instance>) -> Self {
        Self { components }
    }

    pub fn n(&self) -> usize {
        self.components.len()
    }

    pub fn get(&self, i: usize) -> Option<&Instance> {
        self.components.get(i)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct VecInner<const N: usize> {
    components: Vec<LiteralInstance>,
}

impl<const N: usize> VecInner<N> {
    pub fn new(components: Vec<LiteralInstance>) -> Self {
        assert!(components.len() == N);
        Self { components }
    }
    pub fn new_with_value(val: &LiteralInstance) -> Self {
        Self {
            components: Vec::from_iter((0..N).map(|_| val.clone())),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &LiteralInstance> {
        self.components.iter()
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut LiteralInstance> {
        self.components.iter_mut()
    }
}

impl<const N: usize> From<Vec<LiteralInstance>> for VecInner<N> {
    fn from(components: Vec<LiteralInstance>) -> Self {
        assert!(components.len() == N);
        Self { components }
    }
}

pub type Vec2 = VecInner<2>;
pub type Vec3 = VecInner<3>;
pub type Vec4 = VecInner<4>;

#[derive(Clone, Debug, PartialEq, From)]
pub enum VecInstance {
    Vec2(Vec2),
    Vec3(Vec3),
    Vec4(Vec4),
}

impl VecInstance {
    fn new(components: Vec<LiteralInstance>) -> Self {
        match components.len() {
            2 => Self::Vec2(VecInner::new(components)),
            3 => Self::Vec3(VecInner::new(components)),
            4 => Self::Vec4(VecInner::new(components)),
            _ => panic!("VecInstance must have 2, 3 or 4 commponents"),
        }
    }

    fn n(&self) -> u8 {
        match self {
            VecInstance::Vec2(_) => 2,
            VecInstance::Vec3(_) => 3,
            VecInstance::Vec4(_) => 4,
        }
    }

    fn get(&self, i: usize) -> Option<&LiteralInstance> {
        match self {
            VecInstance::Vec2(v) => v.components.get(i),
            VecInstance::Vec3(v) => v.components.get(i),
            VecInstance::Vec4(v) => v.components.get(i),
        }
    }

    fn iter(&self) -> impl Iterator<Item = &LiteralInstance> {
        match self {
            Self::Vec2(v) => v.components.iter(),
            Self::Vec3(v) => v.components.iter(),
            Self::Vec4(v) => v.components.iter(),
        }
    }
    fn iter_mut(&mut self) -> impl Iterator<Item = &mut LiteralInstance> {
        match self {
            Self::Vec2(v) => v.components.iter_mut(),
            Self::Vec3(v) => v.components.iter_mut(),
            Self::Vec4(v) => v.components.iter_mut(),
        }
    }
}

impl From<Vec<LiteralInstance>> for VecInstance {
    fn from(components: Vec<LiteralInstance>) -> Self {
        match components.len() {
            2 => Self::Vec2(VecInner { components }),
            3 => Self::Vec2(VecInner { components }),
            4 => Self::Vec2(VecInner { components }),
            _ => panic!("number of VecInstance components must be 2, 3, 4"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MatInner<const C: usize, const R: usize> {
    components: Vec<VecInner<R>>,
}

impl<const C: usize, const R: usize> MatInner<C, R> {
    fn new(components: Vec<VecInner<R>>) -> Self {
        Self { components }
    }
    fn new_with_value(val: &LiteralInstance) -> Self {
        Self {
            components: Vec::from_iter((0..C).map(|_| VecInner::new_with_value(val))),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &LiteralInstance> {
        self.components.iter().flat_map(|c| c.iter())
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut LiteralInstance> {
        self.components.iter_mut().flat_map(|c| c.iter_mut())
    }
}

pub type Mat2x2 = MatInner<2, 2>;
pub type Mat2x3 = MatInner<2, 3>;
pub type Mat2x4 = MatInner<2, 4>;
pub type Mat3x2 = MatInner<3, 2>;
pub type Mat3x3 = MatInner<3, 3>;
pub type Mat3x4 = MatInner<3, 4>;
pub type Mat4x2 = MatInner<4, 2>;
pub type Mat4x3 = MatInner<4, 3>;
pub type Mat4x4 = MatInner<4, 4>;

#[derive(Clone, Debug, PartialEq)]
pub enum MatInstance {
    Mat2x2(Mat2x2),
    Mat2x3(Mat2x3),
    Mat2x4(Mat2x4),
    Mat3x2(Mat3x2),
    Mat3x3(Mat3x3),
    Mat3x4(Mat3x4),
    Mat4x2(Mat4x2),
    Mat4x3(Mat4x3),
    Mat4x4(Mat4x4),
}

impl<const C: usize, const R: usize> From<Vec<VecInner<R>>> for MatInner<C, R> {
    fn from(components: Vec<VecInner<R>>) -> Self {
        assert!(components.len() == C);
        Self { components }
    }
}

impl MatInstance {
    fn r(&self) -> u8 {
        match self {
            MatInstance::Mat2x2(_) => 2,
            MatInstance::Mat2x3(_) => 3,
            MatInstance::Mat2x4(_) => 4,
            MatInstance::Mat3x2(_) => 2,
            MatInstance::Mat3x3(_) => 3,
            MatInstance::Mat3x4(_) => 4,
            MatInstance::Mat4x2(_) => 2,
            MatInstance::Mat4x3(_) => 3,
            MatInstance::Mat4x4(_) => 4,
        }
    }
    fn c(&self) -> u8 {
        match self {
            MatInstance::Mat2x2(_) => 2,
            MatInstance::Mat2x3(_) => 2,
            MatInstance::Mat2x4(_) => 2,
            MatInstance::Mat3x2(_) => 3,
            MatInstance::Mat3x3(_) => 3,
            MatInstance::Mat3x4(_) => 3,
            MatInstance::Mat4x2(_) => 4,
            MatInstance::Mat4x3(_) => 4,
            MatInstance::Mat4x4(_) => 4,
        }
    }

    fn col(&self, i: usize) -> Option<VecInstance> {
        match self {
            MatInstance::Mat2x2(m) => m.components.get(i).map(|c| c.clone().into()),
            MatInstance::Mat2x3(m) => m.components.get(i).map(|c| c.clone().into()),
            MatInstance::Mat2x4(m) => m.components.get(i).map(|c| c.clone().into()),
            MatInstance::Mat3x2(m) => m.components.get(i).map(|c| c.clone().into()),
            MatInstance::Mat3x3(m) => m.components.get(i).map(|c| c.clone().into()),
            MatInstance::Mat3x4(m) => m.components.get(i).map(|c| c.clone().into()),
            MatInstance::Mat4x2(m) => m.components.get(i).map(|c| c.clone().into()),
            MatInstance::Mat4x3(m) => m.components.get(i).map(|c| c.clone().into()),
            MatInstance::Mat4x4(m) => m.components.get(i).map(|c| c.clone().into()),
        }
    }

    fn get(&self, i: usize, j: usize) -> Option<&LiteralInstance> {
        match self {
            MatInstance::Mat2x2(m) => m.components.get(i).and_then(|c| c.components.get(j)),
            MatInstance::Mat2x3(m) => m.components.get(i).and_then(|c| c.components.get(j)),
            MatInstance::Mat2x4(m) => m.components.get(i).and_then(|c| c.components.get(j)),
            MatInstance::Mat3x2(m) => m.components.get(i).and_then(|c| c.components.get(j)),
            MatInstance::Mat3x3(m) => m.components.get(i).and_then(|c| c.components.get(j)),
            MatInstance::Mat3x4(m) => m.components.get(i).and_then(|c| c.components.get(j)),
            MatInstance::Mat4x2(m) => m.components.get(i).and_then(|c| c.components.get(j)),
            MatInstance::Mat4x3(m) => m.components.get(i).and_then(|c| c.components.get(j)),
            MatInstance::Mat4x4(m) => m.components.get(i).and_then(|c| c.components.get(j)),
        }
    }

    fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = &LiteralInstance> + 'a> {
        match self {
            MatInstance::Mat2x2(m) => Box::new(m.iter()),
            MatInstance::Mat2x3(m) => Box::new(m.iter()),
            MatInstance::Mat2x4(m) => Box::new(m.iter()),
            MatInstance::Mat3x2(m) => Box::new(m.iter()),
            MatInstance::Mat3x3(m) => Box::new(m.iter()),
            MatInstance::Mat3x4(m) => Box::new(m.iter()),
            MatInstance::Mat4x2(m) => Box::new(m.iter()),
            MatInstance::Mat4x3(m) => Box::new(m.iter()),
            MatInstance::Mat4x4(m) => Box::new(m.iter()),
        }
    }
    fn iter_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &mut LiteralInstance> + 'a> {
        match self {
            MatInstance::Mat2x2(m) => Box::new(m.iter_mut()),
            MatInstance::Mat2x3(m) => Box::new(m.iter_mut()),
            MatInstance::Mat2x4(m) => Box::new(m.iter_mut()),
            MatInstance::Mat3x2(m) => Box::new(m.iter_mut()),
            MatInstance::Mat3x3(m) => Box::new(m.iter_mut()),
            MatInstance::Mat3x4(m) => Box::new(m.iter_mut()),
            MatInstance::Mat4x2(m) => Box::new(m.iter_mut()),
            MatInstance::Mat4x3(m) => Box::new(m.iter_mut()),
            MatInstance::Mat4x4(m) => Box::new(m.iter_mut()),
        }
    }
}

// TODO
// impl From<Vec<VecInstance>> for MatInstance {
//     fn from(components: Vec<VecInstance>) -> Self {
//         match (components.len(), components[0].n()) {
//             (2, 2) => Self::Mat2x2(MatInner { components: components.into_iter().map(|v| match v {
//                 VecInstance::Vec2(v) => v,
//                 _ => panic!("invalid VecInstance")
//             }).collect_vec() })
//             _ => panic!("number of MatInstance components must be 2, 3, 4"),
//         }
//     }
// }

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PtrInstance {
    ty: Type,
    address: Address,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RefInstance {
    ty: Type,
    address: Address,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MemView {
    Whole,
    Member(String, Box<MemView>),
    Index(usize, Box<MemView>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Address {
    ptr: usize,
    view: MemView,
}

impl MemView {
    fn append_member(&mut self, m: String) {
        match self {
            MemView::Whole => *self = MemView::Member(m, Box::new(MemView::Whole)),
            MemView::Member(_, v) | MemView::Index(_, v) => v.append_member(m),
        }
    }
    fn append_index(&mut self, i: usize) {
        match self {
            MemView::Whole => *self = MemView::Index(i, Box::new(MemView::Whole)),
            MemView::Member(_, v) | MemView::Index(_, v) => v.append_index(i),
        }
    }
}

impl RefInstance {
    fn value<'a>(&'a self, ctx: &'a Context) -> Result<&'a Instance, ConstEvalError> {
        fn rec_view<'a>(inst: &'a Instance, view: &'a MemView) -> Option<&'a Instance> {
            match view {
                MemView::Whole => Some(inst),
                MemView::Member(m, v) => match inst {
                    Instance::Struct(s) => {
                        let inst = s.components.get(m)?;
                        rec_view(inst, v)
                    }
                    _ => None,
                },
                MemView::Index(i, v) => match inst {
                    Instance::Array(a) => {
                        let inst = a.components.get(*i)?;
                        rec_view(inst, v)
                    }
                    _ => None,
                },
            }
        }

        let inst = ctx
            .memory
            .get(self.address.ptr)
            .ok_or_else(|| ConstEvalError::InvalidRef(self.address.clone()))?;
        rec_view(inst, &self.address.view)
            .ok_or_else(|| ConstEvalError::InvalidRef(self.address.clone()))
    }
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
