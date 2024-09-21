use std::collections::HashMap;

use derive_more::derive::From;
use wgsl_parse::syntax;

use super::{ConstEvalError, Context, Type};
use crate::syntax_util::struct_decl;

#[derive(Clone, Debug, From, PartialEq)]
pub enum Instance {
    Literal(LiteralInstance),
    Struct(StructInstance),
    Array(ArrayInstance),
    Vec(VecInstance),
    Mat(MatInstance),
    Ptr(PtrInstance),
    Ref(RefInstance),
    Type(Type),
    Void,
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
    pub name: String,
    pub components: HashMap<String, Instance>,
}

impl StructInstance {
    pub fn name(&self) -> &str {
        &self.name
    }
    pub fn declaration<'s>(&self, wesl: &'s syntax::TranslationUnit) -> Option<&'s syntax::Struct> {
        struct_decl(&self.name, wesl)
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct ArrayInstance {
    pub components: Vec<Instance>,
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
    pub components: Vec<LiteralInstance>,
}

pub type Vec2 = VecInner<2>;
pub type Vec3 = VecInner<3>;
pub type Vec4 = VecInner<4>;

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

#[derive(Clone, Debug, PartialEq, From)]
pub enum VecInstance {
    Vec2(Vec2),
    Vec3(Vec3),
    Vec4(Vec4),
}

impl VecInstance {
    pub fn new(components: Vec<LiteralInstance>) -> Self {
        match components.len() {
            2 => Self::Vec2(VecInner::new(components)),
            3 => Self::Vec3(VecInner::new(components)),
            4 => Self::Vec4(VecInner::new(components)),
            _ => panic!("VecInstance must have 2, 3 or 4 commponents"),
        }
    }

    pub fn n(&self) -> u8 {
        match self {
            VecInstance::Vec2(_) => 2,
            VecInstance::Vec3(_) => 3,
            VecInstance::Vec4(_) => 4,
        }
    }

    pub fn get(&self, i: usize) -> Option<&LiteralInstance> {
        match self {
            VecInstance::Vec2(v) => v.components.get(i),
            VecInstance::Vec3(v) => v.components.get(i),
            VecInstance::Vec4(v) => v.components.get(i),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &LiteralInstance> {
        match self {
            Self::Vec2(v) => v.components.iter(),
            Self::Vec3(v) => v.components.iter(),
            Self::Vec4(v) => v.components.iter(),
        }
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut LiteralInstance> {
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
    pub components: Vec<VecInner<R>>,
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

impl<const C: usize, const R: usize> MatInner<C, R> {
    pub fn new(components: Vec<VecInner<R>>) -> Self {
        Self { components }
    }
    pub fn new_with_value(val: &LiteralInstance) -> Self {
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

impl<const C: usize, const R: usize> From<Vec<VecInner<R>>> for MatInner<C, R> {
    fn from(components: Vec<VecInner<R>>) -> Self {
        assert!(components.len() == C);
        Self { components }
    }
}

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

impl MatInstance {
    pub fn r(&self) -> u8 {
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
    pub fn c(&self) -> u8 {
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

    pub fn col(&self, i: usize) -> Option<VecInstance> {
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

    pub fn get(&self, i: usize, j: usize) -> Option<&LiteralInstance> {
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

    pub fn iter<'a>(&'a self) -> Box<dyn Iterator<Item = &LiteralInstance> + 'a> {
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
    pub fn iter_mut<'a>(&'a mut self) -> Box<dyn Iterator<Item = &mut LiteralInstance> + 'a> {
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PtrInstance {
    pub ty: Type,
    pub address: Address,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RefInstance {
    pub ty: Type,
    pub address: Address,
}

impl RefInstance {
    pub fn value<'a>(&'a self, ctx: &'a Context) -> Result<&'a Instance, ConstEvalError> {
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MemView {
    Whole,
    Member(String, Box<MemView>),
    Index(usize, Box<MemView>),
}

impl MemView {
    pub fn append_member(&mut self, m: String) {
        match self {
            MemView::Whole => *self = MemView::Member(m, Box::new(MemView::Whole)),
            MemView::Member(_, v) | MemView::Index(_, v) => v.append_member(m),
        }
    }
    pub fn append_index(&mut self, i: usize) {
        match self {
            MemView::Whole => *self = MemView::Index(i, Box::new(MemView::Whole)),
            MemView::Member(_, v) | MemView::Index(_, v) => v.append_index(i),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Address {
    pub ptr: usize,
    pub view: MemView,
}

impl Address {
    pub fn new(ptr: usize) -> Self {
        Self {
            ptr,
            view: MemView::Whole,
        }
    }
}
