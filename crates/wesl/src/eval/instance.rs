use core::panic;
use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    ops::Deref,
    rc::Rc,
};

use derive_more::derive::From;
use half::f16;
use itertools::Itertools;

use crate::eval::Ty;

use super::{EvalError, Type};

type E = EvalError;

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

impl Instance {
    pub fn view<'a>(&'a self, view: &'a MemView) -> Result<&Instance, E> {
        match view {
            MemView::Whole => Ok(self),
            MemView::Member(m, v) => match self {
                Instance::Struct(s) => {
                    let inst = s
                        .members
                        .get(m)
                        .ok_or_else(|| E::Component(s.ty(), m.clone()))?;
                    inst.view(v)
                }
                _ => Err(E::Component(self.ty(), m.clone())),
            },
            MemView::Index(i, view) => match self {
                Instance::Array(a) => {
                    let inst = a
                        .components
                        .get(*i)
                        .ok_or_else(|| E::OutOfBounds(*i, a.ty(), a.n()))?;
                    inst.view(view)
                }
                _ => Err(E::NotIndexable(self.ty())),
            },
        }
    }
    pub fn view_mut<'a>(&'a mut self, view: &'a MemView) -> Result<&mut Instance, E> {
        let ty = self.ty();
        match view {
            MemView::Whole => Ok(self),
            MemView::Member(m, v) => match self {
                Instance::Struct(s) => {
                    let inst = s
                        .members
                        .get_mut(m)
                        .ok_or_else(|| E::Component(ty, m.clone()))?;
                    inst.view_mut(v)
                }
                _ => Err(E::Component(ty, m.clone())),
            },
            MemView::Index(i, v) => match self {
                Instance::Array(a) => {
                    let n = a.n();
                    let inst = a
                        .components
                        .get_mut(*i)
                        .ok_or_else(|| E::OutOfBounds(*i, ty, n))?;
                    inst.view_mut(v)
                }
                _ => Err(E::NotIndexable(ty)),
            },
        }
    }
    pub fn len(&self) -> usize {
        match self {
            Instance::Array(a) => a.n(),
            Instance::Vec(v) => v.n() as usize,
            Instance::Mat(m) => m.c() as usize,
            _ => 0,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, From)]
pub enum LiteralInstance {
    Bool(bool),
    AbstractInt(i64),
    AbstractFloat(f64),
    I32(i32),
    U32(u32),
    F32(f32),
    F16(f16),
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructInstance {
    pub name: String,
    pub members: HashMap<String, Instance>,
}

impl StructInstance {
    pub fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub struct ArrayInstance {
    pub components: Vec<Instance>,
}

impl ArrayInstance {
    ///
    /// # Panics
    /// panics if the components are not all the same type
    pub(crate) fn new(components: Vec<Instance>) -> Self {
        assert!(components.iter().map(|c| c.ty()).all_equal());
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
    pub(crate) fn new(components: Vec<LiteralInstance>) -> Self {
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
    /// # Panics
    /// * if the components length is not [2, 3, 4]
    /// * if the components are not all the same type
    pub(crate) fn new(components: Vec<LiteralInstance>) -> Self {
        assert!(components.iter().map(|c| c.ty()).all_equal());
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
    pub(crate) fn new(components: Vec<VecInner<R>>) -> Self {
        assert!(components.len() == C);
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
    /// # Panics
    /// * if the number of columns is not [2, 3, 4]
    /// * if the colums don't have the same number of rows
    /// * if the number of rows is not [2, 3, 4]
    pub(crate) fn new(components: Vec<Vec<LiteralInstance>>) -> Self {
        assert!(
            components.iter().map(|c| c.len()).all_equal(),
            "MatInstance columns must have the same number for rows"
        );
        macro_rules! make_mat {
            ($vec:expr, $mat:ident) => {{
                let components = components
                    .into_iter()
                    .map(|c| VecInner::<$vec>::new(c))
                    .collect_vec();
                Self::$mat(MatInner::new(components))
            }};
        }
        match components.len() {
            2 => match components[0].len() {
                2 => make_mat!(2, Mat2x2),
                3 => make_mat!(3, Mat2x3),
                4 => make_mat!(4, Mat2x4),
                _ => panic!("number of MatInstance rows must be 2, 3 or 4"),
            },
            3 => match components[0].len() {
                2 => make_mat!(2, Mat3x2),
                3 => make_mat!(3, Mat3x3),
                4 => make_mat!(4, Mat3x4),
                _ => panic!("number of MatInstance rows must be 2, 3 or 4"),
            },
            4 => match components[0].len() {
                2 => make_mat!(2, Mat4x2),
                3 => make_mat!(3, Mat4x3),
                4 => make_mat!(4, Mat4x4),
                _ => panic!("number of MatInstance rows must be 2, 3 or 4"),
            },
            _ => panic!("number of MatInstance columns must be 2, 3 or 4"),
        }
    }

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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AccessMode {
    Read,
    Write,
    ReadWrite,
}
impl AccessMode {
    pub fn is_read(&self) -> bool {
        matches!(self, Self::Read | Self::ReadWrite)
    }
    pub fn is_write(&self) -> bool {
        matches!(self, Self::Write | Self::ReadWrite)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PtrInstance {
    pub ty: Type,
    pub view: MemView,
    pub ptr: Rc<RefCell<Instance>>,
    pub access: AccessMode,
}

impl From<RefInstance> for PtrInstance {
    fn from(r: RefInstance) -> Self {
        Self {
            ty: r.ty.clone(),
            view: r.view.clone(),
            ptr: r.ptr.clone(),
            access: r.access,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct RefInstance {
    pub ty: Type,
    pub view: MemView,
    pub ptr: Rc<RefCell<Instance>>,
    pub access: AccessMode,
}

impl RefInstance {
    pub fn new(ptr: Rc<RefCell<Instance>>, access: AccessMode) -> Self {
        let ty = ptr.borrow().ty();
        Self {
            ty,
            view: MemView::Whole,
            ptr,
            access,
        }
    }
}

impl From<PtrInstance> for RefInstance {
    fn from(p: PtrInstance) -> Self {
        Self {
            ty: p.ty.clone(),
            view: p.view.clone(),
            ptr: p.ptr.clone(),
            access: p.access,
        }
    }
}

pub struct RefView<'a> {
    r: Ref<'a, Instance>,
    v: &'a MemView,
}

impl<'a> Deref for RefView<'a> {
    type Target = Instance;

    fn deref(&self) -> &Self::Target {
        self.r.view(self.v).expect("invalid reference")
    }
}

pub struct RefViewMut<'a> {
    r: RefMut<'a, Instance>,
    v: &'a MemView,
}
impl<'a> RefViewMut<'a> {
    pub fn write(&mut self, value: Instance) -> Result<Instance, E> {
        let view = self.r.view_mut(self.v).expect("invalid reference");
        if value.ty() != view.ty() {
            return Err(E::WriteRefType(value.ty(), view.ty()));
        }
        let old = std::mem::replace(view, value);
        Ok(old)
    }
}
impl<'a> Deref for RefViewMut<'a> {
    type Target = Instance;

    fn deref(&self) -> &Self::Target {
        self.r.view(self.v).expect("invalid reference")
    }
}

impl RefInstance {
    pub fn view_member(&self, member: String) -> Result<Self, E> {
        if !self.access.is_read() {
            return Err(E::NotRead);
        }
        let mut view = self.view.clone();
        view.append_member(member);
        let inst = self.ptr.borrow();
        let inst = inst.view(&self.view)?;
        Ok(Self {
            ty: inst.ty(),
            view,
            ptr: self.ptr.clone(),
            access: self.access,
        })
    }
    pub fn view_index(&self, index: usize) -> Result<Self, E> {
        if !self.access.is_read() {
            return Err(E::NotRead);
        }
        let mut view = self.view.clone();
        view.append_index(index);
        let inst = self.ptr.borrow();
        let inst = inst.view(&self.view)?;
        Ok(Self {
            ty: inst.ty(),
            view,
            ptr: self.ptr.clone(),
            access: self.access,
        })
    }

    pub fn read<'a>(&'a self) -> Result<RefView<'a>, E> {
        if !self.access.is_read() {
            Err(E::NotRead)
        } else {
            let r = self.ptr.borrow();
            Ok(RefView { r, v: &self.view })
        }
    }

    pub fn write(&mut self, value: Instance) -> Result<Instance, E> {
        if !self.access.is_write() {
            Err(E::NotWrite)
        } else {
            let mut r = self.ptr.borrow_mut();
            let view = r.view_mut(&self.view).expect("invalid reference");
            if value.ty() != view.ty() {
                return Err(E::WriteRefType(value.ty(), view.ty()));
            }
            let old = std::mem::replace(view, value);
            Ok(old)
        }
    }

    pub fn read_write<'a>(&'a mut self) -> Result<RefViewMut<'a>, E> {
        if !self.access.is_write() {
            Err(E::NotReadWrite)
        } else {
            let r = self.ptr.borrow_mut();
            Ok(RefViewMut { r, v: &self.view })
        }
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

// #[derive(Clone, Debug, PartialEq, Eq)]
// pub struct Address {
//     pub ptr: usize,
//     pub view: MemView,
// }

// impl Address {
//     pub fn new(ptr: usize) -> Self {
//         Self {
//             ptr,
//             view: MemView::Whole,
//         }
//     }
// }
