use std::ops::Range;

use derive_more::derive::{AsMut, AsRef, Deref, DerefMut, From};

pub type Id = u32;

#[derive(Default, Clone, Debug, PartialEq, Eq, Deref, DerefMut, AsRef, AsMut, From)]
pub struct Span(Range<usize>);

impl Span {
    pub fn new(range: Range<usize>) -> Self {
        Self(range)
    }
    pub fn range(&self) -> Range<usize> {
        self.0.clone()
    }
    pub fn extend(&self, other: &Span) -> Self {
        Self(self.start..other.end)
    }
}

#[derive(Default, Clone, Debug, PartialEq, Deref, DerefMut, AsRef, AsMut, From)]
pub struct Spanned<T> {
    pub span: Span,
    #[deref(forward)]
    #[deref_mut(forward)]
    #[as_ref(T)]
    #[as_mut(T)]
    #[from(T)]
    pub node: Box<T>,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self::new_boxed(Box::new(node), span)
    }
    pub fn new_boxed(node: Box<T>, span: Span) -> Self {
        Self { span, node }
    }
    pub fn span(&self) -> &Span {
        &self.span
    }
    pub fn node(&self) -> &T {
        self
    }
    pub fn node_mut(&mut self) -> &mut T {
        self
    }
    pub fn into_inner(self) -> T {
        *self.node
    }
}

impl<T> From<Spanned<T>> for (Span, T) {
    fn from(value: Spanned<T>) -> Self {
        (value.span, *value.node)
    }
}
