use std::ops::Deref;

pub type Span = std::ops::Range<usize>;

pub(crate) type S<T> = Spanned<T>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Spanned<T>(T, Span);

impl<T> Spanned<T> {
    pub fn new(t: T, s: Span) -> Self {
        Self(t, s)
    }

    pub fn span(&self) -> Span {
        self.1.clone()
    }

    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> AsRef<T> for Spanned<T> {
    fn as_ref(&self) -> &T {
        &self.0
    }
}

impl<T> From<Spanned<T>> for Spanned<Box<T>> {
    fn from(value: Spanned<T>) -> Self {
        Spanned(value.0.into(), value.1)
    }
}
