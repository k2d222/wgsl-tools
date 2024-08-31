use crate::resolve::{ImportError, Module, Resolver};

impl<R: Resolver> Module<R> {
    pub fn mangle(&mut self) -> Result<(), ImportError> {
        Ok(())
    }
}
