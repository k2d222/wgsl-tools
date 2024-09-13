use std::collections::HashMap;

use thiserror::Error;
use wgsl_parse::syntax::TranslationUnit;

#[derive(Clone, Debug, Error)]
pub enum Error {
    #[error("invalid feature flag: `{0}`")]
    InvalidFeatureFlag(String),
}

pub fn run(module: &mut TranslationUnit, features: &HashMap<String, bool>) -> Result<(), Error> {
    Ok(())
}
