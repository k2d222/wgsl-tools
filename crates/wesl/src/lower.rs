use crate::{attributes::query_attributes, eval::Lower, Context, Error, SourceMap};
use wgsl_parse::syntax::*;

pub(crate) fn lower_sourcemap(
    wesl: &mut TranslationUnit,
    sourcemap: &impl SourceMap,
) -> Result<(), Error> {
    if cfg!(feature = "imports") {
        wesl.imports.clear();
    }
    if cfg!(feature = "attributes") {
        for attrs in query_attributes(wesl) {
            attrs.retain(|attr| match attr.name.as_str() {
                "generic" => false,
                _ => true,
            })
        }
    }
    if cfg!(feature = "eval") {
        let mut ctx = Context::new(wesl);
        let mut new_wesl = wesl.clone();
        new_wesl
            .lower(&mut ctx)
            .map_err(|e| Error::from(e).to_diagnostic(&ctx, sourcemap))?;
        *wesl = new_wesl;
    }
    Ok(())
}
