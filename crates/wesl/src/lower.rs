use crate::{sourcemap::NoSourceMap, syntax_util::IterUses, Error, SourceMap};

#[cfg(feature = "attributes")]
use crate::attributes::query_attributes;

use wgsl_parse::syntax::*;

/// Like [`lower`], but provides better error diagnostics.
pub fn lower_sourcemap(
    wesl: &mut TranslationUnit,
    _sourcemap: &impl SourceMap,
) -> Result<(), Error> {
    #[cfg(feature = "imports")]
    wesl.imports.clear();

    #[cfg(feature = "attributes")]
    for attrs in query_attributes(wesl) {
        attrs.retain(|attr| match attr {
            Attribute::Custom(CustomAttribute { name, .. }) if name == "generic" => false,
            _ => true,
        })
    }

    remove_type_aliases(wesl);

    #[cfg(feature = "eval")]
    {
        // TODO
        // let mut ctx = Context::new(wesl);
        // let mut new_wesl = wesl.clone();
        // new_wesl
        //     .lower(&mut ctx)
        //     .map_err(|e| Error::from(e).to_diagnostic(&ctx, sourcemap))?;
        // *wesl = new_wesl;
    }
    Ok(())
}

/// Performs conversions on the final syntax tree to make it more compatible with naga,
/// catch errors early and perform optimizations.
pub fn lower(wesl: &mut TranslationUnit) -> Result<(), Error> {
    lower_sourcemap(wesl, &NoSourceMap)
}

// Eliminate all type aliases.
// Naga doesn't like this: `alias T = u32; vec<T>`
pub fn remove_type_aliases(wesl: &mut TranslationUnit) {
    let take_next_alias = |wesl: &mut TranslationUnit| {
        let index = wesl
            .global_declarations
            .iter()
            .position(|decl| matches!(decl, GlobalDeclaration::TypeAlias(_)));
        index.map(|index| {
            let decl = wesl.global_declarations.swap_remove(index);
            match decl {
                GlobalDeclaration::TypeAlias(alias) => alias,
                _ => unreachable!(),
            }
        })
    };

    while let Some(alias) = take_next_alias(wesl) {
        fn rec(ty: &mut TypeExpression, alias: &TypeAlias) {
            if ty.template_args.is_none() && ty.name == alias.name {
                *ty = alias.ty.clone();
                return;
            }
            for ty in ty.uses_mut() {
                rec(ty, alias);
            }
        }

        for ty in wesl.uses_mut() {
            rec(ty, &alias)
        }
    }
}
