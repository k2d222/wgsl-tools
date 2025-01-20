use crate::{sourcemap::NoSourceMap, Error, SourceMap};

#[cfg(feature = "attributes")]
use crate::attributes::query_attrs;

use wgsl_parse::syntax::*;

/// Like [`lower`], but provides better error diagnostics.
pub fn lower_sourcemap(
    wesl: &mut TranslationUnit,
    _sourcemap: &impl SourceMap,
) -> Result<(), Error> {
    #[cfg(feature = "imports")]
    wesl.imports.clear();

    #[cfg(feature = "attributes")]
    for attrs in query_attrs(wesl) {
        attrs.retain(|attr| {
            !matches!(attr, 
            Attribute::Custom(CustomAttribute { name, .. }) if name == "generic")
        })
    }

    remove_type_aliases(wesl);
    remove_global_consts(wesl);

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

/// Eliminate all type aliases.
/// Naga doesn't like this: `alias T = u32; vec<T>`
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

    while let Some(mut alias) = take_next_alias(wesl) {
        // we rename the alias and all references to its type expression,
        // and drop the alias declaration.
        alias.ident.rename(format!("{}", alias.ty));
    }
}

/// Eliminate all const-declarations.
///
/// Replace usages of the const-declaration with its expression.
///
/// # Panics
/// panics if the const-declaration is ill-formed, i.e. has no initializer.
pub fn remove_global_consts(wesl: &mut TranslationUnit) {
    let take_next_const = |wesl: &mut TranslationUnit| {
        let index = wesl.global_declarations.iter().position(|decl| {
            matches!(
                decl,
                GlobalDeclaration::Declaration(Declaration {
                    kind: DeclarationKind::Const,
                    ..
                })
            )
        });
        index.map(|index| {
            let decl = wesl.global_declarations.swap_remove(index);
            match decl {
                GlobalDeclaration::Declaration(d) => d,
                _ => unreachable!(),
            }
        })
    };

    while let Some(mut decl) = take_next_const(wesl) {
        // we rename the const and all references to its expression in parentheses,
        // and drop the const declaration.
        decl.ident
            .rename(format!("({})", decl.initializer.unwrap()));
    }
}
