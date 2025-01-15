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
            if ty.template_args.is_none() && ty.ident == alias.ident {
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

/// Eliminate all const-declarations.
///
/// Replace usages of the const-declaration with its expression.
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

    // TODO
    // while let Some(decl) = take_next_const(wesl) {
    //     fn rec(expr: &mut Expression, name: &str, repl: &Expression) {
    //         match expr {
    //             Expression::TypeOrIdentifier(ty)
    //                 if ty.template_args.is_none() && ty.name == name =>
    //             {
    //                 *expr = repl.clone();
    //                 return;
    //             }
    //             _ => {
    //                 for expr in expr.exprs_mut() {
    //                     rec(expr, name, repl);
    //                 }
    //             }
    //         }
    //     }

    //     for ty in wesl.exprs_mut() {
    //         if let Some(init) = &decl.initializer {
    //             rec(ty, &decl.name, init)
    //         }
    //     }
    // }
}
