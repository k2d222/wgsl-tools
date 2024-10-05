use crate::{
    attributes::query_attributes,
    eval::{Lower, SyntaxUtil},
    sourcemap::NoSourceMap,
    syntax_util::IterUses,
    Context, Error, SourceMap,
};
use wgsl_parse::syntax::*;

/// Like [`lower`], but provides better error diagnostics.
pub fn lower_sourcemap(
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
    {
        // eliminate all type aliases.
        // Naga doesn't like this: `alias T = u32; vec<T>`
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
                let ty_ = ty.clone();
                for ty in ty.uses_mut() {
                    // println!("{alias}: {ty_} -> {ty}");
                    rec(ty, alias);
                }
            }

            for ty in wesl.uses_mut() {
                rec(ty, &alias)
            }
        }
    }
    if cfg!(feature = "eval") {
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
