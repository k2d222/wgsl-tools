use std::collections::HashSet;

use wgsl_parse::{
    syntax::{Ident, TranslationUnit, TypeExpression},
    visit::Visit,
};

/// removes unused declarations.
pub fn strip_except(wgsl: &mut TranslationUnit, keep: &[String]) {
    let global_idents = wgsl
        .global_declarations
        .iter()
        .filter_map(|decl| decl.ident().cloned())
        .collect::<HashSet<_>>();

    let mut keep: HashSet<Ident> = HashSet::from_iter(
        keep.iter()
            .filter_map(|name| global_idents.iter().find(|ident| ident.name().eq(name)))
            .cloned(),
    );
    let mut next_keep: HashSet<Ident> = HashSet::new();

    loop {
        for decl in &mut wgsl.global_declarations {
            if let Some(ident) = decl.ident() {
                if keep.contains(ident) {
                    let used = Visit::<TypeExpression>::visit(decl)
                        .filter(|ty| global_idents.contains(&ty.ident))
                        .map(|ty| ty.ident.clone())
                        .collect::<HashSet<_>>();
                    next_keep.extend(used);
                }
            }
        }

        next_keep.retain(|name| !keep.contains(name));
        keep.extend(next_keep.iter().cloned());

        if next_keep.len() == 0 {
            break;
        }
    }

    wgsl.global_declarations.retain(|decl| {
        if let Some(ident) = decl.ident() {
            keep.contains(ident)
        } else {
            true
        }
    })
}
