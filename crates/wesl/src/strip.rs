use std::collections::HashSet;

use wgsl_parse::syntax::TranslationUnit;

use crate::syntax_util::{decl_name, IterUses};

/// removes unused declarations.
pub fn strip(wgsl: &mut TranslationUnit, keep: &[String]) {
    let mut keep: HashSet<String> = HashSet::from_iter(keep.iter().cloned());
    let mut next_keep: HashSet<String> = HashSet::new();

    loop {
        for decl in &mut wgsl.global_declarations {
            if let Some(name) = decl_name(decl) {
                if keep.contains(name) {
                    let used = decl
                        .uses_mut()
                        .map(|name| name.to_string())
                        .collect::<HashSet<String>>();
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
        if let Some(name) = decl_name(decl) {
            keep.contains(name)
        } else {
            true
        }
    })
}
