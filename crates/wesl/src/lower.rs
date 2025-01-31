use crate::{Diagnostic, Error};

#[cfg(feature = "attributes")]
use crate::attributes::query_attrs;

use wgsl_parse::syntax::*;

/// Performs conversions on the final syntax tree to make it more compatible with naga,
/// catch errors early and perform optimizations.
pub fn lower(wesl: &mut TranslationUnit, _keep: &[String]) -> Result<(), Error> {
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
    // remove_abstract_types(wesl);

    #[cfg(feature = "eval")]
    {
        use crate::eval::{make_explicit_conversions, mark_functions_const, Context, Lower};
        mark_functions_const(wesl);
        let wesl2 = wesl.clone();
        let mut ctx = Context::new(&wesl2);
        make_explicit_conversions(wesl, &ctx);
        wesl.lower(&mut ctx)
            .map_err(|e| Diagnostic::from(e).with_ctx(&ctx))?;

        // lowering makes const function unused, so we remove them if not in keep list.
        wesl.global_declarations.retain_mut(|decl| {
            if let GlobalDeclaration::Function(decl) = decl {
                if decl.attributes.contains(&Attribute::Const)
                    && !_keep.contains(&*decl.ident.name())
                {
                    false
                } else {
                    decl.attributes.retain(|attr| *attr != Attribute::Const);
                    true
                }
            } else {
                true
            }
        });
    }
    Ok(())
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

// /// Eliminate most uses of AbstractInt and AbstractFloat.
// /// Naga doesn't like automatic type conversions in several places:
// /// * return statements
// /// * function calls
// /// * shift when lhs is abstract
// /// * ...probably more?
// trait MakeExplicit {
//     fn make_explicit(&mut self, scope: &mut Scope);
// }

// struct Scope {
//     // TODO: copy on write
//     stack: Vec<HashMap<String, TypeExpression>>,
// }

// impl Scope {
//     fn new() -> Self {
//         Self {
//             stack: vec![Default::default()],
//         }
//     }
//     pub fn push(&mut self) {
//         self.stack.push(Default::default())
//     }
//     pub fn pop(&mut self) {
//         self.stack.pop().expect("failed to pop scope");
//     }
//     pub fn insert(&mut self, name: String, ty: TypeExpression) {
//         self.stack.last_mut().unwrap().insert(name, ty);
//     }
//     pub fn contains(&self, name: &str) -> bool {
//         self.stack
//             .iter()
//             .rev()
//             .any(|scope| scope.contains_key(name))
//     }
//     pub fn get(&self, name: &str) -> Option<&TypeExpression> {
//         self.stack.iter().rev().find_map(|scope| scope.get(name))
//     }
// }

// impl MakeExplicit for TranslationUnit {
//     fn make_explicit(&mut self, scope: &mut Scope) {
//         for decl in &mut self.global_declarations {
//             match decl {
//                 GlobalDeclaration::Void => todo!(),
//                 GlobalDeclaration::Declaration(decl) => decl.make_explicit(),
//                 GlobalDeclaration::TypeAlias(decl) => decl.make_explicit(),
//                 GlobalDeclaration::Struct(decl) => decl.make_explicit(),
//                 GlobalDeclaration::Function(decl) => decl.make_explicit(),
//                 GlobalDeclaration::ConstAssert(decl) => decl.make_explicit(),
//             }
//         }
//     }
// }
