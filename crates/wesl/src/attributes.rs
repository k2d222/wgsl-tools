use wesl_macros::query_mut;
use wgsl_parse::syntax::*;

pub(crate) fn query_attrs(wesl: &mut TranslationUnit) -> impl Iterator<Item = &mut Vec<Attribute>> {
    query_mut!(wesl.{
        imports.[].attributes,
        global_directives.[].{
            GlobalDirective::Diagnostic.attributes,
            GlobalDirective::Enable.attributes,
            GlobalDirective::Requires.attributes,
        },
        global_declarations.[].{
            GlobalDeclaration::Declaration.attributes,
            GlobalDeclaration::TypeAlias.attributes,
            GlobalDeclaration::Struct.{
                attributes,
                members.[].attributes,
            },
            GlobalDeclaration::Function.{
                attributes,
                parameters.[].attributes,
                body.{ attributes, statements.[].(stat_query_attrs) }
            },
            GlobalDeclaration::ConstAssert.attributes,
        }
    })
}

pub(crate) fn stat_query_attrs(
    stat: &mut StatementNode,
) -> impl Iterator<Item = &mut Vec<Attribute>> {
    let stat = stat.node_mut();
    query_mut!(stat.{
        Statement::Compound.{ attributes, statements.[].(stat_query_attrs) },
        Statement::If.{
            attributes,
            else_if_clauses.[].body.statements.[].(stat_query_attrs),
            else_clause.[].body.statements.[].(stat_query_attrs),
        },
        Statement::Switch.{
            attributes,
            clauses.[].{ attributes, body.statements.[].(stat_query_attrs) },
        },
        Statement::Loop.{
            attributes,
            body.statements.[].(stat_query_attrs),
            continuing.[].{
                attributes,
                body.statements.[].(stat_query_attrs),
                break_if.[].{ attributes }
            },
        },
        Statement::For.{
            attributes,
            body.statements.[].(stat_query_attrs),
        },
        Statement::While.{
            attributes,
            body.statements.[].(stat_query_attrs),
        },
        Statement::Break.attributes,
        Statement::Continue.attributes,
        Statement::Return.attributes,
        Statement::Discard.attributes,
        Statement::FunctionCall.attributes,
        Statement::ConstAssert.attributes,
        Statement::Declaration.attributes,
    })
}
