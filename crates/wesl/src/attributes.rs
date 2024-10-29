use wgsl_parse::syntax::*;
use wgsl_parse_macros::query_mut;

pub(crate) fn query_attributes(
    wesl: &mut TranslationUnit,
) -> impl Iterator<Item = &mut Vec<Attribute>> {
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
                body.{ attributes, statements.[].(statement_query_attributes) }
            },
            GlobalDeclaration::ConstAssert.attributes,
        }
    })
}

pub(crate) fn statement_query_attributes(
    stat: &mut StatementNode,
) -> impl Iterator<Item = &mut Vec<Attribute>> {
    let stat = stat.node_mut();
    query_mut!(stat.{
        Statement::Compound.{ attributes, statements.[].(statement_query_attributes) },
        Statement::If.{
            attributes,
            else_if_clauses.[].body.statements.[].(statement_query_attributes),
            else_clause.[].body.statements.[].(statement_query_attributes),
        },
        Statement::Switch.{
            attributes,
            clauses.[].{ attributes, body.statements.[].(statement_query_attributes) },
        },
        Statement::Loop.{
            attributes,
            body.statements.[].(statement_query_attributes),
            continuing.[].{
                attributes,
                body.statements.[].(statement_query_attributes),
                break_if.[].{ attributes }
            },
        },
        Statement::For.{
            attributes,
            body.statements.[].(statement_query_attributes),
        },
        Statement::While.{
            attributes,
            body.statements.[].(statement_query_attributes),
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
