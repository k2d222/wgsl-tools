use super::syntax_spanned::*;
use core::fmt;
use std::{
    fmt::{Display, Formatter},
    ops::Deref,
};

use itertools::Itertools;

use super::span::Spanned;

struct Indent<T: Display>(pub T);

impl<T: Display> Display for Indent<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let indent = "    ";
        let inner_display = self.0.to_string();
        let fmt = inner_display
            .lines()
            .map(|l| format!("{}{}", indent, l))
            .format("\n");
        write!(f, "{}", fmt)?;
        Ok(())
    }
}

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.as_ref().fmt(f)
    }
}

// impl<'s, T> Display for Print<'s, &'s Spanned<T>>
// where
//     Print<'s, &'s T>: Display,
// {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         Print::new(&self.0, self.source()).fmt(f)
//     }
// }

impl Display for WithSource<'_, &TranslationUnit> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let directives = self
            .global_directives
            .iter()
            .map(|x| x.with_source(self.source()))
            .format("\n");
        let declarations = self
            .global_declarations
            .iter()
            .map(|x| x.with_source(self.source()))
            .format("\n\n");
        writeln!(f, "{directives}\n\n{declarations}")
    }
}

impl Display for WithSource<'_, &GlobalDirective> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.deref() {
            GlobalDirective::Diagnostic(print) => write!(f, "{}", print.with_source(self.source())),
            GlobalDirective::Enable(print) => write!(f, "{}", print.with_source(self.source())),
            GlobalDirective::Requires(print) => write!(f, "{}", print.with_source(self.source())),
        }
    }
}

impl Display for WithSource<'_, &DiagnosticDirective> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let severity = &self.severity;
        let rule = &self.source()[self.rule_name.clone()];
        writeln!(f, "diagnostic ({severity}, {rule});")
    }
}

// impl Display for DiagnosticSeverity {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         match self {
//             Self::Error => write!(f, "error"),
//             Self::Warning => write!(f, "warning"),
//             Self::Info => write!(f, "info"),
//             Self::Off => write!(f, "off"),
//         }
//     }
// }

impl Display for WithSource<'_, &EnableDirective> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let exts = self
            .extensions
            .iter()
            .map(|span| &self.source()[span.clone()])
            .format(", ");
        writeln!(f, "enable {exts};")
    }
}

impl Display for WithSource<'_, &RequiresDirective> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let exts = self
            .extensions
            .iter()
            .map(|span| &self.source()[span.clone()])
            .format(", ");
        writeln!(f, "requires {exts};")
    }
}

impl Display for WithSource<'_, &GlobalDeclaration> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.deref() {
            GlobalDeclaration::Void => write!(f, ";"),
            GlobalDeclaration::Declaration(print) => {
                write!(f, "{}", print.with_source(self.source()))
            }
            GlobalDeclaration::TypeAlias(print) => {
                write!(f, "{}", print.with_source(self.source()))
            }
            GlobalDeclaration::Struct(print) => write!(f, "{}", print.with_source(self.source())),
            GlobalDeclaration::Function(print) => write!(f, "{}", print.with_source(self.source())),
            GlobalDeclaration::ConstAssert(print) => {
                write!(f, "{}", print.with_source(self.source()))
            }
        }
    }
}

impl Display for WithSource<'_, &Declaration> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, self.source(), false);
        let kind = &self.kind;
        let tplt = fmt_template(&self.template_args, self.source());
        let name = &self.source()[self.name.clone()];
        let typ = self
            .typ
            .as_ref()
            .map(|typ| format!(": {}", typ.with_source(self.source())))
            .unwrap_or_default();
        let init = self
            .initializer
            .as_ref()
            .map(|typ| format!(" = {}", typ.with_source(self.source())))
            .unwrap_or_default();
        write!(f, "{attrs}{kind}{tplt} {name}{typ}{init};")
    }
}

// impl Display for DeclarationKind {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         match self {
//             DeclarationKind::Const => write!(f, "const"),
//             DeclarationKind::Override => write!(f, "override"),
//             DeclarationKind::Let => write!(f, "let"),
//             DeclarationKind::Var => write!(f, "var"),
//         }
//     }
// }

impl Display for WithSource<'_, &TypeAlias> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = &self.source()[self.name.clone()];
        let typ = self.typ.with_source(self.source());
        write!(f, "alias {name} = {typ};")
    }
}

impl Display for WithSource<'_, &Struct> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = &self.source()[self.name.clone()];
        let members = Indent(
            self.members
                .iter()
                .map(|mem| mem.with_source(self.source()))
                .format(",\n"),
        );
        write!(f, "struct {name} {{\n{members}\n}}")
    }
}

impl Display for WithSource<'_, &StructMember> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, self.source(), false);
        let name = &self.source()[self.name.clone()];
        let typ = self.typ.with_source(self.source());
        write!(f, "{attrs}{name}: {typ}")
    }
}

impl Display for WithSource<'_, &Function> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, self.source(), false);
        let name = &self.source()[self.name.clone()];
        let params = self
            .parameters
            .iter()
            .map(|p| p.with_source(self.source()))
            .format(", ");
        let ret_attrs = fmt_attrs(&self.return_attributes, self.source(), true);
        let ret_typ = self
            .return_type
            .as_ref()
            .map(|typ| format!("-> {} ", typ.with_source(self.source())))
            .unwrap_or_default();
        let body = self.body.with_source(self.source());
        write!(f, "{attrs}fn {name}({params}) {ret_attrs}{ret_typ}{body}")
    }
}

impl Display for WithSource<'_, &FormalParameter> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, self.source(), true);
        let name = &self.source()[self.name.clone()];
        let typ = self.typ.with_source(self.source());
        write!(f, "{attrs}{name}: {typ}")
    }
}

impl Display for WithSource<'_, &ConstAssert> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let expr = self.expression.with_source(self.source());
        write!(f, "const_assert {expr};",)
    }
}

impl Display for WithSource<'_, &Attribute> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = &self.source()[self.name.clone()];
        let args = self
            .arguments
            .as_ref()
            .map(|args| {
                format!(
                    "({})",
                    args.iter()
                        .map(|arg| arg.with_source(self.source()))
                        .format(", ")
                )
            })
            .unwrap_or_default();
        write!(f, "@{name}{args}")
    }
}

fn fmt_attrs(attrs: &[Spanned<Attribute>], source: &'_ str, inline: bool) -> String {
    let print = attrs
        .iter()
        .map(|attr| attr.with_source(source))
        .format(" ");
    let suffix = if attrs.is_empty() {
        ""
    } else if inline {
        " "
    } else {
        "\n"
    };
    format!("{print}{suffix}")
}

impl Display for WithSource<'_, &Expression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.deref() {
            Expression::Literal(print) => write!(f, "{}", print),
            Expression::Parenthesized(print) => {
                let expr = print.with_source(self.source());
                write!(f, "({expr})")
            }
            Expression::NamedComponent(print) => write!(f, "{}", print.with_source(self.source())),
            Expression::Indexing(print) => write!(f, "{}", print.with_source(self.source())),
            Expression::Unary(print) => write!(f, "{}", print.with_source(self.source())),
            Expression::Binary(print) => write!(f, "{}", print.with_source(self.source())),
            Expression::FunctionCall(print) => write!(f, "{}", print.with_source(self.source())),
            Expression::Identifier(print) => write!(f, "{}", print.with_source(self.source())),
            Expression::Type(print) => write!(f, "{}", print.with_source(self.source())),
        }
    }
}

// impl Display for LiteralExpression {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         match self {
//             LiteralExpression::True => write!(f, "true"),
//             LiteralExpression::False => write!(f, "false"),
//             LiteralExpression::AbstractInt(num) => write!(f, "{num}"),
//             LiteralExpression::AbstractFloat(num) => write!(f, "{num}"),
//             LiteralExpression::I32(num) => write!(f, "{num}i"),
//             LiteralExpression::U32(num) => write!(f, "{num}u"),
//             LiteralExpression::F32(num) => write!(f, "{num}f"),
//             LiteralExpression::F16(num) => write!(f, "{num}h"),
//         }
//     }
// }

// impl Display for Print<'_, &ParenthesizedExpression> {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         let expr = Print::new(self.print.deref(), self.source());
//         write!(f, "({expr})")
//     }
// }

impl Display for WithSource<'_, &NamedComponentExpression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let base = self.base.with_source(self.source());
        let component = &self.source()[self.component.clone()];
        write!(f, "{base}.{component}")
    }
}

impl Display for WithSource<'_, &IndexingExpression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let base = self.base.with_source(self.source());
        let index = self.index.with_source(self.source());
        write!(f, "{base}[{index}]")
    }
}

impl Display for WithSource<'_, &UnaryExpression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let operator = &self.operator;
        let operand = self.operand.with_source(self.source());
        write!(f, "{operator}{operand}")
    }
}

// impl Display for UnaryOperator {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         match self {
//             UnaryOperator::LogicalNegation => write!(f, "!"),
//             UnaryOperator::Negation => write!(f, "-"),
//             UnaryOperator::BitwiseComplement => write!(f, "~"),
//             UnaryOperator::AddressOf => write!(f, "&"),
//             UnaryOperator::Indirection => write!(f, "*"),
//         }
//     }
// }

impl Display for WithSource<'_, &BinaryExpression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let operator = &self.operator;
        let left = self.left.with_source(self.source());
        let right = self.right.with_source(self.source());
        write!(f, "{left} {operator} {right}")
    }
}

// impl Display for BinaryOperator {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         match self {
//             BinaryOperator::ShortCircuitOr => write!(f, "||"),
//             BinaryOperator::ShortCircuitAnd => write!(f, "&&"),
//             BinaryOperator::Addition => write!(f, "+"),
//             BinaryOperator::Subtraction => write!(f, "-"),
//             BinaryOperator::Multiplication => write!(f, "*"),
//             BinaryOperator::Division => write!(f, "/"),
//             BinaryOperator::Remainder => write!(f, "%"),
//             BinaryOperator::Equality => write!(f, "=="),
//             BinaryOperator::Inequality => write!(f, "!="),
//             BinaryOperator::LessThan => write!(f, "<"),
//             BinaryOperator::LessThanEqual => write!(f, "<="),
//             BinaryOperator::GreaterThan => write!(f, ">"),
//             BinaryOperator::GreaterThanEqual => write!(f, ">="),
//             BinaryOperator::BitwiseOr => write!(f, "|"),
//             BinaryOperator::BitwiseAnd => write!(f, "&"),
//             BinaryOperator::BitwiseXor => write!(f, "^"),
//             BinaryOperator::ShiftLeft => write!(f, "<<"),
//             BinaryOperator::ShiftRight => write!(f, ">>"),
//         }
//     }
// }

impl Display for WithSource<'_, &FunctionCallExpression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = &self.source()[self.name.clone()];
        let tplt = fmt_template(&self.template_args, self.source());
        let args = self
            .arguments
            .iter()
            .map(|arg| arg.with_source(self.source()))
            .format(", ");
        write!(f, "{name}{tplt}({args})")
    }
}

impl Display for WithSource<'_, &IdentifierExpression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = &self.source()[(*self).clone()];
        write!(f, "{name}")
    }
}

impl Display for WithSource<'_, &TypeExpression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = &self.source()[self.name.clone()];
        let tplt = fmt_template(&self.template_args, self.source());
        write!(f, "{name}{tplt}")
    }
}

// impl Display for Print<'_, &TemplateArg> {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         Ok(())
//     }
// }

fn fmt_template(tplt: &Option<Vec<Spanned<TemplateArg>>>, source: &'_ str) -> String {
    match tplt {
        Some(tplt) => {
            let print = tplt.iter().map(|arg| arg.with_source(source)).format(", ");
            format!("<{print}>")
        }
        None => "".to_string(),
    }
}

impl Display for WithSource<'_, &Statement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.deref() {
            Statement::Void => write!(f, ";"),
            Statement::Compound(print) => write!(f, "{}", print.with_source(self.source())),
            Statement::Assignment(print) => write!(f, "{}", print.with_source(self.source())),
            Statement::Increment(expr) => {
                write!(f, "{}++;", expr.with_source(self.source()))
            }
            Statement::Decrement(expr) => {
                write!(f, "{}--;", expr.with_source(self.source()))
            }
            Statement::If(print) => write!(f, "{}", print.with_source(self.source())),
            Statement::Switch(print) => write!(f, "{}", print.with_source(self.source())),
            Statement::Loop(print) => write!(f, "{}", print.with_source(self.source())),
            Statement::For(print) => write!(f, "{}", print.with_source(self.source())),
            Statement::While(print) => write!(f, "{}", print.with_source(self.source())),
            Statement::Break => write!(f, "break;"),
            Statement::Continue => write!(f, "continue;"),
            Statement::Return(expr) => {
                let expr = expr
                    .as_ref()
                    .map(|expr| format!(" {}", expr.with_source(self.source())))
                    .unwrap_or_default();
                write!(f, "return{expr};")
            }
            Statement::Discard => write!(f, "discard;"),
            Statement::FunctionCall(expr) => write!(f, "{};", expr.with_source(self.source())),
            Statement::ConstAssert(print) => write!(f, "{}", print.with_source(self.source())),
            Statement::Declaration(print) => write!(f, "{}", print.with_source(self.source())),
        }
    }
}

impl Display for WithSource<'_, &CompoundStatement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, self.source(), true);
        let stmts = Indent(
            self.statements
                .iter()
                .map(|stmt| stmt.with_source(self.source()))
                .format("\n"),
        );
        write!(f, "{attrs}{{\n{stmts}\n}}")
    }
}

impl Display for WithSource<'_, &AssignmentStatement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let operator = &self.operator;
        let lhs = self.lhs.with_source(self.source());
        let rhs = self.rhs.with_source(self.source());
        write!(f, "{lhs} {operator} {rhs};")
    }
}

impl Display for AssignmentOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            AssignmentOperator::Equal => write!(f, "="),
            AssignmentOperator::PlusEqual => write!(f, "+="),
            AssignmentOperator::MinusEqual => write!(f, "-="),
            AssignmentOperator::TimesEqual => write!(f, "*="),
            AssignmentOperator::DivisionEqual => write!(f, "/="),
            AssignmentOperator::ModuloEqual => write!(f, "%="),
            AssignmentOperator::AndEqual => write!(f, "&="),
            AssignmentOperator::OrEqual => write!(f, "|="),
            AssignmentOperator::XorEqual => write!(f, "^="),
            AssignmentOperator::ShiftRightAssign => write!(f, ">>="),
            AssignmentOperator::ShiftLeftAssign => write!(f, "<<="),
        }
    }
}

// impl Display for Print<'_, &IncrementStatement> {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         Ok(())
//     }
// }

// impl Display for Print<'_, &DecrementStatement> {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         Ok(())
//     }
// }

impl Display for WithSource<'_, &IfStatement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, self.source(), false);
        let expr = self.if_clause.0.with_source(self.source());
        let stmt = self.if_clause.1.with_source(self.source());
        write!(f, "{attrs}if ({expr}) {stmt}")?;
        for else_if_clause in self.else_if_clauses.iter() {
            let expr = else_if_clause.0.with_source(self.source());
            let stmt = &else_if_clause.1.with_source(self.source());
            write!(f, "\nelse if ({expr}) {stmt}")?;
        }
        if let Some(ref else_stmt) = self.else_clause {
            let else_stmt = else_stmt.with_source(self.source());
            write!(f, "\nelse {else_stmt}")?;
        }
        Ok(())
    }
}

impl Display for WithSource<'_, &SwitchStatement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, self.source(), false);
        let expr = self.expression.with_source(self.source());
        let body_attrs = fmt_attrs(&self.body_attributes, self.source(), false);
        let clauses = Indent(
            self.clauses
                .iter()
                .map(|clause| clause.with_source(self.source()))
                .format("\n"),
        );
        write!(f, "{attrs}switch {expr} {body_attrs}{{\n{clauses}\n}}")
    }
}

impl Display for WithSource<'_, &SwitchClause> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let cases = self
            .case_selectors
            .iter()
            .map(|case| case.with_source(self.source()))
            .format(", ");
        let body = self.body.with_source(self.source());
        write!(f, "case {cases} {body}")
    }
}

impl Display for WithSource<'_, &CaseSelector> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.deref() {
            CaseSelector::Default => write!(f, "default"),
            CaseSelector::Expression(expr) => {
                write!(f, "{}", expr.with_source(self.source()))
            }
        }
    }
}

impl Display for WithSource<'_, &LoopStatement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, self.source(), false);
        let body_attrs = fmt_attrs(&self.body.attributes, self.source(), false);
        let body = Indent(
            self.body
                .statements
                .iter()
                .map(|stmt| stmt.with_source(self.source()))
                .format("\n"),
        );
        let continuing = self
            .continuing
            .as_ref()
            .map(|cont| format!("{}\n", Indent(cont.with_source(self.source()))))
            .unwrap_or_default();
        write!(f, "{attrs}loop {body_attrs}{{\n{body}\n{continuing}}}")
    }
}

impl Display for WithSource<'_, &ContinuingStatement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let body_attrs = fmt_attrs(&self.body.attributes, self.source(), false);
        let body = Indent(
            self.body
                .statements
                .iter()
                .map(|stmt| stmt.with_source(self.source()))
                .format("\n"),
        );
        let break_if = self
            .break_if
            .as_ref()
            .map(|cont| format!("{};\n", Indent(cont.with_source(self.source()))))
            .unwrap_or_default();
        write!(f, "continuing {body_attrs}{{\n{body}\n{break_if}}}")
    }
}

// impl Display for Print<'_, &BreakIfStatement> {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         Ok(())
//     }
// }

impl Display for WithSource<'_, &ForStatement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, self.source(), false);
        let mut init = self
            .initializer
            .as_ref()
            .map(|stmt| format!("{}", stmt.with_source(self.source())))
            .unwrap_or_default();
        if init.ends_with(';') {
            init.pop();
        }
        let cond = self
            .condition
            .as_ref()
            .map(|expr| format!("{}", expr.with_source(self.source())))
            .unwrap_or_default();
        let mut updt = self
            .update
            .as_ref()
            .map(|stmt| format!("{}", stmt.with_source(self.source())))
            .unwrap_or_default();
        if updt.ends_with(';') {
            updt.pop();
        }
        let body = &self.body.with_source(self.source());
        write!(f, "{attrs}for ({init}; {cond}; {updt}) {body}")
    }
}

impl Display for WithSource<'_, &WhileStatement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, self.source(), false);
        let cond = self.condition.with_source(self.source());
        let body = self.body.with_source(self.source());
        write!(f, "{attrs}while ({cond}) {body}")
    }
}
