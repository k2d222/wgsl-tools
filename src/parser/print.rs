use crate::parser::ast::*;
use core::fmt;
use std::{
    fmt::{Display, Formatter},
    ops::Deref,
};

use itertools::Itertools;

use super::span::Spanned;

/// A type that implements display for the syntax tree.
/// It *should* output a valid wgsl file equivalent to the parsed file.
/// It should only be used for debugging!

pub fn print<'s, T>(print: &'s T, source: &'s str)
where
    Print<'s, &'s T>: Display,
{
    println!("{}", Print::new(print, source));
}

pub struct Print<'s, T> {
    print: T,
    source: &'s str,
}

impl<'s, T> Print<'s, T> {
    fn new(print: T, source: &'s str) -> Self {
        Self { print, source }
    }
}

struct Indent<T: Display>(pub T);

impl<'s, T> Deref for Print<'s, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.print
    }
}

// struct Surround<A, B, C>(A, B, C);

// impl<A: Display, B: Display, C: Display> Display for Surround<A, B, C> {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         write!(f, "{}{}{}", self.0, self.1, self.2)
//     }
// }

// fn prefix<T: Display>(prefix: impl Display, print: T) -> impl Display {
//     Surround(prefix, print, "")
// }

// fn suffix<T: Display>(print: T, suffix: &'static str) -> impl Display {
//     Surround("", print, suffix)
// }

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
//         Print::new(&self.0, self.source).fmt(f)
//     }
// }

impl Display for Print<'_, &TranslationUnit> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let directives = self
            .global_directives
            .iter()
            .map(|x| Print::new(x.as_ref(), self.source))
            .format("\n");
        let declarations = self
            .global_declarations
            .iter()
            .map(|x| Print::new(x.as_ref(), self.source))
            .format("\n\n");
        writeln!(f, "{directives}\n\n{declarations}")
    }
}

impl Display for Print<'_, &GlobalDirective> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let source = self.source;
        match self.deref() {
            GlobalDirective::Diagnostic(print) => write!(f, "{}", Print { print, source }),
            GlobalDirective::Enable(print) => write!(f, "{}", Print { print, source }),
            GlobalDirective::Requires(print) => write!(f, "{}", Print { print, source }),
        }
    }
}

impl Display for Print<'_, &DiagnosticDirective> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let severity = &self.severity;
        let rule = &self.source[self.rule_name.clone()];
        writeln!(f, "diagnostic ({severity}, {rule});")
    }
}

impl Display for DiagnosticSeverity {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Error => write!(f, "error"),
            Self::Warning => write!(f, "warning"),
            Self::Info => write!(f, "info"),
            Self::Off => write!(f, "off"),
        }
    }
}

impl Display for Print<'_, &EnableDirective> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let exts = self
            .extensions
            .iter()
            .map(|span| &self.source[span.clone()])
            .format(", ");
        writeln!(f, "enable {exts};")
    }
}

impl Display for Print<'_, &RequiresDirective> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let exts = self
            .extensions
            .iter()
            .map(|span| &self.source[span.clone()])
            .format(", ");
        writeln!(f, "requires {exts};")
    }
}

impl Display for Print<'_, &GlobalDeclaration> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let source = self.source;
        match self.deref() {
            GlobalDeclaration::Void => write!(f, ";"),
            GlobalDeclaration::Declaration(print) => write!(f, "{}", Print { print, source }),
            GlobalDeclaration::TypeAlias(print) => write!(f, "{}", Print { print, source }),
            GlobalDeclaration::Struct(print) => write!(f, "{}", Print { print, source }),
            GlobalDeclaration::Function(print) => write!(f, "{}", Print { print, source }),
            GlobalDeclaration::ConstAssert(print) => write!(f, "{}", Print { print, source }),
        }
    }
}

impl Display for Print<'_, &Declaration> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, self.source, false);
        let kind = &self.kind;
        let tplt = fmt_template(&self.template_args, self.source);
        let name = &self.source[self.name.clone()];
        let typ = self
            .typ
            .as_ref()
            .map(|typ| format!(": {}", Print::new(typ, self.source)))
            .unwrap_or_default();
        let init = self
            .initializer
            .as_ref()
            .map(|typ| format!(" = {}", Print::new(typ.as_ref(), self.source)))
            .unwrap_or_default();
        write!(f, "{attrs}{kind}{tplt} {name}{typ}{init};")
    }
}

impl Display for DeclarationKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            DeclarationKind::Const => write!(f, "const"),
            DeclarationKind::Override => write!(f, "override"),
            DeclarationKind::Let => write!(f, "let"),
            DeclarationKind::Var => write!(f, "var"),
        }
    }
}

impl Display for Print<'_, &TypeAlias> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = &self.source[self.name.clone()];
        let typ = Print::new(&self.typ, self.source);
        write!(f, "alias {name} = {typ};")
    }
}

impl Display for Print<'_, &Struct> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = &self.source[self.name.clone()];
        let members = Indent(
            self.members
                .iter()
                .map(|mem| Print::new(mem.as_ref(), self.source))
                .format(",\n"),
        );
        write!(f, "struct {name} {{\n{members}\n}}")
    }
}

impl Display for Print<'_, &StructMember> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, self.source, false);
        let name = &self.source[self.name.clone()];
        let typ = Print::new(&self.typ, self.source);
        write!(f, "{attrs}{name}: {typ}")
    }
}

impl Display for Print<'_, &Function> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, self.source, false);
        let name = &self.source[self.name.clone()];
        let params = self
            .parameters
            .iter()
            .map(|p| Print::new(p.as_ref(), self.source))
            .format(", ");
        let ret_attrs = fmt_attrs(&self.return_attributes, self.source, true);
        let ret_typ = self
            .return_type
            .as_ref()
            .map(|typ| format!("-> {} ", Print::new(typ, self.source)))
            .unwrap_or_default();
        let body = Print::new(&self.body, self.source);
        write!(f, "{attrs}fn {name}({params}) {ret_attrs}{ret_typ}{body}")
    }
}

impl Display for Print<'_, &FormalParameter> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, self.source, true);
        let name = &self.source[self.name.clone()];
        let typ = Print::new(&self.typ, self.source);
        write!(f, "{attrs}{name}: {typ}")
    }
}

impl Display for Print<'_, &ConstAssert> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let expr = Print::new(self.expression.as_ref(), self.source);
        write!(f, "const_assert {expr};",)
    }
}

impl Display for Print<'_, &Attribute> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = &self.source[self.name.clone()];
        let args = self
            .arguments
            .as_ref()
            .map(|args| {
                format!(
                    "({})",
                    args.iter()
                        .map(|arg| Print::new(arg.as_ref(), self.source))
                        .format(", ")
                )
            })
            .unwrap_or_default();
        write!(f, "@{name}{args}")
    }
}

fn fmt_attrs(attrs: &Vec<Spanned<Attribute>>, source: &'_ str, inline: bool) -> String {
    let print = attrs
        .iter()
        .map(|attr| Print::new(attr.as_ref(), source))
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

impl Display for Print<'_, &Expression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let source = self.source;
        match self.deref() {
            Expression::Literal(print) => write!(f, "{}", print),
            Expression::Parenthesized(print) => {
                let expr = Print::new(print.deref(), self.source);
                write!(f, "({expr})")
            }
            Expression::NamedComponent(print) => write!(f, "{}", Print::new(print, source)),
            Expression::Indexing(print) => write!(f, "{}", Print::new(print, source)),
            Expression::Unary(print) => write!(f, "{}", Print::new(print, source)),
            Expression::Binary(print) => write!(f, "{}", Print::new(print, source)),
            Expression::FunctionCall(print) => write!(f, "{}", Print::new(print, source)),
            Expression::Identifier(print) => write!(f, "{}", Print::new(print, source)),
            Expression::Type(print) => write!(f, "{}", Print::new(print, source)),
        }
    }
}

impl Display for LiteralExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LiteralExpression::True => write!(f, "true"),
            LiteralExpression::False => write!(f, "false"),
            LiteralExpression::AbstractInt(num) => write!(f, "{num}"),
            LiteralExpression::AbstractFloat(num) => write!(f, "{num}"),
            LiteralExpression::I32(num) => write!(f, "{num}i"),
            LiteralExpression::U32(num) => write!(f, "{num}u"),
            LiteralExpression::F32(num) => write!(f, "{num}f"),
            LiteralExpression::F16(num) => write!(f, "{num}h"),
        }
    }
}

// impl Display for Print<'_, &ParenthesizedExpression> {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         let expr = Print::new(self.print.deref(), self.source);
//         write!(f, "({expr})")
//     }
// }

impl Display for Print<'_, &NamedComponentExpression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let base = Print::new(self.base.as_ref().as_ref(), self.source);
        let component = &self.source[self.component.clone()];
        write!(f, "{base}.{component}")
    }
}

impl Display for Print<'_, &IndexingExpression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let base = Print::new(self.base.as_ref().as_ref(), self.source);
        let index = Print::new(self.index.as_ref().as_ref(), self.source);
        write!(f, "{base}[{index}]")
    }
}

impl Display for Print<'_, &UnaryExpression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let operator = &self.operator;
        let operand = Print::new(self.operand.as_ref().as_ref(), self.source);
        write!(f, "{operator}{operand}")
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOperator::LogicalNegation => write!(f, "!"),
            UnaryOperator::Negation => write!(f, "-"),
            UnaryOperator::BitwiseComplement => write!(f, "~"),
            UnaryOperator::AddressOf => write!(f, "&"),
            UnaryOperator::Indirection => write!(f, "*"),
        }
    }
}

impl Display for Print<'_, &BinaryExpression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let operator = &self.operator;
        let left = Print::new(self.left.as_ref().as_ref(), self.source);
        let right = Print::new(self.right.as_ref().as_ref(), self.source);
        write!(f, "{left} {operator} {right}")
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOperator::ShortCircuitOr => write!(f, "||"),
            BinaryOperator::ShortCircuitAnd => write!(f, "&&"),
            BinaryOperator::Addition => write!(f, "+"),
            BinaryOperator::Subtraction => write!(f, "-"),
            BinaryOperator::Multiplication => write!(f, "*"),
            BinaryOperator::Division => write!(f, "/"),
            BinaryOperator::Remainder => write!(f, "%"),
            BinaryOperator::Equality => write!(f, "=="),
            BinaryOperator::Inequality => write!(f, "!="),
            BinaryOperator::LessThan => write!(f, "<"),
            BinaryOperator::LessThanEqual => write!(f, "<="),
            BinaryOperator::GreaterThan => write!(f, ">"),
            BinaryOperator::GreaterThanEqual => write!(f, ">="),
            BinaryOperator::BitwiseOr => write!(f, "|"),
            BinaryOperator::BitwiseAnd => write!(f, "&"),
            BinaryOperator::BitwiseXor => write!(f, "^"),
            BinaryOperator::ShiftLeft => write!(f, "<<"),
            BinaryOperator::ShiftRight => write!(f, ">>"),
        }
    }
}

impl Display for Print<'_, &FunctionCallExpression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = &self.source[self.name.clone()];
        let tplt = fmt_template(&self.template_args, self.source);
        let args = self
            .arguments
            .iter()
            .map(|arg| Print::new(arg.as_ref(), self.source))
            .format(", ");
        write!(f, "{name}{tplt}({args})")
    }
}

impl Display for Print<'_, &IdentifierExpression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = &self.source[self.print.clone()];
        write!(f, "{name}")
    }
}

impl Display for Print<'_, &TypeExpression> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = &self.source[self.name.clone()];
        let tplt = fmt_template(&self.template_args, self.source);
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
            let print = tplt
                .iter()
                .map(|arg| Print::new(arg.as_ref(), source))
                .format(", ");
            format!("<{print}>")
        }
        None => "".to_string(),
    }
}

impl Display for Print<'_, &Statement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let source = self.source;
        match self.deref() {
            Statement::Void => write!(f, ";"),
            Statement::Compound(print) => write!(f, "{}", Print::new(print, source)),
            Statement::Assignment(print) => write!(f, "{}", Print::new(print, source)),
            Statement::Increment(expr) => write!(f, "{}++;", Print::new(expr.as_ref(), source)),
            Statement::Decrement(expr) => write!(f, "{}--;", Print::new(expr.as_ref(), source)),
            Statement::If(print) => write!(f, "{}", Print::new(print, source)),
            Statement::Switch(print) => write!(f, "{}", Print::new(print, source)),
            Statement::Loop(print) => write!(f, "{}", Print::new(print, source)),
            Statement::For(print) => write!(f, "{}", Print::new(print, source)),
            Statement::While(print) => write!(f, "{}", Print::new(print, source)),
            Statement::Break => write!(f, "break;"),
            Statement::Continue => write!(f, "continue;"),
            Statement::Return(expr) => {
                let expr = expr
                    .as_ref()
                    .map(|expr| format!(" {}", Print::new(expr.as_ref(), self.source)))
                    .unwrap_or_default();
                write!(f, "return{expr};")
            }
            Statement::Discard => write!(f, "discard;"),
            Statement::FunctionCall(expr) => write!(f, "{};", Print::new(expr, source)),
            Statement::ConstAssert(print) => write!(f, "{}", Print::new(print, source)),
            Statement::Declaration(print) => write!(f, "{}", Print::new(print, source)),
        }
    }
}

impl Display for Print<'_, &CompoundStatement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, self.source, true);
        let stmts = Indent(
            self.statements
                .iter()
                .map(|stmt| Print::new(stmt.as_ref(), self.source))
                .format("\n"),
        );
        write!(f, "{attrs}{{\n{stmts}\n}}")
    }
}

impl Display for Print<'_, &AssignmentStatement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let operator = &self.operator;
        let lhs = Print::new(self.lhs.as_ref(), self.source);
        let rhs = Print::new(self.rhs.as_ref(), self.source);
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

impl Display for Print<'_, &IfStatement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, self.source, false);
        let expr = Print::new(self.if_clause.0.as_ref(), self.source);
        let stmt = Print::new(&self.if_clause.1, self.source);
        write!(f, "{attrs}if ({expr}) {stmt}")?;
        for else_if_clause in self.else_if_clauses.iter() {
            let expr = Print::new(else_if_clause.0.as_ref(), self.source);
            let stmt = Print::new(&else_if_clause.1, self.source);
            write!(f, "\nelse if ({expr}) {stmt}")?;
        }
        if let Some(ref else_stmt) = self.else_clause {
            let else_stmt = Print::new(else_stmt, self.source);
            write!(f, "\nelse {else_stmt}")?;
        }
        Ok(())
    }
}

impl Display for Print<'_, &SwitchStatement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, self.source, false);
        let expr = Print::new(self.expression.as_ref(), self.source);
        let body_attrs = fmt_attrs(&self.body_attributes, self.source, false);
        let clauses = Indent(
            self.clauses
                .iter()
                .map(|clause| Print::new(clause, self.source))
                .format("\n"),
        );
        write!(f, "{attrs}switch {expr} {body_attrs}{{\n{clauses}\n}}")
    }
}

impl Display for Print<'_, &SwitchClause> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let cases = self
            .case_selectors
            .iter()
            .map(|case| Print::new(case, self.source))
            .format(", ");
        let body = Print::new(&self.body, self.source);
        write!(f, "case {cases} {body}")
    }
}

impl Display for Print<'_, &CaseSelector> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.deref() {
            CaseSelector::Default => write!(f, "default"),
            CaseSelector::Expression(expr) => {
                write!(f, "{}", Print::new(expr.as_ref(), self.source))
            }
        }
    }
}

impl Display for Print<'_, &LoopStatement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, self.source, false);
        let body_attrs = fmt_attrs(&self.body.attributes, self.source, false);
        let body = Indent(
            self.body
                .statements
                .iter()
                .map(|stmt| Print::new(stmt.as_ref(), self.source))
                .format("\n"),
        );
        let continuing = self
            .continuing
            .as_ref()
            .map(|cont| format!("{}\n", Indent(Print::new(cont.as_ref(), self.source))))
            .unwrap_or_default();
        write!(f, "{attrs}loop {body_attrs}{{\n{body}\n{continuing}}}")
    }
}

impl Display for Print<'_, &ContinuingStatement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let body_attrs = fmt_attrs(&self.body.attributes, self.source, false);
        let body = Indent(
            self.body
                .statements
                .iter()
                .map(|stmt| Print::new(stmt.as_ref(), self.source))
                .format("\n"),
        );
        let break_if = self
            .break_if
            .as_ref()
            .map(|cont| format!("{};\n", Indent(Print::new(cont.as_ref(), self.source))))
            .unwrap_or_default();
        write!(f, "continuing {body_attrs}{{\n{body}\n{break_if}}}")
    }
}

// impl Display for Print<'_, &BreakIfStatement> {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         Ok(())
//     }
// }

impl Display for Print<'_, &ForStatement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, self.source, false);
        let mut init = self
            .initializer
            .as_ref()
            .map(|stmt| format!("{}", Print::new(stmt.as_ref().as_ref(), self.source)))
            .unwrap_or_default();
        if init.ends_with(";") {
            init.pop();
        }
        let cond = self
            .condition
            .as_ref()
            .map(|expr| format!("{}", Print::new(expr.as_ref(), self.source)))
            .unwrap_or_default();
        let mut updt = self
            .update
            .as_ref()
            .map(|stmt| format!("{}", Print::new(stmt.as_ref().as_ref(), self.source)))
            .unwrap_or_default();
        if updt.ends_with(";") {
            updt.pop();
        }
        let body = Print::new(&self.body, self.source);
        write!(f, "{attrs}for ({init}; {cond}; {updt}) {body}")
    }
}

impl Display for Print<'_, &WhileStatement> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, self.source, false);
        let cond = Print::new(self.condition.as_ref(), self.source);
        let body = Print::new(&self.body, self.source);
        write!(f, "{attrs}while ({cond}) {body}")
    }
}
