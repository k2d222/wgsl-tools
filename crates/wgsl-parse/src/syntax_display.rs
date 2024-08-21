use crate::syntax::*;
use core::fmt;
use std::fmt::{Display, Formatter};

use itertools::Itertools;

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

impl Display for TranslationUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let directives = self.global_directives.iter().format("\n");
        let declarations = self.global_declarations.iter().format("\n\n");
        writeln!(f, "{directives}\n\n{declarations}")
    }
}

impl Display for GlobalDirective {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GlobalDirective::Diagnostic(print) => write!(f, "{}", print),
            GlobalDirective::Enable(print) => write!(f, "{}", print),
            GlobalDirective::Requires(print) => write!(f, "{}", print),
        }
    }
}

impl Display for DiagnosticDirective {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let severity = &self.severity;
        let rule = &self.rule_name;
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

impl Display for EnableDirective {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let exts = self.extensions.iter().format(", ");
        writeln!(f, "enable {exts};")
    }
}

impl Display for RequiresDirective {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let exts = self.extensions.iter().format(", ");
        writeln!(f, "requires {exts};")
    }
}

impl Display for GlobalDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GlobalDeclaration::Void => write!(f, ";"),
            GlobalDeclaration::Declaration(print) => write!(f, "{}", print),
            GlobalDeclaration::TypeAlias(print) => write!(f, "{}", print),
            GlobalDeclaration::Struct(print) => write!(f, "{}", print),
            GlobalDeclaration::Function(print) => write!(f, "{}", print),
            GlobalDeclaration::ConstAssert(print) => write!(f, "{}", print),
        }
    }
}

impl Display for Declaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, false);
        let kind = &self.kind;
        let tplt = fmt_template(&self.template_args);
        let name = &self.name;
        let typ = self
            .typ
            .as_ref()
            .map(|typ| format!(": {}", typ))
            .unwrap_or_default();
        let init = self
            .initializer
            .as_ref()
            .map(|typ| format!(" = {}", typ))
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

impl Display for TypeAlias {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = &self.name;
        let typ = &self.typ;
        write!(f, "alias {name} = {typ};")
    }
}

impl Display for Struct {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = &self.name;
        let members = Indent(self.members.iter().map(|mem| mem).format(",\n"));
        write!(f, "struct {name} {{\n{members}\n}}")
    }
}

impl Display for StructMember {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, false);
        let name = &self.name;
        let typ = &self.typ;
        write!(f, "{attrs}{name}: {typ}")
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, false);
        let name = &self.name;
        let params = self.parameters.iter().map(|p| p).format(", ");
        let ret_attrs = fmt_attrs(&self.return_attributes, true);
        let ret_typ = self
            .return_type
            .as_ref()
            .map(|typ| format!("-> {} ", typ))
            .unwrap_or_default();
        let body = &self.body;
        write!(f, "{attrs}fn {name}({params}) {ret_attrs}{ret_typ}{body}")
    }
}

impl Display for FormalParameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, true);
        let name = &self.name;
        let typ = &self.typ;
        write!(f, "{attrs}{name}: {typ}")
    }
}

impl Display for ConstAssert {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let expr = &self.expression;
        write!(f, "const_assert {expr};",)
    }
}

impl Display for Attribute {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = &self.name;
        let args = self
            .arguments
            .as_ref()
            .map(|args| format!("({})", args.iter().map(|arg| arg).format(", ")))
            .unwrap_or_default();
        write!(f, "@{name}{args}")
    }
}

fn fmt_attrs(attrs: &Vec<Attribute>, inline: bool) -> String {
    let print = attrs.iter().map(|attr| attr).format(" ");
    let suffix = if attrs.is_empty() {
        ""
    } else if inline {
        " "
    } else {
        "\n"
    };
    format!("{print}{suffix}")
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Literal(print) => write!(f, "{}", print),
            Expression::Parenthesized(expr) => {
                write!(f, "({expr})")
            }
            Expression::NamedComponent(print) => write!(f, "{}", print),
            Expression::Indexing(print) => write!(f, "{}", print),
            Expression::Unary(print) => write!(f, "{}", print),
            Expression::Binary(print) => write!(f, "{}", print),
            Expression::FunctionCall(print) => write!(f, "{}", print),
            Expression::Identifier(print) => write!(f, "{}", print),
            Expression::Type(print) => write!(f, "{}", print),
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

// impl Display for ParenthesizedExpression {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         let expr = self);
//         write!(f, "({expr})")
//     }
// }

impl Display for NamedComponentExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let base = &self.base;
        let component = &self.component;
        write!(f, "{base}.{component}")
    }
}

impl Display for IndexingExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let base = &self.base;
        let index = &self.index;
        write!(f, "{base}[{index}]")
    }
}

impl Display for UnaryExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let operator = &self.operator;
        let operand = &self.operand;
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

impl Display for BinaryExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let operator = &self.operator;
        let left = &self.left;
        let right = &self.right;
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

impl Display for FunctionCallExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = &self.name;
        let tplt = fmt_template(&self.template_args);
        let args = self.arguments.iter().map(|arg| arg).format(", ");
        write!(f, "{name}{tplt}({args})")
    }
}

// impl Display for IdentifierExpression {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         write!(f, "{self}")
//     }
// }

impl Display for TypeExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = &self.name;
        let tplt = fmt_template(&self.template_args);
        write!(f, "{name}{tplt}")
    }
}

// impl Display for TemplateArg {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         Ok(())
//     }
// }

fn fmt_template(tplt: &Option<Vec<TemplateArg>>) -> String {
    match tplt {
        Some(tplt) => {
            let print = tplt.iter().format(", ");
            format!("<{print}>")
        }
        None => "".to_string(),
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Void => write!(f, ";"),
            Statement::Compound(print) => write!(f, "{}", print),
            Statement::Assignment(print) => write!(f, "{}", print),
            Statement::Increment(expr) => write!(f, "{}++;", expr),
            Statement::Decrement(expr) => write!(f, "{}--;", expr),
            Statement::If(print) => write!(f, "{}", print),
            Statement::Switch(print) => write!(f, "{}", print),
            Statement::Loop(print) => write!(f, "{}", print),
            Statement::For(print) => write!(f, "{}", print),
            Statement::While(print) => write!(f, "{}", print),
            Statement::Break => write!(f, "break;"),
            Statement::Continue => write!(f, "continue;"),
            Statement::Return(expr) => {
                let expr = expr
                    .as_ref()
                    .map(|expr| format!(" {}", expr))
                    .unwrap_or_default();
                write!(f, "return{expr};")
            }
            Statement::Discard => write!(f, "discard;"),
            Statement::FunctionCall(expr) => write!(f, "{};", expr),
            Statement::ConstAssert(print) => write!(f, "{}", print),
            Statement::Declaration(print) => write!(f, "{}", print),
        }
    }
}

impl Display for CompoundStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, true);
        let stmts = Indent(self.statements.iter().map(|stmt| stmt).format("\n"));
        write!(f, "{attrs}{{\n{stmts}\n}}")
    }
}

impl Display for AssignmentStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let operator = &self.operator;
        let lhs = &self.lhs;
        let rhs = &self.rhs;
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

// impl Display for IncrementStatement {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         Ok(())
//     }
// }

// impl Display for DecrementStatement {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         Ok(())
//     }
// }

impl Display for IfStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, false);
        let expr = &self.if_clause.0;
        let stmt = &self.if_clause.1;
        write!(f, "{attrs}if ({expr}) {stmt}")?;
        for else_if_clause in self.else_if_clauses.iter() {
            let expr = &else_if_clause.0;
            let stmt = &else_if_clause.1;
            write!(f, "\nelse if ({expr}) {stmt}")?;
        }
        if let Some(ref else_stmt) = self.else_clause {
            write!(f, "\nelse {else_stmt}")?;
        }
        Ok(())
    }
}

impl Display for SwitchStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, false);
        let expr = &self.expression;
        let body_attrs = fmt_attrs(&self.body_attributes, false);
        let clauses = Indent(self.clauses.iter().map(|clause| clause).format("\n"));
        write!(f, "{attrs}switch {expr} {body_attrs}{{\n{clauses}\n}}")
    }
}

impl Display for SwitchClause {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let cases = self.case_selectors.iter().map(|case| case).format(", ");
        let body = &self.body;
        write!(f, "case {cases} {body}")
    }
}

impl Display for CaseSelector {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            CaseSelector::Default => write!(f, "default"),
            CaseSelector::Expression(expr) => {
                write!(f, "{}", expr)
            }
        }
    }
}

impl Display for LoopStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, false);
        let body_attrs = fmt_attrs(&self.body.attributes, false);
        let body = Indent(self.body.statements.iter().map(|stmt| stmt).format("\n"));
        let continuing = self
            .continuing
            .as_ref()
            .map(|cont| format!("{}\n", Indent(cont)))
            .unwrap_or_default();
        write!(f, "{attrs}loop {body_attrs}{{\n{body}\n{continuing}}}")
    }
}

impl Display for ContinuingStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let body_attrs = fmt_attrs(&self.body.attributes, false);
        let body = Indent(self.body.statements.iter().map(|stmt| stmt).format("\n"));
        let break_if = self
            .break_if
            .as_ref()
            .map(|cont| format!("{};\n", Indent(cont)))
            .unwrap_or_default();
        write!(f, "continuing {body_attrs}{{\n{body}\n{break_if}}}")
    }
}

// impl Display for BreakIfStatement {
//     fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
//         Ok(())
//     }
// }

impl Display for ForStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, false);
        let mut init = self
            .initializer
            .as_ref()
            .map(|stmt| format!("{}", stmt))
            .unwrap_or_default();
        if init.ends_with(";") {
            init.pop();
        }
        let cond = self
            .condition
            .as_ref()
            .map(|expr| format!("{}", expr))
            .unwrap_or_default();
        let mut updt = self
            .update
            .as_ref()
            .map(|stmt| format!("{}", stmt))
            .unwrap_or_default();
        if updt.ends_with(";") {
            updt.pop();
        }
        let body = &self.body;
        write!(f, "{attrs}for ({init}; {cond}; {updt}) {body}")
    }
}

impl Display for WhileStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let attrs = fmt_attrs(&self.attributes, false);
        let cond = &self.condition;
        let body = &self.body;
        write!(f, "{attrs}while ({cond}) {body}")
    }
}
