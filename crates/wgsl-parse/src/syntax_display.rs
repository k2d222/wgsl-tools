use crate::{span::Spanned, syntax::*};
use core::fmt;
use std::fmt::{Display, Formatter};

use itertools::Itertools;

// unstable: https://doc.rust-lang.org/std/fmt/struct.FormatterFn.html
struct FormatFn<F: (Fn(&mut Formatter) -> fmt::Result)>(F);

impl<F: Fn(&mut Formatter) -> fmt::Result> Display for FormatFn<F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        (self.0)(f)
    }
}

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.node().fmt(f)
    }
}

struct Indent<T: Display>(pub T);

impl<T: Display> Display for Indent<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let indent = "    ";
        let inner_display = self.0.to_string();
        let fmt = inner_display
            .lines()
            .format_with("\n", |l, f| f(&format_args!("{indent}{l}")));
        write!(f, "{}", fmt)?;
        Ok(())
    }
}

impl Display for TranslationUnit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if cfg!(feature = "imports") && !self.imports.is_empty() {
            let imports = self.imports.iter().format("\n");
            write!(f, "{imports}\n\n")?;
        }
        if !self.global_directives.is_empty() {
            let directives = self.global_directives.iter().format("\n");
            write!(f, "{directives}\n\n")?;
        }
        let declarations = self
            .global_declarations
            .iter()
            // .filter(|decl| !matches!(decl, GlobalDeclaration::Void))
            .format("\n\n");
        write!(f, "{declarations}\n")
    }
}

#[cfg(feature = "imports")]
impl Display for Import {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        let path = self.path.display();
        let content = &self.content;
        write!(f, "{path}{content};")
    }
}

#[cfg(feature = "imports")]
impl Display for ImportContent {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ImportContent::Star(item) => {
                write!(f, "{}/*", item.name)?;
                if let Some(rename) = &item.rename {
                    write!(f, " as {rename}")?;
                }
                Ok(())
            }
            ImportContent::Item(item) => {
                write!(f, "{}", item.name)?;
                if let Some(rename) = &item.rename {
                    write!(f, " as {rename}")?;
                }
                Ok(())
            }
            ImportContent::Collection(coll) => {
                let coll = coll.iter().format(", ");
                write!(f, "{{ {coll} }}")
            }
        }
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
        if cfg!(feature = "attributes") {
            write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        }
        let severity = &self.severity;
        let rule = &self.rule_name;
        write!(f, "diagnostic ({severity}, {rule});")
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
        if cfg!(feature = "attributes") {
            write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        }
        let exts = self.extensions.iter().format(", ");
        write!(f, "enable {exts};")
    }
}

impl Display for RequiresDirective {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if cfg!(feature = "attributes") {
            write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        }
        let exts = self.extensions.iter().format(", ");
        write!(f, "requires {exts};")
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
        write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        let kind = &self.kind;
        let tplt = fmt_template(&self.template_args);
        let name = &self.name;
        let typ = self
            .ty
            .iter()
            .format_with("", |ty, f| f(&format_args!(": {}", ty)));
        let init = self
            .initializer
            .iter()
            .format_with("", |ty, f| f(&format_args!(" = {}", ty)));
        write!(f, "{kind}{tplt} {name}{typ}{init};")
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
        if cfg!(feature = "attributes") {
            write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        }
        let name = &self.name;
        let typ = &self.ty;
        write!(f, "alias {name} = {typ};")
    }
}

impl Display for Struct {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if cfg!(feature = "attributes") {
            write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        }
        let name = &self.name;
        let members = Indent(self.members.iter().format(",\n"));
        write!(f, "struct {name} {{\n{members}\n}}")
    }
}

impl Display for StructMember {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        let name = &self.name;
        let typ = &self.ty;
        write!(f, "{name}: {typ}")
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        let name = &self.name;
        let params = self.parameters.iter().format(", ");
        let ret_ty = self.return_type.iter().format_with("", |ty, f| {
            f(&FormatFn(|f: &mut Formatter| {
                write!(f, "-> ")?;
                write!(f, "{}", fmt_attrs(&self.return_attributes, true))?;
                write!(f, "{ty} ")?;
                Ok(())
            }))
        });
        let body = &self.body;
        write!(f, "fn {name}({params}) {ret_ty}{body}")
    }
}

impl Display for FormalParameter {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        let name = &self.name;
        let typ = &self.ty;
        write!(f, "{name}: {typ}")
    }
}

impl Display for ConstAssert {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if cfg!(feature = "attributes") {
            write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        }
        let expr = &self.expression;
        write!(f, "const_assert {expr};",)
    }
}

impl Display for Attribute {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = &self.name;
        let args = self.arguments.iter().format_with("", |args, f| {
            f(&format_args!("({})", args.iter().format(", ")))
        });
        write!(f, "@{name}{args}")
    }
}

fn fmt_attrs(attrs: &[Attribute], inline: bool) -> impl fmt::Display + '_ {
    FormatFn(move |f| {
        let print = attrs.iter().format(" ");
        let suffix = if attrs.is_empty() {
            ""
        } else if inline {
            " "
        } else {
            "\n"
        };
        write!(f, "{print}{suffix}")
    })
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Literal(print) => write!(f, "{print}"),
            Expression::Parenthesized(print) => {
                write!(f, "{print}")
            }
            Expression::NamedComponent(print) => write!(f, "{print}"),
            Expression::Indexing(print) => write!(f, "{print}"),
            Expression::Unary(print) => write!(f, "{print}"),
            Expression::Binary(print) => write!(f, "{print}"),
            Expression::FunctionCall(print) => write!(f, "{print}"),
            Expression::Identifier(print) => write!(f, "{print}"),
            Expression::Type(print) => write!(f, "{print}"),
        }
    }
}

impl Display for LiteralExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LiteralExpression::Bool(true) => write!(f, "true"),
            LiteralExpression::Bool(false) => write!(f, "false"),
            LiteralExpression::AbstractInt(num) => write!(f, "{num}"),
            LiteralExpression::AbstractFloat(num) => write!(f, "{num:?}"), // using the Debug formatter to print the trailing .0 in floats representing integers. because format!("{}", 3.0f32) == "3"
            LiteralExpression::I32(num) => write!(f, "{num}i"),
            LiteralExpression::U32(num) => write!(f, "{num}u"),
            LiteralExpression::F32(num) => write!(f, "{num}f"),
            LiteralExpression::F16(num) => write!(f, "{num}h"),
        }
    }
}

impl Display for ParenthesizedExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let expr = &self.expression;
        write!(f, "({expr})")
    }
}

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

impl Display for FunctionCall {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let ty = &self.ty;
        let args = self.arguments.iter().format(", ");
        write!(f, "{ty}({args})")
    }
}

impl Display for IdentifierExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let ident = &self.name;
        write!(f, "{ident}")
    }
}

impl Display for TypeExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let name = &self.name;
        let tplt = fmt_template(&self.template_args);
        write!(f, "{name}{tplt}")
    }
}

impl Display for TemplateArg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let expr = &self.expression;
        write!(f, "{expr}")
    }
}

fn fmt_template(tplt: &Option<Vec<TemplateArg>>) -> impl fmt::Display + '_ {
    tplt.iter().format_with("", |tplt, f| {
        f(&format_args!("<{}>", tplt.iter().format(", ")))
    })
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Void => write!(f, ";"),
            Statement::Compound(print) => write!(f, "{print}"),
            Statement::Assignment(print) => write!(f, "{print}"),
            Statement::Increment(print) => write!(f, "{print}"),
            Statement::Decrement(print) => write!(f, "{print}"),
            Statement::If(print) => write!(f, "{print}"),
            Statement::Switch(print) => write!(f, "{print}"),
            Statement::Loop(print) => write!(f, "{print}"),
            Statement::For(print) => write!(f, "{print}"),
            Statement::While(print) => write!(f, "{print}"),
            Statement::Break(print) => write!(f, "{print}"),
            Statement::Continue(print) => write!(f, "{print}"),
            Statement::Return(print) => write!(f, "{print}"),
            Statement::Discard(print) => write!(f, "{print}"),
            Statement::FunctionCall(print) => write!(f, "{print}"),
            Statement::ConstAssert(print) => write!(f, "{print}"),
            Statement::Declaration(print) => write!(f, "{print}"),
        }
    }
}

impl Display for CompoundStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        let stmts = Indent(
            self.statements
                .iter()
                // .filter(|stmt| !matches!(stmt, Statement::Void))
                .format("\n"),
        );
        write!(f, "{{\n{stmts}\n}}")
    }
}

impl Display for AssignmentStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if cfg!(feature = "attributes") {
            write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        }
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

impl Display for IncrementStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if cfg!(feature = "attributes") {
            write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        }
        let expr = &self.expression;
        write!(f, "{expr}++;")
    }
}

impl Display for DecrementStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if cfg!(feature = "attributes") {
            write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        }
        let expr = &self.expression;
        write!(f, "{expr}--;")
    }
}

impl Display for IfStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        let if_clause = &self.if_clause;
        write!(f, "{if_clause}")?;
        for else_if_clause in self.else_if_clauses.iter() {
            write!(f, "\n{else_if_clause}")?;
        }
        if let Some(else_clause) = &self.else_clause {
            write!(f, "\n{else_clause}")?;
        }
        Ok(())
    }
}

impl Display for IfClause {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let expr = &self.expression;
        let stmt = &self.body;
        write!(f, "if {expr} {stmt}")
    }
}

impl Display for ElseIfClause {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if cfg!(feature = "attributes") {
            write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        }
        let expr = &self.expression;
        let stmt = &self.body;
        write!(f, "else if {expr} {stmt}")
    }
}

impl Display for ElseClause {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if cfg!(feature = "attributes") {
            write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        }
        let stmt = &self.body;
        write!(f, "else {stmt}")
    }
}

impl Display for SwitchStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        let expr = &self.expression;
        let body_attrs = fmt_attrs(&self.body_attributes, false);
        let clauses = Indent(self.clauses.iter().format("\n"));
        write!(f, "switch {expr} {body_attrs}{{\n{clauses}\n}}")
    }
}

impl Display for SwitchClause {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if cfg!(feature = "attributes") {
            write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        }
        let cases = self.case_selectors.iter().format(", ");
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
        write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        let body_attrs = fmt_attrs(&self.body.attributes, false);
        let stmts = Indent(
            self.body
                .statements
                .iter()
                // .filter(|stmt| !matches!(stmt, Statement::Void))
                .format("\n"),
        );
        let continuing = self
            .continuing
            .iter()
            .format_with("", |cont, f| f(&format_args!("{}\n", Indent(cont))));
        write!(f, "loop {body_attrs}{{\n{stmts}\n{continuing}}}")
    }
}

impl Display for ContinuingStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if cfg!(feature = "attributes") {
            write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        }
        let body_attrs = fmt_attrs(&self.body.attributes, false);
        let stmts = Indent(
            self.body
                .statements
                .iter()
                // .filter(|stmt| !matches!(stmt, Statement::Void))
                .format("\n"),
        );
        let break_if = self
            .break_if
            .iter()
            .format_with("", |stat, f| f(&format_args!("{}\n", Indent(stat))));
        write!(f, "continuing {body_attrs}{{\n{stmts}\n{break_if}}}")
    }
}

impl Display for BreakIfStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if cfg!(feature = "attributes") {
            write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        }
        let expr = &self.expression;
        write!(f, "break if {expr};")
    }
}

impl Display for ForStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        let mut init = self
            .initializer
            .as_ref()
            .map(|stmt| format!("{}", stmt))
            .unwrap_or_default();
        if init.ends_with(';') {
            init.pop();
        }
        let cond = self
            .condition
            .iter()
            .format_with("", |expr, f| f(&format_args!("{expr}")));
        let mut updt = self
            .update
            .as_ref()
            .map(|stmt| format!("{}", stmt))
            .unwrap_or_default();
        if updt.ends_with(';') {
            updt.pop();
        }
        let body = &self.body;
        write!(f, "for ({init}; {cond}; {updt}) {body}")
    }
}

impl Display for WhileStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        let cond = &self.condition;
        let body = &self.body;
        write!(f, "while ({cond}) {body}")
    }
}

impl Display for BreakStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if cfg!(feature = "attributes") {
            write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        }
        write!(f, "break;")
    }
}

impl Display for ContinueStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if cfg!(feature = "attributes") {
            write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        }
        write!(f, "continue;")
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if cfg!(feature = "attributes") {
            write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        }
        let expr = self
            .expression
            .iter()
            .format_with("", |expr, f| f(&format_args!(" {expr}")));
        write!(f, "return{expr};")
    }
}

impl Display for DiscardStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if cfg!(feature = "attributes") {
            write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        }
        write!(f, "discard;")
    }
}

impl Display for FunctionCallStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if cfg!(feature = "attributes") {
            write!(f, "{}", fmt_attrs(&self.attributes, false))?;
        }
        let call = &self.call;
        write!(f, "{call};")
    }
}
