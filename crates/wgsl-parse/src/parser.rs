//! The [`Parser`] takes WGSL source code and returns a [syntax tree].
//!
//! [syntax tree]: syntax

use std::str::FromStr;

use lalrpop_util::lalrpop_mod;

use crate::{error::Error, lexer::Lexer, syntax};

lalrpop_mod!(
    #[allow(clippy::type_complexity)]
    wgsl
);
lalrpop_mod!(
    #[allow(clippy::type_complexity)]
    wgsl_recognize
);

pub struct Parser;

impl Parser {
    pub fn parse_str(source: &str) -> Result<syntax::TranslationUnit, Error> {
        let lexer = Lexer::new(source);
        let parser = wgsl::TranslationUnitParser::new();
        parser.parse(lexer).map_err(Into::into)
    }
    pub fn parse(lexer: &mut Lexer) -> Result<syntax::TranslationUnit, Error> {
        let parser = wgsl::TranslationUnitParser::new();
        parser.parse(lexer).map_err(Into::into)
    }
}

impl Parser {
    pub fn recognize_str(source: &str) -> Result<(), Error> {
        let lexer = Lexer::new(source);
        let parser = wgsl_recognize::TranslationUnitParser::new();
        parser.parse(lexer).map_err(Into::into)
    }
    pub fn recognize(lexer: &mut Lexer) -> Result<(), Error> {
        let parser = wgsl_recognize::TranslationUnitParser::new();
        parser.parse(lexer).map_err(Into::into)
    }
    pub fn recognize_template_list(lexer: &mut Lexer) -> Result<(), Error> {
        let parser = wgsl_recognize::TryTemplateListParser::new();
        parser.parse(lexer).map_err(Into::into)
    }
}

impl FromStr for syntax::TranslationUnit {
    type Err = Error;

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        let lexer = Lexer::new(source);
        let parser = wgsl::TranslationUnitParser::new();
        parser.parse(lexer).map_err(Into::into)
    }
}
impl FromStr for syntax::GlobalDirective {
    type Err = Error;

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        let lexer = Lexer::new(source);
        let parser = wgsl::GlobalDirectiveParser::new();
        parser.parse(lexer).map_err(Into::into)
    }
}
impl FromStr for syntax::GlobalDeclaration {
    type Err = Error;

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        let lexer = Lexer::new(source);
        let parser = wgsl::GlobalDeclParser::new();
        parser.parse(lexer).map_err(Into::into)
    }
}
impl FromStr for syntax::Statement {
    type Err = Error;

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        let lexer = Lexer::new(source);
        let parser = wgsl::StatementParser::new();
        parser.parse(lexer).map_err(Into::into)
    }
}
impl FromStr for syntax::Expression {
    type Err = Error;

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        let lexer = Lexer::new(source);
        let parser = wgsl::ExpressionParser::new();
        parser.parse(lexer).map_err(Into::into)
    }
}
#[cfg(feature = "imports")]
impl FromStr for syntax::Import {
    type Err = Error;

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        let lexer = Lexer::new(source);
        let parser = wgsl::ImportParser::new();
        parser.parse(lexer).map_err(Into::into)
    }
}
