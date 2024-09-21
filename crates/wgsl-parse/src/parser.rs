//! The [`Parser`] takes WGSL source code and returns a [syntax tree].
//!
//! [syntax tree]: syntax

lalrpop_mod!(
    #[allow(clippy::type_complexity)]
    wgsl
);
lalrpop_mod!(
    #[allow(clippy::type_complexity)]
    wgsl_recognize
);

use std::str::FromStr;

use lalrpop_util::lalrpop_mod;

use crate::{error::SpannedError, lexer::Lexer, syntax, Error};

pub struct Parser;

impl Parser {
    pub fn parse_str(source: &str) -> Result<syntax::TranslationUnit, SpannedError> {
        let lexer = Lexer::new(source);
        let parser = wgsl::TranslationUnitParser::new();
        let res = parser.parse(lexer);
        res.map_err(|e| SpannedError::new(e, source))
    }
    pub fn parse<'s>(
        mut lexer: &'s mut Lexer,
    ) -> Result<syntax::TranslationUnit, SpannedError<'s>> {
        let parser = wgsl::TranslationUnitParser::new();
        let res = parser.parse(&mut lexer);
        res.map_err(|e| SpannedError::new(e, lexer.source()))
    }
}

impl FromStr for syntax::TranslationUnit {
    type Err = Error;

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        let lexer = Lexer::new(source);
        let parser = wgsl::TranslationUnitParser::new();
        let res = parser.parse(lexer);
        res.map_err(|e| SpannedError::new(e, source).into_owned())
    }
}
impl FromStr for syntax::GlobalDirective {
    type Err = Error;

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        let lexer = Lexer::new(source);
        let parser = wgsl::GlobalDirectiveParser::new();
        let res = parser.parse(lexer);
        res.map_err(|e| SpannedError::new(e, source).into_owned())
    }
}
impl FromStr for syntax::GlobalDeclaration {
    type Err = Error;

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        let lexer = Lexer::new(source);
        let parser = wgsl::GlobalDeclParser::new();
        let res = parser.parse(lexer);
        res.map_err(|e| SpannedError::new(e, source).into_owned())
    }
}
impl FromStr for syntax::Expression {
    type Err = Error;

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        let lexer = Lexer::new(source);
        let parser = wgsl::ExpressionParser::new();
        let res = parser.parse(lexer);
        res.map_err(|e| SpannedError::new(e, source).into_owned())
    }
}
#[cfg(feature = "imports")]
impl FromStr for syntax::Import {
    type Err = Error;

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        let lexer = Lexer::new(source);
        let parser = wgsl::ImportParser::new();
        let res = parser.parse(lexer);
        res.map_err(|e| SpannedError::new(e, source).into_owned())
    }
}

impl Parser {
    pub fn recognize_str(source: &str) -> Result<(), SpannedError> {
        let lexer = Lexer::new(source);
        let parser = wgsl_recognize::TranslationUnitParser::new();
        let res = parser.parse(lexer);
        res.map_err(|e| SpannedError::new(e, source))
    }
    pub fn recognize<'s>(mut lexer: &'s mut Lexer) -> Result<(), SpannedError<'s>> {
        let parser = wgsl_recognize::TranslationUnitParser::new();
        let res = parser.parse(&mut lexer);
        res.map_err(|e| SpannedError::new(e, lexer.source()))
    }
    pub fn recognize_template_list<'s>(mut lexer: &'s mut Lexer) -> Result<(), SpannedError<'s>> {
        let parser = wgsl_recognize::TryTemplateListParser::new();
        let res = parser.parse(&mut lexer);
        res.map_err(|e| SpannedError::new(e, lexer.source()))
    }
}
