pub mod ast;
pub mod ast_impl;
pub mod lexer;
mod parser_support;
pub mod print;
pub mod span;

use lalrpop_util::{lalrpop_mod, ParseError};
use lexer::{Lexer, Token};
use thiserror::Error;

use self::ast::TranslationUnit;

#[derive(Error, Clone, Debug, Default, PartialEq)]
pub enum Error {
    #[default]
    #[error("lexer error")]
    LexerError,
    #[error("invalid diagnostic severity")]
    ParseDiagnosticSeverity,
}

lalrpop_mod!(pub wgsl_recognize, "/parser/wgsl_recognize.rs");
lalrpop_mod!(pub wgsl_spanned, "/parser/wgsl_spanned.rs");

pub fn parse_recognize(
    source: &str,
) -> Result<(), ParseError<usize, Token, (usize, Error, usize)>> {
    let lexer = Lexer::new(&source);
    let parser = wgsl_recognize::TranslationUnitParser::new();
    parser.parse(lexer)
}

pub fn parse_spanned(
    source: &str,
) -> Result<TranslationUnit, ParseError<usize, Token, (usize, Error, usize)>> {
    let lexer = Lexer::new(&source);
    let parser = wgsl_spanned::TranslationUnitParser::new();
    parser.parse(lexer)
}
