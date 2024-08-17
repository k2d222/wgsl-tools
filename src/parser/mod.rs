use lalrpop_util::{lalrpop_mod, ParseError};

pub mod ast;
pub mod lexer;

use lexer::{Error, Lexer, Span, Token};

use self::ast::TranslationUnit;

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
