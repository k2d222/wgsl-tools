use lalrpop_util::{lalrpop_mod, ParseError};

pub mod ast;
pub mod lexer;

use lexer::{Error, Lexer, Span, Token};

lalrpop_mod!(pub wgsl, "/parser/wgsl.rs");

pub fn parse(source: &str) -> Result<Vec<Span>, ParseError<usize, Token, (usize, Error, usize)>> {
    let mut idents = Vec::new();
    let lexer = Lexer::new(&source);
    let parser = wgsl::TranslationUnitParser::new();
    parser.parse(&mut idents, lexer).map(|()| idents)
}
