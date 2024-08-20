//! A parser for WGSL files, written directly from the [specification] with lalrpop.
//!
//! # Parsing
//!
//! ```rust
//! let source = "@fragment fn frag_main() -> @location(0) vec4f { return vec4(1); }";
//! let module = parser::parse(source);
//! println!("{module:?}");
//! ```
//!
//! # Syntax tree
//!
//! The [syntax tree] was designed with [`Span`s][span::Span] a bit everywhere. These spans
//! allow referring to the source `&str` without a lifetime bind.
//! In the future, different variants of the [`Span`][span::Span] will allow modifying the
//! Source code while preserving correct syntax tree spans. (TODO)
//!
//! # Parsing variants
//!
//! ## recognize
//!
//! Parsing exists in two flavors: *recognize* and *spanned*. The *recognize* version
//! simply ensures that the input file has no syntax errors. The *spanned* version returns
//! a full [syntax tree].
//!
//! # Stringify
//!
//! To be documented (TODO)
//!
//! [specification]: https://www.w3.org/TR/WGSL/
//! [syntax tree]: syntax

pub mod error;
pub mod lexer;
pub mod parser;
pub mod print;
pub mod span;
pub mod syntax;

mod parser_support;
mod syntax_impl;

pub use lexer::Lexer;
pub use parser::Parser;

// pub fn parse_recognize(
//     source: &str,
// ) -> Result<(), ParseError<usize, Token, (usize, Error, usize)>> {
//     let lexer = Lexer::new(&source);
//     let parser = wgsl_recognize::TranslationUnitParser::new();
//     parser.parse(lexer)
// }

// pub fn parse_spanned(
//     source: &str,
// ) -> Result<TranslationUnit, ParseError<usize, Token, (usize, Error, usize)>> {
//     let lexer = Lexer::new(&source);
//     let parser = wgsl_spanned::TranslationUnitParser::new();
//     parser.parse(lexer)
// }
