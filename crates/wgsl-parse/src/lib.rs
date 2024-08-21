//! A parser for WGSL files, written directly from the [specification] with lalrpop.
//!
//! # Parsing a source file
//!
//! ```rust
//! let source = "@fragment fn frag_main() -> @location(0) vec4f { return vec4(1); }";
//! let module = wgsl_parse::Parser::parse_str(source);
//! println!("{module:?}");
//! ```
//!
//! # Syntax tree
//!
//! see [syntax tree]
//!
//! TODO: example how to create a module manually.
//!
//! # Stringification
//!
//! The syntax tree elements implement [`Display`][std::fmt::Display].
//!
//! ```rust
//! let source = "@fragment fn frag_main() -> @location(0) vec4f { return vec4(1); }";
//! let module = wgsl_parse::Parser::parse_str(source);
//! // modify the module as needed...
//! println!("{module}");
//! ```
//!
//! [specification]: https://www.w3.org/TR/WGSL/
//! [syntax tree]: syntax
//! [spanned syntax tree]: syntax_spanned

pub mod error;
pub mod lexer;
pub mod parser;
pub mod span;
pub mod syntax;
pub mod syntax_spanned;

mod parser_support;
mod parser_support_spanned;
mod syntax_display;
mod syntax_display_spanned;
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
