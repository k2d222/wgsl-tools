//! A parser for WGSL files, written directly from the [specification] with [lalrpop].
//!
//! # Parsing a source file
//!
//! ```rust
//! # use wgsl_parse::syntax::*;
//! let source = "@fragment fn frag_main() -> @location(0) vec4f { return vec4(1); }";
//! let parsed = wgsl_parse::Parser::parse_str(source).unwrap();
//!
//! let compare = TranslationUnit {
//!     global_directives: vec![],
//!     global_declarations: vec![GlobalDeclaration::Function(Function {
//!         attributes: vec![Attribute {
//!             name: "fragment".to_string(),
//!             arguments: None
//!         }],
//!         name: "frag_main".to_string(),
//!         parameters: vec![],
//!         return_attributes: vec![Attribute {
//!             name: "location".to_string(),
//!             arguments: Some(vec![Expression::Literal(LiteralExpression::AbstractInt(0))])
//!         }],
//!         return_type: Some(TypeExpression {
//!             name: "vec4f".to_string(),
//!             template_args: None
//!         }),
//!         body: CompoundStatement {
//!             attributes: vec![],
//!             statements: vec![Statement::Return(Some(Expression::FunctionCall(
//!                 FunctionCallExpression {
//!                     name: "vec4".to_string(),
//!                     template_args: None,
//!                     arguments: vec![Expression::Literal(LiteralExpression::AbstractInt(1))]
//!                 }
//!             )))]
//!         }
//!     })]
//! };
//!
//! assert_eq!(parsed, compare);
//! ```
//!
//! # Syntax tree
//!
//! See [syntax tree].
//!
//! Modifying the syntax tree:
//! ```rust
//!     let source = "const hello = 0u;";
//!     let mut module = wgsl_parse::Parser::parse_str(source).unwrap();
//!
//!     // modify the module as needed...
//!     let decl = &mut module
//!         .global_declarations
//!         .iter_mut()
//!         .find_map(|decl| match decl {
//!             wgsl_parse::syntax::GlobalDeclaration::Declaration(decl) => Some(decl),
//!             _ => None,
//!         })
//!         .unwrap();
//!     decl.name = "world".to_string();
//!
//!     assert_eq!(format!("{module}").trim(), "const world = 0u;");
//! ```
//!
//! # Stringification
//!
//! The syntax tree elements implement [`Display`][std::fmt::Display].
//! The display is always pretty-printed.
//!
//! TODO: implement :# for pretty vs. inline formatting.
//!
//! ```rust
//! let source = "@fragment fn frag_main() -> @location(0) vec4f { return vec4(1); }";
//! let mut module = wgsl_parse::Parser::parse_str(source).unwrap();
//!
//! // modify the module as needed...
//!
//! println!("{module}");
//! ```
//!
//! [lalrpop]: https://lalrpop.github.io/lalrpop/
//! [specification]: https://www.w3.org/TR/WGSL/

pub mod error;
pub mod lexer;
pub mod parser;
pub mod syntax;

mod parser_support;
mod syntax_display;
mod syntax_impl;

pub use lexer::Lexer;
pub use parser::Parser;
