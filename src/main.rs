mod imports;
mod parser;

use clap::Parser;
use itertools::Itertools;
use lalrpop_util::{lexer::Token, ParseError};
use std::{error::Error, fs, path::PathBuf};

#[derive(Parser, Debug)]
#[command(version = "0.1", author = "Mathis Brossier", about = "")]
struct Cli {
    input: PathBuf,
}

fn print_type_of<T>(_: &T) {
    println!("{}", std::any::type_name::<T>())
}

fn panic_parse_error(err: ParseError<usize, Token, &str>, source: &str) {
    use annotate_snippets::*;
    let renderer = Renderer::styled();
    let rendered = match err {
        ParseError::InvalidToken { location } => {
            let end = (location + 1..)
                .find(|e| source.is_char_boundary(*e))
                .unwrap_or(location);
            let message = Level::Error.title("invalid token").snippet(
                Snippet::source(source).fold(true).annotation(
                    Level::Error
                        .span(location..end)
                        .label("this token is unknown"),
                ),
            );
            format!("{}", renderer.render(message))
        }
        ParseError::UnrecognizedEof { location, expected } => {
            let annot = format!("expected {}", expected.iter().format(", "),);
            let message = Level::Error.title("unexpected end of file").snippet(
                Snippet::source(source)
                    .fold(true)
                    .annotation(Level::Error.span(location..location).label(&annot)),
            );
            format!("{}", renderer.render(message))
        }
        ParseError::UnrecognizedToken { token, expected } => {
            let title = format!("unexpected token `{}`", &source[token.0..token.2]);
            let annot = format!(
                "expected {}, found {}",
                expected.iter().format(", "),
                token.1
            );
            let message = Level::Error.title(&title).snippet(
                Snippet::source(source)
                    .fold(true)
                    .annotation(Level::Error.span(token.0..token.2).label(&annot)),
            );
            format!("{}", renderer.render(message))
        }
        ParseError::ExtraToken { token } => {
            let title = format!("extra token `{}`", &source[token.0..token.2]);
            let annot = format!("extra {} here", token.1);
            let message = Level::Error.title(&title).snippet(
                Snippet::source(source)
                    .fold(true)
                    .annotation(Level::Error.span(token.0..token.2).label(&annot)),
            );
            format!("{}", renderer.render(message))
        }
        ParseError::User { error } => {
            let message = Level::Error.title(error);
            format!("{}", renderer.render(message))
        }
    };

    panic!("{rendered}");
}

fn main() {
    // let mut parser = tree_sitter::Parser::new();
    // parser.set_language(&tree_sitter_wesl::language()).unwrap();

    let cli = Cli::parse();

    let parser = parser::parser::TranslationUnitParser::new();

    let source = fs::read_to_string(&cli.input).expect("could not open input file");
    let mut idents = Vec::new();
    let ast = match parser.parse(&mut idents, &source) {
        Ok(ast) => ast,
        Err(err) => panic_parse_error(err, &source),
    };
    print_type_of(&ast);
    // println!("{ast:?}");
    let idents = idents
        .into_iter()
        .map(|span| &source[span])
        .collect::<Vec<_>>();
    println!("{idents:?}");

    // let source = fs::read_to_string(&cli.input).expect("could not open input file");
    // let tree = parser.parse(&source, None).expect("parse failure");
    // println!("{tree:?}")
    // let source = include_str!("test.wgsl");
    // println!("source: {source}");

    // let res = imports::run(&cli.input);
    // println!("{res:?}");
}
