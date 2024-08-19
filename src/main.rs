// mod import;
mod parse;

use clap::{Args, Parser, Subcommand};
use itertools::Itertools;
use lalrpop_util::ParseError;
use parse::lexer::Token;
use std::{fs, path::PathBuf};

#[derive(Parser)]
#[command(version, author, about)]
#[command(propagate_version = true)]
struct Cli {
    /// main command
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// check correctness of the source file
    Check(CommonArgs),
    /// parse the source and convert it back to code from the syntax tree.
    Parse(CommonArgs),
    /// output the syntax tree to stdout
    Dump(CommonArgs),
}

#[derive(Args)]
struct CommonArgs {
    /// wgsl file entry-point
    input: PathBuf,
}

fn panic_parse_error(
    err: ParseError<usize, Token, (usize, parse::Error, usize)>,
    source: &str,
) -> ! {
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
            let message = Level::Error.title("invalid token").snippet(
                Snippet::source(source).fold(true).annotation(
                    Level::Error
                        .span(error.0..error.2)
                        .label("this token is invalid"),
                ),
            );
            format!("{}", renderer.render(message))
        }
    };

    panic!("{rendered}");
}

fn main() {
    let cli = Cli::parse();

    let args = match &cli.command {
        Command::Check(args) => args,
        Command::Parse(args) => args,
        Command::Dump(args) => args,
    };

    let source = fs::read_to_string(&args.input).expect("could not open input file");

    match &cli.command {
        Command::Check(_) => {
            print!("{} -- ", args.input.display());
            match parse::parse_recognize(&source) {
                Ok(ast) => ast,
                Err(err) => panic_parse_error(err, &source),
            };
            println!("OK");
        }
        Command::Parse(_) => {
            let ast = match parse::parse_spanned(&source) {
                Ok(ast) => ast,
                Err(err) => panic_parse_error(err, &source),
            };
            parse::print::print(&ast, &source);
        }
        Command::Dump(_) => {
            let ast = match parse::parse_spanned(&source) {
                Ok(ast) => ast,
                Err(err) => panic_parse_error(err, &source),
            };
            println!("{ast:?}")
        }
    }
}
