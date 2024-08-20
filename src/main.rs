//! The Command-line interface for the `wgsl-tools` suite.
//!
//! Very much a work in progress.

use clap::{Args, Parser, Subcommand};
use std::{fs, path::PathBuf};
use wgsl_parse::Parser as WgslParser;

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
            match WgslParser::recognize_str(&source) {
                Ok(()) => println!("OK"),
                Err(err) => eprintln!("{err}"),
            };
        }
        Command::Parse(_) => {
            match WgslParser::parse_str(&source) {
                Ok(ast) => {
                    wgsl_parse::print::print(&ast, &source);
                }
                Err(err) => eprintln!("{err}"),
            };
        }
        Command::Dump(_) => {
            match WgslParser::parse_str(&source) {
                Ok(ast) => println!("{ast:?}"),
                Err(err) => eprintln!("{err}"),
            };
        }
    }
}
