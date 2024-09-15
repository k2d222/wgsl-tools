//! The Command-line interface for the `wgsl-tools` suite.
//!
//! Very much a work in progress.

use clap::{command, Args, Parser, Subcommand, ValueEnum};
use std::{collections::HashMap, fmt::Display, fs, path::PathBuf};
use weslc::{
    CompileOptions, FileResolver, FileResource, Mangler, Resolver, FILE_MANGLER_ESCAPE,
    FILE_MANGLER_HASH, FILE_MANGLER_NONE,
};
use wgsl_parse::{syntax::TranslationUnit, Parser as WgslParser};

#[derive(Parser)]
#[command(version, author, about)]
#[command(propagate_version = true)]
struct Cli {
    /// main command
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Clone, Debug)]
enum Command {
    /// check correctness of the source file
    Check(CommonArgs),
    /// parse the source and convert it back to code from the syntax tree
    Parse(CommonArgs),
    /// output the syntax tree to stdout
    Dump(CommonArgs),
    /// compile a source file and outputs the compiled file to stdout
    Compile(CompileArgs),
}

#[derive(Args, Clone, Debug)]
struct CommonArgs {
    /// wgsl file entry-point
    input: PathBuf,
}

#[derive(Args, Clone, Debug)]
struct CompileArgs {
    #[command(flatten)]
    common: CommonArgs,
    #[arg(short, long, default_value_t = ManglerKind::Escape)]
    /// name mangling strategy
    mangler: ManglerKind,
    #[arg(long)]
    /// disable imports
    no_imports: bool,
    #[arg(long)]
    /// disable conditional compilation
    no_cond_comp: bool,
    /// conditional compilation features to enable
    #[arg(long)]
    enable_features: Vec<String>,
    /// conditional compilation features to disable
    #[arg(long)]
    disable_features: Vec<String>,
}

#[derive(Default, Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum ManglerKind {
    /// escaped path mangler  foo/bar/{item} -> foo_bar_item
    #[default]
    Escape,
    /// hash mangler          foo/bar/{item} -> item_1985638328947
    Hash,
    /// disable mangling (warning: will break if case of name conflicts!)
    None,
}

impl Display for ManglerKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ManglerKind::Escape => f.write_str("escape"),
            ManglerKind::Hash => f.write_str("hash"),
            ManglerKind::None => f.write_str("none"),
        }
    }
}

#[derive(Clone, Debug, thiserror::Error)]
enum CliError {
    #[error("input file not found")]
    FileNotFound,
    #[error("{0}")]
    CompileError(#[from] weslc::Error),
}

fn make_mangler(kind: ManglerKind) -> &'static dyn Mangler<FileResource> {
    match kind {
        ManglerKind::Escape => &FILE_MANGLER_ESCAPE,
        ManglerKind::Hash => &FILE_MANGLER_HASH,
        ManglerKind::None => &FILE_MANGLER_NONE,
    }
}

fn run_compile(args: &CompileArgs) -> Result<TranslationUnit, CliError> {
    let base = args
        .common
        .input
        .parent()
        .ok_or(CliError::FileNotFound)?
        .to_path_buf();
    let name = PathBuf::from(
        args.common
            .input
            .file_name()
            .ok_or(CliError::FileNotFound)?,
    );

    let resolver = FileResolver::new(base);
    let entrypoint = resolver.resolve_path(&name, None);

    let mangler = make_mangler(args.mangler);

    let mut features = HashMap::new();
    features.extend(args.enable_features.iter().map(|f| (f.clone(), true)));
    features.extend(args.disable_features.iter().map(|f| (f.clone(), false)));

    let compile_options = CompileOptions {
        use_imports: !args.no_imports,
        use_condcomp: !args.no_cond_comp,
        features,
    };

    let wgsl = weslc::compile(&entrypoint, resolver, mangler, &compile_options)?;
    Ok(wgsl)
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Command::Check(args) | Command::Parse(args) | Command::Dump(args) => {
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
                        Ok(module) => {
                            println!("{module}")
                        }
                        Err(err) => eprintln!("{err}"),
                    };
                }
                Command::Dump(_) => {
                    match WgslParser::parse_str(&source) {
                        Ok(module) => println!("{module:?}"),
                        Err(err) => eprintln!("{err}"),
                    };
                }
                _ => unreachable!(),
            }
        }
        Command::Compile(args) => {
            match run_compile(args) {
                Ok(module) => println!("{module}"),
                Err(err) => eprintln!("{err}"),
            };
        }
    };
}
