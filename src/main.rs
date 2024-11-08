//! The Command-line interface for the `wgsl-tools` suite.
//!
//! Very much a work in progress.

use clap::{command, Args, Parser, Subcommand, ValueEnum};
use std::{
    collections::HashMap,
    error::Error,
    fmt::Display,
    fs::{self, File},
    io::Read,
    path::PathBuf,
    str::FromStr,
};
use wesl::{
    eval::{Eval, EvalError, Instance, RefInstance},
    syntax::{self, AccessMode, AddressSpace},
    BasicSourceMap, CompileOptions, Diagnostic, FileResolver, Mangler, Resource, MANGLER_ESCAPE,
    MANGLER_HASH, MANGLER_NONE,
};
use wgsl_parse::{error::FormatError, syntax::TranslationUnit, Parser as WgslParser};

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
    /// evaluate a const expression
    Eval(EvalArgs),
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
    /// show nicer error messages by computing a sourcemap
    #[arg(long)]
    no_sourcemap: bool,
    /// disable imports
    #[arg(long)]
    no_imports: bool,
    /// disable conditional compilation
    #[arg(long)]
    no_cond_comp: bool,
    /// disable generics
    #[arg(long)]
    no_generics: bool,
    /// disable stripping unused declarations
    #[arg(long)]
    no_strip: bool,
    /// exposed shader entry-points
    #[arg(long)]
    entry_points: Option<Vec<String>>,
    /// conditional compilation features to enable
    #[arg(long)]
    enable_features: Vec<String>,
    /// conditional compilation features to disable
    #[arg(long)]
    disable_features: Vec<String>,
}

// from clap cookbook: https://docs.rs/clap/latest/clap/_derive/_cookbook/typed_derive/index.html
fn parse_key_val<T, U>(s: &str) -> Result<(T, U), Box<dyn Error + Send + Sync + 'static>>
where
    T: FromStr,
    T::Err: Error + Send + Sync + 'static,
    U: FromStr,
    U::Err: Error + Send + Sync + 'static,
{
    let pos = s
        .find('=')
        .ok_or_else(|| format!("invalid KEY=value: no `=` found in `{}`", s))?;
    Ok((s[..pos].parse()?, s[pos + 1..].parse()?))
}

/// reference: https://gpuweb.github.io/gpuweb/#binding-type
#[derive(Clone, Copy, Debug)]
enum BindingType {
    Uniform,
    Storage,
    ReadOnlyStorage,
    Filtering,
    NonFiltering,
    Comparison,
    Float,
    UnfilterableFloat,
    Sint,
    Uint,
    Depth,
    WriteOnly,
    ReadWrite,
    ReadOnly,
}

impl FromStr for BindingType {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "uniform" => Ok(Self::Uniform),
            "storage" => Ok(Self::Storage),
            "read-only-storage" => Ok(Self::ReadOnlyStorage),
            "filtering" => Ok(Self::Filtering),
            "non-filtering" => Ok(Self::NonFiltering),
            "comparison" => Ok(Self::Comparison),
            "float" => Ok(Self::Float),
            "unfilterable-float" => Ok(Self::UnfilterableFloat),
            "sint" => Ok(Self::Sint),
            "uint" => Ok(Self::Uint),
            "depth" => Ok(Self::Depth),
            "write-only" => Ok(Self::WriteOnly),
            "read-write" => Ok(Self::ReadWrite),
            "read-only" => Ok(Self::ReadOnly),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Debug)]
struct Binding {
    group: u32,
    binding: u32,
    kind: BindingType,
    ty: String,
    data: PathBuf,
}

impl FromStr for Binding {
    type Err = Box<dyn Error + Send + Sync + 'static>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut it = s.split(':');
        let binding = (|| {
            Ok(Binding {
                group: it
                    .next()
                    .ok_or("missing @group number")?
                    .parse()
                    .map_err(|e| format!("failed to parse group: {e}"))?,
                binding: it
                    .next()
                    .ok_or("missing @binding number")?
                    .parse()
                    .map_err(|e| format!("failed to parse binding: {e}"))?,
                kind: it
                    .next()
                    .ok_or("missing resource binding type")?
                    .parse()
                    .map_err(|()| format!("invalid resource binding type"))?,
                ty: it
                    .next()
                    .ok_or("missing wgsl type")?
                    .parse()
                    .map_err(|e| format!("failed to parse wgsl type: {e}"))?,
                data: PathBuf::from(it.next().ok_or("missing data")?),
            })
        })();
        binding.map_err(|e: String| format!("failed to parse binding: {e}").into())
    }
}

#[derive(Args, Clone, Debug)]
struct EvalArgs {
    /// context to evaluate the expression into
    #[command(flatten)]
    compile: CompileArgs,
    /// run the eval() to at shader-execution-time instead of at pipeline-creation-time
    #[arg(long)]
    runtime: bool,
    /// the expression to evaluate
    expr: String,
    /// bindings. Only `Uniform` and `buffer` bindings are supported at the moment.
    /// syntax: colon-separated group,binding,binding_type,wgsl_type,path
    ///  * group and binding are @group and @binding numbers
    ///  * binding_type is the `GPU*BindingType`
    ///  * path is a path to a binary file of the buffer contents.
    /// example: 0:0:storage:array<vec3<u32>,5>:./my_buffer.bin
    #[arg(long, value_parser = Binding::from_str)]
    bindings: Vec<Binding>,
    #[arg(long, value_parser = parse_key_val::<String, String>)]
    overrides: Vec<(String, String)>,
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
    CompileError(#[from] wesl::Error),
}

fn make_mangler(kind: ManglerKind) -> Box<dyn Mangler> {
    match kind {
        ManglerKind::Escape => Box::new(MANGLER_ESCAPE),
        ManglerKind::Hash => Box::new(MANGLER_HASH),
        ManglerKind::None => Box::new(MANGLER_NONE),
    }
}

fn run_compile(args: &CompileArgs) -> Result<(TranslationUnit, Option<BasicSourceMap>), CliError> {
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
    let entrypoint: Resource = name.into();

    let mangler = make_mangler(args.mangler);

    let mut features = HashMap::new();
    features.extend(args.enable_features.iter().map(|f| (f.clone(), true)));
    features.extend(args.disable_features.iter().map(|f| (f.clone(), false)));

    let compile_options = CompileOptions {
        use_imports: !args.no_imports,
        use_condcomp: !args.no_cond_comp,
        use_generics: !args.no_generics,
        strip: !args.no_strip,
        entry_points: args.entry_points.clone(),
        features,
    };

    if !args.no_sourcemap {
        let (wgsl, sourcemap) =
            wesl::compile_with_sourcemap(&entrypoint, &resolver, &mangler, &compile_options);
        Ok((wgsl?, Some(sourcemap)))
    } else {
        let wgsl = wesl::compile(&entrypoint, &resolver, &mangler, &compile_options)?;
        Ok((wgsl, None))
    }
}

fn parse_binding(
    b: &Binding,
    wgsl: &TranslationUnit,
) -> Result<((u32, u32), RefInstance), CliError> {
    let mut file = File::open(&b.data).expect("failed to open binding file");
    let mut buf = Vec::new();
    file.read_to_end(&mut buf)
        .expect("failed to read binding file");
    let ty_expr =
        b.ty.parse::<syntax::Expression>()
            .map_err(|e| Diagnostic::from(e).with_source(b.ty.to_string()))
            .map_err(wesl::Error::Error)?;
    let mut ctx = wesl::eval::Context::new(wgsl);
    let ty = ty_expr
        .eval_value(&mut ctx)
        .and_then(|inst| match inst {
            Instance::Type(ty) => Ok(ty),
            _ => Err(EvalError::UnknownType(inst.to_string())),
        })
        .map_err(|e| {
            wesl::Error::Error(
                Diagnostic::from(e)
                    .with_ctx(&ctx)
                    .with_source(b.ty.to_string()),
            )
        })?;
    let (storage, access) = match b.kind {
        BindingType::Uniform => (AddressSpace::Uniform, AccessMode::Read),
        BindingType::Storage => (
            AddressSpace::Storage(Some(AccessMode::ReadWrite)),
            AccessMode::ReadWrite,
        ),
        BindingType::ReadOnlyStorage => (
            AddressSpace::Storage(Some(AccessMode::Read)),
            AccessMode::Read,
        ),
        BindingType::Filtering => todo!(),
        BindingType::NonFiltering => todo!(),
        BindingType::Comparison => todo!(),
        BindingType::Float => todo!(),
        BindingType::UnfilterableFloat => todo!(),
        BindingType::Sint => todo!(),
        BindingType::Uint => todo!(),
        BindingType::Depth => todo!(),
        BindingType::WriteOnly => todo!(),
        BindingType::ReadWrite => todo!(),
        BindingType::ReadOnly => todo!(),
    };
    let inst = Instance::from_buffer(&buf, &ty, &mut ctx)
        .ok_or_else(|| EvalError::NotConstructible(ty))
        .map_err(|e| wesl::Error::Error(Diagnostic::from(e)))?;
    Ok((
        (b.group, b.binding),
        RefInstance::from_instance(inst, storage, access),
    ))
}

fn parse_override(src: &str, wgsl: &TranslationUnit) -> Result<Instance, CliError> {
    let mut ctx = wesl::eval::Context::new(wgsl);
    let expr = src
        .parse::<syntax::Expression>()
        .map_err(|e| wesl::Error::Error(Diagnostic::from(e).with_source(src.to_string())))?;
    let inst = expr.eval_value(&mut ctx).map_err(|e| {
        wesl::Error::Error(
            Diagnostic::from(e)
                .with_ctx(&ctx)
                .with_source(src.to_string()),
        )
    })?;
    Ok(inst)
}

fn run_eval(args: &EvalArgs) -> Result<Instance, CliError> {
    let (wgsl, sourcemap) = run_compile(&args.compile)?;

    let bindings = args
        .bindings
        .iter()
        .map(|b| parse_binding(b, &wgsl))
        .collect::<Result<_, _>>()?;

    let overrides = args
        .overrides
        .iter()
        .map(|(name, expr)| -> Result<(String, Instance), CliError> {
            Ok((name.to_string(), parse_override(expr, &wgsl)?))
        })
        .collect::<Result<_, _>>()?;

    let inst = (|| {
        let expr = args
            .expr
            .parse::<syntax::Expression>()
            .map_err(|e| Diagnostic::from(e).with_source(args.expr.to_string()))?;

        let (res, mut ctx) = if args.runtime {
            wesl::eval_runtime(&expr, &wgsl, bindings, overrides)
        } else {
            wesl::eval_const(&expr, &wgsl)
        };

        // TODO: remove this
        for b in &args.bindings {
            let binding = ctx.binding(b.group, b.binding).unwrap();
            let binding = Instance::Ref(binding.clone()).eval_value(&mut ctx).unwrap();
            println!("{}, {} = {binding}", b.group, b.binding);
        }

        let res = res.map_err(|e| {
            Diagnostic::from(e)
                .with_source(args.expr.clone())
                .with_ctx(&ctx)
        });
        if let Some(sourcemap) = sourcemap {
            res.map_err(|e| e.with_sourcemap(&sourcemap))
        } else {
            res
        }
    })()
    .map_err(|e| CliError::CompileError(wesl::Error::Error(e)))?;

    Ok(inst)
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
                        Err(err) => eprintln!("{}", err.with_source((&source).into())),
                    };
                }
                Command::Parse(_) => {
                    match WgslParser::parse_str(&source) {
                        Ok(module) => {
                            println!("{module}")
                        }
                        Err(err) => eprintln!("{}", err.with_source((&source).into())),
                    };
                }
                Command::Dump(_) => {
                    match WgslParser::parse_str(&source) {
                        Ok(module) => println!("{module:?}"),
                        Err(err) => eprintln!("{}", err.with_source((&source).into())),
                    };
                }
                _ => unreachable!(),
            }
        }
        Command::Compile(args) => {
            match run_compile(args) {
                Ok(module) => println!("{}", module.0),
                Err(err) => eprintln!("{err}"),
            };
        }
        Command::Eval(args) => {
            match run_eval(args) {
                Ok(module) => println!("{module}"),
                Err(err) => eprintln!("{err}"),
            };
        }
    };
}
