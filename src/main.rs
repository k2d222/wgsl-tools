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
    eval::{Eval, EvalAttrs, EvalError, HostShareable, Instance, RefInstance},
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
    data: Box<[u8]>,
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
                data: {
                    let path = PathBuf::from(it.next().ok_or("missing data")?);
                    let mut file = File::open(&path).expect("failed to open binding file");
                    let mut buf = Vec::new();
                    file.read_to_end(&mut buf)
                        .expect("failed to read binding file");
                    buf.into_boxed_slice()
                },
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
    #[error("binding `@group({0}) @binding({1})` not found")]
    BindingNotFound(u32, u32),
    #[error(
        "binding `@group({0}) @binding({1})` ({2} bytes) incompatible with type `{3}` ({4} bytes)"
    )]
    BindingIncompatible(u32, u32, u32, wesl::eval::Type, u32),
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
        use_stripping: !args.no_strip,
        entry_points: args.entry_points.clone(),
        features,
    };

    if !args.no_sourcemap {
        let (wgsl, sourcemap) =
            wesl::compile_sourcemap(&entrypoint, &resolver, &mangler, &compile_options);
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
    let mut ctx = wesl::eval::Context::new(wgsl);

    let ty_expr = wgsl
        .global_declarations
        .iter()
        .find_map(|d| match d {
            syntax::GlobalDeclaration::Declaration(d) => {
                let (group, binding) = d.eval_group_binding(&mut ctx).ok()?;
                if group == b.group && binding == b.binding {
                    d.ty.clone()
                } else {
                    None
                }
            }
            _ => None,
        })
        .ok_or_else(|| CliError::BindingNotFound(b.group, b.binding))?;

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
                    .with_source(ty_expr.to_string()),
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
    let inst = Instance::from_buffer(&b.data, &ty, &mut ctx).ok_or_else(|| {
        CliError::BindingIncompatible(
            b.group,
            b.binding,
            b.data.len() as u32,
            ty.clone(),
            ty.size_of(&mut ctx).unwrap_or_default(),
        )
    })?;
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

fn run_eval(args: &EvalArgs) -> Result<(Instance, Vec<Binding>), CliError> {
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

    let expr = args
        .expr
        .parse::<syntax::Expression>()
        .map_err(|e| wesl::Error::Error(Diagnostic::from(e).with_source(args.expr.to_string())))?;

    let (res, mut ctx) = if args.runtime {
        wesl::eval_runtime(&expr, &wgsl, bindings, overrides)
    } else {
        wesl::eval_const(&expr, &wgsl)
    };

    let res = res.map_err(|e| {
        Diagnostic::from(e)
            .with_source(args.expr.clone())
            .with_ctx(&ctx)
    });
    let inst = if let Some(sourcemap) = sourcemap {
        res.map_err(|e| wesl::Error::Error(e.with_sourcemap(&sourcemap)))
    } else {
        res.map_err(|e| wesl::Error::Error(e))
    }?;

    let bindings = args
        .bindings
        .iter()
        .filter_map(|b| {
            let inst = ctx.binding(b.group, b.binding)?.clone();
            let buf = inst.read().ok()?.to_buffer(&mut ctx)?;
            Some(Binding {
                group: b.group,
                binding: b.binding,
                kind: b.kind,
                data: buf.into(),
            })
        })
        .collect();

    Ok((inst, bindings))
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
                Ok((inst, bindings)) => println!("{inst}\n{bindings:?}"),
                Err(err) => eprintln!("{err}"),
            };
        }
    };
}
