//! The Command-line interface for `wesl-rs`.

use clap::{command, Args, Parser, Subcommand, ValueEnum};
use std::{
    collections::HashMap,
    error::Error,
    fs::{self, File},
    io::{Read, Write},
    path::PathBuf,
    str::FromStr,
};
use wesl::{
    eval::{Eval, EvalAttrs, EvalError, HostShareable, Instance, RefInstance, Ty},
    syntax::{self, AccessMode, AddressSpace},
    CompileOptions, CompileResult, Diagnostic, FileResolver, ManglerKind, PkgBuilder, Router,
    SyntaxUtil, VirtualResolver, Wesl,
};
use wgsl_parse::{syntax::TranslationUnit, Parser as WgslParser};

#[derive(Parser)]
#[command(version, author, about)]
#[command(propagate_version = true)]
struct Cli {
    /// Main command
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Clone, Debug)]
enum Command {
    /// Check correctness of the source file
    Check(CheckArgs),
    /// Parse the source and convert it back to code from the syntax tree
    // Parse(CommonArgs),
    // /// Output the syntax tree to stdout
    // Dump(CommonArgs),
    // /// Compile a source file and outputs the compiled file to stdout
    Compile(CompileArgs),
    /// Evaluate a const-expression
    Eval(EvalArgs),
    /// Execute a WGSL shader function on the CPU
    Exec(ExecArgs),
    /// Generate a publishable Cargo package from WESL source code
    Package(PkgArgs),
}

#[derive(Default, Clone, Copy, Debug, ValueEnum)]
pub enum ClapManglerKind {
    /// Escaped path mangler. `foo/bar/{item} -> foo_bar_item`
    #[default]
    Escape,
    /// Hash mangler. `foo/bar/{item} -> item_1985638328947`
    Hash,
    /// Make valid identifiers with unicode "confusables" characters.
    /// `foo/{bar<baz, moo>} -> foo::barᐸbazˏmooᐳ`
    Unicode,
    /// Disable mangling (warning: will break if case of name conflicts!)
    None,
}

impl From<ClapManglerKind> for ManglerKind {
    fn from(value: ClapManglerKind) -> Self {
        match value {
            ClapManglerKind::Escape => ManglerKind::Escape,
            ClapManglerKind::Hash => ManglerKind::Hash,
            ClapManglerKind::Unicode => ManglerKind::Unicode,
            ClapManglerKind::None => ManglerKind::None,
        }
    }
}

#[derive(Args, Clone, Debug)]
struct CompOptsArgs {
    /// Name mangling strategy
    #[arg(long, default_value = "escape")]
    mangler: ClapManglerKind,
    /// Show nicer error messages by computing a sourcemap
    #[arg(long)]
    no_sourcemap: bool,
    /// Disable imports
    #[arg(long)]
    no_imports: bool,
    /// Disable conditional compilation
    #[arg(long)]
    no_cond_comp: bool,
    /// Disable generics
    #[arg(long)]
    no_generics: bool,
    /// Disable stripping unused declarations
    #[arg(long)]
    no_strip: bool,
    /// Disable lowering output to compatibility-mode WGSL
    #[arg(long)]
    no_lower: bool,
    /// Disable performing validation checks
    #[arg(long)]
    no_validate: bool,
    /// Disable performing validation checks with naga
    #[cfg(feature = "naga")]
    #[arg(long)]
    no_naga: bool,
    /// Root module declaration names to keep. Keeps all root module declarations by
    /// default. Can be repeated to keep multiple declarations
    #[arg(long)]
    keep: Option<Vec<String>>,
    /// Conditional compilation features to enable. Can be repeated
    #[arg(long)]
    enable: Vec<String>,
    /// Conditional compilation features to disable. Can be repeated
    #[arg(long)]
    disable: Vec<String>,
}

impl From<&CompOptsArgs> for CompileOptions {
    fn from(opts: &CompOptsArgs) -> Self {
        let mut features = HashMap::new();
        features.extend(opts.enable.iter().map(|f| (f.clone(), true)));
        features.extend(opts.disable.iter().map(|f| (f.clone(), false)));

        Self {
            use_imports: !opts.no_imports,
            use_condcomp: !opts.no_cond_comp,
            use_generics: !opts.no_generics,
            use_stripping: !opts.no_strip,
            use_lower: !opts.no_lower,
            use_validate: !opts.no_validate,
            entry_points: if opts.no_strip {
                None
            } else {
                opts.keep.clone()
            },
            features,
        }
    }
}

#[derive(Args, Clone, Debug)]
struct CompileArgs {
    #[command(flatten)]
    options: CompOptsArgs,
    /// WESL file entry point
    file: Option<PathBuf>,
}

#[derive(Args, Clone, Debug)]
struct CheckArgs {
    /// Input file type (wgsl or wesl)
    #[arg(long, default_value = "wesl")]
    kind: CheckKind,
    /// Validate output using Naga
    #[cfg(feature = "naga")]
    #[arg(long)]
    naga: bool,
    /// WGSL file entry point
    file: Option<PathBuf>,
}

#[derive(ValueEnum, Clone, Debug, Default)]
enum CheckKind {
    /// Check that an input file is valid WGSL
    Wgsl,
    /// Check that an input file is valid WESL
    #[default]
    Wesl,
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
                    .map_err(|()| "invalid resource binding type".to_string())?,
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
    /// Context to evaluate the expression into
    #[command(flatten)]
    options: CompOptsArgs,
    /// Output as binary (WGSL memory representation) for storable types
    #[arg(short, long)]
    binary: bool,
    /// Optional WESL entrypoint module to evaluate the expression into
    #[arg(long)]
    file: Option<PathBuf>,
    /// Const-expression to evaluate
    expr: String,
}

#[derive(Args, Clone, Debug)]
struct ExecArgs {
    /// Context to evaluate the expression into
    #[command(flatten)]
    options: CompOptsArgs,
    /// Bound resources. Only buffer bindings are supported at the moment.
    /// Syntax: colon-separated group,binding,binding_type,path
    /// Example: 0:0:storage:./my_buffer.bin
    ///  - group and binding are @group and @binding numbers
    ///  - binding_type is the `GPUBufferBindingType` (uniform, storage, read-only-storage)
    ///  - path is a path to a binary file of the buffer contents
    #[arg(long = "resource", value_parser = Binding::from_str, verbatim_doc_comment)]
    resources: Vec<Binding>,
    /// Pipeline-overridable constants.
    /// Syntax: name=expression
    #[arg(long = "override", value_parser = parse_key_val::<String, String>)]
    overrides: Vec<(String, String)>,
    /// Output as binary (WGSL memory representation) for storable types
    #[arg(short, long = "out-binary")]
    binary: bool,
    /// WESL entrypoint module to evaluate the expression into
    file: Option<PathBuf>,
    /// Function name to execute
    entrypoint: String,
}

#[derive(Args, Clone, Debug)]
struct PkgArgs {
    /// name of the generated crate
    name: String,
    /// directory containing the .wesl shader files
    dir: PathBuf,
}

#[derive(Clone, Debug, thiserror::Error)]
enum CliError {
    #[error("input file not found")]
    FileNotFound,
    #[error("resource `@group({0}) @binding({1})` not found")]
    ResourceNotFound(u32, u32),
    #[error(
        "resource `@group({0}) @binding({1})` ({2} bytes) incompatible with type `{3}` ({4} bytes)"
    )]
    ResourceIncompatible(u32, u32, u32, wesl::eval::Type, u32),
    #[error("Could not convert instance to buffer (type `{0}` is not storable)")]
    NotStorable(wesl::eval::Type),
    #[error("{0}")]
    WeslError(#[from] wesl::Error),
    #[error("{0}")]
    WeslDiagnostic(#[from] wesl::Diagnostic<wesl::Error>),
    #[cfg(feature = "naga")]
    #[error("naga error: {}", .0.emit_to_string(&.1))]
    Naga(naga::front::wgsl::ParseError, String),
}

enum FileOrSource {
    File(PathBuf),
    Source(String),
}

fn run_compile(
    options: &CompOptsArgs,
    file_or_source: FileOrSource,
) -> Result<CompileResult, CliError> {
    let compile_options = CompileOptions::from(options);

    let compiler = Wesl::new_barebones()
        .set_options(compile_options)
        .use_sourcemap(!options.no_sourcemap)
        .set_mangler(options.mangler.into());

    match file_or_source {
        FileOrSource::File(path) => {
            let base = path.parent().ok_or(CliError::FileNotFound)?;
            let name = path.file_name().ok_or(CliError::FileNotFound)?;
            let resolver = FileResolver::new(base);

            let res = compiler.set_custom_resolver(resolver).compile(name)?;
            Ok(res)
        }
        FileOrSource::Source(source) => {
            let base = std::env::current_dir().unwrap();
            let name = "command-line";
            let mut router = Router::new();
            let mut resolver = VirtualResolver::new();
            resolver.add_module("", source);
            router.mount_resolver(name, resolver);
            router.mount_fallback_resolver(FileResolver::new(base));

            let res = compiler.set_custom_resolver(router).compile(name)?;
            Ok(res)
        }
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
        .ok_or_else(|| CliError::ResourceNotFound(b.group, b.binding))?;

    let ty = ty_expr
        .eval_value(&mut ctx)
        .and_then(|inst| match inst {
            Instance::Type(ty) => Ok(ty),
            _ => Err(EvalError::UnknownType(inst.to_string())),
        })
        .map_err(|e| {
            Diagnostic::from(e)
                .with_ctx(&ctx)
                .with_source(ty_expr.to_string())
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
        CliError::ResourceIncompatible(
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
        .map_err(|e| Diagnostic::from(e).with_source(src.to_string()))?;
    let inst = expr.eval_value(&mut ctx).map_err(|e| {
        Diagnostic::from(e)
            .with_ctx(&ctx)
            .with_source(src.to_string())
    })?;
    Ok(inst)
}

fn main() {
    let cli = Cli::try_parse()
        .inspect_err(|e| {
            eprintln!("invalid arguments: {e}");
            std::process::exit(1)
        })
        .unwrap();
    run(cli).inspect_err(|e| eprintln!("{e}")).ok();
}

fn file_or_source(path: Option<PathBuf>) -> Option<FileOrSource> {
    path.map(FileOrSource::File).or_else(|| {
        let mut buf = String::new();
        std::io::stdin()
            .read_to_string(&mut buf)
            .ok()
            .map(|_| FileOrSource::Source(buf))
    })
}

fn run(cli: Cli) -> Result<(), CliError> {
    match cli.command {
        Command::Check(args) => {
            let source = if let Some(file) = &args.file {
                fs::read_to_string(file).map_err(|_| CliError::FileNotFound)?
            } else {
                let mut source = String::new();
                std::io::stdin()
                    .read_to_string(&mut source)
                    .map_err(|_| CliError::FileNotFound)?;
                source
            };

            match &args.kind {
                CheckKind::Wgsl => {
                    #[cfg(feature = "naga")]
                    if args.naga {
                        naga::front::wgsl::parse_str(&source)
                            .map_err(|e| CliError::Naga(e, source.clone()))?;
                    }
                    WgslParser::recognize_str(&source)
                        .map_err(|e| Diagnostic::from(e).with_source(source))?;
                }
                CheckKind::Wesl => {
                    let mut wesl = WgslParser::parse_str(&source)
                        .map_err(|e| Diagnostic::from(e).with_source(source))?;
                    wesl.retarget_idents();
                    todo!("validating WESL needs more work")
                    // wesl::validate_wesl(&wesl)?;
                    // let wgsl_source = wesl.to_string();
                    // #[cfg(feature = "naga")]
                    // if args.naga {
                    //     naga::front::wgsl::parse_str(&wgsl_source)
                    //         .map_err(|e| CliError::Naga(e, wgsl_source))?;
                    // }
                }
            }
            println!("OK");
        }
        Command::Compile(args) => {
            let comp = file_or_source(args.file)
                .map(|input| run_compile(&args.options, input))
                .unwrap_or_else(|| {
                    Ok(CompileResult {
                        syntax: TranslationUnit::default(),
                        sourcemap: None,
                    })
                })?;
            #[cfg(feature = "naga")]
            if !args.options.no_naga {
                let source = comp.to_string();
                naga::front::wgsl::parse_str(&source).map_err(|e| CliError::Naga(e, source))?;
            }
            println!("{}", comp);
        }
        Command::Eval(args) => {
            let comp = file_or_source(args.file)
                .map(|input| run_compile(&args.options, input))
                .unwrap_or_else(|| {
                    Ok(CompileResult {
                        syntax: TranslationUnit::default(),
                        sourcemap: None,
                    })
                })?;
            let mut eval = comp.eval(&args.expr)?;
            if args.binary {
                let buf = eval
                    .inst
                    .to_buffer(&mut eval.ctx)
                    .ok_or_else(|| CliError::NotStorable(eval.inst.ty()))?;
                std::io::stdout().write_all(buf.as_slice()).unwrap();
            } else {
                println!("{}", eval.inst)
            }
        }
        Command::Exec(args) => {
            let comp = file_or_source(args.file)
                .map(|input| run_compile(&args.options, input))
                .unwrap_or_else(|| {
                    Ok(CompileResult {
                        syntax: TranslationUnit::default(),
                        sourcemap: None,
                    })
                })?;

            let resources = args
                .resources
                .iter()
                .map(|b| parse_binding(b, &comp.syntax))
                .collect::<Result<_, _>>()?;

            let overrides = args
                .overrides
                .iter()
                .map(|(name, expr)| -> Result<(String, Instance), CliError> {
                    Ok((name.to_string(), parse_override(expr, &comp.syntax)?))
                })
                .collect::<Result<_, _>>()?;

            let mut exec = comp.exec(&args.entrypoint, resources, overrides)?;

            if args.binary {
                let buf = exec
                    .inst
                    .to_buffer(&mut exec.ctx)
                    .ok_or_else(|| CliError::NotStorable(exec.inst.ty()))?;
                std::io::stdout().write_all(buf.as_slice()).unwrap();
            } else {
                println!("return: {}", exec.inst)
            }

            let resources = args
                .resources
                .iter()
                .filter_map(|r| {
                    let inst = exec.resource(r.group, r.binding)?.clone();
                    let inst = inst.read().ok()?.to_owned();
                    Some((r.group, r.binding, inst))
                })
                .collect::<Vec<_>>();

            for (group, binding, inst) in resources {
                if args.binary {
                    let buf = inst
                        .to_buffer(&mut exec.ctx)
                        .ok_or_else(|| CliError::NotStorable(exec.inst.ty()))?;
                    std::io::stdout().write_all(buf.as_slice()).unwrap();
                } else {
                    println!("resource: group={group} binding={binding} value={inst}")
                }
            }
        }
        Command::Package(args) => {
            let code = PkgBuilder::new(&args.name)
                .scan_directory(args.dir)
                .expect("failed to scan WESL files")
                .validate()
                .map_err(|e| {
                    eprintln!("{e}");
                    panic!()
                })
                .unwrap()
                .codegen()
                .expect("failed to build package");
            println!("{code}");
        }
    };
    Ok(())
}
