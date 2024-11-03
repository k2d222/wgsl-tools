#![cfg_attr(not(test), allow(dead_code, unused_imports))]

use std::{
    collections::HashMap,
    fmt::Display,
    fs::File,
    io::BufReader,
    path::{Path, PathBuf},
};

use serde::Deserialize;
use wesl::{
    eval::Instance,
    syntax::{Expression, Statement, TranslationUnit},
    CompileOptions, Resource, VirtualFileResolver, MANGLER_HASH,
};

#[test]
fn webgpu_samples() {
    let dir = std::fs::read_dir("webgpu-samples").expect("missing directory webgpu-samples");
    for entry in dir {
        let entry = entry.expect("error reading entry");
        let path = entry.path();
        if path.extension().unwrap() == "wgsl" {
            println!("testing webgpu-sample `{}`", path.display());
            let source = std::fs::read_to_string(path).expect("failed to read file");
            let mut source_module = wgsl_parse::Parser::parse_str(&source)
                .inspect_err(|err| eprintln!("{err}"))
                .expect("parse error");
            source_module.remove_voids();
            let disp = format!("{source_module}");
            let disp_module = wgsl_parse::Parser::parse_str(&disp)
                .inspect_err(|err| eprintln!("{err}"))
                .expect("parse error");
            assert_eq!(source_module, disp_module);
        }
    }
}

#[derive(PartialEq, Deserialize)]
#[serde(rename_all = "lowercase")]
enum GptTestSyntaxKind {
    Declaration,
    Statement,
    Expression,
}

#[derive(PartialEq, Deserialize)]
#[serde(rename_all = "lowercase")]
#[serde(tag = "kind")]
enum GptTestKind {
    Syntax {
        syntax: GptTestSyntaxKind,
    },
    Eval {
        eval: String,
        result: Option<String>,
    },
    Context,
}

impl Display for GptTestKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GptTestKind::Syntax { .. } => f.write_str("Syntax"),
            GptTestKind::Eval { .. } => f.write_str("Eval"),
            GptTestKind::Context => f.write_str("Context"),
        }
    }
}

#[derive(Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
enum GptTestExpect {
    Pass,
    Fail,
}

impl Display for GptTestExpect {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GptTestExpect::Pass => f.write_str("Pass"),
            GptTestExpect::Fail => f.write_str("Fail"),
        }
    }
}

#[derive(Deserialize)]
struct GptTest {
    name: String,
    desc: String,
    #[serde(flatten)]
    kind: GptTestKind,
    code: String,
    expect: GptTestExpect,
    note: Option<String>,
}

fn test_eval(
    eval: &String,
    result: &Option<String>,
    code: &String,
) -> Result<(bool, Instance), wesl::Error> {
    let ctx = code
        .parse::<TranslationUnit>()
        .map_err(|e| wesl::Error::from(e.error))?;

    let eval_inst = wesl::eval_const(eval, &ctx)?;

    match result {
        Some(expect) => {
            let expect_inst = wesl::eval_const(expect, &ctx)?;
            Ok((eval_inst == expect_inst, eval_inst))
        }
        None => Ok((false, eval_inst)),
    }
}

// #[test]
// fn gpt_test() {
//     let dir = std::fs::read_dir("gpt-tests").expect("missing directory gpt-tests");
//     let mut total_fails = 0;
//     let mut total_count = 0;

//     for entry in dir {
//         let entry = entry.expect("error reading entry");
//         let path = entry.path();
//         if path.extension().is_some_and(|path| path == "json") {
//             let (fails, count) = json_test(&path);
//             println!("{fails}/{count} failures");
//             total_fails += fails;
//             total_count += count;
//         }
//     }

//     let total_pass = total_count - total_fails;
//     println!("SUMMARY: {total_pass}/{total_count} Pass, {total_fails}/{total_count} Fails");
//     assert!(total_fails == 0);
// }

#[test]
fn spec_test() {
    let dir = std::fs::read_dir("spec-tests").expect("missing directory spec-tests");
    let mut total_fails = 0;
    let mut total_count = 0;

    for entry in dir {
        let entry = entry.expect("error reading entry");
        let path = entry.path();
        if path.extension().is_some_and(|path| path == "json") {
            let (fails, count) = json_test(&path);
            println!("{fails}/{count} failures");
            total_fails += fails;
            total_count += count;
        }
    }

    let total_pass = total_count - total_fails;
    println!("SUMMARY: {total_pass}/{total_count} Pass, {total_fails}/{total_count} Fails");
    assert!(total_fails == 0);
}

fn json_test(path: &Path) -> (u32, u32) {
    let mut fails = 0;
    let mut todos = 0;
    println!("testing json-test `{}`", path.display());

    let file = File::open(path).expect("failed to read file");
    let reader = BufReader::new(file);
    let json: Vec<GptTest> = serde_json::from_reader(reader)
        .inspect_err(|err| eprintln!("{err}"))
        .expect("invalid json test file");

    for test in &json {
        if test.kind == GptTestKind::Context {
            todos += 1;
            continue;
        }
        print!(
            " * `{}` kind: {}, expect: {}, result: ",
            test.name, test.kind, test.expect
        );

        match &test.kind {
            GptTestKind::Syntax { syntax } => {
                let res = match syntax {
                    GptTestSyntaxKind::Declaration => {
                        test.code.parse::<TranslationUnit>().map(|_| ())
                    }
                    GptTestSyntaxKind::Statement => test.code.parse::<Statement>().map(|_| ()),
                    GptTestSyntaxKind::Expression => test.code.parse::<Expression>().map(|_| ()),
                };
                let pass = res
                    .is_ok()
                    .then_some(GptTestExpect::Pass)
                    .unwrap_or(GptTestExpect::Fail);
                println!("{pass}");
                if pass != test.expect {
                    println!(
                        "   TEST FAILED\n   * {}{}\n   * code:`{}`\n   * result: {:?}\n",
                        test.desc,
                        test.note
                            .as_ref()
                            .map(|note| format!(" (note: {note})"))
                            .unwrap_or_default(),
                        test.code,
                        res
                    );
                    fails += 1;
                }
            }
            GptTestKind::Eval { eval, result } => {
                let res = test_eval(&eval, &result, &test.code);
                let pass = matches!(res, Ok((true, _)))
                    .then_some(GptTestExpect::Pass)
                    .unwrap_or(GptTestExpect::Fail);
                println!("{pass}");
                if pass != test.expect {
                    println!(
                                "   TEST FAILED\n   * {}{}\n   * code:`{}`\n   * eval:`{:?}`\n   * expected: {:?}\n   * result: {:?}\n",
                                test.desc,
                                test.note
                                    .as_ref()
                                    .map(|note| format!(" (note: {note})"))
                                    .unwrap_or_default(),
                                test.code,
                                eval,
                                result,
                                res.map(|(_, inst)| inst.to_string())
                            );
                    fails += 1;
                }
            }
            GptTestKind::Context => {
                println!("TODO");
            }
        }
    }

    (fails, json.len() as u32 - todos)
}

// see schema: https://github.com/wgsl-tooling-wg/wesl-testsuite/blob/main/src/TestSchema.ts
#[derive(Deserialize)]
struct WgslTestSrc {
    name: String,
    src: HashMap<String, String>,
    notes: Option<String>,
}

#[derive(Deserialize)]
struct ParsingTest {
    src: String,
    #[serde(default)]
    fails: bool,
}

fn wesl_testsuite_test_parsing(path: &Path) {
    println!("testing wesl-testsuite `{}`", path.display());
    let file = File::open(path).expect("failed to read file");
    let reader = BufReader::new(file);
    let json: Vec<ParsingTest> = serde_json::from_reader(reader)
        .inspect_err(|err| eprintln!("{err}"))
        .expect("invalid json test file");

    for test in json {
        println!(
            " * expect {}: `{}`",
            if test.fails { "Fail" } else { "Pass" },
            test.src
        );
        if test.fails {
            wgsl_parse::Parser::parse_str(&test.src)
                .expect_err("parse success but expected failure");
        } else {
            let source_module = wgsl_parse::Parser::parse_str(&test.src)
                .inspect_err(|err| eprintln!("{err}"))
                .expect("parse error");
            let disp = format!("{source_module}");
            let disp_module = wgsl_parse::Parser::parse_str(&disp)
                .inspect_err(|err| eprintln!("{err}"))
                .expect("parse error");
            assert_eq!(source_module, disp_module);
        }
    }
}

fn wesl_testsuite_test(path: &Path) {
    println!("testing wesl-testsuite `{}`", path.display());
    let file = File::open(path).expect("failed to read file");
    let reader = BufReader::new(file);
    let json: Vec<WgslTestSrc> = serde_json::from_reader(reader)
        .inspect_err(|err| eprintln!("{err}"))
        .expect("invalid json test file");

    for test in json {
        println!(
            " * `{}` {}",
            test.name,
            test.notes
                .map(|note| format!(" (note: {note})"))
                .unwrap_or_default()
        );

        let mut resolver = VirtualFileResolver::new();

        for (path, file) in test.src {
            let path = PathBuf::from(path);
            resolver
                .add_file(path.into(), file)
                .inspect_err(|err| eprintln!("{err}"))
                .expect("failed to add virtual file");
        }

        let entrypoint: Resource = PathBuf::from("./main.wgsl").into();
        let mangler = &MANGLER_HASH;
        let compile_options = CompileOptions::default();

        wesl::compile(&entrypoint, &resolver, mangler, &compile_options)
            .inspect_err(|err| eprintln!("{err}"))
            .expect("parse error");
    }
}

#[test]
fn wesl_testsuite_import_syntax() {
    let path = PathBuf::from("wesl-testsuite/importSyntaxCases.json");
    wesl_testsuite_test_parsing(&path);
}

#[test]
fn wesl_testsuite_import_cases() {
    let path = PathBuf::from("wesl-testsuite/importCases.json");
    wesl_testsuite_test(&path);
}
