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
    CompileOptions, HashMangler, Resource, VirtualResolver,
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
enum SyntaxKind {
    Declaration,
    Statement,
    Expression,
}

#[derive(PartialEq, Deserialize)]
#[serde(rename_all = "lowercase")]
#[serde(tag = "kind")]
enum TestKind {
    Syntax {
        syntax: SyntaxKind,
    },
    Eval {
        eval: String,
        result: Option<String>,
    },
    Context,
}

impl Display for TestKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TestKind::Syntax { .. } => f.write_str("Syntax"),
            TestKind::Eval { .. } => f.write_str("Eval"),
            TestKind::Context => f.write_str("Context"),
        }
    }
}

#[derive(Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
enum Expectation {
    Pass,
    Fail,
}

impl Display for Expectation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expectation::Pass => f.write_str("Pass"),
            Expectation::Fail => f.write_str("Fail"),
        }
    }
}

#[derive(Deserialize)]
struct Test {
    name: String,
    desc: String,
    #[serde(flatten)]
    kind: TestKind,
    code: String,
    expect: Expectation,
    note: Option<String>,
}

fn test_eval(
    eval: &String,
    result: &Option<String>,
    code: &String,
) -> Result<(bool, Instance), wesl::Error> {
    let wesl = code
        .parse::<TranslationUnit>()
        .map_err(|e| wesl::Error::from(e.error))?;

    let expr = eval.parse::<Expression>().map_err(|e| e.error)?;
    let (eval_inst, _) = wesl::eval_const(&expr, &wesl);
    let eval_inst = eval_inst?;

    match result {
        Some(expect) => {
            let expr = expect.parse::<Expression>().map_err(|e| e.error)?;
            let (expect_inst, _) = wesl::eval_const(&expr, &wesl);
            let expect_inst = expect_inst?;
            Ok((eval_inst == expect_inst, eval_inst))
        }
        None => Ok((false, eval_inst)),
    }
}

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
    let json: Vec<Test> = serde_json::from_reader(reader)
        .inspect_err(|err| eprintln!("{err}"))
        .expect("invalid json test file");

    for test in &json {
        if test.kind == TestKind::Context {
            todos += 1;
            continue;
        }
        print!(
            " * `{}` kind: {}, expect: {}, result: ",
            test.name, test.kind, test.expect
        );

        match &test.kind {
            TestKind::Syntax { syntax } => {
                let res = match syntax {
                    SyntaxKind::Declaration => test.code.parse::<TranslationUnit>().map(|_| ()),
                    SyntaxKind::Statement => test.code.parse::<Statement>().map(|_| ()),
                    SyntaxKind::Expression => test.code.parse::<Expression>().map(|_| ()),
                };
                let pass = res
                    .is_ok()
                    .then_some(Expectation::Pass)
                    .unwrap_or(Expectation::Fail);
                println!("{pass}");
                if pass != test.expect {
                    println!(
                        "   [FAIL]\n   * {}{}\n   * code:`{}`\n   * result: {:?}\n",
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
            TestKind::Eval { eval, result } => {
                let res = test_eval(&eval, &result, &test.code);
                let pass = matches!(res, Ok((true, _)))
                    .then_some(Expectation::Pass)
                    .unwrap_or(Expectation::Fail);
                println!("{pass}");
                if pass != test.expect {
                    println!(
                                "   [FAIL]\n   * {}{}\n   * code:`{}`\n   * eval:`{:?}`\n   * expected: {:?}\n   * result: {:?}\n",
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
            TestKind::Context => {
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
    #[serde(default)]
    name: Option<String>,
    src: String,
    #[serde(default)]
    fails: bool,
    #[serde(default)]
    result: Option<String>,
}

fn wesl_testsuite_test_parsing(path: &Path) {
    println!("testing wesl-testsuite `{}`", path.display());
    let file = File::open(path).expect("failed to read file");
    let reader = BufReader::new(file);
    let json: Vec<ParsingTest> = serde_json::from_reader(reader)
        .inspect_err(|err| eprintln!("{err}"))
        .expect("invalid json test file");

    let mut fails = 0;
    let mut expected_fail = 0;
    let mut expected_pass = 0;
    let count = json.len();

    for test in json {
        let expects = if test.fails { "Fail" } else { "Pass" };
        println!(
            " * test {}: expect {expects}",
            test.name.unwrap_or(String::from("<no name>"))
        );
        let parse = wgsl_parse::Parser::parse_str(&test.src);
        if test.fails && parse.is_ok() {
            println!(
                "   [FAIL]\n   * result: {}\n   * parse success but expected failure\n   * code:`{}`\n",
                test.result.unwrap_or(String::from("<no message>")),
                test.src,
            );
            // println!("   [FAIL] parse success but expected failure");
            fails += 1;
            expected_fail += 1;
        } else if !test.fails && parse.is_err() {
            println!(
                "   [FAIL]\n   * result: {}\n   * parse failure but expected success\n   * error: {}\n   * code:`{}`\n",
                test.result.unwrap_or(String::from("<no message>")),
                parse.unwrap_err(),
                test.src,
            );
            fails += 1;
            expected_pass += 1;
        }
    }

    let pass = count - fails;
    println!("SUMMARY: {pass}/{count} Pass, {fails}/{count} Fails");
    println!(
        "of which expected Fail: {expected_fail}/{fails}, expected Pass: {expected_pass}/{fails}"
    );
    assert!(fails == 0);
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

        let mut resolver = VirtualResolver::new();

        for (path, file) in test.src {
            resolver.add_module(path, file);
        }

        let root_module = Resource::new(PathBuf::from("main.wgsl"));
        let compile_options = CompileOptions::default();

        wesl::compile(&root_module, &resolver, &HashMangler, &compile_options)
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

#[test]
fn wesl_testsuite_cts_parse() {
    let path = PathBuf::from("wesl-testsuite/ctsParseTests.json");
    wesl_testsuite_test_parsing(&path);
}
