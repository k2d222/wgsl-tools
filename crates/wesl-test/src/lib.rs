#![cfg_attr(not(test), allow(dead_code, unused_imports))]

use std::{
    collections::HashMap,
    fs::File,
    io::BufReader,
    path::{Path, PathBuf},
};

use serde::Deserialize;
use wesl::{CompileOptions, Resource, VirtualFileResolver, MANGLER_HASH};

#[test]
fn webgpu_samples() {
    let dir = std::fs::read_dir("webgpu-samples").expect("missing directory webgpu-samples");
    for entry in dir {
        let entry = entry.expect("error reading entry");
        let path = entry.path();
        if path.extension().unwrap() == "wgsl" {
            println!("testing webgpu-sample `{}`", path.display());
            let source = std::fs::read_to_string(path).expect("failed to read file");
            let source_module = wgsl_parse::Parser::parse_str(&source)
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
            if test.fails { "FAILURE" } else { "SUCCESS" },
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
            " * `{}`{}",
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

        wesl::compile(&entrypoint, resolver, mangler, &compile_options)
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
