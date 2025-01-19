fn main() {
    wesl::PkgBuilder::new("random")
        .scan_directory("src/shaders/random")
        .expect("failed to scan WESL files")
        .validate()
        .inspect_err(|e| {
            eprintln!("{e}");
            panic!();
        })
        .unwrap()
        .build_artefact()
        .expect("failed to build artefact")
}
