use wesl::include_wesl;

fn main() {
    // let source = include_wesl!("main.wgsl");
    // println!("{source}");

    let mut pkg_resolver = wesl::PkgResolver::new();
    pkg_resolver.add_package(&wesl_random::random::Mod);
    let mut file_resolver = wesl::FileResolver::new("src/shaders");
    file_resolver.set_extension("wgsl");
    pkg_resolver.mount_fallback_resolver(file_resolver);
    let source2 = wesl::Wesl::new_spec_compliant()
        .set_custom_resolver(pkg_resolver)
        .compile("main.wgsl")
        .inspect_err(|e| {
            eprintln!("{e}");
            panic!();
        })
        .unwrap()
        .to_string();

    // assert_eq!(source, source2);
}
