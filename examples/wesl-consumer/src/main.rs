fn main() {
    #[cfg(feature = "build-time")]
    let source = {
        use wesl::include_wesl;
        include_wesl!("main")
    };

    #[cfg(not(feature = "build-time"))]
    let source = wesl::Wesl::new_spec_compliant("src/shaders")
        .add_package(&wesl_random::random::Mod)
        .compile("main")
        .inspect_err(|e| {
            eprintln!("{e}");
            panic!();
        })
        .unwrap()
        .to_string();

    println!("{source}");
}
