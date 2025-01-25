fn main() {
    #[cfg(feature = "build-time")]
    wesl::Wesl::new_spec_compliant("src/shaders")
        .add_package(&wesl_random::random::Mod)
        .build_artefact("main", "main");
}
