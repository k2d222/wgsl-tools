fn main() {
    #[cfg(feature = "build-time")]
    wesl::Wesl::new("src/shaders")
        .add_package(&wesl_random::random::Mod)
        .build_artefact("main", "main");
}
