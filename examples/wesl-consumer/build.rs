use wesl::{FileResolver, PkgResolver, Wesl};

fn main() {
    let mut pkg_resolver = PkgResolver::new();
    pkg_resolver.add_package(&wesl_random::random::Mod);
    let mut file_resolver = FileResolver::new("src/shaders");
    file_resolver.set_extension("wgsl");
    pkg_resolver.mount_fallback_resolver(file_resolver);
    Wesl::new_spec_compliant()
        .set_resolver(pkg_resolver)
        .build_artefact("main.wgsl");
}
