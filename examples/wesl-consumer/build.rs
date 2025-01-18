use wesl::{FileResolver, Wesl};
use wesl_pkg::PkgResolver;

fn main() {
    let mut resolver = PkgResolver::new();
    resolver.add_package(&wesl_random::random::Mod);
    resolver.mount_fallback_resolver(FileResolver::new("src/shaders"));
    Wesl::new_spec_compliant()
        .set_resolver(resolver)
        .build("main.wgsl");
}
