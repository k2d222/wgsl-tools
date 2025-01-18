fn main() {
    wesl_pkg::PackageBuilder::new("random")
        .set_root("src/shaders/random")
        .build()
        .unwrap()
}
