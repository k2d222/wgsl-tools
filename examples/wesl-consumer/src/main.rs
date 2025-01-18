use wesl::include_wesl;

fn main() {
    let source = include_wesl!("main.wgsl");
    println!("{source}");
}
