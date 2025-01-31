var<private> baz = 10;

fn bar(x: u32) {
    baz += 1;
}

fn foo() -> u32 {
    bar(100);
    baz += 1;
    return 10u;
}
