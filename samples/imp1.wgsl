var<private> baz: u32 = 10u;

fn foo(x: vec3u) {
    baz += 1;
}


fn fib(n: u32) -> u32 {
    var a: u32 = 0;
    var b: u32 = 1;
    for (var i: u32 = 2; i <= n; i = i + 1) {
        b = a + b;
        a = b - a;
    }
    return b;
}

fn bar() -> bool {
    let x = 9 == 9u + baz;
    // baz += 1;
    return x;
}
