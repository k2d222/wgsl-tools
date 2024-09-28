import imp1/{f1, c1};

@compute
fn main() -> u32 {
    @if(true)
    const c1 = 2;
    c1 += 1;
    let foo = bool();
    let x = c1 + f32(foo);
    return foo;
}
