import imp1/{f1, c1};

@compute
fn main() -> vec4f {
    @if(true)
    let c1 = 2u;
    return f1();
}
