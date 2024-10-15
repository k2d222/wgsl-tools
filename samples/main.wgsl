// compute the nth term of the fibonacci sequence
// this function is recusive and that is NOT allowed by the WGSL standard.
// @const
// fn fib_rec(n: u32) -> u32 {
//     if n == 0 {
//         return 0;
//     }
//     else if n == 1 {
//         return 1;
//     }
//     else {
//         return fib_rec(n - 1) + fib_rec(n - 2);
//     }
// }

// // compute the nth term of the fibonacci sequence
// // this function is valid WGSL.
// @const
// fn fib_iter(n: u32) -> u32 {
//     var curr = 0u;
//     var prev1 = 1u;
//     var prev2 = 0u;

//     for (var i = 2u; i <= n; i++){
//         curr = prev1 + prev2;
//         prev2 = prev1;
//         prev1 = curr;
//     }

//     return curr;
// }

// @const
// fn main() -> array<u32, 2> {
//     var x = 5u;
//     x = 7;
//     return array(x, x);
//     // return array(
//     //     fib_rec(10),
//     //     fib_iter(10),
//     // );
// }
// @group(0) @binding(0)
// var<storage, read> foo: u32;

@const fn modifyValue(p: ptr<function, i32>) { *p = 2; } @const fn main() -> i32 { var a: i32 = 1; modifyValue(&a); return a; }
