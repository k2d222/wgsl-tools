
// TODO: this is broken
// @const
// fn main() -> i32 {
//     let x = vec3<f32>;
//     switch x {
//         case vec3(0., 0, 0) {
//             return x;
//         }
//     }
// }

@const
@type(S, u32 | f32 | i32)
@type(T, S | vec2<S> | vec3<S> | vec4<S>)
fn abs(e: T) -> T {
    @type(T, vec2<S> | vec3<S> | vec4<S>) {
        // implementation with vec3
        return max(-e, e);
    }
    @type(T, S) {
        // implementation with scalars
        return max(-e, e);
    }
}

@const
fn main() -> i32 {
    return abs<i32>(-5);
}
