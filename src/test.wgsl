import "foo/bar/baz.wgsl"::{ hello };
// some comment in between
//  import error::{ error };

import foo::bar::{ hello   as h1 };
import foo::bar::{ hello as h2, world as w };
import foo ::bar::  {
    hello as h3,
    world
};

fn main() -> vec4f {
    return vec4f();
}
