# WESL-Rust

***A work in progress!***

This project implements the necessary tools to build complex shaders, like what [naga_oil](https://github.com/bevyengine/naga_oil) does for [bevy](https://bevyengine.org/), but in a framework-agnostic way. At [wgsl-tooling-wg](https://github.com/wgsl-tooling-wg) we aim at standardizing a few language extensions to facilitate the work of engine developers and shader wizards.

Current and planned extensions: *imports*, *conditional compilation*, *generics*, *modules*.

## Usage

This project can be used as a rust library or as a standalone CLI.

### Using the CLI

- Install `cargo install --git https://github.com/wgsl-tooling-wg/wesl-rs`
- Compile a WESL shader `wesl compile <entrypoint.wgsl>`
- Run eval() `wesl eval <entrypoint.wgsl> <expression to eval>`

### Using the Crate

Refer to the crate documentation on [docs.rs](https://docs.rs/wesl).

## Status
update: 2025-01

We are working towards an [MVP](https://github.com/wgsl-tooling-wg/wesl-spec/issues/54) release planned for 2025 Q1. It includes imports, conditional compilation and packaging.

**The crate [wgsl-parse](https://github.com/wgsl-tooling-wg/wesl-rs/tree/main/crates/wgsl-parse)** contains a WGSL-compliant syntax tree and parser, with optional syntax extensions from the [WESL specification](https://github.com/wgsl-tooling-wg/wesl-spec).

**The crate [wesl](https://github.com/wgsl-tooling-wg/wesl-rs/tree/main/crates/wesl)** contains an implementation of the [WESL specification](https://github.com/wgsl-tooling-wg/wesl-spec), i.e. a compiler that takes WESL files and generates valid WGSL.
  - [x] ["conditional translation"](https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/ConditionalTranslation.md) 100%
  - [x] ["imports"](https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/Imports.md) 90%
  - [ ] ["packaging"](https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/Packaging.md) 80%
  - [ ] ["generics"](https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/Generics.md) 50%
  - [ ] ["modules"](https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/Generics.md) 50%
  - [ ] eval/exec: 70%
  - [ ] WESL validator: 10%
  - [ ] WGSL optimization / compatibility: 10%

**This crate** contains a CLI to run the compiler and the parser.

**See also:**
- The online playground, [wesl.thissma.fr](https://wesl.thissma.fr/) / [github.com/wesl-playground](https://github.com/k2d222/wesl-playground)
- Nathalie Cuthbert's implementation, [mew](https://github.com/ncthbrt/mew)
- Lee Mighdoll's implementation, [wgsl-linker](https://github.com/wgsl-tooling-wg/wesl-js)

## Contributing

Contributions are welcome. Please join the [discord](https://discord.gg/Ng5FWmHuSv) server and introduce yourself first, or contact via [email](mailto:mathis.brossier@gmail.com).

## License

Except where noted (below and/or in individual files), all code in this repository is dual-licensed under either:

* MIT License ([LICENSE-MIT](LICENSE-MIT) or [http://opensource.org/licenses/MIT](http://opensource.org/licenses/MIT))
* Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or [http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0))

at your option.

### Your contributions

Unless you explicitly state otherwise,
any contribution intentionally submitted for inclusion in the work by you,
as defined in the Apache-2.0 license,
shall be dual licensed as above,
without any additional terms or conditions.
