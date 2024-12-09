# WGSL Tools

***A work in progress!***

This project implements the necessary tools to build complex shaders, like what [naga_oil](https://github.com/bevyengine/naga_oil) does for [bevy](https://bevyengine.org/), but in a framework-agnostic way. At [wgsl-tooling-wg](https://github.com/wgsl-tooling-wg), we aim at standardizing a few language extensions to facilitate the work of engine developers and shader wizards.

Current and planned extensions: *imports*, *conditional compilation*, *generics*, *modules*.

## Usage

This project can be used as a rust library or as a standalone CLI.

### Using the CLI

- Install `cargo install --git https://github.com/k2d222/wgsl-tools`
- Compile a WESL shader `wesl compile <entrypoint.wgsl>` (TODO: use the wesl extension)
- Run eval() `wesl eval <entrypoint.wgsl> <expression to eval>`

### Using the Crate

documentation coming soon

## Status
update: 2024-10

**The crate [wgsl-parse](https://github.com/k2d222/wgsl-tools/tree/main/crates/wgsl-parse)** contains a WGSL-compliant syntax tree and parser, with optional syntax extensions from the [WESL specification](https://github.com/wgsl-tooling-wg/wesl-spec).

**The crate [wesl](https://github.com/k2d222/wgsl-tools/tree/main/crates/wesl)** contains an implementation of the [WESL specification](https://github.com/wgsl-tooling-wg/wesl-spec), i.e. a compiler that takes WESL files and generates valid WGSL.
  - [x] ["conditional translation"](https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/ConditionalTranslation.md)
  - [x] ["imports"](https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/Imports.md) (WIP, the spec will evolve)
  - [ ] ["generics"](https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/Generics.md) (WIP, the spec will evolve)
  - [ ] ["modules"](https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/Generics.md) (WIP, the spec will evolve)
  - [ ] eval/exec: 70%
  - [ ] WESL validator: 0%
  - [ ] WGSL optimization / compatibility: 10%

**This crate** contains a CLI to run the compiler and the parser.

**See also:**
- The online playground, [wesl.thissma.fr](https://wesl.thissma.fr/) / [github.com/wesl-playground](https://github.com/k2d222/wesl-playground)
- Nathalie Cuthbert's implementation, [mew](https://github.com/ncthbrt/mew)
- Lee Mighdoll's implementation, [wgsl-linker](https://github.com/wgsl-tooling-wg/wgsl-linker)

## Goals

* *Correct*, mirror concepts present in [the wgsl spec](https://www.w3.org/TR/WGSL/).
* *Flexible*, allow extending the wgsl syntax with well-defined extensions (see [wgsl-tooling-wg](https://github.com/wgsl-tooling-wg)).
* *User-friendly*, by sticking to the spec as much as possible, by providing clear and well-documented interfaces.

## Non-goals

* *Performant*, we care about correctness, less so about time and memory constraints. The interface must be as user-friendly as possible. These tools target offline compilation first.
