use lalrpop_util::lalrpop_mod;

pub mod ast;

lalrpop_mod!(pub parser, "/parser/wgsl.rs");
