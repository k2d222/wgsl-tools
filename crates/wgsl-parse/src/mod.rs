mod parser_support;

lalrpop_mod!(pub(crate) wgsl_recognize, "wgsl_recognize.rs");
lalrpop_mod!(pub(crate) wgsl_spanned, "wgsl_spanned.rs");
