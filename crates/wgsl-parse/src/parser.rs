lalrpop_mod!(wgsl_recognize);
lalrpop_mod!(wgsl_spanned);

use lalrpop_util::lalrpop_mod;

use crate::{error::SpannedError, lexer::Lexer, syntax::TranslationUnit};

pub struct Parser;

impl Parser {
    pub fn parse_str<'s>(source: &'s str) -> Result<TranslationUnit, SpannedError<'s>> {
        let lexer = Lexer::new(source);
        let parser = wgsl_spanned::TranslationUnitParser::new();
        let res = parser.parse(lexer);
        res.map_err(|e| SpannedError::new(e, source))
    }
    pub fn parse<'s>(mut lexer: &'s mut Lexer) -> Result<TranslationUnit, SpannedError<'s>> {
        let parser = wgsl_spanned::TranslationUnitParser::new();
        let res = parser.parse(&mut lexer);
        res.map_err(|e| SpannedError::new(e, lexer.source()))
    }
}

impl Parser {
    pub fn recognize_str<'s>(source: &'s str) -> Result<(), SpannedError<'s>> {
        let lexer = Lexer::new(source);
        let parser = wgsl_recognize::TranslationUnitParser::new();
        let res = parser.parse(lexer);
        res.map_err(|e| SpannedError::new(e, source))
    }
    pub fn recognize<'s>(mut lexer: &'s mut Lexer) -> Result<(), SpannedError<'s>> {
        let parser = wgsl_recognize::TranslationUnitParser::new();
        let res = parser.parse(&mut lexer);
        res.map_err(|e| SpannedError::new(e, lexer.source()))
    }
    pub fn recognize_template_list<'s>(mut lexer: &'s mut Lexer) -> Result<(), SpannedError<'s>> {
        let parser = wgsl_recognize::TryTemplateListParser::new();
        let res = parser.parse(&mut lexer);
        res.map_err(|e| SpannedError::new(e, lexer.source()))
    }
}
