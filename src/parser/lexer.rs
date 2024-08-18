use std::fmt::Display;

use lexical::{FromLexical, NumberFormatBuilder, ParseFloatOptions, ParseIntegerOptions};
use logos::{Logos, Source, SpannedIter};

use super::{wgsl_recognize, Error, Span};

fn maybe_template_end(lex: &mut logos::Lexer<Token>) -> Token {
    if let Some(depth) = lex.extras.template_depths.last() {
        if lex.extras.depth == *depth {
            // found a ">" on the same nesting level as the opening "<"
            lex.extras.template_depths.pop();
            return Token::TemplateArgsEnd;
        }
    }

    Token::SymGreaterThan
}

fn incr_depth(lex: &mut logos::Lexer<Token>) {
    lex.extras.depth += 1;
}

fn decr_depth(lex: &mut logos::Lexer<Token>) {
    lex.extras.depth -= 1;
}

// TODO: adjust the settings to match wgsl literal syntax.
fn parse_lit<T: FromLexical>(lex: &mut logos::Lexer<Token>) -> Option<T> {
    let str = lex.slice();
    lexical::parse_partial(str).map(|(n, _)| n).ok()
}

#[derive(Clone, Debug, PartialEq)]
pub struct LexerState {
    depth: usize,
    template_depths: Vec<usize>,
}

// follwing the spec at this date: https://www.w3.org/TR/2024/WD-WGSL-20240731/
#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(
    skip r"\s+",
    skip r"//[^\n\r]*[\n\r]*", // line comment
    skip r"/\*[^*]*\*+(?:[^/*][^*]*\*+)*/", // block comment
    extras = LexerState,
    error = Error)]
pub enum Token {
    // syntactic tokens
    // https://www.w3.org/TR/WGSL/#syntactic-tokens
    #[token("&")]
    SymAnd,
    #[token("&&")]
    SymAndAnd,
    #[token("->")]
    SymArrow,
    #[token("@")]
    SymAttr,
    #[token("/")]
    SymForwardSlash,
    #[token("!")]
    SymBang,
    #[token("[", incr_depth)]
    SymBracketLeft,
    #[token("]", decr_depth)]
    SymBracketRight,
    #[token("{")]
    SymBraceLeft,
    #[token("}")]
    SymBraceRight,
    #[token(":")]
    SymColon,
    #[token(",")]
    SymComma,
    #[token("=")]
    SymEqual,
    #[token("==")]
    SymEqualEqual,
    #[token("!=")]
    SymNotEqual,
    #[token(">", maybe_template_end)]
    SymGreaterThan,
    #[token(">=")]
    SymGreaterThanEqual,
    #[token(">>")]
    SymShiftRight,
    #[token("<")]
    SymLessThan,
    #[token("<=")]
    SymLessThanEqual,
    #[token("<<")]
    SymShiftLeft,
    #[token("%")]
    SymModulo,
    #[token("-")]
    SymMinus,
    #[token("--")]
    SymMinusMinus,
    #[token(".")]
    SymPeriod,
    #[token("+")]
    SymPlus,
    #[token("++")]
    SymPlusPlus,
    #[token("|")]
    SymOr,
    #[token("||")]
    SymOrOr,
    #[token("(", incr_depth)]
    SymParenLeft,
    #[token(")", decr_depth)]
    SymParenRight,
    #[token(";")]
    SymSemicolon,
    #[token("*")]
    SymStar,
    #[token("~")]
    SymTilde,
    #[token("_")]
    SymUnderscore,
    #[token("^")]
    SymXor,
    #[token("+=")]
    SymPlusEqual,
    #[token("-=")]
    SymMinusEqual,
    #[token("*=")]
    SymTimesEqual,
    #[token("/=")]
    SymDivisionEqual,
    #[token("%=")]
    SymModuloEqual,
    #[token("&=")]
    SymAndEqual,
    #[token("|=")]
    SymOrEqual,
    #[token("^=")]
    SymXorEqual,
    #[token(">>=")]
    SymShiftRightAssign,
    #[token("<<=")]
    SymShiftLeftAssign,

    // keywords
    // https://www.w3.org/TR/WGSL/#keyword-summary
    #[token("alias")]
    KwAlias,
    #[token("break")]
    KwBreak,
    #[token("case")]
    KwCase,
    #[token("const")]
    KwConst,
    #[token("const_assert")]
    KwConstAssert,
    #[token("continue")]
    KwContinue,
    #[token("continuing")]
    KwContinuing,
    #[token("default")]
    KwDefault,
    #[token("diagnostic")]
    KwDiagnostic,
    #[token("discard")]
    KwDiscard,
    #[token("else")]
    KwElse,
    #[token("enable")]
    KwEnable,
    #[token("false")]
    KwFalse,
    #[token("fn")]
    KwFn,
    #[token("for")]
    KwFor,
    #[token("if")]
    KwIf,
    #[token("let")]
    KwLet,
    #[token("loop")]
    KwLoop,
    #[token("override")]
    KwOverride,
    #[token("requires")]
    KwRequires,
    #[token("return")]
    KwReturn,
    #[token("struct")]
    KwStruct,
    #[token("switch")]
    KwSwitch,
    #[token("true")]
    KwTrue,
    #[token("var")]
    KwVar,
    #[token("while")]
    KwWhile,

    // XXX: should we also register reserved words as tokens?
    #[regex(r#"([_\p{XID_Start}][\p{XID_Continue}]+)|([\p{XID_Start}])"#, |lex| lex.slice().to_string())]
    Ident(String),
    #[regex(r#"0|[1-9][0-9]*"#, parse_lit)] // dec
    #[regex(r#"0[xX][0-9a-fA-F]+"#, parse_lit)] // hex
    AbstractInt(i32),
    #[regex(r#"(\d+\.\d*|\.\d+)([eE][+-]?\d+)?"#, parse_lit)] // dec
    #[regex(
        r#"0[xX]([\da-fA-F]+\.[\da-fA-F]*|\.[\da-fA-F]+)([pP][+-]?\d+)?"#,
        parse_lit
    )]
    // hex
    AbstractFloat(f32),
    #[regex(r#"(0|[1-9][0-9]*)i"#, parse_lit)] // dec
    #[regex(r#"0[xX][0-9a-fA-F]+i"#, parse_lit)]
    // hex
    I32(i32),
    #[regex(r#"(0|[1-9][0-9]*)u"#, parse_lit)] // dec
    #[regex(r#"0[xX][0-9a-fA-F]+u"#, parse_lit)]
    // hex
    U32(u32),
    #[regex(r#"(\d+\.\d*|\.\d+)([eE][+-]?\d+)?f"#, parse_lit)] // dec
    #[regex(
        r#"0[xX]([\da-fA-F]+\.[\da-fA-F]*|\.[\da-fA-F]+)([pP][+-]?\d+)?f"#,
        parse_lit
    )]
    // hex
    F32(f32),
    #[regex(r#"(\d+\.\d*|\.\d+)([eE][+-]?\d+)?h"#, parse_lit)] // dec
    #[regex(
        r#"0[xX]([\da-fA-F]+\.[\da-fA-F]*|\.[\da-fA-F]+)([pP][+-]?\d+)?h"#,
        parse_lit
    )]
    // hex
    F16(f32),
    #[regex(r#"@[\w_]+(\s?\([^\)]*\))?"#, |lex| lex.slice().to_string())]
    Attribute(String),
    TemplateArgsStart,
    TemplateArgsEnd,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::SymAnd => f.write_str("&"),
            Token::SymAndAnd => f.write_str("&&"),
            Token::SymArrow => f.write_str("->"),
            Token::SymAttr => f.write_str("@"),
            Token::SymForwardSlash => f.write_str("/"),
            Token::SymBang => f.write_str("!"),
            Token::SymBracketLeft => f.write_str("["),
            Token::SymBracketRight => f.write_str("]"),
            Token::SymBraceLeft => f.write_str("{"),
            Token::SymBraceRight => f.write_str("}"),
            Token::SymColon => f.write_str(":"),
            Token::SymComma => f.write_str(","),
            Token::SymEqual => f.write_str("="),
            Token::SymEqualEqual => f.write_str("=="),
            Token::SymNotEqual => f.write_str("!="),
            Token::SymGreaterThan => f.write_str(">"),
            Token::SymGreaterThanEqual => f.write_str(">="),
            Token::SymShiftRight => f.write_str(">>"),
            Token::SymLessThan => f.write_str("<"),
            Token::SymLessThanEqual => f.write_str("<="),
            Token::SymShiftLeft => f.write_str("<<"),
            Token::SymModulo => f.write_str("%"),
            Token::SymMinus => f.write_str("-"),
            Token::SymMinusMinus => f.write_str("--"),
            Token::SymPeriod => f.write_str("."),
            Token::SymPlus => f.write_str("+"),
            Token::SymPlusPlus => f.write_str("++"),
            Token::SymOr => f.write_str("|"),
            Token::SymOrOr => f.write_str("||"),
            Token::SymParenLeft => f.write_str("("),
            Token::SymParenRight => f.write_str(")"),
            Token::SymSemicolon => f.write_str(";"),
            Token::SymStar => f.write_str("*"),
            Token::SymTilde => f.write_str("~"),
            Token::SymUnderscore => f.write_str("_"),
            Token::SymXor => f.write_str("^"),
            Token::SymPlusEqual => f.write_str("+="),
            Token::SymMinusEqual => f.write_str("-="),
            Token::SymTimesEqual => f.write_str("*="),
            Token::SymDivisionEqual => f.write_str("/="),
            Token::SymModuloEqual => f.write_str("%="),
            Token::SymAndEqual => f.write_str("&="),
            Token::SymOrEqual => f.write_str("|="),
            Token::SymXorEqual => f.write_str("^="),
            Token::SymShiftRightAssign => f.write_str(">>="),
            Token::SymShiftLeftAssign => f.write_str("<<="),
            Token::KwAlias => f.write_str("alias"),
            Token::KwBreak => f.write_str("break"),
            Token::KwCase => f.write_str("case"),
            Token::KwConst => f.write_str("const"),
            Token::KwConstAssert => f.write_str("const_assert"),
            Token::KwContinue => f.write_str("continue"),
            Token::KwContinuing => f.write_str("continuing"),
            Token::KwDefault => f.write_str("default"),
            Token::KwDiagnostic => f.write_str("diagnostic"),
            Token::KwDiscard => f.write_str("discard"),
            Token::KwElse => f.write_str("else"),
            Token::KwEnable => f.write_str("enable"),
            Token::KwFalse => f.write_str("false"),
            Token::KwFn => f.write_str("fn"),
            Token::KwFor => f.write_str("for"),
            Token::KwIf => f.write_str("if"),
            Token::KwLet => f.write_str("let"),
            Token::KwLoop => f.write_str("loop"),
            Token::KwOverride => f.write_str("override"),
            Token::KwRequires => f.write_str("requires"),
            Token::KwReturn => f.write_str("return"),
            Token::KwStruct => f.write_str("struct"),
            Token::KwSwitch => f.write_str("switch"),
            Token::KwTrue => f.write_str("true"),
            Token::KwVar => f.write_str("var"),
            Token::KwWhile => f.write_str("while"),
            Token::Ident(s) => write!(f, "identifier `{s}`"),
            Token::AbstractInt(n) => write!(f, "{n}"),
            Token::AbstractFloat(n) => write!(f, "{n}"),
            Token::I32(n) => write!(f, "{n}i"),
            Token::U32(n) => write!(f, "{n}u"),
            Token::F32(n) => write!(f, "{n}f"),
            Token::F16(n) => write!(f, "{n}h"),
            Token::Attribute(s) => write!(f, "attribute `{s}`"),
            Token::TemplateArgsStart => f.write_str("<"),
            Token::TemplateArgsEnd => f.write_str(">"),
        }
    }
}

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), (Loc, Error, Loc)>;

#[derive(Clone)]
pub struct Lexer<'s> {
    source: &'s str,
    token_stream: SpannedIter<'s, Token>,
    next_token: Option<(Result<Token, Error>, Span)>,
    parse_template_list: bool,
}

impl<'s> Lexer<'s> {
    pub fn new(source: &'s str) -> Self {
        let mut token_stream = Token::lexer_with_extras(
            source,
            LexerState {
                depth: 0,
                template_depths: Vec::new(),
            },
        )
        .spanned();
        let next_token = token_stream.next();
        Self {
            source,
            token_stream,
            next_token,
            parse_template_list: false,
        }
    }

    pub fn recognize_template_list(source: &str) -> bool {
        let mut lexer = Lexer::new(&source);
        match lexer.next_token {
            Some((Ok(ref mut t), _)) if *t == Token::SymLessThan => *t = Token::TemplateArgsStart,
            _ => (),
        };
        lexer.parse_template_list = true;
        lexer.token_stream.extras.template_depths.push(0);
        let parser = wgsl_recognize::TryTemplateListParser::new();
        parser.parse(lexer).is_ok()
    }
}

impl<'s> Iterator for Lexer<'s> {
    type Item = Spanned<Token, usize, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut next_token = self.token_stream.next();
        match next_token {
            Some((Ok(ref mut t), ref span)) if *t == Token::SymLessThan => match self.next_token {
                Some((Ok(Token::Ident(_)), _)) => {
                    let source = &self.source[span.start..];
                    if Lexer::recognize_template_list(source) {
                        *t = Token::TemplateArgsStart;
                        let cur_depth = self.token_stream.extras.depth;
                        self.token_stream.extras.template_depths.push(cur_depth);
                    }
                }
                _ => (),
            },
            _ => (),
        };

        if self.parse_template_list
            && matches!(self.next_token, Some((Ok(Token::TemplateArgsEnd), _)))
        {
            next_token = None; // push eof after end of template
        }

        std::mem::swap(&mut self.next_token, &mut next_token);

        next_token.map(|(token, span)| match token {
            Ok(tok) => Ok((span.start, tok, span.end)),
            Err(err) => Err((span.start, err, span.end)),
        })
    }
}
