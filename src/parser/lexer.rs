use std::{cell::RefCell, fmt::Display, iter::Peekable, rc::Rc};

use lalrpop_util::ParseError;
use logos::{Logos, SpannedIter};

use super::wgsl::TryTemplateListParser;

pub type Span = std::ops::Range<usize>;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Error;

fn maybe_template_end(lex: &mut logos::Lexer<Token>) -> Token {
    if lex.extras.parse_template && lex.extras.depth == 0 {
        // found a ">" on the same nesting level as the opening "<"
        Token::TemplateArgsEnd
    } else {
        Token::SymGreaterThan
    }
}

fn incr_depth(lex: &mut logos::Lexer<Token>) {
    lex.extras.depth += 1;
}

fn decr_depth(lex: &mut logos::Lexer<Token>) {
    lex.extras.depth -= 1;
}

#[derive(Clone, Debug, PartialEq)]
pub struct LexerState {
    depth: usize,
    parse_template: bool,
}

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
    #[regex(r#"([_\p{XID_Start}][\p{XID_Continue}]+)|([\p{XID_Start}])"#)]
    Ident,
    #[regex(r#"(\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?[iufh]?|0[xX]([\da-fA-F](\.[\da-fA-F]*)?|\.[\da-fA-F]+)([pP][+-]?\d+)?[iufh]?"#)]
    NumLit,
    // #[regex(r#"@[\w_]+(\s*\([^\)]*\))?"#)] // BUG: is this a bug in logos' regex? I can't use \s*
    #[regex(r#"@[\w_]+(\s?\([^\)]*\))?"#)]
    Attribute,
    TemplateList,
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
            Token::Ident => f.write_str("an identifier"),
            Token::NumLit => f.write_str("a numeric literal"),
            Token::Attribute => f.write_str("an attribute"),
            Token::TemplateList => f.write_str("a template list"),
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
}

impl<'s> Lexer<'s> {
    pub fn new(source: &'s str) -> Self {
        let mut token_stream = Token::lexer_with_extras(
            source,
            LexerState {
                depth: 0,
                parse_template: false,
            },
        )
        .spanned();
        let next_token = token_stream.next();
        Self {
            source,
            token_stream,
            next_token,
        }
    }

    pub fn parse_template_list(
        source: &str,
    ) -> Result<(Vec<Span>, Span), ParseError<usize, Token, (usize, Error, usize)>> {
        let mut idents = Vec::new();
        let mut lexer = Lexer::new(&source);
        match lexer.next_token {
            Some((Ok(ref mut t), _)) if *t == Token::SymLessThan => *t = Token::TemplateArgsStart,
            _ => (),
        };
        lexer.token_stream.extras.parse_template = true;
        let parser = TryTemplateListParser::new();
        parser.parse(&mut idents, lexer).map(|span| (idents, span))
    }
}

impl<'s> Iterator for Lexer<'s> {
    type Item = Spanned<Token, usize, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut next_token = self.token_stream.next();
        match next_token {
            Some((Ok(ref mut t), _)) if *t == Token::SymLessThan => match self.next_token {
                Some((Ok(Token::Ident), ref prev_span)) => {
                    let source = &self.source[prev_span.end..];
                    match Lexer::parse_template_list(source) {
                        Ok((idents, span)) => {
                            println!("template {idents:?}");
                            *t = Token::TemplateList;
                            self.token_stream.bump(span.end - span.start);
                        }
                        _ => (),
                    };
                }
                _ => (),
            },
            _ => (),
        };

        if matches!(self.next_token, Some((Ok(Token::TemplateArgsEnd), _))) {
            next_token = None; // push eof after end of template
        }

        std::mem::swap(&mut self.next_token, &mut next_token);

        next_token.map(|(token, span)| match token {
            Ok(tok) => Ok((span.start, tok, span.end)),
            Err(err) => Err((span.start, err, span.end)),
        })
    }
}
