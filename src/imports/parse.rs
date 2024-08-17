use unicode_ident::{is_xid_continue, is_xid_start};
use winnow::{
    combinator::{
        alt, cut_err, delimited, opt, preceded, repeat, repeat_till, separated, seq, terminated,
    },
    error::{ContextError, ErrMode, StrContext, StrContextValue},
    stream::{AsChar, Location},
    token::{any, take_till, take_until, take_while},
    Located, PResult, Parser,
};

pub type Span = std::ops::Range<usize>;

#[derive(Clone, Debug)]
pub enum Path<'s> {
    Quoted(&'s str),
    Hierarchy(Vec<&'s str>),
}

#[derive(Clone, Debug)]
pub struct Symbol<'s> {
    pub name: &'s str,
    pub rename: Option<&'s str>,
}

#[derive(Clone, Debug)]
pub struct ImportSection<'s> {
    pub span: Span,
    pub imports: Vec<Import<'s>>,
}

#[derive(Clone, Debug)]
pub struct Import<'s> {
    pub span: Span,
    pub path: Path<'s>,
    pub items: Vec<Symbol<'s>>,
}

//
// pub fn surrounded<'s, F, O, A, E>(
//     occurrences: impl Into<Range>,
//     sep: impl Parser<&'s str, O, E> + Clone,
//     parser: F,
// ) -> impl Parser<&'s str, A, E>
// where
//     F: Parser<&'s str, O, E>,
//     A: Accumulate<O>,
//     E: ParserError<&'s str>,
// {
//     delimited(
//         sep.clone(),
//         separated(occurrences, parser, sep.clone()),
//         sep,
//     )
// }

fn block_comment<'s>(i: &mut Located<&'s str>) -> PResult<()> {
    ("/*", take_until(0.., "*/"), "*/").void().parse_next(i)
}

fn line_comment<'s>(i: &mut Located<&'s str>) -> PResult<()> {
    ("//", take_till(0.., AsChar::is_newline), any)
        .void()
        .parse_next(i)
}

fn comment<'s>(i: &mut Located<&'s str>) -> PResult<()> {
    alt((block_comment, line_comment)).parse_next(i)
}

// https://www.w3.org/TR/WGSL/#blankspace-and-line-breaks
fn blankspace0<'s>(i: &mut Located<&'s str>) -> PResult<()> {
    take_while(0.., char::is_whitespace).void().parse_next(i)
}

fn blankspace<'s>(i: &mut Located<&'s str>) -> PResult<()> {
    take_while(1.., char::is_whitespace).void().parse_next(i)
}

fn newlines0<'s>(i: &mut Located<&'s str>) -> PResult<()> {
    repeat(0.., alt((comment, blankspace))).parse_next(i)
}

// // https://www.w3.org/TR/WGSL/#blankspace-and-line-breaks
// pub fn line_break<'s>(i: &mut Located<&'s str>) -> PResult<()> {
//     take_while(1.., char::).void().parse_next(i)
// }

// https://www.w3.org/TR/WGSL/#identifier
fn identifier<'s>(i: &mut Located<&'s str>) -> PResult<&'s str> {
    (
        any.verify(|c| is_xid_start(*c)),
        take_while(0.., is_xid_continue),
    )
        .take()
        .parse_next(i)
}

fn path<'s>(i: &mut Located<&'s str>) -> PResult<Path<'s>> {
    alt((
        delimited('"', take_till(0.., '"'), '"').map(Path::Quoted),
        separated(1.., identifier, delimited(blankspace0, "::", blankspace0)).map(Path::Hierarchy),
    ))
    .context(StrContext::Label("import path"))
    .parse_next(i)
}

fn symbol<'s>(i: &mut Located<&'s str>) -> PResult<Symbol<'s>> {
    cut_err(seq! {Symbol {
        name: identifier,
        rename: opt(preceded((blankspace, "as", blankspace), identifier))

    }})
    .context(StrContext::Label("import symbol"))
    .parse_next(i)
}

fn items<'s>(i: &mut Located<&'s str>) -> PResult<Vec<Symbol<'s>>> {
    cut_err(delimited(
        ('{', blankspace0),
        separated(0.., symbol, (blankspace0, ',', blankspace0)),
        (blankspace0, '}')
            .context(StrContext::Expected(StrContextValue::CharLiteral('}')))
            .context(StrContext::Expected(StrContextValue::CharLiteral(',')))
            .context(StrContext::Expected(StrContextValue::StringLiteral("as"))),
    ))
    .context(StrContext::Label("import items"))
    .parse_next(i)
}

fn import<'s>(i: &mut Located<&'s str>) -> PResult<Import<'s>> {
    let ((path, items), span) = (
        preceded(("import", blankspace), path),
        cut_err(preceded(
            blankspace0,
            terminated(
                opt(preceded((blankspace0, "::", blankspace0), items)),
                (blankspace0, ';').context(StrContext::Expected(StrContextValue::CharLiteral(';'))),
            ),
        )),
    )
        .context(StrContext::Label("import clause"))
        .with_span()
        .parse_next(i)?;
    Ok(Import {
        span,
        path,
        items: items.unwrap_or_default(),
    })
}

// fn skip_unknown<'s>(i: &mut Located<&'s str>) -> PResult<()> {
//     let starters = ("fn ", "import ", "//", "/*");
//     i.find_slice()
//     peek(alt(starters))
// }

// fn token<'s>(i: &mut Located<&'s str>) -> PResult<Token<'s>> {
//     alt((import.map(Token::Import), import.map(Token::Import))).parse_next(i)
// }

// fn function<'s>(i: &mut Located<&'s str>) -> PResult<Function<'s>> {}

fn skip_to_fn<'s>(i: &mut Located<&'s str>) -> PResult<()> {
    let starters = ("fn ", "//", "/*");
    repeat_till(0.., any, alt(starters))
        .map(|((), _)| ())
        .parse_next(i)
}

fn imports<'s>(i: &mut Located<&'s str>) -> PResult<Vec<Import<'s>>> {
    preceded(newlines0, separated(0.., import, newlines0)).parse_next(i)
}

#[derive(Debug, Clone)]
pub struct WgslParseError {
    message: String,
    span: std::ops::Range<usize>,
    input: String,
}

impl WgslParseError {
    fn from_parse(error: ErrMode<ContextError>, input: &str, span: Span) -> Self {
        match error {
            ErrMode::Incomplete(_) => todo!(),
            ErrMode::Backtrack(error) | ErrMode::Cut(error) => {
                let message = error.to_string();
                let input = input.to_owned();
                // let start = span;
                // let end = (start + 1..)
                //     .find(|e| input.is_char_boundary(*e))
                //     .unwrap_or(start);
                Self {
                    message,
                    span,
                    input,
                }
            }
        }
    }
}

impl std::fmt::Display for WgslParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = annotate_snippets::Level::Error
            .title(&self.message)
            .snippet(
                annotate_snippets::Snippet::source(&self.input)
                    .fold(true)
                    .annotation(annotate_snippets::Level::Error.span(self.span.clone())),
            );
        let renderer = annotate_snippets::Renderer::plain();
        let rendered = renderer.render(message);
        rendered.fmt(f)
    }
}

impl std::error::Error for WgslParseError {}

pub fn parse_imports<'s>(i: &'s str) -> Result<ImportSection<'s>, WgslParseError> {
    let loc = &mut Located::new(i);
    // let start = loc.location();
    let parsed = imports.with_span().parse_next(loc);

    match parsed {
        Ok((imports, span)) => Ok(ImportSection { span, imports }),
        Err(e) => {
            let end = loc.location();
            let span = end..end; // TODO: error ranges
            Err(WgslParseError::from_parse(e, i, span))
        }
    }
}
