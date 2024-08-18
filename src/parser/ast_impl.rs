use std::{
    fmt::{self, Display, Formatter},
    str::FromStr,
};

use super::{ast::*, Error, Span};

impl FromStr for DiagnosticSeverity {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "error" => Ok(Self::Error),
            "warning" => Ok(Self::Warning),
            "info" => Ok(Self::Info),
            "off" => Ok(Self::Off),
            _ => Err(Error::ParseDiagnosticSeverity),
        }
    }
}

pub trait OuterSpan {
    fn outer_span(&self) -> Span;
}
