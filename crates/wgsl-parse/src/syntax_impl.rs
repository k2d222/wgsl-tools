use std::str::FromStr;

use super::{error::Error, syntax::*};

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
