use std::fmt::Display;

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::middle::IRType;

use super::lexer::Token;

#[derive(Debug, Diagnostic, Error)]
#[error("Compile Error")]
pub struct CompileError {
    #[label("here")]
    location: SourceSpan,

    #[diagnostic_source]
    error: FrontendError,
}

#[derive(Debug, Diagnostic, Error)]
pub enum FrontendError {
    #[diagnostic(transparent)]
    #[error("error during parsing")]
    ParseError(#[from] ParseError),

    #[diagnostic(transparent)]
    #[error("error during lowering")]
    LoweringError(#[from] LoweringError),
}
#[derive(Debug, Diagnostic, Error)]
pub enum ParseError {
    #[error("expected a {}, got {}", expected_sep_name, found_sep_name)]
    MismatchedSeparator {
        expected_sep_name: &'static str,
        found_sep_name: &'static str,
    },
    #[error("unexpected token {}{}", tok.as_ref(), expected.as_ref().map(|e| format!(", expected {}", e.as_ref())).as_deref().unwrap_or(""))]
    UnexpectedToken { tok: Token, expected: Option<Token> },
}
#[derive(Debug, Diagnostic, Error)]

pub enum LoweringError {
    #[error("Type error: expected value of type {expected}, but type was {got}")]
    TypeMismatch { expected: IRType, got: IRType },
    #[error("tried to define variable {ident} twice! ")]
    Redefinition {
        ident: String,
        #[label("first defined here")]
        first_def_span: SourceSpan,
    },
}
impl Display for IRType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_list {
            f.write_str("List<")?;
        }
        f.write_str(self.primitive.as_ref())?;
        if self.is_list {
            f.write_str(">")?;
        }
        Ok(())
    }
}
