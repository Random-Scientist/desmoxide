use std::{fmt::Display, num::NonZeroU32};

use logos::{Logos, Span};
use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::middle::IRType;

use super::lexer::Token;

#[derive(Debug, Diagnostic, Error)]
#[error("Compile Error")]
pub struct CompileError {
    #[label("here")]
    location: SourceSpan,

    #[source]
    #[diagnostic_source]
    error: FrontendError,
}

#[derive(Debug, Diagnostic, Error)]
pub enum FrontendError {
    #[error(transparent)]
    #[diagnostic(transparent)]
    ParseError(#[from] ParseError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    LoweringError(#[from] LoweringError),
}
impl FrontendError {
    pub(crate) fn with_span(self, span: Span, line_number: Option<NonZeroU32>) -> CompileError {
        CompileError {
            location: SourceSpan::new(span.start.into(), span.end - span.start),
            error: self,
        }
    }
}
#[derive(Debug, Diagnostic, Error)]
pub enum ParseError {
    #[error("{}found unexpected token {}",
        expected.as_ref().map(|e| format!("expected one of {}, ", e.iter().map(|v| v.as_ref()).collect::<Vec<_>>().join(", "))).as_deref().unwrap_or(""),
        tok.as_ref(),
    )]
    UnexpectedToken {
        tok: Token,
        expected: Option<Box<[Token]>>,
    },
    #[error("Lexer error")]
    LexError {
        #[source]
        err: <Token as Logos<'static>>::Error,
        #[label("while attempting to lex token here")]
        span: SourceSpan,
    },
    #[error("part of expression is empty")]
    IncompleteExpression,
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
