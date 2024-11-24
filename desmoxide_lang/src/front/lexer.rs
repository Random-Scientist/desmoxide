use std::str::FromStr;

use logos::Lexer;

use strum::AsRefStr;
use thiserror::Error;

use super::{Ident, Interner};

#[inline]
fn ident(lex: &mut Lexer<Token>) -> Ident {
    let slice = lex.slice();
    lex.extras.get_or_intern(slice).into()
}
#[derive(Error, Debug, PartialEq, Clone, Default)]
pub enum LexError {
    #[error(transparent)]
    NumberParseError(<f64 as FromStr>::Err),
    #[default]
    #[error("failed to lex token")]
    LexError,
}

#[non_exhaustive]
#[derive(logos::Logos, Debug, PartialEq, Clone, Copy, AsRefStr)]
#[logos(skip r"[ \t\n\f]+")]
// this is a rather scuffed hack because you can't pass in an Extras type with an arbitrary lifetime.
// it just so happens that the 'source lifetime is good enough for our usage, so we use that
// (even though we really shouldn't be able to name it for hygiene reasons)
#[logos(extras = &'s mut Interner)]
#[logos(error = LexError)]
pub(crate) enum Token {
    #[regex(r"\d+\.[0-9]+", |lex| {lex.slice().parse().map_err(|e: <f64 as FromStr>::Err| LexError::NumberParseError(e))})]
    Literal(f64),

    // Variable name in the form "v" (single letter) or "v_{blah}"
    #[regex(r"[a-zA-Z](_\{[a-zA-Z0-9]+\})", callback = ident)]
    #[regex(r"[a-zA-Z]", priority = 1, callback = ident)]
    /// Variable identifier
    Ident(Ident),
    /// Represents an ambiguous case where this could either be the Ident "d" or the token D indicating an integration or differentiation variable following it
    #[token("d", priority = 2)]
    D,

    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LGroup,
    #[token("}")]
    RGroup,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token("_")]
    Subscript,
    #[token("^")]
    Superscript,

    #[token(r"=")]
    Eq,
    #[token(r"≥")]
    #[token(r"\ge")]
    Ge,
    #[token(r"≤")]
    #[token(r"\le")]
    Le,
    #[token(r">")]
    #[token(r"\gt")]
    Gt,
    #[token(r"<")]
    #[token(r"\lt")]
    Lt,

    // OPERATORS -------------------------------------------
    #[token(r"\frac")]
    Frac,
    #[token(r"\div")]
    #[token("/")]
    Div,
    #[regex(r"\++")]
    Plus,
    #[token("-")]
    Minus,
    #[token(r"\cdot")]
    #[token("*")]
    Mul,
    #[token("...")]
    Range,
    #[token(".")]
    Dot,
    #[token(r"\sqrt")]
    Sqrt,
    #[token(r"\sum")]
    Sum,
    #[token(r"\prod")]
    Prod,
    #[token(r"\int")]
    Integral,

    // BUILTINS --------------------------------------------
    #[token(r"\operatorname{random}")]
    Random,
    #[token(r"\min")]
    Min,
    #[token(r"\max")]
    Max,
    #[token(r"\operatorname{count}")]
    Count,
    #[token(r"\operatorname{total}")]
    Total,
    #[token(r"\operatorname{length}")]
    Length,
    #[token(r"\operatorname{join}")]
    Join,
    #[token(r"\operatorname{sort}")]
    Sort,
    #[token(r"\operatorname{shuffle}")]
    Shuffle,
    #[token(r"\operatorname{unique}")]
    Unique,
    #[token(r"\operatorname{for}")]
    For,
    #[token(r"\sin")]
    Sin,
    #[token(r"\cos")]
    Cos,
    #[token(r"\tan")]
    Tan,
    #[token(r"\csc")]
    Csc,
    #[token(r"\sec")]
    Sec,
    #[token(r"\cot")]
    Cot,

    #[token(r"\operatorname{mod}")]
    Mod,
    #[token(r"\operatorname{floor}")]
    Floor,
    #[token(r"\operatorname{ceil}")]
    Ceil,
    // SPECIAL VALUES --------------------------------------------
    #[token(r"\infty")]
    Infty,

    #[token(r"\left", callback = logos::skip, priority = 10000)]
    #[token(r"\\left", callback = logos::skip, priority = 10000)]
    #[token(r"\right", callback = logos::skip, priority = 10000)]
    #[token(r"\\right", callback = logos::skip, priority = 10000)]
    Invalid,
}
