use logos::{Filter, Lexer, SpannedIter};
use string_interner::{backend::BucketBackend, symbol::SymbolU32, StringInterner};
use strum::AsRefStr;

use super::Ident;

pub(crate) struct LexerExtras {
    // lexer takes ownership of the corresponding Frontend's interner for the duration of
    // lexing (i.e. only one expression may be lexed by a Frontend at a time)
    intern: StringInterner<BucketBackend<SymbolU32>>,
}

#[inline]
fn ident(lex: &mut Lexer<Token>) -> Ident {
    let slice = lex.slice();
    lex.extras.intern.get_or_intern(slice).into()
}
// #[inline]
// fn ident_from_d_dx(lex: &mut Lexer<Token>) -> Ident {
//     let mut slice = lex.slice();
//     slice = &slice[9..(slice.len() - 1)];
//     lex.extras.intern.get_or_intern(slice).into()
// }
// #[inline]
// fn handle_d(lex: &mut Lexer<Token>) -> Filter<Token> {
//     let source = lex.source();

//     let start = lex.span().start;

//     if start > 1 && source.get((start - 2)..start).is_some_and(|c| c == "}{") {
//         let mut group_ctr = 0;
//         for i in source[0..(start - 1)].rsplit(r"\frac") {
//             let mut iter = i.chars().rev();
//             loop {
//                 match iter.next() {
//                     Some('}') => {
//                         group_ctr += 1;
//                     }
//                     Some('{') => {
//                         group_ctr -= 1;
//                     }
//                     Some(_) => {}
//                     None => {
//                         if group_ctr == 0 {
//                             return Filter::Emit(Token::D);
//                         }
//                     }
//                 }
//             }
//         }
//     }
//     Filter::Emit(Token::Ident(lex.extras.intern.get_or_intern_static("d")))
// }
#[non_exhaustive]
#[derive(logos::Logos, Debug, PartialEq, Clone, Copy, AsRefStr)]
//#[logos(skip r"[ \t\n\f]+|\\+left|\\+right" )]
#[logos(skip r"[ \t\n\f]+")]
#[logos(extras = LexerExtras)]
pub(crate) enum Token {
    #[regex(r"\d+\.[0-9]+", |lex| {lex.slice().parse().ok()})]
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
pub struct SpannedTokenIter<'source> {
    inner: SpannedIter<'source, Token>,
    fracs: u32,
}
