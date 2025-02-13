use std::{iter::Peekable, num::NonZeroU32};

use logos::{Logos, Span};

use crate::front::lexer::Token;

use super::{ExprNode, ParsingContext};
use miette::SourceSpan;

pub(crate) trait GenericSpanIter<'source>:
    Iterator<Item = (Result<Token, <Token as Logos<'source>>::Error>, Span)>
{
    fn peek(&mut self) -> Option<&Self::Item>;
}
impl<'source, T: Iterator<Item = (Result<Token, <Token as Logos<'source>>::Error>, Span)>>
    GenericSpanIter<'source> for Peekable<T>
{
    #[inline]
    fn peek(&mut self) -> Option<&Self::Item> {
        self.peek()
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
/// An index type that represents the index of an AST node for a certain lifetime.
pub(crate) struct NodeId {
    /// Inner index, stored as index + 1 for layout optimization reasons
    inner: NonZeroU32,
}
#[derive(Debug, Clone)]
pub struct ExprNodeList {
    nodes: Vec<ExprNode>,
}
/// Holds all of the state required to parse an individual expression, recursively constructing an AST from a stream of tokens
pub(crate) struct Parser<'source, Iter> {
    /// Stream of [`Token`]s (and associated [`Span`]s) to parse
    pub(super) lexer: Iter,
    /// Provides context for error generation (source code access and expression id)
    pub(super) ctx: ParsingContext<'source>,
    /// Contains the covered [`Span`]s of the parsing carried out by various recursion levels
    pub(super) span_stack: Vec<Option<SourceSpan>>,
    /// Contains a list of [`ExprNode`]s which refer to each other to form a tree via [`NodeId`]s.
    pub(super) nodes: ExprNodeList,
    /// A parallel list of [`Span`]s, each containing the corresponding [`Span`] of the ExprNode at the same index in `nodes`
    pub(super) node_spans: Vec<SourceSpan>,
}

impl<'source, Lex> Parser<'source, Lex> {
    pub(crate) fn new_with(val: Lex, ctx: ParsingContext<'source>) -> Self {
        Self {
            lexer: val,
            ctx,
            span_stack: Vec::new(),
            nodes: ExprNodeList::new(),
            node_spans: Vec::new(),
        }
    }
}

impl NodeId {
    /// Converts the inner value (a NonZeroU32 to allow for niche opt) to a usable index for zero-indexed arrays
    #[inline]
    pub(crate) fn as_idx(self) -> usize {
        // Safety: self.inner is a non-zero unsigned positive integer, subtracting one from it will never underflow
        unsafe { self.inner.get().unchecked_sub(1) as usize }
    }
}
impl NodeId {
    #[inline]
    /// Creates an untrusted NodeId from a 0-indexed index value
    pub(crate) fn from_idx(idx: usize) -> NodeId {
        assert!(
            idx < u32::MAX as usize,
            "Tried to create an out-of-bounds NodeId"
        );
        // Safety: inner can never be zero because idx + 1 is checked to not to overflow (the only situation where adding 1 could result in 0).
        // unchecked add is sound because it will never overflow
        Self {
            inner: unsafe { NonZeroU32::new_unchecked((idx as u32).unchecked_add(1)) },
        }
    }
}

impl ExprNodeList {
    pub(crate) fn new() -> Self {
        Self { nodes: Vec::new() }
    }
}
