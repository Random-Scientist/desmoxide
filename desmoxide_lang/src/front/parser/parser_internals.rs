use std::{iter::Peekable, marker::PhantomData, mem::transmute, num::NonZeroU32};

use logos::{Logos, Span};

use crate::{front::lexer::Token, util::branded::PhantomInvariant};

use super::{BrandedExprNode, ExprNode, ParsingContext};

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
#[desmoxide_derive::inject_brand_lifetime('brand)]
mod branded_structs {
    #[derive(Debug, Clone, Copy)]
    #[repr(transparent)]
    /// An index type that represents the index of an AST node for a certain lifetime.
    ///
    /// If the `'brand` lifetime is `'static` this represents an untrusted index
    /// (i.e. it is not certain whether the AST node this points to lies in-bounds or not)
    ///
    /// If the `'brand` lifetime is some other lifetime, this represents a trusted index that known to be in-bounds for the
    /// corresponding [`Parser`] struct for the duration of that lifetime
    pub(crate) struct NodeId<'brand> {
        /// Inner index, stored as index + 1 for layout optimization reasons
        inner: NonZeroU32,
    }
    /// Holds all of the state required to parse an individual expression, recursively constructing an AST from a stream of tokens
    pub(crate) struct Parser<'source, 'brand, Iter> {
        /// Stream of [`Token`]s to parse
        pub(super) lexer: Iter,
        /// Provides context for error generation (source code access and expression id)
        pub(super) ctx: ParsingContext<'source>,
        /// Contains the covered [`Span`]s of the parsing carried out by various recursion levels
        pub(super) span_stack: Vec<Span>,
        /// Contains a list of [`ExprNode`]s which refer to each other to form a tree via [`NodeId`]s.
        /// # Invariant
        /// All [`NodeId`]s contained within [`ExprNode`]s in this vec have indices which lie within 0..nodes.len() (i.e. it is safe to do unchecked indexing with them)
        pub(super) nodes: Vec<ExprNode>,
        /// A parallel list of [`Span`]s, each containing the corresponding [`Span`] of the ExprNode at the same index in `nodes`
        pub(super) node_spans: Vec<Span>,
    }
}

impl<'source, Lex> BrandedParser<'source, 'static, Lex> {
    pub(crate) fn new_with(val: Lex, ctx: ParsingContext<'source>) -> Self {
        Self::new(val, ctx, Vec::new(), Vec::new(), Vec::new())
    }
}

impl BrandedNodeId<'_> {
    /// Converts the inner value (a NonZeroU32 to allow for niche opt) to a usable index for zero-indexed arrays
    #[inline]
    pub(crate) fn as_idx(self) -> usize {
        // Safety: self.inner is a non-zero unsigned positive integer, subtracting one from it will never underflow
        unsafe { self.inner.get().unchecked_sub(1) as usize }
    }
}
impl NodeId {
    #[inline]
    /// Creates an untrusted NodeId (`'static` brand) from a 0-indexed index value
    pub(crate) fn from_idx(idx: usize) -> BrandedNodeId<'static> {
        assert!(
            idx < u32::MAX as usize,
            "Tried to create an out-of-bounds NodeId"
        );
        // Safety: inner can never be zero because idx + 1 is checked to not to overflow (the only situation where adding 1 could result in 0).
        // unchecked add is sound because it will never overflow
        Self::new(unsafe { NonZeroU32::new_unchecked((idx as u32).unchecked_add(1)) })
    }
}
