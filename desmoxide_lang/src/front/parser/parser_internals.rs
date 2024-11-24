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

/// [`BrandedParser`] with the `'static` brand
pub(crate) type Parser<'source, T> = BrandedParser<'source, 'static, T>;
/// Holds all of the state required to parse an individual expression, recursively constructing an AST from a stream of tokens
//#[inject_brand_lifetime('brand)]
pub(crate) struct BrandedParser<'source, 'brand, Iter> {
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
    __invariant: PhantomInvariant<'brand>,
}

#[derive(Debug, Clone, Copy)]
/// Token which lives for an invariant `'brand` lifetime, indicating that the user is working with the [`Parser`] with the same brand from within a call to [`Parser::with_tok`] or [`Parser::with_tok_mut`]
pub(crate) struct UncheckedToken<'brand>(PhantomData<fn(&'brand ()) -> &'brand ()>);

impl<'source, Lex> BrandedParser<'source, '_, Lex> {
    pub(crate) fn new_with(
        val: Lex,
        ctx: ParsingContext<'source>,
    ) -> BrandedParser<'source, 'static, Lex> {
        BrandedParser {
            lexer: val,
            ctx,
            span_stack: Vec::new(),
            nodes: Vec::new(),
            node_spans: Vec::new(),
            __invariant: PhantomData,
        }
    }

    /// Directly inserts a [`BrandedExprNode`] (as well as its accompanying [`Span`]) into this parser, returning
    /// a [`BrandedNodeId`] with the brand of the node that points to it.
    /// # Safety:
    /// improper use can violate [`Parser`]'s internal invariants.
    ///
    /// [`Parser`] assumes that all [`NodeId`]s contained in [`ExprNode`]s in its internal node array point to valid indices in the array.
    /// This function does not ensure that the provided [`ExprNode`] upholds this property, directly appending it to the array
    #[inline]
    pub(crate) unsafe fn insert_unchecked<'brand>(
        &mut self,
        node: BrandedExprNode<'brand>,
        span: Span,
    ) -> BrandedNodeId<'brand> {
        self.node_spans.push(span);
        let new_id = BrandedNodeId::with_lt::<'brand>(self.nodes.len());
        self.nodes.push(node.unbrand_val());
        new_id
    }
    pub(crate) fn insert(&mut self, node: BrandedExprNode<'_>, span: Span) -> NodeId {
        let u = node.unbrand_val();
        let len = self.nodes.len();
        if u.with_children(|children| children.iter().any(|v| v.as_idx() >= len))
            .is_some_and(|v| v)
        {
            panic!("AST node referred to an out-of-bounds child node!");
        }
        // Safety: child nodes of the AST node passed in were checked to be in-bounds
        unsafe { self.insert_unchecked(u, span) }
    }
}
impl<'brand, Lex> BrandedParser<'_, 'brand, Lex> {
    /// Checks whether a given [`NodeId`] is in-bounds for this parser, panicing on failiure. If the provided [`NodeId`] is in-bounds,
    /// it returns a new [`NodeId`] with the parser and token's `'brand`
    #[inline]
    pub(crate) fn check_id(
        &self,
        _tok: UncheckedToken<'brand>,
        id: BrandedNodeId<'_>,
    ) -> BrandedNodeId<'brand> {
        assert!(
            (id.inner.get() as usize) <= self.nodes.len(),
            "expected NodeId to be in-bounds!"
        );
        // Safety: this is safe because UncheckedToken ensures this index can only be used for the duration of the corresponding UncheckedToken's lifetime
        unsafe { id.cast_to_lt::<'brand>() }
    }
    /// Gets a shared reference to an [`ExprNode`] with unchecked indexing. This is sound because a [`NodeId`]`<'brand>`
    /// cannot be forged for reasons discussed in other parts of the documentation
    #[inline]
    pub(crate) fn node_unchecked(
        &self,
        _tok: UncheckedToken<'brand>,
        id: BrandedNodeId<'brand>,
    ) -> &BrandedExprNode<'brand> {
        // Safety: id is guaranteed to be in-bounds due to branding, reference transmute is allowed because it only affects lifetime parameters
        unsafe {
            transmute::<&BrandedExprNode<'static>, &BrandedExprNode<'brand>>(
                self.nodes.get_unchecked(id.as_idx()),
            )
        }
    }
    #[inline]
    pub(crate) fn node_unchecked_mut(
        &mut self,
        _tok: UncheckedToken<'brand>,
        id: BrandedNodeId<'brand>,
    ) -> &mut BrandedExprNode<'brand> {
        // Safety: id is guaranteed to be in-bounds due to branding, reference transmute is allowed because it only affects lifetime parameters
        // the ExprNode is safe to mutate through this reference because it requires any mutated NodeIds to have the same 'brand
        unsafe {
            transmute::<&mut BrandedExprNode<'static>, &mut BrandedExprNode<'brand>>(
                self.nodes.get_unchecked_mut(id.as_idx()),
            )
        }
    }
    /// Inserts a new [`ExprNode`] (along with its corresponding [`Span`]), skipping validation because the brand guarantees
    ///  that all [`NodeId`]s within the new [`ExprNode`] are in-bounds (see above)
    #[inline]
    pub(crate) fn insert_with_token(
        &mut self,
        _tok: UncheckedToken<'brand>,
        node: BrandedExprNode<'brand>,
        span: Span,
    ) -> BrandedNodeId<'brand> {
        // Safety: Brand ensures all indices are in-bounds
        unsafe { self.insert_unchecked(node, span) }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
/// An index type that represents the index of an AST node for a certain lifetime.
///
/// If the `'brand` lifetime is `'static` this represents an untrusted index
/// (i.e. it is not certain whether the AST node this points to lies in-bounds or not)
///
/// If the `'brand` lifetime is some other lifetime, this represents a trusted index that known to be in-bounds for the
/// corresponding [`Parser`] struct for the duration of that lifetime
pub(crate) struct BrandedNodeId<'brand> {
    /// Invariant phantom lifetime, used to store the brand
    _invariant_brand: PhantomData<fn(&'brand ()) -> &'brand ()>,
    /// Inner index, stored as index + 1 for layout optimization reasons
    inner: NonZeroU32,
}
/// BrandedNodeId with a `'static` brand
pub(crate) type NodeId = BrandedNodeId<'static>;

impl BrandedNodeId<'_> {
    /// Converts the inner value (a NonZeroU32 to allow for niche opt) to a usable index for zero-indexed arrays
    #[inline]
    pub(crate) fn as_idx(self) -> usize {
        // Safety: self.inner is a non-zero unsigned positive integer, subtracting one from it will never underflow
        unsafe { self.inner.get().unchecked_sub(1) as usize }
    }
    #[inline]
    /// Creates an untrusted NodeId (`'static` brand) from a 0-indexed index value
    pub(crate) fn from_idx(idx: usize) -> BrandedNodeId<'static> {
        // Safety: a NodeId<'static> is always safe to create
        unsafe { Self::with_lt(idx) }
    }
    #[inline]
    /// Removes the brand on this `NodeId`, if it is present
    pub(crate) fn unbrand(self) -> NodeId {
        // Safety: a NodeId<'static> is always safe to create
        unsafe { self.cast_to_lt::<'static>() }
    }
    /// Constructs a NodeId with the specified lifetime parameter as its brand
    /// # Safety
    /// Caller must ensure that the NodeId is actually valid for the applied brand (i.e. it is in-bounds for the Parser struct that it corresponds to).
    /// `'static` is the exception to this. It is always safe to apply the `'static` brand lifetime
    #[inline]
    unsafe fn with_lt<'lt>(idx: usize) -> BrandedNodeId<'lt> {
        assert!(
            idx < u32::MAX as usize,
            "Tried to create an out-of-bounds NodeId"
        );
        BrandedNodeId {
            // Safety: inner can never be zero because idx + 1 is checked to not to overflow (the only situation where adding 1 could result in 0).
            // unchecked add is sound because it will never overflow
            inner: unsafe { NonZeroU32::new_unchecked((idx as u32).unchecked_add(1)) },
            _invariant_brand: PhantomData,
        }
    }
    /// Casts this [`BrandedNodeId`] to use the specified lifetime parameter as its brand
    /// # Safety
    /// Caller must ensure that the NodeId is actually valid for the applied brand (i.e. it is in-bounds for the corresponding [`Parser`] struct).
    /// `'static` is the exception to this. It is always safe to apply the `'static` brand lifetime
    #[inline]
    unsafe fn cast_to_lt<'lt>(self) -> BrandedNodeId<'lt> {
        // Safety: transmute only affects lifetimes
        unsafe { transmute(self) }
    }
}
