use std::{marker::PhantomData, mem::transmute, num::NonZeroU32};

use logos::{Span, SpannedIter};

use crate::front::lexer::Token;

use super::{BrandedExprNode, ExprNode};

#[derive(Debug, Clone, Copy)]
#[repr(transparent)]
/// An index type that represents the index of an AST node for a certain lifetime.
///
/// If the `'brand` lifetime is `'static` this represents a checked index (i.e. it is not certain whether the AST node this points to lies in-bounds or not)
///
/// If the `'brand` lifetime is some other lifetime, this represents an unchecked index that is valid to use for
/// the duration of that lifetime (and the AST with the corresponding brand is allowed to do unchecked indexing with this NodeId)
pub(crate) struct BrandedNodeId<'brand> {
    phantom: PhantomData<fn(&'brand ()) -> &'brand ()>,
    inner: NonZeroU32,
}
/// BrandedNodeId with a `'static` brand
pub(crate) type NodeId = BrandedNodeId<'static>;

impl BrandedNodeId<'_> {
    /// Converts the inner value (a NonZeroU32 to allow for niche opt) to a usable index for zero-indexed arrays
    #[inline]
    pub(crate) fn as_idx(self) -> usize {
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
            phantom: PhantomData,
        }
    }
    /// Casts this BrandedNodeId to use the specified lifetime parameter as its brand
    /// # Safety
    /// Caller must ensure that the NodeId is actually valid for the applied brand (i.e. it is in-bounds for the Parser struct that it corresponds to).
    /// `'static` is the exception to this. It is always safe to apply the `'static` brand lifetime
    #[inline]
    unsafe fn cast_to_lt<'lt>(self) -> BrandedNodeId<'lt> {
        // Safety: transmute only affects lifetimes
        unsafe { transmute(self) }
    }
}

impl<'brand> BrandedExprNode<'brand> {
    /// Runs the given closure with a reference to a slice containing the child NodeIds of this ExprNode
    /// (used when ensuring that provided NodeIds of an untrusted ExprNode are in-bounds if they are not provided via the unchecked API)
    ///
    /// Shortcuts to return `None` if a node has no children
    fn with_children<R>(&self, cb: impl FnOnce(&[BrandedNodeId<'brand>]) -> R) -> Option<R> {
        match self {
            BrandedExprNode::NumberLit(_) => None,
            BrandedExprNode::Ident(_) => None,
            BrandedExprNode::Binary(id1, id2, _) => Some(cb(&[*id1, *id2])),
            BrandedExprNode::Unary(id1, _) => Some(cb(&[*id1])),
            BrandedExprNode::RangeList(range_list) => {
                Some(if let Some(im) = &range_list.interval_marker {
                    cb(&[*im, range_list.start, range_list.end])
                } else {
                    cb(&[range_list.start, range_list.end])
                })
            }
            BrandedExprNode::ListLit(thin_boxed_slice) => Some(cb(thin_boxed_slice)),
            BrandedExprNode::ListComp(thin_boxed_slice, i) => {
                let mut v = thin_boxed_slice
                    .iter()
                    .map(|v| &v.1)
                    .copied()
                    .collect::<Vec<_>>();
                v.push(*i);
                Some(cb(&v))
            }
            BrandedExprNode::Sum { initial, end, .. } => Some(cb(&[*initial, *end])),
            BrandedExprNode::Prod { initial, end, .. } => Some(cb(&[*initial, *end])),
            BrandedExprNode::Integral {
                lower,
                upper,
                integrand,
            } => Some(cb(&[*lower, *upper, *integrand])),
            BrandedExprNode::Derivative { diff } => Some(cb(&[*diff])),
            BrandedExprNode::IntegrandOrDiff { expression, .. } => Some(cb(&[*expression])),
        }
    }
    /// Removes the brand on this ExprNode, if it is present, replacing the brand lifetime with `'static`
    pub(crate) fn unbrand_val(self) -> ExprNode {
        // Safety: transmutes that only affect lifetime parameters are unconditionally allowed (i.e. lifetime parameters cannot cause layout instability)
        // the 'static brand is always safe to apply because it is the one brand where the creation of a corresponding UncheckedToken<'static>
        // is impossible due to the construction invariants on it (see Parser::with_token and Parser::with_token_mut)
        unsafe { transmute::<BrandedExprNode<'_>, BrandedExprNode<'static>>(self) }
    }
    /// Removes the brand on this reference to an ExprNode, if it is present, replacing the brand lifetime with `'static`
    pub(crate) fn unbrand_ref(&self) -> &ExprNode {
        // Safety: transmutes that only affect references are unconditionally allowed, for a discussion of the effect of the lifetime
        // cast see the above safety comments in unbrand_val above
        unsafe { transmute::<&BrandedExprNode<'_>, &BrandedExprNode<'static>>(self) }
    }
    /// Removes the brand on this mutable reference to an ExprNode, if it is present, replacing the brand lifetime with `'static`
    pub(crate) fn unbrand_mut(&mut self) -> &mut ExprNode {
        // Safety: transmutes that only affect references are unconditionally allowed, for a discussion of the effect of the lifetime
        // cast see the above safety comments in unbrand_val above
        unsafe { transmute::<&mut BrandedExprNode<'_>, &mut BrandedExprNode<'static>>(self) }
    }
}
/// BrandedParser with the `'static` brand
pub(crate) type Parser<'source> = BrandedParser<'source, 'static>;
/// Holds all of the state required to parse an individual expression, recursively constructing an AST from a stream of tokens
pub(crate) struct BrandedParser<'source, 'brand> {
    /// Stream of `Token`s to parse
    lexer: SpannedIter<'source, Token>,
    /// Contains the covered spans of the parsing carried out by various recursion levels
    span_stack: Vec<Span>,
    /// Contains a list of ExprNodes which can refer to each other by NodeId.
    /// # Invariant
    /// All `BrandedNodeId`s contained within `ExprNode`s in this vec have indices which fall on 0..nodes.len() (i.e. it is safe to do unchecked indexing with them)
    nodes: Vec<ExprNode>,
    /// A parallel list of `Span`s, each containing the corresponding Span of the ExprNode at the same index in `nodes`
    node_spans: Vec<Span>,
    /// PhantomData that holds an invariant phantom lifetime for brand verification
    _brand: PhantomData<fn(&'brand ()) -> &'brand ()>,
}

#[derive(Debug, Clone, Copy)]
/// Token which lives for an invariant `'brand` lifetime, indicating that the user is accessing the parser from within a call to `with_tok` or `with_tok_mut`
pub(crate) struct UncheckedToken<'brand>(PhantomData<fn(&'brand ()) -> &'brand ()>);

impl<'source> BrandedParser<'source, '_> {
    /// Runs a closure with a reference to the Parser and UncheckedToken that live for a higher-kinded lifetime.
    /// Crucially, this lifetime *cannot* be `'static`, as the closure only knows that both values live for the duration
    /// of the closure and no longer (the closure does not run for `'static`, therefore the lifetime inferred by the HKT cannot be `'static`).
    ///
    /// This enables the `Parser` to be certain that `BrandedNodeId`s with the same `'brand` as it are in-bounds
    /// (because that parser previously checked them), which allows for unchecked indexing, as well as safe direct mutation of AST nodes
    pub(crate) fn with_tok<R>(
        &self,
        func: impl for<'brand> FnOnce(UncheckedToken<'brand>, &BrandedParser<'source, 'brand>) -> R,
    ) -> R {
        func(UncheckedToken(PhantomData), self)
    }
    /// [`Parser::with_tok`] but with a mutable reference to the Parser
    pub(crate) fn with_tok_mut<R>(
        &mut self,
        func: impl for<'brand> FnOnce(UncheckedToken<'brand>, &mut BrandedParser<'source, 'brand>) -> R,
    ) -> R {
        func(UncheckedToken(PhantomData), self)
    }
    /// Get a reference to a node from an id, panicing if the NodeID points out of bounds
    #[inline]
    pub(crate) fn node(&self, id: BrandedNodeId<'_>) -> &ExprNode {
        &self.nodes[id.as_idx()]
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
impl<'brand> BrandedParser<'_, 'brand> {
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
