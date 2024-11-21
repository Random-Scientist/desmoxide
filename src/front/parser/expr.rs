use std::mem;

use string_interner::symbol::SymbolU32;

use crate::util::thin_boxed_slice::ThinBoxedSlice;

use super::{BrandedNodeId, NodeId};

#[derive(Debug, Clone)]
pub(crate) struct Expr {
    nodes: Vec<ExprNode>,
    root: NodeId,
}

pub(crate) type ExprNode = BrandedExprNode<'static>;
#[derive(Debug, Clone)]
pub(crate) enum BrandedExprNode<'brand> {
    /// a numeric literal
    /// ## Examples
    /// * `1.0`
    /// * `1`
    NumberLit(f64),
    /// an identifier
    /// ## Examples
    /// * `a`
    /// * `a_{var}`
    Ident(Ident),
    /// A binary operation
    /// ## Examples
    /// * `a + b`
    /// * `\frac{a}{b}`
    /// * `ab`
    Binary(BrandedNodeId<'brand>, BrandedNodeId<'brand>, BinaryOp),
    /// A unary operation
    /// ## Examples
    /// * `-a`
    /// * `\sqrt{b}`
    Unary(BrandedNodeId<'brand>, UnaryOp),
    /// A range list literal
    /// ## Examples
    /// * `[1...100]`
    /// * `[1,4...10]`
    RangeList(RangeList<'brand>),
    /// A normal list literal
    /// ## Examples
    /// * `[1, 2, a, b, f(c)]`
    /// * `[c^{2}, \frac{a}{b}, 3]`
    ListLit(ThinBoxedSlice<BrandedNodeId<'brand>>),
    /// A list comprehension
    /// ## Examples
    /// * `i for i = [1...10]`
    /// * `(i, j) for i = [1..10], j = [1...20]`
    ListComp {
        body: BrandedNodeId<'brand>,
        vars: ThinBoxedSlice<(Ident, BrandedNodeId<'brand>)>,
    },
    /// Ambiguous case in the parsing phase, either a function call or implicit multiplication, depending on the type of `prefix` (which cannot be known at parse-time)
    /// ## Examples
    /// * `f_{unc}(a, b)` (function call)
    /// * `v_{ar}(a, b)` (implicit multiply by point constructor)
    /// * `v_{ar}(a)` (implicit multiply)
    Parens {
        prefix: Ident,
        args: ThinBoxedSlice<BrandedNodeId<'brand>>,
    },
    /// with expression
    ///
    With {
        expr: BrandedNodeId<'brand>,
        with_defs: ThinBoxedSlice<(Ident, BrandedNodeId<'brand>)>,
    },
    /// Sum expression
    /// ## Examples
    /// * `\sum_{l_{ower}=1}^{10}l_{ower}`
    /// * `\sum_{n=a}^{b}n`
    Sum {
        /// Ident of the summation variable
        var: Ident,
        /// Initial value of the summation variable
        initial: BrandedNodeId<'brand>,
        /// Final value of the summation variable
        end: BrandedNodeId<'brand>,
    },
    /// Product expression
    /// ## Examples
    /// * `\prod_{n=2}^{10}n`
    /// * `\prod_{n=a}^{b}n`
    Prod {
        /// Ident of the product variable
        var: Ident,
        /// Initial value of the summation variable
        initial: BrandedNodeId<'brand>,
        /// Final value of the summation variable
        end: BrandedNodeId<'brand>,
    },
    /// Integral expression
    /// ## Examples
    /// * `\int_{a}^{b}xdx`
    /// * `\int_{1}^{2}tdt`
    Integral {
        /// Start bound of the integral
        lower: BrandedNodeId<'brand>,
        /// End bound of the integral
        upper: BrandedNodeId<'brand>,
        /// NodeId<'node> to the Integrand of this integral
        integrand: BrandedNodeId<'brand>,
    },
    /// Derivative expression
    /// ## Examples
    /// * `f'(a)`
    /// * `\frac{d}{dx}x^{2}`
    Derivative { diff: BrandedNodeId<'brand> },
    IntegrandOrDiff {
        integration_or_diff_var: Ident,
        expression: BrandedNodeId<'brand>,
    },
}
impl<'brand> BrandedExprNode<'brand> {
    /// Runs the given closure with a reference to a slice containing the child NodeIds of this [`ExprNode`]
    /// (used when ensuring that provided [`NodeId`]s of an untrusted [`ExprNode`] are in-bounds if they are not provided via the unchecked API)
    ///
    /// Short-circuits and returns [`None`] if a node has no children
    pub(crate) fn with_children<R>(
        &self,
        cb: impl FnOnce(&[BrandedNodeId<'brand>]) -> R,
    ) -> Option<R> {
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
            BrandedExprNode::ListComp { body, vars } => {
                let mut v = vars.iter().map(|v| &v.1).copied().collect::<Vec<_>>();
                v.push(*body);
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
            BrandedExprNode::Parens { args, .. } => Some(cb(args)),
            BrandedExprNode::With { expr, with_defs } => {
                let mut v = with_defs.iter().map(|v| &v.1).copied().collect::<Vec<_>>();
                v.push(*expr);
                Some(cb(&v))
            }
        }
    }
    /// Removes the brand on this [`ExprNode`], if it is present, replacing the brand lifetime with `'static`
    pub(crate) fn unbrand_val(self) -> ExprNode {
        // Safety: transmutes that only affect lifetime parameters are unconditionally allowed (i.e. lifetime parameters cannot cause layout instability)
        // the 'static brand is always safe to apply because it is the one brand where the creation of a corresponding UncheckedToken<'static>
        // is impossible due to the construction invariants on it (see Parser::with_token and Parser::with_token_mut)
        unsafe { mem::transmute::<BrandedExprNode<'_>, BrandedExprNode<'static>>(self) }
    }
    /// Removes the brand on this reference to an [`ExprNode`], if it is present, replacing the brand lifetime with `'static`
    pub(crate) fn unbrand_ref(&self) -> &ExprNode {
        // Safety: transmutes that only affect references are unconditionally allowed, for a discussion of the effect of the lifetime
        // cast see the above safety comments in unbrand_val above
        unsafe { mem::transmute::<&BrandedExprNode<'_>, &BrandedExprNode<'static>>(self) }
    }
    /// Removes the brand on this mutable reference to an [`ExprNode`], if it is present, replacing the brand lifetime with `'static`
    pub(crate) fn unbrand_mut(&mut self) -> &mut ExprNode {
        // Safety: transmutes that only affect references are unconditionally allowed, for a discussion of the effect of the lifetime
        // cast see the above safety comments in unbrand_val above
        unsafe { mem::transmute::<&mut BrandedExprNode<'_>, &mut BrandedExprNode<'static>>(self) }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Ident(Ident),
    Literal(f64),
}
/// Range list literal. Encodes a list which starts at `start` and increases or decreases by
/// a fixed interval until reaching the last value whose absolute value
/// is less than or equal to the abosolute value of `end`
#[derive(Debug, Clone)]
pub(crate) struct RangeList<'brand> {
    /// The value to start this range list at
    start: BrandedNodeId<'brand>,

    /// Optional, a secondary value which can be used to set the interval
    /// The formula to get the interval is:
    ///
    /// `interval := interval_marker - start`
    interval_marker: Option<BrandedNodeId<'brand>>,

    /// The value that marks the end of this range list.
    end: BrandedNodeId<'brand>,
}
#[derive(Debug, Clone, Copy)]
pub(crate) enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    NthRoot,
    Mod,
    Min,
    Max,
}
#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Sqrt,
    Neg,
    Sin,
    Cos,
    Tan,
    Csc,
    Sec,
    Cot,
    InvSin,
    InvCos,
    InvTan,
    InvCsc,
    InvSec,
    InvCot,
    Floor,
    Ceil,
    Gamma,
}

#[derive(Debug, Clone, Copy, PartialEq, Hash)]
pub(crate) struct Ident(SymbolU32);
impl From<SymbolU32> for Ident {
    fn from(value: SymbolU32) -> Self {
        Self(value)
    }
}
