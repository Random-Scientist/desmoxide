
use crate::util::thin_boxed_slice::ThinBoxedSlice;

use parser_internals::{BrandedNodeId, NodeId};
use string_interner::symbol::SymbolU32;

pub(crate) struct Expr {
    nodes: Vec<ExprNode>,
    root: NodeId,
}
pub(crate) mod parser_internals;

pub(crate) type ExprNode = BrandedExprNode<'static>;
pub(crate) enum BrandedExprNode<'brand> {
    NumberLit(f64),
    Ident(Ident),
    Binary(BrandedNodeId<'brand>, BrandedNodeId<'brand>, BinaryOp),
    Unary(BrandedNodeId<'brand>, UnaryOp),
    RangeList(RangeList<'brand>),
    ListLit(ThinBoxedSlice<BrandedNodeId<'brand>>),
    ListComp(
        ThinBoxedSlice<(Ident, BrandedNodeId<'brand>)>,
        BrandedNodeId<'brand>,
    ),
    Sum {
        /// Ident of the summation variable
        var: Ident,
        /// Initial value of the summation variable
        initial: BrandedNodeId<'brand>,
        /// Final value of the summation variable
        end: BrandedNodeId<'brand>,
    },
    Prod {
        /// Ident of the product variable
        var: Ident,
        /// Initial value of the summation variable
        initial: BrandedNodeId<'brand>,
        /// Final value of the summation variable
        end: BrandedNodeId<'brand>,
    },
    Integral {
        /// Start bound of the integral
        lower: BrandedNodeId<'brand>,
        /// End bound of the integral
        upper: BrandedNodeId<'brand>,
        /// NodeId<'node> to the Integrand of this integral
        integrand: BrandedNodeId<'brand>,
    },
    Derivative {
        diff: BrandedNodeId<'brand>,
    },
    IntegrandOrDiff {
        integration_or_diff_var: Ident,
        expression: BrandedNodeId<'brand>,
    },
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

#[derive(Debug, Clone, Copy)]
pub(crate) struct Ident(SymbolU32);
