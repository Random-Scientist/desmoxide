use std::mem;

use string_interner::symbol::SymbolU32;

use crate::{
    middle::Comparison,
    util::{
        branded::{CheckByChildIndices, HasBrandLifetime, NotInteriorMutable},
        thin_boxed_slice::ThinBoxedSlice,
    },
};

use super::{BrandedNodeId, NodeId};

#[derive(Debug, Clone)]
pub(crate) struct Expr {
    nodes: Vec<ExprNode<'static>>,
    root: NodeId,
}

/// Node in the AST of a desmos expression
#[derive(Debug, Clone)]
pub(crate) enum ExprNode<'brand> {
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
    /// * `[1...]`
    /// * `[1,4...10]`
    /// * `[1,4...]`
    RangeList {
        start: BrandedNodeId<'brand>,
        interval_marker: Option<BrandedNodeId<'brand>>,
        /// note to implementors: the semantics of the ending of arithmetic sequence are rounding-based, not greter than or equal to.
        /// the implementation is expected to choose the closest
        end: Option<BrandedNodeId<'brand>>,
    },
    /// A standard list literal
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
    /// Ambiguous case in the parsing phase, either a function call, implicit multiplication, or the point constructor, depending on the type of `prefix` (which cannot be known at parse-time)
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
    /// `integrand` invariably points to an IntegrandOrDiff node
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
    /// `diff` invariably points to an IntegrandOrDiff node
    Derivative { diff: BrandedNodeId<'brand> },
    IntegrandOrDiff {
        integration_or_diff_var: Ident,
        expression: BrandedNodeId<'brand>,
    },
}
// Safety: 'brand is required to be invariant by BrandedNodeId,
// raw constructor is safe because because the brand lifetime is constrained by BrandedNodeId,
// which is only safely constructible for 'static
// does not contain any BrandedToken s
unsafe impl<'b> HasBrandLifetime<'b> for ExprNode<'b> {
    type Downcast<'downcast> = ExprNode<'downcast>;
}
// Safety: ExprNode does not contain any interior mutable types
unsafe impl NotInteriorMutable for ExprNode<'_> {}

// unsafe impl CheckByChildIndices for ExprNode<'a> {
//     type Index = BrandedNodeId<'a>;

//     fn with_children<R, T: FnOnce(&[Self::Index]) -> R>(&self, f: T) -> Option<R> {
//         self.with_children(f)
//     }
// }

impl<'brand> ExprNode<'brand> {
    /// Runs the given closure with a reference to a slice containing the child NodeIds of this [`ExprNode`], Short-circuits by returning [`None`] if a node has no children.
    ///
    /// (used when ensuring that provided [`NodeId`]s of an untrusted [`ExprNode`] are in-bounds if they are not provided via the unchecked API)
    ///
    /// ## INVARIANT
    /// the closure is called on every possible child index contained in this node variant
    pub(crate) fn with_children<R>(
        &self,
        cb: impl FnOnce(&[BrandedNodeId<'brand>]) -> R,
    ) -> Option<R> {
        Some(match self {
            ExprNode::NumberLit(_) => return None,
            ExprNode::Ident(_) => return None,
            ExprNode::Binary(id1, id2, _) => cb(&[*id1, *id2]),
            ExprNode::Unary(id1, _) => cb(&[*id1]),
            ExprNode::RangeList {
                start,
                interval_marker,
                end,
            } => match (interval_marker, end) {
                (None, None) => cb(&[*start]),
                (Some(v), None) => cb(&[*start, *v]),
                (None, Some(v)) => cb(&[*start, *v]),
                (Some(a), Some(b)) => cb(&[*start, *a, *b]),
            },
            ExprNode::ListLit(thin_boxed_slice) => cb(thin_boxed_slice),
            ExprNode::ListComp { body, vars } => {
                let mut v = vars.iter().map(|v| &v.1).copied().collect::<Vec<_>>();
                v.push(*body);
                cb(&v)
            }
            ExprNode::Sum { initial, end, .. } => cb(&[*initial, *end]),
            ExprNode::Prod { initial, end, .. } => cb(&[*initial, *end]),
            ExprNode::Integral {
                lower,
                upper,
                integrand,
            } => cb(&[*lower, *upper, *integrand]),
            ExprNode::Derivative { diff } => cb(&[*diff]),
            ExprNode::IntegrandOrDiff { expression, .. } => cb(&[*expression]),
            ExprNode::Parens { args, .. } => cb(args),
            ExprNode::With { expr, with_defs } => {
                let mut v = with_defs.iter().map(|v| &v.1).copied().collect::<Vec<_>>();
                v.push(*expr);
                cb(&v)
            }
        })
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

#[derive(Clone, Debug)]
pub(crate) enum ParsedExpression {
    Var(VariableDefOrExplicit),
    Func(FunctionDef),
    Eq(EquationDef),
    Empty,
}
#[derive(Clone, Debug)]
pub(crate) struct VariableDefOrExplicit {
    name: Ident,
    value: Expr,
}
#[derive(Clone, Debug)]
pub(crate) struct FunctionDef {
    name: Ident,
    parameters: Box<[Ident]>,
    body: Expr,
}
#[derive(Clone, Debug)]
pub(crate) struct EquationDef {
    lhs: Expr,
    comparison: Comparison,
    rhs: Expr,
}
