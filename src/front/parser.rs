use std::num::NonZeroU32;

use crate::util::thin_boxed_slice::ThinBoxedSlice;
use logos::{Span, SpannedIter};
use string_interner::symbol::SymbolU32;

use super::lexer::Token;

pub(crate) struct Expr {
    nodes: Vec<ExprNode>,
    root: NodeId,
}

pub(crate) struct Parser<'source> {
    lexer: SpannedIter<'source, Token>,
    span_stack: Vec<Span>,
    nodes: Vec<ExprNode>,
    node_spans: Vec<Span>,
}

mod parser_priv {
    use std::marker::PhantomData;

    use super::{NodeId, Parser};

    pub(crate) struct UncheckedToken<'a>(PhantomData<&'a ()>);
    pub(crate) struct UncheckedNodeId<'a>(NodeId, PhantomData<&'a ()>);

    impl std::ops::Deref for UncheckedNodeId<'_> {
        type Target = NodeId;

        fn deref(&self) -> &Self::Target {
            &self.0
        }
    }

    impl<'source> Parser<'source> {
        pub(crate) fn with_tok<R>(
            &self,
            func: impl for<'tok> FnOnce(UncheckedToken<'tok>, &Parser<'source>) -> R,
        ) -> R {
            func(UncheckedToken(PhantomData), self)
        }
        pub(crate) fn with_tok_mut<R>(
            &mut self,
            func: impl for<'tok> FnOnce(UncheckedToken<'tok>, &mut Parser<'source>) -> R,
        ) -> R {
            func(UncheckedToken(PhantomData), self)
        }
    }
}
impl Parser<'_> {}

#[derive(Debug, Clone)]
pub(crate) enum ExprNode {
    NumberLit(f64),
    Ident(Ident),
    Binary(NodeId, NodeId, BinaryOp),
    Unary(NodeId, UnaryOp),
    RangeList(RangeList),
    ListLit(ThinBoxedSlice<NodeId>),
    ListComp(ThinBoxedSlice<(Ident, NodeId)>, NodeId),
    Sum {
        /// Ident of the summation variable
        var: Ident,
        /// Initial value of the summation variable
        initial: NodeId,
        /// Final value of the summation variable
        end: NodeId,
    },
    Prod {
        /// Ident of the product variable
        var: Ident,
        /// Initial value of the summation variable
        initial: NodeId,
        /// Final value of the summation variable
        end: NodeId,
    },
    Integral {
        /// Start bound of the integral
        lower: NodeId,
        /// End bound of the integral
        upper: NodeId,
        /// NodeId to the Integrand of this integral
        integrand: NodeId,
    },
    Derivative {
        diff: NodeId,
    },
    IntegrandOrDiff {
        integration_or_diff_var: Ident,
        expression: NodeId,
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
pub(crate) struct RangeList {
    /// The value to start this range list at
    start: NodeId,

    /// Optional, a secondary value which can be used to set the interval
    /// The formula to get the interval is:
    ///
    /// `interval := interval_marker - start`
    interval_marker: Option<NodeId>,

    /// The value that marks the end of this range list.
    end: NodeId,
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

#[derive(Debug, Clone)]
pub(crate) struct Ident(SymbolU32);

#[derive(Debug, Clone)]
pub(crate) struct NodeId {
    inner: NonZeroU32,
}
