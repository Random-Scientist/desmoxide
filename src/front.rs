use std::{cell::Cell, num::NonZeroU32};

use parser::{Expr, Ident};

use crate::middle::Comparison;

/// Defines a common "compile error" type with metadata for printing a helpful message, including span information
mod error;
/// Processes an input string into a linear sequence of Tokens
mod lexer;
/// Lowers `Expr`s to DesMIR
mod lower;
/// Parses a sequence of `Token`s to an Expr
mod parser;

pub struct ExpressionId(NonZeroU32);
pub(crate) enum ParsedExpression {
    Var(VariableDef),
    Func(FunctionDef),
    Eq(EquationDef),
}
pub(crate) struct VariableDef {
    name: Ident,
    value: Expr,
}
pub(crate) struct FunctionDef {
    name: Ident,
    parameters: Box<[Ident]>,
    body: Expr,
}
pub(crate) struct EquationDef {
    lhs: Expr,
    comparison: Comparison,
    rhs: Expr,
}
pub struct Expression {
    // cache the latest parsed version of this expression
    cache: Cell<Option<ParsedExpression>>,
    backing: String,
}
pub struct Frontend {
    exprs: Vec<Expression>,
}
