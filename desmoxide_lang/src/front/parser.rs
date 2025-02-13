use std::sync::Arc;

pub(crate) use expr::{Expr, ExprNode, Ident};
use logos::{Lexer, Span};
use miette::SourceSpan;
use parser_internals::GenericSpanIter;
pub(crate) use parser_internals::NodeId;

use crate::util::restartable_iter::RestartableIterExt;

use super::{
    error::{FrontendError, ParseError},
    DesmoxideSourceCode, ExpressionId, ParsedExpression, Parser, Token,
};

/// Defines the AST node type, ([`ExprNode`]) and parsed AST ([`Expr`])
pub(crate) mod expr;
/// Defines the [`Parser`] type, which consumes a stream of tokens and constructs a parsed AST
pub(crate) mod parser_internals;

/// Parses a full [`Expression`](super::Expression) (i.e. a full line of a Desmos graph).
/// Can be one of the following:
/// * `[ident] = [expr]`: variable definition ([`ParsedExpression::Var`])
/// * `[expr] [comparison] [expr]`: equation ([`ParsedExpression::Eq`])
/// * `[ident]([ident], [ident], ...) = [expr]`: function definition ([`ParsedExpression::Func`])
/// * `[empty]`: empty expression ([`ParsedExpression::Empty`])
pub(crate) fn parse_expression(
    lex: Lexer<'_, Token>,
    ctx: ParsingContext<'_>,
) -> Result<ParsedExpression, FrontendError> {
    let mut spanned_iter = lex.spanned().restartable();
    match spanned_iter.next() {
        Some((tok, span)) => todo!(),
        None => return Ok(ParsedExpression::Empty),
    }
    loop {}

    let p = Parser::new_with(spanned_iter, ctx);
    todo!();
}

impl<'source, Lex: GenericSpanIter<'source>> Parser<'source, Lex> {
    /// Parses an [`Expr`] out of this, which is a self contained chunk of code that produces a value (akin to a Rust block)
    /// The actual semantics of [`Expr`]s are ambiguous without further context, for this reason the lowering
    /// code takes in a LoweringContext which is queried to resolve any ambiguities as the AST is lowered
    pub(crate) fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        todo!()
    }
    fn next_token(&mut self) -> Result<Token, ParseError> {
        let Some((tok, span)) = self.lexer.next() else {
            return Err(ParseError::IncompleteExpression);
        };
        let mapped = self.ctx.map_span(span);
        if let Some(r) = self.span_stack.last_mut() {
            if r.is_none() {
                *r = Some(mapped);
            }
        }
        tok.map_err(|e| ParseError::LexError {
            err: e,
            span: mapped,
        })
    }
    fn spanstack_push(&mut self) {
        self.span_stack.push(None);
    }
    fn spanstack_pop(&mut self) {
        let _ = self.span_stack.pop();
    }
}
#[derive(Debug, Clone, Copy)]
pub(super) struct ParsingContext<'a> {
    pub(super) sources: &'a Arc<DesmoxideSourceCode>,
    pub(super) expression_id: ExpressionId,
    pub(super) start_offset: usize,
}
impl ParsingContext<'_> {
    fn map_span(&self, s: Span) -> SourceSpan {
        SourceSpan::new((s.start + self.start_offset).into(), s.len())
    }
}
