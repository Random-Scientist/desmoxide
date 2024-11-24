use std::{
    cell::Cell,
    cmp::Ordering,
    collections::{hash_map::Entry, BTreeMap, HashMap},
    fmt::Debug,
    mem,
    num::NonZeroU32,
    ops::Deref,
    sync::Arc,
};

use error::ParseError;
use lexer::Token;
use logos::{Logos, Span};
use miette::{SourceCode, SourceSpan, SpanContents};
use parser::{expr::ParsedExpression, parser_internals::Parser, Ident, ParsingContext};
use string_interner::{backend::BufferBackend, symbol::SymbolU32, StringInterner};

/// Defines a common "compile error" type with metadata for printing a helpful message, including span information
mod error;
/// Processes an input string into a linear sequence of Tokens
mod lexer;
/// Lowers `Expr`s to DesMIR
mod lower;
/// Parses a sequence of `Token`s to an Expr
mod parser;

#[derive(Clone, Debug)]
pub struct Frontend {
    id_counter: u32,
    cache: HashMap<ExpressionId, ExpressionCache>,
    sources: Arc<DesmoxideSourceCode>,
    intern: InternerCell,
}
impl Frontend {
    pub fn new() -> Frontend {
        Self {
            id_counter: 0,
            cache: HashMap::new(),
            sources: DesmoxideSourceCode::new_arc(),
            intern: InternerCell(Some(StringInterner::new()).into()),
        }
    }
    pub fn new_with_exprs(expressions: Vec<(u32, String)>) -> Self {
        if expressions.is_empty() {
            return Self::new();
        }
        let mut cache = HashMap::new();
        let cache_borrow = &mut cache;
        assert!(expressions.len() <= u32::MAX as usize);
        let mut v = DesmoxideSourceCode::new();
        for (id, expr) in expressions.into_iter() {
            let id = ExpressionId::new(id);
            cache_borrow.insert(id, ExpressionCache::new());
            v.sources.insert(
                id,
                Expression {
                    line_number: None,
                    source: expr.into_boxed_str().into(),
                },
            );
        }
        Frontend {
            id_counter: 0,
            cache,
            sources: Arc::new(v),
            intern: InternerCell(Cell::new(Some(StringInterner::new()))),
        }
    }
    /// Adds a new [`Expression`] to this [`Frontend`], returning a mutable reference to the inner backing string, as well as an
    /// [`ExpressionId`] that can be used to retrieve a reference to the inner string at a later time
    pub fn insert_expr(&mut self, body: String, lineno: Option<NonZeroU32>) -> ExpressionId {
        let mut new_id = u32::MAX - self.id_counter;
        while self.cache.contains_key(&ExpressionId::new(new_id)) {
            // check for underflow
            assert!(new_id != 0);
            new_id -= 1;
            self.id_counter += 1;
        }
        let new_id = ExpressionId::new(new_id);
        self.sources.insert_expression(new_id, body, lineno);
        let _ = match self.cache.entry(new_id) {
            Entry::Vacant(vacant) => vacant.insert(ExpressionCache::new()),
            Entry::Occupied(_) => unreachable!(),
        };
        new_id
    }
    pub fn update_expr(&mut self, id: ExpressionId, update: impl FnOnce(&str) -> String) {
        let exp = self.cache.get_mut(&id).unwrap();
        exp.inner.set(None);
        self.sources.modify_source(id, update);
    }
    /// Returns a reference to the backing string of an expression
    pub fn expr(&self, id: ExpressionId) -> Arc<str> {
        self.sources.get_source(id)
    }
    pub(crate) fn parse_expr(&self, id: ExpressionId) -> Result<(), ParseError> {
        let mut intern = self
            .intern
            .take()
            .expect("String interner should have been present!");
        let cache = self.cache.get(&id).unwrap();
        let res = parser::parse_expression(
            Token::lexer_with_extras(&self.sources.get_source(id), &mut intern),
            ParsingContext {
                sources: &self.sources,
                expression_id: id,
            },
        );
        self.intern.set(Some(intern));
        cache.set(Some(res?));
        Ok(())
    }
}
impl Default for Frontend {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ExpressionId(u32);
impl ExpressionId {
    pub(crate) fn new(val: u32) -> Self {
        Self(val)
    }
}
#[derive(Debug, Clone)]
pub(crate) struct Expression {
    /// optional 1-indexed line number
    line_number: Option<NonZeroU32>,
    source: Arc<str>,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// type that enforces a total ordering on (expression id, line number) without requiring a line number for every ExpressionId
pub(crate) struct ExpressionIdWithMaybeLineNo {
    id: ExpressionId,
    lineno: Option<NonZeroU32>,
}
impl PartialOrd for ExpressionIdWithMaybeLineNo {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for ExpressionIdWithMaybeLineNo {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self.lineno, other.lineno) {
            (Some(lhs), Some(rhs)) => {
                match lhs.cmp(&rhs) {
                    // tie-break equal with ID
                    Ordering::Equal => self.id.cmp(&other.id),
                    a => a,
                }
            }
            (Some(_), None) => Ordering::Greater,
            (None, Some(_)) => Ordering::Less,
            (None, None) => self.id.cmp(&other.id),
        }
    }
}
#[derive(Debug, Clone)]
pub(crate) struct DesmoxideSourceCode {
    /// The list of backing source code for expressions
    sources: HashMap<ExpressionId, Expression>,
    /// BTreeSet that maintains a total ordering of all expressions tracked by this [`Frontend`], mapping an
    /// [`ExpressionIdWithMaybeLineNo`] to the byte length of its corresponding [`Expression`]'s source
    expression_total_order: BTreeMap<ExpressionIdWithMaybeLineNo, usize>,
}
impl DesmoxideSourceCode {
    fn new_arc() -> Arc<Self> {
        Self::new().into()
    }
    fn new() -> Self {
        Self {
            sources: HashMap::new(),
            expression_total_order: BTreeMap::new(),
        }
    }
    fn get_source(self: &Arc<Self>, id: ExpressionId) -> Arc<str> {
        Arc::clone(&self.sources.get(&id).unwrap().source)
    }
    /// Forks this SourceCode, passing the forked value through a closure before storing it in back to the `Arc`
    fn fork_with(self: &mut Arc<Self>, func: impl FnOnce(Self) -> Self) {
        replace_with::replace_with(self, Self::new_arc, |this| {
            Arc::new(func(Arc::unwrap_or_clone(this)))
        });
    }
    fn fork(self: &Arc<Self>) -> Arc<Self> {
        Arc::clone(self)
    }
    fn modify_source(self: &mut Arc<Self>, id: ExpressionId, gen: impl FnOnce(&str) -> String) {
        let new = gen(&self.sources.get(&id).unwrap().source);
        self.fork_with(move |mut this| {
            let len = new.len();
            let r = this.sources.get_mut(&id).unwrap();
            r.source = new.into_boxed_str().into();
            let k = ExpressionIdWithMaybeLineNo {
                id,
                lineno: r.line_number,
            };
            *this.expression_total_order.get_mut(&k).unwrap() = len;
            this
        });
    }
    fn update_lineno(self: &mut Arc<Self>, id: ExpressionId, mut lineno: Option<NonZeroU32>) {
        self.fork_with(|mut this| {
            let r = &mut this.sources.get_mut(&id).unwrap().line_number;
            mem::swap(r, &mut lineno);
            let len = this
                .expression_total_order
                .remove(&ExpressionIdWithMaybeLineNo { id, lineno })
                .expect("expression at id should already exist");
            this.expression_total_order
                .insert(ExpressionIdWithMaybeLineNo { id, lineno: *r }, len);
            this
        });
    }
    fn insert_expression(
        self: &mut Arc<Self>,
        id: ExpressionId,
        body: String,
        lineno: Option<NonZeroU32>,
    ) {
        self.fork_with(|mut this| {
            let len = body.len();
            this.sources.insert(
                id,
                Expression {
                    line_number: lineno,
                    source: body.into_boxed_str().into(),
                },
            );
            this.expression_total_order
                .insert(ExpressionIdWithMaybeLineNo { id, lineno }, len);
            this
        });
    }
    fn map_span(&self, expr: ExpressionId, span: Span) -> SourceSpan {
        let lineno = self.sources.get(&expr).unwrap().line_number;
        let id_with = ExpressionIdWithMaybeLineNo { id: expr, lineno };
        assert!(self.expression_total_order.contains_key(&id_with));
        let off = self
            .expression_total_order
            .range(..id_with)
            .fold(0, |v, (_, &a)| v + a);
        SourceSpan::new((off + span.start).into(), span.end - span.start)
    }
}
pub(crate) struct DesmoxideSpanContents {
    bytes: Arc<str>,
    span: SourceSpan,
    line: usize,
    column: usize,
    line_count: usize,
    expression_number: Option<NonZeroU32>,
    expr_name: Box<str>,
}
impl SpanContents for DesmoxideSpanContents {
    fn data(&self) -> &[u8] {
        self.bytes.as_bytes()
    }

    fn span(&self) -> &SourceSpan {
        &self.span
    }

    fn line(&self) -> usize {
        self.line
    }

    fn column(&self) -> usize {
        self.column
    }

    fn line_count(&self) -> usize {
        self.line
    }
    fn name(&self) -> Option<&str> {
        Some(&*self.expr_name)
    }
}

impl SourceCode for DesmoxideSourceCode {
    fn read_span<'a>(
        &'a self,
        span: &miette::SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> Result<Box<dyn miette::SpanContents + 'a>, miette::MietteError> {
        let mut accum = 0;
        let mut id = None;
        for (&i, &len) in self.expression_total_order.iter() {
            let new = accum + self.sources.get(&i.id).unwrap().source.len();
            if new > span.offset() {
                // means that the requested span starts within this expression
                id = Some(i);
                break;
            }
            accum = new;
        }
        let Some(id) = id else {
            return Err(miette::MietteError::OutOfBounds);
        };
        todo!()
    }
}
pub(crate) struct ExpressionCache {
    /// Cache that holds the latest parsed version of this Expression
    inner: Cell<Option<ParsedExpression>>,
}
impl Debug for ExpressionCache {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Expression")
            .field("cache", &self.clone())
            .finish()
    }
}
impl Clone for ExpressionCache {
    fn clone(&self) -> Self {
        // Teeechnically this impl could cause issues (panic/change of value), but only if we get interrupted (we dont use signal handlers so this wont happen :))
        let t = self.inner.take();
        self.inner.set(t.clone());
        Self {
            inner: Cell::new(t),
        }
    }
}
impl Default for ExpressionCache {
    fn default() -> Self {
        Self::new()
    }
}
impl Deref for ExpressionCache {
    type Target = Cell<Option<ParsedExpression>>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl ExpressionCache {
    pub fn new() -> Self {
        Self {
            inner: Cell::new(None),
        }
    }
}
pub(crate) type Interner = StringInterner<BufferBackend<SymbolU32>>;
pub(crate) struct InternerCell(Cell<Option<Interner>>);
impl Clone for InternerCell {
    fn clone(&self) -> Self {
        let v = self.0.take();
        self.0.set(v.clone());
        Self(Cell::new(v))
    }
}
impl Debug for InternerCell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = self.0.take();
        self.0.set(s.clone());
        f.debug_tuple("InternCell").field(&s).finish()
    }
}
impl Deref for InternerCell {
    type Target = Cell<Option<StringInterner<BufferBackend<SymbolU32>>>>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
