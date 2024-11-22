use std::{
    cell::Cell,
    collections::{hash_map::Entry, HashMap},
    fmt::Debug,
    marker::PhantomData,
    num::NonZeroU32,
    ops::Deref,
    sync::Arc,
};

use error::ParseError;
use lexer::Token;
use logos::Logos;
use miette::{MietteSpanContents, SourceCode, SourceSpan, SpanContents};
use parser::{expr::ParsedExpression, parser_internals::Parser, Ident};
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
        let (intern, res) = parser::parse_expression(Token::lexer_with_extras(
            &self.sources.get_source(id),
            &mut intern,
        ));
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExpressionId(u32);
impl ExpressionId {
    pub(crate) fn new(val: u32) -> Self {
        Self(val)
    }
}
#[derive(Debug, Clone)]
pub(crate) struct Expression {
    // 1-indexed, 0: no line number
    line_number: Option<NonZeroU32>,
    source: Arc<str>,
}

#[derive(Debug, Clone)]
pub(crate) struct DesmoxideSourceCode {
    sources: HashMap<ExpressionId, Expression>,
    line_number_mapping: HashMap<NonZeroU32, ExpressionId>,
}
impl DesmoxideSourceCode {
    fn new_arc() -> Arc<Self> {
        Self::new().into()
    }
    fn new() -> Self {
        Self {
            sources: HashMap::new(),
            line_number_mapping: HashMap::new(),
        }
    }
    fn get_source(self: &Arc<Self>, id: ExpressionId) -> Arc<str> {
        Arc::clone(&self.sources.get(&id).unwrap().source)
    }
    fn with_fork(self: &mut Arc<Self>, func: impl FnOnce(Self) -> Self) {
        replace_with::replace_with(self, Self::new_arc, |this| {
            Arc::new(func(Arc::unwrap_or_clone(this)))
        });
    }
    fn modify_source(self: &mut Arc<Self>, id: ExpressionId, gen: impl FnOnce(&str) -> String) {
        let new = gen(&self.sources.get(&id).unwrap().source);
        self.with_fork(move |mut this| {
            this.sources.get_mut(&id).unwrap().source = new.into_boxed_str().into();
            this
        });
    }
    fn update_lineno(self: &mut Arc<Self>, id: ExpressionId, lineno: Option<NonZeroU32>) {
        self.with_fork(|this| {
            this.sources.get_mut(&id).unwrap().line_number = lineno;
            if let Some(line) = lineno {
                this.line_number_mapping.insert(line, id);
            }
        });
    }
    fn insert_expression(
        self: &mut Arc<Self>,
        id: ExpressionId,
        body: String,
        lineno: Option<NonZeroU32>,
    ) {
        self.with_fork(|mut this| {
            this.sources.insert(
                id,
                Expression {
                    line_number: lineno,
                    source: body.into_boxed_str().into(),
                },
            );
            if let Some(i) = lineno {
                this.line_number_mapping.insert(i, id);
            }
            this
        });
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
        &self.bytes.as_bytes()
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
