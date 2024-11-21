use std::{
    cell::Cell,
    collections::{hash_map::Entry, HashMap},
    fmt::Debug,
    ops::Deref,
};

use error::ParseError;
use lexer::Token;
use logos::Logos;
use parser::{parser_internals::Parser, Expr, Ident};
use string_interner::{
    backend::BufferBackend,
    symbol::SymbolU32,
    StringInterner,
};

use crate::middle::Comparison;

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
    exprs: HashMap<ExpressionId, Expression>,
    intern: InternerCell,
}
impl Frontend {
    pub fn new() -> Frontend {
        Self {
            id_counter: 0,
            exprs: HashMap::new(),
            intern: InternerCell(Some(StringInterner::new()).into()),
        }
    }
    pub fn new_with_exprs(expressions: Vec<(u32, String)>) -> Self {
        let mut s = Self::new();
        if expressions.is_empty() {
            return s;
        }
        assert!(expressions.len() <= u32::MAX as usize);
        s.exprs.extend(
            expressions
                .into_iter()
                .map(|(id, src)| (ExpressionId::new(id), Expression::with_source(src))),
        );
        s
    }
    /// Adds a new [`Expression`] to this [`Frontend`], returning a mutable reference to the inner backing string, as well as an
    /// [`ExpressionId`] that can be used to retrieve a reference to the inner string at a later time
    pub fn new_expr(&mut self) -> (&mut String, ExpressionId) {
        let mut new_id = u32::MAX - self.id_counter;
        while self.exprs.contains_key(&ExpressionId::new(new_id)) {
            // check for underflow
            assert!(new_id != 0);
            new_id -= 1;
            self.id_counter += 1;
        }
        let new_id = ExpressionId::new(new_id);
        let v = match self.exprs.entry(new_id) {
            Entry::Vacant(vacant) => vacant.insert(Expression::new()),
            Entry::Occupied(_) => unreachable!(),
        };
        (&mut v.backing, new_id)
    }
    /// Returns a mutable reference to the backing string of an [`Expression`].
    ///
    /// The implementation pessimistically invalidates the parse cache for this expression
    /// due to the mutability of the reference. Prefer using [`Frontend::expr`] where possible.
    pub fn update_expr(&mut self, id: ExpressionId) -> &mut String {
        let exp = self.exprs.get_mut(&id).unwrap();
        exp.cache.set(None);
        &mut exp.backing
    }
    /// Returns a reference to the backing string of an [`Expression`]
    pub fn expr(&self, id: ExpressionId) -> &String {
        &self.exprs.get(&id).unwrap().backing
    }
    pub(crate) fn parse_expr(&self, id: ExpressionId) -> Result<(), ParseError> {
        let mut intern = self
            .intern
            .take()
            .expect("String interner should have been present!");
        let expression = self.exprs.get(&id).unwrap();
        let (intern, res) =
            parser::parse_expression(Token::lexer_with_extras(&expression.backing, &mut intern));
        self.intern.set(Some(intern));
        expression.cache.set(Some(res?));
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

#[derive(Clone, Debug)]
pub(crate) enum ParsedExpression {
    Var(VariableDef),
    Func(FunctionDef),
    Eq(EquationDef),
}
#[derive(Clone, Debug)]
pub(crate) struct VariableDef {
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

pub(crate) struct Expression {
    /// Cache that holds the latest parsed version of this Expression
    cache: Cell<Option<ParsedExpression>>,
    /// holds the expression text
    backing: String,
}
impl Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Expression")
            .field("cache", &self.clone())
            .field("backing", &self.backing)
            .finish()
    }
}
impl Clone for Expression {
    fn clone(&self) -> Self {
        // Teeechnically this impl could cause issues (panic/change of value), but only if we get interrupted (we dont use signal handlers so this wont happen :))
        let t = self.cache.take();
        self.cache.set(t.clone());
        Self {
            cache: Cell::new(t),
            backing: self.backing.clone(),
        }
    }
}
impl Expression {
    pub fn new() -> Self {
        Self {
            cache: Cell::new(None),
            backing: String::new(),
        }
    }
    pub fn with_source(source: String) -> Self {
        let mut s = Self::new();
        s.backing = source;
        s
    }
}
impl Default for Expression {
    fn default() -> Self {
        Self::new()
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
