use la_arena::{Arena, ArenaMap, Idx};

use crate::{
    high::{expression::Expression, item::Item, pattern::Pattern, statement::Statement},
    span::Span,
    visibility::Visibility,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

pub trait SpannedExt: Sized {
    #[must_use]
    fn with_span(self, span: Span) -> Spanned<Self>;
}

impl<T> SpannedExt for T {
    #[inline]
    fn with_span(self, span: Span) -> Spanned<Self> {
        Spanned { value: self, span }
    }
}

#[derive(Debug, Clone, Default)]
pub struct HighAstAllocator {
    pub items: Arena<Item>,
    pub item_spans: ArenaMap<Idx<Item>, Span>,
    pub item_visibilities: ArenaMap<Idx<Item>, Visibility>,

    pub patterns: Arena<Pattern>,
    pub pattern_spans: ArenaMap<Idx<Pattern>, Span>,

    pub expressions: Arena<Spanned<Expression>>,

    pub statements: Arena<Spanned<Statement>>,
}

// Items
impl HighAstAllocator {
    #[must_use]
    pub fn allocate_item(&mut self, span: Span, visibility: Visibility, item: Item) -> Idx<Item> {
        let id = self.items.alloc(item);

        self.item_spans.insert(id, span);
        self.item_visibilities.insert(id, visibility);

        id
    }

    #[inline]
    #[must_use]
    pub fn get_item(&self, id: Idx<Item>) -> &Item {
        &self.items[id]
    }

    #[inline]
    #[must_use]
    pub fn get_item_span(&self, id: Idx<Item>) -> &Span {
        &self.item_spans[id]
    }

    #[inline]
    #[must_use]
    pub fn get_item_visiblity(&self, id: Idx<Item>) -> Visibility {
        self.item_visibilities[id]
    }
}

// Patterns
impl HighAstAllocator {
    #[must_use]
    pub fn allocate_pattern(&mut self, span: Span, pattern: Pattern) -> Idx<Pattern> {
        let id = self.patterns.alloc(pattern);

        self.pattern_spans.insert(id, span);

        id
    }

    #[inline]
    #[must_use]
    pub fn get_pattern(&self, id: Idx<Pattern>) -> &Pattern {
        &self.patterns[id]
    }

    #[inline]
    #[must_use]
    pub fn get_pattern_span(&self, id: Idx<Pattern>) -> Span {
        self.pattern_spans[id]
    }
}

// Expressions
impl HighAstAllocator {
    #[must_use]
    pub fn allocate_expression(
        &mut self,
        span: Span,
        expression: Expression,
    ) -> Idx<Spanned<Expression>> {
        self.expressions.alloc(expression.with_span(span))
    }

    #[inline]
    #[must_use]
    pub fn get_expression(&self, id: Idx<Spanned<Expression>>) -> &Spanned<Expression> {
        &self.expressions[id]
    }

    #[inline]
    #[must_use]
    pub fn get_expression_value(&self, id: Idx<Spanned<Expression>>) -> &Expression {
        &self.get_expression(id).value
    }

    #[inline]
    #[must_use]
    pub fn get_expression_span(&self, id: Idx<Spanned<Expression>>) -> Span {
        self.get_expression(id).span
    }
}

// Statements
impl HighAstAllocator {
    #[must_use]
    pub fn allocate_statement(
        &mut self,
        span: Span,
        statment: Statement,
    ) -> Idx<Spanned<Statement>> {
        self.statements.alloc(statment.with_span(span))
    }

    #[inline]
    #[must_use]
    pub fn get_statement(&self, id: Idx<Spanned<Statement>>) -> &Spanned<Statement> {
        &self.statements[id]
    }

    #[inline]
    #[must_use]
    pub fn get_statement_value(&self, id: Idx<Spanned<Statement>>) -> &Statement {
        &self.get_statement(id).value
    }

    #[inline]
    #[must_use]
    pub fn get_statement_span(&self, id: Idx<Spanned<Statement>>) -> Span {
        self.get_statement(id).span
    }
}
