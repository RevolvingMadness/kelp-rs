use la_arena::{Arena, ArenaMap, Idx};

use crate::{
    high::{expression::Expression, item::Item, pattern::Pattern, statement::Statement},
    span::Span,
    visibility::Visibility,
};

#[derive(Debug, Clone, Default)]
pub struct HighAstAllocator {
    pub items: Arena<Item>,
    pub item_spans: ArenaMap<Idx<Item>, Span>,
    pub item_visibilities: ArenaMap<Idx<Item>, Visibility>,

    pub patterns: Arena<Pattern>,
    pub pattern_spans: ArenaMap<Idx<Pattern>, Span>,

    pub expressions: Arena<Expression>,
    pub expression_spans: ArenaMap<Idx<Expression>, Span>,

    pub statements: Arena<Statement>,
    pub statement_spans: ArenaMap<Idx<Statement>, Span>,
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
    pub fn allocate_expression(&mut self, span: Span, expression: Expression) -> Idx<Expression> {
        let id = self.expressions.alloc(expression);

        self.expression_spans.insert(id, span);

        id
    }

    #[inline]
    #[must_use]
    pub fn get_expression(&self, id: Idx<Expression>) -> &Expression {
        &self.expressions[id]
    }

    #[inline]
    #[must_use]
    pub fn get_expression_span(&self, id: Idx<Expression>) -> Span {
        self.expression_spans[id]
    }
}

// Statements
impl HighAstAllocator {
    #[must_use]
    pub fn allocate_statement(&mut self, span: Span, statment: Statement) -> Idx<Statement> {
        let id = self.statements.alloc(statment);

        self.statement_spans.insert(id, span);

        id
    }

    #[inline]
    #[must_use]
    pub fn get_statement(&self, id: Idx<Statement>) -> &Statement {
        &self.statements[id]
    }

    #[inline]
    #[must_use]
    pub fn get_statement_span(&self, id: Idx<Statement>) -> Span {
        self.statement_spans[id]
    }
}
