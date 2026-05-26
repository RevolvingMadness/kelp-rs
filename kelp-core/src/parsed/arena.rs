use la_arena::{Arena, ArenaMap, Idx};

use crate::{
    parsed::{
        expression::{
            ParsedExpression,
            assignee::{ParsedAssigneeExpression, ParsedAssigneeExpressionId},
            place::{ParsedPlaceExpression, ParsedPlaceExpressionId},
        },
        item::ParsedItem,
        pattern::Pattern,
        statement::Statement,
    },
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
pub struct ParsedAstArena {
    pub items: Arena<ParsedItem>,
    pub item_spans: ArenaMap<Idx<ParsedItem>, Span>,
    pub item_visibilities: ArenaMap<Idx<ParsedItem>, Visibility>,

    pub patterns: Arena<Pattern>,
    pub pattern_spans: ArenaMap<Idx<Pattern>, Span>,

    pub expressions: Arena<Spanned<ParsedExpression>>,

    pub place_expressions: Arena<Spanned<ParsedPlaceExpression>>,

    pub assignee_expressions: Arena<Spanned<ParsedAssigneeExpression>>,

    pub statements: Arena<Spanned<Statement>>,
}

// Items
impl ParsedAstArena {
    #[must_use]
    pub fn allocate_item(
        &mut self,
        span: Span,
        visibility: Visibility,
        item: ParsedItem,
    ) -> Idx<ParsedItem> {
        let id = self.items.alloc(item);

        self.item_spans.insert(id, span);
        self.item_visibilities.insert(id, visibility);

        id
    }

    #[inline]
    #[must_use]
    pub fn get_item(&self, id: Idx<ParsedItem>) -> &ParsedItem {
        &self.items[id]
    }

    #[inline]
    #[must_use]
    pub fn get_item_span(&self, id: Idx<ParsedItem>) -> &Span {
        &self.item_spans[id]
    }

    #[inline]
    #[must_use]
    pub fn get_item_visiblity(&self, id: Idx<ParsedItem>) -> Visibility {
        self.item_visibilities[id]
    }
}

// Patterns
impl ParsedAstArena {
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
impl ParsedAstArena {
    #[inline]
    #[must_use]
    pub fn allocate_expression(
        &mut self,
        span: Span,
        expression: ParsedExpression,
    ) -> Idx<Spanned<ParsedExpression>> {
        self.expressions.alloc(expression.with_span(span))
    }

    #[inline]
    #[must_use]
    pub fn get_expression(&self, id: Idx<Spanned<ParsedExpression>>) -> &Spanned<ParsedExpression> {
        &self.expressions[id]
    }

    #[inline]
    #[must_use]
    pub fn get_expression_value(&self, id: Idx<Spanned<ParsedExpression>>) -> &ParsedExpression {
        &self.get_expression(id).value
    }

    #[inline]
    #[must_use]
    pub fn get_expression_span(&self, id: Idx<Spanned<ParsedExpression>>) -> Span {
        self.get_expression(id).span
    }
}

// Place expressions
impl ParsedAstArena {
    #[inline]
    #[must_use]
    pub fn allocate_place_expression(
        &mut self,
        span: Span,
        expression: ParsedPlaceExpression,
    ) -> ParsedPlaceExpressionId {
        self.place_expressions.alloc(expression.with_span(span))
    }

    #[inline]
    #[must_use]
    pub fn get_place_expression(
        &self,
        id: ParsedPlaceExpressionId,
    ) -> &Spanned<ParsedPlaceExpression> {
        &self.place_expressions[id]
    }

    #[inline]
    #[must_use]
    pub fn get_place_expression_value(
        &self,
        id: ParsedPlaceExpressionId,
    ) -> &ParsedPlaceExpression {
        &self.get_place_expression(id).value
    }

    #[inline]
    #[must_use]
    pub fn get_place_expression_span(&self, id: ParsedPlaceExpressionId) -> Span {
        self.get_place_expression(id).span
    }
}

// Assignee expressions
impl ParsedAstArena {
    #[inline]
    #[must_use]
    pub fn allocate_assignee_expression(
        &mut self,
        span: Span,
        expression: ParsedAssigneeExpression,
    ) -> ParsedAssigneeExpressionId {
        self.assignee_expressions.alloc(expression.with_span(span))
    }

    #[inline]
    #[must_use]
    pub fn get_assignee_expression(
        &self,
        id: ParsedAssigneeExpressionId,
    ) -> &Spanned<ParsedAssigneeExpression> {
        &self.assignee_expressions[id]
    }

    #[inline]
    #[must_use]
    pub fn get_assignee_expression_value(
        &self,
        id: ParsedAssigneeExpressionId,
    ) -> &ParsedAssigneeExpression {
        &self.get_assignee_expression(id).value
    }

    #[inline]
    #[must_use]
    pub fn get_assignee_expression_span(&self, id: ParsedAssigneeExpressionId) -> Span {
        self.get_assignee_expression(id).span
    }
}

// Statements
impl ParsedAstArena {
    #[inline]
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
