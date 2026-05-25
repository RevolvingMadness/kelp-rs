use la_arena::{Arena, ArenaMap, Idx};

use crate::{
    high::expression::{
        assignee::{UnresolvedAssigneeExpression, UnresolvedAssigneeExpressionId},
        place::{UnresolvedPlaceExpression, UnresolvedPlaceExpressionId},
    },
    low::{
        data_type::unresolved::UnresolvedDataType,
        expression::unresolved::{UnresolvedExpression, UnresolvedExpressionId},
        item::Item,
        pattern::UnresolvedPattern,
        statement::UnresolvedStatement,
    },
    visibility::Visibility,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Typed<T> {
    pub value: T,
    pub data_type: UnresolvedDataType,
}

pub trait TypedExt: Sized {
    #[must_use]
    fn with_type(self, span: UnresolvedDataType) -> Typed<Self>;
}

impl<T> TypedExt for T {
    #[inline]
    fn with_type(self, span: UnresolvedDataType) -> Typed<Self> {
        Typed {
            value: self,
            data_type: span,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct LowAstAllocator {
    items: Arena<Item>,
    item_visiblities: ArenaMap<Idx<Item>, Visibility>,

    patterns: Arena<UnresolvedPattern>,

    expressions: Arena<Typed<UnresolvedExpression>>,

    place_expressions: Arena<Typed<UnresolvedPlaceExpression>>,

    assignee_expressions: Arena<Typed<UnresolvedAssigneeExpression>>,

    statements: Arena<UnresolvedStatement>,
}

// Items
impl LowAstAllocator {
    #[inline]
    #[must_use]
    pub fn allocate_item(&mut self, item: Item) -> Idx<Item> {
        self.items.alloc(item)
    }

    #[inline]
    #[must_use]
    pub fn get_item(&self, id: Idx<Item>) -> &Item {
        &self.items[id]
    }

    #[inline]
    #[must_use]
    pub fn get_item_visiblity(&self, id: Idx<Item>) -> Visibility {
        self.item_visiblities[id]
    }
}

// Patterns
impl LowAstAllocator {
    #[inline]
    #[must_use]
    pub fn allocate_pattern(&mut self, pattern: UnresolvedPattern) -> Idx<UnresolvedPattern> {
        self.patterns.alloc(pattern)
    }

    #[inline]
    #[must_use]
    pub fn get_pattern(&self, id: Idx<UnresolvedPattern>) -> &UnresolvedPattern {
        &self.patterns[id]
    }
}

// Expressions
impl LowAstAllocator {
    #[inline]
    #[must_use]
    pub fn allocate_expression(
        &mut self,
        expression: UnresolvedExpression,
        data_type: UnresolvedDataType,
    ) -> UnresolvedExpressionId {
        self.expressions.alloc(expression.with_type(data_type))
    }

    #[inline]
    #[must_use]
    pub fn get_expression(&self, id: UnresolvedExpressionId) -> &Typed<UnresolvedExpression> {
        &self.expressions[id]
    }

    #[inline]
    #[must_use]
    pub fn get_expression_value(&self, id: UnresolvedExpressionId) -> &UnresolvedExpression {
        &self.get_expression(id).value
    }

    #[inline]
    #[must_use]
    pub fn get_expression_type(&self, id: UnresolvedExpressionId) -> &UnresolvedDataType {
        &self.get_expression(id).data_type
    }
}

// Place expressions
impl LowAstAllocator {
    #[inline]
    #[must_use]
    pub fn allocate_place_expression(
        &mut self,
        expression: UnresolvedPlaceExpression,
        data_type: UnresolvedDataType,
    ) -> UnresolvedPlaceExpressionId {
        self.place_expressions
            .alloc(expression.with_type(data_type))
    }

    #[inline]
    #[must_use]
    pub fn get_place_expression(
        &self,
        id: UnresolvedPlaceExpressionId,
    ) -> &Typed<UnresolvedPlaceExpression> {
        &self.place_expressions[id]
    }

    #[inline]
    #[must_use]
    pub fn get_place_expression_value(
        &self,
        id: UnresolvedPlaceExpressionId,
    ) -> &UnresolvedPlaceExpression {
        &self.get_place_expression(id).value
    }

    #[inline]
    #[must_use]
    pub fn get_place_expression_type(
        &self,
        id: UnresolvedPlaceExpressionId,
    ) -> &UnresolvedDataType {
        &self.get_place_expression(id).data_type
    }
}

// Assignee expressions
impl LowAstAllocator {
    #[inline]
    #[must_use]
    pub fn allocate_assignee_expression(
        &mut self,
        expression: UnresolvedAssigneeExpression,
        data_type: UnresolvedDataType,
    ) -> UnresolvedAssigneeExpressionId {
        self.assignee_expressions
            .alloc(expression.with_type(data_type))
    }

    #[inline]
    #[must_use]
    pub fn get_assignee_expression(
        &self,
        id: UnresolvedAssigneeExpressionId,
    ) -> &Typed<UnresolvedAssigneeExpression> {
        &self.assignee_expressions[id]
    }

    #[inline]
    #[must_use]
    pub fn get_assignee_expression_value(
        &self,
        id: UnresolvedAssigneeExpressionId,
    ) -> &UnresolvedAssigneeExpression {
        &self.get_assignee_expression(id).value
    }

    #[inline]
    #[must_use]
    pub fn get_assignee_expression_type(
        &self,
        id: UnresolvedAssigneeExpressionId,
    ) -> &UnresolvedDataType {
        &self.get_assignee_expression(id).data_type
    }
}

// Statements
impl LowAstAllocator {
    #[inline]
    #[must_use]
    pub fn allocate_statement(
        &mut self,
        statment: UnresolvedStatement,
    ) -> Idx<UnresolvedStatement> {
        self.statements.alloc(statment)
    }

    #[inline]
    #[must_use]
    pub fn get_statement(&self, id: Idx<UnresolvedStatement>) -> &UnresolvedStatement {
        &self.statements[id]
    }
}
