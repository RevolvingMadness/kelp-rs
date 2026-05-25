use la_arena::{Arena, ArenaMap, Idx};

use crate::{
    high::expression::{assignee::UnresolvedAssigneeExpression, place::UnresolvedPlaceExpression},
    low::{
        data_type::unresolved::UnresolvedDataType, expression::unresolved::UnresolvedExpression,
        item::Item, pattern::UnresolvedPattern, statement::UnresolvedStatement,
    },
    visibility::Visibility,
};

#[derive(Debug, Clone, Default)]
pub struct LowAstAllocator {
    items: Arena<Item>,
    item_visiblities: ArenaMap<Idx<Item>, Visibility>,

    patterns: Arena<UnresolvedPattern>,

    expressions: Arena<UnresolvedExpression>,
    expression_types: ArenaMap<Idx<UnresolvedExpression>, UnresolvedDataType>,

    place_expressions: Arena<UnresolvedPlaceExpression>,
    place_expression_types: ArenaMap<Idx<UnresolvedPlaceExpression>, UnresolvedDataType>,

    assignee_expressions: Arena<UnresolvedAssigneeExpression>,
    assignee_expression_types: ArenaMap<Idx<UnresolvedAssigneeExpression>, UnresolvedDataType>,

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
    #[must_use]
    pub fn allocate_expression(
        &mut self,
        expression: UnresolvedExpression,
        data_type: UnresolvedDataType,
    ) -> Idx<UnresolvedExpression> {
        let id = self.expressions.alloc(expression);

        self.expression_types.insert(id, data_type);

        id
    }

    #[inline]
    #[must_use]
    pub fn get_expression(&self, id: Idx<UnresolvedExpression>) -> &UnresolvedExpression {
        &self.expressions[id]
    }

    #[inline]
    #[must_use]
    pub fn get_expression_type(&self, id: Idx<UnresolvedExpression>) -> &UnresolvedDataType {
        &self.expression_types[id]
    }
}

// Place expressions
impl LowAstAllocator {
    #[must_use]
    pub fn allocate_place_expression(
        &mut self,
        expression: UnresolvedPlaceExpression,
        data_type: UnresolvedDataType,
    ) -> Idx<UnresolvedPlaceExpression> {
        let id = self.place_expressions.alloc(expression);

        self.place_expression_types.insert(id, data_type);

        id
    }

    #[inline]
    #[must_use]
    pub fn get_place_expression(
        &self,
        id: Idx<UnresolvedPlaceExpression>,
    ) -> &UnresolvedPlaceExpression {
        &self.place_expressions[id]
    }

    #[inline]
    #[must_use]
    pub fn get_place_expression_type(
        &self,
        id: Idx<UnresolvedPlaceExpression>,
    ) -> &UnresolvedDataType {
        &self.place_expression_types[id]
    }
}

// Assignee expressions
impl LowAstAllocator {
    #[must_use]
    pub fn allocate_assignee_expression(
        &mut self,
        expression: UnresolvedAssigneeExpression,
        data_type: UnresolvedDataType,
    ) -> Idx<UnresolvedAssigneeExpression> {
        let id = self.assignee_expressions.alloc(expression);

        self.assignee_expression_types.insert(id, data_type);

        id
    }

    #[inline]
    #[must_use]
    pub fn get_assignee_expression(
        &self,
        id: Idx<UnresolvedAssigneeExpression>,
    ) -> &UnresolvedAssigneeExpression {
        &self.assignee_expressions[id]
    }

    #[inline]
    #[must_use]
    pub fn get_assignee_expression_type(
        &self,
        id: Idx<UnresolvedAssigneeExpression>,
    ) -> &UnresolvedDataType {
        &self.assignee_expression_types[id]
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
