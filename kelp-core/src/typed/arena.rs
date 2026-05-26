use la_arena::{Arena, ArenaMap, Idx};

use crate::{
    parsed::expression::{
        assignee::{ParsedAssigneeExpression, ParsedAssigneeExpressionId},
        place::{ParsedPlaceExpression, ParsedPlaceExpressionId},
    },
    typed::{
        data_type::SemanticDataType,
        expression::{TypedExpression, TypedExpressionId},
        item::TypedItem,
        pattern::TypedPattern,
        statement::TypedStatement,
    },
    visibility::Visibility,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Typed<T> {
    pub value: T,
    pub data_type: SemanticDataType,
}

pub trait TypedExt: Sized {
    #[must_use]
    fn with_type(self, span: SemanticDataType) -> Typed<Self>;
}

impl<T> TypedExt for T {
    #[inline]
    fn with_type(self, span: SemanticDataType) -> Typed<Self> {
        Typed {
            value: self,
            data_type: span,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct TypedAstArena {
    items: Arena<TypedItem>,
    item_visiblities: ArenaMap<Idx<TypedItem>, Visibility>,

    patterns: Arena<TypedPattern>,

    expressions: Arena<Typed<TypedExpression>>,

    place_expressions: Arena<Typed<ParsedPlaceExpression>>,

    assignee_expressions: Arena<Typed<ParsedAssigneeExpression>>,

    statements: Arena<TypedStatement>,
}

// Items
impl TypedAstArena {
    #[inline]
    #[must_use]
    pub fn allocate_item(&mut self, item: TypedItem) -> Idx<TypedItem> {
        self.items.alloc(item)
    }

    #[inline]
    #[must_use]
    pub fn get_item(&self, id: Idx<TypedItem>) -> &TypedItem {
        &self.items[id]
    }

    #[inline]
    #[must_use]
    pub fn get_item_visiblity(&self, id: Idx<TypedItem>) -> Visibility {
        self.item_visiblities[id]
    }
}

// Patterns
impl TypedAstArena {
    #[inline]
    #[must_use]
    pub fn allocate_pattern(&mut self, pattern: TypedPattern) -> Idx<TypedPattern> {
        self.patterns.alloc(pattern)
    }

    #[inline]
    #[must_use]
    pub fn get_pattern(&self, id: Idx<TypedPattern>) -> &TypedPattern {
        &self.patterns[id]
    }
}

// Expressions
impl TypedAstArena {
    #[inline]
    #[must_use]
    pub fn allocate_expression(
        &mut self,
        expression: TypedExpression,
        data_type: SemanticDataType,
    ) -> TypedExpressionId {
        self.expressions.alloc(expression.with_type(data_type))
    }

    #[inline]
    #[must_use]
    pub fn get_expression(&self, id: TypedExpressionId) -> &Typed<TypedExpression> {
        &self.expressions[id]
    }

    #[inline]
    #[must_use]
    pub fn get_expression_value(&self, id: TypedExpressionId) -> &TypedExpression {
        &self.get_expression(id).value
    }

    #[inline]
    #[must_use]
    pub fn get_expression_type(&self, id: TypedExpressionId) -> &SemanticDataType {
        &self.get_expression(id).data_type
    }
}

// Place expressions
impl TypedAstArena {
    #[inline]
    #[must_use]
    pub fn allocate_place_expression(
        &mut self,
        expression: ParsedPlaceExpression,
        data_type: SemanticDataType,
    ) -> ParsedPlaceExpressionId {
        self.place_expressions
            .alloc(expression.with_type(data_type))
    }

    #[inline]
    #[must_use]
    pub fn get_place_expression(
        &self,
        id: ParsedPlaceExpressionId,
    ) -> &Typed<ParsedPlaceExpression> {
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
    pub fn get_place_expression_type(&self, id: ParsedPlaceExpressionId) -> &SemanticDataType {
        &self.get_place_expression(id).data_type
    }
}

// Assignee expressions
impl TypedAstArena {
    #[inline]
    #[must_use]
    pub fn allocate_assignee_expression(
        &mut self,
        expression: ParsedAssigneeExpression,
        data_type: SemanticDataType,
    ) -> ParsedAssigneeExpressionId {
        self.assignee_expressions
            .alloc(expression.with_type(data_type))
    }

    #[inline]
    #[must_use]
    pub fn get_assignee_expression(
        &self,
        id: ParsedAssigneeExpressionId,
    ) -> &Typed<ParsedAssigneeExpression> {
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
    pub fn get_assignee_expression_type(
        &self,
        id: ParsedAssigneeExpressionId,
    ) -> &SemanticDataType {
        &self.get_assignee_expression(id).data_type
    }
}

// Statements
impl TypedAstArena {
    #[inline]
    #[must_use]
    pub fn allocate_statement(&mut self, statment: TypedStatement) -> Idx<TypedStatement> {
        self.statements.alloc(statment)
    }

    #[inline]
    #[must_use]
    pub fn get_statement(&self, id: Idx<TypedStatement>) -> &TypedStatement {
        &self.statements[id]
    }
}
