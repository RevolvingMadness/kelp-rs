use std::fmt::Display;

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    parsed::{
        coordinate::Coordinates,
        entity_selector::EntitySelector,
        expression::{ParsedExpression, ParsedExpressionId},
        semantic_analysis::SemanticAnalysisContext,
    },
    typed::{
        coordinate::TypedCoordinates, data_type::SemanticDataType,
        entity_selector::TypedEntitySelector,
        supports_expression_sigil::TypedSupportsExpressionSigil,
    },
};

#[derive(Debug, Clone)]
pub enum ParsedSupportsExpressionSigil<T> {
    Regular(T),
    Sigil(ParsedExpressionId),
}

pub trait RegularSupportsExpressionSigilExt: Sized {
    fn regular_sigil(self) -> ParsedSupportsExpressionSigil<Self>;
}

impl<T> RegularSupportsExpressionSigilExt for T {
    fn regular_sigil(self) -> ParsedSupportsExpressionSigil<Self> {
        ParsedSupportsExpressionSigil::Regular(self)
    }
}

impl<T: Display> Display for ParsedSupportsExpressionSigil<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Regular(value) => value.fmt(f),
            Self::Sigil(..) => f.write_str("..."),
        }
    }
}

impl ParsedSupportsExpressionSigil<ResourceLocation> {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedSupportsExpressionSigil<ResourceLocation>> {
        Some(match self {
            Self::Regular(value) => TypedSupportsExpressionSigil::Regular(value),
            Self::Sigil(expression) => {
                let expression_span = high_allocator.get_expression_span(expression);

                let expression = ParsedExpression::perform_semantic_analysis(
                    expression,
                    high_allocator,
                    low_allocator,
                    ctx,
                )?;

                let expression_type = low_allocator.get_expression_type(expression);

                expression_type.assert_equals(
                    ctx,
                    expression_span,
                    &SemanticDataType::ResourceLocation,
                )?;

                TypedSupportsExpressionSigil::Sigil(expression)
            }
        })
    }
}

impl ParsedSupportsExpressionSigil<EntitySelector> {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedSupportsExpressionSigil<TypedEntitySelector>> {
        Some(match self {
            Self::Regular(value) => {
                let value = value.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                TypedSupportsExpressionSigil::Regular(value)
            }
            Self::Sigil(expression) => {
                let expression_span = high_allocator.get_expression_span(expression);

                let expression = ParsedExpression::perform_semantic_analysis(
                    expression,
                    high_allocator,
                    low_allocator,
                    ctx,
                )?;

                let expression_type = low_allocator.get_expression_type(expression);

                expression_type.assert_equals(
                    ctx,
                    expression_span,
                    &SemanticDataType::EntitySelector,
                )?;

                TypedSupportsExpressionSigil::Sigil(expression)
            }
        })
    }
}

impl ParsedSupportsExpressionSigil<Coordinates> {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedSupportsExpressionSigil<TypedCoordinates>> {
        Some(match self {
            Self::Regular(coordinates) => {
                let coordinates =
                    coordinates.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                TypedSupportsExpressionSigil::Regular(coordinates)
            }
            Self::Sigil(expression) => {
                let expression_span = high_allocator.get_expression_span(expression);

                let expression = ParsedExpression::perform_semantic_analysis(
                    expression,
                    high_allocator,
                    low_allocator,
                    ctx,
                )?;

                let expression_type = low_allocator.get_expression_type(expression);

                expression_type.assert_equals(
                    ctx,
                    expression_span,
                    &SemanticDataType::Coordinates,
                )?;

                TypedSupportsExpressionSigil::Sigil(expression)
            }
        })
    }
}
