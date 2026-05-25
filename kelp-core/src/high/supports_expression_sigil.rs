use std::fmt::Display;

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    high::{
        coordinate::Coordinates,
        entity_selector::EntitySelector,
        expression::{Expression, ExpressionId},
        semantic_analysis::SemanticAnalysisContext,
    },
    low::{
        coordinate::Coordinates as LowCoordinates, data_type::unresolved::UnresolvedDataType,
        entity_selector::EntitySelector as LowEntitySelector,
        supports_expression_sigil::SupportsExpressionSigil as LowSupportsExpressionSigil,
    },
};

#[derive(Debug, Clone)]
pub enum SupportsExpressionSigil<T> {
    Regular(T),
    Sigil(ExpressionId),
}

pub trait RegularSupportsExpressionSigilExt: Sized {
    fn regular_sigil(self) -> SupportsExpressionSigil<Self>;
}

impl<T> RegularSupportsExpressionSigilExt for T {
    fn regular_sigil(self) -> SupportsExpressionSigil<Self> {
        SupportsExpressionSigil::Regular(self)
    }
}

impl<T: Display> Display for SupportsExpressionSigil<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Regular(value) => value.fmt(f),
            Self::Sigil(..) => f.write_str("..."),
        }
    }
}

impl SupportsExpressionSigil<ResourceLocation> {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<LowSupportsExpressionSigil<ResourceLocation>> {
        Some(match self {
            Self::Regular(value) => LowSupportsExpressionSigil::Regular(value),
            Self::Sigil(expression) => {
                let expression_span = high_allocator.get_expression_span(expression);

                let expression = Expression::perform_semantic_analysis(
                    expression,
                    high_allocator,
                    low_allocator,
                    ctx,
                )?;

                let expression_type = low_allocator.get_expression_type(expression);

                expression_type.assert_equals(
                    ctx,
                    expression_span,
                    &UnresolvedDataType::ResourceLocation,
                )?;

                LowSupportsExpressionSigil::Sigil(expression)
            }
        })
    }
}

impl SupportsExpressionSigil<EntitySelector> {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<LowSupportsExpressionSigil<LowEntitySelector>> {
        Some(match self {
            Self::Regular(value) => {
                let value = value.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                LowSupportsExpressionSigil::Regular(value)
            }
            Self::Sigil(expression) => {
                let expression_span = high_allocator.get_expression_span(expression);

                let expression = Expression::perform_semantic_analysis(
                    expression,
                    high_allocator,
                    low_allocator,
                    ctx,
                )?;

                let expression_type = low_allocator.get_expression_type(expression);

                expression_type.assert_equals(
                    ctx,
                    expression_span,
                    &UnresolvedDataType::EntitySelector,
                )?;

                LowSupportsExpressionSigil::Sigil(expression)
            }
        })
    }
}

impl SupportsExpressionSigil<Coordinates> {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<LowSupportsExpressionSigil<LowCoordinates>> {
        Some(match self {
            Self::Regular(coordinates) => {
                let coordinates =
                    coordinates.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                LowSupportsExpressionSigil::Regular(coordinates)
            }
            Self::Sigil(expression) => {
                let expression_span = high_allocator.get_expression_span(expression);

                let expression = Expression::perform_semantic_analysis(
                    expression,
                    high_allocator,
                    low_allocator,
                    ctx,
                )?;

                let expression_type = low_allocator.get_expression_type(expression);

                expression_type.assert_equals(
                    ctx,
                    expression_span,
                    &UnresolvedDataType::Coordinates,
                )?;

                LowSupportsExpressionSigil::Sigil(expression)
            }
        })
    }
}
