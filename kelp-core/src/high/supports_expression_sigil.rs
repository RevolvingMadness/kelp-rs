use std::fmt::Display;

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    high::{
        coordinate::Coordinates, entity_selector::EntitySelector, expression::Expression,
        semantic_analysis::SemanticAnalysisContext,
    },
    low::{
        coordinate::Coordinates as LowCoordinates, data_type::DataType,
        entity_selector::EntitySelector as LowEntitySelector,
        supports_expression_sigil::SupportsExpressionSigil as LowSupportsExpressionSigil,
    },
};

#[derive(Debug, Clone)]
pub enum SupportsExpressionSigil<T> {
    Regular(T),
    Sigil(Expression),
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
            Self::Sigil(_) => f.write_str("..."),
        }
    }
}

impl SupportsExpressionSigil<ResourceLocation> {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<LowSupportsExpressionSigil<ResourceLocation>> {
        Some(match self {
            Self::Regular(value) => LowSupportsExpressionSigil::Regular(value),
            Self::Sigil(expression) => {
                let (expression_span, expression) = expression.perform_semantic_analysis(ctx)?;

                expression.data_type.assert_equals(
                    ctx,
                    expression_span,
                    &DataType::ResourceLocation,
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
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<LowSupportsExpressionSigil<LowEntitySelector>> {
        Some(match self {
            Self::Regular(value) => {
                let value = value.perform_semantic_analysis(ctx)?;

                LowSupportsExpressionSigil::Regular(value)
            }
            Self::Sigil(expression) => {
                let (expression_span, expression) = expression.perform_semantic_analysis(ctx)?;

                expression.data_type.assert_equals(
                    ctx,
                    expression_span,
                    &DataType::EntitySelector,
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
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<LowSupportsExpressionSigil<LowCoordinates>> {
        Some(match self {
            Self::Regular(coordinates) => {
                let coordinates = coordinates.perform_semantic_analysis(ctx)?;

                LowSupportsExpressionSigil::Regular(coordinates)
            }
            Self::Sigil(expression) => {
                let (expression_span, expression) = expression.perform_semantic_analysis(ctx)?;

                expression
                    .data_type
                    .assert_equals(ctx, expression_span, &DataType::Coordinates)?;

                LowSupportsExpressionSigil::Sigil(expression)
            }
        })
    }
}
