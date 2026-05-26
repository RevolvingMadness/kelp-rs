use std::fmt::Display;

use minecraft_command_types::resource_location::ResourceLocation;

use crate::semantic::data_type::SemanticDataType;
use crate::{
    parsed::{
        coordinate::ParsedCoordinates, entity_selector::ParsedEntitySelector,
        expression::ParsedExpression, semantic_analysis::SemanticAnalysisContext,
    },
    semantic::{
        coordinate::SemanticCoordinates, entity_selector::SemanticEntitySelector,
        supports_expression_sigil::SemanticSupportsExpressionSigil,
    },
};

#[derive(Debug, Clone)]
pub enum ParsedSupportsExpressionSigil<T> {
    Regular(T),
    Sigil(ParsedExpression),
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
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticSupportsExpressionSigil<ResourceLocation>> {
        Some(match self {
            Self::Regular(value) => SemanticSupportsExpressionSigil::Regular(value),
            Self::Sigil(expression) => {
                let (expression_span, expression) = expression.perform_semantic_analysis(ctx)?;

                expression.data_type.assert_equals(
                    ctx,
                    expression_span,
                    &SemanticDataType::ResourceLocation,
                )?;

                SemanticSupportsExpressionSigil::Sigil(expression)
            }
        })
    }
}

impl ParsedSupportsExpressionSigil<ParsedEntitySelector> {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticSupportsExpressionSigil<SemanticEntitySelector>> {
        Some(match self {
            Self::Regular(value) => {
                let value = value.perform_semantic_analysis(ctx)?;

                SemanticSupportsExpressionSigil::Regular(value)
            }
            Self::Sigil(expression) => {
                let (expression_span, expression) = expression.perform_semantic_analysis(ctx)?;

                expression.data_type.assert_equals(
                    ctx,
                    expression_span,
                    &SemanticDataType::EntitySelector,
                )?;

                SemanticSupportsExpressionSigil::Sigil(expression)
            }
        })
    }
}

impl ParsedSupportsExpressionSigil<ParsedCoordinates> {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticSupportsExpressionSigil<SemanticCoordinates>> {
        Some(match self {
            Self::Regular(coordinates) => {
                let coordinates = coordinates.perform_semantic_analysis(ctx)?;

                SemanticSupportsExpressionSigil::Regular(coordinates)
            }
            Self::Sigil(expression) => {
                let (expression_span, expression) = expression.perform_semantic_analysis(ctx)?;

                expression.data_type.assert_equals(
                    ctx,
                    expression_span,
                    &SemanticDataType::Coordinates,
                )?;

                SemanticSupportsExpressionSigil::Sigil(expression)
            }
        })
    }
}
