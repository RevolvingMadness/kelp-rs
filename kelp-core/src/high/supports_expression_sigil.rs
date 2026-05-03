use std::fmt::Display;

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    high::{
        entity_selector::EntitySelector, expression::Expression,
        semantic_analysis::SemanticAnalysisContext,
    },
    low::{
        data_type::DataType, entity_selector::EntitySelector as LowEntitySelector,
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
