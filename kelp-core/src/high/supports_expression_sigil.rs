use std::fmt::Display;

use crate::{
    high::{
        expression::Expression,
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    },
    low::{
        data_type::DataType,
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

impl<T> SupportsExpressionSigil<T> {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        expected_data_type: &DataType,
    ) -> Option<LowSupportsExpressionSigil<T>> {
        Some(match self {
            Self::Regular(value) => LowSupportsExpressionSigil::Regular(value),
            Self::Sigil(expression) => {
                let (expression_span, expression) = expression.perform_semantic_analysis(ctx)?;

                if !expression.data_type.equals(expected_data_type) {
                    return ctx.add_error(
                        expression_span,
                        SemanticAnalysisError::MismatchedTypes {
                            expected: expected_data_type.clone(),
                            actual: expression.data_type,
                        },
                    );
                }

                LowSupportsExpressionSigil::Sigil(expression)
            }
        })
    }
}
