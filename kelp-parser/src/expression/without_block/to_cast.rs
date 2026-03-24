use kelp_core::{
    high::{
        expression::{Expression, ExpressionKind},
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    },
    runtime_storage_type::RuntimeStorageType,
};

use crate::{
    cst::CSTToCastExpression,
    expression::lower_expression,
    span::{span_of_cst_node, text_range_to_span},
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_to_cast_expression(
    node: CSTToCastExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let expression = lower_expression(node.expression()?, ctx)?;
    let runtime_storage_type_token = node.runtime_storage_type_token()?;
    let runtime_storage_type = match runtime_storage_type_token.text() {
        "data" => RuntimeStorageType::Data,
        "score" => RuntimeStorageType::Score,
        _ => {
            ctx.add_error::<()>(
                text_range_to_span(runtime_storage_type_token.text_range()),
                SemanticAnalysisError::UnknownRuntimeStorageType,
            );

            return Some(ExpressionKind::Invalid.with_span(span));
        }
    };

    Some(ExpressionKind::ToCast(Box::new(expression), runtime_storage_type).with_span(span))
}
