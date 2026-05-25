use kelp_core::{high::expression::Expression, runtime_storage::RuntimeStorageType};
use la_arena::Idx;

use crate::{
    cst::CSTToCastExpression,
    expression::lower_expression,
    lower_context::{LowerContext, LowerError},
    span::{span_of_cst_node, text_range_to_span},
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_to_cast_expression(
    node: CSTToCastExpression,
    ctx: &mut LowerContext,
) -> Option<Idx<Expression>> {
    let span = span_of_cst_node(&node);

    let expression = lower_expression(node.expression()?, ctx)?;
    let runtime_storage_type_token = node.runtime_storage_type_token()?;
    let runtime_storage_type = match runtime_storage_type_token.text() {
        "data" => RuntimeStorageType::Data,
        "score" => RuntimeStorageType::Score,
        _ => {
            ctx.add_error_unit(
                text_range_to_span(runtime_storage_type_token.text_range()),
                LowerError::UnknownRuntimeStorageType,
            );

            return Some(ctx.allocator.allocate_expression(span, Expression::Invalid));
        }
    };

    Some(
        ctx.allocator
            .allocate_expression(span, Expression::ToCast(expression, runtime_storage_type)),
    )
}
