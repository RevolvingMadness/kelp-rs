use kelp_core::{
    expression::{Expression, ExpressionKind},
    runtime_storage_type::RuntimeStorageType,
};

use crate::{
    cst::CSTToCastExpression, lower::expression::lower_expression, span::span_of_cst_node,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_to_cast_expression(node: CSTToCastExpression) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let expression = lower_expression(node.expression()?)?;
    let runtime_storage_type_token = node.runtime_storage_type_token()?;
    let runtime_storage_type = match runtime_storage_type_token.text() {
        "data" => RuntimeStorageType::Data,
        _ => RuntimeStorageType::Score,
    };

    Some(ExpressionKind::ToCast(Box::new(expression), runtime_storage_type).with_span(span))
}
