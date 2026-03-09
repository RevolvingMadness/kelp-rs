use kelp_core::{
    expression::{Expression, ExpressionKind},
    semantic_analysis_context::SemanticAnalysisContext,
};

use crate::{
    cst::CSTAsCastExpression,
    lower::{data_type::lower_data_type, expression::lower_expression},
    span::span_of_cst_node,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_as_cast_expression(
    node: CSTAsCastExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let expression = lower_expression(node.expression()?, ctx)?;
    let data_type = lower_data_type(node.data_type()?)?;

    Some(ExpressionKind::AsCast(Box::new(expression), data_type).with_span(span))
}
