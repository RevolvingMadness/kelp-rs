use kelp_core::{
    high::expression::{Expression, ExpressionKind},
    high::snbt_string::SNBTString,
    semantic_analysis_context::SemanticAnalysisContext,
};
use minecraft_command_types::snbt::SNBTString as LowSNBTString;

use crate::{
    cst::CSTFieldAccessExpression,
    expression::lower_expression,
    span::{span_of_cst_node, text_range_to_span},
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_field_access_expression(
    node: CSTFieldAccessExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let expression = lower_expression(node.expression()?, ctx)?;
    let field_token = node.field_name_token()?;

    let field_name_span = text_range_to_span(field_token.text_range());
    let field = field_token.text().to_owned();

    let span = span_of_cst_node(&node);

    Some(
        ExpressionKind::FieldAccess(
            Box::new(expression),
            SNBTString {
                span: field_name_span,
                snbt_string: LowSNBTString(false, field),
            },
        )
        .with_span(span),
    )
}
