use kelp_core::{
    expression::{Expression, ExpressionKind},
    high::snbt_string::HighSNBTString,
};
use minecraft_command_types::snbt::SNBTString;

use crate::{
    cst::CSTFieldAccessExpression,
    lower::expression::lower_expression,
    span::{span_of_cst_node, text_range_to_span},
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_field_access_expression(node: CSTFieldAccessExpression) -> Option<Expression> {
    let expression = lower_expression(node.expression()?)?;
    let field_token = node.field_name_token()?;

    let field_name_span = text_range_to_span(field_token.text_range());
    let field = field_token.text().to_owned();

    let span = span_of_cst_node(&node);

    Some(
        ExpressionKind::FieldAccess(
            Box::new(expression),
            HighSNBTString {
                span: field_name_span,
                snbt_string: SNBTString(false, field),
            },
        )
        .with_span(span),
    )
}
