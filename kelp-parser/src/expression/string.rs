use kelp_core::{
    high::expression::{Expression, ExpressionKind},
    high::snbt_string::HighSNBTString,
    semantic_analysis_context::SemanticAnalysisContext,
};
use minecraft_command_types::snbt::SNBTString;

use crate::{cst::CSTStringExpression, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_string_expression(
    node: CSTStringExpression,
    _ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let text_token = node.string_literal_token()?;
    let text = text_token.text().trim_matches('"');

    Some(
        ExpressionKind::String(HighSNBTString {
            span,
            snbt_string: SNBTString(false, text.to_string()),
        })
        .with_span(span),
    )
}
