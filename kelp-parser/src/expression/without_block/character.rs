use kelp_core::high::{
    expression::{Expression, ExpressionKind},
    semantic_analysis::SemanticAnalysisContext,
    snbt_string::SNBTString,
};
use minecraft_command_types::snbt::SNBTString as LowSNBTString;

use crate::{cst::CSTCharacterExpression, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_character_expression(
    node: CSTCharacterExpression,
    _ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let text_token = node.character_literal_token()?;
    let text = text_token.text().trim_matches('\'');

    Some(
        ExpressionKind::String(SNBTString {
            span,
            snbt_string: LowSNBTString(false, text.to_string()),
        })
        .with_span(span),
    )
}
