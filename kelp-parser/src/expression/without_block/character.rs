use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionId};

use crate::{cst::CSTCharacterExpression, lower_context::LowerContext, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_character_expression(
    node: CSTCharacterExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpressionId> {
    let span = span_of_cst_node(&node);

    let text_token = node.character_literal_token()?;
    let text = text_token.text().trim_matches('\'');

    Some(
        ctx.allocator
            .allocate_expression(span, ParsedExpression::String(text.to_owned())),
    )
}
