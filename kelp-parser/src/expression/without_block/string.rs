use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionId};

use crate::{cst::CSTStringExpression, lower_context::LowerContext, span::span_of_cst_node};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_string_expression(
    node: CSTStringExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpressionId> {
    let span = span_of_cst_node(&node);

    let text_token = node.string_literal_token()?;
    let string = text_token.text().trim_matches('"');

    Some(
        ctx.arena
            .allocate_expression(span, ParsedExpression::String(string.to_owned())),
    )
}
