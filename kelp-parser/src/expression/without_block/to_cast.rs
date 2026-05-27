use kelp_core::{
    parsed::expression::{ParsedExpression, ParsedExpressionKind},
    runtime_storage::RuntimeStorageType,
};

use crate::{
    cst::CSTToCastExpression,
    expression::lower_expression,
    extension_traits::{AstNodeExt, SyntaxTokenExt},
    lower_context::{LowerContext, LowerError},
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_to_cast_expression(
    node: CSTToCastExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = node.span();

    let expression = lower_expression(node.expression()?, ctx)?;
    let runtime_storage_type_token = node.runtime_storage_type_token()?;
    let runtime_storage_type = match runtime_storage_type_token.text() {
        "data" => RuntimeStorageType::Data,
        "score" => RuntimeStorageType::Score,
        _ => {
            ctx.add_error_unit(
                runtime_storage_type_token.span(),
                LowerError::UnknownRuntimeStorageType,
            );

            return Some(ParsedExpressionKind::Invalid.with_span(span));
        }
    };

    Some(ParsedExpressionKind::ToCast(Box::new(expression), runtime_storage_type).with_span(span))
}
