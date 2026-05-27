use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTMethodCallExpression,
    expression::without_block::call::lower_call_arguments,
    extension_traits::{AstNodeExt, LowerableAstNode},
    lower_context::LowerContext,
    path::generic::lower_generic_path_segment,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_method_call_expression(
    node: CSTMethodCallExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let receiver = node.expression()?.lower(ctx)?;

    let callee = lower_generic_path_segment(node.generic_path_segment()?)?;

    let arguments = node
        .call_arguments()
        .map(|arguments| lower_call_arguments(arguments, ctx));

    let span = node.span();

    Some(
        ParsedExpressionKind::MethodCall {
            receiver: Box::new(receiver),
            callee,
            arguments: arguments.unwrap_or_default(),
        }
        .with_span(span),
    )
}
