use kelp_core::{
    operator::UnaryOperator,
    parsed::expression::{ParsedExpression, ParsedExpressionKind},
};

use crate::{
    cst::CSTUnaryExpression,
    extension_traits::{AstNodeExt, LowerableAstNode},
    lower_context::LowerContext,
    syntax::SyntaxKind,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_unary_expression(
    node: CSTUnaryExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = node.span();

    let operator = match node.operator()?.kind() {
        SyntaxKind::ExclamationMark => UnaryOperator::Invert,
        SyntaxKind::Minus => UnaryOperator::Negate,
        SyntaxKind::Star => UnaryOperator::Dereference,
        SyntaxKind::Ampersand => UnaryOperator::Reference,
        _ => return None,
    };

    let operand = node.expression()?.lower(ctx)?;

    Some(ParsedExpressionKind::Unary(operator, Box::new(operand)).with_span(span))
}
