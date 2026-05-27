use kelp_core::{
    operator::UnaryOperator,
    parsed::expression::{ParsedExpression, ParsedExpressionKind},
};

use crate::{
    cst::CSTUnaryExpression, expression::lower_expression, extension_traits::AstNodeExt as _,
    lower_context::LowerContext, syntax::SyntaxKind,
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

    let operand = lower_expression(node.expression()?, ctx)?;

    Some(ParsedExpressionKind::Unary(operator, Box::new(operand)).with_span(span))
}
