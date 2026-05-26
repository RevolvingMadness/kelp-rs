use kelp_core::{
    parsed::expression::{ParsedExpression, ParsedExpressionId},
    operator::UnaryOperator,
};

use crate::{
    cst::CSTUnaryExpression, expression::lower_expression, lower_context::LowerContext,
    span::span_of_cst_node, syntax::SyntaxKind,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_unary_expression(
    node: CSTUnaryExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpressionId> {
    let span = span_of_cst_node(&node);

    let operator = match node.operator()?.kind() {
        SyntaxKind::ExclamationMark => UnaryOperator::Invert,
        SyntaxKind::Minus => UnaryOperator::Negate,
        SyntaxKind::Star => UnaryOperator::Dereference,
        SyntaxKind::Ampersand => UnaryOperator::Reference,
        _ => return None,
    };

    let operand = lower_expression(node.expression()?, ctx)?;

    Some(
        ctx.allocator
            .allocate_expression(span, ParsedExpression::Unary(operator, operand)),
    )
}
