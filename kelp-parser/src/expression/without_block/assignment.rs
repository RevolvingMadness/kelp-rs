use kelp_core::{
    operator::ArithmeticOperator,
    parsed::expression::{ParsedExpression, ParsedExpressionKind},
};

use crate::{
    cst::CSTAssignmentExpression,
    expression::lower_expression,
    extension_traits::{AstNodeExt as _, SyntaxTokenExt},
    lower_context::LowerContext,
    syntax::SyntaxKind,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_assignment_expression(
    node: CSTAssignmentExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpression> {
    let span = node.span();

    let target = lower_expression(node.target()?, ctx)?;
    let value = lower_expression(node.value()?, ctx)?;
    let operator = node.operator()?;

    let operator_span = operator.span();

    let operator = match operator.kind() {
        SyntaxKind::Equal => None,
        SyntaxKind::PlusEqual => Some(ArithmeticOperator::Add),
        SyntaxKind::MinusEqual => Some(ArithmeticOperator::Subtract),
        SyntaxKind::StarEqual => Some(ArithmeticOperator::Multiply),
        SyntaxKind::ForwardSlashEqual => Some(ArithmeticOperator::FloorDivide),
        SyntaxKind::PercentEqual => Some(ArithmeticOperator::Modulo),
        SyntaxKind::AmpersandEqual => Some(ArithmeticOperator::And),
        SyntaxKind::PipeEqual => Some(ArithmeticOperator::Or),
        SyntaxKind::LeftArrowLeftArrowEqual => Some(ArithmeticOperator::LeftShift),
        SyntaxKind::RightArrowRightArrowEqual => Some(ArithmeticOperator::RightShift),
        _ => return None,
    };

    Some(
        (if let Some(operator) = operator {
            ParsedExpressionKind::AugmentedAssignment(
                Box::new(target),
                operator_span,
                operator,
                Box::new(value),
            )
        } else {
            ParsedExpressionKind::Assignment(Box::new(target), Box::new(value))
        })
        .with_span(span),
    )
}
