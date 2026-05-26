use kelp_core::{
    parsed::expression::{ParsedExpression, ParsedExpressionId},
    operator::ArithmeticOperator,
};

use crate::{
    cst::CSTAssignmentExpression,
    expression::lower_expression,
    lower_context::LowerContext,
    span::{span_of_cst_node, text_range_to_span},
    syntax::SyntaxKind,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_assignment_expression(
    node: CSTAssignmentExpression,
    ctx: &mut LowerContext,
) -> Option<ParsedExpressionId> {
    let span = span_of_cst_node(&node);

    let target = lower_expression(node.target()?, ctx)?;
    let value = lower_expression(node.value()?, ctx)?;
    let operator = node.operator()?;

    let operator_span = text_range_to_span(operator.text_range());

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

    let expression = if let Some(operator) = operator {
        ParsedExpression::AugmentedAssignment(target, operator_span, operator, value)
    } else {
        ParsedExpression::Assignment(target, value)
    };

    Some(ctx.allocator.allocate_expression(span, expression))
}
