use kelp_core::{
    high::{expression::{Expression, ExpressionKind}, semantic_analysis::SemanticAnalysisContext},
    operator::ArithmeticOperator,
};

use crate::{
    cst::CSTAssignmentExpression, expression::lower_expression, span::span_of_cst_node,
    syntax::SyntaxKind,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_assignment_expression(
    node: CSTAssignmentExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let target = lower_expression(node.target()?, ctx)?;
    let value = lower_expression(node.value()?, ctx)?;
    let operator = node.operator()?;

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
        SyntaxKind::RightArrowLeftArrow => Some(ArithmeticOperator::Swap),
        _ => return None,
    };

    Some(
        (if let Some(operator) = operator {
            ExpressionKind::AugmentedAssignment(Box::new(target), operator, Box::new(value))
        } else {
            ExpressionKind::Assignment(Box::new(target), Box::new(value))
        })
        .with_span(span),
    )
}
