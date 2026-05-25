use kelp_core::{
    high::expression::{Expression, ExpressionId},
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator},
};

use crate::{
    cst::CSTBinaryExpression, expression::lower_expression, lower_context::LowerContext,
    span::span_of_cst_node, syntax::SyntaxKind,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_binary_expression(
    node: CSTBinaryExpression,
    ctx: &mut LowerContext,
) -> Option<ExpressionId> {
    let span = span_of_cst_node(&node);

    let left = lower_expression(node.lhs()?, ctx)?;
    let right = lower_expression(node.rhs()?, ctx)?;
    let operator = node.operator()?;

    let expression = match operator.kind() {
        SyntaxKind::Plus
        | SyntaxKind::Minus
        | SyntaxKind::Star
        | SyntaxKind::ForwardSlash
        | SyntaxKind::Percent
        | SyntaxKind::Ampersand
        | SyntaxKind::Pipe
        | SyntaxKind::LeftArrowLeftArrow
        | SyntaxKind::RightArrowRightArrow => {
            let operator = match operator.kind() {
                SyntaxKind::Plus => ArithmeticOperator::Add,
                SyntaxKind::Minus => ArithmeticOperator::Subtract,
                SyntaxKind::Star => ArithmeticOperator::Multiply,
                SyntaxKind::ForwardSlash => ArithmeticOperator::FloorDivide,
                SyntaxKind::Percent => ArithmeticOperator::Modulo,
                SyntaxKind::Ampersand => ArithmeticOperator::And,
                SyntaxKind::Pipe => ArithmeticOperator::Or,
                SyntaxKind::LeftArrowLeftArrow => ArithmeticOperator::LeftShift,
                SyntaxKind::RightArrowRightArrow => ArithmeticOperator::RightShift,
                _ => unreachable!(),
            };
            Expression::Arithmetic(left, operator, right)
        }
        SyntaxKind::EqualEqual
        | SyntaxKind::ExclamationMarkEqual
        | SyntaxKind::RightArrow
        | SyntaxKind::RightArrowEqual
        | SyntaxKind::LeftArrow
        | SyntaxKind::LeftArrowEqual => {
            let operator = match operator.kind() {
                SyntaxKind::EqualEqual => ComparisonOperator::EqualTo,
                SyntaxKind::ExclamationMarkEqual => ComparisonOperator::NotEqualTo,
                SyntaxKind::RightArrow => ComparisonOperator::GreaterThan,
                SyntaxKind::RightArrowEqual => ComparisonOperator::GreaterThanOrEqualTo,
                SyntaxKind::LeftArrow => ComparisonOperator::LessThan,
                SyntaxKind::LeftArrowEqual => ComparisonOperator::LessThanOrEqualTo,
                _ => unreachable!(),
            };
            Expression::Comparison(left, operator, right)
        }
        SyntaxKind::AmpersandAmpersand | SyntaxKind::PipePipe => {
            let operator = match operator.kind() {
                SyntaxKind::AmpersandAmpersand => LogicalOperator::And,
                SyntaxKind::PipePipe => LogicalOperator::Or,
                _ => unreachable!(),
            };
            Expression::Logical(left, operator, right)
        }
        _ => return None,
    };

    Some(ctx.allocator.allocate_expression(span, expression))
}
