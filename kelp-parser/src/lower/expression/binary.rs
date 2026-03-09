use kelp_core::{
    expression::{Expression, ExpressionKind},
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator},
    semantic_analysis_context::SemanticAnalysisContext,
};

use crate::{
    cst::CSTBinaryExpression, lower::expression::lower_expression, span::span_of_cst_node,
    syntax::SyntaxKind,
};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_binary_expression(
    node: CSTBinaryExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let span = span_of_cst_node(&node);

    let left = lower_expression(node.lhs()?, ctx)?;
    let right = lower_expression(node.rhs()?, ctx)?;
    let operator = node.operator()?;

    Some(
        (match operator.kind() {
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
                ExpressionKind::Arithmetic(Box::new(left), operator, Box::new(right))
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
                ExpressionKind::Comparison(Box::new(left), operator, Box::new(right))
            }
            SyntaxKind::AmpersandAmpersand | SyntaxKind::PipePipe => {
                let operator = match operator.kind() {
                    SyntaxKind::AmpersandAmpersand => LogicalOperator::And,
                    SyntaxKind::PipePipe => LogicalOperator::Or,
                    _ => unreachable!(),
                };
                ExpressionKind::Logical(Box::new(left), operator, Box::new(right))
            }
            _ => return None,
        })
        .with_span(span),
    )
}
