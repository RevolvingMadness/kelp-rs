use kelp_core::{
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator},
    parsed::expression::{ParsedExpression, ParsedExpressionKind},
};

use crate::{
    cst::CSTBinaryExpression,
    extension_traits::{AstNodeExt, LowerableAstNode},
    lower_context::LowerContext,
    syntax::SyntaxKind,
};

impl LowerableAstNode for CSTBinaryExpression {
    type Lowered = ParsedExpression;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let left = self.lhs()?.lower(ctx)?;
        let right = self.rhs()?.lower(ctx)?;
        let operator = self.operator()?;

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
                    ParsedExpressionKind::Arithmetic(Box::new(left), operator, Box::new(right))
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
                    ParsedExpressionKind::Comparison(Box::new(left), operator, Box::new(right))
                }
                SyntaxKind::AmpersandAmpersand | SyntaxKind::PipePipe => {
                    let operator = match operator.kind() {
                        SyntaxKind::AmpersandAmpersand => LogicalOperator::And,
                        SyntaxKind::PipePipe => LogicalOperator::Or,
                        _ => unreachable!(),
                    };
                    ParsedExpressionKind::Logical(Box::new(left), operator, Box::new(right))
                }
                _ => return None,
            })
            .with_span(self.span()),
        )
    }
}
