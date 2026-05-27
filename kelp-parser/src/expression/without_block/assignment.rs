use kelp_core::{
    operator::ArithmeticOperator,
    parsed::expression::{ParsedExpression, ParsedExpressionKind},
};

use crate::{
    cst::CSTAssignmentExpression,
    extension_traits::{AstNodeExt, LowerableAstNode, SyntaxTokenExt},
    lower_context::LowerContext,
    syntax::SyntaxKind,
};

impl LowerableAstNode for CSTAssignmentExpression {
    type Lowered = ParsedExpression;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let target = self.target()?.lower(ctx)?;
        let value = self.value()?.lower(ctx)?;
        let operator = self.operator()?;

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
            .with_span(self.span()),
        )
    }
}
