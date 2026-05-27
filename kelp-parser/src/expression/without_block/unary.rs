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

impl LowerableAstNode for CSTUnaryExpression {
    type Lowered = ParsedExpression;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let operator = match self.operator()?.kind() {
            SyntaxKind::ExclamationMark => UnaryOperator::Invert,
            SyntaxKind::Minus => UnaryOperator::Negate,
            SyntaxKind::Star => UnaryOperator::Dereference,
            SyntaxKind::Ampersand => UnaryOperator::Reference,
            _ => return None,
        };

        let operand = self.expression()?.lower(ctx)?;

        Some(ParsedExpressionKind::Unary(operator, Box::new(operand)).with_span(self.span()))
    }
}
