use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTIndexExpression,
    extension_traits::{AstNodeExt, LowerableAstNode},
    lower_context::LowerContext,
};

impl LowerableAstNode for CSTIndexExpression {
    type Lowered = ParsedExpression;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let mut expressions = self.expressions();

        let target = expressions.next()?.lower(ctx)?;
        let index = expressions.next()?.lower(ctx)?;

        Some(ParsedExpressionKind::Index(Box::new(target), Box::new(index)).with_span(self.span()))
    }
}
