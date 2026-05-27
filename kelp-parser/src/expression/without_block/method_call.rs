use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTMethodCallExpression,
    extension_traits::{AstNodeExt, LowerableAstNode},
    lower_context::LowerContext,
};

impl LowerableAstNode for CSTMethodCallExpression {
    type Lowered = ParsedExpression;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let receiver = self.expression()?.lower(ctx)?;

        let callee = self.generic_path_segment()?.lower(ctx)?;

        let arguments = self.call_arguments().map(|arguments| arguments.lower(ctx));

        let arguments = match arguments {
            Some(Some(arguments)) => Some(arguments),
            Some(None) => return None,
            None => None,
        };

        Some(
            ParsedExpressionKind::MethodCall {
                receiver: Box::new(receiver),
                callee,
                arguments: arguments.unwrap_or_default(),
            }
            .with_span(self.span()),
        )
    }
}
