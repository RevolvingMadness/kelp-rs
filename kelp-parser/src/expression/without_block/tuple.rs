use kelp_core::{
    parsed::expression::{ParsedExpression, ParsedExpressionKind},
    trait_ext::CollectOptionAllIterExt,
};

use crate::{
    cst::CSTTupleExpression,
    extension_traits::{AstNodeExt, LowerableAstNode},
    lower_context::LowerContext,
};

impl LowerableAstNode for CSTTupleExpression {
    type Lowered = ParsedExpression;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let expressions = self
            .expressions()
            .map(|expression| expression.lower(ctx))
            .collect_option_all()?;

        Some(ParsedExpressionKind::Tuple(expressions).with_span(self.span()))
    }
}
