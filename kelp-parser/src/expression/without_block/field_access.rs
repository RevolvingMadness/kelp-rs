use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTFieldAccessExpression,
    extension_traits::{AstNodeExt, LowerableAstNode, SyntaxTokenExt},
    lower_context::LowerContext,
};

impl LowerableAstNode for CSTFieldAccessExpression {
    type Lowered = ParsedExpression;

    fn lower(&self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        let expression = self.expression()?.lower(ctx)?;

        let field_token = self.field_name_token()?;
        let field_span = field_token.span();
        let field = field_token.text().to_owned();

        Some(
            ParsedExpressionKind::FieldAccess(Box::new(expression), field_span, field)
                .with_span(self.span()),
        )
    }
}
