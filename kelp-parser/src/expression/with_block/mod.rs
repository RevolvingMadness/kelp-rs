use kelp_core::parsed::expression::{ParsedExpression, ParsedExpressionKind};

use crate::{
    cst::CSTExpressionWithBlock,
    extension_traits::{AstNodeExt, LowerableAstNode},
    lower_context::LowerContext,
};

pub mod block;
pub mod r#if;
pub mod r#loop;

impl LowerableAstNode for CSTExpressionWithBlock {
    type Lowered = ParsedExpression;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        match self {
            Self::BlockExpression(node) => {
                let span = node.span();

                let expression = node.lower(ctx)?;

                Some(ParsedExpressionKind::Block(expression).with_span(span))
            }
            Self::IfExpression(node) => node.lower(ctx),
            Self::LoopExpression(node) => node.lower(ctx),
        }
    }
}
