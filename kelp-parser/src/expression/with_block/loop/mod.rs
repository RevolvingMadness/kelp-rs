use kelp_core::parsed::expression::ParsedExpression;

use crate::{
    cst::CSTLoopExpression, extension_traits::LowerableAstNode, lower_context::LowerContext,
};

pub mod infinite;
pub mod iterator;
pub mod predicate;

impl LowerableAstNode for CSTLoopExpression {
    type Lowered = ParsedExpression;

    fn lower(self, ctx: &mut LowerContext) -> Option<Self::Lowered> {
        match self {
            Self::PredicateLoopExpression(node) => node.lower(ctx),
            Self::InfiniteLoopExpression(node) => node.lower(ctx),
            Self::IteratorLoopExpression(node) => node.lower(ctx),
        }
    }
}
