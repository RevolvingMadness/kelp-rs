use kelp_core::high::{
    expression::r#loop::LoopExpression, semantic_analysis::SemanticAnalysisContext,
};

use crate::{
    cst::CSTLoopExpression,
    expression::with_block::r#loop::{
        infinite::lower_infinite_loop_expression, iterator::lower_iterator_loop_expression,
        predicate::lower_predicate_loop_expression,
    },
};

pub mod infinite;
pub mod iterator;
pub mod predicate;

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_loop_expression(
    node: CSTLoopExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<LoopExpression> {
    match node {
        CSTLoopExpression::PredicateLoopExpression(node) => {
            lower_predicate_loop_expression(node, ctx)
        }
        CSTLoopExpression::InfiniteLoopExpression(node) => {
            lower_infinite_loop_expression(node, ctx)
        }
        CSTLoopExpression::IteratorLoopExpression(node) => {
            lower_iterator_loop_expression(node, ctx)
        }
    }
}
