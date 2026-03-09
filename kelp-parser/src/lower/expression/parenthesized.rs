use kelp_core::{expression::Expression, semantic_analysis_context::SemanticAnalysisContext};

use crate::{cst::CSTParenthesizedExpression, lower::expression::lower_expression};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_parenthesized_expression(
    node: CSTParenthesizedExpression,
    ctx: &mut SemanticAnalysisContext,
) -> Option<Expression> {
    let expression = lower_expression(node.expression()?, ctx)?;

    Some(expression)
}
