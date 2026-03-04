use kelp_core::expression::Expression;

use crate::{cst::CSTParenthesizedExpression, lower::expression::lower_expression};

#[must_use]
#[allow(clippy::needless_pass_by_value)]
pub fn lower_parenthesized_expression(node: CSTParenthesizedExpression) -> Option<Expression> {
    let expression = lower_expression(node.expression()?)?;

    Some(expression)
}
