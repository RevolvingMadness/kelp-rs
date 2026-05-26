use la_arena::Idx;

use crate::parsed::{arena::Spanned, expression::place::ParsedPlaceExpressionId};

pub type ParsedAssigneeExpressionId = Idx<Spanned<ParsedAssigneeExpression>>;

#[derive(Debug, Clone)]
pub enum ParsedAssigneeExpression {
    Place(ParsedPlaceExpressionId),

    Tuple(Vec<ParsedAssigneeExpressionId>),
    Underscore,
}
