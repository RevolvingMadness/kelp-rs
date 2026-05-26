use la_arena::Idx;

use crate::{
    parsed::arena::Spanned,
    typed::{
        data::TypedData, data_type::SemanticDataType, environment::value::HighValueId,
        expression::TypedExpressionId, player_score::TypedPlayerScore,
    },
};

pub type ParsedPlaceExpressionId = Idx<Spanned<ParsedPlaceExpression>>;

#[derive(Debug, Clone)]
pub enum ParsedPlaceExpression {
    Value(HighValueId, Vec<SemanticDataType>),
    Score(TypedPlayerScore),
    Data(Box<TypedData>),
    FieldAccess(ParsedPlaceExpressionId, String),
    Index(ParsedPlaceExpressionId, TypedExpressionId),
    Dereference(ParsedPlaceExpressionId),
}
