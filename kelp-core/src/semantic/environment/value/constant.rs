use crate::{
    make_id,
    semantic::{
        data_type::SemanticDataType, environment::value::HighValueId,
        expression::SemanticExpression,
    },
    span::Span,
};

make_id!(HighConstantId);

impl From<HighConstantId> for HighValueId {
    fn from(value: HighConstantId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub struct SemanticConstantDeclaration {
    pub name_span: Span,
    pub name: String,
    pub data_type: SemanticDataType,
    pub value: Option<SemanticExpression>,
}
