use std::collections::HashSet;

use la_arena::Idx;

use crate::{
    parsed::semantic_analysis::RegularFunctionModifiers,
    span::Span,
    typed::{
        data_type::unresolved::SemanticDataType,
        environment::{
            r#type::HighGenericId,
            value::{HighValueId, function::HighFunctionId},
        },
        expression::typed::TypedExpressionId,
        pattern::TypedPattern,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighRegularFunctionId(pub u32);

impl From<HighRegularFunctionId> for HighFunctionId {
    fn from(value: HighRegularFunctionId) -> Self {
        Self(value.0)
    }
}

impl From<HighRegularFunctionId> for HighValueId {
    fn from(value: HighRegularFunctionId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub struct SemanticRegularFunctionDeclaration {
    pub name: String,
    pub modifiers: RegularFunctionModifiers,
    pub generic_ids: Vec<HighGenericId>,
    pub parameters: Vec<(Option<Idx<TypedPattern>>, SemanticDataType)>,
    pub return_type: SemanticDataType,
    pub body: Option<TypedExpressionId>,
    pub calls: HashSet<(Span, HighFunctionId)>,
}
