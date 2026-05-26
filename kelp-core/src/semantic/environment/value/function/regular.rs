use std::collections::HashSet;

use crate::{
    parsed::semantic_analysis::RegularFunctionModifiers,
    span::Span,
    semantic::{
        expression::unresolved::SemanticExpression,
        pattern::SemanticPattern,
    },
};
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::{r#type::HighGenericId, value::function::HighFunctionId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighRegularFunctionId(pub u32);

impl From<HighRegularFunctionId> for HighFunctionId {
    fn from(value: HighRegularFunctionId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub struct SemanticRegularFunctionDeclaration {
    pub name: String,
    pub modifiers: RegularFunctionModifiers,
    pub generic_ids: Vec<HighGenericId>,
    pub parameters: Vec<(Option<SemanticPattern>, SemanticDataType)>,
    pub return_type: SemanticDataType,
    pub body: Option<Box<SemanticExpression>>,
    pub calls: HashSet<(Span, HighFunctionId)>,
}
