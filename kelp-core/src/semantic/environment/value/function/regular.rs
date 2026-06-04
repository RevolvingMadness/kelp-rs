use std::collections::HashSet;

use crate::make_id;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::value::HighValueId;
use crate::semantic::environment::{r#type::HighGenericId, value::function::HighFunctionId};
use crate::{
    parsed::semantic_analysis::RegularFunctionModifiers,
    semantic::{expression::SemanticExpression, pattern::SemanticPattern},
    span::Span,
};

make_id!(HighRegularFunctionId);

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
    pub declared_generic_count: usize,
    pub is_method: bool,
    pub parameters: Vec<(Option<SemanticPattern>, SemanticDataType)>,
    pub return_type: SemanticDataType,
    pub body: Option<Box<SemanticExpression>>,
    pub calls: HashSet<(Span, HighFunctionId)>,
}

impl SemanticRegularFunctionDeclaration {
    #[inline]
    #[must_use]
    pub const fn generic_count(&self) -> usize {
        self.declared_generic_count
    }
}
