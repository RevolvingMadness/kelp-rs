use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::HighGenericId;
use crate::span::Span;

#[derive(Debug, Clone)]
pub struct SemanticTypeAliasDeclaration {
    pub name_span: Span,
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
    pub alias: SemanticDataType,
}

impl SemanticTypeAliasDeclaration {
    #[inline]
    #[must_use]
    pub const fn generic_count(&self) -> usize {
        self.generic_ids.len()
    }
}
