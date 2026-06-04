use crate::semantic::environment::r#type::HighGenericId;
use crate::semantic::environment::r#type::r#struct::regular::SemanticRegularStructDeclaration;
use crate::span::Span;

#[derive(Debug, Clone)]
pub struct ParsedRegularStructDeclaration {
    pub name_span: Span,
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
}

impl From<SemanticRegularStructDeclaration> for ParsedRegularStructDeclaration {
    fn from(value: SemanticRegularStructDeclaration) -> Self {
        Self {
            name_span: value.name_span,
            name: value.name,
            generic_ids: value.generic_ids,
        }
    }
}

impl ParsedRegularStructDeclaration {
    #[inline]
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[inline]
    #[must_use]
    pub fn generic_ids(&self) -> &[HighGenericId] {
        &self.generic_ids
    }

    #[inline]
    #[must_use]
    pub const fn generic_count(&self) -> usize {
        self.generic_ids.len()
    }
}
