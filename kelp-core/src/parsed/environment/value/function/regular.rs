use crate::{
    semantic::environment::{
        r#type::generic::HighGenericId,
        value::function::regular::SemanticRegularFunctionDeclaration,
    },
    span::Span,
};

#[derive(Debug, Clone)]
pub struct ParsedRegularFunctionDeclaration {
    pub name_span: Span,
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
}

impl From<SemanticRegularFunctionDeclaration> for ParsedRegularFunctionDeclaration {
    fn from(value: SemanticRegularFunctionDeclaration) -> Self {
        Self {
            name_span: value.name_span,
            name: value.name,
            generic_ids: value.generic_ids,
        }
    }
}
