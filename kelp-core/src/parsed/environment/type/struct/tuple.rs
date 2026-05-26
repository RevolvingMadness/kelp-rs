use crate::semantic::environment::r#type::r#struct::tuple::SemanticTupleStructDeclaration;
use crate::semantic::environment::r#type::HighGenericId;

#[derive(Debug, Clone)]
pub struct ParsedTupleStructDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
}

impl From<SemanticTupleStructDeclaration> for ParsedTupleStructDeclaration {
    fn from(value: SemanticTupleStructDeclaration) -> Self {
        Self {
            name: value.name,
            generic_ids: value.generic_ids,
        }
    }
}
