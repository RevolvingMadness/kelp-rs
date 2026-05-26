use crate::semantic::environment::r#type::r#struct::regular::ResolvedRegularStructDeclaration;
use crate::semantic::environment::r#type::HighGenericId;

#[derive(Debug, Clone)]
pub struct ParsedRegularStructDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
}

impl From<ResolvedRegularStructDeclaration> for ParsedRegularStructDeclaration {
    fn from(value: ResolvedRegularStructDeclaration) -> Self {
        Self {
            name: value.name,
            generic_ids: value.generic_ids,
        }
    }
}
