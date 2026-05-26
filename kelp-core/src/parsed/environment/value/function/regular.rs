use crate::semantic::environment::{
    r#type::HighGenericId, value::function::regular::ResolvedRegularFunctionDeclaration,
};

#[derive(Debug, Clone)]
pub struct UnresolvedRegularFunctionDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
}

impl From<ResolvedRegularFunctionDeclaration> for UnresolvedRegularFunctionDeclaration {
    fn from(value: ResolvedRegularFunctionDeclaration) -> Self {
        Self {
            name: value.name,
            generic_ids: value.generic_ids,
        }
    }
}
