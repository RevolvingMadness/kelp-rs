use crate::semantic::environment::{
    r#type::HighGenericId,
    value::function::builtin::{BuiltinFunctionKind, SemanticBuiltinFunctionDeclaration},
};

#[derive(Debug, Clone)]
pub struct ParsedBuiltinFunctionDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
    pub kind: BuiltinFunctionKind,
}

impl From<SemanticBuiltinFunctionDeclaration> for ParsedBuiltinFunctionDeclaration {
    fn from(value: SemanticBuiltinFunctionDeclaration) -> Self {
        Self {
            name: value.name,
            generic_ids: value.generic_ids,
            kind: value.kind,
        }
    }
}
