use crate::semantic::environment::r#type::builtin_data_type::{
    BuiltinTypeKind, SemanticBuiltinTypeDeclaration,
};

#[derive(Debug, Clone)]
pub struct ParsedBuiltinTypeDeclaration {
    pub name: String,
    pub generic_count: usize,
    pub kind: BuiltinTypeKind,
}

impl From<SemanticBuiltinTypeDeclaration> for ParsedBuiltinTypeDeclaration {
    fn from(value: SemanticBuiltinTypeDeclaration) -> Self {
        Self {
            name: value.name,
            generic_count: value.generic_count,
            kind: value.kind,
        }
    }
}
