use crate::high::environment::resolved::r#type::builtin_data_type::{
    BuiltinTypeKind, ResolvedBuiltinTypeDeclaration,
};

#[derive(Debug, Clone)]
pub struct UnresolvedBuiltinTypeDeclaration {
    pub name: String,
    pub generic_count: usize,
    pub kind: BuiltinTypeKind,
}

impl From<ResolvedBuiltinTypeDeclaration> for UnresolvedBuiltinTypeDeclaration {
    fn from(value: ResolvedBuiltinTypeDeclaration) -> Self {
        Self {
            name: value.name,
            generic_count: value.generic_count,
            kind: value.kind,
        }
    }
}
