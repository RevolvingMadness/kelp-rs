use crate::high::environment::resolved::{
    r#type::HighGenericId,
    value::function::builtin::{BuiltinFunctionKind, ResolvedBuiltinFunctionDeclaration},
};

#[derive(Debug, Clone)]
pub struct UnresolvedBuiltinFunctionDeclaration {
    pub name: String,
    pub generic_ids: Vec<HighGenericId>,
    pub kind: BuiltinFunctionKind,
}

impl From<ResolvedBuiltinFunctionDeclaration> for UnresolvedBuiltinFunctionDeclaration {
    fn from(value: ResolvedBuiltinFunctionDeclaration) -> Self {
        Self {
            name: value.name,
            generic_ids: value.generic_ids,
            kind: value.kind,
        }
    }
}
