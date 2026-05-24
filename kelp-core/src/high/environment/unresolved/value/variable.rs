use crate::high::environment::resolved::value::variable::ResolvedVariableDeclaration;

#[derive(Debug, Clone)]
pub struct UnresolvedVariableDeclaration {
    pub name: String,
}

impl From<ResolvedVariableDeclaration> for UnresolvedVariableDeclaration {
    fn from(value: ResolvedVariableDeclaration) -> Self {
        Self { name: value.name }
    }
}
