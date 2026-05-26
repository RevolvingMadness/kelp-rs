use crate::semantic::environment::value::variable::ResolvedVariableDeclaration;

#[derive(Debug, Clone)]
pub struct UnresolvedVariableDeclaration {
    pub name: String,
}

impl From<ResolvedVariableDeclaration> for UnresolvedVariableDeclaration {
    fn from(value: ResolvedVariableDeclaration) -> Self {
        Self { name: value.name }
    }
}
