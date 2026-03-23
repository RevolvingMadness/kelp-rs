use crate::high::environment::r#type::{HighTypeDeclaration, HighTypeId};

pub mod r#type;

#[derive(Debug, Clone, Default)]
pub struct HighEnvironment {
    pub types: Vec<HighTypeDeclaration>,
}

impl HighEnvironment {
    #[must_use]
    pub fn declare_type(&mut self, declaration: HighTypeDeclaration) -> HighTypeId {
        let id = HighTypeId(self.types.len());

        self.types.push(declaration);

        id
    }
}
