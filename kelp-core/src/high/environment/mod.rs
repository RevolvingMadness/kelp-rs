use crate::high::environment::{
    r#type::{HighTypeDeclaration, HighTypeId},
    value::{HighValueDeclaration, HighValueId},
};

pub mod r#type;
pub mod value;

#[derive(Debug, Clone, Default)]
pub struct HighEnvironment {
    pub types: Vec<HighTypeDeclaration>,
    pub values: Vec<HighValueDeclaration>,
}

impl HighEnvironment {
    #[must_use]
    pub fn declare_type(&mut self, declaration: HighTypeDeclaration) -> HighTypeId {
        let id = HighTypeId(self.types.len());

        self.types.push(declaration);

        id
    }

    #[must_use]
    pub fn declare_value(&mut self, declaration: HighValueDeclaration) -> HighValueId {
        let id = HighValueId(self.values.len());

        self.values.push(declaration);

        id
    }
}
