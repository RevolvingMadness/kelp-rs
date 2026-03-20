use crate::middle::environment::value::variable::VariableDeclaration;

pub mod variable;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueId(pub usize);

#[derive(Debug, Clone)]
pub enum ValueDeclaration {
    Variable(VariableDeclaration),
}

impl ValueDeclaration {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Variable(declaration) => &declaration.name,
        }
    }
}
