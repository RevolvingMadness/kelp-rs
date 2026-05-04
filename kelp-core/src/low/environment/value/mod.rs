use crate::{
    low::environment::value::{
        function::{FunctionDeclaration, FunctionId},
        variable::{VariableDeclaration, VariableId},
    },
    visibility::Visibility,
};

pub mod function;
pub mod variable;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ValueId(pub usize);

impl From<VariableId> for ValueId {
    fn from(value: VariableId) -> Self {
        Self(value.0)
    }
}

impl From<FunctionId> for ValueId {
    fn from(value: FunctionId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub enum ValueDeclarationKind {
    Variable(VariableDeclaration),
    Function(FunctionDeclaration),
}

impl ValueDeclarationKind {
    #[inline]
    #[must_use]
    pub const fn with_visibility(
        self,
        module_path: Vec<String>,
        visibility: Visibility,
    ) -> ValueDeclaration {
        ValueDeclaration {
            visibility,
            module_path,
            kind: self,
        }
    }

    #[inline]
    #[must_use]
    pub const fn public(self, module_path: Vec<String>) -> ValueDeclaration {
        self.with_visibility(module_path, Visibility::Public)
    }

    #[inline]
    #[must_use]
    pub const fn none_visibility(self, module_path: Vec<String>) -> ValueDeclaration {
        self.with_visibility(module_path, Visibility::None)
    }

    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Variable(declaration) => &declaration.name,
            Self::Function(declaration) => declaration.name(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ValueDeclaration {
    pub visibility: Visibility,
    pub module_path: Vec<String>,
    pub kind: ValueDeclarationKind,
}
