use crate::semantic::environment::r#type::module::HighModuleId;
use crate::visibility::Visibility;
use crate::{
    low::environment::value::{
        function::{FunctionDeclaration, FunctionId},
        variable::{VariableDeclaration, VariableId},
    },
    make_id,
};

pub mod function;
pub mod variable;

make_id!(ValueId);

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
    Function(Box<FunctionDeclaration>),
}

impl ValueDeclarationKind {
    #[inline]
    #[must_use]
    pub const fn with_visibility(
        self,
        module_path: Vec<HighModuleId>,
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
    pub const fn public(self, module_path: Vec<HighModuleId>) -> ValueDeclaration {
        self.with_visibility(module_path, Visibility::Public)
    }

    #[inline]
    #[must_use]
    pub const fn none_visibility(self, module_path: Vec<HighModuleId>) -> ValueDeclaration {
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
    pub module_path: Vec<HighModuleId>,
    pub kind: ValueDeclarationKind,
}
