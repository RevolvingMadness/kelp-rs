use crate::low::environment::value::constant::ConstantDeclaration;
use crate::semantic::environment::r#type::module::HighModuleId;
use crate::visibility::Visibility;
use crate::{
    low::environment::value::{function::FunctionDeclaration, variable::VariableDeclaration},
    make_id,
};

pub mod constant;
pub mod function;
pub mod variable;

make_id!(ValueId);

#[derive(Debug, Clone)]
pub enum ValueDeclarationKind {
    Variable(VariableDeclaration),
    Constant(ConstantDeclaration),
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
}

#[derive(Debug, Clone)]
pub struct ValueDeclaration {
    pub visibility: Visibility,
    pub module_path: Vec<HighModuleId>,
    pub kind: ValueDeclarationKind,
}
