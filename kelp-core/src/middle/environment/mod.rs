use std::collections::HashMap;

use crate::middle::{
    data_type::DataType,
    environment::{
        r#type::{
            TypeDeclaration, TypeId,
            r#struct::{StructDeclaration, StructId},
        },
        value::{
            ValueDeclaration, ValueId,
            variable::{VariableDeclaration, VariableId},
        },
    },
};

pub mod r#type;
pub mod value;

#[derive(Debug, Clone, Default)]
pub struct Environment {
    pub types: Vec<TypeDeclaration>,
    pub values: Vec<ValueDeclaration>,
}

impl Environment {
    #[must_use]
    pub fn declare_type(&mut self, declaration: TypeDeclaration) -> TypeId {
        let id = TypeId(self.types.len());

        self.types.push(declaration);

        id
    }

    #[inline]
    #[must_use]
    pub fn get_type(&self, id: TypeId) -> &TypeDeclaration {
        &self.types[id.0]
    }

    #[must_use]
    pub fn declare_struct(
        &mut self,
        name: String,
        generic_types: Vec<DataType>,
        field_types: HashMap<String, DataType>,
    ) -> StructId {
        let id = StructId(self.types.len());

        self.types.push(TypeDeclaration::Struct(StructDeclaration {
            name,
            generic_types,
            field_types,
        }));

        id
    }

    #[must_use]
    pub fn get_struct(&self, id: StructId) -> &StructDeclaration {
        let TypeDeclaration::Struct(declaration) = &self.types[id.0] else {
            unreachable!();
        };

        declaration
    }

    #[must_use]
    pub fn declare_value(&mut self, declaration: ValueDeclaration) -> ValueId {
        let id = ValueId(self.values.len());

        self.values.push(declaration);

        id
    }

    #[inline]
    #[must_use]
    pub fn get_value(&self, id: ValueId) -> &ValueDeclaration {
        &self.values[id.0]
    }

    #[must_use]
    pub fn declare_variable(&mut self, name: String, data_type: Option<DataType>) -> VariableId {
        let id = VariableId(self.types.len());

        self.values
            .push(ValueDeclaration::Variable(VariableDeclaration {
                name,
                data_type,
            }));

        id
    }

    #[must_use]
    pub fn get_variable(&self, id: VariableId) -> &VariableDeclaration {
        #[allow(irrefutable_let_patterns)] // TODO: Remove this
        let ValueDeclaration::Variable(declaration) = &self.values[id.0] else {
            unreachable!();
        };

        declaration
    }
}
