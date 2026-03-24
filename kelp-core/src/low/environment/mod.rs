use std::collections::HashMap;

use crate::{
    low::{
        data_type::DataType,
        environment::{
            r#type::{
                TypeDeclaration, TypeDeclarationKind,
                r#struct::{
                    StructDeclaration, StructId, StructStructDeclaration, StructStructId,
                    TupleStructDeclaration, TupleStructId,
                },
            },
            value::{
                ValueDeclaration, ValueDeclarationKind, ValueId,
                variable::{VariableDeclaration, VariableId},
            },
        },
    },
    visibility::Visibility,
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
    pub fn declare_type(
        &mut self,
        visibility: Visibility,
        module_path: Vec<String>,
        declaration: TypeDeclarationKind,
    ) -> usize {
        let len = self.types.len();

        self.types.push(TypeDeclaration {
            visibility,
            module_path,
            kind: declaration,
        });

        len
    }

    #[inline]
    #[must_use]
    pub fn declare_struct(
        &mut self,
        module_path: Vec<String>,
        visibility: Visibility,
        declaration: StructDeclaration,
    ) -> StructId {
        let id = self.declare_type(
            visibility,
            module_path,
            TypeDeclarationKind::Struct(declaration),
        );

        StructId(id)
    }

    #[inline]
    #[must_use]
    pub fn declare_struct_struct(
        &mut self,
        module_path: Vec<String>,
        visibility: Visibility,
        name: String,
        generic_types: Vec<DataType>,
        field_types: HashMap<String, DataType>,
    ) -> StructId {
        self.declare_struct(
            module_path,
            visibility,
            StructDeclaration::Struct(StructStructDeclaration {
                name,
                generic_types,
                field_types,
            }),
        )
    }

    #[inline]
    #[must_use]
    pub fn declare_tuple_struct(
        &mut self,
        module_path: Vec<String>,
        visibility: Visibility,
        name: String,
        generic_types: Vec<DataType>,
        field_types: Vec<DataType>,
    ) -> StructId {
        self.declare_struct(
            module_path,
            visibility,
            StructDeclaration::Tuple(TupleStructDeclaration {
                name,
                generic_types,
                field_types,
            }),
        )
    }

    #[must_use]
    pub fn get_struct(&self, id: StructId) -> (Visibility, &[String], &StructDeclaration) {
        let (visibility, module_path, TypeDeclarationKind::Struct(declaration)) =
            self.types[id.0].as_tuple()
        else {
            unreachable!();
        };

        (visibility, module_path, declaration)
    }

    #[must_use]
    pub fn get_struct_struct(
        &self,
        id: StructStructId,
    ) -> (Visibility, &[String], &StructStructDeclaration) {
        let (
            visibility,
            module_path,
            TypeDeclarationKind::Struct(StructDeclaration::Struct(declaration)),
        ) = self.types[id.0].as_tuple()
        else {
            unreachable!();
        };

        (visibility, module_path, declaration)
    }

    #[must_use]
    pub fn get_tuple_struct(
        &self,
        id: TupleStructId,
    ) -> (Visibility, &[String], &TupleStructDeclaration) {
        let (
            visibility,
            module_path,
            TypeDeclarationKind::Struct(StructDeclaration::Tuple(declaration)),
        ) = self.types[id.0].as_tuple()
        else {
            unreachable!();
        };

        (visibility, module_path, declaration)
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
    pub fn declare_variable(
        &mut self,
        module_path: Vec<String>,
        visibility: Visibility,
        name: String,
        data_type: Option<DataType>,
    ) -> VariableId {
        let id = VariableId(self.values.len());

        self.values.push(
            ValueDeclarationKind::Variable(VariableDeclaration { name, data_type })
                .with_visibility(module_path, visibility),
        );

        id
    }
}
