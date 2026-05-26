use std::collections::HashMap;

use la_arena::Idx;

use crate::{
    low::environment::{
        r#type::{
            TypeDeclaration, TypeDeclarationKind,
            r#struct::{
                RegularStructDeclaration, RegularStructId, StructDeclaration, StructId,
                TupleStructDeclaration, TupleStructId,
            },
        },
        value::{
            ValueDeclaration, ValueDeclarationKind, ValueId,
            function::{
                FunctionDeclaration, FunctionId,
                regular::{RegularFunctionDeclaration, RegularFunctionId},
            },
            variable::{VariableDeclaration, VariableId},
        },
    },
    typed::{
        data_type::resolved::DataType, expression::typed::TypedExpressionId, pattern::TypedPattern,
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
        module_path: Vec<String>,
        visibility: Visibility,
        declaration: TypeDeclarationKind,
    ) -> u32 {
        let len = self.types.len() as u32;

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
            module_path,
            visibility,
            TypeDeclarationKind::Struct(declaration),
        );

        StructId(id)
    }

    #[inline]
    #[must_use]
    pub fn declare_regular_struct(
        &mut self,
        module_path: Vec<String>,
        visibility: Visibility,
        name: String,
        generic_types: Vec<DataType>,
        field_types: HashMap<String, DataType>,
    ) -> RegularStructId {
        let id = self.declare_struct(
            module_path,
            visibility,
            StructDeclaration::Struct(RegularStructDeclaration {
                name,
                generic_types,
                field_types,
            }),
        );

        RegularStructId(id.0)
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
    ) -> TupleStructId {
        let id = self.declare_struct(
            module_path,
            visibility,
            StructDeclaration::Tuple(TupleStructDeclaration {
                name,
                generic_types,
                field_types,
            }),
        );

        TupleStructId(id.0)
    }

    #[must_use]
    pub fn get_struct(&self, id: StructId) -> (&[String], Visibility, &StructDeclaration) {
        #[allow(irrefutable_let_patterns)]
        let TypeDeclaration {
            visibility,
            module_path,
            kind: TypeDeclarationKind::Struct(declaration),
        } = &self.types[id.0 as usize]
        else {
            unreachable!();
        };

        (module_path, *visibility, declaration)
    }

    #[must_use]
    pub fn get_regular_struct(
        &self,
        id: RegularStructId,
    ) -> (&[String], Visibility, &RegularStructDeclaration) {
        let (module_path, visibility, StructDeclaration::Struct(declaration)) =
            self.get_struct(id.into())
        else {
            unreachable!();
        };

        (module_path, visibility, declaration)
    }

    #[must_use]
    pub fn get_tuple_struct(
        &self,
        id: TupleStructId,
    ) -> (&[String], Visibility, &TupleStructDeclaration) {
        let (module_path, visibility, StructDeclaration::Tuple(declaration)) =
            self.get_struct(id.into())
        else {
            unreachable!();
        };

        (module_path, visibility, declaration)
    }

    #[must_use]
    pub fn declare_value(
        &mut self,
        module_path: Vec<String>,
        visibility: Visibility,
        declaration: ValueDeclarationKind,
    ) -> ValueId {
        let id = self.values.len() as u32;

        self.values.push(ValueDeclaration {
            visibility,
            module_path,
            kind: declaration,
        });

        ValueId(id)
    }

    #[inline]
    #[must_use]
    pub fn get_value(&self, id: ValueId) -> &ValueDeclaration {
        &self.values[id.0 as usize]
    }

    #[inline]
    #[must_use]
    pub fn get_variable(&self, id: ValueId) -> (&[String], Visibility, &VariableDeclaration) {
        let ValueDeclaration {
            visibility,
            module_path,
            kind: ValueDeclarationKind::Variable(declaration),
        } = self.get_value(id)
        else {
            unreachable!();
        };

        (module_path, *visibility, declaration)
    }

    #[must_use]
    pub fn declare_variable(
        &mut self,
        module_path: Vec<String>,
        visibility: Visibility,
        name: String,
        data_type: DataType,
    ) -> VariableId {
        let id = VariableId(self.values.len() as u32);

        self.values.push(
            ValueDeclarationKind::Variable(VariableDeclaration { name, data_type })
                .with_visibility(module_path, visibility),
        );

        id
    }

    #[must_use]
    pub fn declare_function(
        &mut self,
        module_path: Vec<String>,
        visibility: Visibility,
        declaration: FunctionDeclaration,
    ) -> FunctionId {
        let id = FunctionId(self.values.len() as u32);

        self.values.push(
            ValueDeclarationKind::Function(Box::new(declaration))
                .with_visibility(module_path, visibility),
        );

        id
    }

    #[must_use]
    pub fn get_function<I: Into<FunctionId>>(
        &self,
        id: I,
    ) -> (&[String], Visibility, &FunctionDeclaration) {
        let id = id.into();

        let ValueDeclaration {
            visibility,
            module_path,
            kind: ValueDeclarationKind::Function(declaration),
        } = &self.values[id.0 as usize]
        else {
            unreachable!();
        };

        (module_path, *visibility, declaration)
    }

    pub fn update_regular_function(
        &mut self,
        id: RegularFunctionId,
        new_parameters: Vec<(Idx<TypedPattern>, DataType)>,
        new_body: TypedExpressionId,
    ) {
        let ValueDeclaration {
            kind: ValueDeclarationKind::Function(declaration),
            ..
        } = &mut self.values[id.0 as usize]
        else {
            unreachable!();
        };

        let FunctionDeclaration::Regular(RegularFunctionDeclaration {
            parameters: old_parameters,
            body: old_body,
            ..
        }) = &mut **declaration
        else {
            unreachable!();
        };

        *old_parameters = new_parameters;
        *old_body = new_body;
    }
}
