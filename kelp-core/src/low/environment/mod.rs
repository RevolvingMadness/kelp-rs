use std::collections::HashMap;

use crate::low::data_type::DataType;
use crate::low::environment::value::constant::{ConstantDeclaration, ConstantId};
use crate::low::environment::{
    r#type::{
        TypeDeclaration,
        r#struct::{
            RegularStructDeclaration, RegularStructId, StructDeclaration, StructId,
            TupleStructDeclaration, TupleStructId,
        },
    },
    value::{
        ValueDeclaration, ValueId,
        function::{
            FunctionDeclaration, FunctionId,
            regular::{RegularFunctionDeclaration, RegularFunctionId},
        },
        variable::{VariableDeclaration, VariableId},
    },
};
use crate::semantic::{expression::SemanticExpression, pattern::SemanticPattern};

pub mod r#type;
pub mod value;

#[derive(Debug, Clone, Default)]
pub struct Environment {
    pub types: Vec<TypeDeclaration>,
    pub values: Vec<ValueDeclaration>,
}

impl Environment {
    #[must_use]
    pub fn declare_type(&mut self, declaration: TypeDeclaration) -> u32 {
        let len = self.types.len() as u32;

        self.types.push(declaration);

        len
    }

    #[inline]
    #[must_use]
    pub fn declare_struct(&mut self, declaration: StructDeclaration) -> StructId {
        let id = self.declare_type(TypeDeclaration::Struct(declaration));

        StructId(id)
    }

    #[inline]
    #[must_use]
    pub fn declare_regular_struct(
        &mut self,
        name: String,
        generic_types: Vec<DataType>,
        field_types: HashMap<String, DataType>,
    ) -> RegularStructId {
        let id = self.declare_struct(StructDeclaration::Struct(RegularStructDeclaration {
            name,
            generic_types,
            field_types,
        }));

        RegularStructId(id.0)
    }

    #[inline]
    #[must_use]
    pub fn declare_tuple_struct(
        &mut self,
        name: String,
        generic_types: Vec<DataType>,
        field_types: Vec<DataType>,
    ) -> TupleStructId {
        let id = self.declare_struct(StructDeclaration::Tuple(TupleStructDeclaration {
            name,
            generic_types,
            field_types,
        }));

        TupleStructId(id.0)
    }

    #[must_use]
    pub fn get_struct(&self, id: StructId) -> &StructDeclaration {
        #[allow(irrefutable_let_patterns)]
        let TypeDeclaration::Struct(declaration) = &self.types[id.0 as usize] else {
            unreachable!();
        };

        declaration
    }

    #[must_use]
    pub fn get_regular_struct(&self, id: RegularStructId) -> &RegularStructDeclaration {
        let StructDeclaration::Struct(declaration) = self.get_struct(id.into()) else {
            unreachable!();
        };

        declaration
    }

    #[must_use]
    pub fn get_tuple_struct(&self, id: TupleStructId) -> &TupleStructDeclaration {
        let StructDeclaration::Tuple(declaration) = self.get_struct(id.into()) else {
            unreachable!();
        };

        declaration
    }

    #[must_use]
    pub fn declare_value(&mut self, declaration: ValueDeclaration) -> ValueId {
        let id = self.values.len() as u32;

        self.values.push(declaration);

        ValueId(id)
    }

    #[inline]
    #[must_use]
    pub fn get_value(&self, id: ValueId) -> &ValueDeclaration {
        &self.values[id.0 as usize]
    }

    #[inline]
    #[must_use]
    pub fn get_variable(&self, id: ValueId) -> &VariableDeclaration {
        let ValueDeclaration::Variable(declaration) = self.get_value(id) else {
            unreachable!();
        };

        declaration
    }

    #[must_use]
    pub fn declare_variable(&mut self, name: String, data_type: DataType) -> VariableId {
        let id = self.declare_value(ValueDeclaration::Variable(VariableDeclaration {
            name,
            data_type,
        }));

        VariableId(id.0)
    }

    #[must_use]
    pub fn declare_constant(&mut self, name: String, data_type: DataType) -> ConstantId {
        let id = self.declare_value(ValueDeclaration::Constant(ConstantDeclaration {
            name,
            data_type,
        }));

        ConstantId(id.0)
    }

    #[must_use]
    pub fn declare_function(&mut self, declaration: FunctionDeclaration) -> FunctionId {
        let id = FunctionId(self.values.len() as u32);

        self.values
            .push(ValueDeclaration::Function(Box::new(declaration)));

        id
    }

    #[must_use]
    pub fn get_function<I: Into<FunctionId>>(&self, id: I) -> &FunctionDeclaration {
        let id = id.into();

        let ValueDeclaration::Function(declaration) = &self.values[id.0 as usize] else {
            unreachable!();
        };

        declaration
    }

    pub fn update_regular_function(
        &mut self,
        id: RegularFunctionId,
        new_parameters: Vec<(SemanticPattern, DataType)>,
        new_body: SemanticExpression,
    ) {
        let ValueDeclaration::Function(declaration) = &mut self.values[id.0 as usize] else {
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
