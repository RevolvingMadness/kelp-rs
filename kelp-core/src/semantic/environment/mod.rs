use std::collections::{HashMap, hash_map::Iter};

use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::implementation::SemanticImplementation;
use crate::semantic::environment::r#type::generic::{HighGenericId, SemanticGenericDeclaration};
use crate::semantic::environment::r#type::r#struct::unit::{
    HighUnitStructId, SemanticUnitStructDeclaration,
};
use crate::semantic::environment::{
    r#type::{
        HighTypeId, SemanticTypeDeclaration,
        r#struct::{
            HighStructId, SemanticStructDeclaration,
            regular::{HighRegularStructId, SemanticRegularStructDeclaration},
            tuple::{HighTupleStructId, SemanticTupleStructDeclaration},
        },
    },
    value::{
        HighValueId, SemanticValueDeclaration,
        function::{
            HighFunctionId, SemanticFunctionDeclaration,
            builtin::{HighBuiltinFunctionId, SemanticBuiltinFunctionDeclaration},
            regular::{HighRegularFunctionId, SemanticRegularFunctionDeclaration},
        },
        variable::{HighVariableId, SemanticVariableDeclaration},
    },
};
use crate::span::Span;
use crate::visibility::Visibility;

pub mod implementation;
pub mod r#type;
pub mod value;

#[derive(Debug, Clone, Default)]
pub struct SemanticEnvironment {
    types: HashMap<HighTypeId, SemanticTypeDeclaration>,
    values: HashMap<HighValueId, SemanticValueDeclaration>,

    implementations: HashMap<HighTypeId, Vec<SemanticImplementation>>,
}

impl SemanticEnvironment {
    #[inline]
    pub fn declare_type(&mut self, id: HighTypeId, declaration: SemanticTypeDeclaration) {
        self.types.insert(id, declaration);
    }

    #[inline]
    pub fn declare_value(&mut self, id: HighValueId, declaration: SemanticValueDeclaration) {
        self.values.insert(id, declaration);
    }

    pub fn update_value(&mut self, id: HighValueId, f: impl FnOnce(&mut SemanticValueDeclaration)) {
        let declaration = self.values.get_mut(&id).unwrap();

        f(declaration);
    }

    pub fn update_regular_function(
        &mut self,
        id: HighRegularFunctionId,
        f: impl FnOnce(&mut SemanticRegularFunctionDeclaration),
    ) {
        self.update_value(id.into(), |declaration| {
            let SemanticValueDeclaration::Function(declaration) = declaration else {
                unreachable!("Value is not a function");
            };

            let SemanticFunctionDeclaration::Regular(declaration) = declaration else {
                unreachable!("Function is not regular");
            };

            f(declaration);
        });
    }

    pub fn declare_variable(
        &mut self,
        id: HighVariableId,
        name_span: Span,
        name: String,
        data_type: SemanticDataType,
    ) {
        self.declare_value(
            id.into(),
            SemanticValueDeclaration::Variable(SemanticVariableDeclaration {
                name_span,
                name,
                data_type,
            }),
        );
    }

    #[must_use]
    pub fn declare_function(
        &mut self,
        declaration: SemanticValueDeclaration,
    ) -> HighRegularFunctionId {
        let id = HighRegularFunctionId(self.values.len() as u32);

        self.values.insert(id.into(), declaration);

        id
    }

    #[must_use]
    pub fn get_type<I: Into<HighTypeId>>(&self, id: I) -> &SemanticTypeDeclaration {
        let id = id.into();

        self.types.get(&id).unwrap()
    }

    #[must_use]
    pub fn get_implementations<I: Into<HighTypeId>>(
        &self,
        id: I,
    ) -> Option<&[SemanticImplementation]> {
        let id = id.into();

        self.implementations.get(&id).map(AsRef::as_ref)
    }

    #[must_use]
    pub fn get_implementations_mut<I: Into<HighTypeId>>(
        &mut self,
        id: I,
    ) -> &mut Vec<SemanticImplementation> {
        let id = id.into();

        self.implementations.entry(id).or_default()
    }

    pub fn add_implementation<I: Into<HighTypeId>>(
        &mut self,
        id: I,
        implementation: SemanticImplementation,
    ) {
        let implementations = self.get_implementations_mut(id);

        implementations.push(implementation);
    }

    #[must_use]
    pub fn get_generic(&self, id: HighGenericId) -> &SemanticGenericDeclaration {
        let SemanticTypeDeclaration::Generic(declaration) = self.get_type(id) else {
            unreachable!();
        };

        declaration
    }

    #[must_use]
    pub fn get_struct(&self, id: HighStructId) -> &SemanticStructDeclaration {
        let SemanticTypeDeclaration::Struct(declaration) = self.get_type(id) else {
            unreachable!();
        };

        declaration
    }

    #[must_use]
    pub fn get_regular_struct(&self, id: HighRegularStructId) -> &SemanticRegularStructDeclaration {
        let SemanticStructDeclaration::Struct(declaration) = self.get_struct(id.into()) else {
            unreachable!();
        };

        declaration
    }

    #[must_use]
    pub fn get_tuple_struct(&self, id: HighTupleStructId) -> &SemanticTupleStructDeclaration {
        let SemanticStructDeclaration::Tuple(declaration) = self.get_struct(id.into()) else {
            unreachable!();
        };

        declaration
    }

    #[must_use]
    pub fn get_unit_struct(&self, id: HighUnitStructId) -> &SemanticUnitStructDeclaration {
        let SemanticStructDeclaration::Unit(declaration) = self.get_struct(id.into()) else {
            unreachable!();
        };

        declaration
    }

    #[inline]
    #[must_use]
    pub fn get_value<I: Into<HighValueId>>(&self, id: I) -> &SemanticValueDeclaration {
        let id = id.into();

        self.values.get(&id).unwrap()
    }

    #[inline]
    #[must_use]
    pub fn get_value_mut<I: Into<HighValueId>>(&mut self, id: I) -> &mut SemanticValueDeclaration {
        let id = id.into();

        self.values.get_mut(&id).unwrap()
    }

    #[must_use]
    pub fn get_variable(&self, id: HighVariableId) -> &SemanticVariableDeclaration {
        let SemanticValueDeclaration::Variable(declaration) = self.get_value(id) else {
            unreachable!();
        };

        declaration
    }

    #[must_use]
    pub fn get_function(&self, id: HighFunctionId) -> &SemanticFunctionDeclaration {
        let SemanticValueDeclaration::Function(declaration) = self.get_value(id) else {
            unreachable!();
        };

        declaration
    }

    #[must_use]
    pub fn get_regular_function(
        &self,
        id: HighRegularFunctionId,
    ) -> &SemanticRegularFunctionDeclaration {
        let SemanticFunctionDeclaration::Regular(declaration) = self.get_function(id.into()) else {
            unreachable!();
        };

        declaration
    }

    #[must_use]
    pub fn get_builtin_function(
        &self,
        id: HighBuiltinFunctionId,
    ) -> &SemanticBuiltinFunctionDeclaration {
        let SemanticFunctionDeclaration::Builtin(declaration) = self.get_function(id.into()) else {
            unreachable!();
        };

        declaration
    }

    #[must_use]
    pub fn iter_types(&self) -> Iter<'_, HighTypeId, SemanticTypeDeclaration> {
        self.types.iter()
    }

    #[must_use]
    pub fn iter_values(&self) -> Iter<'_, HighValueId, SemanticValueDeclaration> {
        self.values.iter()
    }
}
