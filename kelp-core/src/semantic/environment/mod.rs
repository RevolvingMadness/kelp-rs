use std::collections::{HashMap, hash_map::Iter};

use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::implementation::SemanticImplementation;
use crate::semantic::environment::{
    r#type::{
        HighGenericId, HighTypeId, SemanticTypeDeclaration, SemanticTypeDeclarationKind,
        r#struct::{
            HighStructId, SemanticStructDeclaration,
            regular::{HighRegularStructId, SemanticRegularStructDeclaration},
            tuple::{HighTupleStructId, SemanticTupleStructDeclaration},
        },
    },
    value::{
        HighValueId, SemanticValueDeclaration, SemanticValueDeclarationKind,
        function::{
            HighFunctionId, SemanticFunctionDeclaration,
            builtin::{HighBuiltinFunctionId, SemanticBuiltinFunctionDeclaration},
            regular::{HighRegularFunctionId, SemanticRegularFunctionDeclaration},
        },
        variable::{HighVariableId, SemanticVariableDeclaration},
    },
};
use crate::visibility::Visibility;

pub mod implementation;
pub mod r#type;
pub mod value;

#[derive(Debug, Clone, Default)]
pub struct SemanticEnvironment {
    types: HashMap<HighTypeId, SemanticTypeDeclaration>,
    values: HashMap<HighValueId, SemanticValueDeclaration>,

    pub implementations: HashMap<HighTypeId, Vec<SemanticImplementation>>,
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
            let SemanticValueDeclaration {
                kind: SemanticValueDeclarationKind::Function(declaration),
                ..
            } = declaration
            else {
                unreachable!("Value is not a function");
            };

            let SemanticFunctionDeclaration::Regular(declaration) = &mut **declaration else {
                unreachable!("Function is not regular");
            };

            f(declaration);
        });
    }

    pub fn declare_variable(
        &mut self,
        id: HighVariableId,
        visibility: Visibility,
        module_path: Vec<String>,
        name: String,
        data_type: SemanticDataType,
    ) {
        self.declare_value(
            id.into(),
            SemanticValueDeclaration {
                visibility,
                module_path,
                kind: SemanticValueDeclarationKind::Variable(SemanticVariableDeclaration {
                    name,
                    data_type,
                }),
            },
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

    #[inline]
    #[must_use]
    pub fn get_type(&self, id: HighTypeId) -> &SemanticTypeDeclaration {
        self.types.get(&id).unwrap()
    }

    #[must_use]
    pub fn get_generic(&self, id: HighGenericId) -> (&[String], Visibility, &String) {
        let SemanticTypeDeclaration {
            module_path,
            visibility,
            kind: SemanticTypeDeclarationKind::Generic(name),
        } = self.get_type(id.into())
        else {
            unreachable!();
        };

        (module_path, *visibility, name)
    }

    #[must_use]
    pub fn get_struct(
        &self,
        id: HighStructId,
    ) -> (&[String], Visibility, &SemanticStructDeclaration) {
        let SemanticTypeDeclaration {
            module_path,
            visibility,
            kind: SemanticTypeDeclarationKind::Struct(declaration),
        } = self.get_type(id.into())
        else {
            unreachable!();
        };

        (module_path, *visibility, declaration)
    }

    #[must_use]
    pub fn get_regular_struct(
        &self,
        id: HighRegularStructId,
    ) -> (&[String], Visibility, &SemanticRegularStructDeclaration) {
        let (module_path, visibility, SemanticStructDeclaration::Struct(declaration)) =
            self.get_struct(id.into())
        else {
            unreachable!();
        };

        (module_path, visibility, declaration)
    }

    #[must_use]
    pub fn get_tuple_struct(
        &self,
        id: HighTupleStructId,
    ) -> (&[String], Visibility, &SemanticTupleStructDeclaration) {
        let (module_path, visibility, SemanticStructDeclaration::Tuple(declaration)) =
            self.get_struct(id.into())
        else {
            unreachable!();
        };

        (module_path, visibility, declaration)
    }

    #[inline]
    #[must_use]
    pub fn get_value(&self, id: HighValueId) -> &SemanticValueDeclaration {
        self.values.get(&id).unwrap()
    }

    #[must_use]
    pub fn get_variable(
        &self,
        id: HighVariableId,
    ) -> (&[String], Visibility, &SemanticVariableDeclaration) {
        let SemanticValueDeclaration {
            module_path,
            visibility,
            kind: SemanticValueDeclarationKind::Variable(declaration),
        } = self.get_value(id.into())
        else {
            unreachable!();
        };

        (module_path, *visibility, declaration)
    }

    #[must_use]
    pub fn get_function(
        &self,
        id: HighFunctionId,
    ) -> (&[String], Visibility, &SemanticFunctionDeclaration) {
        let SemanticValueDeclaration {
            module_path,
            visibility,
            kind: SemanticValueDeclarationKind::Function(declaration),
        } = self.get_value(id.into())
        else {
            unreachable!();
        };

        (module_path, *visibility, declaration)
    }

    #[inline]
    #[must_use]
    pub fn get_function_declaration(&self, id: HighFunctionId) -> &SemanticFunctionDeclaration {
        self.get_function(id).2
    }

    #[must_use]
    pub fn get_regular_function(
        &self,
        id: HighRegularFunctionId,
    ) -> &SemanticRegularFunctionDeclaration {
        let SemanticFunctionDeclaration::Regular(declaration) =
            self.get_function_declaration(id.into())
        else {
            unreachable!();
        };

        declaration
    }

    #[must_use]
    pub fn get_builtin_function(
        &self,
        id: HighBuiltinFunctionId,
    ) -> &SemanticBuiltinFunctionDeclaration {
        let SemanticFunctionDeclaration::Builtin(declaration) =
            self.get_function_declaration(id.into())
        else {
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
