use std::collections::{HashMap, hash_map::Iter};

use crate::{
    high::environment::{
        r#type::{
            HighGenericId, HighTypeDeclaration, HighTypeDeclarationKind, HighTypeId,
            r#struct::{
                HighStructDeclaration, HighStructId,
                regular::{HighRegularStructDeclaration, HighRegularStructId},
                tuple::{HighTupleStructDeclaration, HighTupleStructId},
            },
        },
        value::{
            HighValueDeclaration, HighValueDeclarationKind, HighValueId,
            function::{
                HighFunctionDeclaration, HighFunctionId,
                builtin::{HighBuiltinFunctionDeclaration, HighBuiltinFunctionId},
                regular::{HighRegularFunctionDeclaration, HighRegularFunctionId},
            },
            variable::{HighVariableDeclaration, HighVariableId},
        },
    },
    low::data_type::unresolved::UnresolvedDataType,
    visibility::Visibility,
};

pub mod names;
pub mod r#type;
pub mod value;

#[derive(Debug, Clone)]
pub struct HighImpl {
    pub generic_names: Vec<String>,
    pub target_type: UnresolvedDataType,
    pub functions: HashMap<String, HighValueId>,
}

#[derive(Debug, Clone, Default)]
pub struct HighEnvironment {
    types: HashMap<HighTypeId, HighTypeDeclaration>,
    values: HashMap<HighValueId, HighValueDeclaration>,

    pub impls: HashMap<HighTypeId, Vec<HighImpl>>,
}

impl HighEnvironment {
    #[inline]
    pub fn declare_type(&mut self, id: HighTypeId, declaration: HighTypeDeclaration) {
        self.types.insert(id, declaration);
    }

    #[inline]
    pub fn declare_value(&mut self, id: HighValueId, declaration: HighValueDeclaration) {
        self.values.insert(id, declaration);
    }

    pub fn update_value(&mut self, id: HighValueId, f: impl FnOnce(&mut HighValueDeclaration)) {
        let declaration = self.values.get_mut(&id).unwrap();

        f(declaration);
    }

    pub fn update_regular_function(
        &mut self,
        id: HighRegularFunctionId,
        f: impl FnOnce(&mut HighRegularFunctionDeclaration),
    ) {
        self.update_value(id.into(), |declaration| {
            let HighValueDeclaration {
                kind: HighValueDeclarationKind::Function(declaration),
                ..
            } = declaration
            else {
                unreachable!("Value is not a function");
            };

            let HighFunctionDeclaration::Regular(declaration) = &mut **declaration else {
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
        data_type: UnresolvedDataType,
    ) {
        self.declare_value(
            id.into(),
            HighValueDeclaration {
                visibility,
                module_path,
                kind: HighValueDeclarationKind::Variable(HighVariableDeclaration {
                    name,
                    data_type,
                }),
            },
        );
    }

    #[must_use]
    pub fn declare_function(&mut self, declaration: HighValueDeclaration) -> HighRegularFunctionId {
        let id = HighRegularFunctionId(self.values.len() as u32);

        self.values.insert(id.into(), declaration);

        id
    }

    #[inline]
    #[must_use]
    pub fn get_type(&self, id: HighTypeId) -> &HighTypeDeclaration {
        self.types.get(&id).unwrap()
    }

    #[must_use]
    pub fn get_generic(&self, id: HighGenericId) -> (&[String], Visibility, &String) {
        let HighTypeDeclaration {
            module_path,
            visibility,
            kind: HighTypeDeclarationKind::Generic(name),
        } = self.get_type(id.into())
        else {
            unreachable!();
        };

        (module_path, *visibility, name)
    }

    #[must_use]
    pub fn get_struct(&self, id: HighStructId) -> (&[String], Visibility, &HighStructDeclaration) {
        let HighTypeDeclaration {
            module_path,
            visibility,
            kind: HighTypeDeclarationKind::Struct(declaration),
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
    ) -> (&[String], Visibility, &HighRegularStructDeclaration) {
        let (module_path, visibility, HighStructDeclaration::Struct(declaration)) =
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
    ) -> (&[String], Visibility, &HighTupleStructDeclaration) {
        let (module_path, visibility, HighStructDeclaration::Tuple(declaration)) =
            self.get_struct(id.into())
        else {
            unreachable!();
        };

        (module_path, visibility, declaration)
    }

    #[inline]
    #[must_use]
    pub fn get_value(&self, id: HighValueId) -> &HighValueDeclaration {
        self.values.get(&id).unwrap()
    }

    #[must_use]
    pub fn get_variable(
        &self,
        id: HighVariableId,
    ) -> (&[String], Visibility, &HighVariableDeclaration) {
        let HighValueDeclaration {
            module_path,
            visibility,
            kind: HighValueDeclarationKind::Variable(declaration),
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
    ) -> (&[String], Visibility, &HighFunctionDeclaration) {
        let HighValueDeclaration {
            module_path,
            visibility,
            kind: HighValueDeclarationKind::Function(declaration),
        } = self.get_value(id.into())
        else {
            unreachable!();
        };

        (module_path, *visibility, declaration)
    }

    #[must_use]
    pub fn get_regular_function(
        &self,
        id: HighRegularFunctionId,
    ) -> (&[String], Visibility, &HighRegularFunctionDeclaration) {
        let (module_path, visibility, HighFunctionDeclaration::Regular(declaration)) =
            self.get_function(id.into())
        else {
            unreachable!();
        };

        (module_path, visibility, declaration)
    }

    #[must_use]
    pub fn get_builtin_function(
        &self,
        id: HighBuiltinFunctionId,
    ) -> (&[String], Visibility, &HighBuiltinFunctionDeclaration) {
        let (module_path, visibility, HighFunctionDeclaration::Builtin(declaration)) =
            self.get_function(id.into())
        else {
            unreachable!();
        };

        (module_path, visibility, declaration)
    }

    #[must_use]
    pub fn iter_types(&self) -> Iter<'_, HighTypeId, HighTypeDeclaration> {
        self.types.iter()
    }

    #[must_use]
    pub fn iter_values(&self) -> Iter<'_, HighValueId, HighValueDeclaration> {
        self.values.iter()
    }
}
