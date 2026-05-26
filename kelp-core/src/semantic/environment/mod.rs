use std::collections::{hash_map::Iter, HashMap};

use crate::visibility::Visibility;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::{
    r#type::{
        r#struct::{
            regular::{HighRegularStructId, ResolvedRegularStructDeclaration}, tuple::{HighTupleStructId, SemanticTupleStructDeclaration},
            HighStructId,
            ResolvedStructDeclaration,
        }, HighGenericId, HighTypeId, ResolvedTypeDeclaration,
        ResolvedTypeDeclarationKind,
    },
    value::{
        function::{
            builtin::{HighBuiltinFunctionId, ResolvedBuiltinFunctionDeclaration}, regular::{HighRegularFunctionId, ResolvedRegularFunctionDeclaration},
            HighFunctionId,
            ResolvedFunctionDeclaration,
        }, variable::{HighVariableId, ResolvedVariableDeclaration}, HighValueId,
        ResolvedValueDeclaration,
        ResolvedValueDeclarationKind,
    },
};

pub mod r#type;
pub mod value;

#[derive(Debug, Clone)]
pub struct HighImpl {
    pub generic_names: Vec<String>,
    pub target_type: SemanticDataType,
    pub types: HashMap<String, HighTypeId>,
    pub values: HashMap<String, HighValueId>,
}

#[derive(Debug, Clone, Default)]
pub struct ResolvedEnvironment {
    types: HashMap<HighTypeId, ResolvedTypeDeclaration>,
    values: HashMap<HighValueId, ResolvedValueDeclaration>,

    pub impls: HashMap<HighTypeId, Vec<HighImpl>>,
}

impl ResolvedEnvironment {
    #[inline]
    pub fn declare_type(&mut self, id: HighTypeId, declaration: ResolvedTypeDeclaration) {
        self.types.insert(id, declaration);
    }

    #[inline]
    pub fn declare_value(&mut self, id: HighValueId, declaration: ResolvedValueDeclaration) {
        self.values.insert(id, declaration);
    }

    pub fn update_value(&mut self, id: HighValueId, f: impl FnOnce(&mut ResolvedValueDeclaration)) {
        let declaration = self.values.get_mut(&id).unwrap();

        f(declaration);
    }

    pub fn update_regular_function(
        &mut self,
        id: HighRegularFunctionId,
        f: impl FnOnce(&mut ResolvedRegularFunctionDeclaration),
    ) {
        self.update_value(id.into(), |declaration| {
            let ResolvedValueDeclaration {
                kind: ResolvedValueDeclarationKind::Function(declaration),
                ..
            } = declaration
            else {
                unreachable!("Value is not a function");
            };

            let ResolvedFunctionDeclaration::Regular(declaration) = &mut **declaration else {
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
            ResolvedValueDeclaration {
                visibility,
                module_path,
                kind: ResolvedValueDeclarationKind::Variable(ResolvedVariableDeclaration {
                    name,
                    data_type,
                }),
            },
        );
    }

    #[must_use]
    pub fn declare_function(
        &mut self,
        declaration: ResolvedValueDeclaration,
    ) -> HighRegularFunctionId {
        let id = HighRegularFunctionId(self.values.len() as u32);

        self.values.insert(id.into(), declaration);

        id
    }

    #[inline]
    #[must_use]
    pub fn get_type(&self, id: HighTypeId) -> &ResolvedTypeDeclaration {
        self.types.get(&id).unwrap()
    }

    #[must_use]
    pub fn get_generic(&self, id: HighGenericId) -> (&[String], Visibility, &String) {
        let ResolvedTypeDeclaration {
            module_path,
            visibility,
            kind: ResolvedTypeDeclarationKind::Generic(name),
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
    ) -> (&[String], Visibility, &ResolvedStructDeclaration) {
        let ResolvedTypeDeclaration {
            module_path,
            visibility,
            kind: ResolvedTypeDeclarationKind::Struct(declaration),
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
    ) -> (&[String], Visibility, &ResolvedRegularStructDeclaration) {
        let (module_path, visibility, ResolvedStructDeclaration::Struct(declaration)) =
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
        let (module_path, visibility, ResolvedStructDeclaration::Tuple(declaration)) =
            self.get_struct(id.into())
        else {
            unreachable!();
        };

        (module_path, visibility, declaration)
    }

    #[inline]
    #[must_use]
    pub fn get_value(&self, id: HighValueId) -> &ResolvedValueDeclaration {
        self.values.get(&id).unwrap()
    }

    #[must_use]
    pub fn get_variable(
        &self,
        id: HighVariableId,
    ) -> (&[String], Visibility, &ResolvedVariableDeclaration) {
        let ResolvedValueDeclaration {
            module_path,
            visibility,
            kind: ResolvedValueDeclarationKind::Variable(declaration),
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
    ) -> (&[String], Visibility, &ResolvedFunctionDeclaration) {
        let ResolvedValueDeclaration {
            module_path,
            visibility,
            kind: ResolvedValueDeclarationKind::Function(declaration),
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
    ) -> (&[String], Visibility, &ResolvedRegularFunctionDeclaration) {
        let (module_path, visibility, ResolvedFunctionDeclaration::Regular(declaration)) =
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
    ) -> (&[String], Visibility, &ResolvedBuiltinFunctionDeclaration) {
        let (module_path, visibility, ResolvedFunctionDeclaration::Builtin(declaration)) =
            self.get_function(id.into())
        else {
            unreachable!();
        };

        (module_path, visibility, declaration)
    }

    #[must_use]
    pub fn iter_types(&self) -> Iter<'_, HighTypeId, ResolvedTypeDeclaration> {
        self.types.iter()
    }

    #[must_use]
    pub fn iter_values(&self) -> Iter<'_, HighValueId, ResolvedValueDeclaration> {
        self.values.iter()
    }
}
