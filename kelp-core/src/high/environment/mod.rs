use crate::{
    high::environment::{
        r#type::{
            HighTypeDeclaration, HighTypeDeclarationKind, HighTypeId,
            r#struct::{
                HighStructDeclaration, HighStructId,
                regular::{HighStructStructDeclaration, HighStructStructId},
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
    low::{
        data_type::unresolved::UnresolvedDataType, expression::unresolved::UnresolvedExpression,
        pattern::UnresolvedPattern,
    },
    visibility::Visibility,
};

pub mod r#type;
pub mod value;

#[derive(Debug, Clone, Default)]
pub struct HighEnvironment {
    pub types: Vec<HighTypeDeclaration>,
    pub values: Vec<HighValueDeclaration>,
}

impl HighEnvironment {
    #[must_use]
    pub fn declare_type(&mut self, declaration: HighTypeDeclaration) -> HighTypeId {
        let id = HighTypeId(self.types.len() as u32);

        self.types.push(declaration);

        id
    }

    #[must_use]
    pub fn declare_value(
        &mut self,
        visibility: Visibility,
        module_path: Vec<String>,
        declaration: HighValueDeclarationKind,
    ) -> HighValueId {
        let id = HighValueId(self.values.len() as u32);

        self.values.push(HighValueDeclaration {
            visibility,
            module_path,
            kind: declaration,
        });

        id
    }

    #[inline]
    pub fn update_declaration(
        &mut self,
        id: HighValueId,
        f: impl FnOnce(&mut HighValueDeclaration),
    ) {
        f(self.values.get_mut(id.0 as usize).unwrap());
    }

    pub fn update_function(
        &mut self,
        id: HighRegularFunctionId,
        new_parameters: Vec<(UnresolvedPattern, UnresolvedDataType)>,
        new_body: UnresolvedExpression,
    ) {
        self.update_declaration(id.into(), |declaration| {
            let HighValueDeclaration {
                kind:
                    HighValueDeclarationKind::Function(HighFunctionDeclaration::Regular(
                        HighRegularFunctionDeclaration {
                            parameters: old_parameters,
                            body: old_body,
                            ..
                        },
                    )),
                ..
            } = declaration
            else {
                unreachable!("Value is not a function");
            };

            *old_parameters = new_parameters
                .into_iter()
                .map(|(pattern, data_type)| (Some(pattern), data_type))
                .collect();
            *old_body = Some(new_body);
        });
    }

    #[must_use]
    pub fn declare_variable(
        &mut self,
        visiblity: Visibility,
        module_path: Vec<String>,
        name: String,
        data_type: UnresolvedDataType,
    ) -> HighVariableId {
        let id = self.declare_value(
            visiblity,
            module_path,
            HighValueDeclarationKind::Variable(HighVariableDeclaration { name, data_type }),
        );

        HighVariableId(id.0)
    }

    #[must_use]
    pub fn declare_function(&mut self, declaration: HighValueDeclaration) -> HighRegularFunctionId {
        let id = HighRegularFunctionId(self.values.len() as u32);

        self.values.push(declaration);

        id
    }

    #[must_use]
    pub fn get_type(&self, id: HighTypeId) -> (&[String], Visibility, &HighTypeDeclarationKind) {
        let HighTypeDeclaration {
            module_path,
            visibility,
            kind: declaration,
        } = &self.types[id.0 as usize];

        (module_path, *visibility, declaration)
    }

    #[must_use]
    pub fn get_struct(&self, id: HighStructId) -> (&[String], Visibility, &HighStructDeclaration) {
        let (module_path, visibility, HighTypeDeclarationKind::Struct(declaration)) =
            self.get_type(id.into())
        else {
            unreachable!();
        };

        (module_path, visibility, declaration)
    }

    #[must_use]
    pub fn get_struct_struct(
        &self,
        id: HighStructStructId,
    ) -> (&[String], Visibility, &HighStructStructDeclaration) {
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

    #[must_use]
    pub fn get_value(&self, id: HighValueId) -> (&[String], Visibility, &HighValueDeclarationKind) {
        let HighValueDeclaration {
            module_path,
            visibility,
            kind: declaration,
        } = &self.values[id.0 as usize];

        (module_path, *visibility, declaration)
    }

    #[must_use]
    pub fn get_variable(
        &self,
        id: HighVariableId,
    ) -> (&[String], Visibility, &HighVariableDeclaration) {
        let (module_path, visibility, HighValueDeclarationKind::Variable(declaration)) =
            self.get_value(id.into())
        else {
            unreachable!();
        };

        (module_path, visibility, declaration)
    }

    #[must_use]
    pub fn get_function(
        &self,
        id: HighFunctionId,
    ) -> (&[String], Visibility, &HighFunctionDeclaration) {
        let (module_path, visibility, HighValueDeclarationKind::Function(declaration)) =
            self.get_value(id.into())
        else {
            unreachable!();
        };

        (module_path, visibility, declaration)
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
}
