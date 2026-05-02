use crate::{
    high::environment::{
        r#type::{HighTypeDeclaration, HighTypeId},
        value::{
            HighValueDeclaration, HighValueDeclarationKind, HighValueId,
            function::{HighFunctionDeclaration, HighFunctionId},
            variable::{HighVariableDeclaration, HighVariableId},
        },
    },
    low::{data_type::DataType, expression::unresolved::UnresolvedExpression, pattern::Pattern},
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
        let id = HighTypeId(self.types.len());

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
        let id = HighValueId(self.values.len());

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
        f(self.values.get_mut(id.0).unwrap());
    }

    pub fn update_function(
        &mut self,
        id: HighFunctionId,
        new_parameters: Vec<(Pattern, DataType)>,
        new_body: UnresolvedExpression,
    ) {
        self.update_declaration(id.into(), |declaration| {
            let HighValueDeclaration {
                kind:
                    HighValueDeclarationKind::Function(HighFunctionDeclaration {
                        parameters: old_parameters,
                        body: old_body,
                        ..
                    }),
                ..
            } = declaration
            else {
                unreachable!("Value is not a function");
            };

            *old_parameters = new_parameters
                .into_iter()
                .map(|(pattern, data_type)| (Some(pattern), Some(data_type)))
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
        data_type: Option<DataType>,
    ) -> HighVariableId {
        let id = self.declare_value(
            visiblity,
            module_path,
            HighValueDeclarationKind::Variable(HighVariableDeclaration { name, data_type }),
        );

        HighVariableId(id.0)
    }

    #[must_use]
    pub fn declare_function(&mut self, declaration: HighValueDeclaration) -> HighFunctionId {
        let id = HighFunctionId(self.values.len());

        self.values.push(declaration);

        id
    }
}
