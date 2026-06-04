use crate::{
    parsed::environment::{
        ParsedEnvironment,
        r#type::{
            ParsedTypeDeclaration, ParsedTypeDeclarationKind, module::ParsedModuleDeclaration,
        },
    },
    semantic::environment::{
        SemanticEnvironment,
        r#type::{HighTypeId, module::HighModuleId},
        value::HighValueId,
    },
    span::Span,
};
use std::collections::HashMap;

#[derive(Debug, Default, Clone)]
pub struct Scope {
    types: HashMap<String, HighTypeId>,
    values: HashMap<String, HighValueId>,
}

impl Scope {
    #[inline]
    #[must_use]
    pub fn into_tuple(self) -> (HashMap<String, HighTypeId>, HashMap<String, HighValueId>) {
        (self.types, self.values)
    }

    #[inline]
    pub fn declare_type(&mut self, name: String, id: HighTypeId) {
        self.types.insert(name, id);
    }

    #[inline]
    #[must_use]
    pub fn type_is_declared(&self, name: &str) -> bool {
        self.types.contains_key(name)
    }

    #[must_use]
    pub fn get_type_id(&self, name: &str) -> Option<HighTypeId> {
        self.types.get(name).copied()
    }

    #[inline]
    #[must_use]
    pub fn get_type_declaration_span(
        &self,
        environment: &ParsedEnvironment,
        name: &str,
    ) -> Option<Span> {
        let id = self.get_type_id(name)?;

        let declaration = environment.get_type(id);

        declaration.kind.name_span()
    }

    #[inline]
    pub fn declare_value(&mut self, name: String, id: HighValueId) {
        self.values.insert(name, id);
    }

    #[inline]
    #[must_use]
    pub fn value_is_declared(&self, name: &str) -> bool {
        self.values.contains_key(name)
    }

    #[must_use]
    pub fn get_value_id(&self, name: &str) -> Option<HighValueId> {
        self.values.get(name).copied()
    }

    #[inline]
    #[must_use]
    pub fn get_value_declaration_span(
        &self,
        environment: &SemanticEnvironment,
        name: &str,
    ) -> Option<Span> {
        let id = self.get_value_id(name)?;

        let declaration = environment.get_value(id);

        declaration.kind.name_span()
    }

    #[inline]
    #[must_use]
    pub fn into_module_declaration(
        self,
        environment: &ParsedEnvironment,
        id: HighModuleId,
    ) -> ParsedModuleDeclaration {
        let ParsedTypeDeclaration {
            kind: ParsedTypeDeclarationKind::Module(declaration),
            ..
        } = environment.get_type(id)
        else {
            unreachable!();
        };

        ParsedModuleDeclaration {
            name_span: declaration.name_span,
            name: declaration.name.clone(),
            types: self.types,
            values: self.values,
        }
    }
}
