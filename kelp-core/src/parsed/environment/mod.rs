use crate::parsed::environment::r#type::ParsedTypeDeclaration;
use crate::parsed::environment::value::ParsedValueDeclaration;
use crate::semantic::environment::r#type::HighTypeId;
use crate::semantic::environment::value::HighValueId;

pub mod r#type;
pub mod value;

#[derive(Debug, Clone, Default)]
pub struct ParsedEnvironment {
    types: Vec<ParsedTypeDeclaration>,
    values: Vec<ParsedValueDeclaration>,
}

impl ParsedEnvironment {
    #[must_use]
    pub fn declare_type(&mut self, declaration: ParsedTypeDeclaration) -> HighTypeId {
        let id = HighTypeId(self.types.len() as u32);

        self.types.push(declaration);

        id
    }

    #[inline]
    #[must_use]
    pub fn get_type(&self, id: HighTypeId) -> &ParsedTypeDeclaration {
        &self.types[id.0 as usize]
    }

    #[must_use]
    pub fn declare_value(&mut self, declaration: ParsedValueDeclaration) -> HighValueId {
        let id = HighValueId(self.values.len() as u32);

        self.values.push(declaration);

        id
    }

    #[inline]
    #[must_use]
    pub fn get_value(&self, id: HighValueId) -> &ParsedValueDeclaration {
        &self.values[id.0 as usize]
    }
}