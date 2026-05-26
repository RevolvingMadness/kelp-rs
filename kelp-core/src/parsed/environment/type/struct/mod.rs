use crate::parsed::environment::r#type::r#struct::{
    regular::ParsedRegularStructDeclaration, tuple::ParsedTupleStructDeclaration,
};
use crate::parsed::semantic_analysis::{
    SemanticAnalysisContext, info::error::SemanticAnalysisError,
};
use crate::semantic::environment::{
    r#type::{HighTypeId, r#struct::ResolvedStructDeclaration},
    value::HighValueId,
};

pub mod regular;
pub mod tuple;

#[derive(Debug, Clone)]
pub enum ParsedStructDeclaration {
    Struct(ParsedRegularStructDeclaration),
    Tuple(ParsedTupleStructDeclaration),
}

impl From<ResolvedStructDeclaration> for ParsedStructDeclaration {
    fn from(value: ResolvedStructDeclaration) -> Self {
        match value {
            ResolvedStructDeclaration::Struct(declaration) => Self::Struct(declaration.into()),
            ResolvedStructDeclaration::Tuple(declaration) => Self::Tuple(declaration.into()),
        }
    }
}

impl ParsedStructDeclaration {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Struct(declaration) => &declaration.name,
            Self::Tuple(declaration) => &declaration.name,
        }
    }

    pub fn get_value_id_semantic_analysis(
        &self,
        ctx: &SemanticAnalysisContext,
        id: HighTypeId,
        name: &str,
    ) -> Result<HighValueId, SemanticAnalysisError> {
        if let Some(impls) = ctx.resolved_environment.impls.get(&id) {
            for implementation in impls {
                if let Some(id) = implementation.values.get(name) {
                    return Ok(*id);
                }
            }
        }

        Err(SemanticAnalysisError::TypeDoesntContainValue {
            type_name: self.name().to_owned(),
            value_name: name.to_owned(),
        })
    }
}
