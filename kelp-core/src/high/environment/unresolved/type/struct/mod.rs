use crate::high::{
    environment::{
        resolved::{
            r#type::{HighTypeId, r#struct::ResolvedStructDeclaration},
            value::HighValueId,
        },
        unresolved::r#type::r#struct::{
            regular::UnresolvedRegularStructDeclaration, tuple::UnresolvedTupleStructDeclaration,
        },
    },
    semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
};

pub mod regular;
pub mod tuple;

#[derive(Debug, Clone)]
pub enum UnresolvedStructDeclaration {
    Struct(UnresolvedRegularStructDeclaration),
    Tuple(UnresolvedTupleStructDeclaration),
}

impl From<ResolvedStructDeclaration> for UnresolvedStructDeclaration {
    fn from(value: ResolvedStructDeclaration) -> Self {
        match value {
            ResolvedStructDeclaration::Struct(declaration) => Self::Struct(declaration.into()),
            ResolvedStructDeclaration::Tuple(declaration) => Self::Tuple(declaration.into()),
        }
    }
}

impl UnresolvedStructDeclaration {
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
