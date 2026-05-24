use crate::{
    high::{
        environment::{
            resolved::{
                r#type::{
                    HighGenericId, HighTypeId, ResolvedTypeDeclaration, ResolvedTypeDeclarationKind,
                },
                value::HighValueId,
            },
            unresolved::r#type::{
                alias::UnresolvedTypeAliasDeclaration,
                builtin_data_type::UnresolvedBuiltinTypeDeclaration,
                module::UnresolvedModuleDeclaration, r#struct::UnresolvedStructDeclaration,
            },
        },
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    },
    visibility::Visibility,
};

pub mod alias;
pub mod builtin_data_type;
pub mod module;
pub mod r#struct;

#[derive(Debug, Clone)]
pub enum UnresolvedTypeDeclarationKind {
    Module(UnresolvedModuleDeclaration),
    Struct(UnresolvedStructDeclaration),
    Alias(UnresolvedTypeAliasDeclaration),
    Generic(String),
    Builtin(UnresolvedBuiltinTypeDeclaration),
}

impl From<ResolvedTypeDeclarationKind> for UnresolvedTypeDeclarationKind {
    fn from(value: ResolvedTypeDeclarationKind) -> Self {
        match value {
            ResolvedTypeDeclarationKind::Module(declaration) => Self::Module(declaration.into()),
            ResolvedTypeDeclarationKind::Struct(declaration) => Self::Struct(declaration.into()),
            ResolvedTypeDeclarationKind::Alias(declaration) => Self::Alias(declaration.into()),
            ResolvedTypeDeclarationKind::Generic(name) => Self::Generic(name),
            ResolvedTypeDeclarationKind::Builtin(declaration) => Self::Builtin(declaration.into()),
        }
    }
}

impl UnresolvedTypeDeclarationKind {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Module(declaration) => &declaration.name,
            Self::Struct(declaration) => declaration.name(),
            Self::Alias(declaration) => &declaration.name,
            Self::Generic(name) => name,
            Self::Builtin(data_type) => &data_type.name,
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnresolvedTypeDeclaration {
    pub module_path: Vec<String>,
    pub visibility: Visibility,
    pub kind: UnresolvedTypeDeclarationKind,
}

impl From<ResolvedTypeDeclaration> for UnresolvedTypeDeclaration {
    fn from(value: ResolvedTypeDeclaration) -> Self {
        Self {
            module_path: value.module_path,
            visibility: value.visibility,
            kind: value.kind.into(),
        }
    }
}

impl UnresolvedTypeDeclaration {
    #[must_use]
    pub fn is_visible(&self, current_module_path: &[String]) -> bool {
        if matches!(self.visibility, Visibility::Public) {
            return true;
        }

        current_module_path.starts_with(&self.module_path)
    }

    pub fn get_visible_type_id(
        &self,
        ctx: &SemanticAnalysisContext,
        id: HighTypeId,
        name: &str,
    ) -> Result<HighTypeId, SemanticAnalysisError> {
        match &self.kind {
            UnresolvedTypeDeclarationKind::Module(declaration) => {
                declaration.get_type_id_semantic_analysis(name)
            }
            UnresolvedTypeDeclarationKind::Struct(declaration) => {
                if let Some(impls) = ctx.resolved_environment.impls.get(&id) {
                    for implementation in impls {
                        if let Some(id) = implementation.types.get(name) {
                            return Ok(*id);
                        }
                    }
                }

                Err(SemanticAnalysisError::TypeDoesntContainType {
                    container_type_name: declaration.name().to_owned(),
                    type_name: name.to_owned(),
                })
            }
            _ => Err(SemanticAnalysisError::TypeDoesntContainItems {
                type_name: self.kind.name().to_owned(),
            }),
        }
    }

    pub fn get_visible_value_id(
        &self,
        ctx: &SemanticAnalysisContext,
        id: HighTypeId,
        name: &str,
    ) -> Result<HighValueId, SemanticAnalysisError> {
        match &self.kind {
            UnresolvedTypeDeclarationKind::Module(declaration) => {
                declaration.get_value_id_semantic_analysis(name)
            }
            UnresolvedTypeDeclarationKind::Struct(declaration) => {
                declaration.get_value_id_semantic_analysis(ctx, id, name)
            }
            _ => Err(SemanticAnalysisError::TypeDoesntContainItems {
                type_name: self.kind.name().to_owned(),
            }),
        }
    }
}
