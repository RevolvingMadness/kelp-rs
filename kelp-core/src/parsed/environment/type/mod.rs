use crate::parsed::environment::{
    r#type::alias::ParsedTypeAliasDeclaration,
    r#type::builtin_data_type::ParsedBuiltinTypeDeclaration,
    r#type::module::ParsedModuleDeclaration, r#type::r#struct::ParsedStructDeclaration,
};
use crate::semantic::environment::{
    r#type::{HighTypeId, SemanticTypeDeclaration, SemanticTypeDeclarationKind},
    value::HighValueId,
};
use crate::{
    parsed::semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    visibility::Visibility,
};

pub mod alias;
pub mod builtin_data_type;
pub mod module;
pub mod r#struct;

#[derive(Debug, Clone)]
pub enum ParsedTypeDeclarationKind {
    Module(ParsedModuleDeclaration),
    Struct(ParsedStructDeclaration),
    Alias(ParsedTypeAliasDeclaration),
    Generic(String),
    Builtin(ParsedBuiltinTypeDeclaration),
}

impl From<SemanticTypeDeclarationKind> for ParsedTypeDeclarationKind {
    fn from(value: SemanticTypeDeclarationKind) -> Self {
        match value {
            SemanticTypeDeclarationKind::Module(declaration) => Self::Module(declaration.into()),
            SemanticTypeDeclarationKind::Struct(declaration) => Self::Struct(declaration.into()),
            SemanticTypeDeclarationKind::Alias(declaration) => Self::Alias(declaration.into()),
            SemanticTypeDeclarationKind::Generic(name) => Self::Generic(name),
            SemanticTypeDeclarationKind::Builtin(declaration) => Self::Builtin(declaration.into()),
        }
    }
}

impl ParsedTypeDeclarationKind {
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
pub struct ParsedTypeDeclaration {
    pub module_path: Vec<String>,
    pub visibility: Visibility,
    pub kind: ParsedTypeDeclarationKind,
}

impl From<SemanticTypeDeclaration> for ParsedTypeDeclaration {
    fn from(value: SemanticTypeDeclaration) -> Self {
        Self {
            module_path: value.module_path,
            visibility: value.visibility,
            kind: value.kind.into(),
        }
    }
}

impl ParsedTypeDeclaration {
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
            ParsedTypeDeclarationKind::Module(declaration) => {
                declaration.get_type_id_semantic_analysis(name)
            }
            ParsedTypeDeclarationKind::Struct(declaration) => {
                if let Some(impls) = ctx.semantic_environment.impls.get(&id) {
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
            ParsedTypeDeclarationKind::Module(declaration) => {
                declaration.get_value_id_semantic_analysis(name)
            }
            ParsedTypeDeclarationKind::Struct(declaration) => {
                declaration.get_value_id_semantic_analysis(ctx, id, name)
            }
            _ => Err(SemanticAnalysisError::TypeDoesntContainItems {
                type_name: self.kind.name().to_owned(),
            }),
        }
    }
}
