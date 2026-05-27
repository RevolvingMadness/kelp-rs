use crate::parsed::environment::{
    r#type::alias::ParsedTypeAliasDeclaration,
    r#type::builtin_data_type::ParsedBuiltinTypeDeclaration,
    r#type::module::ParsedModuleDeclaration, r#type::r#struct::ParsedStructDeclaration,
};
use crate::path::generic::GenericPathSegment;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::HighGenericId;
use crate::semantic::environment::r#type::r#struct::HighStructId;
use crate::semantic::environment::{r#type::HighTypeId, value::HighValueId};
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
                if let Some(impls) = ctx.semantic_environment.implementations.get(&id) {
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

    pub fn into_data_type(
        self,
        ctx: &mut SemanticAnalysisContext,
        id: HighTypeId,
        segment: &GenericPathSegment<SemanticDataType>,
    ) -> SemanticDataType {
        match self.kind {
            ParsedTypeDeclarationKind::Module(ParsedModuleDeclaration { name, .. }) => {
                ctx.add_error_type(segment.name_span, SemanticAnalysisError::NotAType(name))
            }
            ParsedTypeDeclarationKind::Struct(declaration) => {
                let id = HighStructId(id.0);

                let expected_generics = declaration.generic_count();
                let actual_generics = segment.generic_types.len();

                if actual_generics != expected_generics {
                    return ctx.add_invalid_generics_type(
                        segment.name_span,
                        declaration.name(),
                        expected_generics,
                        actual_generics,
                    );
                }

                SemanticDataType::Struct(id, segment.generic_types.clone())
            }
            ParsedTypeDeclarationKind::Alias(declaration) => {
                let expected_generics = declaration.generic_ids.len();
                let actual_generics = segment.generic_types.len();

                if actual_generics != expected_generics {
                    return ctx.add_invalid_generics_type(
                        segment.name_span,
                        &declaration.name,
                        expected_generics,
                        actual_generics,
                    );
                }

                let alias = declaration.alias.perform_semantic_analysis(ctx);

                alias.substitute_generics(&declaration.generic_ids, &segment.generic_types)
            }
            ParsedTypeDeclarationKind::Generic(name) => {
                let expected_generics = 0;
                let actual_generics = segment.generic_types.len();

                if actual_generics != expected_generics {
                    return ctx.add_invalid_generics_type(
                        segment.name_span,
                        &name,
                        expected_generics,
                        actual_generics,
                    );
                }

                SemanticDataType::Generic(HighGenericId(id.0))
            }
            ParsedTypeDeclarationKind::Builtin(data_type) => data_type.into_data_type(ctx, segment),
        }
    }
}
