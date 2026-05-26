use crate::parsed::semantic_analysis::SemanticAnalysisContext;
use crate::parsed::semantic_analysis::info::error::SemanticAnalysisError;
use crate::path::generic::GenericPathSegment;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::r#struct::HighStructId;
use crate::semantic::environment::r#type::{
    alias::SemanticTypeAliasDeclaration, builtin_data_type::SemanticBuiltinTypeDeclaration,
    module::SemanticModuleDeclaration, r#struct::SemanticStructDeclaration,
};
use crate::visibility::Visibility;

pub mod alias;
pub mod builtin_data_type;
pub mod module;
pub mod r#struct;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighGenericId(pub u32);

impl From<HighGenericId> for HighTypeId {
    fn from(value: HighGenericId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighTypeId(pub u32);

#[derive(Debug, Clone)]
pub enum SemanticTypeDeclarationKind {
    Module(SemanticModuleDeclaration),
    Struct(SemanticStructDeclaration),
    Alias(SemanticTypeAliasDeclaration),
    Generic(String),
    Builtin(SemanticBuiltinTypeDeclaration),
}

impl SemanticTypeDeclarationKind {
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

    pub fn into_data_type(
        self,
        ctx: &mut SemanticAnalysisContext,
        id: HighTypeId,
        segment: &GenericPathSegment<SemanticDataType>,
    ) -> SemanticDataType {
        match self {
            Self::Module(SemanticModuleDeclaration { name, .. }) => {
                ctx.add_error_type(segment.name_span, SemanticAnalysisError::NotAType(name))
            }
            Self::Struct(declaration) => {
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
            Self::Alias(declaration) => {
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

                declaration
                    .alias
                    .substitute_generics(&declaration.generic_ids, &segment.generic_types)
            }
            Self::Generic(name) => {
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
            Self::Builtin(data_type) => data_type.into_data_type(ctx, segment),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SemanticTypeDeclaration {
    pub module_path: Vec<String>,
    pub visibility: Visibility,
    pub kind: SemanticTypeDeclarationKind,
}
