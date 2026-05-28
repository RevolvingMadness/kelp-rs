use crate::parsed::environment::{
    r#type::alias::ParsedTypeAliasDeclaration,
    r#type::builtin_data_type::ParsedBuiltinTypeDeclaration,
    r#type::module::ParsedModuleDeclaration, r#type::r#struct::ParsedStructDeclaration,
};
use crate::path::generic::GenericPathSegment;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::HighGenericId;
use crate::semantic::environment::r#type::HighTypeId;
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
                declaration.into_data_type(ctx, id, segment)
            }
            ParsedTypeDeclarationKind::Alias(declaration) => {
                declaration.into_data_type(ctx, segment)
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
            ParsedTypeDeclarationKind::Builtin(declaration) => {
                declaration.into_data_type(ctx, segment)
            }
        }
    }
}
