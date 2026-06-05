use crate::parsed::environment::r#type::generic::ParsedGenericDeclaration;
use crate::parsed::environment::{
    r#type::alias::ParsedTypeAliasDeclaration,
    r#type::builtin_data_type::ParsedBuiltinTypeDeclaration,
    r#type::module::ParsedModuleDeclaration, r#type::r#struct::ParsedStructDeclaration,
};
use crate::parsed::semantic_analysis::info::error::ItemKind;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::module::HighModuleId;
use crate::semantic::environment::r#type::{HighGenericId, HighVisibleTypeId};
use crate::span::Span;
use crate::{
    parsed::semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    visibility::Visibility,
};

pub mod alias;
pub mod builtin_data_type;
pub mod generic;
pub mod module;
pub mod r#struct;

#[derive(Debug, Clone)]
pub enum ParsedTypeDeclarationKind {
    Module(ParsedModuleDeclaration),
    Struct(ParsedStructDeclaration),
    Alias(ParsedTypeAliasDeclaration),
    Generic(ParsedGenericDeclaration),
    Builtin(ParsedBuiltinTypeDeclaration),
}

impl ParsedTypeDeclarationKind {
    #[must_use]
    pub const fn name_span(&self) -> Option<Span> {
        Some(match self {
            Self::Module(declaration) => return declaration.name_span,
            Self::Struct(declaration) => declaration.name_span(),
            Self::Alias(declaration) => declaration.name_span,
            Self::Generic(declaration) => declaration.name_span,
            Self::Builtin(..) => return None,
        })
    }

    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Module(declaration) => &declaration.name,
            Self::Struct(declaration) => declaration.name(),
            Self::Alias(declaration) => &declaration.name,
            Self::Generic(declaration) => &declaration.name,
            Self::Builtin(data_type) => &data_type.name,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParsedTypeDeclaration {
    pub module_path: Vec<HighModuleId>,
    pub visibility: Visibility,
    pub kind: ParsedTypeDeclarationKind,
}

impl ParsedTypeDeclaration {
    #[must_use]
    pub fn into_data_type(
        self,
        ctx: &mut SemanticAnalysisContext,
        id: HighVisibleTypeId,
        name_span: Span,
        generic_spans: &[Span],
        generic_types: &[SemanticDataType],
    ) -> SemanticDataType {
        match self.kind {
            ParsedTypeDeclarationKind::Module(ParsedModuleDeclaration { name, .. }) => ctx
                .add_error_type(SemanticAnalysisError::NotAType {
                    type_span: name_span,
                    type_name: name,
                }),
            ParsedTypeDeclarationKind::Struct(declaration) => {
                declaration.into_data_type(ctx, id, name_span, generic_types)
            }
            ParsedTypeDeclarationKind::Alias(declaration) => {
                declaration.into_data_type(ctx, name_span, generic_types)
            }
            ParsedTypeDeclarationKind::Generic(declaration) => {
                let expected_generics = 0;
                let actual_generics = generic_types.len();

                if actual_generics != expected_generics {
                    return ctx.add_invalid_generics_type(
                        name_span,
                        Some(declaration.name_span),
                        expected_generics,
                        actual_generics,
                    );
                }

                SemanticDataType::Generic(HighGenericId(id.0))
            }
            ParsedTypeDeclarationKind::Builtin(declaration) => {
                declaration.into_data_type(ctx, name_span, generic_spans, generic_types)
            }
        }
    }
}
