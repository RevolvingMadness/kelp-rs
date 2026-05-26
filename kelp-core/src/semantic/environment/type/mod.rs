use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::{
    alias::SemanticTypeAliasDeclaration,
    builtin_data_type::SemanticBuiltinTypeDeclaration,
    module::SemanticModuleDeclaration,
    r#struct::{HighStructId, SemanticStructDeclaration},
};
use crate::{
    parsed::semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    span::Span,
    visibility::Visibility,
};

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
}

#[derive(Debug, Clone)]
pub struct SemanticTypeDeclaration {
    pub module_path: Vec<String>,
    pub visibility: Visibility,
    pub kind: SemanticTypeDeclarationKind,
}

impl SemanticTypeDeclaration {
    pub fn resolve_partially(
        self,
        ctx: &mut SemanticAnalysisContext,
        id: HighTypeId,
        generic_spans: Vec<Span>,
        generic_types: Vec<SemanticDataType>,
        path_span: Span,
    ) -> SemanticDataType {
        match self.kind {
            SemanticTypeDeclarationKind::Module(SemanticModuleDeclaration { name, .. }) => {
                ctx.add_error_type(path_span, SemanticAnalysisError::NotAType(name))
            }
            SemanticTypeDeclarationKind::Struct(declaration) => {
                let id = HighStructId(id.0);

                let expected_generics = declaration.generic_count();
                let actual_generics = generic_types.len();

                if actual_generics != expected_generics {
                    return ctx.add_invalid_generics_type(
                        path_span,
                        declaration.name(),
                        expected_generics,
                        actual_generics,
                    );
                }

                SemanticDataType::Struct(id, generic_types)
            }
            SemanticTypeDeclarationKind::Alias(declaration) => {
                let expected_generics = declaration.generic_ids.len();
                let actual_generics = generic_types.len();

                if actual_generics != expected_generics {
                    return ctx.add_invalid_generics_type(
                        path_span,
                        &declaration.name,
                        expected_generics,
                        actual_generics,
                    );
                }

                declaration
                    .alias
                    .substitute_generics(&declaration.generic_ids, &generic_types)
            }
            SemanticTypeDeclarationKind::Generic(name) => {
                let expected_generics = 0;
                let actual_generics = generic_types.len();

                if actual_generics != expected_generics {
                    return ctx.add_invalid_generics_type(
                        path_span,
                        &name,
                        expected_generics,
                        actual_generics,
                    );
                }

                SemanticDataType::Generic(HighGenericId(id.0))
            }
            SemanticTypeDeclarationKind::Builtin(data_type) => data_type
                .to_data_type_semantic_analysis(ctx, path_span, generic_spans, generic_types),
        }
    }
}
