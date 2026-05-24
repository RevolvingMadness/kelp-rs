use crate::{
    high::{
        environment::resolved::r#type::{
            alias::ResolvedTypeAliasDeclaration,
            builtin_data_type::ResolvedBuiltinTypeDeclaration,
            module::{HighModuleId, ResolvedModuleDeclaration},
            r#struct::{
                HighStructId, ResolvedStructDeclaration, regular::HighRegularStructId,
                tuple::HighTupleStructId,
            },
        },
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    },
    low::data_type::unresolved::UnresolvedDataType,
    span::Span,
    visibility::Visibility,
};

pub mod alias;
pub mod builtin_data_type;
pub mod module;
pub mod r#struct;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighGenericId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighTypeId(pub u32);

impl From<HighModuleId> for HighTypeId {
    fn from(value: HighModuleId) -> Self {
        Self(value.0)
    }
}

impl From<HighStructId> for HighTypeId {
    fn from(value: HighStructId) -> Self {
        Self(value.0)
    }
}

impl From<HighRegularStructId> for HighTypeId {
    fn from(value: HighRegularStructId) -> Self {
        Self(value.0)
    }
}

impl From<HighTupleStructId> for HighTypeId {
    fn from(value: HighTupleStructId) -> Self {
        Self(value.0)
    }
}

impl From<HighGenericId> for HighTypeId {
    fn from(value: HighGenericId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub enum ResolvedTypeDeclarationKind {
    Module(ResolvedModuleDeclaration),
    Struct(ResolvedStructDeclaration),
    Alias(ResolvedTypeAliasDeclaration),
    Generic(String),
    Builtin(ResolvedBuiltinTypeDeclaration),
}

impl ResolvedTypeDeclarationKind {
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
pub struct ResolvedTypeDeclaration {
    pub module_path: Vec<String>,
    pub visibility: Visibility,
    pub kind: ResolvedTypeDeclarationKind,
}

impl ResolvedTypeDeclaration {
    pub fn resolve_partially(
        self,
        ctx: &mut SemanticAnalysisContext,
        id: HighTypeId,
        generic_spans: Vec<Span>,
        generic_types: Vec<UnresolvedDataType>,
        path_span: Span,
    ) -> UnresolvedDataType {
        match self.kind {
            ResolvedTypeDeclarationKind::Module(ResolvedModuleDeclaration { name, .. }) => {
                ctx.add_error_type(path_span, SemanticAnalysisError::NotAType(name))
            }
            ResolvedTypeDeclarationKind::Struct(declaration) => {
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

                UnresolvedDataType::Struct(id, generic_types)
            }
            ResolvedTypeDeclarationKind::Alias(declaration) => {
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
            ResolvedTypeDeclarationKind::Generic(name) => {
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

                UnresolvedDataType::Generic(HighGenericId(id.0))
            }
            ResolvedTypeDeclarationKind::Builtin(data_type) => data_type
                .to_data_type_semantic_analysis(ctx, path_span, generic_spans, generic_types),
        }
    }
}
