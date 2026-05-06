use crate::{
    high::{
        environment::r#type::{
            alias::HighAliasDeclaration,
            builtin_data_type::HighBuiltinTypeDeclaration,
            module::HighModuleDeclaration,
            r#struct::{
                HighStructDeclaration, HighStructId, regular::HighStructStructId,
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
pub struct HighTypeId(pub usize);

impl From<HighStructId> for HighTypeId {
    fn from(value: HighStructId) -> Self {
        Self(value.0)
    }
}

impl From<HighStructStructId> for HighTypeId {
    fn from(value: HighStructStructId) -> Self {
        Self(value.0)
    }
}

impl From<HighTupleStructId> for HighTypeId {
    fn from(value: HighTupleStructId) -> Self {
        Self(value.0)
    }
}

#[derive(Debug, Clone)]
pub enum HighTypeDeclarationKind {
    Module(HighModuleDeclaration),
    Struct(HighStructDeclaration),
    Alias(HighAliasDeclaration),
    Generic(String),
    Builtin(HighBuiltinTypeDeclaration),
}

impl HighTypeDeclarationKind {
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

    #[must_use]
    pub const fn generic_count(&self) -> Option<usize> {
        Some(match self {
            Self::Module(..) => return None,
            Self::Struct(declaration) => declaration.generic_count(),
            Self::Alias(declaration) => declaration.generic_names.len(),
            Self::Generic(..) => 0,
            Self::Builtin(builtin_type) => builtin_type.generic_count,
        })
    }
}

#[derive(Debug, Clone)]
pub struct HighTypeDeclaration {
    pub module_path: Vec<String>,
    pub visibility: Visibility,
    pub kind: HighTypeDeclarationKind,
}

impl HighTypeDeclaration {
    #[inline]
    #[must_use]
    pub fn as_tuple_owned(self) -> (Visibility, Vec<String>, HighTypeDeclarationKind) {
        (self.visibility, self.module_path, self.kind)
    }

    pub fn resolve_partially(
        self,
        ctx: &mut SemanticAnalysisContext,
        id: HighTypeId,
        generic_spans: Vec<Span>,
        generic_types: Vec<UnresolvedDataType>,
        path_span: Span,
    ) -> UnresolvedDataType {
        match self.kind {
            HighTypeDeclarationKind::Module(..) => ctx.add_error_type(
                path_span,
                SemanticAnalysisError::NotAType(self.kind.name().to_owned()),
            ),
            HighTypeDeclarationKind::Struct(declaration) => {
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
            HighTypeDeclarationKind::Alias(declaration) => {
                let expected_generics = declaration.generic_names.len();
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
                    .substitute_generics(&declaration.generic_names, &generic_types)
            }
            HighTypeDeclarationKind::Generic(name) => UnresolvedDataType::Generic(name),
            HighTypeDeclarationKind::Builtin(data_type) => data_type
                .to_data_type_semantic_analysis(ctx, path_span, generic_spans, generic_types),
        }
    }
}
