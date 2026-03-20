use crate::{
    builtin_data_type::BuiltinDataType,
    high::{
        data_type::resolved::ResolvedDataType,
        environment::r#type::{alias::HighAliasDeclaration, r#struct::HighStructDeclaration},
        semantic_analysis_context::SemanticAnalysisContext,
    },
    middle::{data_type::DataType, environment::r#type::r#struct::StructId},
    span::Span,
};

pub mod alias;
pub mod r#struct;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighTypeId(pub usize);

#[derive(Debug, Clone)]
pub enum HighTypeDeclaration {
    Struct(HighStructDeclaration),
    Alias(HighAliasDeclaration),
    Builtin(BuiltinDataType),
}

impl HighTypeDeclaration {
    #[must_use]
    pub fn name(&self) -> &str {
        match self {
            Self::Struct(declaration) => &declaration.name,
            Self::Alias(declaration) => &declaration.name,
            Self::Builtin(data_type) => data_type.name(),
        }
    }

    #[must_use]
    pub const fn generic_count(&self) -> usize {
        match self {
            Self::Struct(declaration) => declaration.generic_names.len(),
            Self::Alias(declaration) => declaration.generic_names.len(),
            Self::Builtin(builtin_type) => builtin_type.generic_count(),
        }
    }

    #[must_use]
    pub fn resolve_generics(
        self,
        id: HighTypeId,
        name_span: Span,
        generic_types: Vec<ResolvedDataType>,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<ResolvedDataType> {
        let expected_generic_count = self.generic_count();
        let actual_generic_count = generic_types.len();

        if actual_generic_count != expected_generic_count {
            let name = self.name().to_owned();

            return ctx.add_invalid_generics(
                name_span,
                name,
                expected_generic_count,
                actual_generic_count,
            );
        }

        match self {
            Self::Struct(_) => Some(ResolvedDataType::Struct(name_span, id, generic_types)),
            Self::Alias(declaration) => declaration.alias,
            Self::Builtin(data_type) => data_type.to_resolved_data_type(generic_types),
        }
    }

    #[must_use]
    pub fn as_monomorphized_struct_id(
        self,
        id: HighTypeId,
        name_span: Span,
        generic_types: Vec<DataType>,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<Option<StructId>> {
        let expected_generic_count = self.generic_count();
        let actual_generic_count = generic_types.len();

        if actual_generic_count != expected_generic_count {
            let name = self.name().to_owned();

            return ctx.add_invalid_generics(
                name_span,
                name,
                expected_generic_count,
                actual_generic_count,
            );
        }

        Some(match self {
            Self::Struct(declaration) => {
                ctx.get_monomorphized_struct_id(id, declaration, generic_types, name_span)
            }
            Self::Alias(declaration) => {
                let alias = declaration.alias.as_ref()?.clone();

                let generic_mapping = declaration
                    .generic_names
                    .into_iter()
                    .zip(generic_types.iter().cloned())
                    .collect();

                let DataType::Struct(id) = alias.lower(ctx, &generic_mapping)? else {
                    return None;
                };

                Some(id)
            }
            Self::Builtin(_) => None,
        })
    }
}
