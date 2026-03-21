use std::collections::BTreeMap;

use crate::{
    high::{
        data_type::resolved::ResolvedDataType,
        semantic_analysis_context::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
        snbt_string::SNBTString,
    },
    middle::data_type::DataType as MiddleDataType,
    span::Span,
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnresolvedDataType {
    Named(Span, String, Vec<Self>),
    TypedCompound(BTreeMap<SNBTString, Self>),
    Reference(Box<Self>),
    Tuple(Vec<Self>),
    Unit,
    Inferred,
}

impl UnresolvedDataType {
    #[must_use]
    pub fn resolve_fully(self, ctx: &mut SemanticAnalysisContext) -> Option<MiddleDataType> {
        Some(match self {
            Self::Named(name_span, name, generic_types) => {
                let generic_types = generic_types
                    .into_iter()
                    .map(|generic_type| generic_type.resolve_fully(ctx))
                    .collect_option_all::<Vec<_>>()?;

                let Some(id) = ctx.get_type_id(&name) else {
                    return ctx.add_error(name_span, SemanticAnalysisError::UnknownType(name));
                };

                let declaration = ctx.get_type(id).clone();

                let expected_generic_count = declaration.generic_count();
                let actual_generic_count = generic_types.len();

                if actual_generic_count != expected_generic_count {
                    let name = declaration.name().to_owned();

                    return ctx.add_invalid_generics(
                        name_span,
                        name,
                        expected_generic_count,
                        actual_generic_count,
                    );
                }

                return declaration.resolve_fully(ctx, id, generic_types, name_span);
            }
            Self::Unit => MiddleDataType::Unit,
            Self::Inferred => MiddleDataType::Inferred,
            Self::Tuple(data_types) => {
                let data_types = data_types
                    .into_iter()
                    .map(|data_type| data_type.resolve_fully(ctx))
                    .collect_option_all()?;

                MiddleDataType::Tuple(data_types)
            }
            Self::Reference(data_type) => {
                let data_type = data_type.resolve_fully(ctx)?;

                MiddleDataType::Reference(Box::new(data_type))
            }
            Self::TypedCompound(compound) => {
                let compound = compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = value.resolve_fully(ctx)?;

                        Some((key.snbt_string, value))
                    })
                    .collect_option_all()?;

                MiddleDataType::TypedCompound(compound)
            }
        })
    }

    #[must_use]
    pub fn resolve_generics(
        self,
        context_generic_names: Option<&Vec<String>>,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<ResolvedDataType> {
        Some(match self {
            Self::Named(name_span, name, generic_types) => {
                let Some(id) = ctx.get_type_id(&name) else {
                    if context_generic_names
                        .is_none_or(|context_generic_names| !context_generic_names.contains(&name))
                    {
                        return ctx.add_error(name_span, SemanticAnalysisError::UnknownType(name));
                    }

                    return Some(ResolvedDataType::Generic(name));
                };

                let generic_types = generic_types
                    .into_iter()
                    .map(|generic_type| generic_type.resolve_generics(context_generic_names, ctx))
                    .collect_option_all()?;

                let declaration = ctx.get_type(id).clone();

                declaration.resolve_generics(id, name_span, generic_types, ctx)?
            }
            Self::Unit => ResolvedDataType::Unit,
            Self::Inferred => ResolvedDataType::Inferred,
            Self::Tuple(data_types) => {
                let data_types = data_types
                    .into_iter()
                    .map(|data_type| data_type.resolve_generics(context_generic_names, ctx))
                    .collect_option_all()?;

                ResolvedDataType::Tuple(data_types)
            }
            Self::Reference(data_type) => {
                let data_type = data_type.resolve_generics(context_generic_names, ctx)?;

                ResolvedDataType::Reference(Box::new(data_type))
            }
            Self::TypedCompound(compound) => {
                let compound = compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = value.resolve_generics(context_generic_names, ctx)?;

                        Some((key.snbt_string, value))
                    })
                    .collect_option_all()?;

                ResolvedDataType::TypedCompound(compound)
            }
        })
    }
}
