use std::collections::HashMap;

use crate::{
    high::{
        data_type::resolved::{GenericResolver, PartiallyResolvedDataType},
        semantic_analysis_context::SemanticAnalysisContext,
        snbt_string::SNBTString,
    },
    middle::data_type::DataType as MiddleDataType,
    path::Path,
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone)]
pub enum UnresolvedDataType {
    Named(Path<Self>),
    TypedCompound(HashMap<SNBTString, Self>),
    Reference(Box<Self>),
    Tuple(Vec<Self>),
    Unit,
    Inferred,
}

impl UnresolvedDataType {
    #[must_use]
    pub fn resolve_fully(self, ctx: &mut SemanticAnalysisContext) -> Option<MiddleDataType> {
        let partially_resolved = self.resolve_partially(None, ctx)?;

        partially_resolved.resolve_fully(ctx, &GenericResolver::empty())
    }

    #[must_use]
    pub fn resolve_partially(
        self,
        context_generic_names: Option<&Vec<String>>,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<PartiallyResolvedDataType> {
        Some(match self {
            Self::Named(path) => {
                if path.segments.len() == 1 {
                    let name = &path.segments[0].name;

                    if context_generic_names.is_some_and(|names| names.contains(name)) {
                        return Some(PartiallyResolvedDataType::Generic(name.clone()));
                    }
                }

                let path = path.resolve_partially(context_generic_names, ctx)?;

                let (id, _, last_segment) = ctx.resolve_type_path(path)?;

                let declaration = ctx.get_type(id).clone();

                declaration.resolve_partially(
                    id,
                    last_segment.span,
                    last_segment.generic_types,
                    ctx,
                )?
            }
            Self::Unit => PartiallyResolvedDataType::Unit,
            Self::Inferred => PartiallyResolvedDataType::Inferred,
            Self::Tuple(data_types) => {
                let data_types = data_types
                    .into_iter()
                    .map(|data_type| data_type.resolve_partially(context_generic_names, ctx))
                    .collect_option_all()?;

                PartiallyResolvedDataType::Tuple(data_types)
            }
            Self::Reference(data_type) => {
                let data_type = data_type.resolve_partially(context_generic_names, ctx)?;

                PartiallyResolvedDataType::Reference(Box::new(data_type))
            }
            Self::TypedCompound(compound) => {
                let compound = compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = value.resolve_partially(context_generic_names, ctx)?;

                        Some((key.snbt_string, value))
                    })
                    .collect_option_all()?;

                PartiallyResolvedDataType::TypedCompound(compound)
            }
        })
    }
}
