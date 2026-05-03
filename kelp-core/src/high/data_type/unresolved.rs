use std::{
    collections::HashMap,
    fmt::{Display, Write},
};

use crate::{
    high::{
        data_type::resolved::GenericResolver, semantic_analysis::SemanticAnalysisContext,
        snbt_string::SNBTString,
    },
    low::data_type::DataType,
    path::generic::GenericPath,
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone)]
pub enum UnresolvedDataType {
    Named(GenericPath<Self>),
    TypedCompound(HashMap<SNBTString, Self>),
    Reference(Box<Self>),
    Tuple(Vec<Self>),
    Unit,
    Never,
    Inferred,
}

impl Display for UnresolvedDataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Named(path) => path.fmt(f),
            Self::TypedCompound(compound) => {
                f.write_str("{")?;

                if !compound.is_empty() {
                    f.write_str(" ")?;
                }

                for (i, (key, data_type)) in compound.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    write!(f, "{}: {}", key.snbt_string.1, data_type)?;
                }

                if !compound.is_empty() {
                    f.write_str(" ")?;
                }

                f.write_str("}")
            }
            Self::Reference(data_type) => write!(f, "&{}", data_type),
            Self::Tuple(data_types) => {
                f.write_char('(')?;

                for (i, data_type) in data_types.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    data_type.fmt(f)?;
                }

                f.write_char(')')?;

                Ok(())
            }
            Self::Unit => f.write_str("()"),
            Self::Never => f.write_char('!'),
            Self::Inferred => f.write_str("_"),
        }
    }
}

impl UnresolvedDataType {
    #[must_use]
    pub fn resolve_fully(self, ctx: &mut SemanticAnalysisContext) -> Option<DataType> {
        let partially_resolved = self.resolve_partially(None, ctx)?;

        Some(partially_resolved.resolve_fully(&GenericResolver::empty()))
    }

    #[must_use]
    pub fn resolve_partially(
        self,
        context_generic_names: Option<&[String]>,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<DataType> {
        Some(match self {
            Self::Named(path) => {
                if path.segments.len() == 1 && path.segments[0].generic_types.is_empty() {
                    let name = &path.segments[0].name;

                    if context_generic_names.is_some_and(|names| names.contains(name)) {
                        return Some(DataType::Generic(name.clone()));
                    }
                }

                let mut path = path.resolve_partially(context_generic_names, ctx)?;

                let id = ctx.get_visible_type_id(&path)?;
                let last_segment = path.segments.pop().unwrap();

                let declaration = ctx.get_type(id).clone();

                declaration.resolve_fully(
                    ctx,
                    id,
                    last_segment.generic_types,
                    last_segment.name_span,
                )?
            }
            Self::Unit => DataType::Unit,
            Self::Never => DataType::Never,
            Self::Inferred => DataType::Inferred,
            Self::Tuple(data_types) => {
                let data_types = data_types
                    .into_iter()
                    .map(|data_type| data_type.resolve_partially(context_generic_names, ctx))
                    .collect_option_all()?;

                DataType::Tuple(data_types)
            }
            Self::Reference(data_type) => {
                let data_type = data_type.resolve_partially(context_generic_names, ctx)?;

                DataType::Reference(Box::new(data_type))
            }
            Self::TypedCompound(compound) => {
                let compound = compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = value.resolve_partially(context_generic_names, ctx)?;

                        Some((key.snbt_string, value))
                    })
                    .collect_option_all()?;

                DataType::TypedCompound(compound)
            }
        })
    }
}
