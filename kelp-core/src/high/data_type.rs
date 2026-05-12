use std::{
    collections::HashMap,
    fmt::{Display, Write},
};

use crate::{
    high::semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    low::data_type::unresolved::UnresolvedDataType,
    path::generic::GenericPath,
};

#[derive(Debug, Clone)]
pub enum DataType {
    Named(GenericPath<Self>),
    TypedCompound(HashMap<String, Self>),
    Reference(Box<Self>),
    Tuple(Vec<Self>),
    Unit,
    Never,
    Inferred,
}

impl Display for DataType {
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

                    write!(f, "{}: {}", key, data_type)?;
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

impl DataType {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> UnresolvedDataType {
        match self {
            Self::Named(path) => {
                let mut path = path.perform_semantic_analysis(ctx);

                let Some(id) = ctx.get_visible_type_id(&path) else {
                    return UnresolvedDataType::Error;
                };

                let last_segment = path.segments.pop().unwrap();

                let declaration = ctx.get_type(id).clone();

                declaration.resolve_partially(
                    ctx,
                    id,
                    last_segment.generic_spans,
                    last_segment.generic_types,
                    last_segment.name_span,
                )
            }
            Self::Unit => UnresolvedDataType::Unit,
            Self::Never => UnresolvedDataType::Never,
            Self::Inferred => UnresolvedDataType::Inferred,
            Self::Tuple(data_types) => {
                let data_types = data_types
                    .into_iter()
                    .map(|data_type| data_type.perform_semantic_analysis(ctx))
                    .collect();

                UnresolvedDataType::Tuple(data_types)
            }
            Self::Reference(data_type) => {
                let data_type = data_type.perform_semantic_analysis(ctx);

                UnresolvedDataType::Reference(Box::new(data_type))
            }
            Self::TypedCompound(compound) => {
                let compound = compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = value.perform_semantic_analysis(ctx);

                        (key, value)
                    })
                    .collect();

                UnresolvedDataType::TypedCompound(compound)
            }
        }
    }
}
