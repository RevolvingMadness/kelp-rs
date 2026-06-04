use std::{
    collections::HashMap,
    fmt::{Display, Write},
};

use crate::semantic::data_type::SemanticDataType;
use crate::{parsed::semantic_analysis::SemanticAnalysisContext, path::generic::TypedPath};

#[derive(Debug, Clone)]
pub enum ParsedDataType {
    Named(TypedPath<Self>),
    TypedCompound(HashMap<String, Self>),
    Reference(Box<Self>),
    Tuple(Vec<Self>),
    Unit,
    Never,
    Inferred,
}

impl Display for ParsedDataType {
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

impl ParsedDataType {
    #[must_use]
    pub fn perform_semantic_analysis(self, ctx: &mut SemanticAnalysisContext) -> SemanticDataType {
        match self {
            Self::Named(path) => {
                let path = path.perform_semantic_analysis(ctx);

                let Some((id, inherited_generic_spans, inherited_generic_types, last_segment)) =
                    ctx.get_visible_type_id(path)
                else {
                    return SemanticDataType::Error;
                };

                let declaration = ctx.parsed_environment.get_type(id).clone();

                declaration.into_data_type(
                    ctx,
                    id,
                    last_segment.name_span,
                    &inherited_generic_spans,
                    &inherited_generic_types,
                )
            }
            Self::Unit => SemanticDataType::Unit,
            Self::Never => SemanticDataType::Never,
            Self::Inferred => SemanticDataType::Inferred,
            Self::Tuple(data_types) => {
                let data_types = data_types
                    .into_iter()
                    .map(|data_type| data_type.perform_semantic_analysis(ctx))
                    .collect();

                SemanticDataType::Tuple(data_types)
            }
            Self::Reference(data_type) => {
                let data_type = data_type.perform_semantic_analysis(ctx);

                SemanticDataType::Reference(Box::new(data_type))
            }
            Self::TypedCompound(compound) => {
                let compound = compound
                    .into_iter()
                    .map(|(key, value)| {
                        let value = value.perform_semantic_analysis(ctx);

                        (key, value)
                    })
                    .collect();

                SemanticDataType::TypedCompound(compound)
            }
        }
    }
}
