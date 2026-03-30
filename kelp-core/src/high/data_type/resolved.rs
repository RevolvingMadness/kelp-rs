use std::collections::HashMap;

use minecraft_command_types::snbt::SNBTString;

use crate::{
    high::{environment::r#type::HighTypeId, semantic_analysis::SemanticAnalysisContext},
    low::data_type::DataType,
    span::Span,
    trait_ext::CollectOptionAllIterExt,
};

pub struct GenericResolver<'a> {
    names: &'a [String],
    types: &'a [DataType],
}

impl<'a> GenericResolver<'a> {
    pub fn create_semantic_analysis(
        ctx: &mut SemanticAnalysisContext,
        name: &str,
        name_span: Span,
        names: &'a [String],
        types: &'a [DataType],
    ) -> Option<Self> {
        let expected_generics = names.len();
        let actual_generics = types.len();

        if actual_generics != expected_generics {
            return ctx.add_invalid_generics(
                name_span,
                name.to_owned(),
                expected_generics,
                actual_generics,
            );
        }

        Some(Self { names, types })
    }

    #[inline]
    #[must_use]
    pub const fn empty() -> Self {
        Self {
            names: &[],
            types: &[],
        }
    }

    #[must_use]
    pub fn resolve(&self, name: &str) -> Option<&DataType> {
        let index = self.names.iter().position(|n| n == name)?;

        self.types.get(index)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PartiallyResolvedDataType {
    Boolean,
    Byte,
    Short,
    Integer,
    Long,
    Float,
    Double,
    String,
    Unit,
    Score(Box<Self>),
    List(Box<Self>),
    TypedCompound(HashMap<SNBTString, Self>),
    Compound(Box<Self>),
    Data(Box<Self>),
    Reference(Box<Self>),
    Tuple(Vec<Self>),
    SNBT,
    Generic(String),
    Struct(Span, HighTypeId, Vec<Self>),
    Inferred,
    InferredInteger,
    InferredFloat,
    ResourceLocation,
}

impl PartiallyResolvedDataType {
    pub fn resolve_fully(
        self,
        ctx: &mut SemanticAnalysisContext,
        resolver: &GenericResolver,
    ) -> Option<DataType> {
        Some(match self {
            Self::Boolean => DataType::Boolean,
            Self::Byte => DataType::Byte,
            Self::Short => DataType::Short,
            Self::Integer => DataType::Integer,
            Self::Long => DataType::Long,
            Self::Float => DataType::Float,
            Self::Double => DataType::Double,
            Self::String => DataType::String,
            Self::Unit => DataType::Unit,
            Self::Score(data_type) => {
                let data_type = data_type.resolve_fully(ctx, resolver)?;

                DataType::Score(Box::new(data_type))
            }
            Self::List(data_type) => {
                let data_type = data_type.resolve_fully(ctx, resolver)?;

                DataType::List(Box::new(data_type))
            }
            Self::TypedCompound(compound) => {
                let compound = compound
                    .into_iter()
                    .map(|(key, data_type)| {
                        let data_type = data_type.resolve_fully(ctx, resolver)?;

                        Some((key, data_type))
                    })
                    .collect_option_all()?;

                DataType::TypedCompound(compound)
            }
            Self::Compound(data_type) => {
                let data_type = data_type.resolve_fully(ctx, resolver)?;

                DataType::Compound(Box::new(data_type))
            }
            Self::Data(data_type) => {
                let data_type = data_type.resolve_fully(ctx, resolver)?;

                DataType::Data(Box::new(data_type))
            }
            Self::Reference(data_type) => {
                let data_type = data_type.resolve_fully(ctx, resolver)?;

                DataType::Reference(Box::new(data_type))
            }
            Self::Tuple(data_types) => {
                let data_types = data_types
                    .into_iter()
                    .map(|data_type| data_type.resolve_fully(ctx, resolver))
                    .collect_option_all()?;

                DataType::Tuple(data_types)
            }
            Self::SNBT => DataType::SNBT,
            Self::Generic(name) => resolver.resolve(&name)?.clone(),
            Self::Struct(name_span, id, generic_types) => {
                let generic_types = generic_types
                    .into_iter()
                    .map(|generic_type| generic_type.resolve_fully(ctx, resolver))
                    .collect_option_all()?;

                let declaration = ctx.get_type(id).clone();

                let id = declaration
                    .resolve_fully(ctx, id, generic_types, name_span)?
                    .as_struct_id()?;

                DataType::Struct(id)
            }
            Self::Inferred => DataType::Inferred,
            Self::InferredInteger => DataType::InferredInteger,
            Self::InferredFloat => DataType::InferredFloat,
            Self::ResourceLocation => DataType::ResourceLocation,
        })
    }
}
