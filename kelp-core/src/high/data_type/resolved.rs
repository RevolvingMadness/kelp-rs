use std::collections::HashMap;

use minecraft_command_types::snbt::SNBTString;

use crate::{
    high::{
        environment::r#type::{HighTypeDeclaration, HighTypeId},
        semantic_analysis_context::SemanticAnalysisContext,
    },
    middle::data_type::DataType,
    span::Span,
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedDataType {
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
}

impl ResolvedDataType {
    pub fn lower(
        self,
        ctx: &mut SemanticAnalysisContext,
        generic_mapping: &HashMap<String, DataType>,
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
                let data_type = data_type.lower(ctx, generic_mapping)?;

                DataType::Score(Box::new(data_type))
            }
            Self::List(data_type) => {
                let data_type = data_type.lower(ctx, generic_mapping)?;

                DataType::List(Box::new(data_type))
            }
            Self::TypedCompound(compound) => {
                let compound = compound
                    .into_iter()
                    .map(|(key, data_type)| {
                        let data_type = data_type.lower(ctx, generic_mapping)?;

                        Some((key, data_type))
                    })
                    .collect_option_all()?;

                DataType::TypedCompound(compound)
            }
            Self::Compound(data_type) => {
                let data_type = data_type.lower(ctx, generic_mapping)?;

                DataType::Compound(Box::new(data_type))
            }
            Self::Data(data_type) => {
                let data_type = data_type.lower(ctx, generic_mapping)?;

                DataType::Data(Box::new(data_type))
            }
            Self::Reference(data_type) => {
                let data_type = data_type.lower(ctx, generic_mapping)?;

                DataType::Reference(Box::new(data_type))
            }
            Self::Tuple(data_types) => {
                let data_types = data_types
                    .into_iter()
                    .map(|data_type| data_type.lower(ctx, generic_mapping))
                    .collect_option_all()?;

                DataType::Tuple(data_types)
            }
            Self::SNBT => DataType::SNBT,
            Self::Generic(name) => generic_mapping.get(&name)?.clone(),
            Self::Struct(name_span, id, generic_types) => {
                let generic_types = generic_types
                    .into_iter()
                    .map(|generic_type| generic_type.lower(ctx, generic_mapping))
                    .collect_option_all()?;

                let HighTypeDeclaration::Struct(declaration) = ctx.get_type(id) else {
                    return None;
                };

                let declaration = declaration.clone();

                let id =
                    ctx.get_monomorphized_struct_id(id, declaration, generic_types, name_span)?;

                DataType::Struct(id)
            }
            Self::Inferred => DataType::Inferred,
            Self::InferredInteger => DataType::InferredInteger,
            Self::InferredFloat => DataType::InferredFloat,
        })
    }
}
