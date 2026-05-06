use strum::{Display, EnumIter, EnumString};

use crate::{
    high::semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    low::data_type::unresolved::UnresolvedDataType,
    span::Span,
};

#[derive(Display, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumString, EnumIter)]
#[strum(serialize_all = "snake_case")]
pub enum BuiltinDataType {
    #[strum(serialize = "boolean", serialize = "bool")]
    Boolean,
    Byte,
    Short,
    #[strum(serialize = "integer", serialize = "int")]
    Integer,
    Long,
    Float,
    Double,
    #[strum(serialize = "string", serialize = "str")]
    String,
    #[strum(serialize = "()", serialize = "unit")]
    Score,
    List,
    Compound,
    Data,
    EntitySelector,
    ResourceLocation,
    Coordinates,
}

impl BuiltinDataType {
    #[must_use]
    pub fn to_data_type_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        span: Span,
        mut generic_spans: Vec<Span>,
        mut generic_types: Vec<UnresolvedDataType>,
    ) -> UnresolvedDataType {
        let expected_generic_count = self.generic_count();
        let actual_generic_count = generic_types.len();

        if actual_generic_count != expected_generic_count {
            return ctx.add_invalid_generics_type(
                span,
                self.name(),
                expected_generic_count,
                actual_generic_count,
            );
        }

        match self {
            Self::Boolean => UnresolvedDataType::Boolean,
            Self::Byte => UnresolvedDataType::Byte,
            Self::Short => UnresolvedDataType::Short,
            Self::Integer => UnresolvedDataType::Integer,
            Self::Long => UnresolvedDataType::Long,
            Self::Float => UnresolvedDataType::Float,
            Self::Double => UnresolvedDataType::Double,
            Self::String => UnresolvedDataType::String,
            Self::List | Self::Compound | Self::Data | Self::Score => {
                let element_type = generic_types.remove(0);

                match self {
                    Self::List => UnresolvedDataType::List(Box::new(element_type)),
                    Self::Compound => UnresolvedDataType::Compound(Box::new(element_type)),
                    Self::Data => {
                        let Some(data_type) = element_type.get_data_type(&ctx.high_environment)
                        else {
                            let element_span = generic_spans.remove(0);

                            return ctx.add_error_type(
                                element_span,
                                SemanticAnalysisError::TypeIsNotDataCompatible(element_type),
                            );
                        };

                        UnresolvedDataType::Data(Box::new(data_type))
                    }
                    Self::Score => {
                        if !element_type.is_score_compatible() {
                            let element_span = generic_spans.remove(0);

                            return ctx.add_error_type(
                                element_span,
                                SemanticAnalysisError::TypeIsNotDataCompatible(element_type),
                            );
                        }

                        UnresolvedDataType::Score(Box::new(element_type))
                    }
                    _ => unreachable!(),
                }
            }
            Self::EntitySelector => UnresolvedDataType::EntitySelector,
            Self::ResourceLocation => UnresolvedDataType::ResourceLocation,
            Self::Coordinates => UnresolvedDataType::Coordinates,
        }
    }

    #[must_use]
    pub fn to_resolved_data_type(
        &self,
        mut generic_types: Vec<UnresolvedDataType>,
    ) -> Option<UnresolvedDataType> {
        if generic_types.len() != self.generic_count() {
            return None;
        }

        Some(match self {
            Self::Boolean => UnresolvedDataType::Boolean,
            Self::Byte => UnresolvedDataType::Byte,
            Self::Short => UnresolvedDataType::Short,
            Self::Integer => UnresolvedDataType::Integer,
            Self::Long => UnresolvedDataType::Long,
            Self::Float => UnresolvedDataType::Float,
            Self::Double => UnresolvedDataType::Double,
            Self::String => UnresolvedDataType::String,
            Self::List | Self::Compound | Self::Data | Self::Score => {
                let element_type = generic_types.remove(0);

                match self {
                    Self::List => UnresolvedDataType::List(Box::new(element_type)),
                    Self::Compound => UnresolvedDataType::Compound(Box::new(element_type)),
                    Self::Data => UnresolvedDataType::Data(Box::new(element_type)),
                    Self::Score => UnresolvedDataType::Score(Box::new(element_type)),
                    _ => unreachable!(),
                }
            }
            Self::EntitySelector => UnresolvedDataType::EntitySelector,
            Self::ResourceLocation => UnresolvedDataType::ResourceLocation,
            Self::Coordinates => UnresolvedDataType::Coordinates,
        })
    }

    #[must_use]
    pub const fn generic_count(&self) -> usize {
        match self {
            Self::Boolean
            | Self::Byte
            | Self::Short
            | Self::Integer
            | Self::Long
            | Self::Float
            | Self::Double
            | Self::String
            | Self::EntitySelector
            | Self::ResourceLocation
            | Self::Coordinates => 0,
            Self::List | Self::Compound | Self::Data | Self::Score => 1,
        }
    }

    #[must_use]
    pub const fn name(&self) -> &str {
        match self {
            Self::Boolean => "bool",
            Self::Byte => "byte",
            Self::Short => "short",
            Self::Integer => "integer",
            Self::Long => "long",
            Self::Float => "float",
            Self::Double => "double",
            Self::String => "string",
            Self::Score => "score",
            Self::List => "list",
            Self::Compound => "compound",
            Self::Data => "data",
            Self::EntitySelector => "entity_selector",
            Self::ResourceLocation => "resource_location",
            Self::Coordinates => "coordinates",
        }
    }
}
