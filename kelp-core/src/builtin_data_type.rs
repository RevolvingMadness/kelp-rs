use strum::{Display, EnumString};

use crate::low::data_type::DataType;

#[derive(Display, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumString)]
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
    Unit,
    Score,
    List,
    Compound,
    Data,
    #[strum(
        serialize = "snbt",
        serialize = "nbt",
        serialize = "SNBT",
        serialize = "NBT"
    )]
    SNBT,
    ResourceLocation,
}

impl BuiltinDataType {
    #[must_use]
    pub fn to_data_type(self, mut generic_types: Vec<DataType>) -> Option<DataType> {
        if generic_types.len() != self.generic_count() {
            return None;
        }

        Some(match self {
            Self::Unit => DataType::Unit,
            Self::Boolean => DataType::Boolean,
            Self::Byte => DataType::Byte,
            Self::Short => DataType::Short,
            Self::Integer => DataType::Integer,
            Self::Long => DataType::Long,
            Self::Float => DataType::Float,
            Self::Double => DataType::Double,
            Self::String => DataType::String,
            Self::SNBT => DataType::SNBT,
            Self::List | Self::Compound | Self::Data | Self::Score => {
                let element_type = generic_types.remove(0);

                match self {
                    Self::List => DataType::List(Box::new(element_type)),
                    Self::Compound => DataType::Compound(Box::new(element_type)),
                    Self::Data => DataType::Data(Box::new(element_type)),
                    Self::Score => DataType::Score(Box::new(element_type)),
                    _ => unreachable!(),
                }
            }
            Self::ResourceLocation => DataType::ResourceLocation,
        })
    }

    #[must_use]
    pub fn to_resolved_data_type(&self, mut generic_types: Vec<DataType>) -> Option<DataType> {
        if generic_types.len() != self.generic_count() {
            return None;
        }

        Some(match self {
            Self::Unit => DataType::Unit,
            Self::Boolean => DataType::Boolean,
            Self::Byte => DataType::Byte,
            Self::Short => DataType::Short,
            Self::Integer => DataType::Integer,
            Self::Long => DataType::Long,
            Self::Float => DataType::Float,
            Self::Double => DataType::Double,
            Self::String => DataType::String,
            Self::SNBT => DataType::SNBT,
            Self::List | Self::Compound | Self::Data | Self::Score => {
                let element_type = generic_types.remove(0);

                match self {
                    Self::List => DataType::List(Box::new(element_type)),
                    Self::Compound => DataType::Compound(Box::new(element_type)),
                    Self::Data => DataType::Data(Box::new(element_type)),
                    Self::Score => DataType::Score(Box::new(element_type)),
                    _ => unreachable!(),
                }
            }
            Self::ResourceLocation => DataType::ResourceLocation,
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
            | Self::Unit
            | Self::SNBT
            | Self::ResourceLocation => 0,
            Self::List | Self::Compound | Self::Data | Self::Score => 1,
        }
    }

    #[must_use]
    pub const fn name(&self) -> &str {
        match self {
            Self::Boolean => "boolean",
            Self::Byte => "byte",
            Self::Short => "short",
            Self::Integer => "integer",
            Self::Long => "long",
            Self::Float => "float",
            Self::Double => "double",
            Self::String => "string",
            Self::Unit => "unit",
            Self::Score => "score",
            Self::List => "list",
            Self::Compound => "compound",
            Self::Data => "data",
            Self::SNBT => "snbt",
            Self::ResourceLocation => "resource_location",
        }
    }
}
