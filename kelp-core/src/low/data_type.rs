use std::{
    collections::BTreeMap,
    fmt::{Display, Write},
};

use minecraft_command_types::{
    nbt_path::NbtPathNode,
    snbt::{SNBT, SNBTString},
};

use crate::low::environment::{
    Environment, r#type::r#struct::StructId, value::function::FunctionId,
};
use crate::runtime_storage::RuntimeStorageType;

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FieldAccessType {
    #[default]
    Name,
    Index,
}

impl FieldAccessType {
    #[must_use]
    pub fn into_nbt_path_node(self, field: String) -> NbtPathNode {
        match self {
            Self::Name => NbtPathNode::Named(SNBTString(false, field), None),
            Self::Index => {
                let index = field.parse::<usize>().unwrap();

                NbtPathNode::Index(Some(SNBT::macroable_integer(index as i32)))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DataType {
    Boolean,
    Byte,
    Short,
    Integer,
    Long,
    Float,
    Double,
    String,
    Unit,
    Never,
    Score(Box<Self>),
    List(Box<Self>),
    TypedCompound(BTreeMap<String, Self>),
    Compound(Box<Self>),
    Data(Box<Self>),
    Reference(Box<Self>),
    Tuple(Vec<Self>),
    Function(FunctionId),
    Struct(StructId),
    Inferred,
    InferredInteger,
    InferredFloat,
    ResourceLocation,
    EntitySelector,
    Coordinates,
}

impl DataType {
    #[must_use]
    pub fn get_field_access_type(&self, environment: &Environment) -> Option<FieldAccessType> {
        Some(match self {
            Self::TypedCompound(..) => FieldAccessType::Name,
            Self::Compound(..) => FieldAccessType::Name,
            Self::Data(..) => FieldAccessType::Name,
            Self::Reference(data_type) => data_type.get_field_access_type(environment)?,
            Self::Tuple(..) => FieldAccessType::Index,
            Self::Struct(id) => environment.get_struct(*id).get_field_access_type(),

            _ => return None,
        })
    }

    #[must_use]
    pub fn get_runtime_storage_type(&self) -> RuntimeStorageType {
        match self {
            Self::Boolean
            | Self::Byte
            | Self::Short
            | Self::Integer
            | Self::InferredInteger
            | Self::Score(..) => RuntimeStorageType::Score,
            Self::Reference(data_type) => data_type.get_runtime_storage_type(),
            _ => RuntimeStorageType::Data,
        }
    }

    #[must_use]
    pub fn get_iterable_type(&self) -> Option<Self> {
        Some(match self {
            Self::Reference(inner) => return inner.get_iterable_type(),

            Self::List(data_type) => *data_type.clone(),
            Self::String => Self::String,

            Self::Data(data_type) => data_type.get_iterable_type()?,
            _ => return None,
        })
    }

    #[must_use]
    pub const fn is_integer_like(&self) -> bool {
        self.is_restricted_integer_like() || matches!(self, Self::Long)
    }

    #[must_use]
    pub const fn is_restricted_integer_like(&self) -> bool {
        matches!(
            self,
            Self::Byte | Self::Short | Self::Integer | Self::InferredInteger
        )
    }

    #[must_use]
    pub const fn is_float_like(&self) -> bool {
        matches!(self, Self::Float | Self::Double | Self::InferredFloat)
    }

    #[must_use]
    pub const fn is_numeric(&self) -> bool {
        self.is_integer_like() || self.is_float_like()
    }

    #[must_use]
    pub fn equals(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Inferred, _) | (_, Self::Inferred) => true,

            (Self::InferredInteger, rhs) | (rhs, Self::InferredInteger)
                if rhs.is_integer_like() =>
            {
                true
            }

            (Self::InferredFloat, rhs) | (rhs, Self::InferredFloat) if rhs.is_float_like() => true,

            (Self::List(data_types), Self::List(other_data_types))
            | (Self::Compound(data_types), Self::Compound(other_data_types))
            | (Self::Score(data_types), Self::Score(other_data_types))
            | (Self::Data(data_types), Self::Data(other_data_types)) => {
                data_types.equals(other_data_types)
            }

            (Self::Tuple(data_types), Self::Tuple(other_data_types)) => {
                data_types.len() == other_data_types.len()
                    && data_types
                        .iter()
                        .zip(other_data_types)
                        .all(|(s, o)| s.equals(o))
            }

            (Self::TypedCompound(data_types), Self::TypedCompound(other_data_types)) => {
                other_data_types.iter().all(|(key, data_type)| {
                    data_types.get(key).is_some_and(|sv| sv.equals(data_type))
                })
            }

            (Self::Compound(inner), Self::TypedCompound(map))
            | (Self::TypedCompound(map), Self::Compound(inner)) => {
                map.values().all(|v| v.equals(inner))
            }

            _ => self == other,
        }
    }
}

pub struct DataTypeDisplay<'a> {
    pub data_type: &'a DataType,
    pub environment: &'a Environment,
}

impl Display for DataTypeDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.data_type {
            DataType::Boolean => f.write_str("bool"),
            DataType::Byte => f.write_str("byte"),
            DataType::Short => f.write_str("short"),
            DataType::Integer => f.write_str("integer"),
            DataType::Long => f.write_str("long"),
            DataType::Float => f.write_str("float"),
            DataType::Double => f.write_str("double"),
            DataType::String => f.write_str("string"),
            DataType::Unit => f.write_str("()"),
            DataType::Never => f.write_char('!'),
            DataType::Score(data_type) => {
                f.write_str("score<")?;
                data_type.display(self.environment).fmt(f)?;
                f.write_char('>')?;

                Ok(())
            }
            DataType::List(data_type) => {
                f.write_str("list<")?;
                data_type.display(self.environment).fmt(f)?;
                f.write_char('>')?;

                Ok(())
            }
            DataType::TypedCompound(compound) => {
                f.write_char('{')?;

                if !compound.is_empty() {
                    f.write_char(' ')?;
                }

                for (i, (key, value_data_type)) in compound.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    write!(f, "{}: ", key)?;

                    value_data_type.display(self.environment).fmt(f)?;
                }

                if !compound.is_empty() {
                    f.write_char(' ')?;
                }

                f.write_char('}')?;

                Ok(())
            }
            DataType::Compound(data_type) => {
                f.write_str("compound<")?;
                data_type.display(self.environment).fmt(f)?;
                f.write_char('>')?;

                Ok(())
            }
            DataType::Data(data_type) => {
                f.write_str("data<")?;
                data_type.display(self.environment).fmt(f)?;
                f.write_char('>')?;

                Ok(())
            }
            DataType::Reference(data_type) => {
                f.write_char('&')?;
                data_type.display(self.environment).fmt(f)?;

                Ok(())
            }
            DataType::Tuple(data_types) => {
                f.write_char('(')?;

                for (i, data_type) in data_types.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    data_type.display(self.environment).fmt(f)?;
                }

                f.write_char(')')?;

                Ok(())
            }
            DataType::Function(id) => {
                // Maybe display full path?

                let (_, _, declaration) = self.environment.get_function(*id);

                let generic_types = declaration.generic_types();

                write!(f, "fn {}", declaration.name())?;

                if !generic_types.is_empty() {
                    f.write_str("<")?;

                    for (i, data_type) in generic_types.iter().enumerate() {
                        if i != 0 {
                            f.write_str(", ")?;
                        }

                        data_type.display(self.environment).fmt(f)?;
                    }

                    f.write_str(">")?;
                }

                f.write_char('(')?;

                for (i, data_type) in declaration.parameter_types().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    data_type.display(self.environment).fmt(f)?;
                }

                write!(
                    f,
                    ") -> {}",
                    declaration.return_type().display(self.environment)
                )?;

                Ok(())
            }
            DataType::Struct(id) => {
                // Maybe display full path?

                let declaration = self.environment.get_struct(*id);

                let generic_types = declaration.generic_types();

                f.write_str(declaration.name())?;

                if !generic_types.is_empty() {
                    f.write_str("<")?;

                    for (i, generic_type) in generic_types.iter().enumerate() {
                        if i != 0 {
                            f.write_str(", ")?;
                        }

                        generic_type.display(self.environment).fmt(f)?;
                    }

                    f.write_str(">")?;
                }

                Ok(())
            }
            DataType::Inferred => f.write_char('_'),
            DataType::InferredInteger => f.write_str("{integer}"),
            DataType::InferredFloat => f.write_str("{float}"),
            DataType::ResourceLocation => f.write_str("resource_location"),
            DataType::EntitySelector => f.write_str("entity_selector"),
            DataType::Coordinates => f.write_str("coordinates"),
        }
    }
}

impl DataType {
    #[must_use]
    pub const fn display<'a>(&'a self, environment: &'a Environment) -> DataTypeDisplay<'a> {
        DataTypeDisplay {
            data_type: self,
            environment,
        }
    }
}
