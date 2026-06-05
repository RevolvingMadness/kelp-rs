use std::{
    collections::HashMap,
    fmt::{Display, Write},
};

use crate::{
    parsed::typed_path::ParsedTypedPath,
    parsed::{data::Data, player_score::ParsedPlayerScore},
};

#[derive(Debug, Clone)]
pub enum PatternType {
    Boolean,
    Byte,
    Short,
    Integer,
    Long,
    Float,
    Double,
    String,
    Score(ParsedPlayerScore),
    Data(Box<Data>),
    Tuple(Vec<Self>),
    RegularStruct(ParsedTypedPath, HashMap<String, Self>),
    TupleStruct(ParsedTypedPath, Vec<Self>),
    Reference(Box<Self>),
    Compound(HashMap<String, Self>),
    Any,
}

impl Display for PatternType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Boolean => f.write_str("bool"),
            Self::Byte => f.write_str("byte"),
            Self::Short => f.write_str("short"),
            Self::Integer => f.write_str("integer"),
            Self::Long => f.write_str("long"),
            Self::Float => f.write_str("float"),
            Self::Double => f.write_str("double"),
            Self::String => f.write_str("string"),
            Self::Score(score) => score.fmt(f),
            Self::Data(data) => data.fmt(f),
            Self::Tuple(pattern_types) => {
                f.write_str("(")?;

                for (i, pattern_type) in pattern_types.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    write!(f, "{}", pattern_type)?;
                }

                f.write_str(")")
            }
            Self::RegularStruct(path, field_types) => {
                write!(f, "{} {{", path)?;

                if !field_types.is_empty() {
                    f.write_char(' ')?;
                }

                for (i, (field_name, field_type)) in field_types.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    write!(f, "{}: {}", field_name, field_type)?;
                }

                if !field_types.is_empty() {
                    f.write_char(' ')?;
                }

                f.write_str("}")?;

                Ok(())
            }
            Self::TupleStruct(path, field_types) => {
                write!(f, "{}(", path)?;

                if !field_types.is_empty() {
                    f.write_char(' ')?;
                }

                for (i, field_type) in field_types.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    field_type.fmt(f)?;
                }

                if !field_types.is_empty() {
                    f.write_char(' ')?;
                }

                f.write_char(')')?;

                Ok(())
            }
            Self::Reference(pattern) => write!(f, "&{}", pattern),
            Self::Compound(compound) => {
                f.write_char('{')?;

                if !compound.is_empty() {
                    f.write_char(' ')?;
                }

                for (i, (key, pattern_type)) in compound.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    write!(f, "{}: {}", key, pattern_type)?;
                }

                if !compound.is_empty() {
                    f.write_char(' ')?;
                }

                f.write_char('}')?;

                Ok(())
            }
            Self::Any => f.write_str("_"),
        }
    }
}
