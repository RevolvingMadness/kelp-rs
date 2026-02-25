use std::{collections::BTreeMap, fmt::Display};

use crate::high::snbt_string::HighSNBTString;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PatternType {
    Boolean,
    Byte,
    Short,
    Integer,
    Long,
    Float,
    Double,
    String,
    Tuple(Vec<Self>),
    Struct(String, BTreeMap<HighSNBTString, Self>),
    Reference(Box<Self>),
    Compound(BTreeMap<HighSNBTString, Self>),
    Dereference(Box<Self>),
    Any,
}

impl Display for PatternType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Boolean => f.write_str("boolean"),
            Self::Byte => f.write_str("byte"),
            Self::Short => f.write_str("short"),
            Self::Integer => f.write_str("integer"),
            Self::Long => f.write_str("long"),
            Self::Float => f.write_str("float"),
            Self::Double => f.write_str("double"),
            Self::String => f.write_str("string"),
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
            Self::Struct(name, fields) => {
                write!(f, "{} {{", name)?;

                if !fields.is_empty() {
                    f.write_str(" ")?;
                }

                for (i, (key, pattern_type)) in fields.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    write!(f, "{}: {}", key.snbt_string.1, pattern_type)?;
                }

                if !fields.is_empty() {
                    f.write_str(" ")?;
                }

                f.write_str("}")?;

                Ok(())
            }
            Self::Reference(pattern) => write!(f, "&{}", pattern),
            Self::Compound(compound) => {
                f.write_str("{")?;

                if !compound.is_empty() {
                    f.write_str(" ")?;
                }

                for (i, (key, pattern_type)) in compound.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    write!(f, "{}: {}", key.snbt_string.1, pattern_type)?;
                }

                if !compound.is_empty() {
                    f.write_str(" ")?;
                }

                f.write_str("}")
            }
            Self::Any => f.write_str("_"),
            Self::Dereference(pattern_type) => write!(f, "&{}", pattern_type),
        }
    }
}
