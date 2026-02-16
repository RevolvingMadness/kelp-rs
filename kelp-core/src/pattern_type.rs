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
    Tuple(Vec<PatternType>),
    Struct(String, BTreeMap<HighSNBTString, PatternType>),
    Reference(Box<PatternType>),
    Compound(BTreeMap<HighSNBTString, PatternType>),
    Dereference(Box<PatternType>),
    Any,
}

impl Display for PatternType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PatternType::Boolean => f.write_str("boolean"),
            PatternType::Byte => f.write_str("byte"),
            PatternType::Short => f.write_str("short"),
            PatternType::Integer => f.write_str("integer"),
            PatternType::Long => f.write_str("long"),
            PatternType::Float => f.write_str("float"),
            PatternType::Double => f.write_str("double"),
            PatternType::String => f.write_str("string"),
            PatternType::Tuple(pattern_types) => {
                f.write_str("(")?;

                for (i, pattern_type) in pattern_types.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    write!(f, "{}", pattern_type)?;
                }

                f.write_str(")")
            }
            PatternType::Struct(name, fields) => {
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
            PatternType::Reference(pattern) => write!(f, "&{}", pattern),
            PatternType::Compound(compound) => {
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
            PatternType::Any => f.write_str("_"),
            PatternType::Dereference(pattern_type) => write!(f, "&{}", pattern_type),
        }
    }
}
