use std::{collections::HashMap, fmt::Display};

use crate::high::{
    data::DataTarget, nbt_path::NbtPath, player_score::PlayerScore, snbt_string::SNBTString,
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
    Score(PlayerScore),
    Data(DataTarget, NbtPath),
    Tuple(Vec<Self>),
    Struct(String, HashMap<SNBTString, Self>),
    Reference(Box<Self>),
    Compound(HashMap<SNBTString, Self>),
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
            Self::Score(score) => score.fmt(f),
            Self::Data(target, _) => {
                target.fmt(f)?;

                f.write_str(" ...")?;

                Ok(())
            }
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
        }
    }
}
