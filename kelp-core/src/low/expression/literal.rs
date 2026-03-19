use ordered_float::NotNan;

use crate::{high::snbt_string::SNBTString, pattern_type::PatternType};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum LiteralExpression {
    Boolean(bool),
    Byte(i8),
    Short(i16),
    Integer(i32),
    Long(i64),
    Float(NotNan<f32>),
    Double(NotNan<f64>),
    String(SNBTString),
}

impl LiteralExpression {
    #[must_use]
    pub const fn get_pattern_type(&self) -> PatternType {
        match self {
            Self::Boolean(_) => PatternType::Boolean,
            Self::Byte(_) => PatternType::Byte,
            Self::Short(_) => PatternType::Short,
            Self::Integer(_) => PatternType::Integer,
            Self::Long(_) => PatternType::Long,
            Self::Float(_) => PatternType::Float,
            Self::Double(_) => PatternType::Double,
            Self::String(_) => PatternType::String,
        }
    }
}
