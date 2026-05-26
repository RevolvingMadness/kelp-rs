use ordered_float::NotNan;

use crate::{parsed::snbt_string::SNBTString, pattern_type::PatternType};

#[derive(Debug, Clone)]
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
            Self::Boolean(..) => PatternType::Boolean,
            Self::Byte(..) => PatternType::Byte,
            Self::Short(..) => PatternType::Short,
            Self::Integer(..) => PatternType::Integer,
            Self::Long(..) => PatternType::Long,
            Self::Float(..) => PatternType::Float,
            Self::Double(..) => PatternType::Double,
            Self::String(..) => PatternType::String,
        }
    }
}
