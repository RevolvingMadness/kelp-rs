use std::{collections::BTreeMap, fmt::Display};

use minecraft_command_types::snbt::SNBTString;
use parser_rs::parser_range::ParserRange;

use crate::{
    high::snbt_string::HighSNBTString,
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator},
    place::PlaceTypeKind,
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
};

pub mod high;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GenericDataTypeKind {
    Byte,
    Short,
    Integer,
    Long,
    Float,
    Double,
    String,
    Unit,
    Score,
    List,
    Compound,
    Data,
    Custom,
    Tuple,
    SNBT,
}

impl Display for GenericDataTypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            GenericDataTypeKind::Byte => "byte",
            GenericDataTypeKind::Short => "short",
            GenericDataTypeKind::Integer => "integer",
            GenericDataTypeKind::Long => "long",
            GenericDataTypeKind::Float => "float",
            GenericDataTypeKind::Double => "double",
            GenericDataTypeKind::String => "string",
            GenericDataTypeKind::Unit => "unit",
            GenericDataTypeKind::Score => "score",
            GenericDataTypeKind::List => "list",
            GenericDataTypeKind::Compound => "compound",
            GenericDataTypeKind::Data => "data",
            GenericDataTypeKind::Custom => "custom",
            GenericDataTypeKind::Tuple => "tuple",
            GenericDataTypeKind::SNBT => "snbt",
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DataTypeKind {
    Boolean,
    Byte,
    Short,
    Integer,
    Long,
    Float,
    Double,
    String,
    Unit,
    Score,
    List(Box<DataTypeKind>),
    TypedCompound(BTreeMap<SNBTString, DataTypeKind>),
    Compound(Box<DataTypeKind>),
    Data(Box<DataTypeKind>),
    Reference(Box<DataTypeKind>),
    Custom(String, Vec<DataTypeKind>),
    Tuple(Vec<DataTypeKind>),
    SNBT,
}

impl Display for DataTypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataTypeKind::Boolean => f.write_str("boolean"),
            DataTypeKind::Byte => f.write_str("byte"),
            DataTypeKind::Short => f.write_str("short"),
            DataTypeKind::Integer => f.write_str("integer"),
            DataTypeKind::Long => f.write_str("long"),
            DataTypeKind::Float => f.write_str("float"),
            DataTypeKind::Double => f.write_str("double"),
            DataTypeKind::String => f.write_str("string"),
            DataTypeKind::Unit => f.write_str("unit"),
            DataTypeKind::Score => f.write_str("score"),
            DataTypeKind::List(data_type) => write!(f, "list<{}>", data_type),
            DataTypeKind::TypedCompound(compound) => {
                f.write_str("{")?;

                if !compound.is_empty() {
                    f.write_str(" ")?;
                }

                for (i, (key, value_data_type)) in compound.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    write!(f, "{}: {}", key.1, value_data_type)?;
                }

                if !compound.is_empty() {
                    f.write_str(" ")?;
                }

                f.write_str("}")
            }
            DataTypeKind::Compound(data_type) => write!(f, "compound<{}>", data_type),
            DataTypeKind::Data(data_type) => write!(f, "data<{}>", data_type),
            DataTypeKind::Reference(data_type) => write!(f, "&{}", data_type),
            DataTypeKind::Custom(name, _data_types) => {
                // TODO
                write!(f, "{}", name)
            }
            DataTypeKind::Tuple(data_types) => {
                f.write_str("(")?;

                for (i, data_type) in data_types.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    write!(f, "{}", data_type)?;
                }

                f.write_str(")")
            }
            DataTypeKind::SNBT => write!(f, "snbt"),
        }
    }
}

impl DataTypeKind {
    pub fn get_iterable_type(&self) -> Option<DataTypeKind> {
        Some(match self {
            DataTypeKind::Reference(self_) => match &**self_ {
                DataTypeKind::List(data_type) => *data_type.clone(),
                DataTypeKind::Data(data_type) => data_type.get_iterable_type()?,
                DataTypeKind::String => DataTypeKind::String,
                _ => return None,
            },

            DataTypeKind::List(data_type) => *data_type.clone(),
            DataTypeKind::Data(data_type) => data_type.get_iterable_type()?,
            DataTypeKind::String => DataTypeKind::String,
            _ => return None,
        })
    }

    pub fn as_place_type(self) -> Option<PlaceTypeKind> {
        Some(match self {
            DataTypeKind::Score => PlaceTypeKind::Score,
            DataTypeKind::Data(data_type) => PlaceTypeKind::Data(*data_type),
            _ => return None,
        })
    }

    pub fn dereference(self) -> Option<DataTypeKind> {
        Some(match self {
            DataTypeKind::Reference(data_type) => *data_type,
            DataTypeKind::Score => DataTypeKind::Score,
            DataTypeKind::Data(data_type) => *data_type,

            _ => return None,
        })
    }

    pub fn is_lvalue(&self) -> bool {
        matches!(self, DataTypeKind::Score | DataTypeKind::Data(_))
    }

    pub fn can_cast_to(&self, data_type: &DataTypeKind) -> bool {
        if self.equals(data_type) {
            return true;
        }

        matches!(
            (self, data_type),
            (DataTypeKind::Byte, DataTypeKind::Short)
                | (DataTypeKind::Byte, DataTypeKind::Integer)
                | (DataTypeKind::Byte, DataTypeKind::Long)
                | (DataTypeKind::Byte, DataTypeKind::Float)
                | (DataTypeKind::Byte, DataTypeKind::Double)
                | (DataTypeKind::Short, DataTypeKind::Byte)
                | (DataTypeKind::Short, DataTypeKind::Integer)
                | (DataTypeKind::Short, DataTypeKind::Long)
                | (DataTypeKind::Short, DataTypeKind::Float)
                | (DataTypeKind::Short, DataTypeKind::Double)
                | (DataTypeKind::Integer, DataTypeKind::Byte)
                | (DataTypeKind::Integer, DataTypeKind::Short)
                | (DataTypeKind::Integer, DataTypeKind::Long)
                | (DataTypeKind::Integer, DataTypeKind::Float)
                | (DataTypeKind::Integer, DataTypeKind::Double)
                | (DataTypeKind::Long, DataTypeKind::Byte)
                | (DataTypeKind::Long, DataTypeKind::Short)
                | (DataTypeKind::Long, DataTypeKind::Integer)
                | (DataTypeKind::Long, DataTypeKind::Float)
                | (DataTypeKind::Long, DataTypeKind::Double)
                | (DataTypeKind::Float, DataTypeKind::Byte)
                | (DataTypeKind::Float, DataTypeKind::Short)
                | (DataTypeKind::Float, DataTypeKind::Integer)
                | (DataTypeKind::Float, DataTypeKind::Long)
                | (DataTypeKind::Float, DataTypeKind::Double)
                | (DataTypeKind::Double, DataTypeKind::Byte)
                | (DataTypeKind::Double, DataTypeKind::Short)
                | (DataTypeKind::Double, DataTypeKind::Integer)
                | (DataTypeKind::Double, DataTypeKind::Long)
                | (DataTypeKind::Double, DataTypeKind::Float)
        )
    }
    pub fn is_condition(&self) -> bool {
        matches!(self, DataTypeKind::Boolean | DataTypeKind::Data(_))
    }

    pub fn is_score_value(&self) -> bool {
        match self {
            DataTypeKind::Boolean
            | DataTypeKind::Byte
            | DataTypeKind::Short
            | DataTypeKind::Integer
            | DataTypeKind::Score => true,
            DataTypeKind::Data(data_type) | DataTypeKind::Reference(data_type) => {
                data_type.is_score_value()
            }
            _ => false,
        }
    }

    pub fn is_snbt_like(&self) -> bool {
        match self {
            DataTypeKind::Boolean => true,
            DataTypeKind::Byte => true,
            DataTypeKind::Short => true,
            DataTypeKind::Integer => true,
            DataTypeKind::Long => true,
            DataTypeKind::Float => true,
            DataTypeKind::Double => true,
            DataTypeKind::String => true,
            DataTypeKind::Score => true,
            DataTypeKind::List(data_type) => data_type.is_snbt_like(),
            DataTypeKind::TypedCompound(compound) => {
                compound.values().all(|value| value.is_snbt_like())
            }
            DataTypeKind::Compound(data_type) => data_type.is_snbt_like(),
            DataTypeKind::SNBT => true,
            _ => false,
        }
    }

    pub fn can_be_assigned_to_data(&self) -> bool {
        match self {
            DataTypeKind::Unit => true,
            DataTypeKind::Score => true,
            DataTypeKind::List(data_type) => data_type.can_be_assigned_to_data(),
            DataTypeKind::TypedCompound(compound) => compound
                .values()
                .all(|data_type| data_type.can_be_assigned_to_data()),
            DataTypeKind::Compound(compound) => compound.can_be_assigned_to_data(),
            DataTypeKind::Data(data_type) => data_type.can_be_assigned_to_data(),
            DataTypeKind::Reference(data_type) => data_type.can_be_assigned_to_data(),
            DataTypeKind::Custom(_, _) => true,
            DataTypeKind::Tuple(data_types) => data_types
                .iter()
                .all(|data_type| data_type.can_be_assigned_to_data()),
            DataTypeKind::SNBT => true,
            _ => self.is_snbt_like(),
        }
    }

    pub fn can_be_indexed(&self) -> bool {
        match self {
            DataTypeKind::Reference(data_type) => data_type.can_be_indexed(),

            DataTypeKind::List(_) | DataTypeKind::Data(_) | DataTypeKind::SNBT => true,

            _ => false,
        }
    }

    pub fn has_fields(&self) -> bool {
        match self {
            DataTypeKind::Reference(data_type) => data_type.has_fields(),

            DataTypeKind::TypedCompound(_)
            | DataTypeKind::Compound(_)
            | DataTypeKind::Data(_)
            | DataTypeKind::Tuple(_)
            | DataTypeKind::SNBT => true,

            _ => false,
        }
    }

    pub fn has_field(&self, field: &HighSNBTString) -> bool {
        match self {
            DataTypeKind::Reference(data_type) => data_type.has_field(field),

            DataTypeKind::TypedCompound(compound) => {
                compound.keys().any(|key| *key == field.snbt_string)
            }
            DataTypeKind::Tuple(data_types) => {
                if let Ok(index) = field.snbt_string.1.parse::<i32>() {
                    data_types.len() > (index as usize)
                } else {
                    false
                }
            }
            DataTypeKind::Compound(_) | DataTypeKind::Data(_) | DataTypeKind::SNBT => true,
            _ => false,
        }
    }

    fn raw_get_arithmetic_result(
        &self,
        _operator: &ArithmeticOperator,
        other: &DataTypeKind,
    ) -> Option<DataTypeKind> {
        Some(match (self, other) {
            (DataTypeKind::Byte, DataTypeKind::Byte) => DataTypeKind::Byte,
            (DataTypeKind::Short, DataTypeKind::Short) => DataTypeKind::Short,
            (DataTypeKind::Integer, DataTypeKind::Integer) => DataTypeKind::Integer,
            (DataTypeKind::Long, DataTypeKind::Long) => DataTypeKind::Long,
            (DataTypeKind::Score, other) | (other, DataTypeKind::Score)
                if other.is_score_value() =>
            {
                DataTypeKind::Score
            }
            _ => return None,
        })
    }

    pub fn get_arithmetic_result(
        &self,
        operator: &ArithmeticOperator,
        other: &DataTypeKind,
    ) -> Option<DataTypeKind> {
        match (self, other) {
            (DataTypeKind::Reference(self_), DataTypeKind::Reference(other)) => {
                self_.raw_get_arithmetic_result(operator, other)
            }
            (DataTypeKind::Reference(self_), other) => {
                self_.raw_get_arithmetic_result(operator, other)
            }
            (_, DataTypeKind::Reference(other)) => self.raw_get_arithmetic_result(operator, other),

            _ => self.raw_get_arithmetic_result(operator, other),
        }
    }

    pub fn can_perform_augmented_assignment(
        &self,
        operator: &ArithmeticOperator,
        other: &DataTypeKind,
    ) -> bool {
        match (self, other) {
            (_, other) if *operator == ArithmeticOperator::Swap && !other.is_lvalue() => false,
            (DataTypeKind::Byte, DataTypeKind::Byte) => true,
            (DataTypeKind::Short, DataTypeKind::Short) => true,
            (DataTypeKind::Integer, DataTypeKind::Integer) => true,
            (DataTypeKind::Long, DataTypeKind::Long) => true,
            (DataTypeKind::Data(inner), other) => {
                inner.can_perform_augmented_assignment(operator, other)
            }
            (DataTypeKind::Score, other) | (other, DataTypeKind::Score)
                if other.is_score_value() =>
            {
                true
            }
            _ => false,
        }
    }

    pub fn perform_assignment_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        span: ParserRange,
        other: &DataTypeKind,
    ) -> Option<()> {
        match (self, other) {
            (DataTypeKind::Score, other) => {
                if !other.is_score_value() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeAssignedToScore(other.clone()),
                        ),
                    });
                }
            }
            (DataTypeKind::Data(data_type), other) => {
                if !other.equals(data_type) {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::MismatchedTypes {
                                expected: *data_type.clone(),
                                actual: other.clone(),
                            },
                        ),
                    });
                }
            }
            (self_, other) => {
                if !other.equals(self) {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::MismatchedTypes {
                                expected: self_.clone(),
                                actual: other.clone(),
                            },
                        ),
                    });
                }
            }
        }

        Some(())
    }

    pub fn raw_can_perform_comparison(
        &self,
        _operator: &ComparisonOperator,
        other: &DataTypeKind,
    ) -> bool {
        match (self, other) {
            (DataTypeKind::Byte, DataTypeKind::Byte) => true,
            (DataTypeKind::Short, DataTypeKind::Short) => true,
            (DataTypeKind::Integer, DataTypeKind::Integer) => true,
            (DataTypeKind::Long, DataTypeKind::Long) => true,
            (DataTypeKind::Float, DataTypeKind::Float) => true,
            (DataTypeKind::Double, DataTypeKind::Double) => true,
            (DataTypeKind::Score, other) | (other, DataTypeKind::Score)
                if other.is_score_value() =>
            {
                true
            }
            _ => false,
        }
    }

    pub fn can_perform_comparison(
        &self,
        operator: &ComparisonOperator,
        other: &DataTypeKind,
    ) -> bool {
        if (*operator == ComparisonOperator::EqualTo || *operator == ComparisonOperator::NotEqualTo)
            && self.equals(other)
        {
            return true;
        }

        match (self, other) {
            (DataTypeKind::Reference(self_), DataTypeKind::Reference(other)) => {
                self_.raw_can_perform_comparison(operator, other)
            }
            (_, DataTypeKind::Reference(other)) => self.raw_can_perform_comparison(operator, other),
            (DataTypeKind::Reference(self_), other) => {
                self_.raw_can_perform_comparison(operator, other)
            }

            _ => self.raw_can_perform_comparison(operator, other),
        }
    }

    pub fn can_perform_logical_comparison(
        &self,
        _operator: &LogicalOperator,
        other: &DataTypeKind,
    ) -> bool {
        matches!(
            (self, other),
            (DataTypeKind::Boolean, DataTypeKind::Boolean)
        )
    }

    pub fn get_index_result(&self) -> Option<DataTypeKind> {
        Some(match self {
            DataTypeKind::Reference(self_) => self_.get_index_result()?,

            DataTypeKind::List(data_type) => *data_type.clone(),
            DataTypeKind::Data(data_type) => data_type.get_index_result()?,
            DataTypeKind::SNBT => DataTypeKind::SNBT,

            _ => return None,
        })
    }

    pub fn get_field_result(&self, field: &SNBTString) -> Option<DataTypeKind> {
        Some(match self {
            DataTypeKind::Reference(self_) => self_.get_field_result(field)?,

            DataTypeKind::TypedCompound(compound) => {
                return compound.get(field).cloned();
            }
            DataTypeKind::Compound(data_type) => *data_type.clone(),
            DataTypeKind::Data(data_type) => return data_type.get_field_result(field),
            DataTypeKind::Tuple(items) => {
                return if let Ok(index) = field.1.parse::<i32>() {
                    items.get(index as usize).cloned()
                } else {
                    None
                };
            }
            _ => return None,
        })
    }

    pub fn can_be_referenced(&self) -> bool {
        matches!(self, DataTypeKind::Data(_) | DataTypeKind::Score)
    }

    pub fn can_be_dereferenced(&self) -> bool {
        matches!(
            self,
            DataTypeKind::Reference(_) | DataTypeKind::Score | DataTypeKind::Data(_)
        )
    }

    pub fn get_negated_result(&self) -> Option<DataTypeKind> {
        Some(match self {
            DataTypeKind::Reference(self_) => match **self_ {
                DataTypeKind::Byte => DataTypeKind::Byte,
                DataTypeKind::Short => DataTypeKind::Short,
                DataTypeKind::Integer => DataTypeKind::Integer,
                DataTypeKind::Long => DataTypeKind::Long,
                DataTypeKind::Score => DataTypeKind::Score,
                _ => return None,
            },

            DataTypeKind::Byte => DataTypeKind::Byte,
            DataTypeKind::Short => DataTypeKind::Short,
            DataTypeKind::Integer => DataTypeKind::Integer,
            DataTypeKind::Long => DataTypeKind::Long,
            DataTypeKind::Score => DataTypeKind::Score,
            _ => return None,
        })
    }

    pub fn get_inverted_result(&self) -> Option<DataTypeKind> {
        Some(match self {
            DataTypeKind::Reference(self_) => match **self_ {
                DataTypeKind::Boolean => DataTypeKind::Boolean,
                _ => return None,
            },

            DataTypeKind::Boolean => DataTypeKind::Boolean,

            _ => return None,
        })
    }

    pub fn equals(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::SNBT, other) | (other, Self::SNBT) => other.is_snbt_like(),
            (Self::List(self_type), Self::List(other_type)) => self_type.equals(other_type),
            (Self::Tuple(self_elements), Self::Tuple(other_elements)) => {
                self_elements.len() == other_elements.len()
                    && self_elements
                        .iter()
                        .zip(other_elements)
                        .all(|(s, o)| s.equals(o))
            }
            (Self::TypedCompound(self_compound), Self::TypedCompound(other_compound)) => {
                other_compound.iter().all(|(key, other_type)| {
                    self_compound
                        .get(key)
                        .is_some_and(|self_type| self_type.equals(other_type))
                })
            }
            (Self::Compound(self_type), Self::Compound(other_type)) => self_type.equals(other_type),
            (Self::Compound(compound_data_type), Self::TypedCompound(data_types))
            | (Self::TypedCompound(data_types), Self::Compound(compound_data_type)) => data_types
                .values()
                .all(|data_type| data_type.equals(compound_data_type)),
            (Self::Data(self_data), Self::Data(other_data)) => self_data.equals(other_data),
            (Self::Custom(self_name, self_generics), Self::Custom(other_name, other_generics)) => {
                self_name == other_name
                    && self_generics.len() == other_generics.len()
                    && self_generics
                        .iter()
                        .zip(other_generics)
                        .all(|(left, right)| left.equals(right))
            }
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}
