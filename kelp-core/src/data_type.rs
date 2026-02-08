use std::{collections::BTreeMap, fmt::Display};

use minecraft_command_types::{impl_has_macro_false, snbt::SNBTString};
use parser_rs::parser_range::ParserRange;

use crate::{
    expression::{ArithmeticOperator, ComparisonOperator, HighSNBTString, LogicalOperator},
    place::PlaceType,
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
    trait_ext::OptionIterExt,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DataTypeKind {
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
    Any,
}

impl Display for DataTypeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            DataTypeKind::Byte => "byte",
            DataTypeKind::Short => "short",
            DataTypeKind::Integer => "integer",
            DataTypeKind::Long => "long",
            DataTypeKind::Float => "float",
            DataTypeKind::Double => "double",
            DataTypeKind::String => "string",
            DataTypeKind::Unit => "unit",
            DataTypeKind::Score => "score",
            DataTypeKind::List => "list",
            DataTypeKind::Compound => "compound",
            DataTypeKind::Data => "data",
            DataTypeKind::Custom => "custom",
            DataTypeKind::Tuple => "tuple",
            DataTypeKind::Any => "any",
        })
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
    Score,
    List(Box<DataType>),
    TypedCompound(BTreeMap<SNBTString, DataType>),
    Compound(Box<DataType>),
    Data(Box<DataType>),
    Reference(Box<DataType>),
    Custom(String, Vec<DataType>),
    Tuple(Vec<DataType>),
    Any,
}

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DataType::Boolean => f.write_str("boolean"),
            DataType::Byte => f.write_str("byte"),
            DataType::Short => f.write_str("short"),
            DataType::Integer => f.write_str("integer"),
            DataType::Long => f.write_str("long"),
            DataType::Float => f.write_str("float"),
            DataType::Double => f.write_str("double"),
            DataType::String => f.write_str("string"),
            DataType::Unit => f.write_str("unit"),
            DataType::Score => f.write_str("score"),
            DataType::List(data_type) => write!(f, "list<{}>", data_type),
            DataType::TypedCompound(compound) => {
                f.write_str("{")?;

                if !compound.is_empty() {
                    f.write_str(" ")?;
                }

                for (i, (key, value)) in compound.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    write!(f, "{}: {}", key.1, value)?;
                }

                if !compound.is_empty() {
                    f.write_str(" ")?;
                }

                f.write_str("}")
            }
            DataType::Compound(data_type) => write!(f, "compound<{}>", data_type),
            DataType::Data(data_type) => write!(f, "data<{}>", data_type),
            DataType::Reference(data_type) => write!(f, "&{}", data_type),
            DataType::Custom(name, _data_types) => {
                // TODO
                write!(f, "{}", name)
            }
            DataType::Tuple(data_types) => {
                f.write_str("(")?;

                for (i, data_type) in data_types.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    write!(f, "{}", data_type)?;
                }

                f.write_str(")")
            }
            DataType::Any => write!(f, "any"),
        }
    }
}

impl DataType {
    pub fn get_iterable_type(&self) -> Option<DataType> {
        Some(match self {
            DataType::List(data_type) => *data_type.clone(),
            DataType::Data(data_type) => return data_type.get_iterable_type(),
            DataType::String => DataType::String,
            DataType::Any => DataType::Any,
            DataType::Reference(expression) => return expression.get_iterable_type(),
            _ => return None,
        })
    }

    pub fn as_place_type(self) -> Option<PlaceType> {
        Some(match self {
            DataType::Score => PlaceType::Score,
            DataType::Data(_) => PlaceType::Data,
            _ => return None,
        })
    }

    pub fn try_dereference(self) -> DataType {
        if let DataType::Reference(data_type) = self {
            *data_type
        } else {
            self
        }
    }

    pub fn is_lvalue(&self) -> bool {
        matches!(self, DataType::Score | DataType::Data(_) | DataType::Any)
    }

    pub fn can_cast_to(&self, data_type: &DataType) -> bool {
        if self.equals(data_type) {
            return true;
        }

        matches!(
            (self, data_type),
            (DataType::Byte, DataType::Short)
                | (DataType::Byte, DataType::Integer)
                | (DataType::Byte, DataType::Long)
                | (DataType::Byte, DataType::Float)
                | (DataType::Byte, DataType::Double)
                | (DataType::Short, DataType::Byte)
                | (DataType::Short, DataType::Integer)
                | (DataType::Short, DataType::Long)
                | (DataType::Short, DataType::Float)
                | (DataType::Short, DataType::Double)
                | (DataType::Integer, DataType::Byte)
                | (DataType::Integer, DataType::Short)
                | (DataType::Integer, DataType::Long)
                | (DataType::Integer, DataType::Float)
                | (DataType::Integer, DataType::Double)
                | (DataType::Long, DataType::Byte)
                | (DataType::Long, DataType::Short)
                | (DataType::Long, DataType::Integer)
                | (DataType::Long, DataType::Float)
                | (DataType::Long, DataType::Double)
                | (DataType::Float, DataType::Byte)
                | (DataType::Float, DataType::Short)
                | (DataType::Float, DataType::Integer)
                | (DataType::Float, DataType::Long)
                | (DataType::Float, DataType::Double)
                | (DataType::Double, DataType::Byte)
                | (DataType::Double, DataType::Short)
                | (DataType::Double, DataType::Integer)
                | (DataType::Double, DataType::Long)
                | (DataType::Double, DataType::Float)
        )
    }
    pub fn is_condition(&self) -> bool {
        matches!(
            self,
            DataType::Boolean | DataType::Score | DataType::Data(_) | DataType::Any
        )
    }

    pub fn requires_reference(&self) -> bool {
        matches!(self, DataType::Score | DataType::Data(_))
    }

    pub fn can_be_assigned_to(&self) -> bool {
        matches!(self, DataType::Score | DataType::Data(_))
    }

    pub fn is_score_like(&self) -> bool {
        match self {
            DataType::Byte
            | DataType::Short
            | DataType::Integer
            | DataType::Score
            | DataType::Any => true,
            DataType::Data(data_type) | DataType::Reference(data_type) => data_type.is_score_like(),
            _ => false,
        }
    }

    pub fn can_be_assigned_to_score(&self) -> bool {
        match self {
            DataType::Boolean => true,
            _ => self.is_score_like(),
        }
    }

    pub fn can_be_assigned_to_data(&self) -> bool {
        match self {
            DataType::Boolean
                | DataType::Byte
                | DataType::Short
                | DataType::Integer
                | DataType::Long
                | DataType::Score
                | DataType::Any
                | DataType::Float
                | DataType::Double
                | DataType::String
                | DataType::Unit
                | DataType::List(_)
                | DataType::TypedCompound(_)
                | DataType::Compound(_)
                // TODO DataType::Custom?
                | DataType::Tuple(_) => true,
                DataType::Data(data_type) | DataType::Reference(data_type) => data_type.can_be_assigned_to_data(),
                _ => false,
        }
    }

    pub fn can_be_indexed(&self) -> bool {
        matches!(self, DataType::List(_) | DataType::Data(_) | DataType::Any)
    }

    pub fn has_members(&self) -> bool {
        matches!(
            self,
            DataType::TypedCompound(_)
                | DataType::Compound(_)
                | DataType::Data(_)
                | DataType::Tuple(_)
                | DataType::Any
        )
    }

    pub fn has_member(&self, member: &HighSNBTString) -> bool {
        match self {
            DataType::TypedCompound(compound) => {
                compound.keys().any(|key| *key == member.snbt_string)
            }
            DataType::Tuple(data_types) => {
                if let Ok(index) = member.snbt_string.1.parse::<i32>() {
                    data_types.len() > (index as usize)
                } else {
                    false
                }
            }
            DataType::Compound(_) | DataType::Data(_) | DataType::Any => true,
            _ => false,
        }
    }

    pub fn get_arithmetic_result(
        &self,
        operator: &ArithmeticOperator,
        other: &DataType,
    ) -> Option<DataType> {
        Some(match (self, other) {
            (DataType::Any, _) | (_, DataType::Any) => DataType::Any,
            (DataType::Byte, DataType::Byte) => DataType::Byte,
            (DataType::Short, DataType::Short) => DataType::Short,
            (DataType::Integer, DataType::Integer) => DataType::Integer,
            (DataType::Long, DataType::Long) => DataType::Long,
            (DataType::Score, other) if other.is_score_like() => DataType::Score,
            (self_, DataType::Score) if self_.is_score_like() => DataType::Score,
            _ => return None,
        })
    }

    #[allow(clippy::only_used_in_recursion)]
    pub fn can_perform_augmented_assignment(
        &self,
        operator: &ArithmeticOperator,
        other: &DataType,
    ) -> bool {
        match (self, other) {
            (_, other) if *operator == ArithmeticOperator::Swap && !other.is_lvalue() => false,
            (DataType::Any, _) | (_, DataType::Any) => true,
            (DataType::Byte, DataType::Byte) => true,
            (DataType::Short, DataType::Short) => true,
            (DataType::Integer, DataType::Integer) => true,
            (DataType::Long, DataType::Long) => true,
            (DataType::Data(inner), other) => {
                inner.can_perform_augmented_assignment(operator, other)
            }
            (DataType::Score, other) if other.is_score_like() => true,
            (self_, DataType::Score) if self_.is_score_like() => true,
            _ => false,
        }
    }

    pub fn can_perform_comparison(&self, operator: &ComparisonOperator, other: &DataType) -> bool {
        if (*operator == ComparisonOperator::EqualTo || *operator == ComparisonOperator::NotEqualTo)
            && self.equals(other)
        {
            return true;
        }

        match (self, other) {
            (DataType::Any, _) | (_, DataType::Any) => true,
            (DataType::Byte, DataType::Byte) => true,
            (DataType::Short, DataType::Short) => true,
            (DataType::Integer, DataType::Integer) => true,
            (DataType::Long, DataType::Long) => true,
            (DataType::Float, DataType::Float) => true,
            (DataType::Double, DataType::Double) => true,
            (DataType::Score, other) if other.is_score_like() => true,
            (self_, DataType::Score) if self_.is_score_like() => true,
            _ => false,
        }
    }

    pub fn can_perform_logical_comparison(
        &self,
        _operator: &LogicalOperator,
        other: &DataType,
    ) -> bool {
        match (self, other) {
            (DataType::Any, _) | (_, DataType::Any) => true,
            (DataType::Boolean, DataType::Boolean) => true,
            (DataType::Byte, DataType::Byte) => true,
            (DataType::Short, DataType::Short) => true,
            (DataType::Integer, DataType::Integer) => true,
            (DataType::Long, DataType::Long) => true,
            (DataType::Float, DataType::Float) => true,
            (DataType::Double, DataType::Double) => true,
            (DataType::Score, other) if other.is_score_like() => true,
            (self_, DataType::Score) if self_.is_score_like() => true,
            _ => false,
        }
    }

    pub fn get_index_result(&self) -> Option<DataType> {
        Some(match self {
            DataType::List(data_type) => *data_type.clone(),
            DataType::Data(data_type) => return data_type.get_index_result(),
            DataType::Any => DataType::Any,
            _ => return None,
        })
    }

    pub fn get_field_result(&self, field: &SNBTString) -> Option<DataType> {
        Some(match self {
            DataType::TypedCompound(compound) => return compound.get(field).cloned(),
            DataType::Compound(data_type) => *data_type.clone(),
            DataType::Data(data_type) => return data_type.get_field_result(field),
            DataType::Tuple(items) => {
                return if let Ok(index) = field.1.parse::<i32>() {
                    items.get(index as usize).cloned()
                } else {
                    None
                };
            }
            DataType::Any => DataType::Any,
            _ => return None,
        })
    }

    pub fn can_be_referenced(&self) -> bool {
        matches!(self, DataType::Data(_) | DataType::Score | DataType::Any)
    }

    pub fn can_be_dereferenced(&self) -> bool {
        matches!(self, DataType::Reference(_) | DataType::Any)
    }

    pub fn get_negated_result(&self) -> Option<DataType> {
        Some(match self {
            DataType::Any => DataType::Any,
            DataType::Byte => DataType::Byte,
            DataType::Short => DataType::Short,
            DataType::Integer => DataType::Integer,
            DataType::Long => DataType::Long,
            DataType::Score => DataType::Score,
            _ => return None,
        })
    }

    pub fn get_inverted_result(&self) -> Option<DataType> {
        Some(match self {
            DataType::Boolean => DataType::Boolean,
            DataType::Any => DataType::Any,
            DataType::Byte => DataType::Byte,
            DataType::Short => DataType::Short,
            DataType::Integer => DataType::Integer,
            DataType::Long => DataType::Long,
            DataType::Score => DataType::Score,
            _ => return None,
        })
    }

    pub fn equals(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Any, _) | (_, Self::Any) => true,
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HighDataTypeKind {
    Named(ParserRange, String, Vec<HighDataType>),
    TypedCompound(BTreeMap<HighSNBTString, HighDataType>),
    Reference(Box<HighDataType>),
    Unit,
}

impl HighDataTypeKind {
    pub fn resolve(&self) -> DataType {
        match self {
            HighDataTypeKind::Named(_, name, generics) => match name.as_str() {
                "byte" => {
                    if !generics.is_empty() {
                        unreachable!()
                    }

                    DataType::Byte
                }
                "short" => {
                    if !generics.is_empty() {
                        unreachable!()
                    }

                    DataType::Short
                }
                "integer" | "int" => {
                    if generics.is_empty() {
                        unreachable!()
                    }

                    DataType::Integer
                }
                "long" => {
                    if !generics.is_empty() {
                        unreachable!()
                    }

                    DataType::Long
                }
                "float" => {
                    if !generics.is_empty() {
                        unreachable!()
                    }

                    DataType::Float
                }
                "double" => {
                    if !generics.is_empty() {
                        unreachable!()
                    }

                    DataType::Double
                }
                "string" | "str" => {
                    if !generics.is_empty() {
                        unreachable!()
                    }

                    DataType::String
                }
                "score" => {
                    if !generics.is_empty() {
                        unreachable!()
                    }

                    DataType::Score
                }
                "list" => {
                    if generics.is_empty() {
                        DataType::List(Box::new(DataType::Any))
                    } else if generics.len() == 1
                        && let Some(first) = generics.first()
                    {
                        DataType::List(Box::new(first.kind.resolve()))
                    } else {
                        unreachable!()
                    }
                }
                "compound" => {
                    if generics.is_empty() {
                        DataType::Compound(Box::new(DataType::Any))
                    } else if generics.len() == 1
                        && let Some(first) = generics.first()
                    {
                        DataType::Compound(Box::new(first.kind.resolve()))
                    } else {
                        unreachable!()
                    }
                }
                "data" => {
                    if generics.is_empty() {
                        DataType::Data(Box::new(DataType::Any))
                    } else if generics.len() == 1
                        && let Some(first) = generics.first()
                    {
                        DataType::Data(Box::new(first.kind.resolve()))
                    } else {
                        unreachable!()
                    }
                }
                "any" => DataType::Any,
                _ => DataType::Custom(
                    name.clone(),
                    generics
                        .iter()
                        .map(|generic| generic.kind.resolve())
                        .collect(),
                ),
            },
            HighDataTypeKind::Unit => DataType::Unit,
            HighDataTypeKind::Reference(data_type) => {
                DataType::Reference(Box::new(data_type.kind.resolve()))
            }
            HighDataTypeKind::TypedCompound(compound) => DataType::TypedCompound(
                compound
                    .iter()
                    .map(|(key, value)| (key.snbt_string.clone(), value.kind.resolve()))
                    .collect(),
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HighDataType {
    pub span: ParserRange,
    pub kind: HighDataTypeKind,
}

impl_has_macro_false!(HighDataType);

impl HighDataType {
    pub fn perform_semantic_analysis(&self, ctx: &mut SemanticAnalysisContext) -> Option<()> {
        match &self.kind {
            HighDataTypeKind::Named(span, name, generics) => match name.as_str() {
                "byte" => {
                    if !generics.is_empty() {
                        return ctx.add_invalid_generics(
                            self.span,
                            DataTypeKind::Byte,
                            0,
                            generics.len(),
                        );
                    }
                }
                "short" => {
                    if !generics.is_empty() {
                        return ctx.add_invalid_generics(
                            self.span,
                            DataTypeKind::Short,
                            0,
                            generics.len(),
                        );
                    }
                }
                "integer" | "int" => {
                    if !generics.is_empty() {
                        return ctx.add_invalid_generics(
                            self.span,
                            DataTypeKind::Integer,
                            0,
                            generics.len(),
                        );
                    }
                }
                "long" => {
                    if !generics.is_empty() {
                        return ctx.add_invalid_generics(
                            self.span,
                            DataTypeKind::Long,
                            0,
                            generics.len(),
                        );
                    }
                }
                "float" => {
                    if !generics.is_empty() {
                        return ctx.add_invalid_generics(
                            self.span,
                            DataTypeKind::Float,
                            0,
                            generics.len(),
                        );
                    }
                }
                "double" => {
                    if !generics.is_empty() {
                        return ctx.add_invalid_generics(
                            self.span,
                            DataTypeKind::Double,
                            0,
                            generics.len(),
                        );
                    }
                }
                "string" | "str" => {
                    if !generics.is_empty() {
                        return ctx.add_invalid_generics(
                            self.span,
                            DataTypeKind::String,
                            0,
                            generics.len(),
                        );
                    }
                }
                "unit" => {
                    // TODO "did you mean ()?""
                    if ctx.get_variable(name).is_none() {
                        return ctx.add_info(SemanticAnalysisInfo {
                            span: *span,
                            kind: SemanticAnalysisInfoKind::Error(
                                SemanticAnalysisError::UnknownType(name.clone()),
                            ),
                        });
                    }
                }
                "score" => {
                    if !generics.is_empty() {
                        return ctx.add_invalid_generics(
                            self.span,
                            DataTypeKind::Score,
                            0,
                            generics.len(),
                        );
                    }
                }
                "list" => {
                    if generics.len() != 1 {
                        return ctx.add_invalid_generics(
                            self.span,
                            DataTypeKind::List,
                            1,
                            generics.len(),
                        );
                    } else if let Some(first) = generics.first() {
                        return first.perform_semantic_analysis(ctx);
                    }
                }
                "compound" => {
                    if generics.len() != 1 {
                        return ctx.add_invalid_generics(
                            self.span,
                            DataTypeKind::Compound,
                            1,
                            generics.len(),
                        );
                    } else if let Some(first) = generics.first() {
                        return first.perform_semantic_analysis(ctx);
                    }
                }
                "data" => {
                    if generics.len() != 1 {
                        return ctx.add_invalid_generics(
                            self.span,
                            DataTypeKind::Data,
                            1,
                            generics.len(),
                        );
                    } else if let Some(first) = generics.first() {
                        return first.perform_semantic_analysis(ctx);
                    }
                }
                "any" => {
                    if !generics.is_empty() {
                        return ctx.add_invalid_generics(
                            self.span,
                            DataTypeKind::Any,
                            0,
                            generics.len(),
                        );
                    }
                }
                _ => {
                    if ctx.get_variable(name).is_none() {
                        return ctx.add_info(SemanticAnalysisInfo {
                            span: *span,
                            kind: SemanticAnalysisInfoKind::Error(
                                SemanticAnalysisError::UnknownType(name.clone()),
                            ),
                        });
                    }
                }
            },
            HighDataTypeKind::Unit => (),
            HighDataTypeKind::Reference(data_type) => {
                return data_type.perform_semantic_analysis(ctx);
            }
            HighDataTypeKind::TypedCompound(compound) => {
                return compound
                    .values()
                    .map(|value| value.perform_semantic_analysis(ctx))
                    .all_some();
            }
        }

        Some(())
    }
}
