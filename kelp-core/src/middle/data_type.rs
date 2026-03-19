use std::{collections::HashMap, fmt::Display};

use minecraft_command_types::snbt::SNBTString as LowSNBTString;

use crate::{
    high::supports_variable_type_scope::SupportsVariableTypeScope,
    operator::{ArithmeticOperator, ComparisonOperator},
    place::PlaceTypeKind,
    trait_ext::OptionBoolIterExt,
};

#[derive(Debug, Clone, PartialEq, Eq)]
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
    Score(Box<Self>),
    List(Box<Self>),
    TypedCompound(HashMap<LowSNBTString, Self>),
    Compound(Box<Self>),
    Data(Box<Self>),
    Reference(Box<Self>),
    Generic(String),
    Tuple(Vec<Self>),
    SNBT,
    Struct(String, Vec<Self>),
    Inferred,
    InferredInteger,
    InferredFloat,
}

impl Display for DataType {
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
            Self::Unit => f.write_str("()"),
            Self::Score(data_type) => write!(f, "score<{}>", data_type),
            Self::List(data_type) => write!(f, "list<{}>", data_type),
            Self::TypedCompound(compound) => {
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
            Self::Compound(data_type) => write!(f, "compound<{}>", data_type),
            Self::Data(data_type) => write!(f, "data<{}>", data_type),
            Self::Reference(data_type) => write!(f, "&{}", data_type),
            Self::Generic(name) => f.write_str(name),
            Self::Tuple(data_types) => {
                f.write_str("(")?;

                for (i, data_type) in data_types.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    write!(f, "{}", data_type)?;
                }

                f.write_str(")")
            }
            Self::SNBT => f.write_str("snbt"),
            Self::Struct(name, generics) => {
                f.write_str(name)?;

                if !generics.is_empty() {
                    f.write_str("<")?;

                    for (i, data_type) in generics.iter().enumerate() {
                        if i != 0 {
                            f.write_str(", ")?;
                        }

                        write!(f, "{}", data_type)?;
                    }

                    f.write_str(">")?;
                }

                Ok(())
            }
            Self::Inferred => f.write_str("_"),
            Self::InferredInteger => f.write_str("{integer}"),
            Self::InferredFloat => f.write_str("{float}"),
        }
    }
}

impl DataType {
    #[must_use]
    pub fn distribute(self) -> Self {
        match self {
            Self::Data(data_type) => data_type.distribute_data(),
            Self::Reference(data_type) => data_type.distribute_references(),
            Self::Score(data_type) => data_type.distribute_score(),
            _ => self,
        }
    }

    #[must_use]
    pub const fn is_integer_like(&self) -> bool {
        self.is_restrictied_integer_like() || matches!(self, Self::Long)
    }

    #[must_use]
    pub const fn is_restrictied_integer_like(&self) -> bool {
        matches!(
            self,
            Self::Byte | Self::Short | Self::Integer | Self::InferredInteger
        )
    }

    #[must_use]
    pub const fn is_restrictied_float_like(&self) -> bool {
        matches!(self, Self::Float | Self::InferredFloat)
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
    pub fn try_infer(self, other: Option<Self>) -> Self {
        let Some(other) = other else {
            return self;
        };

        match (self, other) {
            (Self::Inferred, other) | (other, Self::Inferred) => other,

            (Self::InferredInteger, other) | (other, Self::InferredInteger)
                if other.is_integer_like() =>
            {
                other
            }

            (Self::InferredFloat, other) | (other, Self::InferredFloat)
                if other.is_float_like() =>
            {
                other
            }

            (self_, _) => self_,
        }
    }

    #[must_use]
    pub fn is_runtime(&self) -> bool {
        match self {
            Self::Score(_) | Self::Data(_) => true,
            Self::Reference(data_type_kind) => data_type_kind.is_runtime(),
            Self::Generic(_) => unreachable!(),
            _ => false,
        }
    }

    #[must_use]
    pub fn to_data(self) -> Self {
        match self {
            Self::Boolean => Self::Boolean,
            Self::Byte => Self::Byte,
            Self::Short => Self::Short,
            Self::Integer | Self::InferredInteger => Self::Integer,
            Self::Long => Self::Long,
            Self::Float | Self::InferredFloat => Self::Float,
            Self::Double => Self::Double,
            Self::String => Self::String,
            Self::Unit => Self::Unit,
            Self::List(data_type) => Self::List(Box::new(data_type.to_data())),
            Self::TypedCompound(compound) => Self::TypedCompound(
                compound
                    .into_iter()
                    .map(|(key, value)| (key, value.to_data()))
                    .collect(),
            ),
            Self::Compound(data_type) => Self::Compound(Box::new(data_type.to_data())),
            Self::Data(data_type) | Self::Score(data_type) | Self::Reference(data_type) => {
                data_type.to_data()
            }
            Self::Generic(_) => unreachable!(),
            Self::Tuple(data_types) => {
                Self::Tuple(data_types.into_iter().map(Self::to_data).collect())
            }
            Self::SNBT => Self::SNBT,
            Self::Struct(name, generic_types) => Self::Struct(name, generic_types),
            Self::Inferred => Self::Inferred,
        }
    }

    #[must_use]
    pub fn to_score(self) -> Option<Self> {
        Some(match self {
            Self::InferredInteger
            | Self::Byte
            | Self::Short
            | Self::Integer
            | Self::Long
            | Self::InferredFloat
            | Self::Float
            | Self::Double => Self::Integer,
            Self::Boolean => Self::Boolean,
            Self::TypedCompound(compound) => Self::TypedCompound(
                compound
                    .into_iter()
                    .map(|(key, value)| value.to_score().map(|value| (key, value)))
                    .collect::<Option<_>>()?,
            ),
            Self::Compound(data_type) => Self::Compound(Box::new(data_type.to_score()?)),
            Self::Score(data_type) | Self::Reference(data_type) | Self::Data(data_type) => {
                data_type.to_score()?
            }
            Self::Generic(_) => unreachable!(),
            Self::Struct(name, generic_types) => Self::Struct(
                name,
                generic_types
                    .into_iter()
                    .map(Self::to_score)
                    .collect::<Option<_>>()?,
            ),
            _ => return None,
        })
    }

    #[must_use]
    pub fn substitute(self, substitutions: &HashMap<String, Self>) -> Option<Self> {
        Some(match self {
            Self::List(inner) => Self::List(Box::new(inner.substitute(substitutions)?)),
            Self::Compound(inner) => Self::Compound(Box::new(inner.substitute(substitutions)?)),
            Self::Data(inner) => Self::Data(Box::new(inner.substitute(substitutions)?)),
            Self::Reference(inner) => Self::Reference(Box::new(inner.substitute(substitutions)?)),
            Self::Tuple(inner) => Self::Tuple(
                inner
                    .into_iter()
                    .map(|t| t.substitute(substitutions))
                    .collect::<Option<_>>()?,
            ),
            Self::TypedCompound(inner) => Self::TypedCompound(
                inner
                    .into_iter()
                    .map(|(k, v)| v.substitute(substitutions).map(|v| (k, v)))
                    .collect::<Option<_>>()?,
            ),
            Self::Generic(ref name) => substitutions.get(name)?.clone(),
            Self::Struct(name, generics) => Self::Struct(
                name,
                generics
                    .into_iter()
                    .map(|g| g.substitute(substitutions))
                    .collect::<Option<_>>()?,
            ),
            _ => self,
        })
    }

    #[inline]
    #[must_use]
    fn reference(self) -> Self {
        Self::Reference(Box::new(self))
    }

    #[must_use]
    pub fn distribute_references(self) -> Self {
        match self {
            Self::Reference(inner) => match inner.distribute_references() {
                Self::List(data_type) => Self::List(Box::new(data_type.reference())),
                Self::Tuple(data_types) => {
                    Self::Tuple(data_types.into_iter().map(Self::reference).collect())
                }
                Self::Struct(name, generic_types) => Self::Struct(
                    name,
                    generic_types.into_iter().map(Self::reference).collect(),
                ),
                Self::TypedCompound(compound) => Self::TypedCompound(
                    compound
                        .into_iter()
                        .map(|(key, data_type)| (key, data_type.reference()))
                        .collect(),
                ),
                Self::Compound(data_type) => Self::Compound(Box::new(data_type.reference())),
                Self::Data(data_type) => Self::Data(Box::new(data_type.reference())),
                inner => inner.reference(),
            },
            _ => self,
        }
    }

    #[must_use]
    pub fn distribute_data(self) -> Self {
        match self {
            Self::Data(inner) => match inner.distribute_data() {
                Self::Reference(data_type) => {
                    Self::Reference(Box::new(data_type.distribute_data()))
                }
                Self::List(data_type) => Self::List(Box::new(data_type.distribute_data())),
                Self::Tuple(data_types) => {
                    Self::Tuple(data_types.into_iter().map(Self::distribute_data).collect())
                }
                Self::TypedCompound(compound) => Self::TypedCompound(
                    compound
                        .into_iter()
                        .map(|(key, data_type)| (key, data_type.distribute_data()))
                        .collect(),
                ),
                Self::Compound(data_type) => Self::Compound(Box::new(data_type.distribute_data())),
                Self::Struct(name, generic_types) => Self::Struct(
                    name,
                    generic_types
                        .into_iter()
                        .map(Self::distribute_data)
                        .collect(),
                ),
                Self::Data(inner) => *inner,
                inner => Self::Data(Box::new(inner)),
            },
            _ => self,
        }
    }

    #[must_use]
    pub fn distribute_score(self) -> Self {
        match self {
            Self::Score(inner) => match inner.distribute_score() {
                Self::Reference(data_type) => {
                    Self::Reference(Box::new(data_type.distribute_score()))
                }
                Self::List(data_type) => Self::List(Box::new(data_type.distribute_score())),
                Self::Tuple(data_types) => {
                    Self::Tuple(data_types.into_iter().map(Self::distribute_score).collect())
                }
                Self::TypedCompound(compound) => Self::TypedCompound(
                    compound
                        .into_iter()
                        .map(|(key, data_type)| (key, data_type.distribute_score()))
                        .collect(),
                ),
                Self::Compound(data_type) => Self::Compound(Box::new(data_type.distribute_score())),
                Self::Struct(name, generic_types) => Self::Struct(
                    name,
                    generic_types
                        .into_iter()
                        .map(Self::distribute_score)
                        .collect(),
                ),
                Self::Data(inner) => Self::Data(Box::new(inner.distribute_score())),
                Self::Score(inner) => *inner,
                inner => Self::Score(Box::new(inner)),
            },
            _ => self,
        }
    }

    #[must_use]
    pub fn get_iterable_type(&self) -> Option<Self> {
        Some(match self {
            Self::Reference(self_) => match &**self_ {
                Self::List(data_type) => *data_type.clone(),
                Self::Data(data_type) => data_type.get_iterable_type()?,
                Self::String => Self::String,
                _ => return None,
            },

            Self::List(data_type) => *data_type.clone(),
            Self::Data(data_type) => data_type.get_iterable_type()?,
            Self::String => Self::String,
            _ => return None,
        })
    }

    pub fn as_place_type(self) -> Result<PlaceTypeKind, Self> {
        Ok(match self {
            Self::Score(inner_type) => PlaceTypeKind::Score(*inner_type),
            Self::Data(inner_type) => PlaceTypeKind::Data(*inner_type),
            _ => return Err(self),
        })
    }

    pub fn as_dereferenced_place_type(self) -> Result<PlaceTypeKind, Self> {
        Ok(match self {
            Self::Reference(data_type) => data_type.as_place_type()?,
            Self::Score(inner_type) => PlaceTypeKind::Score(*inner_type),
            Self::Data(inner_type) => PlaceTypeKind::Data(*inner_type),
            _ => return Err(self),
        })
    }

    #[must_use]
    pub fn dereference(self) -> Option<Self> {
        Some(match self {
            Self::Reference(data_type) => *data_type,
            Self::Score(_) | Self::Data(_) => self,

            _ => return None,
        })
    }

    #[must_use]
    pub const fn is_lvalue(&self) -> bool {
        matches!(self, Self::Score(_) | Self::Data(_))
    }

    #[must_use]
    pub fn can_cast_to(&self, data_type: &Self) -> bool {
        match (self, data_type) {
            (Self::Score(self_type), Self::Score(data_type))
            | (Self::Data(self_type), Self::Data(data_type)) => {
                **self_type == Self::SNBT || self_type == data_type
            }

            (Self::SNBT, _)
            | (
                Self::InferredInteger
                | Self::InferredFloat
                | Self::Byte
                | Self::Short
                | Self::Integer
                | Self::Long
                | Self::Float
                | Self::Double,
                Self::Byte | Self::Short | Self::Integer | Self::Long | Self::Float | Self::Double,
            ) => true,

            _ => false,
        }
    }

    #[must_use]
    pub fn is_condition(&self) -> bool {
        match self {
            Self::Boolean => true,

            Self::Data(data_type) | Self::Score(data_type) | Self::Reference(data_type) => {
                data_type.is_condition()
            }

            Self::Generic(_) => unreachable!(),

            _ => false,
        }
    }

    #[must_use]
    pub fn is_score_compatible(
        &self,
        supports_variable_type_scope: &impl SupportsVariableTypeScope,
    ) -> Option<bool> {
        Some(match self {
            Self::Boolean => true,
            t if t.is_numeric() => true,

            Self::Data(data_type)
            | Self::Score(data_type)
            | Self::Reference(data_type)
            | Self::Compound(data_type) => {
                data_type.is_score_compatible(supports_variable_type_scope)?
            }

            Self::Struct(name, generics) => {
                let declaration = supports_variable_type_scope.get_data_type(name)??;
                let fields =
                    declaration.get_struct_fields(supports_variable_type_scope, generics)?;
                fields
                    .values()
                    .map(|dt| dt.is_score_compatible(supports_variable_type_scope))
                    .run_all_succeeded_true()?
            }
            _ => false,
        })
    }

    #[must_use]
    pub fn is_snbt_like(&self) -> bool {
        match self {
            Self::TypedCompound(compound) => compound.values().all(Self::is_snbt_like),
            Self::List(data_type) | Self::Compound(data_type) => data_type.is_snbt_like(),
            Self::Boolean
            | Self::Byte
            | Self::Short
            | Self::Integer
            | Self::InferredInteger
            | Self::Long
            | Self::Float
            | Self::InferredFloat
            | Self::Double
            | Self::String
            | Self::SNBT
            | Self::Struct(_, _) => true,
            Self::Generic(_) => unreachable!(),
            _ => false,
        }
    }

    #[must_use]
    pub fn has_fields(&self) -> bool {
        match self {
            Self::Reference(data_type) | Self::Score(data_type) => data_type.has_fields(),

            Self::Struct(_, _)
            | Self::TypedCompound(_)
            | Self::Compound(_)
            | Self::Data(_)
            | Self::Tuple(_)
            | Self::SNBT => true,

            Self::Generic(_) => unreachable!(),

            _ => false,
        }
    }

    fn raw_get_arithmetic_result(
        &self,
        supports_variable_type_score: &impl SupportsVariableTypeScope,
        _operator: ArithmeticOperator,
        other: &Self,
    ) -> Option<Self> {
        if self.is_numeric() && other.is_numeric() {
            return Some(match (self, other) {
                (Self::InferredInteger, other) | (other, Self::InferredInteger) => {
                    if matches!(other, Self::InferredFloat) {
                        Self::Float
                    } else {
                        other.clone()
                    }
                }

                (Self::InferredFloat, other) | (other, Self::InferredFloat) => {
                    if matches!(other, Self::Double) {
                        Self::Double
                    } else {
                        Self::Float
                    }
                }

                (Self::Double, _) | (_, Self::Double) => Self::Double,
                (Self::Float, _) | (_, Self::Float) => Self::Float,
                (Self::Long, _) | (_, Self::Long) => Self::Long,
                (Self::Integer, _) | (_, Self::Integer) => Self::Integer,
                (Self::Short, _) | (_, Self::Short) => Self::Short,
                (Self::Byte, _) | (_, Self::Byte) => Self::Byte,

                _ => return None,
            });
        }

        match (self, other) {
            (Self::Score(inner), other) | (other, Self::Score(inner))
                if other.is_score_compatible(supports_variable_type_score)?
                    && inner.is_score_compatible(supports_variable_type_score)? =>
            {
                Some(Self::Score(Box::new(Self::Integer)))
            }
            (Self::Data(inner), other) | (other, Self::Data(inner))
                if other.is_score_compatible(supports_variable_type_score)?
                    && inner.is_score_compatible(supports_variable_type_score)? =>
            {
                Some(Self::Score(Box::new(Self::Integer)))
            }
            _ => None,
        }
    }

    #[must_use]
    pub fn get_arithmetic_result(
        &self,
        supports_variable_type_scope: &impl SupportsVariableTypeScope,
        operator: ArithmeticOperator,
        other: &Self,
    ) -> Option<Self> {
        match (self, other) {
            (Self::Reference(self_), other) | (other, Self::Reference(self_)) => {
                self_.raw_get_arithmetic_result(supports_variable_type_scope, operator, other)
            }

            _ => self.raw_get_arithmetic_result(supports_variable_type_scope, operator, other),
        }
    }

    #[must_use]
    pub fn can_perform_augmented_assignment(
        &self,
        operator: ArithmeticOperator,
        other: &Self,
    ) -> bool {
        match (self, other) {
            (_, other) if operator == ArithmeticOperator::Swap && !other.is_lvalue() => false,
            (Self::Byte | Self::InferredInteger, Self::Byte | Self::InferredInteger)
            | (Self::Short | Self::InferredInteger, Self::Short | Self::InferredInteger)
            | (Self::Integer | Self::InferredInteger, Self::Integer | Self::InferredInteger)
            | (Self::Long | Self::InferredInteger, Self::Long | Self::InferredInteger) => true,
            (Self::Data(inner) | Self::Score(inner), other)
            | (other, Self::Data(inner) | Self::Score(inner)) => {
                inner.can_perform_augmented_assignment(operator, other)
            }
            _ => false,
        }
    }

    #[must_use]
    fn raw_can_perform_comparison(
        &self,
        supports_variable_type_score: &impl SupportsVariableTypeScope,
        operator: ComparisonOperator,
        other: &Self,
    ) -> Option<bool> {
        if self.is_numeric() && other.is_numeric() {
            return Some(true);
        }

        match (self, other) {
            (Self::Score(inner), other_type) | (other_type, Self::Score(inner))
                if inner.is_score_compatible(supports_variable_type_score)?
                    && other_type.is_score_compatible(supports_variable_type_score)? =>
            {
                inner.can_perform_comparison(supports_variable_type_score, operator, other_type)
            }
            (Self::Data(inner), other) | (other, Self::Data(inner))
                if (operator == ComparisonOperator::EqualTo
                    || operator == ComparisonOperator::NotEqualTo)
                    || (inner.is_score_compatible(supports_variable_type_score)?
                        && other.is_score_compatible(supports_variable_type_score)?) =>
            {
                inner.can_perform_comparison(supports_variable_type_score, operator, other)
            }
            _ => Some(false),
        }
    }

    #[must_use]
    pub fn can_perform_comparison(
        &self,
        supports_variable_type_score: &impl SupportsVariableTypeScope,
        operator: ComparisonOperator,
        other: &Self,
    ) -> Option<bool> {
        if (operator == ComparisonOperator::EqualTo || operator == ComparisonOperator::NotEqualTo)
            && self.equals(other)
        {
            return Some(true);
        }

        Some(match (self, other) {
            (Self::Reference(data_type), other) | (other, Self::Reference(data_type)) => data_type
                .raw_can_perform_comparison(supports_variable_type_score, operator, other)?,

            _ => self.raw_can_perform_comparison(supports_variable_type_score, operator, other)?,
        })
    }

    #[must_use]
    pub fn get_index_result(&self) -> Option<Self> {
        Some(match self {
            Self::Reference(self_) => self_.get_index_result()?,

            Self::List(data_type) => *data_type.clone(),
            Self::Data(data_type) => Self::Data(Box::new(data_type.get_index_result()?)),
            Self::SNBT => Self::SNBT,

            _ => return None,
        })
    }

    pub fn get_field_result(
        &self,
        supports_variable_type_scope: &impl SupportsVariableTypeScope,
        field: &str,
    ) -> Option<Self> {
        Some(match self {
            Self::Reference(self_) => {
                self_.get_field_result(supports_variable_type_scope, field)?
            }

            Self::Struct(name, generics) => {
                let declaration = supports_variable_type_scope.get_data_type(name)??;

                let fields =
                    declaration.get_struct_fields(supports_variable_type_scope, generics)?;

                fields.get(field).cloned()?
            }

            Self::TypedCompound(compound) => {
                compound.iter().find(|(key, _)| key.1 == *field)?.1.clone()
            }
            Self::Compound(data_type) => *data_type.clone(),
            Self::Data(data_type) => Self::Data(Box::new(
                data_type.get_field_result(supports_variable_type_scope, field)?,
            )),
            Self::Score(data_type) => Self::Score(Box::new(
                data_type.get_field_result(supports_variable_type_scope, field)?,
            )),
            Self::Tuple(items) => {
                return field
                    .parse::<i32>()
                    .map_or(None, |index| items.get(index as usize).cloned());
            }

            Self::SNBT => Self::SNBT,

            _ => return None,
        })
    }

    #[must_use]
    pub const fn can_be_referenced(&self) -> bool {
        false
    }

    #[must_use]
    pub fn get_dereferenced_result(&self) -> Option<Self> {
        Some(match self {
            Self::Reference(data_type) => *data_type.clone(),
            Self::Score(_) | Self::Data(_) => self.clone(),
            _ => return None,
        })
    }

    // TODO maybe create a can_be_negated method?
    #[must_use]
    pub fn get_negation_result(&self) -> Option<Self> {
        Some(match self {
            Self::Reference(self_) => match **self_ {
                Self::Byte => Self::Byte,
                Self::Short => Self::Short,
                Self::Integer | Self::InferredInteger => Self::Integer,
                Self::Long => Self::Long,
                Self::Score(ref inner_type) => Self::Score(inner_type.clone()),
                _ => return None,
            },

            Self::Byte => Self::Byte,
            Self::Short => Self::Short,
            Self::Integer | Self::InferredInteger => Self::Integer,
            Self::Long => Self::Long,
            Self::Score(inner_type) => Self::Score(inner_type.clone()),
            _ => return None,
        })
    }

    #[must_use]
    pub fn get_inverted_result(&self) -> Option<Self> {
        Some(match self {
            Self::Reference(self_) => match **self_ {
                Self::Boolean => Self::Boolean,
                _ => return None,
            },

            Self::Boolean => Self::Boolean,

            _ => return None,
        })
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
