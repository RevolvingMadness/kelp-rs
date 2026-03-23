use std::{collections::BTreeMap, fmt::Write};

use minecraft_command_types::snbt::SNBTString as LowSNBTString;

use crate::{
    high::semantic_analysis_context::{
        SemanticAnalysisContext, info::error::SemanticAnalysisError,
    },
    middle::environment::{Environment, r#type::r#struct::StructId},
    operator::{ArithmeticOperator, ComparisonOperator},
    place::PlaceTypeKind,
    span::Span,
    visibility::Visibility,
};

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
    Score(Box<Self>),
    List(Box<Self>),
    TypedCompound(BTreeMap<LowSNBTString, Self>),
    Compound(Box<Self>),
    Data(Box<Self>),
    Reference(Box<Self>),
    Tuple(Vec<Self>),
    SNBT,
    Struct(StructId),
    Inferred,
    InferredInteger,
    InferredFloat,
}

impl DataType {
    #[must_use]
    pub fn can_be_represented_as_snbt_float_macro(&self) -> bool {
        match self {
            Self::Score(data_type) | Self::Data(data_type) | Self::Reference(data_type) => {
                data_type.can_be_represented_as_snbt_float_macro()
            }
            Self::Byte
            | Self::Short
            | Self::Integer
            | Self::Long
            | Self::Float
            | Self::Double
            | Self::InferredInteger
            | Self::InferredFloat => true,
            Self::Boolean
            | Self::String
            | Self::Unit
            | Self::List(_)
            | Self::TypedCompound(_)
            | Self::Compound(_)
            | Self::Tuple(_)
            | Self::SNBT
            | Self::Struct(_)
            | Self::Inferred => false,
        }
    }

    #[must_use]
    pub fn as_struct_id(self) -> Option<StructId> {
        let Self::Struct(id) = self else {
            return None;
        };

        Some(id)
    }

    #[must_use]
    pub fn as_struct_id_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        name_span: Span,
        name: &str,
    ) -> Option<StructId> {
        let Self::Struct(id) = self else {
            return ctx.add_error(
                name_span,
                SemanticAnalysisError::NotARegularStruct(name.to_owned()),
            );
        };

        Some(id)
    }

    #[must_use]
    pub fn as_struct_struct_id_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        name_span: Span,
        name: &str,
    ) -> Option<StructId> {
        let Self::Struct(id) = self else {
            return ctx.add_error(
                name_span,
                SemanticAnalysisError::NotARegularStruct(name.to_owned()),
            );
        };

        Some(id)
    }

    pub fn unwrap_all_apply_predicate_all(
        &self,
        environment: &Environment,
        predicate: fn(&Self) -> bool,
    ) -> bool {
        match self {
            Self::TypedCompound(compound) => compound.values().all(predicate),
            Self::Score(data_type)
            | Self::List(data_type)
            | Self::Compound(data_type)
            | Self::Data(data_type)
            | Self::Reference(data_type) => predicate(data_type),
            Self::Tuple(data_types) => data_types.iter().all(predicate),
            Self::Struct(id) => {
                let (_, _, declaration) = environment.get_struct(*id);

                declaration.field_types().all(predicate)
            }
            Self::Boolean
            | Self::Byte
            | Self::Short
            | Self::Integer
            | Self::Long
            | Self::Float
            | Self::Double
            | Self::String
            | Self::Unit
            | Self::SNBT
            | Self::Inferred
            | Self::InferredInteger
            | Self::InferredFloat => predicate(self),
        }
    }

    #[must_use]
    pub fn reduce(self, other: &Self) -> Option<Self> {
        if self.equals(other) {
            if self == Self::Inferred {
                return Some(other.clone());
            }

            if *other == Self::Inferred {
                return Some(self);
            }

            if self == Self::InferredInteger {
                return Some(other.clone());
            }

            if *other == Self::InferredInteger {
                return Some(self);
            }

            if self == Self::InferredFloat {
                return Some(other.clone());
            }

            if *other == Self::InferredFloat {
                return Some(self);
            }

            return Some(self);
        }

        match (self, other) {
            (Self::Inferred, other) => Some(other.clone()),
            (this, Self::Inferred) => Some(this),

            (Self::InferredInteger, other) if other.is_integer_like() => Some(other.clone()),
            (this, Self::InferredInteger) if this.is_integer_like() => Some(this),

            (Self::InferredFloat, other) if other.is_float_like() => Some(other.clone()),
            (this, Self::InferredFloat) if this.is_float_like() => Some(this),

            (Self::SNBT, _) | (_, Self::SNBT) => Some(Self::SNBT),

            (Self::Data(inner), other) => inner
                .reduce(other)
                .map_or(Some(Self::SNBT), |reduced_inner| {
                    Some(Self::Data(Box::new(reduced_inner)))
                }),
            (this, Self::Data(inner)) => this
                .reduce(inner)
                .map_or(Some(Self::SNBT), |reduced_inner| {
                    Some(Self::Data(Box::new(reduced_inner)))
                }),

            (Self::Score(inner), other) => inner
                .reduce(other)
                .map_or(Some(Self::SNBT), |reduced_inner| {
                    Some(Self::Score(Box::new(reduced_inner)))
                }),
            (this, Self::Score(inner)) => this
                .reduce(inner)
                .map_or(Some(Self::SNBT), |reduced_inner| {
                    Some(Self::Score(Box::new(reduced_inner)))
                }),

            (Self::Reference(a), Self::Reference(b)) => {
                Some(Self::Reference(Box::new(a.reduce(b)?)))
            }

            (Self::List(a), Self::List(b)) => Some(Self::List(Box::new(a.reduce(b)?))),

            (Self::Compound(a), Self::Compound(b)) => Some(Self::Compound(Box::new(a.reduce(b)?))),

            (Self::TypedCompound(map), Self::Compound(inner)) => {
                if map.values().all(|v| v.equals(inner)) {
                    Some(Self::Compound(inner.clone()))
                } else {
                    None
                }
            }

            (Self::Compound(inner), Self::TypedCompound(map)) => {
                if map.values().all(|v| v.equals(&inner)) {
                    Some(Self::Compound(inner))
                } else {
                    None
                }
            }

            (Self::TypedCompound(a_map), Self::TypedCompound(b_map)) => {
                if a_map.len() != b_map.len() {
                    return None;
                }
                let mut new_map = BTreeMap::new();
                for (key, a_val) in a_map {
                    let b_val = b_map.get(&key)?;
                    let reduced = a_val.reduce(b_val)?;
                    new_map.insert(key, reduced);
                }
                Some(Self::TypedCompound(new_map))
            }

            (Self::Tuple(a_vec), Self::Tuple(b_vec)) => {
                if a_vec.len() != b_vec.len() {
                    return None;
                }
                let mut new_vec = Vec::new();
                for (a, b) in a_vec.into_iter().zip(b_vec.iter()) {
                    new_vec.push(a.reduce(b)?);
                }
                Some(Self::Tuple(new_vec))
            }

            (Self::Struct(a_id), Self::Struct(b_id)) if a_id == *b_id => Some(Self::Struct(a_id)),

            _ => None,
        }
    }

    #[must_use]
    pub fn format_string(&self, environment: &Environment) -> String {
        let mut output = String::new();

        let _ = self.write_string(&mut output, environment);

        output
    }

    pub fn write_string(&self, output: &mut String, environment: &Environment) -> std::fmt::Result {
        match self {
            Self::Boolean => output.write_str("boolean"),
            Self::Byte => output.write_str("byte"),
            Self::Short => output.write_str("short"),
            Self::Integer => output.write_str("integer"),
            Self::Long => output.write_str("long"),
            Self::Float => output.write_str("float"),
            Self::Double => output.write_str("double"),
            Self::String => output.write_str("string"),
            Self::Unit => output.write_str("()"),
            Self::Score(data_type) => {
                output.write_str("score<")?;
                data_type.write_string(output, environment)?;
                output.write_char('>')?;

                Ok(())
            }
            Self::List(data_type) => {
                output.write_str("list<")?;
                data_type.write_string(output, environment)?;
                output.write_char('>')?;

                Ok(())
            }
            Self::TypedCompound(compound) => {
                output.write_char('{')?;

                if !compound.is_empty() {
                    output.write_char(' ')?;
                }

                for (i, (key, value_data_type)) in compound.iter().enumerate() {
                    if i != 0 {
                        output.write_str(", ")?;
                    }

                    write!(output, "{}: ", key.1)?;
                    value_data_type.write_string(output, environment)?;
                }

                if !compound.is_empty() {
                    output.write_char(' ')?;
                }

                output.write_char('}')?;

                Ok(())
            }
            Self::Compound(data_type) => {
                output.write_str("compound<")?;
                data_type.write_string(output, environment)?;
                output.write_char('>')?;

                Ok(())
            }
            Self::Data(data_type) => {
                output.write_str("data<")?;
                data_type.write_string(output, environment)?;
                output.write_char('>')?;

                Ok(())
            }
            Self::Reference(data_type) => {
                output.write_char('&')?;
                data_type.write_string(output, environment)?;

                Ok(())
            }
            Self::Tuple(data_types) => {
                output.write_char('(')?;

                for (i, data_type) in data_types.iter().enumerate() {
                    if i != 0 {
                        output.write_str(", ")?;
                    }

                    data_type.write_string(output, environment)?;
                }

                output.write_char(')')?;

                Ok(())
            }
            Self::SNBT => output.write_str("snbt"),
            Self::Struct(id) => {
                // TODO: Maybe display full path?

                let (_, _, declaration) = environment.get_struct(*id);

                output.write_str(declaration.name())?;

                let generic_types = declaration.generic_types();

                if !generic_types.is_empty() {
                    output.write_str("<")?;

                    for (i, data_type) in generic_types.iter().enumerate() {
                        if i != 0 {
                            output.write_str(", ")?;
                        }

                        data_type.write_string(output, environment)?;
                    }

                    output.write_str(">")?;
                }

                Ok(())
            }
            Self::Inferred => output.write_char('_'),
            Self::InferredInteger => output.write_str("{integer}"),
            Self::InferredFloat => output.write_str("{float}"),
        }
    }
}

impl DataType {
    #[must_use]
    #[allow(clippy::type_complexity)]
    pub fn unwrap(self) -> Self {
        match self {
            Self::Score(data_type) | Self::Data(data_type) | Self::Reference(data_type) => {
                *data_type
            }

            Self::SNBT
            | Self::Boolean
            | Self::Byte
            | Self::Short
            | Self::Integer
            | Self::Long
            | Self::Float
            | Self::Double
            | Self::String
            | Self::Unit
            | Self::List(_)
            | Self::TypedCompound(_)
            | Self::Compound(_)
            | Self::Tuple(_)
            | Self::Struct(_)
            | Self::Inferred
            | Self::InferredInteger
            | Self::InferredFloat => self,
        }
    }

    #[must_use]
    #[allow(clippy::type_complexity)]
    pub fn unwrap_single(self) -> (Option<fn(Box<Self>) -> Self>, Self) {
        match self {
            Self::Score(data_type) => (Some(Self::Score), *data_type),
            Self::Data(data_type) => (Some(Self::Data), *data_type),
            Self::Reference(data_type) => (Some(Self::Reference), *data_type),

            Self::SNBT
            | Self::Boolean
            | Self::Byte
            | Self::Short
            | Self::Integer
            | Self::Long
            | Self::Float
            | Self::Double
            | Self::String
            | Self::Unit
            | Self::List(_)
            | Self::TypedCompound(_)
            | Self::Compound(_)
            | Self::Tuple(_)
            | Self::Struct(_)
            | Self::Inferred
            | Self::InferredInteger
            | Self::InferredFloat => (None, self),
        }
    }

    #[allow(clippy::type_complexity)]
    #[must_use]
    pub fn unwrap_all(mut self) -> (Vec<fn(Box<Self>) -> Self>, Self) {
        let mut wrappers = Vec::new();

        loop {
            let (wrapper, inner) = self.unwrap_single();

            if let Some(wrapper) = wrapper {
                wrappers.push(wrapper);

                self = inner;
            } else {
                return (wrappers, inner);
            }
        }
    }

    #[must_use]
    pub fn wrap_all(mut self, wrappers: &[fn(Box<Self>) -> Self]) -> Self {
        for wrapper in wrappers.iter().rev() {
            self = wrapper(Box::new(self));
        }

        self
    }

    #[must_use]
    pub fn distribute(self) -> Self {
        match self {
            Self::Data(_) => self.distribute_data(),
            Self::Reference(_) => self.distribute_references(),
            Self::Score(_) => self.distribute_score(),
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
    pub fn is_compiletime(&self, environment: &Environment) -> bool {
        match self {
            Self::Score(_) | Self::Data(_) => false,
            Self::Reference(data_type) => data_type.is_compiletime(environment),
            Self::Struct(id) => {
                let (_, _, declaration) = environment.get_struct(*id);

                declaration.field_types().all(|field_type| field_type.is_compiletime(environment))
            },
            Self::Boolean
            | Self::Byte
            | Self::Short
            | Self::Integer
            | Self::Long
            | Self::Float
            | Self::Double
            | Self::String
            | Self::Unit
            | Self::List(_)
            | Self::TypedCompound(_)
            | Self::Compound(_)
            | Self::Tuple(_)
            | Self::SNBT
            | Self::Inferred // TODO
            | Self::InferredInteger
            | Self::InferredFloat => true,
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
            Self::Tuple(data_types) => {
                Self::Tuple(data_types.into_iter().map(Self::to_data).collect())
            }
            Self::SNBT => Self::SNBT,
            Self::Struct(id) => Self::Struct(id),
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
            Self::Struct(id) => Self::Struct(id),
            _ => return None,
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
                Self::Struct(id) => Self::Struct(id),
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
                Self::Data(inner) => Self::Data(Box::new(inner.distribute_score())),
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

    pub fn dereference(self) -> Result<Self, Self> {
        Ok(match self {
            Self::Reference(data_type) => *data_type,
            Self::Score(_) | Self::Data(_) => self,

            _ => return Err(self),
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
            | (Self::Data(self_type), Self::Data(data_type)) => self_type.can_cast_to(data_type),

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

            _ => false,
        }
    }

    #[must_use]
    pub fn is_score_compatible(&self) -> bool {
        match self {
            Self::Boolean => true,
            t if t.is_numeric() => true,

            Self::Data(data_type) | Self::Score(data_type) | Self::Reference(data_type) => {
                data_type.is_score_compatible()
            }

            _ => false,
        }
    }

    #[must_use]
    pub fn has_fields(&self) -> bool {
        match self {
            Self::Reference(data_type) | Self::Score(data_type) => data_type.has_fields(),

            Self::Struct(_)
            | Self::TypedCompound(_)
            | Self::Compound(_)
            | Self::Data(_)
            | Self::Tuple(_)
            | Self::SNBT => true,

            _ => false,
        }
    }

    fn raw_get_arithmetic_result(
        &self,
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
                if other.is_score_compatible() && inner.is_score_compatible() =>
            {
                Some(Self::Score(Box::new(Self::Integer)))
            }
            (Self::Data(inner), other) | (other, Self::Data(inner))
                if other.is_score_compatible() && inner.is_score_compatible() =>
            {
                Some(Self::Score(Box::new(Self::Integer)))
            }
            _ => None,
        }
    }

    #[must_use]
    pub fn get_arithmetic_result(
        &self,
        operator: ArithmeticOperator,
        other: &Self,
    ) -> Option<Self> {
        match (self, other) {
            (Self::Reference(self_), other) | (other, Self::Reference(self_)) => {
                self_.raw_get_arithmetic_result(operator, other)
            }

            _ => self.raw_get_arithmetic_result(operator, other),
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
        environment: &Environment,
        operator: ComparisonOperator,
        other: &Self,
    ) -> Option<bool> {
        if self.is_numeric() && other.is_numeric() {
            return Some(true);
        }

        match (self, other) {
            (Self::Score(inner), other_type) | (other_type, Self::Score(inner))
                if inner.is_score_compatible() && other_type.is_score_compatible() =>
            {
                inner.can_perform_comparison(environment, operator, other_type)
            }
            (Self::Data(inner), other) | (other, Self::Data(inner))
                if (operator == ComparisonOperator::EqualTo
                    || operator == ComparisonOperator::NotEqualTo)
                    || (inner.is_score_compatible() && other.is_score_compatible()) =>
            {
                inner.can_perform_comparison(environment, operator, other)
            }
            _ => Some(false),
        }
    }

    #[must_use]
    pub fn can_perform_comparison(
        &self,
        environment: &Environment,
        operator: ComparisonOperator,
        other: &Self,
    ) -> Option<bool> {
        if (operator == ComparisonOperator::EqualTo || operator == ComparisonOperator::NotEqualTo)
            && self.equals(other)
        {
            return Some(true);
        }

        Some(match (self, other) {
            (Self::Reference(data_type), other) | (other, Self::Reference(data_type)) => {
                data_type.raw_can_perform_comparison(environment, operator, other)?
            }

            _ => self.raw_can_perform_comparison(environment, operator, other)?,
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

    #[must_use]
    pub fn get_field_result(&self, environment: &Environment, field: &str) -> Option<Self> {
        Some(match self {
            Self::Reference(self_) => self_.get_field_result(environment, field)?,

            Self::Struct(id) => {
                let (visibility, _, declaration) = environment.get_struct(*id);

                if visibility != Visibility::Public {
                    return None;
                }

                declaration.get_field(field).cloned()?
            }

            Self::TypedCompound(compound) => {
                compound.iter().find(|(key, _)| key.1 == *field)?.1.clone()
            }
            Self::Compound(data_type) => *data_type.clone(),
            Self::Data(data_type) => {
                Self::Data(Box::new(data_type.get_field_result(environment, field)?))
            }
            Self::Score(data_type) => {
                Self::Score(Box::new(data_type.get_field_result(environment, field)?))
            }
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
