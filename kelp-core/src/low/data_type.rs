use std::{
    collections::BTreeMap,
    fmt::{Display, Write},
};

use minecraft_command_types::snbt::SNBTString as LowSNBTString;

use crate::{
    high::{
        data_type::resolved::GenericResolver,
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    },
    low::environment::{Environment, r#type::r#struct::StructId, value::function::FunctionId},
    operator::{ArithmeticOperator, ComparisonOperator},
    runtime_storage::RuntimeStorageType,
    span::Span,
};

#[derive(Debug, Clone)]
pub struct CallInfo {
    pub return_type: DataType,
    pub parameter_count: usize,
}

pub struct DataTypeDisplay<'a> {
    pub data_type: &'a DataType,
    pub environment: &'a Environment,
}

impl Display for DataTypeDisplay<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.data_type {
            DataType::Boolean => f.write_str("boolean"),
            DataType::Byte => f.write_str("byte"),
            DataType::Short => f.write_str("short"),
            DataType::Integer => f.write_str("integer"),
            DataType::Long => f.write_str("long"),
            DataType::Float => f.write_str("float"),
            DataType::Double => f.write_str("double"),
            DataType::String => f.write_str("string"),
            DataType::Unit => f.write_str("()"),
            DataType::Never => f.write_char('!'),
            DataType::Score(data_type) => {
                f.write_str("score<")?;
                data_type.display(self.environment).fmt(f)?;
                f.write_char('>')?;

                Ok(())
            }
            DataType::List(data_type) => {
                f.write_str("list<")?;
                data_type.display(self.environment).fmt(f)?;
                f.write_char('>')?;

                Ok(())
            }
            DataType::TypedCompound(compound) => {
                f.write_char('{')?;

                if !compound.is_empty() {
                    f.write_char(' ')?;
                }

                for (i, (key, value_data_type)) in compound.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    write!(f, "{}: ", key.1)?;

                    value_data_type.display(self.environment).fmt(f)?;
                }

                if !compound.is_empty() {
                    f.write_char(' ')?;
                }

                f.write_char('}')?;

                Ok(())
            }
            DataType::Compound(data_type) => {
                f.write_str("compound<")?;
                data_type.display(self.environment).fmt(f)?;
                f.write_char('>')?;

                Ok(())
            }
            DataType::Data(data_type) => {
                f.write_str("data<")?;
                data_type.display(self.environment).fmt(f)?;
                f.write_char('>')?;

                Ok(())
            }
            DataType::Reference(data_type) => {
                f.write_char('&')?;
                data_type.display(self.environment).fmt(f)?;

                Ok(())
            }
            DataType::Tuple(data_types) => {
                f.write_char('(')?;

                for (i, data_type) in data_types.iter().enumerate() {
                    if i != 0 {
                        f.write_str(", ")?;
                    }

                    data_type.display(self.environment).fmt(f)?;
                }

                f.write_char(')')?;

                Ok(())
            }
            DataType::SNBT => f.write_str("snbt"),
            DataType::Generic(name) => name.fmt(f),
            DataType::Function(id) => {
                // TODO: Maybe display full path?

                let (_, _, declaration) = self.environment.get_function(*id);

                write!(f, "fn {}", declaration.name)?;

                if !declaration.generic_types.is_empty() {
                    f.write_str("<")?;

                    for (i, data_type) in declaration.generic_types.iter().enumerate() {
                        if i != 0 {
                            f.write_str(", ")?;
                        }

                        data_type.display(self.environment).fmt(f)?;
                    }

                    f.write_str(">")?;
                }

                f.write_char('(')?;

                if let Some(parameters) = &declaration.parameters {
                    for (i, (_, data_type)) in parameters.iter().enumerate() {
                        if i != 0 {
                            f.write_str(", ")?;
                        }

                        data_type.display(self.environment).fmt(f)?;
                    }
                }

                write!(
                    f,
                    ") -> {}",
                    declaration.return_type.display(self.environment)
                )?;

                Ok(())
            }
            DataType::Struct(id) => {
                // TODO: Maybe display full path?

                let (_, _, declaration) = self.environment.get_struct(*id);

                f.write_str(declaration.name())?;

                let generic_types = declaration.generic_types();

                if !generic_types.is_empty() {
                    f.write_str("<")?;

                    for (i, data_type) in generic_types.iter().enumerate() {
                        if i != 0 {
                            f.write_str(", ")?;
                        }

                        data_type.display(self.environment).fmt(f)?;
                    }

                    f.write_str(">")?;
                }

                Ok(())
            }
            DataType::Inferred => f.write_char('_'),
            DataType::InferredInteger => f.write_str("{integer}"),
            DataType::InferredFloat => f.write_str("{float}"),
            DataType::ResourceLocation => f.write_str("resource_location"),
            DataType::EntitySelector => f.write_str("entity_selector"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeWrapper {
    Reference,
    Score,
    Data,
}

impl TypeWrapper {
    #[must_use]
    pub fn wrap(self, inner: DataType) -> DataType {
        match self {
            Self::Reference => DataType::Reference(Box::new(inner)),
            Self::Score => DataType::Score(Box::new(inner)),
            Self::Data => DataType::Data(Box::new(inner)),
        }
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
    Never,
    Score(Box<Self>),
    List(Box<Self>),
    TypedCompound(BTreeMap<LowSNBTString, Self>),
    Compound(Box<Self>),
    Data(Box<Self>),
    Reference(Box<Self>),
    Tuple(Vec<Self>),
    SNBT,
    Generic(String),
    Function(FunctionId),
    Struct(StructId),
    Inferred,
    InferredInteger,
    InferredFloat,
    ResourceLocation,
    EntitySelector,
}

impl DataType {
    #[must_use]
    pub fn resolve_fully(self, resolver: &GenericResolver) -> Option<Self> {
        Some(match self {
            Self::Score(data_type) => Self::Score(Box::new(data_type.resolve_fully(resolver)?)),
            Self::List(data_type) => Self::List(Box::new(data_type.resolve_fully(resolver)?)),
            Self::TypedCompound(compound) => {
                let compound = compound
                    .into_iter()
                    .map(|(key, data_type)| Some((key, data_type.resolve_fully(resolver)?)))
                    .collect::<Option<_>>()?;

                Self::TypedCompound(compound)
            }
            Self::Compound(data_type) => {
                Self::Compound(Box::new(data_type.resolve_fully(resolver)?))
            }
            Self::Data(data_type) => Self::Data(Box::new(data_type.resolve_fully(resolver)?)),
            Self::Reference(data_type) => {
                Self::Reference(Box::new(data_type.resolve_fully(resolver)?))
            }
            Self::Tuple(data_types) => {
                let data_types = data_types
                    .into_iter()
                    .map(|data_type| data_type.resolve_fully(resolver))
                    .collect::<Option<_>>()?;

                Self::Tuple(data_types)
            }
            Self::Generic(name) => resolver.resolve(&name)?.clone(),
            _ => self,
        })
    }

    #[must_use]
    pub fn get_call_info(&self, environment: &Environment) -> Option<CallInfo> {
        Some(match self {
            Self::Function(id) => {
                let (_, _, declaration) = environment.get_function(*id);

                CallInfo {
                    return_type: declaration.return_type.clone(),
                    parameter_count: declaration.parameters.as_ref().map_or(0, Vec::len),
                }
            }
            Self::ResourceLocation => CallInfo {
                return_type: Self::Integer,
                parameter_count: 0,
            },
            _ => return None,
        })
    }

    #[must_use]
    pub fn get_runtime_storage_type(&self) -> RuntimeStorageType {
        match self {
            Self::Boolean
            | Self::Byte
            | Self::Short
            | Self::Integer
            | Self::InferredInteger
            | Self::Score(_) => RuntimeStorageType::Score,
            Self::Reference(data_type) => data_type.get_runtime_storage_type(),
            _ => RuntimeStorageType::Data,
        }
    }

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
            _ => false,
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
                SemanticAnalysisError::NotAStruct(name.to_owned()),
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
            _ => predicate(self),
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
    pub const fn display<'a>(&'a self, environment: &'a Environment) -> DataTypeDisplay<'a> {
        DataTypeDisplay {
            data_type: self,
            environment,
        }
    }
}

impl DataType {
    #[must_use]
    pub fn unwrap_single(self) -> (Option<TypeWrapper>, Self) {
        match self {
            Self::Reference(inner) => (Some(TypeWrapper::Reference), *inner),
            Self::Score(inner) => (Some(TypeWrapper::Score), *inner),
            Self::Data(inner) => (Some(TypeWrapper::Data), *inner),
            _ => (None, self),
        }
    }

    #[must_use]
    pub fn unwrap_single_reference(&self) -> Option<&Self> {
        Some(match self {
            Self::Reference(inner) => inner,
            Self::Score(inner) => inner,
            Self::Data(inner) => inner,
            _ => return None,
        })
    }

    #[must_use]
    pub fn unwrap_all(mut self) -> (Vec<TypeWrapper>, Self) {
        let mut wrappers = Vec::new();

        loop {
            let (wrapper, inner) = self.unwrap_single();

            let Some(wrapper) = wrapper else {
                return (wrappers, inner);
            };

            wrappers.push(wrapper);

            self = inner;
        }
    }

    #[must_use]
    pub fn unwrap_all_reference(&self) -> &Self {
        let mut current = self;

        loop {
            let inner = current.unwrap_single_reference();

            let Some(inner) = inner else {
                return current;
            };

            current = inner;
        }
    }

    #[must_use]
    pub fn wrap_all(mut self, wrappers: &[TypeWrapper]) -> Self {
        for wrapper in wrappers.iter().rev() {
            self = wrapper.wrap(self);
        }

        self
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
    pub const fn is_float_like(&self) -> bool {
        matches!(self, Self::Float | Self::Double | Self::InferredFloat)
    }

    #[must_use]
    pub const fn is_numeric(&self) -> bool {
        self.is_integer_like() || self.is_float_like()
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
            | Self::Never
            | Self::Unit
            | Self::List(_)
            | Self::TypedCompound(_)
            | Self::Compound(_)
            | Self::Tuple(_)
            | Self::Generic(_)
            | Self::Function(_)
            | Self::SNBT
            | Self::ResourceLocation
            | Self::EntitySelector
            | Self::Inferred // TODO
            | Self::InferredInteger
            | Self::InferredFloat => true,
        }
    }

    #[must_use]
    pub fn distribute_wrapper(self, wrapper: TypeWrapper) -> Self {
        let (outer, inner) = self.unwrap_single();

        if Some(wrapper) != outer {
            return inner;
        }

        let distributed_inner = inner.distribute_wrapper(wrapper);

        match distributed_inner {
            Self::List(data_type) => Self::List(Box::new(wrapper.wrap(*data_type))),
            Self::Tuple(data_types) => {
                let data_types = data_types
                    .into_iter()
                    .map(|data_type| wrapper.wrap(data_type))
                    .collect();

                Self::Tuple(data_types)
            }
            Self::TypedCompound(compound) => {
                let compound = compound
                    .into_iter()
                    .map(|(key, dt)| (key, wrapper.wrap(dt)))
                    .collect();

                Self::TypedCompound(compound)
            }
            Self::Compound(data_type) => {
                let data_type = wrapper.wrap(*data_type);

                Self::Compound(Box::new(data_type))
            }
            Self::Reference(data_type) if wrapper != TypeWrapper::Reference => {
                let data_type = wrapper.wrap(*data_type);

                Self::Reference(Box::new(data_type))
            }
            Self::Data(data_type) if wrapper != TypeWrapper::Data => {
                let data_type = wrapper.wrap(*data_type);

                Self::Data(Box::new(data_type))
            }
            Self::Score(data_type) if wrapper != TypeWrapper::Score => {
                let data_type = wrapper.wrap(*data_type);

                Self::Score(Box::new(data_type))
            }
            Self::Struct(id) if wrapper == TypeWrapper::Reference => Self::Struct(id),
            other => wrapper.wrap(other),
        }
    }

    #[inline]
    #[must_use]
    pub fn distribute_references(self) -> Self {
        self.distribute_wrapper(TypeWrapper::Reference)
    }

    #[inline]
    #[must_use]
    pub fn distribute_score(self) -> Self {
        self.distribute_wrapper(TypeWrapper::Score)
    }

    #[inline]
    #[must_use]
    pub fn distribute_data(self) -> Self {
        self.distribute_wrapper(TypeWrapper::Data)
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
        let data_type = self.unwrap_all_reference();

        matches!(data_type, Self::Boolean)
    }

    #[must_use]
    pub fn is_score_compatible(&self) -> bool {
        let data_type = self.unwrap_all_reference();

        match data_type {
            Self::Boolean => true,
            data_type if data_type.is_numeric() => true,
            _ => false,
        }
    }

    #[must_use]
    pub fn has_fields(&self) -> bool {
        let data_type = self.unwrap_all_reference();

        matches!(
            data_type,
            Self::Struct(_)
                | Self::TypedCompound(_)
                | Self::Compound(_)
                | Self::Data(_)
                | Self::Tuple(_)
                | Self::SNBT
        )
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
                let (_, _, declaration) = environment.get_struct(*id);

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
            | (Self::Score(data_types), Self::Score(other_data_types))
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
