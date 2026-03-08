use std::{collections::BTreeMap, fmt::Display};

use minecraft_command_types::{
    nbt_path::NbtPathNode,
    snbt::{SNBT, SNBTString},
};
use minecraft_command_types_derive::HasMacro;
use strum::{Display, EnumString};

use crate::{
    compile_context::CompileContext,
    datapack::HighDatapack,
    expression::{
        Expression, ExpressionKind, constant::ResolvedExpression,
        supports_variable_type_scope::SupportsVariableTypeScope,
    },
    high::snbt_string::HighSNBTString,
    operator::{ArithmeticOperator, ComparisonOperator, UnaryOperator},
    pattern::{Pattern, PatternKind},
    place::PlaceTypeKind,
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
    span::Span,
    trait_ext::{OptionBoolIterExt, OptionUnitIterExt},
};

pub mod high;

#[derive(Display, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum BuiltinDataTypeKind {
    #[strum(serialize = "boolean", serialize = "bool")]
    Boolean,
    Byte,
    Short,
    #[strum(serialize = "integer", serialize = "int")]
    Integer,
    Long,
    Float,
    Double,
    #[strum(serialize = "string", serialize = "str")]
    String,
    #[strum(serialize = "()", serialize = "unit")]
    Unit,
    Score,
    List,
    Compound,
    Data,
    #[strum(
        serialize = "snbt",
        serialize = "nbt",
        serialize = "SNBT",
        serialize = "NBT"
    )]
    SNBT,
}

impl BuiltinDataTypeKind {
    #[must_use]
    pub fn to_data_type(&self, generic_types: &[DataTypeKind]) -> Option<DataTypeKind> {
        if generic_types.len() != self.generic_count() {
            return None;
        }

        Some(match self {
            Self::Unit => DataTypeKind::Unit,
            Self::Boolean => DataTypeKind::Boolean,
            Self::Byte => DataTypeKind::Byte,
            Self::Short => DataTypeKind::Short,
            Self::Integer => DataTypeKind::Integer,
            Self::Long => DataTypeKind::Long,
            Self::Float => DataTypeKind::Float,
            Self::Double => DataTypeKind::Double,
            Self::String => DataTypeKind::String,
            Self::SNBT => DataTypeKind::SNBT,
            Self::List | Self::Compound | Self::Data | Self::Score => {
                let resolved_generic_type = Box::new(generic_types.first().unwrap().clone());

                match self {
                    Self::List => DataTypeKind::List(resolved_generic_type),
                    Self::Compound => DataTypeKind::Compound(resolved_generic_type),
                    Self::Data => DataTypeKind::Data(resolved_generic_type),
                    Self::Score => DataTypeKind::Score(resolved_generic_type),
                    _ => unreachable!(),
                }
            }
        })
    }

    #[must_use]
    pub const fn generic_count(&self) -> usize {
        match self {
            Self::Boolean
            | Self::Byte
            | Self::Short
            | Self::Integer
            | Self::Long
            | Self::Float
            | Self::Double
            | Self::String
            | Self::Unit
            | Self::SNBT => 0,
            Self::List | Self::Compound | Self::Data | Self::Score => 1,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, HasMacro)]
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
    Score(Box<Self>),
    List(Box<Self>),
    TypedCompound(BTreeMap<SNBTString, Self>),
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

impl Display for DataTypeKind {
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

impl DataTypeKind {
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
            Self::Byte | Self::Short | Self::Integer | Self::InferredInteger => Self::Integer,
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
    pub fn substitute(self, substitutions: &BTreeMap<String, Self>) -> Option<Self> {
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
    fn distribute_references(self) -> Self {
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
    fn distribute_data(self) -> Self {
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
    fn distribute_score(self) -> Self {
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
    pub fn perform_tuple_destructure_semantic_analysis(
        self,
        patterns: &Vec<Pattern>,
        ctx: &mut SemanticAnalysisContext,
        value_span: Span,
        pattern: &Pattern,
    ) -> Option<()> {
        match self {
            Self::Tuple(data_types) => {
                if patterns.len() != data_types.len() {
                    for pattern in patterns {
                        pattern.kind.destructure_unknown(ctx);
                    }

                    return ctx.add_info(SemanticAnalysisInfo {
                        span: pattern.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::MismatchedPatternTypes {
                                expected: Self::Tuple(data_types),
                                actual: pattern.kind.get_type(),
                            },
                        ),
                    });
                }

                patterns
                    .iter()
                    .zip(data_types)
                    .map(|(pattern, data_type)| {
                        data_type
                            .destructure_and_perform_semantic_analysis(ctx, value_span, pattern)
                    })
                    .all_some()
            }
            Self::Reference(_) => self
                .distribute_references()
                .perform_tuple_destructure_semantic_analysis(patterns, ctx, value_span, pattern),
            Self::Data(data_type) => data_type
                .distribute_data()
                .destructure_and_perform_semantic_analysis(ctx, value_span, pattern),
            _ => {
                for pattern in patterns {
                    pattern.kind.destructure_unknown(ctx);
                }

                ctx.add_info(SemanticAnalysisInfo {
                    span: pattern.span,
                    kind: SemanticAnalysisInfoKind::Error(
                        SemanticAnalysisError::MismatchedPatternTypes {
                            expected: self,
                            actual: pattern.kind.get_type(),
                        },
                    ),
                })
            }
        }
    }

    #[must_use]
    pub fn perform_compound_destructure_semantic_analysis(
        self,
        patterns: &BTreeMap<HighSNBTString, Option<Pattern>>,
        ctx: &mut SemanticAnalysisContext,
        value_span: Span,
        pattern: &Pattern,
    ) -> Option<()> {
        match self {
            Self::TypedCompound(ref data_types) => {
                let mut error = false;

                for (key, pattern) in patterns {
                    let data_type = data_types
                        .iter()
                        .find(|(value_key, _)| value_key.1 == key.snbt_string.1)
                        .map(|(_, value)| value.clone());

                    if let Some(data_type) = data_type {
                        if let Some(pattern) = pattern {
                            data_type.destructure_and_perform_semantic_analysis(
                                ctx, value_span, pattern,
                            )?;
                        } else {
                            ctx.declare_variable_known(&key.snbt_string.1, data_type);
                        }
                    } else {
                        ctx.add_info::<()>(SemanticAnalysisInfo {
                            span: key.span,
                            kind: SemanticAnalysisInfoKind::Error(
                                SemanticAnalysisError::TypeDoesntHaveField {
                                    data_type: self.clone(),
                                    field: key.snbt_string.1.clone(),
                                },
                            ),
                        });

                        if let Some(pattern) = pattern {
                            pattern.kind.destructure_unknown(ctx);
                        } else {
                            ctx.declare_variable_unknown(&key.snbt_string.1);
                        }

                        error = true;
                    }
                }

                if error { None } else { Some(()) }
            }
            Self::Compound(data_type) => {
                for (key, pattern) in patterns {
                    let data_type = *data_type.clone();

                    if let Some(pattern) = pattern {
                        data_type
                            .destructure_and_perform_semantic_analysis(ctx, value_span, pattern)?;
                    } else {
                        ctx.declare_variable_known(&key.snbt_string.1, data_type);
                    }
                }

                Some(())
            }
            Self::Data(data_type) => data_type
                .distribute_data()
                .destructure_and_perform_semantic_analysis(ctx, value_span, pattern),
            Self::Reference(_) => self
                .distribute_references()
                .perform_compound_destructure_semantic_analysis(patterns, ctx, value_span, pattern),
            _ => {
                for (key, pattern) in patterns {
                    if let Some(pattern) = pattern {
                        pattern.kind.destructure_unknown(ctx);
                    } else {
                        ctx.declare_variable_unknown(&key.snbt_string.1);
                    }
                }

                ctx.add_info(SemanticAnalysisInfo {
                    span: pattern.span,
                    kind: SemanticAnalysisInfoKind::Error(
                        SemanticAnalysisError::MismatchedPatternTypes {
                            expected: self,
                            actual: pattern.kind.get_type(),
                        },
                    ),
                })
            }
        }
    }

    #[must_use]
    pub fn perform_struct_destructure_semantic_analysis(
        self,
        name: &str,
        field_patterns: &BTreeMap<HighSNBTString, Option<Pattern>>,
        ctx: &mut SemanticAnalysisContext,
        value_span: Span,
        pattern: &Pattern,
    ) -> Option<()> {
        match self {
            Self::Struct(ref struct_name, ref generics) => {
                if struct_name != name {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: pattern.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::MismatchedPatternTypes {
                                expected: self.clone(),
                                actual: pattern.kind.get_type(),
                            },
                        ),
                    });
                }

                let declaration = ctx.get_data_type(struct_name)??;
                let fields = declaration.get_struct_fields(ctx, generics)?;

                let mut error = false;
                for (key, pattern_opt) in field_patterns {
                    if let Some(field_type) = fields.get(&key.snbt_string.1) {
                        let field_type = field_type.clone();

                        if let Some(inner_pattern) = pattern_opt {
                            field_type.destructure_and_perform_semantic_analysis(
                                ctx,
                                value_span,
                                inner_pattern,
                            )?;
                        } else {
                            ctx.declare_variable_known(&key.snbt_string.1, field_type);
                        }
                    } else {
                        error = true;
                        ctx.add_info::<()>(SemanticAnalysisInfo {
                            span: key.span,
                            kind: SemanticAnalysisInfoKind::Error(
                                SemanticAnalysisError::UnexpectedField(key.snbt_string.1.clone()),
                            ),
                        });
                        if let Some(p) = pattern_opt {
                            p.kind.destructure_unknown(ctx);
                        } else {
                            ctx.declare_variable_unknown(&key.snbt_string.1);
                        }
                    }
                }
                if error { None } else { Some(()) }
            }
            Self::Reference(_) => self
                .distribute_references()
                .perform_struct_destructure_semantic_analysis(
                    name,
                    field_patterns,
                    ctx,
                    value_span,
                    pattern,
                ),
            Self::Data(inner) => inner
                .distribute_data()
                .perform_struct_destructure_semantic_analysis(
                    name,
                    field_patterns,
                    ctx,
                    value_span,
                    pattern,
                ),
            Self::Score(inner) => inner
                .distribute_score()
                .perform_struct_destructure_semantic_analysis(
                    name,
                    field_patterns,
                    ctx,
                    value_span,
                    pattern,
                ),
            _ => {
                for inner in field_patterns.values().flatten() {
                    inner.kind.destructure_unknown(ctx);
                }

                ctx.add_info(SemanticAnalysisInfo {
                    span: pattern.span,
                    kind: SemanticAnalysisInfoKind::Error(
                        SemanticAnalysisError::MismatchedPatternTypes {
                            expected: self,
                            actual: pattern.kind.get_type(),
                        },
                    ),
                })
            }
        }
    }

    #[must_use]
    pub fn destructure_and_perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        value_span: Span,
        pattern: &Pattern,
    ) -> Option<()> {
        match &pattern.kind {
            PatternKind::Literal(_) | PatternKind::Wildcard => Some(()),
            PatternKind::Binding(name) => {
                ctx.declare_variable_known(name, self);

                Some(())
            }
            PatternKind::Tuple(patterns) => {
                self.perform_tuple_destructure_semantic_analysis(patterns, ctx, value_span, pattern)
            }
            PatternKind::Compound(patterns) => self
                .perform_compound_destructure_semantic_analysis(patterns, ctx, value_span, pattern),
            PatternKind::Struct(name, field_patterns) => self
                .perform_struct_destructure_semantic_analysis(
                    name,
                    field_patterns,
                    ctx,
                    value_span,
                    pattern,
                ),
            pattern_kind @ PatternKind::Dereference(inner_pattern) => {
                if !self.can_be_dereferenced() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: pattern.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::MismatchedPatternTypes {
                                expected: self,
                                actual: pattern_kind.get_type(),
                            },
                        ),
                    });
                }

                self.destructure_and_perform_semantic_analysis(ctx, value_span, inner_pattern)
            }
        }
    }

    pub fn destructure_tuple(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        patterns: &[Pattern],
        value: ResolvedExpression,
    ) {
        match (self, value) {
            (Self::Tuple(data_types), ResolvedExpression::Tuple(expressions)) => {
                for ((pattern, expression), data_type) in
                    patterns.iter().zip(expressions).zip(data_types)
                {
                    data_type.destructure(datapack, ctx, expression, pattern);
                }
            }
            (Self::Tuple(data_types), ResolvedExpression::Data(target_path)) => {
                let (target, path) = *target_path;

                for (i, (pattern, data_type)) in patterns.iter().zip(data_types).enumerate() {
                    let expression = ResolvedExpression::Data(Box::new((
                        target.clone(),
                        path.clone()
                            .with_node(NbtPathNode::Index(Some(SNBT::Integer(i as i32)))),
                    )));

                    data_type.destructure(datapack, ctx, expression, pattern);
                }
            }
            (Self::Data(inner_type), value @ ResolvedExpression::Data(_)) => {
                inner_type
                    .distribute_data()
                    .destructure_tuple(datapack, ctx, patterns, value);
            }
            (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
        }
    }

    pub fn destructure_compound(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        patterns: &BTreeMap<HighSNBTString, Option<Pattern>>,
        value: ResolvedExpression,
    ) {
        match (self, value) {
            (Self::TypedCompound(data_types), ResolvedExpression::Compound(expressions)) => {
                for ((key, pattern), (_, expression)) in patterns.iter().zip(expressions) {
                    let expression = expression.clone();
                    let data_type = data_types.get(&key.snbt_string).unwrap().clone();

                    if let Some(pattern) = pattern {
                        data_type.destructure(datapack, ctx, expression, pattern);
                    } else {
                        datapack.declare_variable(key.snbt_string.1.clone(), data_type, expression);
                    }
                }
            }
            (Self::Compound(data_type), ResolvedExpression::Compound(expressions)) => {
                for ((key, pattern), (_, expression)) in patterns.iter().zip(expressions) {
                    let expression = expression.clone();
                    let data_type = *data_type.clone();

                    if let Some(pattern) = pattern {
                        data_type.destructure(datapack, ctx, expression, pattern);
                    } else {
                        datapack.declare_variable(key.snbt_string.1.clone(), data_type, expression);
                    }
                }
            }
            (Self::Data(data_type), value @ ResolvedExpression::Data(_)) => {
                data_type.destructure_compound(datapack, ctx, patterns, value);
            }
            (Self::TypedCompound(data_types), ResolvedExpression::Data(target_path)) => {
                let (target, path) = *target_path;

                for (key, pattern) in patterns {
                    let expression = ResolvedExpression::Data(Box::new((
                        target.clone(),
                        path.clone()
                            .with_node(NbtPathNode::Named(key.snbt_string.clone(), None)),
                    )));
                    let data_type = data_types.get(&key.snbt_string).unwrap().clone();

                    if let Some(pattern) = pattern {
                        data_type.destructure(datapack, ctx, expression, pattern);
                    } else {
                        datapack.declare_variable(key.snbt_string.1.clone(), data_type, expression);
                    }
                }
            }
            (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
        }
    }

    pub fn destructure_struct(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        name: &str,
        field_patterns: &BTreeMap<HighSNBTString, Option<Pattern>>,
        value: ResolvedExpression,
    ) {
        match (self, value) {
            (Self::Struct(_, generics), ResolvedExpression::Struct(_, _, fields)) => {
                let declaration = datapack.get_data_type(name).unwrap();
                let field_types = declaration.get_struct_fields(datapack, &generics).unwrap();

                for (key, pattern_opt) in field_patterns {
                    let expression = fields.get(&key.snbt_string.1).unwrap().clone();
                    let data_type = field_types.get(&key.snbt_string.1).unwrap().clone();

                    if let Some(pattern) = pattern_opt {
                        data_type.destructure(datapack, ctx, expression, pattern);
                    } else {
                        datapack.declare_variable(key.snbt_string.1.clone(), data_type, expression);
                    }
                }
            }
            (Self::Struct(_, generics), ResolvedExpression::Data(target_path)) => {
                let (target, path) = *target_path;

                let declaration = datapack.get_data_type(name).unwrap();
                let field_types = declaration.get_struct_fields(datapack, &generics).unwrap();

                for (key, pattern_opt) in field_patterns {
                    let field_path = path
                        .clone()
                        .with_node(NbtPathNode::Named(key.snbt_string.clone(), None));
                    let field_value =
                        ResolvedExpression::Data(Box::new((target.clone(), field_path)));

                    let data_type = field_types.get(&key.snbt_string.1).unwrap().clone();

                    let data_wrapped_type = Self::Data(Box::new(data_type));

                    if let Some(pattern) = pattern_opt {
                        data_wrapped_type.destructure(datapack, ctx, field_value, pattern);
                    } else {
                        datapack.declare_variable(
                            key.snbt_string.1.clone(),
                            data_wrapped_type,
                            field_value,
                        );
                    }
                }
            }
            (Self::Reference(inner), value) => {
                inner.distribute_references().destructure_struct(
                    datapack,
                    ctx,
                    name,
                    field_patterns,
                    value,
                );
            }
            (Self::Data(inner_type), value) => {
                inner_type.distribute_data().destructure_struct(
                    datapack,
                    ctx,
                    name,
                    field_patterns,
                    value,
                );
            }
            (Self::Score(inner_type), value) => {
                inner_type.distribute_score().destructure_struct(
                    datapack,
                    ctx,
                    name,
                    field_patterns,
                    value,
                );
            }
            (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
        }
    }

    pub fn destructure(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        value: ResolvedExpression,
        pattern: &Pattern,
    ) {
        match &pattern.kind {
            PatternKind::Literal(_) | PatternKind::Wildcard => {}
            PatternKind::Binding(name) => {
                datapack.declare_variable(name.clone(), self, value);
            }
            PatternKind::Tuple(patterns) => {
                self.destructure_tuple(datapack, ctx, patterns, value);
            }
            PatternKind::Compound(patterns) => {
                self.destructure_compound(datapack, ctx, patterns, value);
            }
            PatternKind::Dereference(pattern) => {
                let value = value.dereference(datapack, ctx).unwrap();

                self.dereference()
                    .unwrap()
                    .destructure(datapack, ctx, value, pattern);
            }
            PatternKind::Struct(name, field_patterns) => {
                self.destructure_struct(datapack, ctx, name, field_patterns, value);
            }
        }
    }

    #[must_use]
    pub fn perform_equality_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        value_type: &Self,
        value: &Expression,
    ) -> Option<()> {
        if match (self, &value.kind) {
            (Self::List(data_type), ExpressionKind::List(expressions)) => {
                expressions
                    .iter()
                    .map(|expression| {
                        let expression_type = expression.kind.infer_data_type(ctx).unwrap();

                        data_type.perform_equality_semantic_analysis(
                            ctx,
                            &expression_type,
                            expression,
                        )
                    })
                    .all_some()?;

                true
            }
            (Self::TypedCompound(data_types), ExpressionKind::Compound(expressions)) => {
                let mut has_error = false;

                for (key, expression) in expressions {
                    let Some(data_type) = data_types.get(&key.snbt_string) else {
                        has_error = true;

                        ctx.add_info::<()>(SemanticAnalysisInfo {
                            span: key.span,
                            kind: SemanticAnalysisInfoKind::Error(
                                SemanticAnalysisError::UnexpectedKey(key.snbt_string.1.clone()),
                            ),
                        });

                        continue;
                    };

                    let Some(expression_type) = expression.kind.infer_data_type(ctx) else {
                        has_error = true;

                        continue;
                    };

                    if data_type
                        .perform_equality_semantic_analysis(ctx, &expression_type, expression)
                        .is_none()
                    {
                        has_error = true;
                    }
                }

                for expected_key in data_types.keys() {
                    let exists = expressions.keys().any(|k| &k.snbt_string == expected_key);

                    if !exists {
                        has_error = true;

                        ctx.add_info::<()>(SemanticAnalysisInfo {
                            span: value.span,
                            kind: SemanticAnalysisInfoKind::Error(
                                SemanticAnalysisError::MissingKey(expected_key.1.clone()),
                            ),
                        });
                    }
                }

                if has_error {
                    return None;
                }

                true
            }
            (Self::Compound(data_type), ExpressionKind::Compound(expressions)) => {
                expressions
                    .values()
                    .map(|expression| {
                        let expression_type = expression.kind.infer_data_type(ctx).unwrap();

                        data_type.perform_equality_semantic_analysis(
                            ctx,
                            &expression_type,
                            expression,
                        )
                    })
                    .all_some()?;

                true
            }
            (
                Self::Reference(data_type),
                ExpressionKind::Unary(UnaryOperator::Reference, expression),
            ) => {
                let expression_type = expression.kind.infer_data_type(ctx).unwrap();

                data_type.perform_equality_semantic_analysis(ctx, &expression_type, expression)?;

                true
            }
            (Self::Tuple(data_types), ExpressionKind::Tuple(expressions))
                if expressions.len() == data_types.len() =>
            {
                expressions
                    .iter()
                    .zip(data_types)
                    .map(|(expression, data_type)| {
                        let expression_type = expression.kind.infer_data_type(ctx).unwrap();

                        data_type.perform_equality_semantic_analysis(
                            ctx,
                            &expression_type,
                            expression,
                        )
                    })
                    .all_some()?;

                true
            }
            _ => self.equals(value_type),
        } {
            Some(())
        } else {
            ctx.add_info(SemanticAnalysisInfo {
                span: value.span,
                kind: SemanticAnalysisInfoKind::Error(SemanticAnalysisError::MismatchedTypes {
                    expected: self.clone(),
                    actual: value_type.clone(),
                }),
            })
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
            (Self::Score(self_type), Self::Score(data_type)) => self_type == data_type,
            (Self::Data(self_type), Self::Data(_)) => **self_type == Self::SNBT,

            (Self::SNBT, _)
            | (Self::InferredInteger, Self::Byte | Self::Short | Self::Integer | Self::Long)
            | (Self::InferredFloat, Self::Float | Self::Double)
            | (
                Self::Byte | Self::Short | Self::Integer | Self::Long | Self::Float | Self::Double,
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
            t if t.is_integer_like() => true,

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
                    .all_some_true()?
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
        if self.is_integer_like() && other.is_integer_like() {
            return Some(match (self, other) {
                (Self::Byte, _) | (_, Self::Byte) => Self::Byte,
                (Self::Short, _) | (_, Self::Short) => Self::Short,
                (Self::Integer, _) | (_, Self::Integer) => Self::Integer,
                (Self::Long, _) | (_, Self::Long) => Self::Long,
                (Self::InferredInteger, other) | (other, Self::InferredInteger)
                    if other.is_restrictied_integer_like() =>
                {
                    other.clone()
                }
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
        operator: &ArithmeticOperator,
        other: &Self,
    ) -> bool {
        match (self, other) {
            (_, other) if *operator == ArithmeticOperator::Swap && !other.is_lvalue() => false,
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
    pub const fn can_be_dereferenced(&self) -> bool {
        matches!(self, Self::Reference(_) | Self::Score(_) | Self::Data(_))
    }

    // TODO maybe create a can_be_negated method?
    #[must_use]
    pub fn get_negated_result(&self) -> Option<Self> {
        Some(match self {
            Self::Reference(self_) => match **self_ {
                Self::Byte => Self::Byte,
                Self::Short => Self::Short,
                Self::Integer => Self::Integer,
                Self::Long => Self::Long,
                Self::Score(ref inner_type) => Self::Score(inner_type.clone()),
                _ => return None,
            },

            Self::Byte => Self::Byte,
            Self::Short => Self::Short,
            Self::Integer => Self::Integer,
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
