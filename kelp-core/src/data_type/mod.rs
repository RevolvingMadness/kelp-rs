use std::{collections::BTreeMap, fmt::Display};

use minecraft_command_types::{nbt_path::NbtPathNode, snbt::SNBTString};
use minecraft_command_types_derive::HasMacro;
use parser_rs::parser_range::ParserRange;

use crate::{
    datapack::{DataTypeDeclarationKind, HighDatapack},
    expression::{
        Expression, ExpressionKind,
        constant::{ConstantExpression, ConstantExpressionKind},
        supports_variable_type_scope::SupportsVariableTypeScope,
    },
    high::snbt_string::HighSNBTString,
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator, UnaryOperator},
    pattern::{Pattern, PatternKind},
    place::PlaceTypeKind,
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
    trait_ext::{OptionBoolIterExt, OptionUnitIterExt},
};

pub mod high;

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
    Score,
    List(Box<DataTypeKind>),
    TypedCompound(BTreeMap<SNBTString, DataTypeKind>),
    Compound(Box<DataTypeKind>),
    Data(Box<DataTypeKind>),
    Reference(Box<DataTypeKind>),
    Generic(String),
    Tuple(Vec<DataTypeKind>),
    SNBT,
    Struct(String, Vec<DataTypeKind>),
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
            DataTypeKind::Unit => f.write_str("()"),
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
            DataTypeKind::Generic(name) => f.write_str(name),
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
            DataTypeKind::SNBT => f.write_str("snbt"),
            DataTypeKind::Struct(name, generics) => {
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
        }
    }
}

impl DataTypeKind {
    #[must_use]
    pub fn substitute(
        self,
        substitutions: &BTreeMap<String, DataTypeKind>,
    ) -> Option<DataTypeKind> {
        Some(match self {
            DataTypeKind::List(inner) => {
                DataTypeKind::List(Box::new(inner.substitute(substitutions)?))
            }
            DataTypeKind::Compound(inner) => {
                DataTypeKind::Compound(Box::new(inner.substitute(substitutions)?))
            }
            DataTypeKind::Data(inner) => {
                DataTypeKind::Data(Box::new(inner.substitute(substitutions)?))
            }
            DataTypeKind::Reference(inner) => {
                DataTypeKind::Reference(Box::new(inner.substitute(substitutions)?))
            }
            DataTypeKind::Tuple(inner) => DataTypeKind::Tuple(
                inner
                    .into_iter()
                    .map(|t| t.substitute(substitutions))
                    .collect::<Option<_>>()?,
            ),
            DataTypeKind::TypedCompound(inner) => DataTypeKind::TypedCompound(
                inner
                    .into_iter()
                    .map(|(k, v)| v.substitute(substitutions).map(|v| (k, v)))
                    .collect::<Option<_>>()?,
            ),
            DataTypeKind::Generic(ref name) => substitutions.get(name)?.clone(),
            DataTypeKind::Struct(name, generics) => DataTypeKind::Struct(
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
    fn reference(self) -> DataTypeKind {
        DataTypeKind::Reference(Box::new(self))
    }

    #[must_use]
    fn distribute_references(self) -> DataTypeKind {
        match self {
            DataTypeKind::Reference(inner) => match inner.distribute_references() {
                DataTypeKind::List(data_type) => {
                    DataTypeKind::List(Box::new(data_type.reference()))
                }
                DataTypeKind::Tuple(data_types) => DataTypeKind::Tuple(
                    data_types
                        .into_iter()
                        .map(|data_type| data_type.reference())
                        .collect(),
                ),
                DataTypeKind::Struct(name, generic_types) => DataTypeKind::Struct(
                    name,
                    generic_types
                        .into_iter()
                        .map(|generic_type| generic_type.reference())
                        .collect(),
                ),
                DataTypeKind::TypedCompound(compound) => DataTypeKind::TypedCompound(
                    compound
                        .into_iter()
                        .map(|(key, data_type)| (key, data_type.reference()))
                        .collect(),
                ),
                DataTypeKind::Compound(data_type) => {
                    DataTypeKind::Compound(Box::new(data_type.reference()))
                }
                DataTypeKind::Data(data_type) => {
                    DataTypeKind::Data(Box::new(data_type.reference()))
                }
                inner => inner.reference(),
            },
            _ => self,
        }
    }

    #[must_use]
    fn wrap_data(self) -> DataTypeKind {
        match self {
            DataTypeKind::Reference(data_type) => {
                DataTypeKind::Reference(Box::new(data_type.wrap_data()))
            }
            DataTypeKind::Data(inner) => inner.wrap_data(),
            DataTypeKind::List(data_type) => DataTypeKind::List(Box::new(data_type.wrap_data())),
            DataTypeKind::Tuple(data_types) => DataTypeKind::Tuple(
                data_types
                    .into_iter()
                    .map(|data_type| data_type.wrap_data())
                    .collect(),
            ),
            DataTypeKind::TypedCompound(compound) => DataTypeKind::TypedCompound(
                compound
                    .into_iter()
                    .map(|(key, data_type)| (key, data_type.wrap_data()))
                    .collect(),
            ),
            DataTypeKind::Compound(data_type) => {
                DataTypeKind::Compound(Box::new(data_type.wrap_data()))
            }
            DataTypeKind::Struct(name, generic_types) => DataTypeKind::Struct(
                name,
                generic_types
                    .into_iter()
                    .map(|generic_type| generic_type.wrap_data())
                    .collect(),
            ),
            _ => DataTypeKind::Data(Box::new(self)),
        }
    }

    #[must_use]
    pub fn perform_tuple_destructure_semantic_analysis(
        self,
        patterns: &Vec<Pattern>,
        ctx: &mut SemanticAnalysisContext,
        value_span: ParserRange,
        pattern: &Pattern,
    ) -> Option<()> {
        match self {
            DataTypeKind::Tuple(data_types) => {
                if patterns.len() != data_types.len() {
                    for pattern in patterns {
                        pattern.kind.destructure_unknown(ctx);
                    }

                    return ctx.add_info(SemanticAnalysisInfo {
                        span: pattern.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::MismatchedPatternTypes {
                                expected: DataTypeKind::Tuple(data_types),
                                actual: pattern.kind.get_type(),
                            },
                        ),
                    });
                }

                patterns
                    .iter()
                    .zip(data_types)
                    .map(|(pattern, data_type)| {
                        data_type.perform_destructure_semantic_analysis(ctx, value_span, pattern)
                    })
                    .all_some()
            }
            DataTypeKind::Reference(_) => self
                .distribute_references()
                .perform_tuple_destructure_semantic_analysis(patterns, ctx, value_span, pattern),
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
        value_span: ParserRange,
        pattern: &Pattern,
    ) -> Option<()> {
        match self {
            DataTypeKind::TypedCompound(ref data_types) => {
                let mut error = false;

                for (key, pattern) in patterns.iter() {
                    let data_type = data_types
                        .iter()
                        .find(|(value_key, _)| value_key.1 == key.snbt_string.1)
                        .map(|(_, value)| value.clone());

                    if let Some(data_type) = data_type {
                        if let Some(pattern) = pattern {
                            data_type
                                .perform_destructure_semantic_analysis(ctx, value_span, pattern)?;
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
            DataTypeKind::Compound(data_type) => {
                for (key, pattern) in patterns.iter() {
                    let data_type = *data_type.clone();

                    if let Some(pattern) = pattern {
                        data_type
                            .perform_destructure_semantic_analysis(ctx, value_span, pattern)?;
                    } else {
                        ctx.declare_variable_known(&key.snbt_string.1, data_type);
                    }
                }

                Some(())
            }
            DataTypeKind::Data(data_type) => data_type
                .wrap_data()
                .perform_destructure_semantic_analysis(ctx, value_span, pattern),
            DataTypeKind::Reference(_) => self
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
        value_span: ParserRange,
        pattern: &Pattern,
    ) -> Option<()> {
        match self {
            DataTypeKind::Struct(ref struct_name, ref generics) => {
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
                let fields = declaration.get_struct_fields(ctx, struct_name, generics)?;

                let mut error = false;
                for (key, pattern_opt) in field_patterns.iter() {
                    if let Some(field_type_opt) = fields.get(&key.snbt_string.1) {
                        let field_type = field_type_opt.clone()?;
                        if let Some(inner_pattern) = pattern_opt {
                            field_type.perform_destructure_semantic_analysis(
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
                                SemanticAnalysisError::StructHasNoField(key.snbt_string.1.clone()),
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
            DataTypeKind::Reference(_) => self
                .distribute_references()
                .perform_struct_destructure_semantic_analysis(
                    name,
                    field_patterns,
                    ctx,
                    value_span,
                    pattern,
                ),
            DataTypeKind::Data(inner) => inner
                .wrap_data()
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
    pub fn perform_destructure_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        value_span: ParserRange,
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
                    ctx.add_info(SemanticAnalysisInfo {
                        span: pattern.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::MismatchedPatternTypes {
                                expected: self,
                                actual: pattern_kind.get_type(),
                            },
                        ),
                    })
                } else {
                    self.perform_destructure_semantic_analysis(ctx, value_span, inner_pattern)
                }
            }
        }
    }

    pub fn destructure_tuple(
        self,
        patterns: &[Pattern],
        datapack: &mut HighDatapack,
        value: ConstantExpressionKind,
    ) {
        match (self, value) {
            (DataTypeKind::Tuple(data_types), ConstantExpressionKind::Tuple(expressions)) => {
                for ((pattern, expression), data_type) in
                    patterns.iter().zip(expressions).zip(data_types)
                {
                    data_type.destructure(datapack, expression, pattern);
                }
            }
            (DataTypeKind::Reference(self_), ConstantExpressionKind::Reference(value)) => {
                self_.distribute_references().destructure_tuple(
                    patterns,
                    datapack,
                    value.kind.distribute_references(),
                );
            }
            (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
        }
    }

    pub fn destructure_compound(
        self,
        patterns: &BTreeMap<HighSNBTString, Option<Pattern>>,
        datapack: &mut HighDatapack,
        value: ConstantExpressionKind,
    ) {
        match (self, value) {
            (
                DataTypeKind::TypedCompound(data_types),
                ConstantExpressionKind::Compound(expressions),
            ) => {
                for ((key, pattern), (_, expression)) in patterns.iter().zip(expressions) {
                    let expression = expression.clone();
                    let data_type = data_types.get(&key.snbt_string).unwrap().clone();

                    if let Some(pattern) = pattern {
                        data_type.destructure(datapack, expression, pattern);
                    } else {
                        datapack.declare_variable(key.snbt_string.1.clone(), data_type, expression);
                    }
                }
            }
            (DataTypeKind::Compound(data_type), ConstantExpressionKind::Compound(expressions)) => {
                for ((key, pattern), (_, expression)) in patterns.iter().zip(expressions) {
                    let expression = expression.clone();
                    let data_type = *data_type.clone();

                    if let Some(pattern) = pattern {
                        data_type.destructure(datapack, expression, pattern);
                    } else {
                        datapack.declare_variable(key.snbt_string.1.clone(), data_type, expression);
                    }
                }
            }
            (DataTypeKind::Data(data_type), value @ ConstantExpressionKind::Data(_, _)) => {
                data_type.destructure_compound(patterns, datapack, value);
            }
            (
                DataTypeKind::TypedCompound(data_types),
                ConstantExpressionKind::Data(target, path),
            ) => {
                for (key, pattern) in patterns {
                    let expression = ConstantExpressionKind::Data(
                        target.clone(),
                        path.clone()
                            .with_node(NbtPathNode::Named(key.snbt_string.clone(), None)),
                    )
                    .into_dummy_constant_expression();
                    let data_type = data_types.get(&key.snbt_string).unwrap().clone();

                    if let Some(pattern) = pattern {
                        data_type.destructure(datapack, expression, pattern);
                    } else {
                        datapack.declare_variable(key.snbt_string.1.clone(), data_type, expression);
                    }
                }
            }
            (DataTypeKind::Reference(self_), ConstantExpressionKind::Reference(value)) => {
                self_.distribute_references().destructure_compound(
                    patterns,
                    datapack,
                    value.kind.distribute_references(),
                );
            }
            (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
        }
    }

    pub fn destructure_struct(
        self,
        name: &str,
        field_patterns: &BTreeMap<HighSNBTString, Option<Pattern>>,
        datapack: &mut HighDatapack,
        value: ConstantExpressionKind,
    ) {
        match (self, value) {
            (
                DataTypeKind::Struct(s_name, generics),
                ConstantExpressionKind::Struct(v_s_name, _, fields),
            ) if s_name == name && v_s_name == name => {
                let declaration = datapack.get_data_type(name).unwrap();
                let field_types = declaration
                    .get_struct_fields(datapack, name, &generics)
                    .unwrap();

                for (key, pattern_opt) in field_patterns {
                    let expression = fields.get(&key.snbt_string.1).unwrap().clone();
                    let data_type = field_types
                        .get(&key.snbt_string.1)
                        .unwrap()
                        .clone()
                        .unwrap();

                    if let Some(pattern) = pattern_opt {
                        data_type.destructure(datapack, expression, pattern);
                    } else {
                        datapack.declare_variable(key.snbt_string.1.clone(), data_type, expression);
                    }
                }
            }
            (DataTypeKind::Struct(_, generics), ConstantExpressionKind::Data(target, path)) => {
                let declaration = datapack.get_data_type(name).unwrap();
                let field_types = declaration
                    .get_struct_fields(datapack, name, &generics)
                    .unwrap();

                for (key, pattern_opt) in field_patterns {
                    let field_path = path
                        .clone()
                        .with_node(NbtPathNode::Named(key.snbt_string.clone(), None));
                    let field_value = ConstantExpressionKind::Data(target.clone(), field_path)
                        .into_dummy_constant_expression();

                    let data_type = field_types
                        .get(&key.snbt_string.1)
                        .unwrap()
                        .clone()
                        .unwrap();
                    let data_wrapped_type = DataTypeKind::Data(Box::new(data_type));

                    if let Some(pattern) = pattern_opt {
                        data_wrapped_type.destructure(datapack, field_value, pattern);
                    } else {
                        datapack.declare_variable(
                            key.snbt_string.1.clone(),
                            data_wrapped_type,
                            field_value,
                        );
                    }
                }
            }
            (DataTypeKind::Reference(inner), ConstantExpressionKind::Reference(val)) => {
                inner.distribute_references().destructure_struct(
                    name,
                    field_patterns,
                    datapack,
                    val.kind.distribute_references(),
                );
            }
            (DataTypeKind::Data(inner_type), value @ ConstantExpressionKind::Data(_, _)) => {
                inner_type
                    .wrap_data()
                    .destructure_struct(name, field_patterns, datapack, value)
            }
            (self_, value_kind) => unreachable!("{:?} {:?}", self_, value_kind),
        }
    }

    pub fn destructure(
        self,
        datapack: &mut HighDatapack,
        value: ConstantExpression,
        pattern: &Pattern,
    ) {
        match &pattern.kind {
            PatternKind::Literal(_) => {}
            PatternKind::Wildcard => {}
            PatternKind::Binding(name) => {
                datapack.declare_variable(name.clone(), self, value);
            }
            PatternKind::Tuple(patterns) => {
                self.destructure_tuple(patterns, datapack, value.kind);
            }
            PatternKind::Compound(patterns) => {
                self.destructure_compound(patterns, datapack, value.kind);
            }
            PatternKind::Dereference(pattern) => {
                let value = value
                    .kind
                    .dereference(datapack)
                    .into_dummy_constant_expression();

                self.dereference()
                    .unwrap()
                    .destructure(datapack, value, pattern);
            }
            PatternKind::Struct(name, field_patterns) => {
                self.destructure_struct(name, field_patterns, datapack, value.kind);
            }
        }
    }

    pub fn perform_equality_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        value_type: &DataTypeKind,
        value: &Expression,
    ) -> Option<()> {
        if match (self, &value.kind) {
            (DataTypeKind::List(data_type), ExpressionKind::List(expressions)) => {
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
            (DataTypeKind::TypedCompound(data_types), ExpressionKind::Compound(expressions)) => {
                let mut has_error = false;

                for (key, expression) in expressions {
                    match data_types.get(&key.snbt_string) {
                        Some(data_type) => {
                            let expression_type = match expression.kind.infer_data_type(ctx) {
                                Some(t) => t,
                                None => {
                                    has_error = true;

                                    continue;
                                }
                            };

                            if data_type
                                .perform_equality_semantic_analysis(
                                    ctx,
                                    &expression_type,
                                    expression,
                                )
                                .is_none()
                            {
                                has_error = true;
                            }
                        }
                        None => {
                            has_error = true;

                            ctx.add_info::<()>(SemanticAnalysisInfo {
                                span: key.span,
                                kind: SemanticAnalysisInfoKind::Error(
                                    SemanticAnalysisError::CompoundHasNoKey(
                                        key.snbt_string.1.clone(),
                                    ),
                                ),
                            });
                        }
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
            (DataTypeKind::Compound(data_type), ExpressionKind::Compound(expressions)) => {
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
            (DataTypeKind::Data(inner_type), ExpressionKind::Data(_, _)) => {
                inner_type.perform_equality_semantic_analysis(ctx, value_type, value)?;

                true
            }
            (
                DataTypeKind::Reference(data_type),
                ExpressionKind::Unary(UnaryOperator::Reference, expression),
            ) => {
                let expression_type = expression.kind.infer_data_type(ctx).unwrap();

                data_type.perform_equality_semantic_analysis(ctx, &expression_type, expression)?;

                true
            }
            (DataTypeKind::Tuple(data_types), ExpressionKind::Tuple(expressions))
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

    pub fn as_place_type(self) -> Result<PlaceTypeKind, Self> {
        Ok(match self {
            DataTypeKind::Score => PlaceTypeKind::Score,
            DataTypeKind::Data(data_type) => PlaceTypeKind::Data(*data_type),
            _ => return Err(self),
        })
    }

    pub fn dereference(self) -> Option<DataTypeKind> {
        Some(match self {
            DataTypeKind::Reference(data_type) => *data_type,
            DataTypeKind::Score | DataTypeKind::Data(_) => self,

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
        match self {
            DataTypeKind::Boolean | DataTypeKind::Data(_) => true,

            DataTypeKind::Reference(data_type) => data_type.is_condition(),

            DataTypeKind::Generic(_) => unreachable!(),

            _ => false,
        }
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
            DataTypeKind::Generic(_) => unreachable!(),
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
            DataTypeKind::Struct(_, _) => true,
            DataTypeKind::Generic(_) => unreachable!(),
            _ => false,
        }
    }

    pub fn can_be_assigned_to_data(&self, ctx: &mut SemanticAnalysisContext) -> Option<bool> {
        Some(match self {
            DataTypeKind::Unit => true,
            DataTypeKind::Score => true,
            DataTypeKind::List(data_type) => data_type.can_be_assigned_to_data(ctx)?,
            DataTypeKind::TypedCompound(compound) => compound
                .values()
                .map(|data_type| data_type.can_be_assigned_to_data(ctx))
                .all_some_true()?,
            DataTypeKind::Compound(compound) => compound.can_be_assigned_to_data(ctx)?,
            DataTypeKind::Data(data_type) => data_type.can_be_assigned_to_data(ctx)?,
            DataTypeKind::Reference(data_type) => data_type.can_be_assigned_to_data(ctx)?,
            DataTypeKind::Tuple(data_types) => data_types
                .iter()
                .map(|data_type| data_type.can_be_assigned_to_data(ctx))
                .all_some_true()?,
            DataTypeKind::Struct(name, generic_types) => {
                let declaration @ DataTypeDeclarationKind::Struct { .. } =
                    ctx.get_data_type(name)??
                else {
                    return None;
                };

                let fields = declaration.get_struct_fields(ctx, name, generic_types)?;

                fields
                    .values()
                    .map(|field_type| field_type.as_ref()?.can_be_assigned_to_data(ctx))
                    .all_some_true()?
            }
            DataTypeKind::Generic(_) => unreachable!(),
            _ => self.is_snbt_like(),
        })
    }

    pub fn can_be_indexed(&self) -> bool {
        match self {
            DataTypeKind::Reference(data_type) => data_type.can_be_indexed(),

            DataTypeKind::List(_) | DataTypeKind::Data(_) | DataTypeKind::SNBT => true,

            DataTypeKind::Generic(_) => unreachable!(),

            _ => false,
        }
    }

    pub fn has_fields(&self) -> bool {
        match self {
            DataTypeKind::Struct(_, _) => true,

            DataTypeKind::Reference(data_type) => data_type.has_fields(),

            DataTypeKind::TypedCompound(_)
            | DataTypeKind::Compound(_)
            | DataTypeKind::Data(_)
            | DataTypeKind::Tuple(_)
            | DataTypeKind::SNBT => true,

            DataTypeKind::Generic(_) => unreachable!(),

            _ => false,
        }
    }

    pub fn has_field(
        &self,
        ctx: &mut SemanticAnalysisContext,
        field: &HighSNBTString,
    ) -> Option<bool> {
        Some(match self {
            DataTypeKind::Reference(data_type) => data_type.has_field(ctx, field)?,

            DataTypeKind::Struct(name, generic_types) => {
                let declaration @ DataTypeDeclarationKind::Struct { .. } =
                    ctx.get_data_type(name)??
                else {
                    return None;
                };

                let fields = declaration.get_struct_fields(ctx, name, generic_types)?;

                fields.contains_key(&field.snbt_string.1)
            }

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
            DataTypeKind::Data(data_type) => data_type.has_field(ctx, field)?,
            DataTypeKind::Compound(_) | DataTypeKind::SNBT => true,
            DataTypeKind::Generic(_) => unreachable!(),
            _ => false,
        })
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
        operator: &ComparisonOperator,
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
            (DataTypeKind::Data(inner_type), other) | (other, DataTypeKind::Data(inner_type))
                if *operator == ComparisonOperator::EqualTo
                    || *operator == ComparisonOperator::NotEqualTo =>
            {
                inner_type.can_perform_comparison(operator, other)
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
            (DataTypeKind::Reference(data_type), other)
            | (other, DataTypeKind::Reference(data_type)) => {
                data_type.raw_can_perform_comparison(operator, other)
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

    pub fn get_field_result(
        &self,
        supports_variable_type_scope: &impl SupportsVariableTypeScope,
        field: &String,
    ) -> Option<DataTypeKind> {
        Some(match self {
            DataTypeKind::Reference(self_) => {
                self_.get_field_result(supports_variable_type_scope, field)?
            }

            DataTypeKind::Struct(name, generics) => {
                let declaration @ DataTypeDeclarationKind::Struct { .. } =
                    supports_variable_type_scope.get_data_type(name)??
                else {
                    return None;
                };

                let fields =
                    declaration.get_struct_fields(supports_variable_type_scope, name, generics)?;

                fields.get(field).cloned()??
            }

            DataTypeKind::TypedCompound(compound) => {
                compound.iter().find(|(key, _)| key.1 == *field)?.1.clone()
            }
            DataTypeKind::Compound(data_type) => *data_type.clone(),
            DataTypeKind::Data(data_type) => DataTypeKind::Data(Box::new(
                data_type.get_field_result(supports_variable_type_scope, field)?,
            )),
            DataTypeKind::Tuple(items) => {
                return if let Ok(index) = field.parse::<i32>() {
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
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}
