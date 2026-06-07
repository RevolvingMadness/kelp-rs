pub mod assignee;
pub mod command;
pub mod literal;

use std::collections::{BTreeMap, HashMap};

use minecraft_command_types::{
    command::{
        Command,
        data::DataCommandModification,
        execute::{ExecuteIfSubcommand, ExecuteSubcommand, ScoreComparison},
        r#return::ReturnCommand,
    },
    nbt_path::{NbtPath, NbtPathNode, SNBTCompound},
    range::IntegerRange,
    resource_location::ResourceLocation,
    snbt::{SNBT, SNBTString},
};
use ordered_float::NotNan;

use crate::low::expression::Expression;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::{
    r#type::r#struct::{regular::HighRegularStructId, tuple::HighTupleStructId},
    value::HighValueId,
};
use crate::{
    compile_context::{CompileContext, LoopInfo, LoopType},
    data::GeneratedData,
    datapack::Datapack,
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator},
    parsed::expression::{assignee::ParsedAssigneeExpression, place::ParsedPlaceExpression},
    runtime_storage::RuntimeStorageType,
    semantic::{
        coordinate::SemanticCoordinates,
        data::SemanticData,
        entity_selector::SemanticEntitySelector,
        expression::command::SemanticCommand,
        pattern::SemanticPattern,
        player_score::SemanticPlayerScore,
        statement::{EarlyReturnType, SemanticStatement},
        supports_expression_sigil::SemanticSupportsExpressionSigil,
    },
};
use crate::{
    low::data_type::{DataType, FieldAccessType},
    semantic::expression::command::execute::subcommand::r#if::SemanticExecuteIfSubcommand,
};
use crate::{
    low::environment::value::{
        ValueDeclaration, constant::ConstantId, function::FunctionId, variable::VariableId,
    },
    semantic::environment::r#type::r#struct::unit::HighUnitStructId,
};

fn compile_if(
    datapack: &mut Datapack,
    caller_ctx: &mut CompileContext,
    contains_early_return: bool,
    mut body_ctx: CompileContext,
    inverted: bool,
    condition: ExecuteIfSubcommand,
) {
    let should_inline = body_ctx.num_commands() <= 5;

    if should_inline {
        for command in body_ctx.commands {
            caller_ctx.add_command(
                datapack,
                command.run().conditionally(inverted, condition.clone()),
            );
        }
    } else {
        let body_paths = datapack.get_unique_function_paths();
        let body_function_resource_location = ResourceLocation::new_namespace_paths(
            datapack.current_namespace_name(),
            body_paths.clone(),
        );

        if contains_early_return {
            caller_ctx.add_command(
                datapack,
                Command::Return(ReturnCommand::Fail)
                    .run()
                    .unless_function(body_function_resource_location)
                    .conditionally(inverted, condition),
            );
        } else {
            caller_ctx.add_command(
                datapack,
                Command::Function(body_function_resource_location, None)
                    .run()
                    .conditionally(inverted, condition),
            );
        }

        body_ctx.add_command(datapack, Command::Return(ReturnCommand::Value(1)));

        datapack.compile_and_add_to_function(&body_paths, &mut body_ctx);
    }
}

fn compile_if_internal(
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    condition: SemanticExpression,
    body: SemanticExpression,
    else_body: Option<Box<SemanticExpression>>,
    output_data: Option<GeneratedData>,
) {
    let mut body_ctx = ctx.create_child_ctx();
    let main_body_returns_early = body.kind.returns_early();
    let body = body.kind.resolve(datapack, &mut body_ctx);

    if let Some(output_data) = &output_data {
        body.assign_to_data(datapack, &mut body_ctx, output_data.clone());
    }

    let (invert, condition) = condition
        .kind
        .resolve(datapack, ctx)
        .to_execute_condition(datapack, ctx)
        .unwrap();

    if let Some(else_body) = else_body {
        let mut else_body_ctx = ctx.create_child_ctx();
        let else_body_returns_early = else_body.kind.returns_early();
        let else_body = else_body.kind.resolve(datapack, &mut else_body_ctx);

        if let Some(output_data) = output_data {
            else_body.assign_to_data(datapack, &mut else_body_ctx, output_data);
        }

        let should_add_condition = body_ctx.num_commands() > 1 || else_body_ctx.num_commands() > 1;

        let (invert, condition) = if should_add_condition {
            let unique_score = datapack.get_unique_score();

            ctx.add_command(
                datapack,
                condition
                    .into_subcommand(invert)
                    .store_result_score(unique_score.score.clone()),
            );

            (
                true,
                ExecuteIfSubcommand::Score(
                    unique_score.score,
                    ScoreComparison::Range(IntegerRange::new_single(0)),
                    None,
                ),
            )
        } else {
            (invert, condition)
        };

        compile_if(
            datapack,
            ctx,
            main_body_returns_early,
            body_ctx,
            invert,
            condition.clone(),
        );

        compile_if(
            datapack,
            ctx,
            else_body_returns_early,
            else_body_ctx,
            !invert,
            condition,
        );
    } else {
        let should_add_condition = body_ctx.num_commands() > 1;

        let (invert, condition) = if should_add_condition {
            let unique_score = datapack.get_unique_score();

            ctx.add_command(
                datapack,
                condition
                    .into_subcommand(invert)
                    .store_success_score(unique_score.score.clone()),
            );

            (
                true,
                ExecuteIfSubcommand::Score(
                    unique_score.score,
                    ScoreComparison::Range(IntegerRange::new_single(0)),
                    None,
                ),
            )
        } else {
            (invert, condition)
        };

        compile_if(
            datapack,
            ctx,
            main_body_returns_early,
            body_ctx,
            invert,
            condition,
        );
    }
}

fn iterate_string(
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    is_reversed: bool,
    pattern: SemanticPattern,
    iterable: Expression,
    body: SemanticExpression,
) {
    let (unique_data, name) = datapack.get_unique_data_named();
    let unique_data_2 = datapack.get_unique_data();

    iterable.assign_to_data(datapack, ctx, unique_data.clone());

    let mut for_body_ctx = CompileContext::default();

    pattern.destructure(
        datapack,
        &mut for_body_ctx,
        DataType::String,
        Expression::Data(unique_data_2.clone()),
    );

    for_body_ctx.add_command(
        datapack,
        unique_data_2.set(DataCommandModification::String(
            unique_data.target.target.clone(),
            Some(unique_data.path.clone()),
            Some(if is_reversed { -1 } else { 0 }),
            if is_reversed { None } else { Some(1) },
        )),
    );

    body.kind.compile_as_statement(datapack, &mut for_body_ctx);

    let current_namespace_name = datapack.current_namespace_name().to_string();

    let for_function_paths = datapack.get_unique_function_paths();

    let mut condition_ctx = CompileContext::default();

    for_body_ctx.add_command(
        datapack,
        unique_data.clone().set(DataCommandModification::String(
            unique_data.target.target.clone(),
            Some(unique_data.path.clone()),
            Some(i32::from(!is_reversed)),
            if is_reversed { Some(-1) } else { None },
        )),
    );

    let mut map = SNBTCompound::new();
    map.insert(SNBTString(false, name), SNBT::macroable_string(""));
    let unique_path = NbtPath(vec![NbtPathNode::RootCompound(map)]);

    condition_ctx.add_command(
        datapack,
        Command::Function(
            ResourceLocation::new_namespace_paths(
                &current_namespace_name,
                for_function_paths.clone(),
            ),
            None,
        )
        .run()
        .unless_data(unique_data.target.target, unique_path),
    );

    for_body_ctx.extend_context(condition_ctx.clone());
    ctx.extend_context(condition_ctx);

    datapack.compile_and_add_to_function(&for_function_paths, &mut for_body_ctx);
}

fn iterate_data(
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    is_reversed: bool,
    pattern: SemanticPattern,
    iterable_type: DataType,
    iterable: Expression,
    body: SemanticExpression,
) {
    let unique_data = datapack.get_unique_data();

    iterable.assign_to_data(datapack, ctx, unique_data.clone());

    let mut for_body_ctx = CompileContext::default();

    let destructure_unique_data =
        unique_data
            .clone()
            .with_path_node(NbtPathNode::Index(Some(SNBT::macroable_integer(
                if is_reversed { -1 } else { 0 },
            ))));

    pattern.destructure(
        datapack,
        &mut for_body_ctx,
        iterable_type,
        Expression::Data(destructure_unique_data),
    );

    body.kind.compile_as_statement(datapack, &mut for_body_ctx);

    let current_namespace_name = datapack.current_namespace_name().to_string();

    let for_function_paths = datapack.get_unique_function_paths();

    let mut condition_ctx = CompileContext::default();

    for_body_ctx.add_command(datapack, unique_data.clone().remove());

    condition_ctx.add_command(
        datapack,
        Command::Function(
            ResourceLocation::new_namespace_paths(
                &current_namespace_name,
                for_function_paths.clone(),
            ),
            None,
        )
        .run()
        .if_data(unique_data.target.target, unique_data.path),
    );

    for_body_ctx.extend_context(condition_ctx.clone());
    ctx.extend_context(condition_ctx);

    datapack.compile_and_add_to_function(&for_function_paths, &mut for_body_ctx);
}

#[derive(Debug, Clone)]
pub enum SemanticExpressionKind {
    Boolean(bool),
    Byte(i8),
    Short(i16),
    Integer(i32),
    InferredInteger(i32),
    Long(i64),
    Float(NotNan<f32>),
    InferredFloat(NotNan<f32>),
    Double(NotNan<f64>),
    String(String),
    Unit,
    Underscore,
    Negate(Box<SemanticExpression>),
    Invert(Box<SemanticExpression>),
    Reference(Box<ParsedPlaceExpression>),
    Dereference(Box<ParsedPlaceExpression>),
    Arithmetic(
        Box<SemanticExpression>,
        ArithmeticOperator,
        Box<SemanticExpression>,
    ),
    Comparison(
        Box<SemanticExpression>,
        ComparisonOperator,
        Box<SemanticExpression>,
    ),
    Logical(
        Box<SemanticExpression>,
        LogicalOperator,
        Box<SemanticExpression>,
    ),
    AugmentedAssignment(
        Box<ParsedPlaceExpression>,
        ArithmeticOperator,
        Box<SemanticExpression>,
    ),
    Assignment(Box<ParsedAssigneeExpression>, Box<SemanticExpression>),
    List(Vec<SemanticExpression>),
    Compound(HashMap<String, SemanticExpression>),
    Score(SemanticPlayerScore),
    Data(Box<SemanticData>),
    Condition(bool, Box<SemanticExecuteIfSubcommand>),
    Command(Box<SemanticCommand>),
    Index(Box<SemanticExpression>, Box<SemanticExpression>),
    FieldAccess(Box<SemanticExpression>, String),
    AsCast(Box<SemanticExpression>, SemanticDataType),
    ToCast(
        Option<NotNan<f32>>,
        Box<SemanticExpression>,
        RuntimeStorageType,
    ),
    Tuple(Vec<SemanticExpression>),
    Call(Box<SemanticExpression>, Vec<SemanticExpression>),
    Value(HighValueId, Vec<SemanticDataType>),
    RegularStruct(
        HighRegularStructId,
        Vec<SemanticDataType>,
        HashMap<String, SemanticExpression>,
    ),
    TupleStruct(
        HighTupleStructId,
        Vec<SemanticDataType>,
        Vec<SemanticExpression>,
    ),
    UnitStruct(HighUnitStructId),
    If {
        condition: Box<SemanticExpression>,

        body: Box<SemanticExpression>,
        else_body: Option<Box<SemanticExpression>>,
    },
    Block(Vec<SemanticStatement>, Option<Box<SemanticExpression>>),
    WhileLoop(Box<SemanticExpression>, Box<SemanticExpression>),
    Loop(Box<SemanticExpression>),
    ForLoop(
        bool,
        Box<SemanticPattern>,
        Box<SemanticExpression>,
        Box<SemanticExpression>,
    ),
    ResourceLocation(Box<SemanticSupportsExpressionSigil<ResourceLocation>>),
    EntitySelector(Box<SemanticSupportsExpressionSigil<SemanticEntitySelector>>),
    Coordinates(Box<SemanticSupportsExpressionSigil<SemanticCoordinates>>),
    Return(Box<SemanticExpression>),
    // TODO ByteArray(Vec<i8>),
    // TODO IntegerArray(Vec<i32>),
    // TODO LongArray(Vec<i64>),
}

impl SemanticExpressionKind {
    #[must_use]
    pub const fn with(self, data_type: SemanticDataType) -> SemanticExpression {
        SemanticExpression {
            kind: self,
            data_type,
        }
    }

    #[must_use]
    pub fn definitely_diverges(&self) -> bool {
        match self {
            Self::Return(..) => true,

            Self::Block(statements, tail_expression) => {
                for statement in statements {
                    if statement.definitely_diverges() {
                        return true;
                    }
                }

                tail_expression
                    .as_ref()
                    .is_some_and(|expression| expression.kind.definitely_diverges())
            }

            Self::If {
                condition,
                body,
                else_body,
            } => {
                if condition.kind.definitely_diverges() {
                    return true;
                }

                if let Some(else_body) = else_body {
                    body.kind.definitely_diverges() && else_body.kind.definitely_diverges()
                } else {
                    false
                }
            }

            Self::Loop(body) => !body.kind.contains_break(),

            Self::Arithmetic(left, _, right)
            | Self::Comparison(left, _, right)
            | Self::Logical(left, _, right) => {
                left.kind.definitely_diverges() || right.kind.definitely_diverges()
            }

            Self::AugmentedAssignment(_, _, right)
            | Self::Assignment(_, right)
            | Self::Index(_, right) => right.kind.definitely_diverges(),

            Self::Negate(expression)
            | Self::Invert(expression)
            | Self::AsCast(expression, _)
            | Self::ToCast(_, expression, _) => expression.kind.definitely_diverges(),
            Self::Call(callee, arguments) => {
                callee.kind.definitely_diverges()
                    || arguments
                        .iter()
                        .any(|expression| expression.kind.definitely_diverges())
            }
            Self::List(expressions)
            | Self::Tuple(expressions)
            | Self::TupleStruct(_, _, expressions) => expressions
                .iter()
                .any(|expression| expression.kind.definitely_diverges()),
            Self::Compound(compound) | Self::RegularStruct(_, _, compound) => compound
                .values()
                .any(|expression| expression.kind.definitely_diverges()),
            Self::WhileLoop(expression, _) => expression.kind.definitely_diverges(),
            Self::ForLoop(_, _, expression, _) => expression.kind.definitely_diverges(),

            _ => false,
        }
    }

    #[inline]
    #[must_use]
    pub fn contains_break(&self) -> bool {
        matches!(self.get_early_return_type(), Some(EarlyReturnType::Break))
    }

    #[must_use]
    pub fn get_early_return_type(&self) -> Option<EarlyReturnType> {
        match self {
            Self::If {
                body, else_body, ..
            } => body.kind.get_early_return_type().or_else(|| {
                else_body
                    .as_ref()
                    .and_then(|else_body| else_body.kind.get_early_return_type())
            }),

            Self::Block(statements, tail_expression) => {
                for statement in statements {
                    if let Some(kind) = statement.get_early_return_type() {
                        return Some(kind);
                    }
                }

                if let Some(tail_expression) = tail_expression
                    && let Some(kind) = tail_expression.kind.get_early_return_type()
                {
                    return Some(kind);
                }

                None
            }

            Self::WhileLoop(_, body) => body.kind.get_early_return_type(),
            Self::Loop(body) => body.kind.get_early_return_type(),
            Self::ForLoop(_, _, _, body) => body.kind.get_early_return_type(),

            Self::Return(..) => Some(EarlyReturnType::Return),

            _ => None,
        }
    }

    #[inline]
    #[must_use]
    pub fn returns_early(&self) -> bool {
        self.get_early_return_type().is_some()
    }

    #[must_use]
    pub const fn can_be_referenced(&self) -> bool {
        matches!(self, Self::Value(..))
    }

    #[must_use]
    pub const fn is_index_out_of_bounds(&self, index: &SemanticExpression) -> Option<bool> {
        let Self::List(expressions) = self else {
            return None;
        };

        let SemanticExpression {
            kind: Self::Integer(index) | Self::InferredInteger(index),
            ..
        } = index
        else {
            return None;
        };

        Some((*index as usize) >= expressions.len())
    }

    #[must_use]
    pub fn resolve(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> Expression {
        match self {
            Self::Negate(expression) => {
                let expression = expression.kind.resolve(datapack, ctx);

                expression.negate(datapack, ctx).unwrap()
            }
            Self::Invert(expression) => {
                let expression = expression.kind.resolve(datapack, ctx);

                expression.invert().unwrap()
            }
            Self::Reference(expression) => {
                let expression = expression.resolve(datapack, ctx);

                Expression::Reference(Box::new(expression))
            }
            Self::Dereference(place) => {
                let place = place.resolve(datapack, ctx);

                place
                    .resolve(datapack, ctx)
                    .dereference(datapack, ctx)
                    .unwrap()
            }
            Self::Arithmetic(left, operator, right) => {
                let left = left.kind.resolve(datapack, ctx);
                let right = right.kind.resolve(datapack, ctx);

                left.perform_arithmetic(datapack, ctx, operator, right)
            }
            Self::Comparison(left, operator, right) => {
                let left = left.kind.resolve(datapack, ctx);
                let right = right.kind.resolve(datapack, ctx);

                left.perform_comparison(datapack, ctx, operator, right)
            }
            Self::Logical(left, operator, right) => {
                let left = left.kind.resolve(datapack, ctx);
                let right = right.kind.resolve(datapack, ctx);

                left.perform_logical_operation(datapack, ctx, operator, right)
            }
            Self::AugmentedAssignment(target, operator, value) => {
                let target = target.resolve(datapack, ctx);
                let value = value.kind.resolve(datapack, ctx);

                target.augmented_assign(datapack, ctx, operator, value);

                Expression::Unit
            }
            Self::Assignment(target, value) => {
                let target = target.resolve(datapack, ctx);
                let value = value.kind.resolve(datapack, ctx);

                target.assign(datapack, ctx, value);

                Expression::Unit
            }
            Self::List(expressions) => Expression::List(
                expressions
                    .into_iter()
                    .map(|expr| expr.kind.resolve(datapack, ctx))
                    .collect::<Vec<_>>(),
            ),
            Self::Compound(compound) => Expression::Compound(
                compound
                    .into_iter()
                    .map(|(key, value)| (key, value.kind.resolve(datapack, ctx)))
                    .collect::<BTreeMap<_, _>>(),
            ),
            Self::Score(score) => {
                let score = score.compile(datapack, ctx);

                Expression::Score(score)
            }
            Self::Data(data) => {
                let data = data.compile(datapack, ctx);

                Expression::Data(data)
            }
            Self::Condition(inverted, condition) => {
                let condition = condition.compile(datapack, ctx);

                let result_score = datapack.get_unique_score();

                ctx.add_command(
                    datapack,
                    condition
                        .into_subcommand(inverted)
                        .store_success_score(result_score.score.clone()),
                );

                Expression::Score(result_score)
            }
            Self::Command(command) => {
                let command = command.compile(datapack, ctx);

                let result_score = datapack.get_unique_score();

                ctx.add_command(
                    datapack,
                    command.run().store_result_score(result_score.score.clone()),
                );

                Expression::Score(result_score)
            }
            Self::Index(target, index) => {
                let target = target.kind.resolve(datapack, ctx);
                let index = index.kind.resolve(datapack, ctx);

                target.index(ctx, index).unwrap()
            }
            Self::FieldAccess(target, field) => {
                let target = target.kind.resolve(datapack, ctx);

                target.access_field(FieldAccessType::Name, field).unwrap()
            }
            Self::AsCast(expression, data_type) => {
                let expression = expression.kind.resolve(datapack, ctx);
                let data_type = data_type.resolve(datapack).unwrap();

                expression.cast_to(data_type).unwrap()
            }
            Self::ToCast(scale, expression, runtime_storage_type) => {
                let expression = expression.kind.resolve(datapack, ctx);

                match runtime_storage_type {
                    RuntimeStorageType::Score => {
                        if let Some(scale) = scale {
                            expression.to_score_scale(datapack, ctx, scale)
                        } else {
                            expression.to_unique_score(datapack, ctx)
                        }
                    }
                    RuntimeStorageType::Data => {
                        let data = if let Some(scale) = scale {
                            expression.to_data_scale(datapack, ctx, scale)
                        } else {
                            expression.as_data(datapack, ctx, true)
                        };

                        Expression::Data(data)
                    }
                }
            }
            Self::Tuple(expressions) => Expression::Tuple(
                expressions
                    .into_iter()
                    .map(|expression| expression.kind.resolve(datapack, ctx))
                    .collect(),
            ),
            Self::RegularStruct(id, generic_types, field_expressions) => {
                let declaration = datapack.semantic_environment.get_regular_struct(id);

                let name = declaration.name.clone();

                let generic_types = generic_types
                    .into_iter()
                    .map(|data_type| data_type.resolve(datapack).unwrap())
                    .collect();

                let field_types = field_expressions
                    .iter()
                    .map(|(name, expression)| {
                        let data_type = expression.data_type.clone().resolve(datapack).unwrap();

                        (name.clone(), data_type)
                    })
                    .collect();

                let id = datapack.declare_monomorphized_regular_struct(
                    id.into(),
                    name,
                    generic_types,
                    field_types,
                );

                let field_expressions = field_expressions
                    .into_iter()
                    .map(|(name, expression)| {
                        let expression = expression.kind.resolve(datapack, ctx);

                        (name, expression)
                    })
                    .collect();

                Expression::RegularStruct(id, field_expressions)
            }
            Self::TupleStruct(id, generic_types, field_expressions) => {
                let declaration = datapack.semantic_environment.get_tuple_struct(id);

                let name = declaration.name.clone();

                let generic_types = generic_types
                    .into_iter()
                    .map(|data_type| data_type.resolve(datapack).unwrap())
                    .collect();

                let field_types = field_expressions
                    .iter()
                    .map(|expression| expression.data_type.clone().resolve(datapack).unwrap())
                    .collect();

                let id = datapack.declare_monomorphized_tuple_struct(
                    id,
                    name,
                    generic_types,
                    field_types,
                );

                let field_expressions = field_expressions
                    .into_iter()
                    .map(|expression| expression.kind.resolve(datapack, ctx))
                    .collect();

                Expression::TupleStruct(id, field_expressions)
            }
            Self::UnitStruct(id) => {
                let declaration = datapack.semantic_environment.get_unit_struct(id);

                let name = declaration.name.clone();

                let id = datapack.declare_monomorphized_unit_struct(id, name);

                Expression::UnitStruct(id)
            }
            Self::Underscore => unreachable!(),
            Self::Boolean(value) => Expression::Boolean(value),
            Self::Byte(value) => Expression::Byte(value),
            Self::Short(value) => Expression::Short(value),
            Self::Integer(value) | Self::InferredInteger(value) => Expression::Integer(value),
            Self::Long(value) => Expression::Long(value),
            Self::Float(value) | Self::InferredFloat(value) => Expression::Float(value),
            Self::Double(value) => Expression::Double(value),
            Self::String(value) => Expression::String(value),
            Self::Unit => Expression::Unit,
            Self::Value(id, generic_types) => {
                let id = datapack
                    .get_monomorphized_value_id(id, &generic_types)
                    .unwrap();

                let declaration = datapack.get_value(id);

                match declaration {
                    ValueDeclaration::Variable(..) => {
                        let id = VariableId(id.0);

                        datapack.get_variable_value(id)
                    }
                    ValueDeclaration::Constant(..) => {
                        let id = ConstantId(id.0);

                        datapack.get_constant_value(id)
                    }
                    ValueDeclaration::Function(..) => Expression::Function(FunctionId(id.0)),
                }
            }
            Self::Block(statements, tail_expression) => {
                for statement in statements {
                    statement.compile_as_statement(datapack, ctx);
                }

                tail_expression.map_or(Expression::Unit, |tail_expression| {
                    tail_expression.kind.resolve(datapack, ctx)
                })
            }
            Self::If {
                condition,
                body,
                else_body,
            } => {
                let output_data = datapack.get_unique_data();

                compile_if_internal(
                    datapack,
                    ctx,
                    *condition,
                    *body,
                    else_body,
                    Some(output_data.clone()),
                );

                Expression::Data(output_data)
            }
            Self::Call(callee, arguments) => {
                let callee = callee.kind.resolve(datapack, ctx);

                let arguments = arguments
                    .into_iter()
                    .map(|argument| argument.kind.resolve(datapack, ctx))
                    .collect::<Vec<_>>();

                callee.call(datapack, ctx, arguments, true).unwrap()
            }
            Self::WhileLoop(condition, body) => {
                let while_function_paths = datapack.get_unique_function_paths();
                let while_function_resource_location = ResourceLocation::new_namespace_paths(
                    datapack.current_namespace_name(),
                    while_function_paths.clone(),
                );

                let mut condition_ctx = ctx.create_child_ctx();
                let (inverted, condition) = condition
                    .kind
                    .clone()
                    .resolve(datapack, &mut condition_ctx)
                    .to_execute_condition(datapack, &mut condition_ctx)
                    .unwrap();

                let subcommand = if body.kind.returns_early() {
                    ExecuteSubcommand::run_return_fail()
                        .unless_function(while_function_resource_location.clone())
                } else {
                    Command::Function(while_function_resource_location.clone(), None).run()
                };

                condition_ctx.add_command(
                    datapack,
                    condition.clone().into_subcommand(inverted).then(subcommand),
                );

                let mut while_body_ctx = ctx.create_child_ctx();
                while_body_ctx.loop_info = Some(LoopInfo {
                    resource_location: while_function_resource_location,
                    type_: LoopType::While(inverted, Box::new(condition)),
                });

                body.kind
                    .compile_as_statement(datapack, &mut while_body_ctx);

                while_body_ctx.extend_context(condition_ctx.clone());
                ctx.extend_context(condition_ctx);

                datapack.compile_and_add_to_function(&while_function_paths, &mut while_body_ctx);

                Expression::Unit
            }
            Self::Loop(body) => {
                let loop_function_paths = datapack.get_unique_function_paths();
                let loop_function_resource_location = ResourceLocation::new_namespace_paths(
                    datapack.current_namespace_name(),
                    loop_function_paths.clone(),
                );

                let iteration_command = if body.kind.returns_early() {
                    ExecuteSubcommand::If(
                        true,
                        ExecuteIfSubcommand::Function(
                            loop_function_resource_location.clone(),
                            Box::new(ExecuteSubcommand::run_return_fail()),
                        ),
                    )
                    .into()
                } else {
                    Command::Function(loop_function_resource_location.clone(), None)
                };

                let mut loop_body_ctx = ctx.create_child_ctx();

                loop_body_ctx.loop_info = Some(LoopInfo {
                    resource_location: loop_function_resource_location,
                    type_: LoopType::Loop,
                });

                body.kind.compile_as_statement(datapack, &mut loop_body_ctx);

                loop_body_ctx.add_command(datapack, iteration_command.clone());
                ctx.add_command(datapack, iteration_command);

                datapack.compile_and_add_to_function(&loop_function_paths, &mut loop_body_ctx);

                Expression::Unit
            }
            Self::ForLoop(is_reversed, pattern, iterable, body) => {
                let iterable_type = iterable
                    .data_type
                    .resolve(datapack)
                    .unwrap()
                    .get_iterable_type()
                    .unwrap();

                let iterable = iterable.kind.resolve(datapack, ctx);

                match iterable.try_into_iter(is_reversed) {
                    Ok(expressions) => {
                        let pattern = *pattern;

                        for expression in expressions {
                            pattern.clone().destructure(
                                datapack,
                                ctx,
                                iterable_type.clone(),
                                expression,
                            );

                            body.kind.clone().compile_as_statement(datapack, ctx);
                        }
                    }
                    Err(iterable) => {
                        if iterable_type.equals(&DataType::String) {
                            iterate_string(datapack, ctx, is_reversed, *pattern, iterable, *body);
                        } else {
                            iterate_data(
                                datapack,
                                ctx,
                                is_reversed,
                                *pattern,
                                iterable_type,
                                iterable,
                                *body,
                            );
                        }
                    }
                }

                Expression::Unit
            }
            Self::ResourceLocation(resource_location) => {
                let resource_location = resource_location.compile(datapack, ctx);

                Expression::ResourceLocation(resource_location)
            }
            Self::EntitySelector(selector) => {
                let selector = selector.compile(datapack, ctx);

                Expression::EntitySelector(selector)
            }
            Self::Coordinates(coordinates) => {
                let coordinates = coordinates.compile(datapack, ctx);

                Expression::Coordinates(coordinates)
            }
            Self::Return(expression) => {
                let expression = expression.kind.resolve(datapack, ctx);

                let target = datapack.function_return_targets.last().unwrap().clone();

                target.assign(datapack, ctx, expression);

                ctx.add_command(datapack, Command::Return(ReturnCommand::Fail));

                Expression::Never
            }
        }
    }

    #[must_use]
    pub fn resolve_constant(self, datapack: &mut Datapack) -> Expression {
        match self {
            Self::Boolean(value) => Expression::Boolean(value),
            Self::Byte(value) => Expression::Byte(value),
            Self::Short(value) => Expression::Short(value),
            Self::Integer(value) => Expression::Integer(value),
            Self::InferredInteger(value) => Expression::Integer(value),
            Self::Long(value) => Expression::Long(value),
            Self::Float(value) => Expression::Float(value),
            Self::InferredFloat(value) => Expression::Float(value),
            Self::Double(value) => Expression::Double(value),
            Self::String(value) => Expression::String(value),
            Self::Unit => todo!(),
            Self::Underscore => todo!(),
            Self::Negate(semantic_expression) => todo!(),
            Self::Invert(semantic_expression) => todo!(),
            Self::Reference(parsed_place_expression) => todo!(),
            Self::Dereference(parsed_place_expression) => todo!(),
            Self::Arithmetic(semantic_expression, arithmetic_operator, semantic_expression1) => {
                todo!()
            }
            Self::Comparison(semantic_expression, comparison_operator, semantic_expression1) => {
                todo!()
            }
            Self::Logical(semantic_expression, logical_operator, semantic_expression1) => todo!(),
            Self::AugmentedAssignment(
                parsed_place_expression,
                arithmetic_operator,
                semantic_expression,
            ) => todo!(),
            Self::Assignment(parsed_assignee_expression, semantic_expression) => todo!(),
            Self::List(semantic_expressions) => todo!(),
            Self::Compound(hash_map) => todo!(),
            Self::Score(semantic_player_score) => todo!(),
            Self::Data(semantic_data) => todo!(),
            Self::Condition(_, semantic_execute_if_subcommand) => todo!(),
            Self::Command(semantic_command) => todo!(),
            Self::Index(semantic_expression, semantic_expression1) => todo!(),
            Self::FieldAccess(semantic_expression, _) => todo!(),
            Self::AsCast(semantic_expression, semantic_data_type) => todo!(),
            Self::ToCast(not_nan, semantic_expression, runtime_storage_type) => todo!(),
            Self::Tuple(semantic_expressions) => todo!(),
            Self::Call(semantic_expression, semantic_expressions) => todo!(),
            Self::Value(high_value_id, semantic_data_types) => todo!(),
            Self::RegularStruct(id, generic_types, hash_map) => todo!(),
            Self::TupleStruct(id, generic_types, semantic_expressions) => {
                todo!()
            }
            Self::UnitStruct(id) => {
                todo!()
            }
            Self::If {
                condition,
                body,
                else_body,
            } => todo!(),
            Self::Block(semantic_statements, semantic_expression) => todo!(),
            Self::WhileLoop(semantic_expression, semantic_expression1) => todo!(),
            Self::Loop(semantic_expression) => todo!(),
            Self::ForLoop(_, semantic_pattern, semantic_expression, semantic_expression1) => {
                todo!()
            }
            Self::ResourceLocation(semantic_supports_expression_sigil) => todo!(),
            Self::EntitySelector(semantic_supports_expression_sigil) => todo!(),
            Self::Coordinates(semantic_supports_expression_sigil) => todo!(),
            Self::Return(semantic_expression) => todo!(),
        }
    }

    pub fn compile_as_statement(self, datapack: &mut Datapack, ctx: &mut CompileContext) {
        match self {
            Self::Assignment(target, value) => {
                let target = target.resolve(datapack, ctx);

                let value = value.kind.resolve(datapack, ctx);

                target.assign(datapack, ctx, value);
            }
            Self::AugmentedAssignment(target, operator, value) => {
                let target = target.resolve(datapack, ctx);
                let value = value.kind.resolve(datapack, ctx);

                target.augmented_assign(datapack, ctx, operator, value);
            }
            Self::List(list) => {
                for element in list {
                    element.kind.compile_as_statement(datapack, ctx);
                }
            }
            Self::Compound(compound) => {
                for value in compound.into_values() {
                    value.kind.compile_as_statement(datapack, ctx);
                }
            }
            Self::Condition(_, condition) => {
                let condition = condition.compile(datapack, ctx);

                ctx.add_command(datapack, condition.into_subcommand(false));
            }
            Self::Command(command) => {
                let command = command.compile(datapack, ctx);

                ctx.add_command(datapack, command);
            }
            Self::AsCast(expression, _) | Self::ToCast(_, expression, _) => {
                expression.kind.compile_as_statement(datapack, ctx);
            }
            Self::Tuple(tuple) => {
                for element in tuple {
                    element.kind.compile_as_statement(datapack, ctx);
                }
            }
            Self::RegularStruct(_, _, field_expressions) => {
                for field_expression in field_expressions.into_values() {
                    field_expression.kind.compile_as_statement(datapack, ctx);
                }
            }
            Self::TupleStruct(_, _, field_expressions) => {
                for field_expression in field_expressions {
                    field_expression.kind.compile_as_statement(datapack, ctx);
                }
            }
            Self::UnitStruct(_) => {}
            Self::Block(statements, tail_expression) => {
                for statement in statements {
                    statement.compile_as_statement(datapack, ctx);
                }

                if let Some(tail_expression) = tail_expression {
                    tail_expression.kind.compile_as_statement(datapack, ctx);
                }
            }
            Self::If {
                condition,
                body,
                else_body,
            } => {
                compile_if_internal(datapack, ctx, *condition, *body, else_body, None);
            }
            Self::Call(callee, arguments) => {
                let callee = callee.kind.resolve(datapack, ctx);

                let arguments = arguments
                    .into_iter()
                    .map(|argument| argument.kind.resolve(datapack, ctx))
                    .collect::<Vec<_>>();

                callee.call(datapack, ctx, arguments, false);
            }
            Self::WhileLoop(condition, body) => {
                let while_function_paths = datapack.get_unique_function_paths();
                let while_function_resource_location = ResourceLocation::new_namespace_paths(
                    datapack.current_namespace_name(),
                    while_function_paths.clone(),
                );

                let mut condition_ctx = ctx.create_child_ctx();
                let (inverted, condition) = condition
                    .kind
                    .clone()
                    .resolve(datapack, &mut condition_ctx)
                    .to_execute_condition(datapack, &mut condition_ctx)
                    .unwrap();

                let subcommand = if body.kind.returns_early() {
                    ExecuteSubcommand::run_return_fail()
                        .unless_function(while_function_resource_location.clone())
                } else {
                    Command::Function(while_function_resource_location.clone(), None).run()
                };

                condition_ctx.add_command(
                    datapack,
                    condition.clone().into_subcommand(inverted).then(subcommand),
                );

                let mut while_body_ctx = ctx.create_child_ctx();
                while_body_ctx.loop_info = Some(LoopInfo {
                    resource_location: while_function_resource_location,
                    type_: LoopType::While(inverted, Box::new(condition)),
                });

                body.kind
                    .compile_as_statement(datapack, &mut while_body_ctx);

                while_body_ctx.extend_context(condition_ctx.clone());
                ctx.extend_context(condition_ctx);

                datapack.compile_and_add_to_function(&while_function_paths, &mut while_body_ctx);
            }
            Self::Loop(body) => {
                let loop_function_paths = datapack.get_unique_function_paths();
                let loop_function_resource_location = ResourceLocation::new_namespace_paths(
                    datapack.current_namespace_name(),
                    loop_function_paths.clone(),
                );

                let iteration_command = if body.kind.returns_early() {
                    Command::Execute(
                        ExecuteIfSubcommand::Function(
                            loop_function_resource_location.clone(),
                            Box::new(ExecuteSubcommand::run_return_fail()),
                        )
                        .unless(),
                    )
                } else {
                    Command::Function(loop_function_resource_location.clone(), None)
                };

                let mut loop_body_ctx = ctx.create_child_ctx();
                loop_body_ctx.loop_info = Some(LoopInfo {
                    resource_location: loop_function_resource_location,
                    type_: LoopType::Loop,
                });

                body.kind.compile_as_statement(datapack, &mut loop_body_ctx);

                loop_body_ctx.add_command(datapack, iteration_command.clone());
                ctx.add_command(datapack, iteration_command);

                datapack.compile_and_add_to_function(&loop_function_paths, &mut loop_body_ctx);
            }
            Self::ForLoop(is_reversed, pattern, iterable, body) => {
                let iterable_type = iterable
                    .data_type
                    .resolve(datapack)
                    .unwrap()
                    .get_iterable_type()
                    .unwrap();

                let iterable = iterable.kind.resolve(datapack, ctx);

                match iterable.try_into_iter(is_reversed) {
                    Ok(expressions) => {
                        let pattern = *pattern;

                        for expression in expressions {
                            pattern.clone().destructure(
                                datapack,
                                ctx,
                                iterable_type.clone(),
                                expression,
                            );

                            body.kind.clone().compile_as_statement(datapack, ctx);
                        }
                    }
                    Err(iterable) => {
                        if iterable_type.equals(&DataType::String) {
                            iterate_string(datapack, ctx, is_reversed, *pattern, iterable, *body);
                        } else {
                            iterate_data(
                                datapack,
                                ctx,
                                is_reversed,
                                *pattern,
                                iterable_type,
                                iterable,
                                *body,
                            );
                        }
                    }
                }
            }
            Self::Underscore => {
                #[cfg(debug_assertions)]
                unreachable!();
            }
            Self::EntitySelector(selector) => {
                selector.compile_as_statement(datapack, ctx);
            }
            Self::Return(expression) => {
                let expression = expression.kind.resolve(datapack, ctx);

                let target = datapack.function_return_targets.last().unwrap().clone();

                target.assign(datapack, ctx, expression);

                ctx.add_command(datapack, Command::Return(ReturnCommand::Fail));
            }
            Self::Data(data) => {
                let _ = data.compile(datapack, ctx);
            }
            Self::Negate(expression) => {
                expression.kind.compile_as_statement(datapack, ctx);
            }
            Self::Invert(expression) => {
                expression.kind.compile_as_statement(datapack, ctx);
            }
            Self::Reference(_) | Self::Dereference(_) => {}
            Self::Arithmetic(left, _, right) => {
                left.kind.compile_as_statement(datapack, ctx);
                right.kind.compile_as_statement(datapack, ctx);
            }
            Self::Comparison(left, _, right) => {
                left.kind.compile_as_statement(datapack, ctx);
                right.kind.compile_as_statement(datapack, ctx);
            }
            Self::Logical(left, _, right) => {
                left.kind.compile_as_statement(datapack, ctx);
                right.kind.compile_as_statement(datapack, ctx);
            }
            Self::Score(score) => {
                let _ = score.compile(datapack, ctx);
            }
            Self::Index(_, index) => {
                index.kind.compile_as_statement(datapack, ctx);
            }
            Self::FieldAccess(_, _) => {}
            Self::ResourceLocation(resource_location) => {
                resource_location.compile_as_statement(datapack, ctx);
            }
            Self::Coordinates(coordinates) => {
                coordinates.compile_as_statement(datapack, ctx);
            }
            Self::Boolean(..) => {}
            Self::Byte(..) => {}
            Self::Short(..) => {}
            Self::Integer(..) => {}
            Self::InferredInteger(..) => {}
            Self::Long(..) => {}
            Self::Float(..) => {}
            Self::InferredFloat(..) => {}
            Self::Double(..) => {}
            Self::String(..) => {}
            Self::Unit => {}
            Self::Value(..) => {}
        }
    }
}

#[derive(Debug, Clone)]
pub struct SemanticExpression {
    pub kind: SemanticExpressionKind,
    pub data_type: SemanticDataType,
}
