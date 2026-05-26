use std::collections::{BTreeMap, HashMap};

use la_arena::Idx;
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

use crate::{
    compile_context::{CompileContext, LoopInfo, LoopType},
    datapack::Datapack,
    field_access_type::FieldAccessType,
    low::data::GeneratedData,
    low::{
        data_type::DataType,
        environment::value::{ValueDeclarationKind, function::FunctionId, variable::VariableId},
        expression::Expression,
    },
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator},
    runtime_storage::RuntimeStorageType,
    typed::{
        arena::{Typed, TypedAstArena},
        coordinate::TypedCoordinates,
        data::TypedData,
        data_type::SemanticDataType,
        entity_selector::TypedEntitySelector,
        environment::{
            r#type::r#struct::{regular::HighRegularStructId, tuple::HighTupleStructId},
            value::HighValueId,
        },
        expression::{
            assignee::{TypedAssigneeExpression, TypedAssigneeExpressionId},
            command::{TypedCommand, execute::subcommand::r#if::TypedExecuteIfSubcommand},
            place::{TypedPlaceExpression, TypedPlaceExpressionId},
        },
        pattern::TypedPattern,
        player_score::TypedPlayerScore,
        statement::{EarlyReturnType, TypedStatement},
        supports_expression_sigil::TypedSupportsExpressionSigil,
    },
};

pub mod assignee;
pub mod command;
pub mod literal;
pub mod place;

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
    arena: &TypedAstArena,
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    condition: TypedExpressionId,
    body: TypedExpressionId,
    else_body: Option<TypedExpressionId>,
    output_data: Option<GeneratedData>,
) {
    let mut body_ctx = ctx.create_child_ctx();
    let main_body_returns_early = TypedExpression::returns_early(body, arena);
    let body = TypedExpression::resolve(body, arena, datapack, &mut body_ctx);

    if let Some(output_data) = &output_data {
        body.assign_to_data(datapack, &mut body_ctx, output_data.clone());
    }

    let (invert, condition) = TypedExpression::resolve(condition, arena, datapack, ctx)
        .to_execute_condition(datapack, ctx)
        .unwrap();

    if let Some(else_body) = else_body {
        let mut else_body_ctx = ctx.create_child_ctx();
        let else_body_returns_early = TypedExpression::returns_early(else_body, arena);
        let else_body = TypedExpression::resolve(else_body, arena, datapack, &mut else_body_ctx);

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
    arena: &TypedAstArena,
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    is_reversed: bool,
    pattern: Idx<TypedPattern>,
    iterable: Expression,
    body: TypedExpressionId,
) {
    let (unique_data, name) = datapack.get_unique_data_named();
    let unique_data_2 = datapack.get_unique_data();

    iterable.assign_to_data(datapack, ctx, unique_data.clone());

    let mut for_body_ctx = CompileContext::default();

    TypedPattern::destructure(
        pattern,
        arena,
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

    TypedExpression::compile_as_statement(body, arena, datapack, &mut for_body_ctx);

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

#[allow(clippy::too_many_arguments)]
fn iterate_data(
    arena: &TypedAstArena,
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    is_reversed: bool,
    pattern: Idx<TypedPattern>,
    iterable_type: DataType,
    iterable: Expression,
    body: TypedExpressionId,
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

    TypedPattern::destructure(
        pattern,
        arena,
        datapack,
        &mut for_body_ctx,
        iterable_type,
        Expression::Data(destructure_unique_data),
    );

    TypedExpression::compile_as_statement(body, arena, datapack, &mut for_body_ctx);

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

pub type TypedExpressionId = Idx<Typed<TypedExpression>>;

#[derive(Debug, Clone)]
pub enum TypedExpression {
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
    Negate(TypedExpressionId),
    Invert(TypedExpressionId),
    Reference(TypedPlaceExpressionId),
    Dereference(TypedPlaceExpressionId),
    Arithmetic(TypedExpressionId, ArithmeticOperator, TypedExpressionId),
    Comparison(TypedExpressionId, ComparisonOperator, TypedExpressionId),
    Logical(TypedExpressionId, LogicalOperator, TypedExpressionId),
    AugmentedAssignment(
        TypedPlaceExpressionId,
        ArithmeticOperator,
        TypedExpressionId,
    ),
    Assignment(TypedAssigneeExpressionId, TypedExpressionId),
    List(Vec<TypedExpressionId>),
    Compound(HashMap<String, TypedExpressionId>),
    Score(TypedPlayerScore),
    Data(Box<TypedData>),
    Condition(bool, Box<TypedExecuteIfSubcommand>),
    Command(Box<TypedCommand>),
    Index(TypedExpressionId, TypedExpressionId),
    FieldAccess(TypedExpressionId, String),
    AsCast(TypedExpressionId, SemanticDataType),
    ToCast(Option<NotNan<f32>>, TypedExpressionId, RuntimeStorageType),
    Tuple(Vec<TypedExpressionId>),
    Call(TypedExpressionId, Vec<TypedExpressionId>),
    Value(HighValueId, Vec<SemanticDataType>),
    RegularStruct(
        HighRegularStructId,
        Vec<SemanticDataType>,
        HashMap<String, TypedExpressionId>,
    ),
    TupleStruct(
        HighTupleStructId,
        Vec<SemanticDataType>,
        Vec<TypedExpressionId>,
    ),
    If {
        condition: TypedExpressionId,

        body: TypedExpressionId,
        else_body: Option<TypedExpressionId>,
    },
    Block(Vec<Idx<TypedStatement>>, Option<TypedExpressionId>),
    WhileLoop(TypedExpressionId, TypedExpressionId),
    Loop(TypedExpressionId),
    ForLoop(
        bool,
        Idx<TypedPattern>,
        TypedExpressionId,
        TypedExpressionId,
    ),
    ResourceLocation(Box<TypedSupportsExpressionSigil<ResourceLocation>>),
    EntitySelector(Box<TypedSupportsExpressionSigil<TypedEntitySelector>>),
    Coordinates(Box<TypedSupportsExpressionSigil<TypedCoordinates>>),
    Return(TypedExpressionId),
    // TODO ByteArray(Vec<i8>),
    // TODO IntegerArray(Vec<i32>),
    // TODO LongArray(Vec<i64>),
}

impl TypedExpression {
    #[must_use]
    pub fn definitely_diverges(id: TypedExpressionId, arena: &TypedAstArena) -> bool {
        match arena.get_expression_value(id) {
            Self::Return(..) => true,

            Self::Block(statements, tail_expression) => {
                for statement in statements {
                    if TypedStatement::definitely_diverges(*statement, arena) {
                        return true;
                    }
                }

                tail_expression
                    .as_ref()
                    .is_some_and(|expression| Self::definitely_diverges(*expression, arena))
            }

            Self::If {
                condition,
                body,
                else_body,
            } => {
                if Self::definitely_diverges(*condition, arena) {
                    return true;
                }

                if let Some(else_body) = else_body {
                    Self::definitely_diverges(*body, arena)
                        && Self::definitely_diverges(*else_body, arena)
                } else {
                    false
                }
            }

            Self::Loop(body) => !Self::contains_break(*body, arena),

            Self::Arithmetic(left, _, right)
            | Self::Comparison(left, _, right)
            | Self::Logical(left, _, right) => {
                Self::definitely_diverges(*left, arena) || Self::definitely_diverges(*right, arena)
            }

            Self::AugmentedAssignment(_, _, right)
            | Self::Assignment(_, right)
            | Self::Index(_, right) => Self::definitely_diverges(*right, arena),

            Self::Negate(expression)
            | Self::Invert(expression)
            | Self::AsCast(expression, _)
            | Self::ToCast(_, expression, _) => Self::definitely_diverges(*expression, arena),
            Self::Call(callee, arguments) => {
                Self::definitely_diverges(*callee, arena)
                    || arguments
                        .iter()
                        .any(|expression| Self::definitely_diverges(*expression, arena))
            }
            Self::List(expressions)
            | Self::Tuple(expressions)
            | Self::TupleStruct(_, _, expressions) => expressions
                .iter()
                .any(|expression| Self::definitely_diverges(*expression, arena)),
            Self::Compound(compound) | Self::RegularStruct(_, _, compound) => compound
                .values()
                .any(|expression| Self::definitely_diverges(*expression, arena)),
            Self::WhileLoop(expression, _) => Self::definitely_diverges(*expression, arena),
            Self::ForLoop(_, _, expression, _) => Self::definitely_diverges(*expression, arena),

            _ => false,
        }
    }

    #[inline]
    #[must_use]
    pub fn contains_break(id: TypedExpressionId, arena: &TypedAstArena) -> bool {
        matches!(
            Self::get_early_return_type(id, arena),
            Some(EarlyReturnType::Break)
        )
    }

    #[must_use]
    pub fn get_early_return_type(
        id: TypedExpressionId,
        arena: &TypedAstArena,
    ) -> Option<EarlyReturnType> {
        match arena.get_expression_value(id) {
            Self::If {
                body, else_body, ..
            } => Self::get_early_return_type(*body, arena).or_else(|| {
                else_body
                    .as_ref()
                    .and_then(|else_body| Self::get_early_return_type(*else_body, arena))
            }),

            Self::Block(statements, tail_expression) => {
                for statement in statements {
                    if let Some(kind) = TypedStatement::get_early_return_type(*statement, arena) {
                        return Some(kind);
                    }
                }

                if let Some(tail_expression) = tail_expression
                    && let Some(kind) = Self::get_early_return_type(*tail_expression, arena)
                {
                    return Some(kind);
                }

                None
            }

            Self::WhileLoop(_, body) => Self::get_early_return_type(*body, arena),
            Self::Loop(body) => Self::get_early_return_type(*body, arena),
            Self::ForLoop(_, _, _, body) => Self::get_early_return_type(*body, arena),

            Self::Return(..) => Some(EarlyReturnType::Return),

            _ => None,
        }
    }

    #[inline]
    #[must_use]
    pub fn returns_early(id: TypedExpressionId, arena: &TypedAstArena) -> bool {
        Self::get_early_return_type(id, arena).is_some()
    }

    #[must_use]
    pub const fn can_be_referenced(&self) -> bool {
        matches!(self, Self::Value(..))
    }

    #[must_use]
    pub fn resolve(
        id: TypedExpressionId,
        arena: &TypedAstArena,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> Expression {
        match arena.get_expression_value(id) {
            Self::Negate(expression) => {
                let expression = Self::resolve(*expression, arena, datapack, ctx);

                expression.negate(datapack, ctx).unwrap()
            }
            Self::Invert(expression) => {
                let expression = Self::resolve(*expression, arena, datapack, ctx);

                expression.invert().unwrap()
            }
            Self::Reference(expression) => {
                let expression = TypedPlaceExpression::resolve(*expression, arena, datapack, ctx);

                Expression::Reference(Box::new(expression))
            }
            Self::Dereference(place) => {
                let place = TypedPlaceExpression::resolve(*place, arena, datapack, ctx);

                place
                    .resolve(datapack, ctx)
                    .dereference(datapack, ctx)
                    .unwrap()
            }
            Self::Arithmetic(left, operator, right) => {
                let left = Self::resolve(*left, arena, datapack, ctx);
                let right = Self::resolve(*right, arena, datapack, ctx);

                left.perform_arithmetic(datapack, ctx, *operator, right)
            }
            Self::Comparison(left, operator, right) => {
                let left = Self::resolve(*left, arena, datapack, ctx);
                let right = Self::resolve(*right, arena, datapack, ctx);

                left.perform_comparison(datapack, ctx, *operator, right)
            }
            Self::Logical(left, operator, right) => {
                let left = Self::resolve(*left, arena, datapack, ctx);
                let right = Self::resolve(*right, arena, datapack, ctx);

                left.perform_logical_operation(datapack, ctx, *operator, right)
            }
            Self::AugmentedAssignment(target, operator, value) => {
                let target = TypedPlaceExpression::resolve(*target, arena, datapack, ctx);
                let value = Self::resolve(*value, arena, datapack, ctx);

                target.augmented_assign(datapack, ctx, *operator, value);

                Expression::Unit
            }
            Self::Assignment(target, value) => {
                let target = TypedAssigneeExpression::resolve(*target, arena, datapack, ctx);
                let value = Self::resolve(*value, arena, datapack, ctx);

                target.assign(datapack, ctx, value);

                Expression::Unit
            }
            Self::List(expressions) => Expression::List(
                expressions
                    .iter()
                    .copied()
                    .map(|expression| Self::resolve(expression, arena, datapack, ctx))
                    .collect::<Vec<_>>(),
            ),
            Self::Compound(compound) => Expression::Compound(
                compound
                    .iter()
                    .map(|(key, value)| (key.clone(), Self::resolve(*value, arena, datapack, ctx)))
                    .collect::<BTreeMap<_, _>>(),
            ),
            Self::Score(score) => {
                let score = score.clone().compile(arena, datapack, ctx);

                Expression::Score(score)
            }
            Self::Data(data) => {
                let data = data.clone().compile(arena, datapack, ctx);

                Expression::Data(data)
            }
            Self::Condition(inverted, condition) => {
                let inverted = *inverted;

                let condition = condition.clone().compile(arena, datapack, ctx);

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
                let command = command.clone().compile(arena, datapack, ctx);

                let result_score = datapack.get_unique_score();

                ctx.add_command(
                    datapack,
                    command.run().store_result_score(result_score.score.clone()),
                );

                Expression::Score(result_score)
            }
            Self::Index(target, index) => {
                let target = Self::resolve(*target, arena, datapack, ctx);
                let index = Self::resolve(*index, arena, datapack, ctx);

                target.index(ctx, index).unwrap()
            }
            Self::FieldAccess(target, field) => {
                let target = Self::resolve(*target, arena, datapack, ctx);

                target.access_field(FieldAccessType::Name, field).unwrap()
            }
            Self::AsCast(expression, data_type) => {
                let expression = Self::resolve(*expression, arena, datapack, ctx);
                let data_type = data_type.clone().resolve(datapack).unwrap();

                expression.cast_to(data_type).unwrap()
            }
            Self::ToCast(scale, expression, runtime_storage_type) => {
                let expression = Self::resolve(*expression, arena, datapack, ctx);

                match runtime_storage_type {
                    RuntimeStorageType::Score => {
                        if let Some(scale) = scale {
                            expression.to_score_scale(datapack, ctx, *scale)
                        } else {
                            expression.to_unique_score(datapack, ctx)
                        }
                    }
                    RuntimeStorageType::Data => {
                        let data = if let Some(scale) = scale {
                            expression.to_data_scale(datapack, ctx, *scale)
                        } else {
                            expression.as_data(datapack, ctx, true)
                        };

                        Expression::Data(data)
                    }
                }
            }
            Self::Tuple(expressions) => Expression::Tuple(
                expressions
                    .iter()
                    .copied()
                    .map(|expression| Self::resolve(expression, arena, datapack, ctx))
                    .collect(),
            ),
            Self::RegularStruct(id, generic_types, field_expressions) => {
                let id = *id;

                let (module_path, visiblity, declaration) =
                    datapack.semantic_environment.get_regular_struct(id);

                let module_path = module_path.to_vec();
                let name = declaration.name.clone();

                let generic_types = generic_types
                    .iter()
                    .cloned()
                    .map(|data_type| data_type.resolve(datapack).unwrap())
                    .collect();

                let field_types = field_expressions
                    .iter()
                    .map(|(name, expression)| {
                        let data_type = arena.get_expression_type(*expression).clone();

                        let data_type = data_type.resolve(datapack).unwrap();

                        (name.clone(), data_type)
                    })
                    .collect();

                let id = datapack.declare_monomorphized_regular_struct(
                    module_path,
                    visiblity,
                    id.into(),
                    name,
                    generic_types,
                    field_types,
                );

                let field_expressions = field_expressions
                    .iter()
                    .map(|(name, expression)| {
                        let expression = Self::resolve(*expression, arena, datapack, ctx);

                        (name.clone(), expression)
                    })
                    .collect();

                Expression::RegularStruct(id, field_expressions)
            }
            Self::TupleStruct(id, generic_types, field_expressions) => {
                let id = *id;

                let (module_path, visiblity, declaration) =
                    datapack.semantic_environment.get_tuple_struct(id);

                let module_path = module_path.to_vec();
                let name = declaration.name.clone();

                let generic_types = generic_types
                    .iter()
                    .cloned()
                    .map(|data_type| data_type.resolve(datapack).unwrap())
                    .collect();

                let field_types = field_expressions
                    .iter()
                    .map(|expression| {
                        let data_type = arena.get_expression_type(*expression).clone();

                        data_type.resolve(datapack).unwrap()
                    })
                    .collect();

                let id = datapack.declare_monomorphized_tuple_struct(
                    module_path,
                    visiblity,
                    id.into(),
                    name,
                    generic_types,
                    field_types,
                );

                let field_expressions = field_expressions
                    .iter()
                    .copied()
                    .map(|expression| Self::resolve(expression, arena, datapack, ctx))
                    .collect();

                Expression::TupleStruct(id, field_expressions)
            }
            Self::Underscore => unreachable!(),
            Self::Boolean(value) => Expression::Boolean(*value),
            Self::Byte(value) => Expression::Byte(*value),
            Self::Short(value) => Expression::Short(*value),
            Self::Integer(value) | Self::InferredInteger(value) => Expression::Integer(*value),
            Self::Long(value) => Expression::Long(*value),
            Self::Float(value) | Self::InferredFloat(value) => Expression::Float(*value),
            Self::Double(value) => Expression::Double(*value),
            Self::String(value) => Expression::String(value.clone()),
            Self::Unit => Expression::Unit,
            Self::Value(id, generic_types) => {
                let id = datapack
                    .get_monomorphized_value_id(*id, generic_types)
                    .unwrap();

                let declaration = datapack.get_value(id);

                match &declaration.kind {
                    ValueDeclarationKind::Variable(..) => {
                        let id = VariableId(id.0);

                        datapack.get_variable_value(id)
                    }
                    ValueDeclarationKind::Function(..) => Expression::Function(FunctionId(id.0)),
                }
            }
            Self::Block(statements, tail_expression) => {
                for statement in statements {
                    TypedStatement::compile_as_statement(*statement, arena, datapack, ctx);
                }

                tail_expression.map_or(Expression::Unit, |tail_expression| {
                    Self::resolve(tail_expression, arena, datapack, ctx)
                })
            }
            Self::If {
                condition,
                body,
                else_body,
            } => {
                let output_data = datapack.get_unique_data();

                compile_if_internal(
                    arena,
                    datapack,
                    ctx,
                    *condition,
                    *body,
                    *else_body,
                    Some(output_data.clone()),
                );

                Expression::Data(output_data)
            }
            Self::Call(callee, arguments) => {
                let callee = Self::resolve(*callee, arena, datapack, ctx);

                let arguments = arguments
                    .iter()
                    .copied()
                    .map(|argument| Self::resolve(argument, arena, datapack, ctx))
                    .collect::<Vec<_>>();

                callee.call(arena, datapack, ctx, arguments, true).unwrap()
            }
            Self::WhileLoop(condition, body) => {
                let while_function_paths = datapack.get_unique_function_paths();
                let while_function_resource_location = ResourceLocation::new_namespace_paths(
                    datapack.current_namespace_name(),
                    while_function_paths.clone(),
                );

                let mut condition_ctx = ctx.create_child_ctx();
                let (inverted, condition) = Self::resolve(*condition, arena, datapack, ctx)
                    .to_execute_condition(datapack, &mut condition_ctx)
                    .unwrap();

                let subcommand = if Self::returns_early(*body, arena) {
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

                Self::compile_as_statement(*body, arena, datapack, &mut while_body_ctx);

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

                let iteration_command = if Self::returns_early(*body, arena) {
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

                Self::compile_as_statement(*body, arena, datapack, &mut loop_body_ctx);

                loop_body_ctx.add_command(datapack, iteration_command.clone());
                ctx.add_command(datapack, iteration_command);

                datapack.compile_and_add_to_function(&loop_function_paths, &mut loop_body_ctx);

                Expression::Unit
            }
            Self::ForLoop(is_reversed, pattern, iterable, body) => {
                let is_reversed = *is_reversed;

                let iterable_type = arena.get_expression_type(*iterable).clone();

                let iterable_type = iterable_type.resolve(datapack).unwrap();

                let iterable_type = iterable_type.get_iterable_type().unwrap();

                let iterable = Self::resolve(*iterable, arena, datapack, ctx);

                match iterable.try_into_iter(is_reversed) {
                    Ok(expressions) => {
                        for expression in expressions {
                            TypedPattern::destructure(
                                *pattern,
                                arena,
                                datapack,
                                ctx,
                                iterable_type.clone(),
                                expression,
                            );

                            Self::compile_as_statement(*body, arena, datapack, ctx);
                        }
                    }
                    Err(iterable) => {
                        if iterable_type.equals(&DataType::String) {
                            iterate_string(
                                arena,
                                datapack,
                                ctx,
                                is_reversed,
                                *pattern,
                                iterable,
                                *body,
                            );
                        } else {
                            iterate_data(
                                arena,
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
                let resource_location = resource_location.clone().compile(arena, datapack, ctx);

                Expression::ResourceLocation(resource_location)
            }
            Self::EntitySelector(selector) => {
                let selector = selector.clone().compile(arena, datapack, ctx);

                Expression::EntitySelector(selector)
            }
            Self::Coordinates(coordinates) => {
                let coordinates = coordinates.clone().compile(arena, datapack, ctx);

                Expression::Coordinates(coordinates)
            }
            Self::Return(expression) => {
                let expression = Self::resolve(*expression, arena, datapack, ctx);

                let target = datapack.function_return_targets.last().unwrap().clone();

                target.assign(datapack, ctx, expression);

                ctx.add_command(datapack, Command::Return(ReturnCommand::Fail));

                Expression::Never
            }
        }
    }

    pub fn compile_as_statement(
        id: TypedExpressionId,
        arena: &TypedAstArena,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) {
        match arena.get_expression_value(id) {
            Self::Assignment(target, value) => {
                let target = TypedAssigneeExpression::resolve(*target, arena, datapack, ctx);

                let value = Self::resolve(*value, arena, datapack, ctx);

                target.assign(datapack, ctx, value);
            }
            Self::AugmentedAssignment(target, operator, value) => {
                let operator = *operator;

                let target = TypedPlaceExpression::resolve(*target, arena, datapack, ctx);
                let value = Self::resolve(*value, arena, datapack, ctx);

                target.augmented_assign(datapack, ctx, operator, value);
            }
            Self::List(list) => {
                for element in list {
                    Self::compile_as_statement(*element, arena, datapack, ctx);
                }
            }
            Self::Compound(compound) => {
                for value in compound.values().copied() {
                    Self::compile_as_statement(value, arena, datapack, ctx);
                }
            }
            Self::Condition(_, condition) => {
                let condition = condition.clone().compile(arena, datapack, ctx);

                ctx.add_command(datapack, condition.into_subcommand(false));
            }
            Self::Command(command) => {
                let command = command.clone().compile(arena, datapack, ctx);

                ctx.add_command(datapack, command);
            }
            Self::AsCast(expression, _) | Self::ToCast(_, expression, _) => {
                Self::compile_as_statement(*expression, arena, datapack, ctx);
            }
            Self::Tuple(tuple) => {
                for element in tuple {
                    Self::compile_as_statement(*element, arena, datapack, ctx);
                }
            }
            Self::RegularStruct(_, _, field_expressions) => {
                for field_expression in field_expressions.values().copied() {
                    Self::compile_as_statement(field_expression, arena, datapack, ctx);
                }
            }
            Self::TupleStruct(_, _, field_expressions) => {
                for field_expression in field_expressions {
                    Self::compile_as_statement(*field_expression, arena, datapack, ctx);
                }
            }
            Self::Block(statements, tail_expression) => {
                for statement in statements.iter().copied() {
                    TypedStatement::compile_as_statement(statement, arena, datapack, ctx);
                }

                if let Some(tail_expression) = tail_expression {
                    Self::compile_as_statement(*tail_expression, arena, datapack, ctx);
                }
            }
            Self::If {
                condition,
                body,
                else_body,
            } => {
                compile_if_internal(arena, datapack, ctx, *condition, *body, *else_body, None);
            }
            Self::Call(callee, arguments) => {
                let callee = Self::resolve(*callee, arena, datapack, ctx);

                let arguments = arguments
                    .iter()
                    .copied()
                    .map(|argument| Self::resolve(argument, arena, datapack, ctx))
                    .collect::<Vec<_>>();

                callee.call(arena, datapack, ctx, arguments, false);
            }
            Self::WhileLoop(condition, body) => {
                let while_function_paths = datapack.get_unique_function_paths();
                let while_function_resource_location = ResourceLocation::new_namespace_paths(
                    datapack.current_namespace_name(),
                    while_function_paths.clone(),
                );

                let mut condition_ctx = ctx.create_child_ctx();
                let (inverted, condition) = Self::resolve(*condition, arena, datapack, ctx)
                    .to_execute_condition(datapack, &mut condition_ctx)
                    .unwrap();

                let subcommand = if Self::returns_early(*body, arena) {
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

                Self::compile_as_statement(*body, arena, datapack, &mut while_body_ctx);

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

                let iteration_command = if Self::returns_early(*body, arena) {
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

                Self::compile_as_statement(*body, arena, datapack, &mut loop_body_ctx);

                loop_body_ctx.add_command(datapack, iteration_command.clone());
                ctx.add_command(datapack, iteration_command);

                datapack.compile_and_add_to_function(&loop_function_paths, &mut loop_body_ctx);
            }
            Self::ForLoop(is_reversed, pattern, iterable, body) => {
                let is_reversed = *is_reversed;

                let iterable_type = arena
                    .get_expression_type(*iterable)
                    .clone()
                    .resolve(datapack)
                    .unwrap()
                    .get_iterable_type()
                    .unwrap();

                let iterable = Self::resolve(*iterable, arena, datapack, ctx);

                match iterable.try_into_iter(is_reversed) {
                    Ok(expressions) => {
                        let pattern = *pattern;

                        for expression in expressions {
                            TypedPattern::destructure(
                                pattern,
                                arena,
                                datapack,
                                ctx,
                                iterable_type.clone(),
                                expression,
                            );

                            Self::compile_as_statement(*body, arena, datapack, ctx);
                        }
                    }
                    Err(iterable) => {
                        if iterable_type.equals(&DataType::String) {
                            iterate_string(
                                arena,
                                datapack,
                                ctx,
                                is_reversed,
                                *pattern,
                                iterable,
                                *body,
                            );
                        } else {
                            iterate_data(
                                arena,
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
                selector.clone().compile_as_statement(arena, datapack, ctx);
            }
            Self::Return(expression) => {
                let expression = Self::resolve(*expression, arena, datapack, ctx);

                let target = datapack.function_return_targets.last().unwrap().clone();

                target.assign(datapack, ctx, expression);

                ctx.add_command(datapack, Command::Return(ReturnCommand::Fail));
            }
            Self::Data(data) => {
                let _ = data.clone().compile(arena, datapack, ctx);
            }
            Self::Negate(expression) => {
                Self::compile_as_statement(*expression, arena, datapack, ctx);
            }
            Self::Invert(expression) => {
                Self::compile_as_statement(*expression, arena, datapack, ctx);
            }
            Self::Reference(_) | Self::Dereference(_) => {}
            Self::Arithmetic(left, _, right) => {
                Self::compile_as_statement(*left, arena, datapack, ctx);
                Self::compile_as_statement(*right, arena, datapack, ctx);
            }
            Self::Comparison(left, _, right) => {
                Self::compile_as_statement(*left, arena, datapack, ctx);
                Self::compile_as_statement(*right, arena, datapack, ctx);
            }
            Self::Logical(left, _, right) => {
                Self::compile_as_statement(*left, arena, datapack, ctx);
                Self::compile_as_statement(*right, arena, datapack, ctx);
            }
            Self::Score(score) => {
                let _ = score.clone().compile(arena, datapack, ctx);
            }
            Self::Index(_, index) => {
                Self::compile_as_statement(*index, arena, datapack, ctx);
            }
            Self::FieldAccess(_, _) => {}
            Self::ResourceLocation(resource_location) => {
                resource_location
                    .clone()
                    .compile_as_statement(arena, datapack, ctx);
            }
            Self::Coordinates(coordinates) => {
                coordinates
                    .clone()
                    .compile_as_statement(arena, datapack, ctx);
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
