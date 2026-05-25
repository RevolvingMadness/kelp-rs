use std::collections::{BTreeMap, HashMap};

use la_arena::Idx;
use minecraft_command_types::{
    command::{
        Command,
        data::DataCommandModification,
        execute::{ExecuteIfSubcommand, ExecuteSubcommand, ScoreComparison},
        r#return::ReturnCommand,
    },
    nbt_path::{NbtPath as LowNbtPath, NbtPathNode, SNBTCompound},
    range::IntegerRange,
    resource_location::ResourceLocation,
    snbt::{SNBT, SNBTString},
};
use ordered_float::NotNan;

use crate::{
    ast_allocator::low::{LowAstAllocator, Typed},
    compile_context::{CompileContext, LoopInfo, LoopType},
    data::GeneratedData,
    datapack::Datapack,
    high::{
        environment::resolved::{
            r#type::r#struct::{regular::HighRegularStructId, tuple::HighTupleStructId},
            value::HighValueId,
        },
        expression::{
            assignee::{UnresolvedAssigneeExpression, UnresolvedAssigneeExpressionId},
            place::{UnresolvedPlaceExpression, UnresolvedPlaceExpressionId},
        },
    },
    low::{
        coordinate::Coordinates,
        data::Data,
        data_type::{
            resolved::{FieldAccessType, ResolvedDataType},
            unresolved::UnresolvedDataType,
        },
        entity_selector::EntitySelector,
        environment::value::{ValueDeclarationKind, function::FunctionId, variable::VariableId},
        expression::{
            command::{
                Command as MiddleCommand,
                execute::subcommand::r#if::ExecuteIfSubcommand as MiddleExecuteIfSubcommand,
            },
            resolved::ResolvedExpression,
        },
        pattern::UnresolvedPattern,
        player_score::PlayerScore,
        statement::{EarlyReturnType, UnresolvedStatement},
        supports_expression_sigil::SupportsExpressionSigil,
    },
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator},
    runtime_storage::RuntimeStorageType,
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
    allocator: &LowAstAllocator,
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    condition: UnresolvedExpressionId,
    body: UnresolvedExpressionId,
    else_body: Option<UnresolvedExpressionId>,
    output_data: Option<GeneratedData>,
) {
    let mut body_ctx = ctx.create_child_ctx();
    let main_body_returns_early = UnresolvedExpression::returns_early(body, allocator);
    let body = UnresolvedExpression::resolve(body, allocator, datapack, &mut body_ctx);

    if let Some(output_data) = &output_data {
        body.assign_to_data(datapack, &mut body_ctx, output_data.clone());
    }

    let (invert, condition) = UnresolvedExpression::resolve(condition, allocator, datapack, ctx)
        .to_execute_condition(datapack, ctx)
        .unwrap();

    if let Some(else_body) = else_body {
        let mut else_body_ctx = ctx.create_child_ctx();
        let else_body_returns_early = UnresolvedExpression::returns_early(else_body, allocator);
        let else_body =
            UnresolvedExpression::resolve(else_body, allocator, datapack, &mut else_body_ctx);

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
    allocator: &LowAstAllocator,
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    is_reversed: bool,
    pattern: Idx<UnresolvedPattern>,
    iterable: ResolvedExpression,
    body: UnresolvedExpressionId,
) {
    let (unique_data, name) = datapack.get_unique_data_named();
    let unique_data_2 = datapack.get_unique_data();

    iterable.assign_to_data(datapack, ctx, unique_data.clone());

    let mut for_body_ctx = CompileContext::default();

    UnresolvedPattern::destructure(
        pattern,
        allocator,
        datapack,
        &mut for_body_ctx,
        ResolvedDataType::String,
        ResolvedExpression::Data(unique_data_2.clone()),
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

    UnresolvedExpression::compile_as_statement(body, allocator, datapack, &mut for_body_ctx);

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
    let unique_path = LowNbtPath(vec![NbtPathNode::RootCompound(map)]);

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
    allocator: &LowAstAllocator,
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    is_reversed: bool,
    pattern: Idx<UnresolvedPattern>,
    iterable_type: ResolvedDataType,
    iterable: ResolvedExpression,
    body: UnresolvedExpressionId,
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

    UnresolvedPattern::destructure(
        pattern,
        allocator,
        datapack,
        &mut for_body_ctx,
        iterable_type,
        ResolvedExpression::Data(destructure_unique_data),
    );

    UnresolvedExpression::compile_as_statement(body, allocator, datapack, &mut for_body_ctx);

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

pub type UnresolvedExpressionId = Idx<Typed<UnresolvedExpression>>;

#[derive(Debug, Clone)]
pub enum UnresolvedExpression {
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
    Negate(UnresolvedExpressionId),
    Invert(UnresolvedExpressionId),
    Reference(UnresolvedPlaceExpressionId),
    Dereference(UnresolvedPlaceExpressionId),
    Arithmetic(
        UnresolvedExpressionId,
        ArithmeticOperator,
        UnresolvedExpressionId,
    ),
    Comparison(
        UnresolvedExpressionId,
        ComparisonOperator,
        UnresolvedExpressionId,
    ),
    Logical(
        UnresolvedExpressionId,
        LogicalOperator,
        UnresolvedExpressionId,
    ),
    AugmentedAssignment(
        UnresolvedPlaceExpressionId,
        ArithmeticOperator,
        UnresolvedExpressionId,
    ),
    Assignment(UnresolvedAssigneeExpressionId, UnresolvedExpressionId),
    List(Vec<UnresolvedExpressionId>),
    Compound(HashMap<String, UnresolvedExpressionId>),
    Score(PlayerScore),
    Data(Box<Data>),
    Condition(bool, Box<MiddleExecuteIfSubcommand>),
    Command(Box<MiddleCommand>),
    Index(UnresolvedExpressionId, UnresolvedExpressionId),
    FieldAccess(UnresolvedExpressionId, String),
    AsCast(UnresolvedExpressionId, UnresolvedDataType),
    ToCast(
        Option<NotNan<f32>>,
        UnresolvedExpressionId,
        RuntimeStorageType,
    ),
    Tuple(Vec<UnresolvedExpressionId>),
    Call(UnresolvedExpressionId, Vec<UnresolvedExpressionId>),
    Value(HighValueId, Vec<UnresolvedDataType>),
    RegularStruct(
        HighRegularStructId,
        Vec<UnresolvedDataType>,
        HashMap<String, UnresolvedExpressionId>,
    ),
    TupleStruct(
        HighTupleStructId,
        Vec<UnresolvedDataType>,
        Vec<UnresolvedExpressionId>,
    ),
    If {
        condition: UnresolvedExpressionId,

        body: UnresolvedExpressionId,
        else_body: Option<UnresolvedExpressionId>,
    },
    Block(
        Vec<Idx<UnresolvedStatement>>,
        Option<UnresolvedExpressionId>,
    ),
    WhileLoop(UnresolvedExpressionId, UnresolvedExpressionId),
    Loop(UnresolvedExpressionId),
    ForLoop(
        bool,
        Idx<UnresolvedPattern>,
        UnresolvedExpressionId,
        UnresolvedExpressionId,
    ),
    ResourceLocation(Box<SupportsExpressionSigil<ResourceLocation>>),
    EntitySelector(Box<SupportsExpressionSigil<EntitySelector>>),
    Coordinates(Box<SupportsExpressionSigil<Coordinates>>),
    Return(UnresolvedExpressionId),
    // TODO ByteArray(Vec<i8>),
    // TODO IntegerArray(Vec<i32>),
    // TODO LongArray(Vec<i64>),
}

impl UnresolvedExpression {
    #[must_use]
    pub fn definitely_diverges(id: UnresolvedExpressionId, allocator: &LowAstAllocator) -> bool {
        match allocator.get_expression_value(id) {
            Self::Return(..) => true,

            Self::Block(statements, tail_expression) => {
                for statement in statements {
                    if UnresolvedStatement::definitely_diverges(*statement, allocator) {
                        return true;
                    }
                }

                tail_expression
                    .as_ref()
                    .is_some_and(|expression| Self::definitely_diverges(*expression, allocator))
            }

            Self::If {
                condition,
                body,
                else_body,
            } => {
                if Self::definitely_diverges(*condition, allocator) {
                    return true;
                }

                if let Some(else_body) = else_body {
                    Self::definitely_diverges(*body, allocator)
                        && Self::definitely_diverges(*else_body, allocator)
                } else {
                    false
                }
            }

            Self::Loop(body) => !Self::contains_break(*body, allocator),

            Self::Arithmetic(left, _, right)
            | Self::Comparison(left, _, right)
            | Self::Logical(left, _, right) => {
                Self::definitely_diverges(*left, allocator)
                    || Self::definitely_diverges(*right, allocator)
            }

            Self::AugmentedAssignment(_, _, right)
            | Self::Assignment(_, right)
            | Self::Index(_, right) => Self::definitely_diverges(*right, allocator),

            Self::Negate(expression)
            | Self::Invert(expression)
            | Self::AsCast(expression, _)
            | Self::ToCast(_, expression, _) => Self::definitely_diverges(*expression, allocator),
            Self::Call(callee, arguments) => {
                Self::definitely_diverges(*callee, allocator)
                    || arguments
                        .iter()
                        .any(|expression| Self::definitely_diverges(*expression, allocator))
            }
            Self::List(expressions)
            | Self::Tuple(expressions)
            | Self::TupleStruct(_, _, expressions) => expressions
                .iter()
                .any(|expression| Self::definitely_diverges(*expression, allocator)),
            Self::Compound(compound) | Self::RegularStruct(_, _, compound) => compound
                .values()
                .any(|expression| Self::definitely_diverges(*expression, allocator)),
            Self::WhileLoop(expression, _) => Self::definitely_diverges(*expression, allocator),
            Self::ForLoop(_, _, expression, _) => Self::definitely_diverges(*expression, allocator),

            _ => false,
        }
    }

    #[inline]
    #[must_use]
    pub fn contains_break(id: UnresolvedExpressionId, allocator: &LowAstAllocator) -> bool {
        matches!(
            Self::get_early_return_type(id, allocator),
            Some(EarlyReturnType::Break)
        )
    }

    #[must_use]
    pub fn get_early_return_type(
        id: UnresolvedExpressionId,
        allocator: &LowAstAllocator,
    ) -> Option<EarlyReturnType> {
        match allocator.get_expression_value(id) {
            Self::If {
                body, else_body, ..
            } => Self::get_early_return_type(*body, allocator).or_else(|| {
                else_body
                    .as_ref()
                    .and_then(|else_body| Self::get_early_return_type(*else_body, allocator))
            }),

            Self::Block(statements, tail_expression) => {
                for statement in statements {
                    if let Some(kind) =
                        UnresolvedStatement::get_early_return_type(*statement, allocator)
                    {
                        return Some(kind);
                    }
                }

                if let Some(tail_expression) = tail_expression
                    && let Some(kind) = Self::get_early_return_type(*tail_expression, allocator)
                {
                    return Some(kind);
                }

                None
            }

            Self::WhileLoop(_, body) => Self::get_early_return_type(*body, allocator),
            Self::Loop(body) => Self::get_early_return_type(*body, allocator),
            Self::ForLoop(_, _, _, body) => Self::get_early_return_type(*body, allocator),

            Self::Return(..) => Some(EarlyReturnType::Return),

            _ => None,
        }
    }

    #[inline]
    #[must_use]
    pub fn returns_early(id: UnresolvedExpressionId, allocator: &LowAstAllocator) -> bool {
        Self::get_early_return_type(id, allocator).is_some()
    }

    #[must_use]
    pub const fn can_be_referenced(&self) -> bool {
        matches!(self, Self::Value(..))
    }

    #[must_use]
    pub fn resolve(
        id: UnresolvedExpressionId,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ResolvedExpression {
        match allocator.get_expression_value(id) {
            Self::Negate(expression) => {
                let expression = Self::resolve(*expression, allocator, datapack, ctx);

                expression.negate(datapack, ctx).unwrap()
            }
            Self::Invert(expression) => {
                let expression = Self::resolve(*expression, allocator, datapack, ctx);

                expression.invert().unwrap()
            }
            Self::Reference(expression) => {
                let expression =
                    UnresolvedPlaceExpression::resolve(*expression, allocator, datapack, ctx);

                ResolvedExpression::Reference(Box::new(expression))
            }
            Self::Dereference(place) => {
                let place = UnresolvedPlaceExpression::resolve(*place, allocator, datapack, ctx);

                place
                    .resolve(datapack, ctx)
                    .dereference(datapack, ctx)
                    .unwrap()
            }
            Self::Arithmetic(left, operator, right) => {
                let left = Self::resolve(*left, allocator, datapack, ctx);
                let right = Self::resolve(*right, allocator, datapack, ctx);

                left.perform_arithmetic(datapack, ctx, *operator, right)
            }
            Self::Comparison(left, operator, right) => {
                let left = Self::resolve(*left, allocator, datapack, ctx);
                let right = Self::resolve(*right, allocator, datapack, ctx);

                left.perform_comparison(datapack, ctx, *operator, right)
            }
            Self::Logical(left, operator, right) => {
                let left = Self::resolve(*left, allocator, datapack, ctx);
                let right = Self::resolve(*right, allocator, datapack, ctx);

                left.perform_logical_operation(datapack, ctx, *operator, right)
            }
            Self::AugmentedAssignment(target, operator, value) => {
                let target = UnresolvedPlaceExpression::resolve(*target, allocator, datapack, ctx);
                let value = Self::resolve(*value, allocator, datapack, ctx);

                target.augmented_assign(datapack, ctx, *operator, value);

                ResolvedExpression::Unit
            }
            Self::Assignment(target, value) => {
                let target =
                    UnresolvedAssigneeExpression::resolve(*target, allocator, datapack, ctx);
                let value = Self::resolve(*value, allocator, datapack, ctx);

                target.assign(datapack, ctx, value);

                ResolvedExpression::Unit
            }
            Self::List(expressions) => ResolvedExpression::List(
                expressions
                    .iter()
                    .copied()
                    .map(|expression| Self::resolve(expression, allocator, datapack, ctx))
                    .collect::<Vec<_>>(),
            ),
            Self::Compound(compound) => ResolvedExpression::Compound(
                compound
                    .iter()
                    .map(|(key, value)| {
                        (key.clone(), Self::resolve(*value, allocator, datapack, ctx))
                    })
                    .collect::<BTreeMap<_, _>>(),
            ),
            Self::Score(score) => {
                let score = score.clone().compile(allocator, datapack, ctx);

                ResolvedExpression::Score(score)
            }
            Self::Data(data) => {
                let data = data.clone().compile(allocator, datapack, ctx);

                ResolvedExpression::Data(data)
            }
            Self::Condition(inverted, condition) => {
                let inverted = *inverted;

                let condition = condition.clone().compile(allocator, datapack, ctx);

                let result_score = datapack.get_unique_score();

                ctx.add_command(
                    datapack,
                    condition
                        .into_subcommand(inverted)
                        .store_success_score(result_score.score.clone()),
                );

                ResolvedExpression::Score(result_score)
            }
            Self::Command(command) => {
                let command = command.clone().compile(allocator, datapack, ctx);

                let result_score = datapack.get_unique_score();

                ctx.add_command(
                    datapack,
                    command.run().store_result_score(result_score.score.clone()),
                );

                ResolvedExpression::Score(result_score)
            }
            Self::Index(target, index) => {
                let target = Self::resolve(*target, allocator, datapack, ctx);
                let index = Self::resolve(*index, allocator, datapack, ctx);

                target.index(ctx, index).unwrap()
            }
            Self::FieldAccess(target, field) => {
                let target = Self::resolve(*target, allocator, datapack, ctx);

                target.access_field(FieldAccessType::Name, field).unwrap()
            }
            Self::AsCast(expression, data_type) => {
                let expression = Self::resolve(*expression, allocator, datapack, ctx);
                let data_type = data_type.clone().resolve(datapack).unwrap();

                expression.cast_to(data_type).unwrap()
            }
            Self::ToCast(scale, expression, runtime_storage_type) => {
                let expression = Self::resolve(*expression, allocator, datapack, ctx);

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

                        ResolvedExpression::Data(data)
                    }
                }
            }
            Self::Tuple(expressions) => ResolvedExpression::Tuple(
                expressions
                    .iter()
                    .copied()
                    .map(|expression| Self::resolve(expression, allocator, datapack, ctx))
                    .collect(),
            ),
            Self::RegularStruct(id, generic_types, field_expressions) => {
                let id = *id;

                let (module_path, visiblity, declaration) =
                    datapack.resolved_environment.get_regular_struct(id);

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
                        let data_type = allocator.get_expression_type(*expression).clone();

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
                        let expression = Self::resolve(*expression, allocator, datapack, ctx);

                        (name.clone(), expression)
                    })
                    .collect();

                ResolvedExpression::RegularStruct(id, field_expressions)
            }
            Self::TupleStruct(id, generic_types, field_expressions) => {
                let id = *id;

                let (module_path, visiblity, declaration) =
                    datapack.resolved_environment.get_tuple_struct(id);

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
                        let data_type = allocator.get_expression_type(*expression).clone();

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
                    .map(|expression| Self::resolve(expression, allocator, datapack, ctx))
                    .collect();

                ResolvedExpression::TupleStruct(id, field_expressions)
            }
            Self::Underscore => unreachable!(),
            Self::Boolean(value) => ResolvedExpression::Boolean(*value),
            Self::Byte(value) => ResolvedExpression::Byte(*value),
            Self::Short(value) => ResolvedExpression::Short(*value),
            Self::Integer(value) | Self::InferredInteger(value) => {
                ResolvedExpression::Integer(*value)
            }
            Self::Long(value) => ResolvedExpression::Long(*value),
            Self::Float(value) | Self::InferredFloat(value) => ResolvedExpression::Float(*value),
            Self::Double(value) => ResolvedExpression::Double(*value),
            Self::String(value) => ResolvedExpression::String(value.clone()),
            Self::Unit => ResolvedExpression::Unit,
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
                    ValueDeclarationKind::Function(..) => {
                        ResolvedExpression::Function(FunctionId(id.0))
                    }
                }
            }
            Self::Block(statements, tail_expression) => {
                for statement in statements {
                    UnresolvedStatement::compile_as_statement(*statement, allocator, datapack, ctx);
                }

                tail_expression.map_or(ResolvedExpression::Unit, |tail_expression| {
                    Self::resolve(tail_expression, allocator, datapack, ctx)
                })
            }
            Self::If {
                condition,
                body,
                else_body,
            } => {
                let output_data = datapack.get_unique_data();

                compile_if_internal(
                    allocator,
                    datapack,
                    ctx,
                    *condition,
                    *body,
                    *else_body,
                    Some(output_data.clone()),
                );

                ResolvedExpression::Data(output_data)
            }
            Self::Call(callee, arguments) => {
                let callee = Self::resolve(*callee, allocator, datapack, ctx);

                let arguments = arguments
                    .iter()
                    .copied()
                    .map(|argument| Self::resolve(argument, allocator, datapack, ctx))
                    .collect::<Vec<_>>();

                callee
                    .call(allocator, datapack, ctx, arguments, true)
                    .unwrap()
            }
            Self::WhileLoop(condition, body) => {
                let while_function_paths = datapack.get_unique_function_paths();
                let while_function_resource_location = ResourceLocation::new_namespace_paths(
                    datapack.current_namespace_name(),
                    while_function_paths.clone(),
                );

                let mut condition_ctx = ctx.create_child_ctx();
                let (inverted, condition) = Self::resolve(*condition, allocator, datapack, ctx)
                    .to_execute_condition(datapack, &mut condition_ctx)
                    .unwrap();

                let subcommand = if Self::returns_early(*body, allocator) {
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

                Self::compile_as_statement(*body, allocator, datapack, &mut while_body_ctx);

                while_body_ctx.extend_context(condition_ctx.clone());
                ctx.extend_context(condition_ctx);

                datapack.compile_and_add_to_function(&while_function_paths, &mut while_body_ctx);

                ResolvedExpression::Unit
            }
            Self::Loop(body) => {
                let loop_function_paths = datapack.get_unique_function_paths();
                let loop_function_resource_location = ResourceLocation::new_namespace_paths(
                    datapack.current_namespace_name(),
                    loop_function_paths.clone(),
                );

                let iteration_command = if Self::returns_early(*body, allocator) {
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

                Self::compile_as_statement(*body, allocator, datapack, &mut loop_body_ctx);

                loop_body_ctx.add_command(datapack, iteration_command.clone());
                ctx.add_command(datapack, iteration_command);

                datapack.compile_and_add_to_function(&loop_function_paths, &mut loop_body_ctx);

                ResolvedExpression::Unit
            }
            Self::ForLoop(is_reversed, pattern, iterable, body) => {
                let is_reversed = *is_reversed;

                let iterable_type = allocator.get_expression_type(*iterable).clone();

                let iterable_type = iterable_type.resolve(datapack).unwrap();

                let iterable_type = iterable_type.get_iterable_type().unwrap();

                let iterable = Self::resolve(*iterable, allocator, datapack, ctx);

                match iterable.try_into_iter(is_reversed) {
                    Ok(expressions) => {
                        for expression in expressions {
                            UnresolvedPattern::destructure(
                                *pattern,
                                allocator,
                                datapack,
                                ctx,
                                iterable_type.clone(),
                                expression,
                            );

                            Self::compile_as_statement(*body, allocator, datapack, ctx);
                        }
                    }
                    Err(iterable) => {
                        if iterable_type.equals(&ResolvedDataType::String) {
                            iterate_string(
                                allocator,
                                datapack,
                                ctx,
                                is_reversed,
                                *pattern,
                                iterable,
                                *body,
                            );
                        } else {
                            iterate_data(
                                allocator,
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

                ResolvedExpression::Unit
            }
            Self::ResourceLocation(resource_location) => {
                let resource_location = resource_location.clone().compile(allocator, datapack, ctx);

                ResolvedExpression::ResourceLocation(resource_location)
            }
            Self::EntitySelector(selector) => {
                let selector = selector.clone().compile(allocator, datapack, ctx);

                ResolvedExpression::EntitySelector(selector)
            }
            Self::Coordinates(coordinates) => {
                let coordinates = coordinates.clone().compile(allocator, datapack, ctx);

                ResolvedExpression::Coordinates(coordinates)
            }
            Self::Return(expression) => {
                let expression = Self::resolve(*expression, allocator, datapack, ctx);

                let target = datapack.function_return_targets.last().unwrap().clone();

                target.assign(datapack, ctx, expression);

                ctx.add_command(datapack, Command::Return(ReturnCommand::Fail));

                ResolvedExpression::Never
            }
        }
    }

    pub fn compile_as_statement(
        id: UnresolvedExpressionId,
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) {
        match allocator.get_expression_value(id) {
            Self::Assignment(target, value) => {
                let target =
                    UnresolvedAssigneeExpression::resolve(*target, allocator, datapack, ctx);

                let value = Self::resolve(*value, allocator, datapack, ctx);

                target.assign(datapack, ctx, value);
            }
            Self::AugmentedAssignment(target, operator, value) => {
                let operator = *operator;

                let target = UnresolvedPlaceExpression::resolve(*target, allocator, datapack, ctx);
                let value = Self::resolve(*value, allocator, datapack, ctx);

                target.augmented_assign(datapack, ctx, operator, value);
            }
            Self::List(list) => {
                for element in list {
                    Self::compile_as_statement(*element, allocator, datapack, ctx);
                }
            }
            Self::Compound(compound) => {
                for value in compound.values().copied() {
                    Self::compile_as_statement(value, allocator, datapack, ctx);
                }
            }
            Self::Condition(_, condition) => {
                let condition = condition.clone().compile(allocator, datapack, ctx);

                ctx.add_command(datapack, condition.into_subcommand(false));
            }
            Self::Command(command) => {
                let command = command.clone().compile(allocator, datapack, ctx);

                ctx.add_command(datapack, command);
            }
            Self::AsCast(expression, _) | Self::ToCast(_, expression, _) => {
                Self::compile_as_statement(*expression, allocator, datapack, ctx);
            }
            Self::Tuple(tuple) => {
                for element in tuple {
                    Self::compile_as_statement(*element, allocator, datapack, ctx);
                }
            }
            Self::RegularStruct(_, _, field_expressions) => {
                for field_expression in field_expressions.values().copied() {
                    Self::compile_as_statement(field_expression, allocator, datapack, ctx);
                }
            }
            Self::TupleStruct(_, _, field_expressions) => {
                for field_expression in field_expressions {
                    Self::compile_as_statement(*field_expression, allocator, datapack, ctx);
                }
            }
            Self::Block(statements, tail_expression) => {
                for statement in statements.iter().copied() {
                    UnresolvedStatement::compile_as_statement(statement, allocator, datapack, ctx);
                }

                if let Some(tail_expression) = tail_expression {
                    Self::compile_as_statement(*tail_expression, allocator, datapack, ctx);
                }
            }
            Self::If {
                condition,
                body,
                else_body,
            } => {
                compile_if_internal(
                    allocator, datapack, ctx, *condition, *body, *else_body, None,
                );
            }
            Self::Call(callee, arguments) => {
                let callee = Self::resolve(*callee, allocator, datapack, ctx);

                let arguments = arguments
                    .iter()
                    .copied()
                    .map(|argument| Self::resolve(argument, allocator, datapack, ctx))
                    .collect::<Vec<_>>();

                callee.call(allocator, datapack, ctx, arguments, false);
            }
            Self::WhileLoop(condition, body) => {
                let while_function_paths = datapack.get_unique_function_paths();
                let while_function_resource_location = ResourceLocation::new_namespace_paths(
                    datapack.current_namespace_name(),
                    while_function_paths.clone(),
                );

                let mut condition_ctx = ctx.create_child_ctx();
                let (inverted, condition) = Self::resolve(*condition, allocator, datapack, ctx)
                    .to_execute_condition(datapack, &mut condition_ctx)
                    .unwrap();

                let subcommand = if Self::returns_early(*body, allocator) {
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

                Self::compile_as_statement(*body, allocator, datapack, &mut while_body_ctx);

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

                let iteration_command = if Self::returns_early(*body, allocator) {
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

                Self::compile_as_statement(*body, allocator, datapack, &mut loop_body_ctx);

                loop_body_ctx.add_command(datapack, iteration_command.clone());
                ctx.add_command(datapack, iteration_command);

                datapack.compile_and_add_to_function(&loop_function_paths, &mut loop_body_ctx);
            }
            Self::ForLoop(is_reversed, pattern, iterable, body) => {
                let is_reversed = *is_reversed;

                let iterable_type = allocator
                    .get_expression_type(*iterable)
                    .clone()
                    .resolve(datapack)
                    .unwrap()
                    .get_iterable_type()
                    .unwrap();

                let iterable = Self::resolve(*iterable, allocator, datapack, ctx);

                match iterable.try_into_iter(is_reversed) {
                    Ok(expressions) => {
                        let pattern = *pattern;

                        for expression in expressions {
                            UnresolvedPattern::destructure(
                                pattern,
                                allocator,
                                datapack,
                                ctx,
                                iterable_type.clone(),
                                expression,
                            );

                            Self::compile_as_statement(*body, allocator, datapack, ctx);
                        }
                    }
                    Err(iterable) => {
                        if iterable_type.equals(&ResolvedDataType::String) {
                            iterate_string(
                                allocator,
                                datapack,
                                ctx,
                                is_reversed,
                                *pattern,
                                iterable,
                                *body,
                            );
                        } else {
                            iterate_data(
                                allocator,
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
                selector
                    .clone()
                    .compile_as_statement(allocator, datapack, ctx);
            }
            Self::Return(expression) => {
                let expression = Self::resolve(*expression, allocator, datapack, ctx);

                let target = datapack.function_return_targets.last().unwrap().clone();

                target.assign(datapack, ctx, expression);

                ctx.add_command(datapack, Command::Return(ReturnCommand::Fail));
            }
            Self::Data(data) => {
                let _ = data.clone().compile(allocator, datapack, ctx);
            }
            Self::Negate(expression) => {
                Self::compile_as_statement(*expression, allocator, datapack, ctx);
            }
            Self::Invert(expression) => {
                Self::compile_as_statement(*expression, allocator, datapack, ctx);
            }
            Self::Reference(_) | Self::Dereference(_) => {}
            Self::Arithmetic(left, _, right) => {
                Self::compile_as_statement(*left, allocator, datapack, ctx);
                Self::compile_as_statement(*right, allocator, datapack, ctx);
            }
            Self::Comparison(left, _, right) => {
                Self::compile_as_statement(*left, allocator, datapack, ctx);
                Self::compile_as_statement(*right, allocator, datapack, ctx);
            }
            Self::Logical(left, _, right) => {
                Self::compile_as_statement(*left, allocator, datapack, ctx);
                Self::compile_as_statement(*right, allocator, datapack, ctx);
            }
            Self::Score(score) => {
                let _ = score.clone().compile(allocator, datapack, ctx);
            }
            Self::Index(_, index) => {
                Self::compile_as_statement(*index, allocator, datapack, ctx);
            }
            Self::FieldAccess(_, _) => {}
            Self::ResourceLocation(resource_location) => {
                resource_location
                    .clone()
                    .compile_as_statement(allocator, datapack, ctx);
            }
            Self::Coordinates(coordinates) => {
                coordinates
                    .clone()
                    .compile_as_statement(allocator, datapack, ctx);
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
