use std::collections::BTreeMap;

use la_arena::Idx;
use minecraft_command_types::{
    command::{
        Command, ScoreValue,
        data::DataCommandModification,
        enums::score_operation_operator::ScoreOperationOperator,
        execute::{ExecuteIfSubcommand, ScoreComparison, ScoreComparisonOperator},
        r#return::ReturnCommand,
    },
    coordinate::Coordinates,
    entity_selector::EntitySelector,
    macroable::Macroable,
    nbt_path::{NbtPathNode, SNBTCompound},
    range::IntegerRange,
    resource_location::ResourceLocation,
    snbt::{SNBT, SNBTString},
};
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext,
    datapack::{
        CompiletimeFunction, CompiletimeFunctionKey, CompiletimeFunctionKeyRef, Datapack,
        RecursiveRuntimeFunction, RegularRuntimeFunction, RuntimeFunction,
    },
    field_access_type::FieldAccessType,
    low::data::GeneratedData,
    low::{
        data_type::DataType,
        environment::{
            r#type::r#struct::{RegularStructId, TupleStructId},
            value::function::{FunctionDeclaration, FunctionId},
        },
        expression::place::PlaceExpression,
    },
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator},
    parsed::semantic_analysis::RegularFunctionModifiers,
    player_score::GeneratedPlayerScore,
    runtime_storage::{RuntimeStorageTarget, RuntimeStorageType},
    typed::{
        arena::TypedAstArena,
        environment::r#type::HighGenericId,
        expression::{TypedExpression, TypedExpressionId},
        pattern::TypedPattern,
    },
};

pub mod assignee;
pub mod place;
pub mod text_component;

pub fn compile_shift_operation(
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    target: &GeneratedPlayerScore,
    amount: ScoreValue,
    operator: ScoreOperationOperator,
) {
    if amount <= 0 {
        return;
    }

    let constant_two = datapack.get_constant_score(2);

    for _ in 0..amount {
        ctx.add_command(
            datapack,
            target
                .score
                .clone()
                .operation(operator, constant_two.score.clone()),
        );
    }
}

#[must_use]
pub fn split_constants_list(list: Vec<Expression>) -> (Vec<SNBT>, Vec<(usize, Expression)>) {
    let mut constants = Vec::new();
    let mut non_constants = Vec::new();

    for (i, expression) in list.into_iter().enumerate() {
        match expression.try_into_snbt() {
            Ok(snbt) => {
                constants.push(snbt);
            }
            Err(expression) => {
                non_constants.push((i, expression));
                constants.push(SNBT::compound(SNBTCompound::new()));
            }
        }
    }

    (constants, non_constants)
}

#[must_use]
pub fn split_constants_compound(
    compound: BTreeMap<String, Expression>,
) -> (SNBTCompound, BTreeMap<String, Expression>) {
    let mut constants = SNBTCompound::new();
    let mut non_constants = BTreeMap::default();

    for (key, expression) in compound {
        match expression.try_into_snbt() {
            Ok(snbt) => {
                constants.insert(SNBTString(false, key), Macroable::Regular(snbt));
            }
            Err(expression) => {
                constants.insert(
                    SNBTString(false, key.clone()),
                    SNBT::macroable_compound(SNBTCompound::new()),
                );

                non_constants.insert(key, expression);
            }
        }
    }

    (constants, non_constants)
}

fn integer_range_from_comparison_operator(
    operator: ScoreComparisonOperator,
    value: i32,
) -> IntegerRange {
    match operator {
        ScoreComparisonOperator::LessThan => IntegerRange::new_max(value - 1),
        ScoreComparisonOperator::LessThanOrEqualTo => IntegerRange::new_max(value),
        ScoreComparisonOperator::EqualTo => IntegerRange::new_single(value),
        ScoreComparisonOperator::GreaterThan => IntegerRange::new_min(value + 1),
        ScoreComparisonOperator::GreaterThanOrEqualTo => IntegerRange::new_min(value),
    }
}

#[must_use]
fn compile_compiletime_function(
    arena: &TypedAstArena,
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    id: FunctionId,
    parameters: Vec<(Idx<TypedPattern>, DataType)>,
    arguments: Vec<Expression>,
    body: TypedExpressionId,
) -> Expression {
    if let Some(function) = datapack
        .cached_compiletime_functions
        .get(&CompiletimeFunctionKeyRef {
            id,
            arguments: &arguments,
        })
    {
        let resource_location = function.resource_location.clone();
        let return_value = function.return_value.clone();

        ctx.add_command(datapack, Command::Function(resource_location, None));

        return return_value;
    }

    let paths = datapack.get_unique_function_paths();

    let resource_location =
        ResourceLocation::new_namespace_paths(datapack.current_namespace_name(), paths.clone());

    let mut function_body_ctx = CompileContext::default();

    for ((pattern, data_type), argument) in parameters.into_iter().zip(arguments.iter().cloned()) {
        TypedPattern::destructure(
            pattern,
            arena,
            datapack,
            &mut function_body_ctx,
            data_type,
            argument,
        );
    }

    let return_value = TypedExpression::resolve(body, arena, datapack, &mut function_body_ctx);

    datapack.compile_and_add_to_function(&paths, &mut function_body_ctx);

    ctx.add_command(datapack, Command::Function(resource_location.clone(), None));

    datapack.cached_compiletime_functions.insert(
        CompiletimeFunctionKey { id, arguments },
        CompiletimeFunction {
            resource_location,
            return_value: return_value.clone(),
        },
    );

    return_value
}

#[must_use]
#[allow(clippy::too_many_arguments)]
fn compile_regular_runtime_function(
    arena: &TypedAstArena,
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    id: FunctionId,
    parameters: Vec<(Idx<TypedPattern>, DataType)>,
    arguments: Vec<RuntimeStorageTarget>,
    body: TypedExpressionId,
    return_runtime_storage_type: RuntimeStorageType,
) -> Expression {
    if let Some(function) = datapack.cached_runtime_functions.get(&id) {
        let RuntimeFunction::Regular(function) = function else {
            unreachable!();
        };

        let resource_location = function.resource_location.clone();
        let parameter_targets = function.parameter_targets.clone();
        let return_target = function.return_target.clone();

        for (parameter_target, argument) in parameter_targets.iter().cloned().zip(arguments.clone())
        {
            parameter_target.assign_target(datapack, ctx, argument);
        }

        ctx.add_command(datapack, Command::Function(resource_location, None));

        for (parameter_target, argument) in parameter_targets.into_iter().zip(arguments) {
            argument.assign_target(datapack, ctx, parameter_target);
        }

        return return_target.to_expression();
    }

    let paths = datapack.get_unique_function_paths();

    let resource_location =
        ResourceLocation::new_namespace_paths(datapack.current_namespace_name(), paths.clone());

    let mut parameter_targets = Vec::with_capacity(parameters.len());

    let mut function_body_ctx = CompileContext::default();

    for ((pattern, data_type), argument) in parameters.into_iter().zip(arguments.clone()) {
        let parameter_target = data_type.get_runtime_storage_type().instantiate(datapack);

        parameter_target
            .clone()
            .assign_target(datapack, ctx, argument);

        TypedPattern::destructure(
            pattern,
            arena,
            datapack,
            &mut function_body_ctx,
            data_type,
            parameter_target.clone().to_expression(),
        );

        parameter_targets.push(parameter_target);
    }

    let return_target = return_runtime_storage_type.instantiate(datapack);

    datapack.cached_runtime_functions.insert(
        id,
        RuntimeFunction::Regular(RegularRuntimeFunction {
            resource_location: resource_location.clone(),
            parameter_targets: parameter_targets.clone(),
            return_target: return_target.clone(),
        }),
    );

    datapack.function_return_targets.push(return_target);
    let result = TypedExpression::resolve(body, arena, datapack, &mut function_body_ctx);
    let return_target = datapack.function_return_targets.pop().unwrap();

    return_target
        .clone()
        .assign(datapack, &mut function_body_ctx, result);

    datapack.compile_and_add_to_function(&paths, &mut function_body_ctx);

    ctx.add_command(datapack, Command::Function(resource_location, None));

    for (parameter_target, argument) in parameter_targets.into_iter().zip(arguments) {
        argument.assign_target(datapack, ctx, parameter_target);
    }

    return_target.to_expression()
}

#[must_use]
fn compile_recursive_runtime_function(
    arena: &TypedAstArena,
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    id: FunctionId,
    parameters: Vec<(Idx<TypedPattern>, DataType)>,
    arguments: Vec<GeneratedData>,
    body: TypedExpressionId,
) -> Expression {
    if let Some(function) = datapack.cached_runtime_functions.get(&id) {
        let RuntimeFunction::Recursive(function) = function else {
            unreachable!();
        };

        let resource_location = function.resource_location.clone();
        let arguments_stack = function.arguments_stack.clone();
        let return_data = function.return_data.clone();
        let prefix_data = function.prefix_data.clone();

        ctx.add_command(
            datapack,
            arguments_stack.clone().append_value(SNBT::empty_list()),
        );

        let arguments_data = arguments_stack.index(-1);

        for argument_data in arguments {
            ctx.add_command(datapack, arguments_data.clone().append_from(argument_data));
        }

        ctx.add_command(
            datapack,
            prefix_data.clone().append_value(SNBT::empty_compound()),
        );

        let this_prefix_data = prefix_data.index(-1);

        ctx.add_command(datapack, Command::Function(resource_location, None));

        ctx.add_command(datapack, arguments_data.remove());

        ctx.add_command(datapack, this_prefix_data.remove());

        let temporary_return_data = datapack.get_unique_data();

        ctx.add_command(
            datapack,
            temporary_return_data.clone().set_from(return_data),
        );

        return Expression::Data(temporary_return_data);
    }

    let paths = datapack.get_unique_function_paths();

    let resource_location =
        ResourceLocation::new_namespace_paths(datapack.current_namespace_name(), paths.clone());

    let old_prefix_data = datapack.prefix_data.take();

    let arguments_stack = datapack.get_unique_data();
    let prefix_data = datapack.get_unique_data();
    let return_data = datapack.get_unique_data();

    datapack.prefix_data = old_prefix_data;

    ctx.add_command(
        datapack,
        arguments_stack.clone().append_value(SNBT::empty_list()),
    );

    let arguments_data = arguments_stack.clone().index(-1);

    for argument_data in arguments {
        ctx.add_command(datapack, arguments_data.clone().append_from(argument_data));
    }

    ctx.add_command(
        datapack,
        prefix_data.clone().append_value(SNBT::empty_compound()),
    );

    let this_prefix_data = prefix_data.clone().index(-1);

    let old_prefix_data = datapack.prefix_data.take();
    datapack.prefix_data = Some(this_prefix_data.clone());

    datapack.cached_runtime_functions.insert(
        id,
        RuntimeFunction::Recursive(Box::new(RecursiveRuntimeFunction {
            resource_location: resource_location.clone(),
            arguments_stack,
            return_data: return_data.clone(),
            prefix_data,
        })),
    );

    let mut function_body_ctx = CompileContext::default();

    for (index, (pattern, data_type)) in parameters.into_iter().enumerate() {
        assert!(matches!(data_type, DataType::Data(..)));

        let argument_data = arguments_data.clone().index(index as i32);

        TypedPattern::destructure(
            pattern,
            arena,
            datapack,
            &mut function_body_ctx,
            data_type,
            Expression::Data(argument_data),
        );
    }

    datapack
        .function_return_targets
        .push(RuntimeStorageTarget::Data(return_data.clone()));

    let result = TypedExpression::resolve(body, arena, datapack, &mut function_body_ctx);

    let return_target = datapack.function_return_targets.pop().unwrap();

    return_target.assign(datapack, &mut function_body_ctx, result);

    datapack.compile_and_add_to_function(&paths, &mut function_body_ctx);

    datapack.prefix_data = old_prefix_data;

    ctx.add_command(datapack, Command::Function(resource_location, None));

    ctx.add_command(datapack, arguments_data.remove());

    ctx.add_command(datapack, this_prefix_data.remove());

    let temporary_return_data = datapack.get_unique_data();

    ctx.add_command(
        datapack,
        temporary_return_data.clone().set_from(return_data),
    );

    Expression::Data(temporary_return_data)
}

#[must_use]
#[allow(clippy::too_many_arguments)]
fn compile_function(
    arena: &TypedAstArena,
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    id: FunctionId,
    modifiers: RegularFunctionModifiers,
    generic_ids: Vec<HighGenericId>,
    generic_types: Vec<DataType>,
    parameters: Vec<(Idx<TypedPattern>, DataType)>,
    arguments: Vec<Expression>,
    body: TypedExpressionId,
    return_runtime_storage_type: RuntimeStorageType,
) -> Expression {
    for (generic_id, generic_type) in generic_ids.into_iter().zip(generic_types) {
        datapack.declare_generic(generic_id, generic_type);
    }

    let RegularFunctionModifiers::Runtime { recursive } = modifiers else {
        return compile_compiletime_function(arena, datapack, ctx, id, parameters, arguments, body);
    };

    if recursive {
        let argument_datas = arguments
            .into_iter()
            .map(|argument| {
                let Expression::Data(data) = argument else {
                    unreachable!();
                };

                data
            })
            .collect();

        compile_recursive_runtime_function(
            arena,
            datapack,
            ctx,
            id,
            parameters,
            argument_datas,
            body,
        )
    } else {
        let argument_targets = arguments
            .into_iter()
            .map(|argument| argument.try_into().unwrap())
            .collect();

        compile_regular_runtime_function(
            arena,
            datapack,
            ctx,
            id,
            parameters,
            argument_targets,
            body,
            return_runtime_storage_type,
        )
    }
}

macro_rules! compute_int {
    ($l:expr, $r:expr, $op:expr, $t:ty) => {
        match $op {
            ArithmeticOperator::Add => $l.wrapping_add($r),
            ArithmeticOperator::Subtract => $l.wrapping_sub($r),
            ArithmeticOperator::Multiply => $l.wrapping_mul($r),
            ArithmeticOperator::FloorDivide => $l.wrapping_div($r),
            ArithmeticOperator::Modulo => $l % $r,
            ArithmeticOperator::And => $l & $r,
            ArithmeticOperator::Or => $l | $r,
            ArithmeticOperator::LeftShift => $l << $r,
            ArithmeticOperator::RightShift => $l >> $r,
        }
    };
}

macro_rules! compute_float {
    ($l:expr, $r:expr, $op:expr) => {
        NotNan::new(match $op {
            ArithmeticOperator::Add => $l + $r,
            ArithmeticOperator::Subtract => $l - $r,
            ArithmeticOperator::Multiply => $l * $r,
            ArithmeticOperator::FloorDivide => $l / $r,
            ArithmeticOperator::Modulo
            | ArithmeticOperator::And
            | ArithmeticOperator::Or
            | ArithmeticOperator::LeftShift
            | ArithmeticOperator::RightShift => {
                unreachable!("Floats do not support bitwise operations")
            }
        })
        .unwrap()
    };
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expression {
    Boolean(bool),

    Byte(i8),
    Short(i16),
    Integer(i32),
    Long(i64),

    Float(NotNan<f32>),
    Double(NotNan<f64>),

    String(String),

    List(Vec<Self>),
    Compound(BTreeMap<String, Self>),
    Tuple(Vec<Self>),

    RegularStruct(RegularStructId, BTreeMap<String, Self>),
    TupleStruct(TupleStructId, Vec<Self>),

    Unit,
    Never,

    ResourceLocation(ResourceLocation),
    EntitySelector(EntitySelector),
    Coordinates(Coordinates),
    Function(FunctionId),
    Reference(Box<PlaceExpression>),

    Score(GeneratedPlayerScore),
    Data(GeneratedData),
}

impl Expression {
    pub fn call(
        self,
        arena: &TypedAstArena,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        arguments: Vec<Self>,
        store_result: bool,
    ) -> Option<Self> {
        Some(match self {
            Self::ResourceLocation(resource_location) => {
                assert!(arguments.is_empty());

                if store_result {
                    let unique_score = datapack.get_unique_score();

                    ctx.add_command(
                        datapack,
                        Command::Function(resource_location, None)
                            .run()
                            .store_result_score(unique_score.score.clone()),
                    );

                    Self::Score(unique_score)
                } else {
                    ctx.add_command(datapack, Command::Function(resource_location, None));

                    return None;
                }
            }
            Self::Function(id) => {
                let (_, _, declaration) = datapack.get_function(id);

                match declaration {
                    FunctionDeclaration::Regular(declaration) => compile_function(
                        arena,
                        datapack,
                        ctx,
                        id,
                        declaration.modifiers,
                        declaration.generic_ids.clone(),
                        declaration.generic_types.clone(),
                        declaration.parameters.clone(),
                        arguments,
                        declaration.body,
                        declaration.return_type.get_runtime_storage_type(),
                    ),
                    FunctionDeclaration::Builtin(declaration) => {
                        declaration.kind.clone().call(datapack, ctx, arguments)
                    }
                }
            }
            _ => unreachable!("The expression '{:?}' is not callable", self),
        })
    }

    #[must_use]
    pub fn dereference(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> Option<Self> {
        Some(match self {
            Self::Reference(place) => place.resolve(datapack, ctx),

            Self::Score(score) => {
                let unique_score = datapack.get_unique_score();

                unique_score.clone().set_from(datapack, ctx, score);

                Self::Score(unique_score)
            }

            Self::Data(data) => {
                let unique_data = datapack.get_unique_data();

                ctx.add_command(datapack, unique_data.clone().set_from(data));

                Self::Data(unique_data)
            }

            _ => return None,
        })
    }

    #[must_use]
    pub fn dereference_place(self) -> Option<PlaceExpression> {
        Some(match self {
            Self::Reference(place) => *place,

            Self::Score(score) => PlaceExpression::Score(score),

            Self::Data(data) => PlaceExpression::Data(data),

            _ => return None,
        })
    }

    pub fn try_into_iter(self, reverse: bool) -> Result<Vec<Self>, Self> {
        let mut vec = match self {
            Self::List(items) => items,
            Self::String(string) => string
                .chars()
                .map(|char| Self::String(char.to_string()))
                .collect(),
            _ => return Err(self),
        };

        if reverse {
            vec.reverse();
        }

        Ok(vec)
    }

    #[must_use]
    pub fn can_into_snbt(&self) -> bool {
        match self {
            Self::RegularStruct(_, field_expressions) => {
                field_expressions.values().all(Self::can_into_snbt)
            }
            Self::List(list) | Self::Tuple(list) => list.iter().all(Self::can_into_snbt),
            Self::Compound(compound) => compound.values().all(Self::can_into_snbt),
            Self::Boolean(..)
            | Self::Byte(..)
            | Self::Short(..)
            | Self::Integer(..)
            | Self::Long(..)
            | Self::Float(..)
            | Self::Double(..)
            | Self::String(..)
            | Self::Unit
            | Self::Never => true,
            Self::TupleStruct(..)
            | Self::ResourceLocation(..)
            | Self::EntitySelector(..)
            | Self::Coordinates(..)
            | Self::Function(..)
            | Self::Score(..)
            | Self::Data(..)
            | Self::Reference(..) => false,
        }
    }

    pub fn try_into_snbt(self) -> Result<SNBT, Self> {
        Ok(match self {
            Self::RegularStruct(name, field_expressions) => {
                if !field_expressions.values().all(Self::can_into_snbt) {
                    return Err(Self::RegularStruct(name, field_expressions));
                }

                let mut compound = SNBTCompound::new();

                for (key, value) in field_expressions {
                    compound.insert(
                        SNBTString(false, key),
                        Macroable::Regular(value.try_into_snbt().unwrap()),
                    );
                }

                SNBT::compound(compound)
            }
            Self::TupleStruct(name, field_expressions) => {
                if !field_expressions.iter().all(Self::can_into_snbt) {
                    return Err(Self::TupleStruct(name, field_expressions));
                }

                let mut list = Vec::new();

                for field_expression in field_expressions {
                    list.push(Macroable::Regular(
                        field_expression.try_into_snbt().unwrap(),
                    ));
                }

                SNBT::list(list)
            }
            Self::Boolean(boolean) => SNBT::Byte(i8::from(boolean)),
            Self::Byte(byte) => SNBT::Byte(byte),
            Self::Short(short) => SNBT::Short(short),
            Self::Integer(integer) => SNBT::Integer(integer),
            Self::Long(long) => SNBT::Long(long),
            Self::Float(float) => SNBT::float(float),
            Self::Double(double) => SNBT::double(double),
            Self::String(string) => SNBT::snbt_string(SNBTString(false, string)),
            Self::List(list) => {
                if !list.iter().all(Self::can_into_snbt) {
                    return Err(Self::List(list));
                }

                let list = list
                    .into_iter()
                    .map(|element| element.try_into_snbt().unwrap())
                    .collect();

                SNBT::list(list)
            }
            Self::Compound(compound) => {
                if !compound.values().all(Self::can_into_snbt) {
                    return Err(Self::Compound(compound));
                }

                let compound = compound
                    .into_iter()
                    .map(|(key, value)| (SNBTString(false, key), value.try_into_snbt().unwrap()))
                    .collect();

                SNBT::compound(compound)
            }
            Self::Tuple(tuple) => {
                if !tuple.iter().all(Self::can_into_snbt) {
                    return Err(Self::Tuple(tuple));
                }

                let tuple = tuple
                    .into_iter()
                    .map(|element| element.try_into_snbt().unwrap())
                    .collect();

                SNBT::list(tuple)
            }
            Self::Unit => {
                let mut compound = SNBTCompound::new();

                compound.insert(
                    SNBTString(false, "__kelp_rs_unit__".to_string()),
                    Macroable::Regular(SNBT::Byte(1)),
                );

                SNBT::compound(compound)
            }
            Self::Never => {
                let mut compound = SNBTCompound::new();

                compound.insert(
                    SNBTString(false, "__kelp_rs_never__".to_string()),
                    Macroable::Regular(SNBT::Byte(1)),
                );

                SNBT::compound(compound)
            }
            _ => return Err(self),
        })
    }

    pub fn try_into_snbt_scale(self, scale: NotNan<f32>) -> Result<SNBT, Self> {
        Ok(match self {
            Self::Byte(byte) => {
                SNBT::float(NotNan::new(f32::from(byte) * scale.into_inner()).unwrap())
            }
            Self::Short(short) => {
                SNBT::float(NotNan::new(f32::from(short) * scale.into_inner()).unwrap())
            }
            Self::Integer(integer) => {
                SNBT::float(NotNan::new(integer as f32 * scale.into_inner()).unwrap())
            }
            Self::Long(long) => SNBT::float(NotNan::new(long as f32 * scale.into_inner()).unwrap()),
            Self::Float(float) => {
                SNBT::float(NotNan::new(float.into_inner() * scale.into_inner()).unwrap())
            }
            Self::Double(double) => SNBT::double(
                NotNan::new(f64::from(double.into_inner() as f32 * scale.into_inner())).unwrap(),
            ),
            _ => {
                return Err(self);
            }
        })
    }

    pub fn as_snbt_macros(self, ctx: &mut CompileContext) -> Macroable<SNBT> {
        match self {
            Self::Boolean(..)
            | Self::Byte(..)
            | Self::Short(..)
            | Self::Integer(..)
            | Self::Long(..)
            | Self::Float(..)
            | Self::Double(..)
            | Self::String(..)
            | Self::Unit => Macroable::Regular(self.try_into_snbt().unwrap()),

            Self::List(list) => Macroable::Regular(SNBT::list(
                list.into_iter()
                    .map(|element| element.as_snbt_macros(ctx))
                    .collect(),
            )),
            Self::Tuple(tuple) => Macroable::Regular(SNBT::list(
                tuple
                    .into_iter()
                    .map(|element| element.as_snbt_macros(ctx))
                    .collect(),
            )),
            Self::Compound(compound) => Macroable::Regular(SNBT::compound(
                compound
                    .into_iter()
                    .map(|(key, value)| (SNBTString(false, key), value.as_snbt_macros(ctx)))
                    .collect(),
            )),
            _ => ctx.get_macro_snbt(self),
        }
    }

    pub fn as_snbt_double(self, ctx: &mut CompileContext) -> Option<Macroable<NotNan<f64>>> {
        Some(match self {
            Self::Byte(..)
            | Self::Short(..)
            | Self::Integer(..)
            | Self::Long(..)
            | Self::Float(..)
            | Self::Double(..) => {
                Macroable::Regular(NotNan::new(self.try_as_f32().unwrap()).unwrap())
            }

            Self::Score(..) | Self::Data(..) => ctx.get_macro_snbt(self),

            _ => return None,
        })
    }

    #[must_use]
    pub fn try_as_f32(&self) -> Option<f64> {
        Some(match self {
            Self::Byte(v) => f64::from(*v),
            Self::Short(v) => f64::from(*v),
            Self::Integer(v) => f64::from(*v),
            Self::Long(v) => *v as f64,
            Self::Float(v) => f64::from(v.into_inner()),
            Self::Double(v) => v.into_inner(),

            _ => return None,
        })
    }

    #[must_use]
    pub fn as_score(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        force: bool,
    ) -> GeneratedPlayerScore {
        if !force && let Some(value) = self.try_as_i32(force) {
            return datapack.get_constant_score(value);
        }

        match self {
            Self::Score(player_score) if !force || player_score.is_generated => player_score,

            _ => {
                let unique_score = datapack.get_unique_score();

                self.assign_to_score(datapack, ctx, unique_score.clone());

                unique_score
            }
        }
    }

    #[must_use]
    pub fn to_unique_score(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> Self {
        match self {
            Self::Score(player_score) if player_score.is_generated => Self::Score(player_score),

            _ => {
                let unique_score = datapack.get_unique_score();
                self.assign_to_score(datapack, ctx, unique_score.clone());
                Self::Score(unique_score)
            }
        }
    }

    #[must_use]
    pub fn to_score_scale(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        scale: NotNan<f32>,
    ) -> Self {
        let unique_score = datapack.get_unique_score();
        self.assign_to_score_scale(datapack, ctx, unique_score.clone(), scale);
        Self::Score(unique_score)
    }

    pub fn assign_to_data(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        data: GeneratedData,
    ) {
        match self.try_into_snbt() {
            Ok(snbt) => {
                ctx.add_command(datapack, data.set_value(snbt));
            }
            Err(self_) => match self_ {
                Self::Score(score) => {
                    score.assign_to_data(datapack, ctx, data);
                }
                Self::Data(inner_data) => {
                    ctx.add_command(datapack, data.set_from(inner_data));
                }
                Self::List(list) => {
                    let (constants, non_constants) = split_constants_list(list);

                    ctx.add_command(datapack, data.clone().set_value(SNBT::list(constants)));

                    for (index, non_constant) in non_constants {
                        non_constant.assign_to_data(
                            datapack,
                            ctx,
                            data.clone().with_path_node(NbtPathNode::Index(Some(
                                SNBT::macroable_integer(index as i32),
                            ))),
                        );
                    }
                }
                Self::Compound(compound) => {
                    let (constants, non_constants) = split_constants_compound(compound);

                    ctx.add_command(datapack, data.clone().set_value(SNBT::compound(constants)));

                    for (key, non_constant) in non_constants {
                        non_constant.assign_to_data(
                            datapack,
                            ctx,
                            data.clone()
                                .with_path_node(NbtPathNode::named(SNBTString(false, key))),
                        );
                    }
                }
                Self::RegularStruct(_, field_expressions) => {
                    for (key, value) in field_expressions {
                        value.assign_to_data(
                            datapack,
                            ctx,
                            data.clone()
                                .with_path_node(NbtPathNode::Named(SNBTString(false, key), None)),
                        );
                    }
                }
                Self::TupleStruct(_, field_expressions) => {
                    for (field_index, field_expression) in field_expressions.into_iter().enumerate()
                    {
                        field_expression.assign_to_data(
                            datapack,
                            ctx,
                            data.clone().with_path_node(NbtPathNode::Index(Some(
                                SNBT::macroable_integer(field_index as i32),
                            ))),
                        );
                    }
                }
                Self::Tuple(expressions) => {
                    let (constants, non_constants) = split_constants_list(expressions);

                    ctx.add_command(datapack, data.clone().set_value(SNBT::list(constants)));

                    for (index, non_constant) in non_constants {
                        non_constant.assign_to_data(
                            datapack,
                            ctx,
                            data.clone().with_path_node(NbtPathNode::Index(Some(
                                SNBT::macroable_integer(index as i32),
                            ))),
                        );
                    }
                }
                Self::Unit | Self::Never => {}
                value => {
                    unreachable!("{:?}", value)
                }
            },
        }
    }

    pub fn assign_to_data_scale(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        data: GeneratedData,
        scale: NotNan<f32>,
    ) {
        match self.try_into_snbt_scale(scale) {
            Ok(snbt) => {
                ctx.add_command(datapack, data.set_value(snbt));
            }
            Err(self_) => match self_ {
                Self::Score(score) => {
                    score.assign_to_data_scale(datapack, ctx, data, scale);
                }
                Self::Data(inner_data) => {
                    ctx.add_command(datapack, data.set_from(inner_data));
                }
                Self::List(list) => {
                    let (constants, non_constants) = split_constants_list(list);

                    ctx.add_command(datapack, data.clone().set_value(SNBT::list(constants)));

                    for (index, non_constant) in non_constants {
                        non_constant.assign_to_data(
                            datapack,
                            ctx,
                            data.clone().with_path_node(NbtPathNode::Index(Some(
                                SNBT::macroable_integer(index as i32),
                            ))),
                        );
                    }
                }
                Self::Compound(compound) => {
                    let (constants, non_constants) = split_constants_compound(compound);

                    ctx.add_command(
                        datapack,
                        data.clone().set_value(SNBT::macroable_compound(constants)),
                    );

                    for (key, non_constant) in non_constants {
                        non_constant.assign_to_data_scale(
                            datapack,
                            ctx,
                            data.clone()
                                .with_path_node(NbtPathNode::named(SNBTString(false, key))),
                            scale,
                        );
                    }
                }
                Self::RegularStruct(_, fields) => {
                    for (key, value) in fields {
                        value.assign_to_data_scale(
                            datapack,
                            ctx,
                            data.clone()
                                .with_path_node(NbtPathNode::Named(SNBTString(false, key), None)),
                            scale,
                        );
                    }
                }
                Self::TupleStruct(_, field_expressions) => {
                    for (field_index, field_expression) in field_expressions.into_iter().enumerate()
                    {
                        field_expression.assign_to_data_scale(
                            datapack,
                            ctx,
                            data.clone().with_path_node(NbtPathNode::Index(Some(
                                SNBT::macroable_integer(field_index as i32),
                            ))),
                            scale,
                        );
                    }
                }
                Self::Tuple(expressions) => {
                    let (constants, non_constants) = split_constants_list(expressions);

                    ctx.add_command(datapack, data.clone().set_value(SNBT::list(constants)));

                    for (index, non_constant) in non_constants {
                        non_constant.assign_to_data_scale(
                            datapack,
                            ctx,
                            data.clone().with_path_node(NbtPathNode::Index(Some(
                                SNBT::macroable_integer(index as i32),
                            ))),
                            scale,
                        );
                    }
                }
                _ => {
                    unreachable!("Checked by ResolvedExpression::try_into_snbt")
                }
            },
        }
    }

    #[must_use]
    pub fn as_data(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        force: bool,
    ) -> GeneratedData {
        #[allow(clippy::single_match_else)]
        match self {
            Self::Data(data) if !force || data.target.is_generated => data,

            _ => {
                let unique_data = datapack.get_unique_data();

                self.assign_to_data(datapack, ctx, unique_data.clone());

                unique_data
            }
        }
    }

    #[must_use]
    pub fn to_data_scale(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        scale: NotNan<f32>,
    ) -> GeneratedData {
        let data = datapack.get_unique_data();

        self.assign_to_data_scale(datapack, ctx, data.clone(), scale);

        data
    }

    #[must_use]
    pub fn as_data_command_modification(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> DataCommandModification {
        match self.try_into_snbt() {
            Ok(snbt) => DataCommandModification::Value(Macroable::Regular(snbt)),
            Err(self_) => {
                let data = self_.as_data(datapack, ctx, false);

                DataCommandModification::From(data.target.target, Some(data.path))
            }
        }
    }

    #[must_use]
    pub fn try_as_i32(&self, force: bool) -> Option<i32> {
        Some(match self {
            Self::Byte(v) => i32::from(*v),
            Self::Short(v) => i32::from(*v),
            Self::Integer(v) => *v,
            Self::Long(v) => *v as i32,
            Self::Float(v) if force => v.into_inner() as i32,
            Self::Double(v) if force => v.into_inner() as i32,
            Self::String(v) if force => v.len() as i32,
            Self::List(v) if force => v.len() as i32,
            Self::Compound(compound) if force => compound.len() as i32,
            _ => return None,
        })
    }

    pub fn operate_on_score(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        score: GeneratedPlayerScore,
        operator: ArithmeticOperator,
    ) {
        if let Some(value) = self.try_as_i32(true) {
            return match operator {
                ArithmeticOperator::Add => {
                    ctx.add_command(datapack, score.score.add_value(value));
                }
                ArithmeticOperator::Subtract => {
                    ctx.add_command(datapack, score.score.remove(value));
                }
                ArithmeticOperator::And | ArithmeticOperator::Or => {
                    datapack
                        .get_constant_score(value)
                        .operate_on_score(datapack, ctx, score, operator);
                }
                ArithmeticOperator::LeftShift => {
                    compile_shift_operation(
                        datapack,
                        ctx,
                        &score,
                        value,
                        ScoreOperationOperator::Multiply,
                    );
                }
                ArithmeticOperator::RightShift => {
                    compile_shift_operation(
                        datapack,
                        ctx,
                        &score,
                        value,
                        ScoreOperationOperator::Divide,
                    );
                }
                _ => {
                    let constant_score = datapack.get_constant_score(value);

                    ctx.add_command(
                        datapack,
                        score
                            .score
                            .clone()
                            .operation(operator.try_into().unwrap(), constant_score.score),
                    );
                }
            };
        }

        if let Self::Score(self_) = self {
            self_.operate_on_score(datapack, ctx, score, operator);
        } else {
            let unique_score = datapack.get_unique_score();

            self.assign_to_score(datapack, ctx, unique_score.clone());

            unique_score.operate_on_score(datapack, ctx, score, operator);
        }
    }

    #[must_use]
    pub fn perform_arithmetic(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        operator: ArithmeticOperator,
        other: Self,
    ) -> Self {
        match (self, other) {
            (Self::Byte(left), Self::Byte(right)) => {
                Self::Byte(compute_int!(left, right, operator, i8))
            }
            (Self::Short(left), Self::Short(right)) => {
                Self::Short(compute_int!(left, right, operator, i16))
            }
            (Self::Integer(left), Self::Integer(right)) => {
                Self::Integer(compute_int!(left, right, operator, i32))
            }
            (Self::Long(left), Self::Long(right)) => {
                Self::Long(compute_int!(left, right, operator, i64))
            }
            (Self::Float(left), Self::Float(right)) => Self::Float(compute_float!(
                left.into_inner(),
                right.into_inner(),
                operator
            )),
            (Self::Double(left), Self::Double(right)) => Self::Double(compute_float!(
                left.into_inner(),
                right.into_inner(),
                operator
            )),

            (Self::Integer(l), Self::Byte(r)) => Self::Byte(compute_int!(l as i8, r, operator, i8)),
            (Self::Byte(l), Self::Integer(r)) => Self::Byte(compute_int!(l, r as i8, operator, i8)),

            (Self::Integer(l), Self::Short(r)) => {
                Self::Short(compute_int!(l as i16, r, operator, i16))
            }
            (Self::Short(l), Self::Integer(r)) => {
                Self::Short(compute_int!(l, r as i16, operator, i16))
            }

            (Self::Integer(l), Self::Long(r)) => {
                Self::Long(compute_int!(i64::from(l), r, operator, i64))
            }
            (Self::Long(l), Self::Integer(r)) => {
                Self::Long(compute_int!(l, i64::from(r), operator, i64))
            }

            (Self::Integer(l), Self::Float(r)) => {
                Self::Float(compute_float!(l as f32, r.into_inner(), operator))
            }
            (Self::Float(l), Self::Integer(r)) => {
                Self::Float(compute_float!(l, r as f32, operator))
            }

            (Self::Integer(l), Self::Double(r)) => {
                Self::Double(compute_float!(f64::from(l), r.into_inner(), operator))
            }
            (Self::Double(l), Self::Integer(r)) => {
                Self::Double(compute_float!(l, f64::from(r), operator))
            }

            (Self::Float(l), Self::Byte(r)) => {
                Self::Float(compute_float!(l, f32::from(r), operator))
            }
            (Self::Byte(l), Self::Float(r)) => {
                Self::Float(compute_float!(f32::from(l), r.into_inner(), operator))
            }

            (Self::Float(l), Self::Short(r)) => {
                Self::Float(compute_float!(l, f32::from(r), operator))
            }
            (Self::Short(l), Self::Float(r)) => {
                Self::Float(compute_float!(f32::from(l), r.into_inner(), operator))
            }

            (Self::Float(l), Self::Long(r)) => Self::Float(compute_float!(l, r as f32, operator)),
            (Self::Long(l), Self::Float(r)) => {
                Self::Float(compute_float!(l as f32, r.into_inner(), operator))
            }

            (Self::Float(l), Self::Double(r)) => Self::Float(compute_float!(
                l.into_inner(),
                r.into_inner() as f32,
                operator
            )),
            (Self::Double(l), Self::Float(r)) => {
                Self::Double(compute_float!(l, f64::from(r.into_inner()), operator))
            }

            (left_kind, right_kind) => {
                // TODO maybe better checking?
                // TODO assign into score

                let unique_score = datapack.get_unique_score();

                left_kind.assign_to_score(datapack, ctx, unique_score.clone());
                right_kind.operate_on_score(datapack, ctx, unique_score.clone(), operator);

                Self::Score(unique_score)
            }
        }
    }

    #[must_use]
    pub fn augmented_assign(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        operator: ArithmeticOperator,
        other: Self,
    ) -> Option<Self> {
        Some(match (self, other) {
            (Self::Score(score), other) => {
                other.operate_on_score(datapack, ctx, score, operator);

                return None;
            }

            (Self::Data(data), other) => {
                let unique_score = datapack.get_unique_score();

                data.clone()
                    .assign_to_score(datapack, ctx, unique_score.clone());
                other.operate_on_score(datapack, ctx, unique_score.clone(), operator);
                unique_score.assign_to_data(datapack, ctx, data);

                return None;
            }

            (left_kind, right_kind) => {
                left_kind.perform_arithmetic(datapack, ctx, operator, right_kind)
            }
        })
    }

    #[must_use]
    pub fn perform_comparison(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        operator: ComparisonOperator,
        other: Self,
    ) -> Self {
        match (self, other) {
            (Self::Byte(left), Self::Byte(right)) => Self::Boolean(match operator {
                ComparisonOperator::LessThan => left < right,
                ComparisonOperator::LessThanOrEqualTo => left <= right,
                ComparisonOperator::GreaterThan => left > right,
                ComparisonOperator::GreaterThanOrEqualTo => left >= right,
                ComparisonOperator::EqualTo => left == right,
                ComparisonOperator::NotEqualTo => left != right,
            }),
            (Self::Short(left), Self::Short(right)) => Self::Boolean(match operator {
                ComparisonOperator::LessThan => left < right,
                ComparisonOperator::LessThanOrEqualTo => left <= right,
                ComparisonOperator::GreaterThan => left > right,
                ComparisonOperator::GreaterThanOrEqualTo => left >= right,
                ComparisonOperator::EqualTo => left == right,
                ComparisonOperator::NotEqualTo => left != right,
            }),
            (Self::Integer(left), Self::Integer(right)) => Self::Boolean(match operator {
                ComparisonOperator::LessThan => left < right,
                ComparisonOperator::LessThanOrEqualTo => left <= right,
                ComparisonOperator::GreaterThan => left > right,
                ComparisonOperator::GreaterThanOrEqualTo => left >= right,
                ComparisonOperator::EqualTo => left == right,
                ComparisonOperator::NotEqualTo => left != right,
            }),
            (Self::Long(left), Self::Long(right)) => Self::Boolean(match operator {
                ComparisonOperator::LessThan => left < right,
                ComparisonOperator::LessThanOrEqualTo => left <= right,
                ComparisonOperator::GreaterThan => left > right,
                ComparisonOperator::GreaterThanOrEqualTo => left >= right,
                ComparisonOperator::EqualTo => left == right,
                ComparisonOperator::NotEqualTo => left != right,
            }),
            (Self::Float(left), Self::Float(right)) => Self::Boolean(match operator {
                ComparisonOperator::LessThan => left < right,
                ComparisonOperator::LessThanOrEqualTo => left <= right,
                ComparisonOperator::GreaterThan => left > right,
                ComparisonOperator::GreaterThanOrEqualTo => left >= right,
                ComparisonOperator::EqualTo => left == right,
                ComparisonOperator::NotEqualTo => left != right,
            }),
            (Self::Double(left), Self::Double(right)) => Self::Boolean(match operator {
                ComparisonOperator::LessThan => left < right,
                ComparisonOperator::LessThanOrEqualTo => left <= right,
                ComparisonOperator::GreaterThan => left > right,
                ComparisonOperator::GreaterThanOrEqualTo => left >= right,
                ComparisonOperator::EqualTo => left == right,
                ComparisonOperator::NotEqualTo => left != right,
            }),
            (left_kind @ Self::Data(..), right_kind)
                if operator == ComparisonOperator::EqualTo
                    || operator == ComparisonOperator::NotEqualTo =>
            {
                let unique_score = datapack.get_unique_score();

                let unique_data = datapack.get_unique_data();

                left_kind.assign_to_data(datapack, ctx, unique_data.clone());

                let data_command_modification =
                    right_kind.as_data_command_modification(datapack, ctx);

                ctx.add_command(
                    datapack,
                    unique_data
                        .set(data_command_modification)
                        .run()
                        .store_success_score(unique_score.score.clone()),
                );

                if operator == ComparisonOperator::EqualTo {
                    ctx.add_command(
                        datapack,
                        ExecuteIfSubcommand::Score(
                            unique_score.score.clone(),
                            ScoreComparison::Range(IntegerRange::new_single(0)),
                            None,
                        )
                        .into_subcommand(operator.should_execute_if_be_inverted())
                        .store_result_score(unique_score.score.clone()),
                    );
                }

                Self::Score(unique_score)
            }
            (self_ @ Self::Data(..), other) | (other, self_ @ Self::Data(..)) => {
                let score = self_.as_score(datapack, ctx, false);

                let other_score = other.as_score(datapack, ctx, false);

                let result_score = datapack.get_unique_score();

                ctx.add_command(
                    datapack,
                    ExecuteIfSubcommand::Score(
                        score.score,
                        ScoreComparison::Score(
                            operator.into_score_comparison_operator(),
                            other_score.score,
                        ),
                        None,
                    )
                    .into_subcommand(operator.should_execute_if_be_inverted())
                    .store_result_score(result_score.score.clone()),
                );

                Self::Score(result_score)
            }
            (Self::Score(score), other) => {
                if let Some(value) = other.try_as_i32(false) {
                    let result_score = datapack.get_unique_score();

                    ctx.add_command(
                        datapack,
                        ExecuteIfSubcommand::Score(
                            score.score,
                            ScoreComparison::Range(integer_range_from_comparison_operator(
                                operator.into_score_comparison_operator(),
                                value,
                            )),
                            None,
                        )
                        .into_subcommand(operator.should_execute_if_be_inverted())
                        .store_result_score(result_score.score.clone()),
                    );

                    return Self::Score(result_score);
                }

                let right_score = other.as_score(datapack, ctx, false);

                let result_score = datapack.get_unique_score();

                ctx.add_command(
                    datapack,
                    ExecuteIfSubcommand::Score(
                        score.score,
                        ScoreComparison::Score(
                            operator.into_score_comparison_operator(),
                            right_score.score,
                        ),
                        None,
                    )
                    .into_subcommand(operator.should_execute_if_be_inverted())
                    .store_result_score(result_score.score.clone()),
                );

                Self::Score(result_score)
            }
            (left_kind, Self::Score(right_score)) => {
                let left_score = left_kind.as_score(datapack, ctx, false);

                let result_score = datapack.get_unique_score();

                ctx.add_command(
                    datapack,
                    ExecuteIfSubcommand::Score(
                        right_score.score,
                        ScoreComparison::Score(
                            operator.into_score_comparison_operator(),
                            left_score.score,
                        ),
                        None,
                    )
                    .into_subcommand(operator.should_execute_if_be_inverted())
                    .store_result_score(result_score.score.clone()),
                );

                Self::Score(result_score)
            }
            (left, right) => unreachable!("{:?} {:?}", left, right),
        }
    }

    #[must_use]
    pub fn cast_to(self, data_type: DataType) -> Option<Self> {
        macro_rules! cast_match {
            (
                $self_expr:expr, $data_type_expr:expr;
                identity {
                    $( $variant:ident $( ( $($variant_args:tt)* ) )? <=> $type:ident $( ( $($type_args:tt)* ) )? ),* $(,)?
                }
                casts {
                    $(
                        $from_variant:ident($val:ident) => {
                            $( $to_type:ident => $to_variant:ident($cast_expr:expr) ),* $(,)?
                        }
                    ),* $(,)?
                }
            ) => {
                match ($self_expr, $data_type_expr) {
                    $(
                        (self_ @ Self::$variant $( ( $($variant_args)* ) )?, DataType::$type $( ( $($type_args)* ) )?) => Some(self_),
                    )*

                    $(
                        $(
                            (Self::$from_variant($val), DataType::$to_type) => Some(Self::$to_variant($cast_expr)),
                        )*
                    )*

                    _ => None,
                }
            };
        }

        cast_match!(self, data_type;
            identity {
                Boolean(..) <=> Boolean,
                Byte(..) <=> Byte,
                Short(..) <=> Short,
                Integer(..) <=> Integer,
                Long(..) <=> Long,
                Float(..) <=> Float,
                Double(..) <=> Double,
                String(..) <=> String,

                List(..) <=> List(..),
                Compound(..) <=> Compound(..),
                Tuple(..) <=> Tuple(..),
                RegularStruct(..) <=> Struct(..),
                TupleStruct(..) <=> Struct(..),

                Unit <=> Unit,
                Never <=> Never,

                ResourceLocation(..) <=> ResourceLocation,
                EntitySelector(..) <=> EntitySelector,
                Coordinates(..) <=> Coordinates,
                Function(..) <=> Function(..),

                Score(..) <=> Score(..),
                Data(..) <=> Data(..),
                Reference(..) <=> Reference(..),
            }
            casts {
                Byte(value) => {
                    Short => Short(i16::from(value)),
                    Integer => Integer(i32::from(value)),
                    Long => Long(i64::from(value)),
                    Float => Float(NotNan::new(f32::from(value)).unwrap()),
                    Double => Double(NotNan::new(f64::from(value)).unwrap()),
                },
                Short(value) => {
                    Byte => Byte(value as i8),
                    Integer => Integer(i32::from(value)),
                    Long => Long(i64::from(value)),
                    Float => Float(NotNan::new(f32::from(value)).unwrap()),
                    Double => Double(NotNan::new(f64::from(value)).unwrap()),
                },
                Integer(value) => {
                    Byte => Byte(value as i8),
                    Short => Short(value as i16),
                    Long => Long(i64::from(value)),
                    Float => Float(NotNan::new(value as f32).unwrap()),
                    Double => Double(NotNan::new(f64::from(value)).unwrap()),
                },
                Long(value) => {
                    Byte => Byte(value as i8),
                    Short => Short(value as i16),
                    Integer => Integer(value as i32),
                    Float => Float(NotNan::new(value as f32).unwrap()),
                    Double => Double(NotNan::new(value as f64).unwrap()),
                },
                Float(value) => {
                    Byte => Byte(value.into_inner() as i8),
                    Short => Short(value.into_inner() as i16),
                    Integer => Integer(value.into_inner() as i32),
                    Long => Long(value.into_inner() as i64),
                    Double => Double(value.into()),
                },
                Double(value) => {
                    Byte => Byte(value.into_inner() as i8),
                    Short => Short(value.into_inner() as i16),
                    Integer => Integer(value.into_inner() as i32),
                    Long => Long(value.into_inner() as i64),
                    Float => Float(unsafe { NotNan::new_unchecked(value.into_inner() as f32) }),
                }
            }
        )
    }

    pub fn to_execute_condition(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> Option<(bool, ExecuteIfSubcommand)> {
        if let Some(value) = self.try_as_i32(true) {
            let unique_score = datapack.get_unique_score();

            ctx.add_command(datapack, unique_score.score.clone().set_value(value));

            return Some(unique_score.to_execute_condition());
        }

        Some(match self {
            Self::Boolean(value) => (
                value,
                ExecuteIfSubcommand::Score(
                    datapack.get_constant_score(1).score,
                    ScoreComparison::Range(IntegerRange::new_single(0)),
                    None,
                ),
            ),
            Self::Score(score) => score.to_execute_condition(),
            Self::Data(..) => {
                let unique_score = datapack.get_unique_score();

                self.assign_to_score(datapack, ctx, unique_score.clone());

                unique_score.to_execute_condition()
            }
            _ => return None,
        })
    }

    pub fn assign_to_score(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        score: GeneratedPlayerScore,
    ) {
        match self {
            Self::Boolean(value) => {
                ctx.add_command(datapack, score.score.set_value(ScoreValue::from(value)));
            }
            Self::Byte(value) => {
                ctx.add_command(datapack, score.score.set_value(ScoreValue::from(value)));
            }
            Self::Short(value) => {
                ctx.add_command(datapack, score.score.set_value(ScoreValue::from(value)));
            }
            Self::Integer(value) => {
                ctx.add_command(datapack, score.score.set_value(value));
            }
            Self::Long(value) => {
                ctx.add_command(datapack, score.score.set_value(value as ScoreValue));
            }
            Self::Float(value) => {
                ctx.add_command(
                    datapack,
                    score.score.set_value(value.into_inner() as ScoreValue),
                );
            }
            Self::Double(value) => {
                ctx.add_command(
                    datapack,
                    score.score.set_value(value.into_inner() as ScoreValue),
                );
            }
            Self::String(value) => {
                ctx.add_command(datapack, score.score.set_value(value.len() as ScoreValue));
            }
            Self::List(value) => {
                ctx.add_command(datapack, score.score.set_value(value.len() as ScoreValue));
            }
            Self::Compound(value) => {
                ctx.add_command(datapack, score.score.set_value(value.len() as ScoreValue));
            }
            Self::Score(source) => {
                score.set_from(datapack, ctx, source);
            }
            Self::Data(data) => {
                data.assign_to_score(datapack, ctx, score);
            }
            Self::Unit | Self::Never => {}
            Self::Tuple(..)
            | Self::RegularStruct(..)
            | Self::TupleStruct(..)
            | Self::ResourceLocation(..)
            | Self::EntitySelector(..)
            | Self::Coordinates(..)
            | Self::Function(..)
            | Self::Reference(..) => {
                unreachable!("The expression '{:?}' cannot be assigned to a score", self)
            }
        }
    }

    pub fn assign_to_score_scale(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        score: GeneratedPlayerScore,
        scale: NotNan<f32>,
    ) {
        match self {
            Self::Byte(value) => {
                ctx.add_command(
                    datapack,
                    score
                        .score
                        .set_value((f32::from(value) * scale.into_inner()) as ScoreValue),
                );
            }
            Self::Short(value) => {
                ctx.add_command(
                    datapack,
                    score
                        .score
                        .set_value((f32::from(value) * scale.into_inner()) as ScoreValue),
                );
            }
            Self::Integer(value) => {
                ctx.add_command(
                    datapack,
                    score
                        .score
                        .set_value((value as f32 * scale.into_inner()) as ScoreValue),
                );
            }
            Self::Long(value) => {
                ctx.add_command(
                    datapack,
                    score
                        .score
                        .set_value((value as f32 * scale.into_inner()) as ScoreValue),
                );
            }
            Self::Float(value) => {
                ctx.add_command(
                    datapack,
                    score
                        .score
                        .set_value((value.into_inner() * scale.into_inner()) as ScoreValue),
                );
            }
            Self::Double(value) => {
                ctx.add_command(
                    datapack,
                    score.score.set_value(
                        (value.into_inner() * f64::from(scale.into_inner())) as ScoreValue,
                    ),
                );
            }
            Self::Score(source) => {
                source.assign_to_score_scale(datapack, ctx, score, scale);
            }
            Self::Data(data) => {
                ctx.add_command(datapack, data.get().run().store_result_score(score.score));
            }
            _ => {
                unreachable!("{:?}", self)
            }
        }
    }

    #[must_use]
    pub fn invert(self) -> Option<Self> {
        Some(match self {
            Self::Boolean(value) => Self::Boolean(!value),

            _ => return None,
        })
    }

    #[must_use]
    pub fn negate(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> Option<Self> {
        Some(match self {
            Self::Byte(value) => Self::Byte(-value),
            Self::Short(value) => Self::Short(-value),
            Self::Integer(value) => Self::Integer(-value),
            Self::Long(value) => Self::Long(-value),
            Self::Float(value) => Self::Float(-value),
            Self::Double(value) => Self::Double(-value),
            Self::Score(..) => {
                let unique_score = datapack.get_unique_score();

                self.assign_to_score(datapack, ctx, unique_score.clone());

                let constant_score = datapack.get_constant_score(-1);

                ctx.add_command(
                    datapack,
                    unique_score.score.clone().multiply(constant_score.score),
                );

                Self::Score(unique_score)
            }
            Self::Data(..) => {
                let unique_score = self.clone().as_score(datapack, ctx, true);

                self.assign_to_score(datapack, ctx, unique_score.clone());

                let constant_score = datapack.get_constant_score(-1);

                ctx.add_command(
                    datapack,
                    unique_score.score.clone().multiply(constant_score.score),
                );

                Self::Score(unique_score)
            }
            _ => return None,
        })
    }

    #[must_use]
    pub fn perform_logical_operation(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        operator: LogicalOperator,
        other: Self,
    ) -> Self {
        match operator {
            LogicalOperator::And => {
                let unique_score = datapack.get_unique_score();

                let (self_inverted, self_condition) =
                    self.to_execute_condition(datapack, ctx).unwrap();
                let (right_inverted, right_condition) =
                    other.to_execute_condition(datapack, ctx).unwrap();

                // 2.
                ctx.add_command(datapack, unique_score.score.clone().set_value(0));

                ctx.add_command(
                    datapack,
                    unique_score
                        .score
                        .clone()
                        .set_value(1)
                        .run()
                        .conditionally(right_inverted, right_condition)
                        .conditionally(self_inverted, self_condition),
                );

                Self::Score(unique_score)
            }
            LogicalOperator::Or => {
                let unique_function_paths = datapack.get_unique_function_paths();

                let unique_score = datapack.get_unique_score();
                ctx.add_command(datapack, unique_score.score.clone().set_value(0));
                ctx.add_command(
                    datapack,
                    Command::Function(
                        ResourceLocation::new_namespace_paths(
                            datapack.current_namespace_name(),
                            unique_function_paths.clone(),
                        ),
                        None,
                    )
                    .run()
                    .store_success_score(unique_score.score.clone()),
                );

                let mut function_ctx = CompileContext::default();

                let return_1 = Command::Return(ReturnCommand::Value(1)).run();

                let (self_inverted, left_condition) = self
                    .to_execute_condition(datapack, &mut function_ctx)
                    .unwrap();

                function_ctx.add_command(
                    datapack,
                    left_condition
                        .then(return_1.clone())
                        .into_subcommand(self_inverted),
                );
                let (other_inverted, other_condition) = other
                    .to_execute_condition(datapack, &mut function_ctx)
                    .unwrap();

                function_ctx.add_command(
                    datapack,
                    other_condition
                        .then(return_1)
                        .into_subcommand(other_inverted),
                );

                function_ctx.add_command(datapack, ReturnCommand::Fail);

                let function_commands = function_ctx.compile();

                datapack
                    .get_function_mut(&unique_function_paths)
                    .add_commands(function_commands);

                Self::Score(unique_score)
            }
        }
    }
}

impl Expression {
    #[must_use]
    pub fn set_field(
        mut self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        access_type: FieldAccessType,
        field: String,
        value: Self,
    ) -> Option<Self> {
        match self {
            Self::Compound(ref mut compound) => {
                compound.insert(field, value);
            }
            Self::Tuple(ref mut expressions) => {
                let index = field.parse::<usize>().unwrap();

                expressions[index] = value;
            }
            Self::TupleStruct(_, ref mut field_expressions) => {
                let index = field.parse::<usize>().unwrap();

                field_expressions[index] = value;
            }
            Self::RegularStruct(_, ref mut field_expressions) => {
                field_expressions.insert(field, value);
            }
            Self::Data(data) => {
                let node = access_type.into_nbt_path_node(field);

                value.assign_to_data(datapack, ctx, data.with_path_node(node));

                return None;
            }

            _ => unreachable!("{:?}", self),
        }

        Some(self)
    }

    #[must_use]
    pub fn access_field(self, access_type: FieldAccessType, field: &str) -> Option<Self> {
        match self {
            Self::Compound(mut compound) => compound.remove(field),
            Self::Tuple(mut expressions) => {
                let index = field.parse::<usize>().unwrap();

                if expressions.is_empty() {
                    None
                } else {
                    Some(expressions.remove(index))
                }
            }
            Self::TupleStruct(_, mut field_expressions) => {
                let index = field.parse::<usize>().unwrap();

                if field_expressions.is_empty() {
                    None
                } else {
                    Some(field_expressions.remove(index))
                }
            }
            Self::RegularStruct(_, mut field_expressions) => field_expressions.remove(field),
            Self::Data(data) => {
                let node = access_type.into_nbt_path_node(field.to_owned());

                Some(Self::Data(data.with_path_node(node)))
            }

            _ => unreachable!("{:?}", self),
        }
    }
}

impl Expression {
    #[must_use]
    pub fn set_index(
        mut self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        index: Self,
        value: Self,
    ) -> Option<Self> {
        match self {
            Self::List(ref mut items) => {
                let Self::Integer(index) = index else {
                    unreachable!();
                };

                items[index as usize] = value;
            }

            Self::Data(data) => {
                let index = index.as_snbt_macros(ctx);

                let node = NbtPathNode::Index(Some(index));

                value.assign_to_data(datapack, ctx, data.with_path_node(node));

                return None;
            }

            _ => unreachable!("{:?}", self),
        }

        Some(self)
    }

    #[must_use]
    pub fn index(self, ctx: &mut CompileContext, index: Self) -> Option<Self> {
        match self {
            Self::List(mut items) => {
                let Self::Integer(index) = index else {
                    unreachable!();
                };

                if items.is_empty() {
                    None
                } else {
                    Some(items.remove(index as usize))
                }
            }
            Self::Data(data) => {
                let index = index.as_snbt_macros(ctx);

                let node = NbtPathNode::Index(Some(index));

                Some(Self::Data(data.with_path_node(node)))
            }

            _ => None,
        }
    }
}
