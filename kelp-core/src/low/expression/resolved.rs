use std::{collections::HashMap, fmt::Write, hash::BuildHasher};

use minecraft_command_types::{
    command::{
        Command,
        data::{DataCommand, DataCommandModification, DataCommandModificationMode, DataTarget},
        enums::{
            numeric_snbt_type::NumericSNBTType, score_operation_operator::ScoreOperationOperator,
            store_type::StoreType,
        },
        execute::{
            ExecuteIfSubcommand, ExecuteStoreSubcommand, ExecuteSubcommand, ScoreComparison,
            ScoreComparisonOperator,
        },
        r#return::ReturnCommand,
        scoreboard::{PlayersScoreboardCommand, ScoreboardCommand},
    },
    entity_selector::EntitySelector,
    macroable::Macroable,
    nbt_path::{NbtPath, NbtPathNode, SNBTCompound},
    range::IntegerRange,
    resource_location::ResourceLocation,
    snbt::{SNBT, SNBTString},
};
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext,
    data::GeneratedDataTarget,
    datapack::Datapack,
    low::{
        data_type::DataType,
        environment::{
            Environment,
            r#type::r#struct::{StructDeclaration, StructStructId, TupleStructId},
            value::function::FunctionId,
        },
        expression::utils::push_scoreboard_players,
    },
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator},
    place::Place,
    player_score::GeneratedPlayerScore,
    trait_ext::CollectOptionAllIterExt,
};

pub fn compile_shift_operation(
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    target: &GeneratedPlayerScore,
    amount: i32,
    operator: ScoreOperationOperator,
) {
    if amount <= 0 {
        return;
    }

    let constant_two = datapack.get_constant_score(2);

    for _ in 0..amount {
        ctx.add_command(
            datapack,
            target.clone().operation(operator, constant_two.clone()),
        );
    }
}

#[must_use]
pub fn split_constants_list(
    list: Vec<ResolvedExpression>,
) -> (Vec<SNBT>, Vec<(usize, ResolvedExpression)>) {
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
pub fn split_constants_compound<S: BuildHasher>(
    compound: HashMap<SNBTString, ResolvedExpression, S>,
) -> (SNBTCompound, HashMap<SNBTString, ResolvedExpression>) {
    let mut constants = SNBTCompound::new();
    let mut non_constants = HashMap::default();

    for (key, expression) in compound {
        match expression.try_into_snbt() {
            Ok(snbt) => {
                constants.insert(key, Macroable::Regular(snbt));
            }
            Err(expression) => {
                constants.insert(
                    key.clone(),
                    Macroable::Regular(SNBT::compound(SNBTCompound::new())),
                );
                non_constants.insert(key, expression);
            }
        }
    }

    (constants, non_constants)
}

fn integer_range_from_comparison_operator(
    operator: &ScoreComparisonOperator,
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

fn compile_function(
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    original_id: FunctionId,
    arguments: &[ResolvedExpression],
) -> ResolvedExpression {
    let (_, _, original_declaration) = datapack.get_function_declaration(original_id);

    let paths = datapack.get_unique_function_paths();

    let parameters = original_declaration.parameters.clone();

    let original_body = original_declaration.body.clone().unwrap();

    let return_runtime_storage_type = original_declaration.return_type.get_runtime_storage_type();

    let mut function_body_ctx = CompileContext::default();

    for ((pattern, data_type), argument) in parameters.into_iter().zip(arguments.iter().cloned()) {
        let Some(pattern) = pattern else {
            continue;
        };

        let Some(data_type) = data_type else {
            continue;
        };

        pattern.destructure(datapack, &mut function_body_ctx, data_type, argument);
    }

    let return_target = return_runtime_storage_type.instantiate(datapack);

    datapack.function_return_targets.push(return_target);
    let result = original_body.kind.resolve(datapack, &mut function_body_ctx);
    let return_target = datapack.function_return_targets.pop().unwrap();
    return_target.clone().assign(datapack, ctx, result);

    datapack.compile_and_add_to_function(&paths, &mut function_body_ctx);

    ctx.add_command(
        datapack,
        Command::Function(
            ResourceLocation::new_namespace_paths(datapack.current_namespace_name(), paths),
            None,
        ),
    );

    return_target.to_expression()
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
            ArithmeticOperator::Swap => unreachable!(),
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
            ArithmeticOperator::Swap => unreachable!(),
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

fn format_generics(output: &mut String, generics: &[DataType], environment: &Environment) {
    if generics.is_empty() {
        return;
    }

    output.push('<');

    for (i, data_type) in generics.iter().enumerate() {
        if i != 0 {
            output.push_str(", ");
        }

        let _ = write!(output, "{}", data_type.display(environment));
    }

    output.push('>');
}

#[derive(Debug, Clone)]
pub enum ResolvedExpression {
    Boolean(bool),
    Byte(i8),
    Short(i16),
    Integer(i32),
    Long(i64),
    Float(NotNan<f32>),
    Double(NotNan<f64>),
    String(SNBTString),
    List(Vec<Self>),
    Compound(HashMap<SNBTString, Self>),
    Tuple(Vec<Self>),
    StructStruct(StructStructId, HashMap<String, Self>),
    TupleStruct(TupleStructId, Vec<Self>),
    Unit,
    Never,
    ResourceLocation(ResourceLocation),
    EntitySelector(EntitySelector),
    Function(FunctionId),

    PlayerScore(GeneratedPlayerScore),
    Data(Box<(GeneratedDataTarget, NbtPath)>),
    Condition(bool, Box<ExecuteIfSubcommand>),
}

impl ResolvedExpression {
    pub fn compile_as_statement(self, datapack: &mut Datapack, ctx: &mut CompileContext) {
        match self {
            Self::List(list) => {
                for element in list {
                    element.compile_as_statement(datapack, ctx);
                }
            }
            Self::Compound(compound) => {
                for value in compound.into_values() {
                    value.compile_as_statement(datapack, ctx);
                }
            }
            Self::Tuple(tuple) => {
                for element in tuple {
                    element.compile_as_statement(datapack, ctx);
                }
            }
            Self::StructStruct(_, field_expressions) => {
                for value in field_expressions.into_values() {
                    value.compile_as_statement(datapack, ctx);
                }
            }
            Self::TupleStruct(_, field_expressions) => {
                for value in field_expressions {
                    value.compile_as_statement(datapack, ctx);
                }
            }
            Self::Condition(inverted, execute_if_subcommand) => {
                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::If(inverted, *execute_if_subcommand)),
                );
            }
            _ => {}
        }
    }

    #[must_use]
    pub fn call_to_value(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        arguments: &[Self],
    ) -> Self {
        match self {
            Self::ResourceLocation(resource_location) => {
                assert!(arguments.is_empty());

                let unique_score = datapack.get_unique_score();

                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Result,
                        ExecuteStoreSubcommand::Score(
                            unique_score.score.clone(),
                            Box::new(ExecuteSubcommand::Run(Box::new(Command::Function(
                                resource_location,
                                None,
                            )))),
                        ),
                    )),
                );

                Self::PlayerScore(unique_score)
            }
            Self::Function(original_id) => compile_function(datapack, ctx, original_id, arguments),
            _ => unreachable!("The expression '{:?}' is not callable", self),
        }
    }

    pub fn call(self, datapack: &mut Datapack, ctx: &mut CompileContext, arguments: &[Self]) {
        match self {
            Self::ResourceLocation(resource_location) => {
                assert!(arguments.is_empty());

                ctx.add_command(datapack, Command::Function(resource_location, None));
            }
            Self::Function(original_id) => {
                compile_function(datapack, ctx, original_id, arguments);
            }
            _ => unreachable!("The expression '{:?}' is not callable", self),
        }
    }

    #[must_use]
    pub fn dereference(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> Option<Self> {
        Some(match self {
            Self::PlayerScore(score) => {
                let unique_score = datapack.get_unique_score();

                score.assign_to_score(datapack, ctx, unique_score.clone());

                Self::PlayerScore(unique_score)
            }
            Self::Data(target_path) => {
                let (target, path) = *target_path;

                let (unique_target, unique_path) = datapack.get_unique_data();

                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Modify(
                        unique_target.target.clone(),
                        unique_path.clone(),
                        DataCommandModificationMode::Set,
                        DataCommandModification::From(target.target, Some(path)),
                    )),
                );

                Self::Data(Box::new((unique_target, unique_path)))
            }
            _ => return None,
        })
    }

    #[must_use]
    pub fn access_field(
        self,
        data_type: &DataType,
        datapack: &Datapack,
        field: &str,
    ) -> Option<Self> {
        match self {
            Self::Compound(compound) => compound.into_iter().find_map(|(actual_field, value)| {
                if actual_field.1 == field {
                    Some(value)
                } else {
                    None
                }
            }),
            Self::Data(target_path) => {
                let (target, path) = *target_path;

                let node = if let DataType::Data(inner) = data_type
                    && let DataType::Struct(id) = &**inner
                    && let (_, _, StructDeclaration::Tuple(_)) = datapack.get_struct_type(*id)
                {
                    let index = field.parse::<i32>().ok()?;

                    NbtPathNode::Index(Some(SNBT::macroable_integer(index)))
                } else if let DataType::Tuple(_) = data_type {
                    let index = field.parse::<i32>().ok()?;

                    NbtPathNode::Index(Some(SNBT::macroable_integer(index)))
                } else {
                    NbtPathNode::Named(SNBTString(false, field.to_owned()), None)
                };

                Some(Self::Data(Box::new((target, path.with_node(node)))))
            }
            Self::Tuple(mut expressions) => {
                let index = field.parse::<i32>().ok()?;

                Some(expressions.remove(index as usize))
            }
            Self::StructStruct(_, fields) => fields
                .into_iter()
                .find_map(|(key, value)| if key == field { Some(value) } else { None }),
            Self::TupleStruct(_, fields) => {
                let field = field.parse::<usize>().ok()?;

                fields.get(field).cloned()
            }
            _ => None,
        }
    }

    #[must_use]
    pub fn can_into_snbt(&self) -> bool {
        match self {
            Self::PlayerScore(_) | Self::Data(_) | Self::Condition(_, _) => false,
            Self::StructStruct(_, field_expressions) => {
                field_expressions.values().all(Self::can_into_snbt)
            }
            Self::List(list) | Self::Tuple(list) => list.iter().all(Self::can_into_snbt),
            Self::Compound(compound) => compound.values().all(Self::can_into_snbt),
            _ => true,
        }
    }

    pub fn try_into_snbt(self) -> Result<SNBT, Self> {
        Ok(match self {
            Self::StructStruct(name, field_expressions) => {
                if !field_expressions.values().all(Self::can_into_snbt) {
                    return Err(Self::StructStruct(name, field_expressions));
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
            Self::Boolean(boolean) => SNBT::byte(i8::from(boolean)),
            Self::Byte(byte) => SNBT::byte(byte),
            Self::Short(short) => SNBT::short(short),
            Self::Integer(integer) => SNBT::integer(integer),
            Self::Long(long) => SNBT::long(long),
            Self::Float(float) => SNBT::float(float),
            Self::Double(double) => SNBT::double(double),
            Self::String(string) => SNBT::snbt_string(string),
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
                    .map(|(key, value)| (key, value.try_into_snbt().unwrap()))
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
                    Macroable::Regular(SNBT::byte(1)),
                );

                SNBT::compound(compound)
            }
            Self::Never => {
                let mut compound = SNBTCompound::new();

                compound.insert(
                    SNBTString(false, "__kelp_rs_never__".to_string()),
                    Macroable::Regular(SNBT::byte(1)),
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
            Self::Boolean(_)
            | Self::Byte(_)
            | Self::Short(_)
            | Self::Integer(_)
            | Self::Long(_)
            | Self::Float(_)
            | Self::Double(_)
            | Self::String(_)
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
                    .map(|(key, value)| (key, value.as_snbt_macros(ctx)))
                    .collect(),
            )),
            _ => ctx.get_macro_snbt(self),
        }
    }

    pub fn as_snbt_double(self, ctx: &mut CompileContext) -> Option<Macroable<NotNan<f64>>> {
        Some(match self {
            Self::Byte(_)
            | Self::Short(_)
            | Self::Integer(_)
            | Self::Long(_)
            | Self::Float(_)
            | Self::Double(_) => {
                Macroable::Regular(NotNan::new(self.try_as_f32().unwrap()).unwrap())
            }

            Self::PlayerScore(_) | Self::Data(_) => ctx.get_macro_snbt(self),

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
            Self::PlayerScore(player_score) if !force || player_score.is_generated => player_score,
            _ => {
                let unique_score = datapack.get_unique_score();
                self.assign_to_score(datapack, ctx, unique_score.clone());
                unique_score
            }
        }
    }

    #[must_use]
    pub fn to_score(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> Self {
        match self {
            Self::PlayerScore(player_score) if player_score.is_generated => {
                Self::PlayerScore(player_score)
            }
            _ => {
                let unique_score = datapack.get_unique_score();
                self.assign_to_score(datapack, ctx, unique_score.clone());
                Self::PlayerScore(unique_score)
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
        Self::PlayerScore(unique_score)
    }

    pub fn assign_to_data(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        target: GeneratedDataTarget,
        path: NbtPath,
    ) {
        match self.try_into_snbt() {
            Ok(snbt) => {
                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Modify(
                        target.target,
                        path,
                        DataCommandModificationMode::Set,
                        DataCommandModification::Value(Macroable::Regular(snbt)),
                    )),
                );
            }
            Err(self_) => match self_ {
                Self::PlayerScore(score) => {
                    score.assign_to_data(datapack, ctx, target, path);
                }
                Self::Data(inner_target_inner_path) => {
                    let (inner_target, inner_path) = *inner_target_inner_path;

                    ctx.add_command(
                        datapack,
                        Command::Data(DataCommand::Modify(
                            target.target,
                            path,
                            DataCommandModificationMode::Set,
                            DataCommandModification::From(inner_target.target, Some(inner_path)),
                        )),
                    );
                }
                Self::List(list) => {
                    let (constants, non_constants) = split_constants_list(list);

                    ctx.add_command(
                        datapack,
                        Command::Data(DataCommand::Modify(
                            target.target.clone(),
                            path.clone(),
                            DataCommandModificationMode::Set,
                            DataCommandModification::Value(SNBT::macroable_list(constants)),
                        )),
                    );

                    for (index, non_constant) in non_constants {
                        non_constant.assign_to_data(
                            datapack,
                            ctx,
                            target.clone(),
                            path.clone().with_node(NbtPathNode::Index(Some(
                                SNBT::macroable_integer(index as i32),
                            ))),
                        );
                    }
                }
                Self::Compound(compound) => {
                    let (constants, non_constants) = split_constants_compound(compound);

                    ctx.add_command(
                        datapack,
                        Command::Data(DataCommand::Modify(
                            target.target.clone(),
                            path.clone(),
                            DataCommandModificationMode::Set,
                            DataCommandModification::Value(SNBT::macroable_compound(constants)),
                        )),
                    );

                    for (key, non_constant) in non_constants {
                        non_constant.assign_to_data(
                            datapack,
                            ctx,
                            target.clone(),
                            path.clone().with_node(NbtPathNode::named(key)),
                        );
                    }
                }
                Self::StructStruct(_, field_expressions) => {
                    for (key, value) in field_expressions {
                        value.assign_to_data(
                            datapack,
                            ctx,
                            target.clone(),
                            path.clone()
                                .with_node(NbtPathNode::Named(SNBTString(false, key), None)),
                        );
                    }
                }
                Self::TupleStruct(_, field_expressions) => {
                    for (field_index, field_expression) in field_expressions.into_iter().enumerate()
                    {
                        field_expression.assign_to_data(
                            datapack,
                            ctx,
                            target.clone(),
                            path.clone().with_node(NbtPathNode::Index(Some(
                                SNBT::macroable_integer(field_index as i32),
                            ))),
                        );
                    }
                }
                Self::Tuple(expressions) => {
                    let (constants, non_constants) = split_constants_list(expressions);

                    ctx.add_command(
                        datapack,
                        Command::Data(DataCommand::Modify(
                            target.target.clone(),
                            path.clone(),
                            DataCommandModificationMode::Set,
                            DataCommandModification::Value(SNBT::macroable_list(constants)),
                        )),
                    );

                    for (index, non_constant) in non_constants {
                        non_constant.assign_to_data(
                            datapack,
                            ctx,
                            target.clone(),
                            path.clone().with_node(NbtPathNode::Index(Some(
                                SNBT::macroable_integer(index as i32),
                            ))),
                        );
                    }
                }
                Self::Condition(inverted, condition) => {
                    ctx.add_command(
                        datapack,
                        Command::Execute(ExecuteSubcommand::Store(
                            StoreType::Result,
                            ExecuteStoreSubcommand::Data(
                                target.target,
                                path,
                                NumericSNBTType::Integer,
                                NotNan::new(1.0).unwrap(),
                                Box::new(ExecuteSubcommand::If(inverted, *condition)),
                            ),
                        )),
                    );
                }
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
        target: GeneratedDataTarget,
        path: NbtPath,
        scale: NotNan<f32>,
    ) {
        match self.try_into_snbt_scale(scale) {
            Ok(snbt) => {
                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Modify(
                        target.target,
                        path,
                        DataCommandModificationMode::Set,
                        DataCommandModification::Value(Macroable::Regular(snbt)),
                    )),
                );
            }
            Err(self_) => match self_ {
                Self::PlayerScore(score) => {
                    score.assign_to_data_scale(datapack, ctx, target, path, scale);
                }
                Self::Data(inner_target_inner_path) => {
                    let (inner_target, inner_path) = *inner_target_inner_path;

                    ctx.add_command(
                        datapack,
                        Command::Data(DataCommand::Modify(
                            target.target,
                            path,
                            DataCommandModificationMode::Set,
                            DataCommandModification::From(inner_target.target, Some(inner_path)),
                        )),
                    );
                }
                Self::List(list) => {
                    let (constants, non_constants) = split_constants_list(list);

                    ctx.add_command(
                        datapack,
                        Command::Data(DataCommand::Modify(
                            target.target.clone(),
                            path.clone(),
                            DataCommandModificationMode::Set,
                            DataCommandModification::Value(SNBT::macroable_list(constants)),
                        )),
                    );

                    for (index, non_constant) in non_constants {
                        non_constant.assign_to_data(
                            datapack,
                            ctx,
                            target.clone(),
                            path.clone().with_node(NbtPathNode::Index(Some(
                                SNBT::macroable_integer(index as i32),
                            ))),
                        );
                    }
                }
                Self::Compound(compound) => {
                    let (constants, non_constants) = split_constants_compound(compound);

                    ctx.add_command(
                        datapack,
                        Command::Data(DataCommand::Modify(
                            target.target.clone(),
                            path.clone(),
                            DataCommandModificationMode::Set,
                            DataCommandModification::Value(SNBT::macroable_compound(constants)),
                        )),
                    );

                    for (key, non_constant) in non_constants {
                        non_constant.assign_to_data_scale(
                            datapack,
                            ctx,
                            target.clone(),
                            path.clone().with_node(NbtPathNode::named(key)),
                            scale,
                        );
                    }
                }
                Self::StructStruct(_, fields) => {
                    for (key, value) in fields {
                        value.assign_to_data_scale(
                            datapack,
                            ctx,
                            target.clone(),
                            path.clone()
                                .with_node(NbtPathNode::Named(SNBTString(false, key), None)),
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
                            target.clone(),
                            path.clone().with_node(NbtPathNode::Index(Some(
                                SNBT::macroable_integer(field_index as i32),
                            ))),
                            scale,
                        );
                    }
                }
                Self::Tuple(expressions) => {
                    let (constants, non_constants) = split_constants_list(expressions);

                    ctx.add_command(
                        datapack,
                        Command::Data(DataCommand::Modify(
                            target.target.clone(),
                            path.clone(),
                            DataCommandModificationMode::Set,
                            DataCommandModification::Value(SNBT::macroable_list(constants)),
                        )),
                    );

                    for (index, non_constant) in non_constants {
                        non_constant.assign_to_data_scale(
                            datapack,
                            ctx,
                            target.clone(),
                            path.clone().with_node(NbtPathNode::Index(Some(
                                SNBT::macroable_integer(index as i32),
                            ))),
                            scale,
                        );
                    }
                }
                Self::Condition(inverted, condition) => {
                    ctx.add_command(
                        datapack,
                        Command::Execute(ExecuteSubcommand::Store(
                            StoreType::Result,
                            ExecuteStoreSubcommand::Data(
                                target.target,
                                path,
                                NumericSNBTType::Integer,
                                scale,
                                Box::new(ExecuteSubcommand::If(inverted, *condition)),
                            ),
                        )),
                    );
                }
                _ => {
                    unreachable!("Checked by ResolvedExpression::try_into_snbt")
                }
            },
        }
    }

    #[must_use]
    pub fn to_data(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        force: bool,
    ) -> (GeneratedDataTarget, NbtPath) {
        #[allow(clippy::single_match_else)]
        match self {
            Self::Data(target_path) if !force => *target_path,
            _ => {
                let (unique_data, path) = datapack.get_unique_data();

                self.assign_to_data(datapack, ctx, unique_data.clone(), path.clone());

                (unique_data, path)
            }
        }
    }

    #[must_use]
    pub fn to_data_scale(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        scale: NotNan<f32>,
    ) -> (GeneratedDataTarget, NbtPath) {
        let (unique_data, path) = datapack.get_unique_data();

        self.assign_to_data_scale(datapack, ctx, unique_data.clone(), path.clone(), scale);

        (unique_data, path)
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
                let (target, path) = self_.to_data(datapack, ctx, false);

                DataCommandModification::From(target.target, Some(path))
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
            Self::String(SNBTString(_, v)) if force => v.len() as i32,
            Self::List(v) if force => v.len() as i32,
            Self::Compound(compound) if force => compound.len() as i32,
            _ => return None,
        })
    }

    pub fn operate_on_score(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        target: GeneratedPlayerScore,
        operator: ArithmeticOperator,
    ) {
        if let Some(value) = self.try_as_i32(true) {
            return match operator {
                ArithmeticOperator::Add => {
                    push_scoreboard_players(
                        datapack,
                        ctx,
                        PlayersScoreboardCommand::Add(target.score, value),
                    );
                }
                ArithmeticOperator::Subtract => {
                    push_scoreboard_players(
                        datapack,
                        ctx,
                        PlayersScoreboardCommand::Remove(target.score, value),
                    );
                }
                ArithmeticOperator::And | ArithmeticOperator::Or => {
                    datapack
                        .get_constant_score(value)
                        .operate_on_score(datapack, ctx, target, operator);
                }
                ArithmeticOperator::LeftShift => {
                    compile_shift_operation(
                        datapack,
                        ctx,
                        &target,
                        value,
                        ScoreOperationOperator::Multiply,
                    );
                }
                ArithmeticOperator::RightShift => {
                    compile_shift_operation(
                        datapack,
                        ctx,
                        &target,
                        value,
                        ScoreOperationOperator::Divide,
                    );
                }
                _ => {
                    let constant_score = datapack.get_constant_score(value);

                    push_scoreboard_players(
                        datapack,
                        ctx,
                        PlayersScoreboardCommand::Operation(
                            target.score.clone(),
                            operator
                                .into_scoreboard_players_operation_operator()
                                .unwrap(),
                            constant_score.score,
                        ),
                    );
                }
            };
        }

        if let Self::PlayerScore(self_) = self {
            self_.operate_on_score(datapack, ctx, target, operator);
        } else {
            let unique_score = datapack.get_unique_score();

            self.assign_to_score(datapack, ctx, unique_score.clone());

            unique_score.operate_on_score(datapack, ctx, target, operator);
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

                Self::PlayerScore(unique_score)
            }
        }
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
            (left_kind @ Self::Data(_), right_kind)
                if operator == ComparisonOperator::EqualTo
                    || operator == ComparisonOperator::NotEqualTo =>
            {
                let unique_score = datapack.get_unique_score();

                let (unique_target, unique_path) = datapack.get_unique_data();

                left_kind.assign_to_data(datapack, ctx, unique_target.clone(), unique_path.clone());

                let data_command_modification =
                    right_kind.as_data_command_modification(datapack, ctx);

                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Success,
                        ExecuteStoreSubcommand::Score(
                            unique_score.score.clone(),
                            Box::new(ExecuteSubcommand::Run(Box::new(Command::Data(
                                DataCommand::Modify(
                                    unique_target.target,
                                    unique_path,
                                    DataCommandModificationMode::Set,
                                    data_command_modification,
                                ),
                            )))),
                        ),
                    )),
                );

                Self::Condition(
                    operator.should_execute_if_be_inverted(),
                    Box::new(ExecuteIfSubcommand::Score(
                        unique_score.score,
                        ScoreComparison::Range(IntegerRange::new_single(0)),
                        None,
                    )),
                )
            }
            (self_ @ Self::Data(_), other) | (other, self_ @ Self::Data(_)) => {
                let score = self_.as_score(datapack, ctx, false);

                let other_score = other.as_score(datapack, ctx, false);

                Self::Condition(
                    operator.should_execute_if_be_inverted(),
                    Box::new(ExecuteIfSubcommand::Score(
                        score.score,
                        ScoreComparison::Score(
                            operator.into_score_comparison_operator(),
                            other_score.score,
                        ),
                        None,
                    )),
                )
            }
            (Self::PlayerScore(score), other) => {
                if let Some(value) = other.try_as_i32(false) {
                    return Self::Condition(
                        operator.should_execute_if_be_inverted(),
                        Box::new(ExecuteIfSubcommand::Score(
                            score.score,
                            ScoreComparison::Range(integer_range_from_comparison_operator(
                                &operator.into_score_comparison_operator(),
                                value,
                            )),
                            None,
                        )),
                    );
                }

                let right_score = other.as_score(datapack, ctx, false);

                Self::Condition(
                    operator.should_execute_if_be_inverted(),
                    Box::new(ExecuteIfSubcommand::Score(
                        score.score,
                        ScoreComparison::Score(
                            operator.into_score_comparison_operator(),
                            right_score.score,
                        ),
                        None,
                    )),
                )
            }
            (left_kind, Self::PlayerScore(right_score)) => {
                let left_score = left_kind.as_score(datapack, ctx, false);

                Self::Condition(
                    operator.should_execute_if_be_inverted(),
                    Box::new(ExecuteIfSubcommand::Score(
                        right_score.score,
                        ScoreComparison::Score(
                            operator.into_score_comparison_operator(),
                            left_score.score,
                        ),
                        None,
                    )),
                )
            }
            _ => unreachable!(),
        }
    }

    #[must_use]
    pub fn cast_to(self, data_type: DataType) -> Option<Self> {
        Some(match (self, data_type) {
            (Self::Byte(value), DataType::Short) => Self::Short(i16::from(value)),
            (Self::Byte(value), DataType::Integer) => Self::Integer(i32::from(value)),
            (Self::Byte(value), DataType::Long) => Self::Long(i64::from(value)),
            (Self::Byte(value), DataType::Float) => {
                Self::Float(NotNan::new(f32::from(value)).unwrap())
            }
            (Self::Byte(value), DataType::Double) => {
                Self::Double(NotNan::new(f64::from(value)).unwrap())
            }

            (Self::Short(value), DataType::Byte) => Self::Byte(value as i8),
            (Self::Short(value), DataType::Integer) => Self::Integer(i32::from(value)),
            (Self::Short(value), DataType::Long) => Self::Long(i64::from(value)),
            (Self::Short(value), DataType::Float) => {
                Self::Float(NotNan::new(f32::from(value)).unwrap())
            }
            (Self::Short(value), DataType::Double) => {
                Self::Double(NotNan::new(f64::from(value)).unwrap())
            }

            (Self::Integer(value), DataType::Byte) => Self::Byte(value as i8),
            (Self::Integer(value), DataType::Short) => Self::Short(value as i16),
            (Self::Integer(value), DataType::Long) => Self::Long(i64::from(value)),
            (Self::Integer(value), DataType::Float) => {
                Self::Float(NotNan::new(value as f32).unwrap())
            }
            (Self::Integer(value), DataType::Double) => {
                Self::Double(NotNan::new(f64::from(value)).unwrap())
            }

            (Self::Long(value), DataType::Byte) => Self::Byte(value as i8),
            (Self::Long(value), DataType::Short) => Self::Short(value as i16),
            (Self::Long(value), DataType::Integer) => Self::Integer(value as i32),
            (Self::Long(value), DataType::Float) => Self::Float(NotNan::new(value as f32).unwrap()),
            (Self::Long(value), DataType::Double) => {
                Self::Double(NotNan::new(value as f64).unwrap())
            }

            (Self::Float(value), DataType::Byte) => Self::Byte(value.into_inner() as i8),
            (Self::Float(value), DataType::Short) => Self::Short(value.into_inner() as i16),
            (Self::Float(value), DataType::Integer) => Self::Integer(value.into_inner() as i32),
            (Self::Float(value), DataType::Long) => Self::Long(value.into_inner() as i64),
            (Self::Float(value), DataType::Double) => Self::Double(value.into()),

            (Self::Double(value), DataType::Byte) => Self::Byte(value.into_inner() as i8),
            (Self::Double(value), DataType::Short) => Self::Short(value.into_inner() as i16),
            (Self::Double(value), DataType::Integer) => Self::Integer(value.into_inner() as i32),
            (Self::Double(value), DataType::Long) => Self::Long(value.into_inner() as i64),
            (Self::Double(value), DataType::Float) => {
                Self::Float(unsafe { NotNan::new_unchecked(value.into_inner() as f32) })
            }

            (self_ @ Self::Boolean(_), DataType::Boolean)
            | (self_ @ Self::Byte(_), DataType::Byte)
            | (self_ @ Self::Short(_), DataType::Short)
            | (self_ @ Self::Integer(_), DataType::Integer)
            | (self_ @ Self::Long(_), DataType::Long)
            | (self_ @ Self::Float(_), DataType::Float)
            | (self_ @ Self::Double(_), DataType::Double)
            | (self_ @ Self::Data(_), DataType::Data(_))
            | (self_ @ Self::PlayerScore(_), DataType::Score(_)) => self_,

            _ => return None,
        })
    }

    pub fn to_execute_condition(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        inverted: bool,
    ) -> Option<(bool, ExecuteIfSubcommand)> {
        if let Some(value) = self.try_as_i32(true) {
            let unique_score = datapack.get_unique_score();

            push_scoreboard_players(
                datapack,
                ctx,
                PlayersScoreboardCommand::Set(unique_score.score.clone(), value),
            );

            return Some(unique_score.to_execute_condition(inverted));
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
            Self::PlayerScore(score) => score.to_execute_condition(inverted),
            Self::Data(_) => {
                let unique_score = datapack.get_unique_score();

                self.assign_to_score(datapack, ctx, unique_score.clone());

                unique_score.to_execute_condition(inverted)
            }
            Self::Condition(inner_inverted, condition) => (inverted ^ inner_inverted, *condition),
            _ => return None,
        })
    }

    #[must_use]
    pub fn as_text_component(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        force_display: bool,
    ) -> SNBT {
        match self {
            Self::PlayerScore(player_score) => player_score.to_text_component(),
            Self::Data(target_path) => {
                let (target, path) = *target_path;

                let mut map = SNBTCompound::new();

                match target.target {
                    DataTarget::Block(coordinates) => {
                        map.insert(
                            SNBTString(false, "block".to_string()),
                            Macroable::Regular(SNBT::string(coordinates)),
                        );
                    }
                    DataTarget::Entity(entity_selector) => {
                        map.insert(
                            SNBTString(false, "entity".to_string()),
                            Macroable::Regular(SNBT::string(entity_selector)),
                        );
                    }
                    DataTarget::Storage(resource_location) => {
                        map.insert(
                            SNBTString(false, "storage".to_string()),
                            Macroable::Regular(SNBT::string(resource_location)),
                        );
                    }
                }

                map.insert(
                    SNBTString(false, "nbt".to_string()),
                    Macroable::Regular(path.to_snbt_string()),
                );

                SNBT::compound(map)
            }
            Self::Boolean(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::byte(i8::from(value))
                }
            }
            Self::Byte(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::byte(value)
                }
            }
            Self::Short(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::short(value)
                }
            }
            Self::Integer(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::integer(value)
                }
            }
            Self::Long(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::long(value)
                }
            }
            Self::Float(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::float(value)
                }
            }
            Self::Double(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::double(value)
                }
            }
            Self::String(string) => {
                if force_display {
                    SNBT::string(format!("\"{}\"", string.1))
                } else {
                    SNBT::snbt_string(string)
                }
            }
            Self::List(list) => SNBT::list(
                list.into_iter()
                    .map(|element| element.as_text_component(datapack, ctx, false))
                    .collect(),
            ),
            Self::Compound(compound) => SNBT::compound(
                compound
                    .into_iter()
                    .map(|(key, value)| (key, value.as_text_component(datapack, ctx, false)))
                    .collect(),
            ),
            Self::Condition(_, _) => {
                let unique_score = datapack.get_unique_score();

                self.assign_to_score(datapack, ctx, unique_score.clone());

                unique_score.to_text_component()
            }
            Self::Tuple(tuple) => {
                let mut list = Vec::new();

                list.push(SNBT::string("("));

                for (i, element) in tuple.into_iter().enumerate() {
                    if i != 0 {
                        list.push(SNBT::string(", "));
                    }

                    list.push(element.as_text_component(datapack, ctx, true));
                }

                list.push(SNBT::string(")"));

                SNBT::list(list)
            }
            Self::Never => SNBT::string('!'),
            Self::Unit => SNBT::string("()"),
            Self::Function(id) => {
                let (_, _, declaration) = datapack.get_function_declaration(id);

                let mut output = format!("fn {}", declaration.name);

                format_generics(
                    &mut output,
                    &declaration.generic_types,
                    &datapack.environment,
                );

                output.push('(');

                for (i, (_, data_type)) in declaration.parameters.iter().enumerate() {
                    let Some(data_type) = data_type else {
                        continue;
                    };

                    if i != 0 {
                        output.push_str(", ");
                    }

                    let _ = write!(output, "{}", data_type.display(&datapack.environment));
                }

                output.push(')');

                if declaration.return_type != DataType::Unit {
                    let _ = write!(
                        output,
                        " -> {}",
                        declaration.return_type.display(&datapack.environment)
                    );
                }

                SNBT::string(output)
            }

            Self::StructStruct(id, fields) => {
                if force_display {
                    // TODO: Maybe display full path?

                    let (visibility, _, declaration) = datapack.get_struct_struct_type(id);

                    let mut output = Vec::new();

                    if visibility.should_display() {
                        output.push(SNBT::string(format!("{} ", visibility)));
                    }

                    output.push(SNBT::string(if declaration.generic_types.is_empty() {
                        if fields.is_empty() {
                            format!("{} {{", declaration.name)
                        } else {
                            format!("{} {{ ", declaration.name)
                        }
                    } else {
                        declaration.name.clone()
                    }));

                    if !declaration.generic_types.is_empty() {
                        output.push(SNBT::string("<"));

                        for (i, generic) in declaration.generic_types.iter().enumerate() {
                            if i != 0 {
                                output.push(SNBT::string(", "));
                            }

                            output.push(SNBT::string(generic.display(&datapack.environment)));
                        }

                        output.push(SNBT::string(if fields.is_empty() { "> {" } else { "> { " }));
                    }

                    for (i, (key, value)) in fields.iter().enumerate() {
                        if i != 0 {
                            output.push(SNBT::string(", "));
                        }

                        output.push(SNBT::string(format!("{}: ", key)));
                        output.push(value.clone().as_text_component(datapack, ctx, true));
                    }

                    if fields.is_empty() {
                        output.push(SNBT::string("}"));
                    } else {
                        output.push(SNBT::string(" }"));
                    }

                    SNBT::list(output)
                } else {
                    let mut output = SNBTCompound::new();

                    for (field_name, field_value) in fields {
                        output.insert(
                            SNBTString(false, field_name),
                            Macroable::Regular(field_value.as_text_component(datapack, ctx, false)),
                        );
                    }

                    SNBT::compound(output)
                }
            }
            Self::TupleStruct(id, fields) => {
                if force_display {
                    // TODO: Maybe display full path?

                    let (visibility, _, declaration) = datapack.get_tuple_struct_type(id);

                    let mut output = Vec::new();

                    if visibility.should_display() {
                        output.push(SNBT::string(format!("{} ", visibility)));
                    }

                    output.push(SNBT::string(if declaration.generic_types.is_empty() {
                        format!("{}(", declaration.name)
                    } else {
                        declaration.name.clone()
                    }));

                    if !declaration.generic_types.is_empty() {
                        output.push(SNBT::string("<"));

                        for (i, generic) in declaration.generic_types.iter().enumerate() {
                            if i != 0 {
                                output.push(SNBT::string(", "));
                            }

                            output.push(SNBT::string(generic.display(&datapack.environment)));
                        }

                        output.push(SNBT::string(">("));
                    }

                    for (i, value) in fields.iter().enumerate() {
                        if i != 0 {
                            output.push(SNBT::string(", "));
                        }

                        output.push(value.clone().as_text_component(datapack, ctx, true));
                    }

                    output.push(SNBT::string(")"));

                    SNBT::list(output)
                } else {
                    let mut output = Vec::new();

                    for field_value in fields {
                        output.push(Macroable::Regular(
                            field_value.as_text_component(datapack, ctx, false),
                        ));
                    }

                    SNBT::list(output)
                }
            }
            Self::ResourceLocation(resource_location) => SNBT::string(resource_location),
            Self::EntitySelector(selector) => SNBT::string(selector),
        }
    }

    #[must_use]
    pub fn as_place(self) -> Option<Place> {
        Some(match self {
            Self::PlayerScore(score) => Place::Score(score),
            Self::Data(target_path) => {
                let (target, path) = *target_path;

                Place::Data(target, path)
            }
            Self::Tuple(expressions) => Place::Tuple(
                expressions
                    .into_iter()
                    .map(Self::as_place)
                    .collect_option_all()?,
            ),
            _ => return None,
        })
    }

    #[must_use]
    pub const fn is_lvalue(&self) -> bool {
        matches!(self, Self::PlayerScore(_) | Self::Data(_))
    }

    pub fn compile_augmented_assignment(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        operator: ArithmeticOperator,
        value: Self,
    ) {
        match self {
            Self::PlayerScore(score) => {
                value.operate_on_score(datapack, ctx, score, operator);
            }
            Self::Data(ref target_path) => {
                let (target, path) = &**target_path;

                let unique_score = datapack.get_unique_score();

                self.clone()
                    .assign_to_score(datapack, ctx, unique_score.clone());

                value.operate_on_score(datapack, ctx, unique_score.clone(), operator);

                unique_score.assign_to_data(datapack, ctx, target.clone(), path.clone());
            }
            _ => unreachable!(),
        }
    }

    pub fn assign_to_score(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        score: GeneratedPlayerScore,
    ) {
        match self {
            Self::Boolean(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(score.score, i32::from(value)),
                );
            }
            Self::Byte(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(score.score, i32::from(value)),
                );
            }
            Self::Short(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(score.score, i32::from(value)),
                );
            }
            Self::Integer(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(score.score, value),
                );
            }
            Self::Long(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(score.score, value as i32),
                );
            }
            Self::Float(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(score.score, value.into_inner() as i32),
                );
            }
            Self::Double(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(score.score, value.into_inner() as i32),
                );
            }
            Self::String(SNBTString(_, value)) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(score.score, value.len() as i32),
                );
            }
            Self::List(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(score.score, value.len() as i32),
                );
            }
            Self::Compound(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(score.score, value.len() as i32),
                );
            }
            Self::PlayerScore(source) => {
                source.assign_to_score(datapack, ctx, score);
            }
            Self::Data(data_target_path) => {
                let (data_target, path) = *data_target_path;

                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Result,
                        ExecuteStoreSubcommand::Score(
                            score.score,
                            Box::new(ExecuteSubcommand::Run(Box::new(Command::Data(
                                DataCommand::Get(data_target.target, Some(path), None),
                            )))),
                        ),
                    )),
                );
            }
            Self::Condition(inverted, condition) => {
                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Success,
                        ExecuteStoreSubcommand::Score(
                            score.score,
                            Box::new(ExecuteSubcommand::If(inverted, *condition)),
                        ),
                    )),
                );
            }
            _ => {
                unreachable!("{:?}", self)
            }
        }
    }

    pub fn assign_to_score_scale(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        target: GeneratedPlayerScore,
        scale: NotNan<f32>,
    ) {
        match self {
            Self::Byte(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(
                        target.score,
                        (f32::from(value) * scale.into_inner()) as i32,
                    ),
                );
            }
            Self::Short(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(
                        target.score,
                        (f32::from(value) * scale.into_inner()) as i32,
                    ),
                );
            }
            Self::Integer(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(
                        target.score,
                        (value as f32 * scale.into_inner()) as i32,
                    ),
                );
            }
            Self::Long(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(
                        target.score,
                        (value as f32 * scale.into_inner()) as i32,
                    ),
                );
            }
            Self::Float(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(
                        target.score,
                        (value.into_inner() * scale.into_inner()) as i32,
                    ),
                );
            }
            Self::Double(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(
                        target.score,
                        (value.into_inner() * f64::from(scale.into_inner())) as i32,
                    ),
                );
            }
            Self::PlayerScore(source) => {
                source.assign_to_score_scale(datapack, ctx, target, scale);
            }
            Self::Data(data_target_path) => {
                let (data_target, path) = *data_target_path;

                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Result,
                        ExecuteStoreSubcommand::Score(
                            target.score,
                            Box::new(ExecuteSubcommand::Run(Box::new(Command::Data(
                                DataCommand::Get(data_target.target, Some(path), Some(scale)),
                            )))),
                        ),
                    )),
                );
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
            Self::Condition(inverted, subcommand) => Self::Condition(!inverted, subcommand),
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
            Self::PlayerScore(_) => {
                let unique_score = datapack.get_unique_score();

                self.assign_to_score(datapack, ctx, unique_score.clone());

                let constant_score = datapack.get_constant_score(-1);

                ctx.add_command(
                    datapack,
                    Command::Scoreboard(ScoreboardCommand::Players(
                        PlayersScoreboardCommand::Operation(
                            unique_score.score.clone(),
                            ScoreOperationOperator::Multiply,
                            constant_score.score,
                        ),
                    )),
                );

                Self::PlayerScore(unique_score)
            }
            Self::Data(_) => {
                let unique_score = self.clone().as_score(datapack, ctx, true);

                self.assign_to_score(datapack, ctx, unique_score.clone());

                let constant_score = datapack.get_constant_score(-1);

                ctx.add_command(
                    datapack,
                    Command::Scoreboard(ScoreboardCommand::Players(
                        PlayersScoreboardCommand::Operation(
                            unique_score.score.clone(),
                            ScoreOperationOperator::Multiply,
                            constant_score.score,
                        ),
                    )),
                );

                Self::PlayerScore(unique_score)
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
                    self.to_execute_condition(datapack, ctx, false).unwrap();
                let (right_inverted, right_condition) =
                    other.to_execute_condition(datapack, ctx, false).unwrap();

                ctx.add_command(
                    datapack,
                    Command::Scoreboard(ScoreboardCommand::Players(PlayersScoreboardCommand::Set(
                        unique_score.score.clone(),
                        0,
                    ))),
                );

                Self::Condition(
                    self_inverted,
                    Box::new(self_condition.then(ExecuteSubcommand::If(
                        right_inverted,
                        right_condition.then(ExecuteSubcommand::Run(Box::new(
                            Command::Scoreboard(ScoreboardCommand::Players(
                                PlayersScoreboardCommand::Set(unique_score.score.clone(), 1),
                            )),
                        ))),
                    ))),
                )
                .compile_as_statement(datapack, ctx);

                Self::PlayerScore(unique_score)
            }
            LogicalOperator::Or => {
                let unique_function_paths = datapack.get_unique_function_paths();

                let unique_score = datapack.get_unique_score();
                ctx.add_command(
                    datapack,
                    Command::Scoreboard(ScoreboardCommand::Players(PlayersScoreboardCommand::Set(
                        unique_score.score.clone(),
                        0,
                    ))),
                );
                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Success,
                        ExecuteStoreSubcommand::Score(
                            unique_score.score.clone(),
                            Box::new(ExecuteSubcommand::Run(Box::new(Command::Function(
                                ResourceLocation::new_namespace_paths(
                                    datapack.current_namespace_name(),
                                    unique_function_paths.clone(),
                                ),
                                None,
                            )))),
                        ),
                    )),
                );

                let mut function_ctx = CompileContext::default();

                let return_one_subcommand =
                    ExecuteSubcommand::Run(Box::new(Command::Return(ReturnCommand::Value(1))));

                let (self_inverted, left_condition) = self
                    .to_execute_condition(datapack, &mut function_ctx, false)
                    .unwrap();

                function_ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::If(
                        self_inverted,
                        left_condition.then(return_one_subcommand.clone()),
                    )),
                );
                let (other_inverted, other_condition) = other
                    .to_execute_condition(datapack, &mut function_ctx, false)
                    .unwrap();

                function_ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::If(
                        other_inverted,
                        other_condition.then(return_one_subcommand),
                    )),
                );
                function_ctx.add_command(datapack, Command::Return(ReturnCommand::Fail));

                let function_commands = function_ctx.compile();

                datapack
                    .get_function_mut(&unique_function_paths)
                    .add_commands(function_commands);

                Self::PlayerScore(unique_score)
            }
        }
    }

    #[must_use]
    pub fn index(
        mut self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
        index: Self,
    ) -> Option<Self> {
        Some(match self {
            Self::List(ref mut items) => {
                if let Self::Integer(index) = index {
                    if index >= 0 && (index as usize) < items.len() {
                        items.swap_remove(index as usize)
                    } else {
                        return None;
                    }
                } else {
                    let (unique_data_target, unique_data_path) = datapack.get_unique_data();

                    self.assign_to_data(
                        datapack,
                        ctx,
                        unique_data_target.clone(),
                        unique_data_path.clone(),
                    );

                    Self::Data(Box::new((
                        unique_data_target,
                        unique_data_path
                            .with_node(NbtPathNode::Index(Some(index.as_snbt_macros(ctx)))),
                    )))
                }
            }
            Self::Data(target_path) => {
                let (target, path) = *target_path;

                Self::Data(Box::new((
                    target,
                    path.with_node(NbtPathNode::Index(Some(index.as_snbt_macros(ctx)))),
                )))
            }
            _ => return None,
        })
    }
}
