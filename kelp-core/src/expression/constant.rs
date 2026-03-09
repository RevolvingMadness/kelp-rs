use std::collections::BTreeMap;

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
    nbt_path::{NbtPath, NbtPathNode, SNBTCompound},
    range::IntegerRange,
    resource_location::ResourceLocation,
    snbt::{SNBT, SNBTString},
};
use minecraft_command_types_derive::HasMacro;
use ordered_float::NotNan;

use crate::{
    compile_context::CompileContext,
    data_type::DataTypeKind,
    datapack::HighDatapack,
    expression::utils::push_scoreboard_players,
    high::{
        data::GeneratedDataTarget, player_score::GeneratedPlayerScore, snbt_string::HighSNBTString,
    },
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator},
    place::Place,
};

pub fn compile_shift_operation(
    datapack: &mut HighDatapack,
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
                constants.push(SNBT::Compound(BTreeMap::new()));
            }
        }
    }

    (constants, non_constants)
}

#[must_use]
pub fn split_constants_compound(
    compound: BTreeMap<HighSNBTString, ResolvedExpression>,
) -> (SNBTCompound, BTreeMap<HighSNBTString, ResolvedExpression>) {
    let mut constants = BTreeMap::new();
    let mut non_constants = BTreeMap::new();

    for (key, expression) in compound {
        match expression.try_into_snbt() {
            Ok(snbt) => {
                constants.insert(key.snbt_string, snbt);
            }
            Err(expression) => {
                constants.insert(key.snbt_string.clone(), SNBT::Compound(BTreeMap::new()));
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

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum ResolvedExpression {
    Boolean(bool),
    Byte(i8),
    Short(i16),
    Integer(i32),
    Long(i64),
    Float(NotNan<f32>),
    Double(NotNan<f64>),
    String(HighSNBTString),
    Underscore,
    List(Vec<Self>),
    Compound(BTreeMap<HighSNBTString, Self>),
    Tuple(Vec<Self>),
    Struct(String, Vec<DataTypeKind>, BTreeMap<String, Self>),
    Unit,

    PlayerScore(GeneratedPlayerScore),
    Data(Box<(GeneratedDataTarget, NbtPath)>),
    Condition(bool, Box<ExecuteIfSubcommand>),
}

impl ResolvedExpression {
    pub fn compile_as_statement(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) {
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
            Self::Struct(_, _, field_values) => {
                for value in field_values.into_values() {
                    value.compile_as_statement(datapack, ctx);
                }
            }
            Self::Condition(inverted, execute_if_subcommand) => {
                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::If(inverted, *execute_if_subcommand)),
                );
            }
            Self::Boolean(_)
            | Self::Byte(_)
            | Self::Short(_)
            | Self::Integer(_)
            | Self::Long(_)
            | Self::Float(_)
            | Self::Double(_)
            | Self::String(_)
            | Self::Underscore
            | Self::Unit
            | Self::PlayerScore(_)
            | Self::Data(_) => {}
        }
    }

    #[must_use]
    pub fn dereference(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> Option<Self> {
        match self {
            Self::PlayerScore(score) => {
                let unique_score = datapack.get_unique_score();

                score.assign_to_score(datapack, ctx, unique_score.clone());

                Some(Self::PlayerScore(unique_score))
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

                Some(Self::Data(Box::new((unique_target, unique_path))))
            }
            Self::Underscore
            | Self::List(_)
            | Self::Compound(_)
            | Self::Tuple(_)
            | Self::Struct(_, _, _)
            | Self::Unit
            | Self::Condition(_, _)
            | Self::Boolean(_)
            | Self::Byte(_)
            | Self::Short(_)
            | Self::Integer(_)
            | Self::Long(_)
            | Self::Float(_)
            | Self::Double(_)
            | Self::String(_) => None,
        }
    }

    #[must_use]
    pub fn access_field(self, field: &str) -> Option<Self> {
        match self {
            Self::List(_)
            | Self::Condition(_, _)
            | Self::Unit
            | Self::PlayerScore(_)
            | Self::Underscore
            | Self::Boolean(_)
            | Self::Byte(_)
            | Self::Short(_)
            | Self::Integer(_)
            | Self::Long(_)
            | Self::Float(_)
            | Self::Double(_)
            | Self::String(_) => None,
            Self::Compound(compound) => compound.into_iter().find_map(|(actual_field, value)| {
                if actual_field.snbt_string.1 == field {
                    Some(value)
                } else {
                    None
                }
            }),
            Self::Data(target_path) => {
                let (target, path) = *target_path;

                Some(Self::Data(Box::new((
                    target,
                    path.with_node(NbtPathNode::Named(
                        SNBTString(false, field.to_owned()),
                        None,
                    )),
                ))))
            }
            Self::Tuple(mut expressions) => {
                let index = field.parse::<i32>().ok()?;

                Some(expressions.remove(index as usize))
            }
            Self::Struct(_, _, fields) => fields
                .into_iter()
                .find_map(|(key, value)| if key == field { Some(value) } else { None }),
        }
    }

    #[must_use]
    pub fn can_into_snbt(&self) -> bool {
        match self {
            Self::Underscore | Self::PlayerScore(_) | Self::Data(_) | Self::Condition(_, _) => {
                false
            }
            Self::Struct(_, _, field_values) => field_values.values().all(Self::can_into_snbt),
            Self::List(list) | Self::Tuple(list) => list.iter().all(Self::can_into_snbt),
            Self::Compound(compound) => compound.values().all(Self::can_into_snbt),
            _ => true,
        }
    }

    pub fn try_into_snbt(self) -> Result<SNBT, Self> {
        Ok(match self {
            Self::Underscore | Self::PlayerScore(_) | Self::Data(_) | Self::Condition(_, _) => {
                return Err(self);
            }
            Self::Struct(name, generic_data_types, field_values) => {
                if !field_values.values().all(Self::can_into_snbt) {
                    return Err(Self::Struct(name, generic_data_types, field_values));
                }

                let mut compound = BTreeMap::new();

                for (key, value) in field_values {
                    compound.insert(SNBTString(false, key), value.try_into_snbt().unwrap());
                }

                SNBT::Compound(compound)
            }
            Self::Boolean(boolean) => SNBT::Byte(i8::from(boolean)),
            Self::Byte(byte) => SNBT::Byte(byte),
            Self::Short(short) => SNBT::Short(short),
            Self::Integer(integer) => SNBT::Integer(integer),
            Self::Long(long) => SNBT::Long(long),
            Self::Float(float) => SNBT::Float(float),
            Self::Double(double) => SNBT::Double(double),
            Self::String(string) => SNBT::String(string.snbt_string),
            Self::List(list) => {
                if !list.iter().all(Self::can_into_snbt) {
                    return Err(Self::List(list));
                }

                let list = list
                    .into_iter()
                    .map(|element| element.try_into_snbt().unwrap())
                    .collect();

                SNBT::List(list)
            }
            Self::Compound(compound) => {
                if !compound.values().all(Self::can_into_snbt) {
                    return Err(Self::Compound(compound));
                }

                let compound = compound
                    .into_iter()
                    .map(|(key, value)| (key.snbt_string, value.try_into_snbt().unwrap()))
                    .collect();

                SNBT::Compound(compound)
            }
            Self::Tuple(tuple) => {
                if !tuple.iter().all(Self::can_into_snbt) {
                    return Err(Self::Tuple(tuple));
                }

                let tuple = tuple
                    .into_iter()
                    .map(|element| element.try_into_snbt().unwrap())
                    .collect();

                SNBT::List(tuple)
            }
            Self::Unit => {
                let mut compound = BTreeMap::new();

                compound.insert(
                    SNBTString(false, "__kelp_rs_unit__".to_string()),
                    SNBT::Byte(1),
                );

                SNBT::Compound(compound)
            }
        })
    }

    pub fn try_into_snbt_scale(self, scale: NotNan<f32>) -> Result<SNBT, Self> {
        Ok(match self {
            Self::Underscore
            | Self::PlayerScore(_)
            | Self::Data(_)
            | Self::Condition(_, _)
            | Self::Struct(_, _, _)
            | Self::Boolean(_)
            | Self::String(_)
            | Self::List(_)
            | Self::Compound(_)
            | Self::Tuple(_)
            | Self::Unit => {
                return Err(self);
            }
            Self::Byte(byte) => {
                SNBT::Float(NotNan::new(f32::from(byte) * scale.into_inner()).unwrap())
            }
            Self::Short(short) => {
                SNBT::Float(NotNan::new(f32::from(short) * scale.into_inner()).unwrap())
            }
            Self::Integer(integer) => {
                SNBT::Float(NotNan::new(integer as f32 * scale.into_inner()).unwrap())
            }
            Self::Long(long) => SNBT::Float(NotNan::new(long as f32 * scale.into_inner()).unwrap()),
            Self::Float(float) => {
                SNBT::Float(NotNan::new(float.into_inner() * scale.into_inner()).unwrap())
            }
            Self::Double(double) => SNBT::Double(
                NotNan::new(f64::from(double.into_inner() as f32 * scale.into_inner())).unwrap(),
            ),
        })
    }

    pub fn as_snbt_macros(self, ctx: &mut CompileContext) -> SNBT {
        match self {
            Self::Boolean(_)
            | Self::Byte(_)
            | Self::Short(_)
            | Self::Integer(_)
            | Self::Long(_)
            | Self::Float(_)
            | Self::Double(_)
            | Self::String(_)
            | Self::Unit => self.try_into_snbt().unwrap(),

            Self::List(list) => SNBT::List(
                list.into_iter()
                    .map(|element| element.as_snbt_macros(ctx))
                    .collect(),
            ),
            Self::Tuple(tuple) => SNBT::List(
                tuple
                    .into_iter()
                    .map(|element| element.as_snbt_macros(ctx))
                    .collect(),
            ),
            Self::Compound(compound) => SNBT::Compound(
                compound
                    .into_iter()
                    .map(|(key, value)| (key.snbt_string, value.as_snbt_macros(ctx)))
                    .collect(),
            ),
            Self::Struct(_, _, _)
            | Self::PlayerScore(_)
            | Self::Data(_)
            | Self::Condition(_, _) => ctx.get_macro_snbt(self),
            Self::Underscore => unreachable!(),
        }
    }

    pub fn assign_to_score(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: GeneratedPlayerScore,
    ) {
        match self {
            Self::Boolean(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target.score, i32::from(value)),
                );
            }
            Self::Byte(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target.score, i32::from(value)),
                );
            }
            Self::Short(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target.score, i32::from(value)),
                );
            }
            Self::Integer(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target.score, value),
                );
            }
            Self::Long(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target.score, value as i32),
                );
            }
            Self::Float(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target.score, value.into_inner() as i32),
                );
            }
            Self::Double(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target.score, value.into_inner() as i32),
                );
            }
            Self::String(HighSNBTString {
                snbt_string: SNBTString(_, value),
                ..
            }) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target.score, value.len() as i32),
                );
            }
            Self::List(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target.score, value.len() as i32),
                );
            }
            Self::Compound(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target.score, value.len() as i32),
                );
            }
            Self::PlayerScore(source) => {
                source.assign_to_score(datapack, ctx, target);
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
                            target.score,
                            Box::new(ExecuteSubcommand::If(inverted, *condition)),
                        ),
                    )),
                );
            }
            Self::Struct(_, _, _) | Self::Tuple(_) | Self::Unit | Self::Underscore => {
                unreachable!()
            }
        }
    }

    pub fn assign_to_score_scale(
        self,
        datapack: &mut HighDatapack,
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
            Self::Condition(_, _)
            | Self::Boolean(_)
            | Self::Struct(_, _, _)
            | Self::Tuple(_)
            | Self::Unit
            | Self::Underscore
            | Self::String(_)
            | Self::List(_)
            | Self::Compound(_) => {
                unreachable!("{:?}", self)
            }
        }
    }

    #[must_use]
    pub fn as_score(
        self,
        datapack: &mut HighDatapack,
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
    pub fn to_score(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> Self {
        match self {
            Self::Struct(name, generic_data_types, field_values) => {
                let mut new_field_values = BTreeMap::new();

                for (key, value) in field_values {
                    let value_score = value.as_score(datapack, ctx, true);

                    new_field_values.insert(key, Self::PlayerScore(value_score));
                }

                Self::Struct(name, generic_data_types, new_field_values)
            }
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
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        scale: NotNan<f32>,
    ) -> Self {
        #[allow(clippy::single_match_else)]
        match self {
            Self::Struct(name, generic_data_types, field_values) => {
                let mut new_field_values = BTreeMap::new();

                for (key, value) in field_values {
                    let value_score = value.as_score_scale(datapack, ctx, scale);

                    new_field_values.insert(key, Self::PlayerScore(value_score));
                }

                Self::Struct(name, generic_data_types, new_field_values)
            }
            _ => {
                let unique_score = datapack.get_unique_score();
                self.assign_to_score_scale(datapack, ctx, unique_score.clone(), scale);
                Self::PlayerScore(unique_score)
            }
        }
    }

    #[must_use]
    pub fn as_score_scale(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        scale: NotNan<f32>,
    ) -> GeneratedPlayerScore {
        let unique_score = datapack.get_unique_score();

        self.assign_to_score_scale(datapack, ctx, unique_score.clone(), scale);

        unique_score
    }

    pub fn assign_to_data(
        self,
        datapack: &mut HighDatapack,
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
                        DataCommandModification::Value(snbt),
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
                            DataCommandModification::Value(SNBT::List(constants)),
                        )),
                    );

                    for (index, non_constant) in non_constants {
                        non_constant.assign_to_data(
                            datapack,
                            ctx,
                            target.clone(),
                            path.clone()
                                .with_node(NbtPathNode::Index(Some(SNBT::Integer(index as i32)))),
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
                            DataCommandModification::Value(SNBT::Compound(constants)),
                        )),
                    );

                    for (key, non_constant) in non_constants {
                        non_constant.assign_to_data(
                            datapack,
                            ctx,
                            target.clone(),
                            path.clone().with_node(NbtPathNode::named(key.snbt_string)),
                        );
                    }
                }
                Self::Underscore => unreachable!(),
                Self::Struct(_, _, fields) => {
                    for (key, value) in fields {
                        value.assign_to_data(
                            datapack,
                            ctx,
                            target.clone(),
                            path.clone()
                                .with_node(NbtPathNode::Named(SNBTString(false, key), None)),
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
                            DataCommandModification::Value(SNBT::List(constants)),
                        )),
                    );

                    for (index, non_constant) in non_constants {
                        non_constant.assign_to_data(
                            datapack,
                            ctx,
                            target.clone(),
                            path.clone()
                                .with_node(NbtPathNode::Index(Some(SNBT::Integer(index as i32)))),
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
                Self::Unit
                | Self::Boolean(_)
                | Self::Byte(_)
                | Self::Short(_)
                | Self::Integer(_)
                | Self::Long(_)
                | Self::Float(_)
                | Self::Double(_)
                | Self::String(_) => {
                    unreachable!("Checked by ResolvedExpression::try_into_snbt")
                }
            },
        }
    }

    pub fn assign_to_data_scale(
        self,
        datapack: &mut HighDatapack,
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
                        DataCommandModification::Value(snbt),
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
                            DataCommandModification::Value(SNBT::List(constants)),
                        )),
                    );

                    for (index, non_constant) in non_constants {
                        non_constant.assign_to_data(
                            datapack,
                            ctx,
                            target.clone(),
                            path.clone()
                                .with_node(NbtPathNode::Index(Some(SNBT::Integer(index as i32)))),
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
                            DataCommandModification::Value(SNBT::Compound(constants)),
                        )),
                    );

                    for (key, non_constant) in non_constants {
                        non_constant.assign_to_data_scale(
                            datapack,
                            ctx,
                            target.clone(),
                            path.clone().with_node(NbtPathNode::named(key.snbt_string)),
                            scale,
                        );
                    }
                }
                Self::Underscore => unreachable!(),
                Self::Struct(_, _, fields) => {
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
                Self::Tuple(expressions) => {
                    let (constants, non_constants) = split_constants_list(expressions);

                    ctx.add_command(
                        datapack,
                        Command::Data(DataCommand::Modify(
                            target.target.clone(),
                            path.clone(),
                            DataCommandModificationMode::Set,
                            DataCommandModification::Value(SNBT::List(constants)),
                        )),
                    );

                    for (index, non_constant) in non_constants {
                        non_constant.assign_to_data_scale(
                            datapack,
                            ctx,
                            target.clone(),
                            path.clone()
                                .with_node(NbtPathNode::Index(Some(SNBT::Integer(index as i32)))),
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
                Self::Unit
                | Self::Boolean(_)
                | Self::Byte(_)
                | Self::Short(_)
                | Self::Integer(_)
                | Self::Long(_)
                | Self::Float(_)
                | Self::Double(_)
                | Self::String(_) => {
                    unreachable!("Checked by ResolvedExpression::try_into_snbt")
                }
            },
        }
    }

    #[must_use]
    pub fn as_data(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        force: bool,
    ) -> (GeneratedDataTarget, NbtPath) {
        if !force && let Self::Data(target_path) = self {
            *target_path
        } else {
            let (unique_data, path) = datapack.get_unique_data();

            self.assign_to_data(datapack, ctx, unique_data.clone(), path.clone());

            (unique_data, path)
        }
    }

    #[must_use]
    pub fn as_data_scale(
        self,
        datapack: &mut HighDatapack,
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
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> DataCommandModification {
        match self.try_into_snbt() {
            Ok(snbt) => DataCommandModification::Value(snbt),
            Err(self_) => {
                let (target, path) = self_.as_data(datapack, ctx, false);

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
            Self::String(HighSNBTString {
                snbt_string: SNBTString(_, v),
                ..
            }) if force => v.len() as i32,
            Self::List(v) if force => v.len() as i32,
            Self::Compound(compound) if force => compound.len() as i32,
            _ => return None,
        })
    }

    pub fn operate_on_score(
        self,
        datapack: &mut HighDatapack,
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
        datapack: &mut HighDatapack,
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
        datapack: &mut HighDatapack,
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
    pub fn cast_to(self, data_type: DataTypeKind) -> Self {
        match (self, data_type) {
            (Self::Byte(value), DataTypeKind::Short) => Self::Short(i16::from(value)),
            (Self::Byte(value), DataTypeKind::Integer) => Self::Integer(i32::from(value)),
            (Self::Byte(value), DataTypeKind::Long) => Self::Long(i64::from(value)),
            (Self::Byte(value), DataTypeKind::Float) => {
                Self::Float(NotNan::new(f32::from(value)).unwrap())
            }
            (Self::Byte(value), DataTypeKind::Double) => {
                Self::Double(NotNan::new(f64::from(value)).unwrap())
            }

            (Self::Short(value), DataTypeKind::Byte) => Self::Byte(value as i8),
            (Self::Short(value), DataTypeKind::Integer) => Self::Integer(i32::from(value)),
            (Self::Short(value), DataTypeKind::Long) => Self::Long(i64::from(value)),
            (Self::Short(value), DataTypeKind::Float) => {
                Self::Float(NotNan::new(f32::from(value)).unwrap())
            }
            (Self::Short(value), DataTypeKind::Double) => {
                Self::Double(NotNan::new(f64::from(value)).unwrap())
            }

            (Self::Integer(value), DataTypeKind::Byte) => Self::Byte(value as i8),
            (Self::Integer(value), DataTypeKind::Short) => Self::Short(value as i16),
            (Self::Integer(value), DataTypeKind::Long) => Self::Long(i64::from(value)),
            (Self::Integer(value), DataTypeKind::Float) => {
                Self::Float(NotNan::new(value as f32).unwrap())
            }
            (Self::Integer(value), DataTypeKind::Double) => {
                Self::Double(NotNan::new(f64::from(value)).unwrap())
            }

            (Self::Long(value), DataTypeKind::Byte) => Self::Byte(value as i8),
            (Self::Long(value), DataTypeKind::Short) => Self::Short(value as i16),
            (Self::Long(value), DataTypeKind::Integer) => Self::Integer(value as i32),
            (Self::Long(value), DataTypeKind::Float) => {
                Self::Float(NotNan::new(value as f32).unwrap())
            }
            (Self::Long(value), DataTypeKind::Double) => {
                Self::Double(NotNan::new(value as f64).unwrap())
            }

            (Self::Float(value), DataTypeKind::Byte) => Self::Byte(value.into_inner() as i8),
            (Self::Float(value), DataTypeKind::Short) => Self::Short(value.into_inner() as i16),
            (Self::Float(value), DataTypeKind::Integer) => Self::Integer(value.into_inner() as i32),
            (Self::Float(value), DataTypeKind::Long) => Self::Long(value.into_inner() as i64),
            (Self::Float(value), DataTypeKind::Double) => Self::Double(value.into()),

            (Self::Double(value), DataTypeKind::Byte) => Self::Byte(value.into_inner() as i8),
            (Self::Double(value), DataTypeKind::Short) => Self::Short(value.into_inner() as i16),
            (Self::Double(value), DataTypeKind::Integer) => {
                Self::Integer(value.into_inner() as i32)
            }
            (Self::Double(value), DataTypeKind::Long) => Self::Long(value.into_inner() as i64),
            (Self::Double(value), DataTypeKind::Float) => {
                Self::Float(unsafe { NotNan::new_unchecked(value.into_inner() as f32) })
            }

            (self_ @ Self::Boolean(_), DataTypeKind::Boolean)
            | (self_ @ Self::Byte(_), DataTypeKind::Byte)
            | (self_ @ Self::Short(_), DataTypeKind::Short)
            | (self_ @ Self::Integer(_), DataTypeKind::Integer)
            | (self_ @ Self::Long(_), DataTypeKind::Long)
            | (self_ @ Self::Float(_), DataTypeKind::Float)
            | (self_ @ Self::Double(_), DataTypeKind::Double)
            | (self_ @ Self::Data(_), DataTypeKind::Data(_))
            | (self_ @ Self::PlayerScore(_), DataTypeKind::Score(_)) => self_,

            _ => unreachable!(""),
        }
    }

    pub fn to_execute_condition(
        self,
        datapack: &mut HighDatapack,
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
            Self::Byte(_)
            | Self::Short(_)
            | Self::Integer(_)
            | Self::Long(_)
            | Self::Float(_)
            | Self::Double(_)
            | Self::String(_)
            | Self::Underscore
            | Self::List(_)
            | Self::Compound(_)
            | Self::Tuple(_)
            | Self::Struct(_, _, _)
            | Self::Unit => return None,
        })
    }

    #[must_use]
    pub fn as_text_component(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        force_display: bool,
    ) -> SNBT {
        match self {
            Self::PlayerScore(player_score) => player_score.to_text_component(),
            Self::Data(target_path) => {
                let (target, path) = *target_path;

                let mut map = BTreeMap::new();

                match target.target {
                    DataTarget::Block(coordinates) => {
                        map.insert(
                            SNBTString(false, "block".to_string()),
                            SNBT::string(coordinates),
                        );
                    }
                    DataTarget::Entity(entity_selector) => {
                        map.insert(
                            SNBTString(false, "entity".to_string()),
                            SNBT::string(entity_selector),
                        );
                    }
                    DataTarget::Storage(resource_location) => {
                        map.insert(
                            SNBTString(false, "storage".to_string()),
                            SNBT::string(resource_location),
                        );
                    }
                }

                map.insert(SNBTString(false, "nbt".to_string()), path.to_snbt_string());

                SNBT::Compound(map)
            }
            Self::Boolean(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::Byte(i8::from(value))
                }
            }
            Self::Byte(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::Byte(value)
                }
            }
            Self::Short(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::Short(value)
                }
            }
            Self::Integer(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::Integer(value)
                }
            }
            Self::Long(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::Long(value)
                }
            }
            Self::Float(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::Float(value)
                }
            }
            Self::Double(value) => {
                if force_display {
                    SNBT::string(value)
                } else {
                    SNBT::Double(value)
                }
            }
            Self::String(string) => {
                if force_display {
                    SNBT::string(format!("\"{}\"", string.snbt_string.1))
                } else {
                    SNBT::String(string.snbt_string)
                }
            }
            Self::List(list) => SNBT::List(
                list.into_iter()
                    .map(|element| element.as_text_component(datapack, ctx, false))
                    .collect(),
            ),
            Self::Compound(compound) => SNBT::Compound(
                compound
                    .into_iter()
                    .map(|(key, value)| {
                        (
                            key.snbt_string,
                            value.as_text_component(datapack, ctx, false),
                        )
                    })
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

                SNBT::List(list)
            }
            Self::Unit => SNBT::string("()"),
            Self::Struct(name, generics, fields) => {
                if force_display {
                    let mut output = Vec::new();

                    output.push(SNBT::string(if generics.is_empty() {
                        if fields.is_empty() {
                            format!("{} {{", name)
                        } else {
                            format!("{} {{ ", name)
                        }
                    } else {
                        name
                    }));

                    if !generics.is_empty() {
                        output.push(SNBT::string("<"));

                        for (i, generic) in generics.into_iter().enumerate() {
                            if i != 0 {
                                output.push(SNBT::string(", "));
                            }

                            output.push(SNBT::string(format!("{}", generic)));
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

                    SNBT::List(output)
                } else {
                    let mut output = BTreeMap::new();

                    for (field_name, field_value) in fields {
                        output.insert(
                            SNBTString(false, field_name),
                            field_value.as_text_component(datapack, ctx, force_display),
                        );
                    }

                    SNBT::Compound(output)
                }
            }
            Self::Underscore => SNBT::string("_"),
        }
    }

    #[must_use]
    pub fn as_place(self) -> Place {
        match self {
            Self::PlayerScore(score) => Place::Score(score),
            Self::Data(target_path) => {
                let (target, path) = *target_path;

                Place::Data(target, path)
            }
            Self::Underscore => Place::Underscore,
            Self::Tuple(expressions) => {
                Place::Tuple(expressions.into_iter().map(Self::as_place).collect())
            }
            _ => unreachable!("This expression is not a place {:?}", self),
        }
    }

    #[must_use]
    pub const fn is_lvalue(&self) -> bool {
        match self {
            Self::PlayerScore(_) | Self::Data(_) => true,
            Self::Boolean(_)
            | Self::Byte(_)
            | Self::Short(_)
            | Self::Integer(_)
            | Self::Long(_)
            | Self::Float(_)
            | Self::Double(_)
            | Self::String(_)
            | Self::Underscore
            | Self::List(_)
            | Self::Compound(_)
            | Self::Tuple(_)
            | Self::Struct(_, _, _)
            | Self::Unit
            | Self::Condition(_, _) => false,
        }
    }

    pub fn compile_augmented_assignment(
        self,
        datapack: &mut HighDatapack,
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
            Self::Boolean(_)
            | Self::Byte(_)
            | Self::Short(_)
            | Self::Integer(_)
            | Self::Long(_)
            | Self::Float(_)
            | Self::Double(_)
            | Self::String(_)
            | Self::Underscore
            | Self::List(_)
            | Self::Compound(_)
            | Self::Tuple(_)
            | Self::Struct(_, _, _)
            | Self::Unit
            | Self::Condition(_, _) => unreachable!(),
        }
    }

    #[must_use]
    pub fn invert(self) -> Self {
        match self {
            Self::Boolean(value) => Self::Boolean(!value),
            Self::Condition(inverted, subcommand) => Self::Condition(!inverted, subcommand),
            _ => unreachable!("Cannot invert expression {:?}", self),
        }
    }

    #[must_use]
    pub fn negate(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> Self {
        match self {
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
            _ => unreachable!("Cannot negate expression {:?}", self),
        }
    }

    #[must_use]
    pub fn perform_logical_operation(
        self,
        datapack: &mut HighDatapack,
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
        datapack: &mut HighDatapack,
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

    pub fn assign_field(
        &mut self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        field: &str,
        value: Self,
    ) {
        match self {
            Self::Compound(compound) => {
                *compound
                    .iter_mut()
                    .find_map(|(actual_field, value)| {
                        if actual_field.snbt_string.1 == field {
                            Some(value)
                        } else {
                            None
                        }
                    })
                    .unwrap() = value;
            }
            Self::Data(target_path) => {
                let (target, path) = &mut **target_path;

                value.assign_to_data(
                    datapack,
                    ctx,
                    target.clone(),
                    path.clone().with_node(NbtPathNode::Named(
                        SNBTString(false, field.to_owned()),
                        None,
                    )),
                );
            }
            Self::Tuple(expressions) => {
                let field = field.parse::<i32>().unwrap();

                *expressions.get_mut(field as usize).unwrap() = value;
            }
            _ => unreachable!("{:?}", self),
        }
    }

    pub fn assign_index(
        &mut self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        index: Self,
        value: Self,
    ) {
        match self {
            Self::List(items) => {
                let Self::Integer(index) = index else {
                    unreachable!();
                };

                if index >= 0 && (index as usize) < items.len() {
                    *items.get_mut(index as usize).unwrap() = value;
                } else {
                    unreachable!("Index is out of range");
                }
            }
            Self::Data(target_path) => {
                let (target, path) = &mut **target_path;

                let index = index.as_snbt_macros(ctx);

                value.assign_to_data(
                    datapack,
                    ctx,
                    target.clone(),
                    path.clone().with_node(NbtPathNode::Index(Some(index))),
                );
            }
            _ => unreachable!("The expression cannot be indexed {:?}", self),
        }
    }
}
