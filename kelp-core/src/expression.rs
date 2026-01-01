use std::collections::BTreeMap;

use minecraft_command_types::{
    command::{
        Command, PlayerScore,
        data::{DataCommand, DataCommandModification, DataCommandModificationMode, DataTarget},
        enums::{score_operation_operator::ScoreOperationOperator, store_type::StoreType},
        execute::{
            ExecuteIfSubcommand, ExecuteStoreSubcommand, ExecuteSubcommand, ScoreComparison,
            ScoreComparisonOperator,
        },
        r#return::ReturnCommand,
        scoreboard::{PlayersScoreboardCommand, ScoreboardCommand},
    },
    has_macro::HasMacro,
    nbt_path::{NbtPath, NbtPathNode, SNBTCompound},
    range::IntegerRange,
    resource_location::ResourceLocation,
    snbt::{SNBT, SNBTString},
};
use minecraft_command_types_derive::HasMacro;
use ordered_float::NotNan;
use parser_rs::ParserRange;

use crate::{
    command::{
        HighCommand, HighPlayerScore, PlayerScoreExt, compile_bitwise_and_score,
        compile_bitwise_or_score, context::CompileContext, data::HighDataTarget,
        execute::HighExecuteIfSubcommand,
    },
    datapack::HighDatapack,
    nbt_path::HighNbtPath,
};

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum ArithmeticOperator {
    Add,
    Subtract,
    Multiply,
    FloorDivide,
    Modulo,
    And,
    Or,
    LeftShift,
    RightShift,
    Swap,
}

impl ArithmeticOperator {
    pub fn is_additive(self) -> bool {
        matches!(self, ArithmeticOperator::Add | ArithmeticOperator::Subtract)
    }

    pub fn into_scoreboard_players_operation_operator(self) -> Option<ScoreOperationOperator> {
        match self {
            ArithmeticOperator::Add => Some(ScoreOperationOperator::Add),
            ArithmeticOperator::Subtract => Some(ScoreOperationOperator::Subtract),
            ArithmeticOperator::Multiply => Some(ScoreOperationOperator::Multiply),
            ArithmeticOperator::FloorDivide => Some(ScoreOperationOperator::Divide),
            ArithmeticOperator::Modulo => Some(ScoreOperationOperator::Modulo),
            ArithmeticOperator::Swap => Some(ScoreOperationOperator::Swap),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum LogicalOperator {
    And,
    Or,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum ComparisonOperator {
    LessThan,
    LessThanOrEqualTo,
    GreaterThan,
    GreaterThanOrEqualTo,
    EqualTo,
    NotEqualTo,
}

impl ComparisonOperator {
    pub fn into_score_comparison_operator(self) -> ScoreComparisonOperator {
        match self {
            ComparisonOperator::LessThan => ScoreComparisonOperator::LessThan,
            ComparisonOperator::LessThanOrEqualTo => ScoreComparisonOperator::LessThanOrEqualTo,
            ComparisonOperator::GreaterThan => ScoreComparisonOperator::GreaterThan,
            ComparisonOperator::GreaterThanOrEqualTo => {
                ScoreComparisonOperator::GreaterThanOrEqualTo
            }
            ComparisonOperator::EqualTo | ComparisonOperator::NotEqualTo => {
                ScoreComparisonOperator::EqualTo
            }
        }
    }

    pub fn should_execute_if_be_inverted(&self) -> bool {
        matches!(self, ComparisonOperator::NotEqualTo)
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum UnaryOperator {
    Negate,
    Invert,
}

pub type ExpressionCompoundKind = BTreeMap<StringExpression, Expression>;
pub type ConstantExpressionCompoundKind = BTreeMap<SNBTString, ConstantExpression>;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum ConstantExpressionKind {
    // <byte>
    Byte(i8),
    ComputedByte(i8),
    // <short>
    Short(i16),
    ComputedShort(i16),
    // <integer>
    Integer(i32),
    ComputedInteger(i32),
    // <long>
    Long(i64),
    ComputedLong(i64),
    // <float>
    Float(NotNan<f32>),
    // <double>
    Double(NotNan<f64>),
    // "<string>"
    String(SNBTString),
    // [<expr>,...]
    List(Vec<ConstantExpression>),
    // {<key>:<value>,...}
    Compound(ConstantExpressionCompoundKind),
    // score <player> <objective>
    PlayerScore(PlayerScore),
    // <target> <path>
    Data(DataTarget, NbtPath),
    // <condition>
    Condition(bool, ExecuteIfSubcommand),
    // <command>
    Command(Command),
}

impl HasMacro for ConstantExpressionKind {
    fn has_macro(&self) -> bool {
        match self {
            ConstantExpressionKind::Byte(_) | ConstantExpressionKind::ComputedByte(_) => false,
            ConstantExpressionKind::Short(_) | ConstantExpressionKind::ComputedShort(_) => false,
            ConstantExpressionKind::Integer(_) | ConstantExpressionKind::ComputedInteger(_) => {
                false
            }
            ConstantExpressionKind::Long(_) | ConstantExpressionKind::ComputedLong(_) => false,
            ConstantExpressionKind::Float(_) => false,
            ConstantExpressionKind::Double(_) => false,
            ConstantExpressionKind::String(SNBTString(has_macro, _)) => *has_macro,
            ConstantExpressionKind::List(constant_expressions) => constant_expressions.has_macro(),
            ConstantExpressionKind::Compound(btree_map) => btree_map.has_macro(),
            ConstantExpressionKind::PlayerScore(player_score) => player_score.has_macro(),
            ConstantExpressionKind::Data(data_target, nbt_path) => {
                data_target.has_macro() || nbt_path.has_macro()
            }
            ConstantExpressionKind::Condition(_, execute_if_subcommand) => {
                execute_if_subcommand.has_macro()
            }
            ConstantExpressionKind::Command(command) => command.has_macro(),
        }
    }

    fn has_macro_conflict(&self) -> bool {
        match self {
            ConstantExpressionKind::Byte(_) | ConstantExpressionKind::ComputedByte(_) => false,
            ConstantExpressionKind::Short(_) | ConstantExpressionKind::ComputedShort(_) => false,
            ConstantExpressionKind::Integer(_) | ConstantExpressionKind::ComputedInteger(_) => {
                false
            }
            ConstantExpressionKind::Long(_) | ConstantExpressionKind::ComputedLong(_) => false,
            ConstantExpressionKind::Float(_) => false,
            ConstantExpressionKind::Double(_) => false,
            ConstantExpressionKind::String(SNBTString(false, value)) => value.contains("$("),
            ConstantExpressionKind::String(SNBTString(true, _)) => false,
            ConstantExpressionKind::List(constant_expressions) => {
                constant_expressions.has_macro_conflict()
            }
            ConstantExpressionKind::Compound(btree_map) => btree_map.has_macro_conflict(),
            ConstantExpressionKind::PlayerScore(player_score) => player_score.has_macro_conflict(),
            ConstantExpressionKind::Data(data_target, nbt_path) => {
                data_target.has_macro_conflict() || nbt_path.has_macro_conflict()
            }
            ConstantExpressionKind::Condition(_, execute_if_subcommand) => {
                execute_if_subcommand.has_macro_conflict()
            }
            ConstantExpressionKind::Command(command) => command.has_macro_conflict(),
        }
    }
}

pub enum AsSNBTResult {
    SNBT(SNBT),
    Macro(usize, Box<ConstantExpressionKind>),
}

impl ConstantExpressionKind {
    pub fn into_dummy_constant_expression(self) -> ConstantExpression {
        ConstantExpression {
            span: ParserRange::default(),
            kind: self,
        }
    }

    pub fn as_assignable(self) -> Option<Assignable> {
        match self {
            ConstantExpressionKind::Data(target, path) => Some(Assignable::Data(target, path)),
            ConstantExpressionKind::PlayerScore(score) => Some(Assignable::PlayerScore(score)),
            _ => None,
        }
    }

    pub fn as_assignable_or_unique_score(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> Assignable {
        match self {
            ConstantExpressionKind::Data(target, path) => Assignable::Data(target, path),
            ConstantExpressionKind::PlayerScore(score) => Assignable::PlayerScore(score),
            _ => {
                let unique_score = datapack.get_unique_player_score();

                self.assign_to_score(datapack, ctx, unique_score.clone());

                Assignable::PlayerScore(unique_score)
            }
        }
    }

    pub fn as_assignable_or_unique_data(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> Assignable {
        match self {
            ConstantExpressionKind::Data(target, path) => Assignable::Data(target, path),
            ConstantExpressionKind::PlayerScore(score) => Assignable::PlayerScore(score),
            _ => {
                let (unique_target, unique_path) = datapack.get_unique_data();

                self.assign_to_data(datapack, ctx, unique_target.clone(), unique_path.clone());

                Assignable::Data(unique_target, unique_path)
            }
        }
    }

    fn try_as_i32(&self, force: bool) -> Option<i32> {
        match self {
            ConstantExpressionKind::Byte(v) | ConstantExpressionKind::ComputedByte(v) => {
                Some(*v as i32)
            }
            ConstantExpressionKind::Short(v) | ConstantExpressionKind::ComputedShort(v) => {
                Some(*v as i32)
            }
            ConstantExpressionKind::Integer(v) | ConstantExpressionKind::ComputedInteger(v) => {
                Some(*v)
            }
            ConstantExpressionKind::Long(v) | ConstantExpressionKind::ComputedLong(v) => {
                Some(*v as i32)
            }
            ConstantExpressionKind::Float(v) if force => Some(v.into_inner() as i32),
            ConstantExpressionKind::Double(v) if force => Some(v.into_inner() as i32),
            ConstantExpressionKind::String(SNBTString(_, v)) if force => Some(v.len() as i32),
            ConstantExpressionKind::List(v) if force => Some(v.len() as i32),
            ConstantExpressionKind::Compound(compound) if force => Some(compound.len() as i32),
            _ => None,
        }
    }

    pub fn compile_as_statement(&self, datapack: &mut HighDatapack, ctx: &mut CompileContext) {
        match self {
            ConstantExpressionKind::List(constant_expressions) => {
                for constant_expression in constant_expressions {
                    constant_expression.kind.compile_as_statement(datapack, ctx);
                }
            }
            ConstantExpressionKind::Compound(compound) => {
                for value in compound.values() {
                    value.kind.compile_as_statement(datapack, ctx);
                }
            }
            ConstantExpressionKind::Condition(inverted, condition) => {
                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::If(*inverted, condition.clone())),
                );
            }
            ConstantExpressionKind::Command(command) => {
                ctx.add_command(datapack, command.clone());
            }
            ConstantExpressionKind::Byte(_)
            | ConstantExpressionKind::ComputedByte(_)
            | ConstantExpressionKind::Short(_)
            | ConstantExpressionKind::ComputedShort(_)
            | ConstantExpressionKind::Integer(_)
            | ConstantExpressionKind::ComputedInteger(_)
            | ConstantExpressionKind::Long(_)
            | ConstantExpressionKind::ComputedLong(_)
            | ConstantExpressionKind::Float(_)
            | ConstantExpressionKind::Double(_)
            | ConstantExpressionKind::String(_)
            | ConstantExpressionKind::PlayerScore(_)
            | ConstantExpressionKind::Data(_, _) => {}
        }
    }

    #[must_use]
    pub fn as_score(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        force: bool,
    ) -> PlayerScore {
        if let Some(value) = self.try_as_i32(force) {
            return if force {
                let unique_score = datapack.get_unique_player_score();
                self.assign_to_score(datapack, ctx, unique_score.clone());
                unique_score
            } else {
                datapack.get_constant_score(value)
            };
        }

        match self {
            ConstantExpressionKind::PlayerScore(player_score) if !force => player_score.clone(),
            _ => {
                let unique_score = datapack.get_unique_player_score();
                self.assign_to_score(datapack, ctx, unique_score.clone());
                unique_score
            }
        }
    }

    #[must_use]
    pub fn as_data(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> (DataTarget, NbtPath) {
        match self {
            ConstantExpressionKind::Data(target, path) => (target.clone(), path.clone()),
            _ => {
                let (unique_data, path) = datapack.get_unique_data();

                self.assign_to_data(datapack, ctx, unique_data.clone(), path.clone());

                (unique_data, path)
            }
        }
    }

    #[must_use]
    pub fn as_data_force(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> (DataTarget, NbtPath) {
        let (unique_data, path) = datapack.get_unique_data();

        self.assign_to_data(datapack, ctx, unique_data.clone(), path.clone());

        (unique_data, path)
    }

    pub fn index(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        index: ConstantExpression,
    ) -> ConstantExpressionKind {
        let index = index.kind.as_snbt_macros(ctx);

        let (self_data_target, self_path) = self.as_data(datapack, ctx);

        ConstantExpressionKind::Data(
            self_data_target,
            self_path.with_node(NbtPathNode::Index(Some(index))),
        )
    }

    pub fn access_member(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        member: SNBTString,
    ) -> ConstantExpressionKind {
        let (self_data_target, self_path) = self.as_data(datapack, ctx);

        ConstantExpressionKind::Data(
            self_data_target,
            self_path.with_node(NbtPathNode::Named(member, None)),
        )
    }

    #[inline]
    pub fn is_data(&self) -> bool {
        matches!(self, ConstantExpressionKind::Data(_, _))
    }

    pub fn as_data_command_modification(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> DataCommandModification {
        if let Some(snbt) = self.as_snbt() {
            return DataCommandModification::Value(snbt);
        }

        let (target, path) = self.as_data(datapack, ctx);

        DataCommandModification::From(target, Some(path))
    }

    pub fn compare(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        other: ConstantExpressionKind,
        operator: ComparisonOperator,
    ) -> ConstantExpressionKind {
        let mut create_data_compare =
            |data_expr: &ConstantExpressionKind, val_expr: ConstantExpressionKind| {
                let unique_score = datapack.get_unique_player_score();

                let (target, path) = data_expr.as_data_force(datapack, ctx);

                let data_command_modification =
                    val_expr.as_data_command_modification(datapack, ctx);

                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Success,
                        ExecuteStoreSubcommand::Score(
                            unique_score.clone(),
                            Box::new(ExecuteSubcommand::Run(Box::new(Command::Data(
                                DataCommand::Modify(
                                    target,
                                    path,
                                    DataCommandModificationMode::Set,
                                    data_command_modification,
                                ),
                            )))),
                        ),
                    )),
                );

                ConstantExpressionKind::Condition(
                    !operator.should_execute_if_be_inverted(),
                    ExecuteIfSubcommand::Score(
                        unique_score,
                        ScoreComparison::Range(IntegerRange::new_single(1)),
                        None,
                    ),
                )
            };

        if operator == ComparisonOperator::EqualTo || operator == ComparisonOperator::NotEqualTo {
            if other.is_data() {
                return create_data_compare(self, other);
            }

            if self.is_data() {
                return create_data_compare(&other, self.clone());
            }
        }

        let mut check_score_range = |val: Option<i32>, score_provider: &ConstantExpressionKind| {
            val.map(|integer| {
                let score = score_provider.as_score(datapack, ctx, false);
                ConstantExpressionKind::Condition(
                    operator.should_execute_if_be_inverted(),
                    ExecuteIfSubcommand::Score(
                        score,
                        ScoreComparison::Range(IntegerRange::new_single(integer)),
                        None,
                    ),
                )
            })
        };

        if operator == ComparisonOperator::EqualTo || operator == ComparisonOperator::NotEqualTo {
            if let Some(res) = check_score_range(self.try_as_i32(false), &other) {
                return res;
            }

            if let Some(res) = check_score_range(other.try_as_i32(false), self) {
                return res;
            }
        }

        let self_score = self.as_score(datapack, ctx, false);
        let other_score = other.as_score(datapack, ctx, false);

        ConstantExpressionKind::Condition(
            operator.should_execute_if_be_inverted(),
            ExecuteIfSubcommand::Score(
                self_score,
                ScoreComparison::Score(operator.into_score_comparison_operator(), other_score),
                None,
            ),
        )
    }

    pub fn operate_on_score(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: &PlayerScore,
        operator: ArithmeticOperator,
    ) {
        if let Some(value) = self.try_as_i32(true) {
            return match operator {
                ArithmeticOperator::Add => {
                    Self::push_scoreboard_players(
                        datapack,
                        ctx,
                        PlayersScoreboardCommand::Add(target.clone(), value),
                    );
                }
                ArithmeticOperator::Subtract => {
                    Self::push_scoreboard_players(
                        datapack,
                        ctx,
                        PlayersScoreboardCommand::Remove(target.clone(), value),
                    );
                }
                ArithmeticOperator::And => {
                    let constant_score = datapack.get_constant_score(value);
                    compile_bitwise_and_score(datapack, ctx, target, &constant_score);
                }
                ArithmeticOperator::Or => {
                    let constant_score = datapack.get_constant_score(value);
                    compile_bitwise_or_score(datapack, ctx, target, &constant_score);
                }
                ArithmeticOperator::LeftShift => {
                    compile_shift_operation(
                        datapack,
                        ctx,
                        target,
                        value,
                        ScoreOperationOperator::Multiply,
                    );
                }
                ArithmeticOperator::RightShift => {
                    compile_shift_operation(
                        datapack,
                        ctx,
                        target,
                        value,
                        ScoreOperationOperator::Divide,
                    );
                }
                _ => {
                    let constant_score = datapack.get_constant_score(value);

                    Self::push_scoreboard_players(
                        datapack,
                        ctx,
                        PlayersScoreboardCommand::Operation(
                            target.clone(),
                            operator
                                .into_scoreboard_players_operation_operator()
                                .unwrap(),
                            constant_score,
                        ),
                    );
                }
            };
        }

        match self {
            ConstantExpressionKind::PlayerScore(source) => {
                source
                    .clone()
                    .operate_on_score(datapack, ctx, target.clone(), operator);
            }
            _ => {
                let unique_score = datapack.get_unique_player_score();

                self.assign_to_score(datapack, ctx, unique_score.clone());

                unique_score
                    .clone()
                    .operate_on_score(datapack, ctx, target.clone(), operator);
            }
        }
    }

    pub fn to_execute_condition(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        inverted: bool,
    ) -> (bool, ExecuteIfSubcommand) {
        if let Some(value) = self.try_as_i32(true) {
            let unique_score = datapack.get_unique_player_score();

            Self::push_scoreboard_players(
                datapack,
                ctx,
                PlayersScoreboardCommand::Set(unique_score.clone(), value),
            );

            return (
                !inverted,
                ExecuteIfSubcommand::Score(
                    unique_score,
                    ScoreComparison::Range(IntegerRange::new_single(0)),
                    None,
                ),
            );
        }

        match self {
            ConstantExpressionKind::PlayerScore(score) => (
                !inverted,
                ExecuteIfSubcommand::Score(
                    score.clone(),
                    ScoreComparison::Range(IntegerRange::new_single(0)),
                    None,
                ),
            ),
            ConstantExpressionKind::Data(target, path) => (
                inverted,
                ExecuteIfSubcommand::Data(target.clone(), path.clone(), None),
            ),
            ConstantExpressionKind::Condition(inner_inverted, condition) => {
                (inverted ^ *inner_inverted, condition.clone())
            }
            ConstantExpressionKind::Command(_) => {
                let unique_score = datapack.get_unique_player_score();

                self.assign_to_score(datapack, ctx, unique_score.clone());

                (
                    !inverted,
                    ExecuteIfSubcommand::Score(
                        unique_score,
                        ScoreComparison::Range(IntegerRange::new_single(0)),
                        None,
                    ),
                )
            }
            ConstantExpressionKind::Byte(_)
            | ConstantExpressionKind::ComputedByte(_)
            | ConstantExpressionKind::Short(_)
            | ConstantExpressionKind::ComputedShort(_)
            | ConstantExpressionKind::Integer(_)
            | ConstantExpressionKind::ComputedInteger(_)
            | ConstantExpressionKind::Long(_)
            | ConstantExpressionKind::ComputedLong(_)
            | ConstantExpressionKind::Float(_)
            | ConstantExpressionKind::Double(_)
            | ConstantExpressionKind::String(_)
            | ConstantExpressionKind::List(_)
            | ConstantExpressionKind::Compound(_) => {
                unreachable!("Handled by try_as_i32")
            }
        }
    }

    pub fn as_text_component(&self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> SNBT {
        match self {
            ConstantExpressionKind::PlayerScore(player_score) => {
                player_score.clone().to_text_component()
            }
            ConstantExpressionKind::Data(target, path) => {
                let mut map = BTreeMap::new();

                match target {
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
            ConstantExpressionKind::Byte(value) => SNBT::Byte(*value),
            ConstantExpressionKind::ComputedByte(value) => SNBT::string(value),
            ConstantExpressionKind::Short(value) => SNBT::Short(*value),
            ConstantExpressionKind::ComputedShort(value) => SNBT::string(value),
            ConstantExpressionKind::Integer(value) => SNBT::Integer(*value),
            ConstantExpressionKind::ComputedInteger(value) => SNBT::string(value),
            ConstantExpressionKind::Long(value) => SNBT::Long(*value),
            ConstantExpressionKind::ComputedLong(value) => SNBT::string(value),
            ConstantExpressionKind::Float(value) => SNBT::Float(*value),
            ConstantExpressionKind::Double(value) => SNBT::Double(*value),
            ConstantExpressionKind::String(snbt_string) => SNBT::String(snbt_string.clone()),
            ConstantExpressionKind::List(constant_expressions) => SNBT::List(
                constant_expressions
                    .iter()
                    .map(|expression| expression.kind.as_text_component(datapack, ctx))
                    .collect(),
            ),
            ConstantExpressionKind::Compound(btree_map) => SNBT::Compound(
                btree_map
                    .iter()
                    .map(|(key, value)| (key.clone(), value.kind.as_text_component(datapack, ctx)))
                    .collect(),
            ),
            ConstantExpressionKind::Condition(_, _) => {
                let unique_ecore = datapack.get_unique_player_score();

                self.assign_to_score(datapack, ctx, unique_ecore.clone());

                unique_ecore.clone().to_text_component()
            }
            ConstantExpressionKind::Command(_) => {
                let unique_score = datapack.get_unique_player_score();

                self.assign_to_score(datapack, ctx, unique_score.clone());

                unique_score.clone().to_text_component()
            }
        }
    }

    fn push_scoreboard_players(
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        command: PlayersScoreboardCommand,
    ) {
        ctx.add_command(
            datapack,
            Command::Scoreboard(ScoreboardCommand::Players(command)),
        );
    }

    pub fn assign_to_score(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: PlayerScore,
    ) {
        match self {
            ConstantExpressionKind::Byte(value) | ConstantExpressionKind::ComputedByte(value) => {
                Self::push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, *value as i32),
                );
            }
            ConstantExpressionKind::Short(value) | ConstantExpressionKind::ComputedShort(value) => {
                Self::push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, *value as i32),
                );
            }
            ConstantExpressionKind::Integer(value)
            | ConstantExpressionKind::ComputedInteger(value) => {
                Self::push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, *value),
                );
            }
            ConstantExpressionKind::Long(value) | ConstantExpressionKind::ComputedLong(value) => {
                Self::push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, *value as i32),
                );
            }
            ConstantExpressionKind::Float(value) => {
                Self::push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, value.into_inner() as i32),
                );
            }
            ConstantExpressionKind::Double(value) => {
                Self::push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, value.into_inner() as i32),
                );
            }
            ConstantExpressionKind::String(SNBTString(_, value)) => {
                Self::push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, value.len() as i32),
                );
            }
            ConstantExpressionKind::List(value) => {
                Self::push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, value.len() as i32),
                );
            }
            ConstantExpressionKind::Compound(value) => {
                Self::push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target, value.len() as i32),
                );
            }
            ConstantExpressionKind::PlayerScore(source) => {
                source.clone().assign_to_score(datapack, ctx, target);
            }
            ConstantExpressionKind::Data(data_target, path) => {
                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Result,
                        ExecuteStoreSubcommand::Score(
                            target,
                            Box::new(ExecuteSubcommand::Run(Box::new(Command::Data(
                                DataCommand::Get(data_target.clone(), Some(path.clone()), None),
                            )))),
                        ),
                    )),
                );
            }
            ConstantExpressionKind::Condition(inverted, condition) => {
                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Success,
                        ExecuteStoreSubcommand::Score(
                            target,
                            Box::new(ExecuteSubcommand::If(*inverted, condition.clone())),
                        ),
                    )),
                );
            }
            ConstantExpressionKind::Command(command) => {
                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Result,
                        ExecuteStoreSubcommand::Score(
                            target,
                            Box::new(ExecuteSubcommand::Run(Box::new(command.clone()))),
                        ),
                    )),
                );
            }
        }
    }

    pub fn assign_to_data(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: DataTarget,
        path: NbtPath,
    ) {
        if let Some(value) = self.as_snbt() {
            ctx.add_command(
                datapack,
                Command::Data(DataCommand::Modify(
                    target,
                    path,
                    DataCommandModificationMode::Set,
                    DataCommandModification::Value(value),
                )),
            );

            return;
        }

        match self {
            ConstantExpressionKind::PlayerScore(score) => {
                score.clone().assign_to_data(datapack, ctx, target, path);
            }
            ConstantExpressionKind::Data(inner_target, inner_path) => {
                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Modify(
                        target,
                        path,
                        DataCommandModificationMode::Set,
                        DataCommandModification::From(
                            inner_target.clone(),
                            Some(inner_path.clone()),
                        ),
                    )),
                );
            }
            ConstantExpressionKind::List(list) => {
                let (constants, non_constants) = split_constants_list(list);

                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Modify(
                        target.clone(),
                        path.clone(),
                        DataCommandModificationMode::Set,
                        DataCommandModification::Value(SNBT::List(constants)),
                    )),
                );

                for (index, non_constant) in non_constants {
                    non_constant.kind.assign_to_data(
                        datapack,
                        ctx,
                        target.clone(),
                        path.clone()
                            .with_node(NbtPathNode::Index(Some(SNBT::Integer(index as i32)))),
                    );
                }
            }
            ConstantExpressionKind::Compound(compound) => {
                let (constants, non_constants) = split_constants_compound(compound);

                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Modify(
                        target.clone(),
                        path.clone(),
                        DataCommandModificationMode::Set,
                        DataCommandModification::Value(SNBT::Compound(constants)),
                    )),
                );

                for (key, non_constant) in non_constants {
                    non_constant.kind.assign_to_data(
                        datapack,
                        ctx,
                        target.clone(),
                        path.clone().with_node(NbtPathNode::named(key)),
                    );
                }
            }
            _ => unreachable!("Cannot assign to data: {:?}", self),
        }
    }

    pub fn as_snbt_macros(self, ctx: &mut CompileContext) -> SNBT {
        match self {
            ConstantExpressionKind::Byte(value) | ConstantExpressionKind::ComputedByte(value) => {
                SNBT::Byte(value)
            }
            ConstantExpressionKind::Short(value) | ConstantExpressionKind::ComputedShort(value) => {
                SNBT::Short(value)
            }
            ConstantExpressionKind::Integer(value)
            | ConstantExpressionKind::ComputedInteger(value) => SNBT::Integer(value),
            ConstantExpressionKind::Long(value) | ConstantExpressionKind::ComputedLong(value) => {
                SNBT::Long(value)
            }
            ConstantExpressionKind::Float(value) => SNBT::Float(value),
            ConstantExpressionKind::Double(value) => SNBT::Double(value),
            ConstantExpressionKind::String(snbt_string) => SNBT::String(snbt_string),
            ConstantExpressionKind::List(expressions) => SNBT::List(
                expressions
                    .clone()
                    .into_iter()
                    .map(|expression| expression.kind.as_snbt_macros(ctx))
                    .collect(),
            ),
            ConstantExpressionKind::Compound(compound) => SNBT::Compound(
                compound
                    .clone()
                    .into_iter()
                    .map(|(key, value)| (key, value.kind.as_snbt_macros(ctx)))
                    .collect(),
            ),
            _ => ctx.get_macro_snbt(self),
        }
    }

    pub fn as_snbt(&self) -> Option<SNBT> {
        match self {
            ConstantExpressionKind::Byte(value) | ConstantExpressionKind::ComputedByte(value) => {
                Some(SNBT::Byte(*value))
            }
            ConstantExpressionKind::Short(value) | ConstantExpressionKind::ComputedShort(value) => {
                Some(SNBT::Short(*value))
            }
            ConstantExpressionKind::Integer(value)
            | ConstantExpressionKind::ComputedInteger(value) => Some(SNBT::Integer(*value)),
            ConstantExpressionKind::Long(value) | ConstantExpressionKind::ComputedLong(value) => {
                Some(SNBT::Long(*value))
            }
            ConstantExpressionKind::Float(value) => Some(SNBT::Float(*value)),
            ConstantExpressionKind::Double(value) => Some(SNBT::Double(*value)),
            ConstantExpressionKind::String(snbt_string) => Some(SNBT::String(snbt_string.clone())),

            ConstantExpressionKind::List(expressions) => expressions
                .iter()
                .map(|expr| expr.kind.as_snbt())
                .collect::<Option<Vec<_>>>()
                .map(SNBT::List),

            ConstantExpressionKind::Compound(compound) => compound
                .iter()
                .map(|(key, value)| value.kind.as_snbt().map(|v| (key.clone(), v)))
                .collect::<Option<_>>()
                .map(SNBT::Compound),

            _ => None,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct ConstantExpression {
    #[has_macro(ignore)]
    pub span: ParserRange,
    pub kind: ConstantExpressionKind,
}

impl ConstantExpression {
    pub fn into_constant_expression(self) -> Expression {
        Expression {
            span: self.span,
            kind: ExpressionKind::Constant(self.kind),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum StringExpressionPart {
    Regular(String),
    Expression(Box<Expression>),
}

impl HasMacro for StringExpressionPart {
    fn has_macro(&self) -> bool {
        matches!(self, StringExpressionPart::Expression(_))
    }

    fn has_macro_conflict(&self) -> bool {
        false
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum StringExpression {
    Simple(String),
    Complex(Vec<StringExpressionPart>),
}

impl HasMacro for StringExpression {
    fn has_macro(&self) -> bool {
        match self {
            StringExpression::Simple(_) => false,
            StringExpression::Complex(parts) => parts.iter().any(|part| part.has_macro()),
        }
    }

    fn has_macro_conflict(&self) -> bool {
        false
    }
}

impl From<String> for StringExpression {
    fn from(value: String) -> Self {
        Self::Simple(value)
    }
}

impl StringExpression {
    pub fn compile(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> SNBTString {
        match self {
            StringExpression::Simple(contents) => SNBTString(false, contents),
            StringExpression::Complex(parts) => {
                let mut has_macro = false;
                let mut output = String::new();

                for part in parts {
                    match part {
                        StringExpressionPart::Regular(s) => output.push_str(&s),
                        StringExpressionPart::Expression(expression) => {
                            let expr = expression.resolve(datapack, ctx).kind.as_snbt_macros(ctx);

                            output.push_str(&expr.to_string());

                            has_macro = true;
                        }
                    }
                }

                SNBTString(has_macro, output)
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum ExpressionKind {
    Constant(ConstantExpressionKind),
    // <operator><expr>
    Unary(UnaryOperator, Box<Expression>),
    // <expr><arithmetic operator><expr>
    Arithmetic(Box<Expression>, ArithmeticOperator, Box<Expression>),
    // <expr><comparison operator><expr>
    Comparison(Box<Expression>, ComparisonOperator, Box<Expression>),
    // <expr><logical operator><expr>
    Logical(Box<Expression>, LogicalOperator, Box<Expression>),
    // <identifier>
    Variable(String),
    // <expr> <operator>= <expr>
    AugmentedAssignment(Box<Expression>, ArithmeticOperator, Box<Expression>),
    // <expr> = <expr>
    Assignment(Box<Expression>, Box<Expression>),
    // "<contents>"
    String(StringExpression),
    // [<expr>,...]
    List(Vec<Expression>),
    // {<key>:<value>,...}
    Compound(ExpressionCompoundKind),
    // score <player> <objective>
    PlayerScore(HighPlayerScore),
    // <target> <path>
    Data(HighDataTarget, HighNbtPath),
    // <condition>
    Condition(bool, HighExecuteIfSubcommand),
    // <command>
    Command(Box<HighCommand>),
    // <expr>[<expr>]
    Index(Box<Expression>, Box<Expression>),
    // <expr>.<expr>
    Member(Box<Expression>, StringExpression),
    // [B;<value>,...]
    // TODO ByteArray(Vec<i8>),
    // [I;<value>,...]
    // TODO IntegerArray(Vec<i32>),
    // [L;<value>,...]
    // TODO LongArray(Vec<i64>),
}

impl ExpressionKind {
    pub fn into_dummy_expression(self) -> Expression {
        Expression {
            span: ParserRange::default(),
            kind: self,
        }
    }

    pub fn eval_constant(&self, datapack: &mut HighDatapack) -> Option<ConstantExpressionKind> {
        match self {
            ExpressionKind::Constant(c) => Some(c.clone()),

            ExpressionKind::Unary(op, expr) => {
                let inner = expr.kind.eval_constant(datapack)?;

                match op {
                    UnaryOperator::Negate => {
                        if let Some(val) = inner.try_as_i32(false) {
                            return Some(ConstantExpressionKind::ComputedInteger(
                                val.wrapping_neg(),
                            ));
                        }
                        None
                    }
                    UnaryOperator::Invert => None,
                }
            }

            ExpressionKind::Arithmetic(lhs, op, rhs) => {
                let left = lhs.kind.eval_constant(datapack)?;
                let right = rhs.kind.eval_constant(datapack)?;

                if let (Some(l), Some(r)) = (left.try_as_i32(false), right.try_as_i32(false)) {
                    return match op {
                        ArithmeticOperator::Add => {
                            Some(ConstantExpressionKind::ComputedInteger(l.wrapping_add(r)))
                        }
                        ArithmeticOperator::Subtract => {
                            Some(ConstantExpressionKind::ComputedInteger(l.wrapping_sub(r)))
                        }
                        ArithmeticOperator::Multiply => {
                            Some(ConstantExpressionKind::ComputedInteger(l.wrapping_mul(r)))
                        }
                        ArithmeticOperator::FloorDivide => {
                            if r == 0 {
                                None
                            } else {
                                Some(ConstantExpressionKind::ComputedInteger(l / r))
                            }
                        }
                        ArithmeticOperator::Modulo => {
                            if r == 0 {
                                None
                            } else {
                                Some(ConstantExpressionKind::ComputedInteger(l % r))
                            }
                        }
                        ArithmeticOperator::And => {
                            Some(ConstantExpressionKind::ComputedInteger(l & r))
                        }
                        ArithmeticOperator::Or => {
                            Some(ConstantExpressionKind::ComputedInteger(l | r))
                        }
                        ArithmeticOperator::LeftShift => {
                            Some(ConstantExpressionKind::ComputedInteger(l << r))
                        }
                        ArithmeticOperator::RightShift => {
                            Some(ConstantExpressionKind::ComputedInteger(l >> r))
                        }
                        _ => None,
                    };
                }

                None
            }
            ExpressionKind::String(StringExpression::Simple(s)) => {
                Some(ConstantExpressionKind::String(SNBTString(false, s.clone())))
            }

            ExpressionKind::List(exprs) => {
                let mut constants = Vec::new();
                for e in exprs {
                    constants.push(
                        e.kind
                            .eval_constant(datapack)?
                            .into_dummy_constant_expression(),
                    );
                }
                Some(ConstantExpressionKind::List(constants))
            }

            ExpressionKind::Compound(_) => None,

            ExpressionKind::Variable(name) => datapack.get_variable(name).map(|value| value.kind),

            _ => None,
        }
    }
}

pub fn split_constants_list(
    list: &[ConstantExpression],
) -> (Vec<SNBT>, Vec<(usize, ConstantExpression)>) {
    let mut constants = Vec::new();
    let mut non_constants = Vec::new();

    for (i, expression) in list.iter().enumerate() {
        if let Some(snbt) = expression.kind.as_snbt() {
            constants.push(snbt);
        } else {
            non_constants.push((i, expression.clone()));
            constants.push(SNBT::Compound(BTreeMap::new()));
        }
    }

    (constants, non_constants)
}

pub fn split_constants_compound(
    compound: &BTreeMap<SNBTString, ConstantExpression>,
) -> (SNBTCompound, ConstantExpressionCompoundKind) {
    let mut constants = BTreeMap::new();
    let mut non_constants = BTreeMap::new();

    for (key, expression) in compound.iter() {
        if let Some(snbt) = expression.kind.as_snbt() {
            constants.insert(key.clone(), snbt);
        } else {
            non_constants.insert(key.clone(), expression.clone());
            constants.insert(key.clone(), SNBT::Compound(BTreeMap::new()));
        }
    }

    (constants, non_constants)
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum Assignable {
    PlayerScore(PlayerScore),
    Data(DataTarget, NbtPath),
}

impl Assignable {
    pub fn operate(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        operator: ArithmeticOperator,
        value: &ConstantExpression,
    ) {
        match self {
            Assignable::PlayerScore(player_score) => {
                let unique_score = value.kind.as_score(datapack, ctx, false);

                player_score.operate_on_score(datapack, ctx, unique_score, operator);
            }
            Assignable::Data(data_target, nbt_path) => {
                let (unique_target, unique_path) = datapack.get_unique_data();
                let (other_target, other_path) = value.kind.as_data(datapack, ctx);

                // C = A
                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Modify(
                        unique_target.clone(),
                        unique_path.clone(),
                        DataCommandModificationMode::Set,
                        DataCommandModification::From(data_target.clone(), Some(nbt_path.clone())),
                    )),
                );

                // A = B
                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Modify(
                        data_target,
                        nbt_path,
                        DataCommandModificationMode::Set,
                        DataCommandModification::From(
                            other_target.clone(),
                            Some(other_path.clone()),
                        ),
                    )),
                );

                // B = C
                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Modify(
                        other_target,
                        other_path,
                        DataCommandModificationMode::Set,
                        DataCommandModification::From(unique_target, Some(unique_path)),
                    )),
                );
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct Expression {
    #[has_macro(ignore)]
    pub span: ParserRange,
    pub kind: ExpressionKind,
}

impl Expression {
    pub fn compile_as_statement(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) {
        match self.kind {
            ExpressionKind::Command(command) => {
                let compiled_command = command.compile(datapack, ctx);

                if let Some(command) = compiled_command {
                    ctx.add_command(datapack, command);
                }
            }
            _ => self
                .resolve(datapack, ctx)
                .kind
                .compile_as_statement(datapack, ctx),
        }
    }

    pub fn assign_to_data(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: DataTarget,
        path: NbtPath,
    ) {
        println!("{:?}", self);

        let resolved = self.resolve(datapack, ctx);

        resolved.kind.assign_to_data(datapack, ctx, target, path);
    }

    pub fn resolve(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> ConstantExpression {
        if let Some(folded) = self.kind.eval_constant(datapack) {
            return folded.into_dummy_constant_expression();
        }

        match self.kind {
            ExpressionKind::Constant(constant_expression_kind) => constant_expression_kind
                .clone()
                .into_dummy_constant_expression(),
            ExpressionKind::Unary(unary_operator, expression) => match unary_operator {
                UnaryOperator::Negate => {
                    let resolved_expression = expression.resolve(datapack, ctx);
                    let new_score = resolved_expression.kind.as_score(datapack, ctx, true);
                    let constant_score = datapack.get_constant_score(-1);

                    ctx.add_command(
                        datapack,
                        Command::Scoreboard(ScoreboardCommand::Players(
                            PlayersScoreboardCommand::Operation(
                                new_score.clone(),
                                ScoreOperationOperator::Multiply,
                                constant_score,
                            ),
                        )),
                    );

                    ConstantExpressionKind::PlayerScore(new_score).into_dummy_constant_expression()
                }
                UnaryOperator::Invert => {
                    let (inverted, condition) = expression
                        .resolve(datapack, ctx)
                        .kind
                        .to_execute_condition(datapack, ctx, true);

                    ConstantExpressionKind::Condition(inverted, condition)
                        .into_dummy_constant_expression()
                }
            },
            ExpressionKind::Arithmetic(_, _, _) => {
                let unique_player_score = datapack.get_unique_player_score();

                self.resolve_into_score(datapack, ctx, &unique_player_score);

                ConstantExpressionKind::PlayerScore(unique_player_score)
                    .into_dummy_constant_expression()
            }
            ExpressionKind::Comparison(left, operator, right) => {
                let left = left.resolve(datapack, ctx);
                let right = right.resolve(datapack, ctx);

                left.kind
                    .compare(datapack, ctx, right.kind, operator)
                    .into_dummy_constant_expression()
            }
            ExpressionKind::Logical(left, operator, right) => {
                let left = left.resolve(datapack, ctx);
                let right = right.resolve(datapack, ctx);

                match operator {
                    LogicalOperator::And => {
                        let unique_score = datapack.get_unique_player_score();

                        let (left_inverted, left_condition) =
                            left.kind.to_execute_condition(datapack, ctx, false);
                        let (right_inverted, right_condition) =
                            right.kind.to_execute_condition(datapack, ctx, false);

                        ctx.add_command(
                            datapack,
                            Command::Scoreboard(ScoreboardCommand::Players(
                                PlayersScoreboardCommand::Set(unique_score.clone(), 0),
                            )),
                        );
                        ConstantExpressionKind::Condition(
                            left_inverted,
                            left_condition.then(ExecuteSubcommand::If(
                                right_inverted,
                                right_condition.then(ExecuteSubcommand::Run(Box::new(
                                    Command::Scoreboard(ScoreboardCommand::Players(
                                        PlayersScoreboardCommand::Set(unique_score.clone(), 1),
                                    )),
                                ))),
                            )),
                        )
                        .into_dummy_constant_expression()
                        .into_constant_expression()
                        .resolve(datapack, ctx)
                        .kind
                        .compile_as_statement(datapack, ctx);

                        ConstantExpressionKind::PlayerScore(unique_score)
                            .into_dummy_constant_expression()
                    }
                    LogicalOperator::Or => {
                        let unique_function_paths = datapack.get_unique_function_paths();

                        let unique_score = datapack.get_unique_player_score();
                        ctx.add_command(
                            datapack,
                            Command::Scoreboard(ScoreboardCommand::Players(
                                PlayersScoreboardCommand::Set(unique_score.clone(), 0),
                            )),
                        );
                        ctx.add_command(
                            datapack,
                            Command::Execute(ExecuteSubcommand::Store(
                                StoreType::Success,
                                ExecuteStoreSubcommand::Score(
                                    unique_score.clone(),
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

                        let return_one_subcommand = ExecuteSubcommand::Run(Box::new(
                            Command::Return(ReturnCommand::Value(1)),
                        ));

                        let (left_inverted, left_condition) =
                            left.kind
                                .to_execute_condition(datapack, &mut function_ctx, false);
                        function_ctx.add_command(
                            datapack,
                            Command::Execute(ExecuteSubcommand::If(
                                left_inverted,
                                left_condition.then(return_one_subcommand.clone()),
                            )),
                        );
                        let (right_inverted, right_condition) =
                            right
                                .kind
                                .to_execute_condition(datapack, &mut function_ctx, false);
                        function_ctx.add_command(
                            datapack,
                            Command::Execute(ExecuteSubcommand::If(
                                right_inverted,
                                right_condition.then(return_one_subcommand),
                            )),
                        );
                        function_ctx.add_command(datapack, Command::Return(ReturnCommand::Fail));

                        let function_commands = function_ctx.compile();

                        datapack
                            .get_function_mut(&unique_function_paths)
                            .add_commands(function_commands);

                        ConstantExpressionKind::PlayerScore(unique_score)
                            .into_dummy_constant_expression()
                    }
                }
            }
            ExpressionKind::Variable(name) => {
                let Some(value) = datapack.get_variable(&name) else {
                    panic!("Variable {} has not been declared", name);
                };

                value
            }
            ExpressionKind::AugmentedAssignment(target, operator, value) => {
                let target = target.resolve(datapack, ctx);
                let value = value.resolve(datapack, ctx);

                let assignable_target = target.kind.as_assignable();

                if let Some(assignable_target) = assignable_target {
                    assignable_target.operate(datapack, ctx, operator, &value);
                } else {
                    unreachable!("This expression cannot be assigned to")
                }

                value
            }
            ExpressionKind::Assignment(target, value) => {
                let target = target.resolve(datapack, ctx);
                let resolved_value = value.clone().resolve(datapack, ctx);

                if let ConstantExpressionKind::PlayerScore(player_score) = target.kind {
                    resolved_value
                        .kind
                        .assign_to_score(datapack, ctx, player_score);
                } else if let ConstantExpressionKind::Data(target, path) = target.kind {
                    value.assign_to_data(datapack, ctx, target, path);
                } else {
                    unreachable!("This expression cannot be assigned to")
                }

                resolved_value
            }
            ExpressionKind::String(value) => match value {
                StringExpression::Simple(value) => {
                    ConstantExpressionKind::String(SNBTString(false, value))
                }
                StringExpression::Complex(parts) => {
                    let has_macro = parts.has_macro();

                    let mut output_string = String::new();

                    for part in parts {
                        match part {
                            StringExpressionPart::Regular(string) => {
                                output_string.push_str(&string)
                            }
                            StringExpressionPart::Expression(expression) => {
                                let expression = expression.resolve(datapack, ctx);

                                output_string.push_str(&ctx.get_macro_string(expression.kind));
                            }
                        }
                    }

                    ConstantExpressionKind::String(SNBTString(has_macro, output_string))
                }
            }
            .into_dummy_constant_expression(),
            ExpressionKind::List(expressions) => ConstantExpressionKind::List(
                expressions
                    .into_iter()
                    .map(|expr| expr.resolve(datapack, ctx))
                    .collect::<Vec<_>>(),
            )
            .into_dummy_constant_expression(),
            ExpressionKind::Compound(compound) => ConstantExpressionKind::Compound(
                compound
                    .into_iter()
                    .map(|(key, value)| {
                        let key = key.compile(datapack, ctx);
                        let value = value.resolve(datapack, ctx);

                        (key, value)
                    })
                    .collect::<BTreeMap<_, _>>(),
            )
            .into_dummy_constant_expression(),
            ExpressionKind::PlayerScore(score) => {
                let score = score.clone().compile(datapack, ctx);

                ConstantExpressionKind::PlayerScore(score).into_dummy_constant_expression()
            }
            ExpressionKind::Data(target, path) => {
                let target = target.kind.clone().compile(datapack, ctx);
                let path = path.clone().compile(datapack, ctx);

                ConstantExpressionKind::Data(target, path).into_dummy_constant_expression()
            }
            ExpressionKind::Condition(inverted, condition) => {
                // TODO why optional?
                let condition = condition.compile(datapack, ctx).unwrap();

                ConstantExpressionKind::Condition(inverted, condition)
                    .into_dummy_constant_expression()
            }
            ExpressionKind::Command(command) => {
                // TODO why optional?
                let command = command.compile(datapack, ctx).unwrap();

                ConstantExpressionKind::Command(command).into_dummy_constant_expression()
            }
            ExpressionKind::Index(target, index) => {
                let target = target.resolve(datapack, ctx);
                let index = index.resolve(datapack, ctx);

                target
                    .kind
                    .index(datapack, ctx, index)
                    .into_dummy_constant_expression()
            }
            ExpressionKind::Member(target, member) => {
                let target = target.resolve(datapack, ctx);

                let member = member.compile(datapack, ctx);

                target
                    .kind
                    .access_member(datapack, ctx, member)
                    .into_dummy_constant_expression()
            }
        }
    }

    pub fn resolve_new_ctx(
        self,
        datapack: &mut HighDatapack,
    ) -> (CompileContext, ConstantExpression) {
        let mut ctx = CompileContext::default();
        let result = self.resolve(datapack, &mut ctx);
        (ctx, result)
    }

    pub fn resolve_into_score(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: &PlayerScore,
    ) {
        match self.kind {
            ExpressionKind::Arithmetic(left, operator, right) => {
                left.resolve_into_score(datapack, ctx, target);
                right
                    .resolve(datapack, ctx)
                    .kind
                    .operate_on_score(datapack, ctx, target, operator);
            }
            _ => {
                self.resolve(datapack, ctx)
                    .kind
                    .assign_to_score(datapack, ctx, target.clone());
            }
        }
    }
}

pub fn compile_shift_operation(
    datapack: &mut HighDatapack,
    ctx: &mut CompileContext,
    target: &PlayerScore,
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
