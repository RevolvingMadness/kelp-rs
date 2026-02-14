use std::collections::BTreeMap;

use minecraft_command_types::{
    command::{
        Command,
        data::{DataCommand, DataCommandModification, DataCommandModificationMode, DataTarget},
        enums::{score_operation_operator::ScoreOperationOperator, store_type::StoreType},
        execute::{
            ExecuteIfSubcommand, ExecuteStoreSubcommand, ExecuteSubcommand, ScoreComparison,
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
use parser_rs::parser_range::ParserRange;

use crate::{
    compile_context::CompileContext,
    data_type::DataTypeKind,
    datapack::HighDatapack,
    expression::{
        Expression, ExpressionKind,
        literal::{LiteralExpression, LiteralExpressionKind},
        supports_variable_type_scope::SupportsVariableTypeScope,
        utils::push_scoreboard_players,
    },
    high::{player_score::GeneratedPlayerScore, snbt_string::HighSNBTString},
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator},
    place::{Place, PlaceType, PlaceTypeKind},
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
};

pub type ConstantExpressionCompoundKind = BTreeMap<HighSNBTString, ConstantExpression>;

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
    compound: &ConstantExpressionCompoundKind,
) -> (SNBTCompound, ConstantExpressionCompoundKind) {
    let mut constants = BTreeMap::new();
    let mut non_constants = BTreeMap::new();

    for (key, expression) in compound.iter() {
        if let Some(snbt) = expression.kind.as_snbt() {
            constants.insert(key.snbt_string.clone(), snbt);
        } else {
            non_constants.insert(key.clone(), expression.clone());
            constants.insert(key.snbt_string.clone(), SNBT::Compound(BTreeMap::new()));
        }
    }

    (constants, non_constants)
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum ConstantExpressionKind {
    Literal(LiteralExpression),
    Underscore,
    List(Vec<ConstantExpression>),
    Compound(ConstantExpressionCompoundKind),
    PlayerScore(GeneratedPlayerScore),
    Data(DataTarget, NbtPath),
    Condition(bool, ExecuteIfSubcommand),
    Command(Command),
    Tuple(Vec<ConstantExpression>),
    Dereference(Box<ConstantExpression>),
    Reference(Box<ConstantExpression>),
    Variable(String),
    Unit,
}

impl ConstantExpressionKind {
    pub fn compile_augmented_assignment(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        operator: ArithmeticOperator,
        value: ConstantExpressionKind,
    ) {
        match self {
            ConstantExpressionKind::PlayerScore(score) => {
                value.operate_on_score(datapack, ctx, score, operator)
            }
            ConstantExpressionKind::Data(target, path) => {
                let unique_score = datapack.get_unique_player_score();

                self.assign_to_score(datapack, ctx, unique_score.clone());

                unique_score.assign_to_data(datapack, ctx, target.clone(), path.clone());
            }
            ConstantExpressionKind::Dereference(expression) => expression
                .clone()
                .kind
                .dereference(datapack)
                .compile_augmented_assignment(datapack, ctx, operator, value),
            ConstantExpressionKind::Variable(name) => {
                debug_assert!(false, "Unreachable");

                let variable_value = &mut datapack.get_variable_mut(name).unwrap().clone().kind;

                variable_value.compile_augmented_assignment(datapack, ctx, operator, value);
            }
            ConstantExpressionKind::Literal(_)
            | ConstantExpressionKind::List(_)
            | ConstantExpressionKind::Compound(_)
            | ConstantExpressionKind::Condition(_, _)
            | ConstantExpressionKind::Command(_)
            | ConstantExpressionKind::Tuple(_)
            | ConstantExpressionKind::Underscore
            | ConstantExpressionKind::Unit
            | ConstantExpressionKind::Reference(_) => unreachable!("{:?}", self),
        }
    }

    pub fn get_dereferenced_type(
        &self,
        supports_variable_type_scope: &impl SupportsVariableTypeScope,
    ) -> Option<DataTypeKind> {
        match self {
            ConstantExpressionKind::Reference(expression) => expression
                .kind
                .infer_data_type(supports_variable_type_scope),
            ConstantExpressionKind::Variable(name) => {
                Some(supports_variable_type_scope.get_variable(name)??)
            }
            _ => unreachable!("Cannot deference type {:?}", self),
        }
    }

    pub fn is_lvalue(&self) -> bool {
        matches!(
            self,
            ConstantExpressionKind::PlayerScore(_)
                | ConstantExpressionKind::Data(_, _)
                | ConstantExpressionKind::Variable(_)
                | ConstantExpressionKind::Dereference(_)
        )
    }

    pub fn can_be_dereferenced(&self) -> bool {
        matches!(
            self,
            ConstantExpressionKind::Reference(_) | ConstantExpressionKind::Variable(_)
        )
    }

    pub fn infer_data_type(
        &self,
        supports_variable_type_scope: &impl SupportsVariableTypeScope,
    ) -> Option<DataTypeKind> {
        Some(match self {
            ConstantExpressionKind::Literal(expression) => expression.kind.get_data_type(),
            ConstantExpressionKind::List(list) => {
                if let Some(first) = list.first() {
                    DataTypeKind::List(Box::new(
                        first.kind.infer_data_type(supports_variable_type_scope)?,
                    ))
                } else {
                    DataTypeKind::List(Box::new(DataTypeKind::SNBT))
                }
            }
            ConstantExpressionKind::Compound(compound) => DataTypeKind::TypedCompound(
                compound
                    .clone()
                    .into_iter()
                    .map(|(key, value)| {
                        value
                            .kind
                            .infer_data_type(supports_variable_type_scope)
                            .map(|data_type| (key.snbt_string, data_type))
                    })
                    .collect::<Option<_>>()?,
            ),
            ConstantExpressionKind::PlayerScore(_) => DataTypeKind::Score,
            ConstantExpressionKind::Data(_, _) => DataTypeKind::Data(Box::new(DataTypeKind::SNBT)),
            ConstantExpressionKind::Condition(_, _) => DataTypeKind::Boolean,
            ConstantExpressionKind::Command(_) => DataTypeKind::Integer,
            ConstantExpressionKind::Tuple(expressions) => DataTypeKind::Tuple(
                expressions
                    .iter()
                    .map(|expression| {
                        expression
                            .kind
                            .infer_data_type(supports_variable_type_scope)
                    })
                    .collect::<Option<_>>()?,
            ),
            ConstantExpressionKind::Variable(name) => {
                supports_variable_type_scope.get_variable(name)??
            }
            ConstantExpressionKind::Unit => DataTypeKind::Unit,
            ConstantExpressionKind::Dereference(expression) => expression
                .kind
                .get_dereferenced_type(supports_variable_type_scope)?,
            ConstantExpressionKind::Reference(expression) => DataTypeKind::Reference(Box::new(
                expression
                    .kind
                    .infer_data_type(supports_variable_type_scope)?,
            )),
            ConstantExpressionKind::Underscore => unreachable!(),
        })
    }

    pub fn into_dummy_constant_expression(self) -> ConstantExpression {
        ConstantExpression {
            span: ParserRange::default(),
            kind: self,
        }
    }

    pub fn try_as_i32(&self, force: bool) -> Option<i32> {
        Some(match self {
            ConstantExpressionKind::Literal(expression) => {
                return expression.kind.try_as_i32(force);
            }
            ConstantExpressionKind::List(v) if force => v.len() as i32,
            ConstantExpressionKind::Compound(compound) if force => compound.len() as i32,
            _ => return None,
        })
    }

    pub fn compile_as_statement(&self, datapack: &mut HighDatapack, ctx: &mut CompileContext) {
        match self {
            ConstantExpressionKind::List(constant_expressions) => {
                for constant_expression in constant_expressions {
                    constant_expression.kind.compile_as_statement(datapack, ctx);
                }
            }
            ConstantExpressionKind::Tuple(expressions) => {
                for expression in expressions {
                    expression.kind.compile_as_statement(datapack, ctx);
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
            ConstantExpressionKind::Literal(_)
            | ConstantExpressionKind::PlayerScore(_)
            | ConstantExpressionKind::Data(_, _)
            | ConstantExpressionKind::Unit => {}
            ConstantExpressionKind::Reference(expression) => {
                expression.kind.compile_as_statement(datapack, ctx)
            }
            ConstantExpressionKind::Dereference(expression) => {
                expression.kind.compile_as_statement(datapack, ctx)
            }
            ConstantExpressionKind::Variable(name) => datapack
                .get_variable(name)
                .unwrap()
                .1
                .kind
                .compile_as_statement(datapack, ctx),
            ConstantExpressionKind::Underscore => unreachable!(),
        }
    }

    #[must_use]
    pub fn as_score(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        force: bool,
    ) -> GeneratedPlayerScore {
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

    pub fn invert(self) -> Option<ConstantExpressionKind> {
        Some(match self {
            ConstantExpressionKind::Literal(expression) => {
                ConstantExpressionKind::Literal(LiteralExpression {
                    span: expression.span,
                    kind: expression.kind.invert()?,
                })
            }
            ConstantExpressionKind::Condition(inverted, subcommand) => {
                ConstantExpressionKind::Condition(!inverted, subcommand)
            }
            _ => return None,
        })
    }

    pub fn index(self, index: ConstantExpressionKind) -> ConstantExpressionKind {
        match self {
            ConstantExpressionKind::Reference(expression) => expression.kind.index(index),

            ConstantExpressionKind::List(mut items) => {
                if let ConstantExpressionKind::Literal(LiteralExpression {
                    kind: LiteralExpressionKind::Integer(index),
                    ..
                }) = index
                {
                    if index >= 0 && (index as usize) < items.len() {
                        items.swap_remove(index as usize).kind
                    } else {
                        unreachable!("Index is out of range");
                    }
                } else {
                    unreachable!("The index expression is not an integer")
                }
            }
            ConstantExpressionKind::Data(target, path) => {
                if let Some(snbt) = index.as_snbt() {
                    ConstantExpressionKind::Data(
                        target,
                        path.with_node(NbtPathNode::Index(Some(snbt))),
                    )
                } else {
                    unreachable!("The index expression is not snbt")
                }
            }
            _ => unreachable!("The expression cannot be indexed {:?}", self),
        }
    }

    pub fn access_field(self, field: SNBTString) -> ConstantExpressionKind {
        match self {
            ConstantExpressionKind::Literal(_)
            | ConstantExpressionKind::List(_)
            | ConstantExpressionKind::PlayerScore(_)
            | ConstantExpressionKind::Condition(_, _)
            | ConstantExpressionKind::Command(_)
            | ConstantExpressionKind::Unit
            | ConstantExpressionKind::Variable(_)
            | ConstantExpressionKind::Dereference(_) => {
                unreachable!("Expression does not have any fields {:?}", self)
            }
            ConstantExpressionKind::Underscore => unreachable!(),

            ConstantExpressionKind::Reference(expression) => expression.kind.access_field(field),

            ConstantExpressionKind::Compound(compound) => {
                compound
                    .into_iter()
                    .find(|(key, _)| key.snbt_string == field)
                    .map(|(_, value)| value)
                    .unwrap()
                    .kind
            }
            ConstantExpressionKind::Data(target, path) => ConstantExpressionKind::Data(
                target,
                path.with_node(NbtPathNode::Named(field, None)),
            ),
            ConstantExpressionKind::Tuple(mut expressions) => {
                if let Ok(index) = field.1.parse::<i32>() {
                    expressions.remove(index as usize).kind
                } else {
                    unreachable!("Tuple does not have field {:?}", field.1);
                }
            }
        }
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

    pub fn perform_arithmetic(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        operator: ArithmeticOperator,
        other: ConstantExpressionKind,
    ) -> ConstantExpressionKind {
        match (self, other) {
            (ConstantExpressionKind::Reference(expression), other) => expression
                .kind
                .perform_arithmetic(datapack, ctx, operator, other),

            (self_, ConstantExpressionKind::Reference(other)) => {
                self_.perform_arithmetic(datapack, ctx, operator, other.kind)
            }

            (ConstantExpressionKind::Literal(left), ConstantExpressionKind::Literal(right)) => {
                ConstantExpressionKind::Literal(LiteralExpression {
                    span: ParserRange { start: 0, end: 0 },
                    kind: left.kind.perform_arithmetic(operator, right.kind).unwrap(),
                })
            }

            (
                left_kind @ (ConstantExpressionKind::PlayerScore(_)
                | ConstantExpressionKind::Data(_, _)),
                right_kind,
            ) => {
                // TODO maybe better checking?
                // TODO assign into score

                let unique_score = datapack.get_unique_player_score();

                left_kind.assign_to_score(datapack, ctx, unique_score.clone());
                right_kind.operate_on_score(datapack, ctx, &unique_score, operator);

                ConstantExpressionKind::PlayerScore(unique_score)
            }

            (
                left_kind,
                right_kind @ (ConstantExpressionKind::PlayerScore(_)
                | ConstantExpressionKind::Data(_, _)),
            ) => {
                // TODO maybe better checking?
                // TODO assign into score

                let unique_score = datapack.get_unique_player_score();

                left_kind.assign_to_score(datapack, ctx, unique_score.clone());
                right_kind.operate_on_score(datapack, ctx, &unique_score, operator);

                ConstantExpressionKind::PlayerScore(unique_score)
            }

            (left_kind, right_kind) => unreachable!("{:?} {:?}", left_kind, right_kind),
        }
    }

    pub fn perform_comparison(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        operator: ComparisonOperator,
        other: ConstantExpressionKind,
    ) -> ConstantExpressionKind {
        match (self, other) {
            (ConstantExpressionKind::Reference(expression), other) => expression
                .kind
                .perform_comparison(datapack, ctx, operator, other),

            (self_, ConstantExpressionKind::Reference(other)) => {
                self_.perform_comparison(datapack, ctx, operator, other.kind)
            }

            (ConstantExpressionKind::Literal(left), ConstantExpressionKind::Literal(right)) => {
                ConstantExpressionKind::Literal(LiteralExpression {
                    span: ParserRange { start: 0, end: 0 },
                    kind: left.kind.compare(operator, right.kind).unwrap(),
                })
            }
            (left_kind @ ConstantExpressionKind::Data(_, _), right_kind)
                if operator == ComparisonOperator::EqualTo
                    || operator == ComparisonOperator::NotEqualTo =>
            {
                let unique_score = datapack.get_unique_player_score();

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
                                    unique_target,
                                    unique_path,
                                    DataCommandModificationMode::Set,
                                    data_command_modification,
                                ),
                            )))),
                        ),
                    )),
                );

                ConstantExpressionKind::Condition(
                    operator.should_execute_if_be_inverted(),
                    ExecuteIfSubcommand::Score(
                        unique_score.score,
                        ScoreComparison::Range(IntegerRange::new_single(0)),
                        None,
                    ),
                )
            }
            (ConstantExpressionKind::PlayerScore(left_score), right_kind) => {
                let right_score = right_kind.as_score(datapack, ctx, false);

                ConstantExpressionKind::Condition(
                    operator.should_execute_if_be_inverted(),
                    ExecuteIfSubcommand::Score(
                        left_score.score,
                        ScoreComparison::Score(
                            operator.into_score_comparison_operator(),
                            right_score.score,
                        ),
                        None,
                    ),
                )
            }
            (left_kind, ConstantExpressionKind::PlayerScore(right_score)) => {
                let left_score = left_kind.as_score(datapack, ctx, false);

                ConstantExpressionKind::Condition(
                    operator.should_execute_if_be_inverted(),
                    ExecuteIfSubcommand::Score(
                        right_score.score,
                        ScoreComparison::Score(
                            operator.into_score_comparison_operator(),
                            left_score.score,
                        ),
                        None,
                    ),
                )
            }
            _ => unreachable!(),
        }
    }

    pub fn perform_logical_operation(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        operator: LogicalOperator,
        other: ConstantExpressionKind,
    ) -> ConstantExpressionKind {
        if let ConstantExpressionKind::Reference(self_) = self {
            return self_
                .kind
                .perform_logical_operation(datapack, ctx, operator, other);
        }

        if let ConstantExpressionKind::Reference(other) = other {
            return self.perform_logical_operation(datapack, ctx, operator, other.kind);
        }

        match operator {
            LogicalOperator::And => {
                let unique_score = datapack.get_unique_player_score();

                let (self_inverted, self_condition) =
                    self.to_execute_condition(datapack, ctx, false);
                let (right_inverted, right_condition) =
                    other.to_execute_condition(datapack, ctx, false);

                ctx.add_command(
                    datapack,
                    Command::Scoreboard(ScoreboardCommand::Players(PlayersScoreboardCommand::Set(
                        unique_score.score.clone(),
                        0,
                    ))),
                );
                ConstantExpressionKind::Condition(
                    self_inverted,
                    self_condition.then(ExecuteSubcommand::If(
                        right_inverted,
                        right_condition.then(ExecuteSubcommand::Run(Box::new(
                            Command::Scoreboard(ScoreboardCommand::Players(
                                PlayersScoreboardCommand::Set(unique_score.score.clone(), 1),
                            )),
                        ))),
                    )),
                )
                .into_dummy_constant_expression()
                .into_constant_expression()
                .resolve(datapack, ctx)
                .compile_as_statement(datapack, ctx);

                ConstantExpressionKind::PlayerScore(unique_score)
            }
            LogicalOperator::Or => {
                let unique_function_paths = datapack.get_unique_function_paths();

                let unique_score = datapack.get_unique_player_score();
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

                let (self_inverted, left_condition) =
                    self.to_execute_condition(datapack, &mut function_ctx, false);
                function_ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::If(
                        self_inverted,
                        left_condition.then(return_one_subcommand.clone()),
                    )),
                );
                let (other_inverted, other_condition) =
                    other.to_execute_condition(datapack, &mut function_ctx, false);
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

                ConstantExpressionKind::PlayerScore(unique_score)
            }
        }
    }

    pub fn operate_on_score(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: &GeneratedPlayerScore,
        operator: ArithmeticOperator,
    ) {
        if let Some(value) = self.try_as_i32(true) {
            return match operator {
                ArithmeticOperator::Add => {
                    push_scoreboard_players(
                        datapack,
                        ctx,
                        PlayersScoreboardCommand::Add(target.score.clone(), value),
                    );
                }
                ArithmeticOperator::Subtract => {
                    push_scoreboard_players(
                        datapack,
                        ctx,
                        PlayersScoreboardCommand::Remove(target.score.clone(), value),
                    );
                }
                ArithmeticOperator::And => {
                    datapack
                        .get_constant_score(value)
                        .operate_on_score(datapack, ctx, target, operator);
                }
                ArithmeticOperator::Or => {
                    datapack
                        .get_constant_score(value)
                        .operate_on_score(datapack, ctx, target, operator);
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

        match self {
            ConstantExpressionKind::PlayerScore(source) => {
                source
                    .clone()
                    .operate_on_score(datapack, ctx, target, operator);
            }
            _ => {
                let unique_score = datapack.get_unique_player_score();

                self.assign_to_score(datapack, ctx, unique_score.clone());

                unique_score
                    .clone()
                    .operate_on_score(datapack, ctx, target, operator);
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

            push_scoreboard_players(
                datapack,
                ctx,
                PlayersScoreboardCommand::Set(unique_score.score.clone(), value),
            );

            return (
                !inverted,
                ExecuteIfSubcommand::Score(
                    unique_score.score,
                    ScoreComparison::Range(IntegerRange::new_single(0)),
                    None,
                ),
            );
        }

        match self {
            ConstantExpressionKind::Literal(expression) => {
                expression.kind.to_execute_condition(datapack).unwrap()
            }
            ConstantExpressionKind::PlayerScore(score) => (
                !inverted,
                ExecuteIfSubcommand::Score(
                    score.score.clone(),
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
            ConstantExpressionKind::Command(command) => {
                let unique_score = datapack.get_unique_player_score();

                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Success,
                        ExecuteStoreSubcommand::Score(
                            unique_score.score.clone(),
                            Box::new(ExecuteSubcommand::Run(Box::new(command.clone()))),
                        ),
                    )),
                );

                (
                    !inverted,
                    ExecuteIfSubcommand::Score(
                        unique_score.score,
                        ScoreComparison::Range(IntegerRange::new_single(0)),
                        None,
                    ),
                )
            }
            ConstantExpressionKind::Variable(name) => datapack
                .get_variable(name)
                .unwrap()
                .1
                .kind
                .to_execute_condition(datapack, ctx, inverted),
            ConstantExpressionKind::List(_)
            | ConstantExpressionKind::Compound(_)
            | ConstantExpressionKind::Tuple(_)
            | ConstantExpressionKind::Unit
            | ConstantExpressionKind::Reference(_)
            | ConstantExpressionKind::Dereference(_)
            | ConstantExpressionKind::Underscore => {
                unreachable!()
            }
        }
    }

    pub fn as_text_component(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        force_display: bool,
    ) -> SNBT {
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
            ConstantExpressionKind::Literal(expression) => {
                expression.kind.as_text_component(force_display)
            }
            ConstantExpressionKind::List(constant_expressions) => SNBT::List(
                constant_expressions
                    .iter()
                    .map(|expression| expression.kind.as_text_component(datapack, ctx, false))
                    .collect(),
            ),
            ConstantExpressionKind::Compound(btree_map) => SNBT::Compound(
                btree_map
                    .iter()
                    .map(|(key, value)| {
                        (
                            key.snbt_string.clone(),
                            value.kind.as_text_component(datapack, ctx, false),
                        )
                    })
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
            ConstantExpressionKind::Tuple(expressions) => {
                let mut items = Vec::new();

                items.push(SNBT::string("("));

                for (i, expression) in expressions.iter().enumerate() {
                    if i != 0 {
                        items.push(SNBT::string(", "));
                    }

                    items.push(expression.kind.as_text_component(datapack, ctx, true));
                }

                items.push(SNBT::string(")"));

                SNBT::List(items)
            }
            ConstantExpressionKind::Unit => SNBT::string("()"),
            ConstantExpressionKind::Reference(expression) => {
                expression
                    .kind
                    .as_text_component(datapack, ctx, force_display)
            }
            ConstantExpressionKind::Dereference(expression) => {
                expression
                    .kind
                    .as_text_component(datapack, ctx, force_display)
            }
            ConstantExpressionKind::Variable(name) => datapack
                .get_variable(name)
                .unwrap()
                .1
                .kind
                .as_text_component(datapack, ctx, force_display),
            ConstantExpressionKind::Underscore => unreachable!(),
        }
    }

    pub fn assign_to_score(
        &self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: GeneratedPlayerScore,
    ) {
        match self {
            ConstantExpressionKind::Literal(expression) => {
                expression.kind.assign_to_score(datapack, ctx, target.score)
            }
            ConstantExpressionKind::List(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target.score, value.len() as i32),
                );
            }
            ConstantExpressionKind::Compound(value) => {
                push_scoreboard_players(
                    datapack,
                    ctx,
                    PlayersScoreboardCommand::Set(target.score, value.len() as i32),
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
                            target.score,
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
                            target.score,
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
                            target.score,
                            Box::new(ExecuteSubcommand::Run(Box::new(command.clone()))),
                        ),
                    )),
                );
            }
            ConstantExpressionKind::Tuple(_) => unreachable!(),
            ConstantExpressionKind::Unit => unreachable!(),
            ConstantExpressionKind::Reference(expression) => {
                expression.kind.assign_to_score(datapack, ctx, target)
            }
            ConstantExpressionKind::Dereference(_) => unreachable!(),
            ConstantExpressionKind::Variable(name) => datapack
                .get_variable(name)
                .unwrap()
                .1
                .kind
                .assign_to_score(datapack, ctx, target),
            ConstantExpressionKind::Underscore => unreachable!(),
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
                        path.clone().with_node(NbtPathNode::named(key.snbt_string)),
                    );
                }
            }
            ConstantExpressionKind::Unit => {
                let mut unit_btreemap = BTreeMap::new();
                unit_btreemap.insert(
                    SNBTString(false, "__kelp_rs_unit__".to_string()),
                    SNBT::Byte(1),
                );

                ctx.add_command(
                    datapack,
                    Command::Data(DataCommand::Modify(
                        target.clone(),
                        path.clone(),
                        DataCommandModificationMode::Set,
                        DataCommandModification::Value(SNBT::Compound(unit_btreemap)),
                    )),
                );
            }
            ConstantExpressionKind::Reference(expression) => {
                expression.kind.assign_to_data(datapack, ctx, target, path)
            }
            _ => unreachable!(),
        }
    }

    pub fn as_snbt_macros(self, ctx: &mut CompileContext) -> SNBT {
        match self {
            ConstantExpressionKind::Literal(expression) => expression.kind.into_snbt(),
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
                    .map(|(key, value)| (key.snbt_string, value.kind.as_snbt_macros(ctx)))
                    .collect(),
            ),
            _ => ctx.get_macro_snbt(self),
        }
    }

    pub fn as_snbt(&self) -> Option<SNBT> {
        match self {
            ConstantExpressionKind::Literal(expression) => {
                Some(expression.clone().kind.into_snbt())
            }

            ConstantExpressionKind::List(expressions) => expressions
                .iter()
                .map(|expr| expr.kind.as_snbt())
                .collect::<Option<Vec<_>>>()
                .map(SNBT::List),

            ConstantExpressionKind::Compound(compound) => compound
                .iter()
                .map(|(key, value)| value.kind.as_snbt().map(|v| (key.snbt_string.clone(), v)))
                .collect::<Option<_>>()
                .map(SNBT::Compound),

            _ => None,
        }
    }

    pub fn cast_to(
        self,
        datapack: &mut HighDatapack,
        data_type: DataTypeKind,
    ) -> ConstantExpressionKind {
        let self_type = self.infer_data_type(datapack).unwrap();

        if self_type.equals(&data_type) {
            return self;
        }

        match (self, data_type) {
            (ConstantExpressionKind::Literal(expression), data_type) => {
                ConstantExpressionKind::Literal(LiteralExpression {
                    span: expression.span,
                    kind: expression.kind.cast_to(data_type),
                })
            }

            (self_ @ ConstantExpressionKind::Data(_, _), DataTypeKind::Data(_)) => self_,

            _ => unreachable!(""),
        }
    }

    pub fn as_place(self) -> Option<Place> {
        Some(match self {
            ConstantExpressionKind::PlayerScore(score) => Place::Score(score),
            ConstantExpressionKind::Data(target, path) => Place::Data(target, path),
            ConstantExpressionKind::Variable(name) => Place::Variable(name),
            ConstantExpressionKind::Underscore => Place::Underscore,
            ConstantExpressionKind::Tuple(expressions) => Place::Tuple(
                expressions
                    .into_iter()
                    .map(|expression| expression.kind.as_place())
                    .collect::<Option<_>>()?,
            ),
            _ => return None,
        })
    }

    pub fn dereference(self, datapack: &mut HighDatapack) -> ConstantExpressionKind {
        match self {
            ConstantExpressionKind::Variable(name) => datapack.get_variable(&name).unwrap().1.kind,
            ConstantExpressionKind::Reference(expression) => expression.kind,
            ConstantExpressionKind::PlayerScore(_) | ConstantExpressionKind::Data(_, _) => self,
            _ => unreachable!("This expression cannot be dereferenced {:?}", self),
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
    pub fn get_place_type(&self, ctx: &impl SupportsVariableTypeScope) -> Option<PlaceType> {
        Some(
            (match &self.kind {
                ConstantExpressionKind::PlayerScore(_) => PlaceTypeKind::Score,
                ConstantExpressionKind::Data(_, _) => PlaceTypeKind::Data(DataTypeKind::SNBT),
                ConstantExpressionKind::Dereference(expression) => {
                    return expression.get_place_type(ctx);
                }
                ConstantExpressionKind::Variable(name) => {
                    PlaceTypeKind::Variable(ctx.get_variable(name)??)
                }
                ConstantExpressionKind::Underscore => PlaceTypeKind::Underscore,
                _ => return None,
            })
            .with_span(self.span),
        )
    }

    pub fn resolve(self, datapack: &mut HighDatapack) -> ConstantExpressionKind {
        match self.kind {
            ConstantExpressionKind::Variable(name) => datapack.get_variable(&name).unwrap().1.kind,
            kind => kind,
        }
    }

    #[must_use]
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match &self.kind {
            ConstantExpressionKind::Literal(expression) => {
                expression.kind.perform_semantic_analysis(ctx, is_lhs)
            }
            ConstantExpressionKind::Unit
            | ConstantExpressionKind::PlayerScore(_)
            | ConstantExpressionKind::Data(_, _) => Some(()),
            ConstantExpressionKind::Reference(expression) => {
                let data_type = expression.kind.infer_data_type(ctx)?;

                if !expression.kind.is_lvalue() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: expression.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeReferenced(data_type),
                        ),
                    });
                }

                Some(())
            }
            ConstantExpressionKind::Dereference(expression) => {
                let data_type = expression.kind.infer_data_type(ctx)?;

                if !expression.kind.can_be_dereferenced() {
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: expression.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::CannotBeDereferenced(data_type),
                        ),
                    });
                }

                Some(())
            }
            ConstantExpressionKind::Variable(name) => {
                if !ctx.variable_is_declared(name) {
                    ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::UndeclaredVariable(name.clone()),
                        ),
                    })
                } else {
                    Some(())
                }
            }
            ConstantExpressionKind::List(list) => list
                .iter()
                .map(|item| item.perform_semantic_analysis(ctx, is_lhs))
                .collect::<Option<()>>(),
            ConstantExpressionKind::Compound(compound) => compound
                .values()
                .map(|value| value.perform_semantic_analysis(ctx, is_lhs))
                .collect::<Option<()>>(),
            ConstantExpressionKind::Condition(_, _) => {
                // TODO future

                Some(())
            }
            ConstantExpressionKind::Command(_command) => {
                // TODO future

                Some(())
            }
            ConstantExpressionKind::Tuple(items) => items
                .iter()
                .map(|item| item.perform_semantic_analysis(ctx, is_lhs))
                .collect::<Option<()>>(),
            ConstantExpressionKind::Underscore => {
                if !is_lhs {
                    ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::UnderscoreExpression,
                        ),
                    })
                } else {
                    Some(())
                }
            }
        }
    }

    pub fn map_kind(
        self,
        f: impl FnOnce(ConstantExpressionKind) -> ConstantExpressionKind,
    ) -> ConstantExpression {
        Self {
            kind: f(self.kind),
            ..self
        }
    }

    pub fn into_constant_expression(self) -> Expression {
        Expression {
            span: self.span,
            kind: ExpressionKind::Constant(self),
        }
    }
}
