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
    expression::{
        Expression, ExpressionKind,
        literal::{LiteralExpression, LiteralExpressionKind},
        supports_variable_type_scope::SupportsVariableTypeScope,
        utils::push_scoreboard_players,
    },
    high::{
        data::GeneratedDataTarget, player_score::GeneratedPlayerScore, snbt_string::HighSNBTString,
    },
    operator::{ArithmeticOperator, ComparisonOperator, LogicalOperator},
    place::{Place, PlaceType, PlaceTypeKind},
    semantic_analysis_context::{
        SemanticAnalysisContext, SemanticAnalysisError, SemanticAnalysisInfo,
        SemanticAnalysisInfoKind,
    },
    span::Span,
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

#[must_use]
pub fn split_constants_list(
    list: Vec<ConstantExpression>,
) -> (Vec<SNBT>, Vec<(usize, ConstantExpression)>) {
    let mut constants = Vec::new();
    let mut non_constants = Vec::new();

    for (i, expression) in list.into_iter().enumerate() {
        if let Some(snbt) = expression.kind.as_snbt() {
            constants.push(snbt);
        } else {
            non_constants.push((i, expression));
            constants.push(SNBT::Compound(BTreeMap::new()));
        }
    }

    (constants, non_constants)
}

#[must_use]
pub fn split_constants_compound(
    compound: ConstantExpressionCompoundKind,
) -> (SNBTCompound, ConstantExpressionCompoundKind) {
    let mut constants = BTreeMap::new();
    let mut non_constants = BTreeMap::new();

    for (key, expression) in compound {
        if let Some(snbt) = expression.kind.as_snbt() {
            constants.insert(key.snbt_string, snbt);
        } else {
            constants.insert(key.snbt_string.clone(), SNBT::Compound(BTreeMap::new()));
            non_constants.insert(key, expression);
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
    Data(GeneratedDataTarget, NbtPath),
    Condition(bool, ExecuteIfSubcommand),
    Command(Command),
    Tuple(Vec<ConstantExpression>),
    Dereference(Box<ConstantExpression>),
    Reference(Box<ConstantExpression>),
    Variable(String),
    Unit,
    Struct(
        String,
        Vec<DataTypeKind>,
        BTreeMap<String, ConstantExpression>,
    ),
}

impl ConstantExpressionKind {
    #[must_use]
    pub const fn with_span(self, span: Span) -> ConstantExpression {
        ConstantExpression { span, kind: self }
    }

    #[must_use]
    pub fn distribute_references(self) -> Self {
        match self {
            Self::Reference(inner) => match inner.kind.distribute_references() {
                Self::Tuple(values) => {
                    let new_values = values
                        .into_iter()
                        .map(|val| ConstantExpression {
                            span: val.span,
                            kind: Self::Reference(Box::new(val)),
                        })
                        .collect();

                    Self::Tuple(new_values)
                }
                Self::Compound(expressions) => {
                    let new_expressions = expressions
                        .into_iter()
                        .map(|(key, val)| {
                            (
                                key,
                                ConstantExpression {
                                    span: val.span,
                                    kind: Self::Reference(Box::new(val)),
                                },
                            )
                        })
                        .collect();

                    Self::Compound(new_expressions)
                }
                inner => Self::Reference(Box::new(inner.into_dummy_constant_expression())),
            },
            _ => self,
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
            Self::Data(ref target, ref path) => {
                let unique_score = datapack.get_unique_score();

                self.clone()
                    .assign_to_score(datapack, ctx, unique_score.clone());

                value.operate_on_score(datapack, ctx, unique_score.clone(), operator);

                unique_score.assign_to_data(datapack, ctx, target.clone(), path.clone());
            }
            Self::Dereference(expression) => expression
                .kind
                .dereference(datapack, ctx)
                .compile_augmented_assignment(datapack, ctx, operator, value),
            Self::Variable(_)
            | Self::Literal(_)
            | Self::List(_)
            | Self::Compound(_)
            | Self::Condition(_, _)
            | Self::Command(_)
            | Self::Tuple(_)
            | Self::Underscore
            | Self::Unit
            | Self::Reference(_)
            | Self::Struct(_, _, _) => unreachable!("{:?}", self),
        }
    }

    pub fn get_dereferenced_type(
        &self,
        supports_variable_type_scope: &impl SupportsVariableTypeScope,
    ) -> Option<DataTypeKind> {
        match self {
            Self::Reference(expression) => expression
                .kind
                .infer_data_type(supports_variable_type_scope),
            Self::Variable(name) => Some(supports_variable_type_scope.get_variable(name)??),
            _ => unreachable!("Cannot deference type {:?}", self),
        }
    }

    #[must_use]
    pub const fn is_lvalue(&self) -> bool {
        matches!(
            self,
            Self::PlayerScore(_) | Self::Data(_, _) | Self::Variable(_) | Self::Dereference(_)
        )
    }

    #[must_use]
    pub const fn can_be_dereferenced(&self) -> bool {
        matches!(self, Self::Reference(_) | Self::Variable(_))
    }

    pub fn infer_data_type(
        &self,
        supports_variable_type_scope: &impl SupportsVariableTypeScope,
    ) -> Option<DataTypeKind> {
        Some(match self {
            Self::Literal(expression) => expression.kind.get_data_type(),
            Self::List(list) => {
                if let Some(first) = list.first() {
                    DataTypeKind::List(Box::new(
                        first.kind.infer_data_type(supports_variable_type_scope)?,
                    ))
                } else {
                    DataTypeKind::List(Box::new(DataTypeKind::SNBT))
                }
            }
            Self::Compound(compound) => DataTypeKind::TypedCompound(
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
            Self::PlayerScore(_) => DataTypeKind::Score,
            Self::Data(_, _) => DataTypeKind::Data(Box::new(DataTypeKind::SNBT)),
            Self::Condition(_, _) => DataTypeKind::Boolean,
            Self::Command(_) => DataTypeKind::Integer,
            Self::Tuple(expressions) => DataTypeKind::Tuple(
                expressions
                    .iter()
                    .map(|expression| {
                        expression
                            .kind
                            .infer_data_type(supports_variable_type_scope)
                    })
                    .collect::<Option<_>>()?,
            ),
            Self::Variable(name) => supports_variable_type_scope.get_variable(name)??,
            Self::Unit => DataTypeKind::Unit,
            Self::Dereference(expression) => expression
                .kind
                .get_dereferenced_type(supports_variable_type_scope)?,
            Self::Reference(expression) => DataTypeKind::Reference(Box::new(
                expression
                    .kind
                    .infer_data_type(supports_variable_type_scope)?,
            )),
            Self::Underscore => unreachable!(),
            Self::Struct(name, generics_types, _) => {
                DataTypeKind::Struct(name.clone(), generics_types.clone(), false)
            }
        })
    }

    #[must_use]
    pub const fn into_dummy_constant_expression(self) -> ConstantExpression {
        ConstantExpression {
            span: Span::dummy(),
            kind: self,
        }
    }

    #[must_use]
    pub fn try_as_i32(&self, force: bool) -> Option<i32> {
        Some(match self {
            Self::Literal(expression) => {
                return expression.kind.try_as_i32(force);
            }
            Self::List(v) if force => v.len() as i32,
            Self::Compound(compound) if force => compound.len() as i32,
            _ => return None,
        })
    }

    pub fn compile_as_statement(&self, datapack: &mut HighDatapack, ctx: &mut CompileContext) {
        match self {
            Self::List(constant_expressions) => {
                for constant_expression in constant_expressions {
                    constant_expression.kind.compile_as_statement(datapack, ctx);
                }
            }
            Self::Tuple(expressions) => {
                for expression in expressions {
                    expression.kind.compile_as_statement(datapack, ctx);
                }
            }
            Self::Compound(compound) => {
                for value in compound.values() {
                    value.kind.compile_as_statement(datapack, ctx);
                }
            }
            Self::Condition(inverted, condition) => {
                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::If(*inverted, condition.clone())),
                );
            }
            Self::Command(command) => {
                ctx.add_command(datapack, command.clone());
            }
            Self::Literal(_) | Self::PlayerScore(_) | Self::Data(_, _) | Self::Unit => {}
            Self::Reference(expression) | Self::Dereference(expression) => {
                expression.kind.compile_as_statement(datapack, ctx);
            }
            Self::Variable(name) => datapack
                .get_variable(name)
                .unwrap()
                .1
                .kind
                .compile_as_statement(datapack, ctx),
            Self::Underscore => unreachable!(),
            Self::Struct(_, _, fields) => {
                for field in fields.values() {
                    field.kind.compile_as_statement(datapack, ctx);
                }
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
        if let Some(value) = self.try_as_i32(force) {
            return if force {
                let unique_score = datapack.get_unique_score();
                self.assign_to_score(datapack, ctx, unique_score.clone());
                unique_score
            } else {
                datapack.get_constant_score(value)
            };
        }

        match self {
            Self::PlayerScore(player_score) if !force => player_score,
            _ => {
                let unique_score = datapack.get_unique_score();
                self.assign_to_score(datapack, ctx, unique_score.clone());
                unique_score
            }
        }
    }

    #[must_use]
    pub fn as_data(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> (GeneratedDataTarget, NbtPath) {
        if let Self::Data(target, path) = self {
            (target, path)
        } else {
            let (unique_data, path) = datapack.get_unique_data();

            self.assign_to_data(datapack, ctx, unique_data.clone(), path.clone());

            (unique_data, path)
        }
    }

    #[must_use]
    pub fn as_data_force(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> (GeneratedDataTarget, NbtPath) {
        let (unique_data, path) = datapack.get_unique_data();

        self.assign_to_data(datapack, ctx, unique_data.clone(), path.clone());

        (unique_data, path)
    }

    #[must_use]
    pub fn invert(self) -> Self {
        match self {
            Self::Literal(expression) => Self::Literal(LiteralExpression {
                span: expression.span,
                kind: expression.kind.invert(),
            }),
            Self::Condition(inverted, subcommand) => Self::Condition(!inverted, subcommand),
            _ => unreachable!("Cannot invert expression {:?}", self),
        }
    }

    #[must_use]
    pub fn negate(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> Self {
        match self {
            Self::Literal(expression) => Self::Literal(LiteralExpression {
                span: expression.span,
                kind: expression.kind.negate(),
            }),
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
            Self::Data(_, _) => {
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
            // TODO? Self::Command
            Self::Dereference(expression) => expression
                .kind
                .dereference(datapack, ctx)
                .negate(datapack, ctx),
            Self::Reference(expression) => expression.kind.negate(datapack, ctx),
            Self::Variable(name) => datapack
                .get_variable(&name)
                .unwrap()
                .1
                .kind
                .negate(datapack, ctx),
            _ => unreachable!("Cannot negate expression {:?}", self),
        }
    }

    #[must_use]
    pub fn index(self, index: Self) -> Self {
        match self {
            Self::Reference(expression) => expression.kind.index(index),

            Self::List(mut items) => {
                if let Self::Literal(LiteralExpression {
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
            Self::Data(target, path) => index.as_snbt().map_or_else(
                || unreachable!("The index expression is not snbt"),
                |snbt| Self::Data(target, path.with_node(NbtPathNode::Index(Some(snbt)))),
            ),
            _ => unreachable!("The expression cannot be indexed {:?}", self),
        }
    }

    #[must_use]
    pub fn access_field(self, field: String) -> Self {
        match self {
            Self::Literal(_)
            | Self::List(_)
            | Self::PlayerScore(_)
            | Self::Condition(_, _)
            | Self::Command(_)
            | Self::Unit
            | Self::Variable(_)
            | Self::Dereference(_) => {
                unreachable!("Expression does not have any fields {:?}", self)
            }
            Self::Underscore => unreachable!(),
            Self::Reference(expression) => expression.kind.access_field(field),
            Self::Compound(compound) => {
                compound
                    .into_iter()
                    .find(|(key, _)| key.snbt_string.1 == *field)
                    .map(|(_, value)| value)
                    .unwrap()
                    .kind
            }
            Self::Data(target, path) => Self::Data(
                target,
                path.with_node(NbtPathNode::Named(SNBTString(false, field), None)),
            ),
            Self::Tuple(mut expressions) => field.parse::<i32>().map_or_else(
                |_| {
                    unreachable!("Tuple does not have field {:?}", field);
                },
                |index| expressions.remove(index as usize).kind,
            ),
            Self::Struct(_, _, fields) => {
                fields
                    .into_iter()
                    .find(|(key, _)| *key == field)
                    .unwrap()
                    .1
                    .kind
            }
        }
    }

    #[must_use]
    pub const fn is_data(&self) -> bool {
        matches!(self, Self::Data(_, _))
    }

    pub fn as_data_command_modification(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> DataCommandModification {
        if let Some(snbt) = self.as_snbt() {
            return DataCommandModification::Value(snbt);
        }

        let (target, path) = self.as_data(datapack, ctx);

        DataCommandModification::From(target.target, Some(path))
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
            (Self::Reference(expression), other) => expression
                .kind
                .perform_arithmetic(datapack, ctx, operator, other),

            (self_, Self::Reference(other)) => {
                self_.perform_arithmetic(datapack, ctx, operator, other.kind)
            }

            (Self::Literal(left), Self::Literal(right)) => Self::Literal(LiteralExpression {
                span: Span::dummy(),
                kind: left.kind.perform_arithmetic(operator, right.kind),
            }),

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
            (Self::Reference(expression), other) => expression
                .kind
                .perform_comparison(datapack, ctx, operator, other),

            (self_, Self::Reference(other)) => {
                self_.perform_comparison(datapack, ctx, operator, other.kind)
            }

            (Self::Literal(left), Self::Literal(right)) => Self::Literal(LiteralExpression {
                span: Span::dummy(),
                kind: left.kind.compare(operator, right.kind).unwrap(),
            }),
            (left_kind @ Self::Data(_, _), right_kind)
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
                    ExecuteIfSubcommand::Score(
                        unique_score.score,
                        ScoreComparison::Range(IntegerRange::new_single(0)),
                        None,
                    ),
                )
            }
            (self_ @ Self::Data(_, _), other) | (other, self_ @ Self::Data(_, _)) => {
                let score = self_.as_score(datapack, ctx, false);

                let other_score = other.as_score(datapack, ctx, false);

                Self::Condition(
                    operator.should_execute_if_be_inverted(),
                    ExecuteIfSubcommand::Score(
                        score.score,
                        ScoreComparison::Score(
                            operator.into_score_comparison_operator(),
                            other_score.score,
                        ),
                        None,
                    ),
                )
            }
            (Self::PlayerScore(score), other) => {
                let right_score = other.as_score(datapack, ctx, false);

                Self::Condition(
                    operator.should_execute_if_be_inverted(),
                    ExecuteIfSubcommand::Score(
                        score.score,
                        ScoreComparison::Score(
                            operator.into_score_comparison_operator(),
                            right_score.score,
                        ),
                        None,
                    ),
                )
            }
            (left_kind, Self::PlayerScore(right_score)) => {
                let left_score = left_kind.as_score(datapack, ctx, false);

                Self::Condition(
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

    #[must_use]
    pub fn perform_logical_operation(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        operator: LogicalOperator,
        other: Self,
    ) -> Self {
        if let Self::Reference(self_) = self {
            return self_
                .kind
                .perform_logical_operation(datapack, ctx, operator, other);
        }

        if let Self::Reference(other) = other {
            return self.perform_logical_operation(datapack, ctx, operator, other.kind);
        }

        match operator {
            LogicalOperator::And => {
                let unique_score = datapack.get_unique_score();

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
                Self::Condition(
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

                Self::PlayerScore(unique_score)
            }
        }
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

        if let Self::PlayerScore(source) = self {
            source.operate_on_score(datapack, ctx, target, operator);
        } else {
            let unique_score = datapack.get_unique_score();

            self.assign_to_score(datapack, ctx, unique_score.clone());

            unique_score.operate_on_score(datapack, ctx, target, operator);
        }
    }

    pub fn to_execute_condition(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        inverted: bool,
    ) -> (bool, ExecuteIfSubcommand) {
        if let Some(value) = self.try_as_i32(true) {
            let unique_score = datapack.get_unique_score();

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
            Self::Literal(expression) => expression.kind.to_execute_condition(datapack).unwrap(),
            Self::PlayerScore(score) => (
                !inverted,
                ExecuteIfSubcommand::Score(
                    score.score,
                    ScoreComparison::Range(IntegerRange::new_single(0)),
                    None,
                ),
            ),
            Self::Data(_, _) => {
                let unique_score = datapack.get_unique_score();

                self.assign_to_score(datapack, ctx, unique_score.clone());

                (
                    !inverted,
                    ExecuteIfSubcommand::Score(
                        unique_score.score,
                        ScoreComparison::Range(IntegerRange::new_single(0)),
                        None,
                    ),
                )
            }
            Self::Condition(inner_inverted, condition) => (inverted ^ inner_inverted, condition),
            Self::Command(command) => {
                let unique_score = datapack.get_unique_score();

                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Success,
                        ExecuteStoreSubcommand::Score(
                            unique_score.score.clone(),
                            Box::new(ExecuteSubcommand::Run(Box::new(command))),
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
            Self::Variable(name) => datapack
                .get_variable(&name)
                .unwrap()
                .1
                .kind
                .to_execute_condition(datapack, ctx, inverted),
            Self::List(_)
            | Self::Compound(_)
            | Self::Tuple(_)
            | Self::Unit
            | Self::Reference(_)
            | Self::Dereference(_)
            | Self::Underscore
            | Self::Struct(_, _, _) => {
                unreachable!()
            }
        }
    }

    pub fn as_text_component(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        force_display: bool,
    ) -> SNBT {
        match self {
            Self::PlayerScore(player_score) => player_score.to_text_component(),
            Self::Data(target, path) => {
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
            Self::Literal(expression) => expression.kind.as_text_component(force_display),
            Self::List(constant_expressions) => SNBT::List(
                constant_expressions
                    .into_iter()
                    .map(|expression| expression.kind.as_text_component(datapack, ctx, false))
                    .collect(),
            ),
            Self::Compound(btree_map) => SNBT::Compound(
                btree_map
                    .into_iter()
                    .map(|(key, value)| {
                        (
                            key.snbt_string,
                            value.kind.as_text_component(datapack, ctx, false),
                        )
                    })
                    .collect(),
            ),
            Self::Condition(_, _) | Self::Command(_) => {
                let unique_score = datapack.get_unique_score();

                self.assign_to_score(datapack, ctx, unique_score.clone());

                unique_score.to_text_component()
            }
            Self::Tuple(expressions) => {
                let mut items = Vec::new();

                items.push(SNBT::string("("));

                for (i, expression) in expressions.into_iter().enumerate() {
                    if i != 0 {
                        items.push(SNBT::string(", "));
                    }

                    items.push(expression.kind.as_text_component(datapack, ctx, true));
                }

                items.push(SNBT::string(")"));

                SNBT::List(items)
            }
            Self::Unit => SNBT::string("()"),
            Self::Reference(expression) | Self::Dereference(expression) => expression
                .kind
                .as_text_component(datapack, ctx, force_display),
            Self::Variable(name) => datapack
                .get_variable(&name)
                .unwrap()
                .1
                .kind
                .as_text_component(datapack, ctx, force_display),
            Self::Underscore => unreachable!(),
            Self::Struct(name, generics, fields) => {
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
                    output.push(value.kind.clone().as_text_component(datapack, ctx, true));
                }

                if fields.is_empty() {
                    output.push(SNBT::string("}"));
                } else {
                    output.push(SNBT::string(" }"));
                }

                SNBT::List(output)
            }
        }
    }

    pub fn assign_to_score(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: GeneratedPlayerScore,
    ) {
        match self {
            Self::Literal(expression) => {
                expression.kind.assign_to_score(datapack, ctx, target.score);
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
            Self::Data(data_target, path) => {
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
                            Box::new(ExecuteSubcommand::If(inverted, condition)),
                        ),
                    )),
                );
            }
            Self::Command(command) => {
                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Result,
                        ExecuteStoreSubcommand::Score(
                            target.score,
                            Box::new(ExecuteSubcommand::Run(Box::new(command))),
                        ),
                    )),
                );
            }
            Self::Tuple(_)
            | Self::Unit
            | Self::Dereference(_)
            | Self::Underscore
            | Self::Struct(_, _, _) => unreachable!(),
            Self::Reference(expression) => {
                expression.kind.assign_to_score(datapack, ctx, target);
            }
            Self::Variable(name) => datapack
                .get_variable(&name)
                .unwrap()
                .1
                .kind
                .assign_to_score(datapack, ctx, target),
        }
    }

    pub fn assign_to_data(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
        target: GeneratedDataTarget,
        path: NbtPath,
    ) {
        if let Some(value) = self.as_snbt() {
            ctx.add_command(
                datapack,
                Command::Data(DataCommand::Modify(
                    target.target,
                    path,
                    DataCommandModificationMode::Set,
                    DataCommandModification::Value(value),
                )),
            );

            return;
        }

        match self {
            Self::PlayerScore(score) => {
                score.assign_to_data(datapack, ctx, target, path);
            }
            Self::Data(inner_target, inner_path) => {
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
                    non_constant.kind.assign_to_data(
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
                    non_constant.kind.assign_to_data(
                        datapack,
                        ctx,
                        target.clone(),
                        path.clone().with_node(NbtPathNode::named(key.snbt_string)),
                    );
                }
            }
            Self::Unit | Self::Literal(_) | Self::Underscore | Self::Variable(_) => unreachable!(),
            Self::Reference(expression) => {
                expression.kind.assign_to_data(datapack, ctx, target, path);
            }
            Self::Struct(_, _, fields) => {
                for (key, value) in fields {
                    value.kind.assign_to_data(
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
                    non_constant.kind.assign_to_data(
                        datapack,
                        ctx,
                        target.clone(),
                        path.clone()
                            .with_node(NbtPathNode::Index(Some(SNBT::Integer(index as i32)))),
                    );
                }
            }
            Self::Condition(inverted, execute_if_subcommand) => {
                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Result,
                        ExecuteStoreSubcommand::Data(
                            target.target,
                            path,
                            NumericSNBTType::Integer,
                            NotNan::new(1.0).unwrap(),
                            Box::new(ExecuteSubcommand::If(inverted, execute_if_subcommand)),
                        ),
                    )),
                );
            }
            Self::Command(command) => {
                ctx.add_command(
                    datapack,
                    Command::Execute(ExecuteSubcommand::Store(
                        StoreType::Result,
                        ExecuteStoreSubcommand::Data(
                            target.target,
                            path,
                            NumericSNBTType::Integer,
                            NotNan::new(1.0).unwrap(),
                            Box::new(ExecuteSubcommand::Run(Box::new(command))),
                        ),
                    )),
                );
            }
            Self::Dereference(expression) => expression
                .kind
                .dereference(datapack, ctx)
                .assign_to_data(datapack, ctx, target, path),
        }
    }

    pub fn as_snbt_macros(self, ctx: &mut CompileContext) -> SNBT {
        match self {
            Self::Literal(expression) => expression.kind.into_snbt(),
            Self::List(expressions) => SNBT::List(
                expressions
                    .into_iter()
                    .map(|expression| expression.kind.as_snbt_macros(ctx))
                    .collect(),
            ),
            Self::Compound(compound) => SNBT::Compound(
                compound
                    .into_iter()
                    .map(|(key, value)| (key.snbt_string, value.kind.as_snbt_macros(ctx)))
                    .collect(),
            ),
            _ => ctx.get_macro_snbt(self),
        }
    }

    pub fn as_snbt(&self) -> Option<SNBT> {
        match self {
            Self::Literal(expression) => Some(expression.clone().kind.into_snbt()),

            Self::List(expressions) | Self::Tuple(expressions) => expressions
                .iter()
                .map(|expr| expr.kind.as_snbt())
                .collect::<Option<Vec<_>>>()
                .map(SNBT::List),

            Self::Compound(compound) => compound
                .iter()
                .map(|(key, value)| value.kind.as_snbt().map(|v| (key.snbt_string.clone(), v)))
                .collect::<Option<_>>()
                .map(SNBT::Compound),

            Self::Unit => {
                let mut unit_btreemap = BTreeMap::new();

                unit_btreemap.insert(
                    SNBTString(false, "__kelp_rs_unit__".to_string()),
                    SNBT::Byte(1),
                );

                Some(SNBT::Compound(unit_btreemap))
            }

            _ => None,
        }
    }

    #[must_use]
    pub fn cast_to(self, datapack: &mut HighDatapack, data_type: DataTypeKind) -> Self {
        let self_type = self.infer_data_type(datapack).unwrap();

        if self_type.equals(&data_type) {
            return self;
        }

        match (self, data_type) {
            (Self::Literal(expression), data_type) => Self::Literal(LiteralExpression {
                span: expression.span,
                kind: expression.kind.cast_to(data_type),
            }),

            (self_ @ Self::Data(_, _), DataTypeKind::Data(_)) => self_,

            _ => unreachable!(""),
        }
    }

    #[must_use]
    pub fn as_place(self) -> Place {
        match self {
            Self::PlayerScore(score) => Place::Score(score),
            Self::Data(target, path) => Place::Data(target, path),
            Self::Variable(name) => Place::Variable(name),
            Self::Underscore => Place::Underscore,
            Self::Tuple(expressions) => Place::Tuple(
                expressions
                    .into_iter()
                    .map(|expression| expression.kind.as_place())
                    .collect(),
            ),
            _ => unreachable!("This expression is not a place {:?}", self),
        }
    }

    #[must_use]
    pub fn dereference(self, datapack: &mut HighDatapack, ctx: &mut CompileContext) -> Self {
        match self {
            Self::Variable(name) => datapack.get_variable(&name).unwrap().1.kind,
            Self::Reference(expression) => expression.kind,
            Self::PlayerScore(_) => {
                let unique_score = datapack.get_unique_score();

                self.assign_to_score(datapack, ctx, unique_score.clone());

                Self::PlayerScore(unique_score)
            }
            Self::Data(_, _) => {
                let (unique_target, unique_path) = datapack.get_unique_data();

                self.assign_to_data(datapack, ctx, unique_target.clone(), unique_path.clone());

                Self::Data(unique_target, unique_path)
            }
            _ => unreachable!("This expression cannot be dereferenced {:?}", self),
        }
    }

    #[must_use]
    pub fn resolve(self, datapack: &mut HighDatapack) -> Self {
        match self {
            Self::Variable(name) => {
                datapack
                    .get_variable(&name)
                    .unwrap_or_else(|| {
                        panic!(
                            "Variable '{}' has not been declared in the current scope ",
                            name
                        )
                    })
                    .1
                    .kind
            }
            Self::Reference(expression) => Self::Reference(Box::new(
                expression
                    .kind
                    .resolve(datapack)
                    .into_dummy_constant_expression(),
            )),
            Self::List(expressions) => Self::List(
                expressions
                    .into_iter()
                    .map(|expression| {
                        expression
                            .kind
                            .resolve(datapack)
                            .into_dummy_constant_expression()
                    })
                    .collect(),
            ),
            Self::Compound(compound) => Self::Compound(
                compound
                    .into_iter()
                    .map(|(key, value)| {
                        (
                            key,
                            value
                                .kind
                                .resolve(datapack)
                                .into_dummy_constant_expression(),
                        )
                    })
                    .collect(),
            ),
            Self::Tuple(expressions) => Self::Tuple(
                expressions
                    .into_iter()
                    .map(|expression| {
                        expression
                            .kind
                            .resolve(datapack)
                            .into_dummy_constant_expression()
                    })
                    .collect(),
            ),
            Self::Dereference(expression) => Self::Dereference(Box::new(
                expression
                    .kind
                    .resolve(datapack)
                    .into_dummy_constant_expression(),
            )),
            _ => self,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub struct ConstantExpression {
    pub span: Span,
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
            ConstantExpressionKind::Unit => Some(()),
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
                    return ctx.add_info(SemanticAnalysisInfo {
                        span: self.span,
                        kind: SemanticAnalysisInfoKind::Error(
                            SemanticAnalysisError::UndeclaredVariable(name.clone()),
                        ),
                    });
                }

                Some(())
            }
            ConstantExpressionKind::PlayerScore(_)
            | ConstantExpressionKind::Data(_, _)
            | ConstantExpressionKind::List(_)
            | ConstantExpressionKind::Compound(_)
            | ConstantExpressionKind::Condition(_, _)
            | ConstantExpressionKind::Command(_)
            | ConstantExpressionKind::Tuple(_)
            | ConstantExpressionKind::Underscore
            | ConstantExpressionKind::Struct(_, _, _) => unreachable!(),
        }
    }

    #[must_use]
    pub const fn into_constant_expression(self) -> Expression {
        Expression {
            span: self.span,
            kind: ExpressionKind::Constant(self),
        }
    }
}
