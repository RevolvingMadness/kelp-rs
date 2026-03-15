use minecraft_command_types::command::enums::score_operation_operator::ScoreOperationOperator;
use minecraft_command_types_derive::HasMacro;

use crate::{
    high::{entity_selector::EntitySelector, expression::Expression, player_score::PlayerScore},
    middle::expression::command::scoreboard::players::{
        PlayersDisplayScoreboardCommand as MiddlePlayersDisplayScoreboardCommand,
        PlayersScoreboardCommand as MiddlePlayersScoreboardCommand,
        ScoreboardNumberFormat as MiddleScoreboardNumberFormat,
    },
    semantic_analysis_context::SemanticAnalysisContext,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum ScoreboardNumberFormat {
    Blank,
    Fixed(Expression),
    Styled(Expression),
}

impl ScoreboardNumberFormat {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<MiddleScoreboardNumberFormat> {
        Some(match self {
            Self::Blank => MiddleScoreboardNumberFormat::Blank,
            Self::Fixed(expression) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx, is_lhs)?;

                MiddleScoreboardNumberFormat::Fixed(expression)
            }
            Self::Styled(expression) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx, is_lhs)?;

                MiddleScoreboardNumberFormat::Styled(expression)
            }
        })
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum PlayersDisplayScoreboardCommand {
    Name(PlayerScore, Option<Expression>),
    NumberFormat(PlayerScore, Option<ScoreboardNumberFormat>),
}

impl PlayersDisplayScoreboardCommand {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<MiddlePlayersDisplayScoreboardCommand> {
        Some(match self {
            Self::Name(score, expression) => {
                let score = score.perform_semantic_analysis(ctx, is_lhs);
                let expression = match expression {
                    Some(expression) => {
                        let (_, expression) = expression.perform_semantic_analysis(ctx, is_lhs)?;

                        Some(expression)
                    }
                    None => None,
                };

                let score = score?;

                MiddlePlayersDisplayScoreboardCommand::Name(score, expression)
            }
            Self::NumberFormat(score, number_format) => {
                let score = score.perform_semantic_analysis(ctx, is_lhs);
                let number_format = match number_format {
                    Some(number_format) => {
                        Some(number_format.perform_semantic_analysis(ctx, is_lhs)?)
                    }
                    None => None,
                };

                let score = score?;

                MiddlePlayersDisplayScoreboardCommand::NumberFormat(score, number_format)
            }
        })
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum PlayersScoreboardCommand {
    List(Option<EntitySelector>),
    Get(PlayerScore),
    Set(PlayerScore, i32),
    Add(PlayerScore, i32),
    Remove(PlayerScore, i32),
    Reset(EntitySelector, Option<String>),
    Enable(PlayerScore),
    Operation(PlayerScore, ScoreOperationOperator, PlayerScore),
    Display(Box<PlayersDisplayScoreboardCommand>),
}

impl PlayersScoreboardCommand {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<MiddlePlayersScoreboardCommand> {
        Some(match self {
            Self::List(selector) => {
                let selector = match selector {
                    Some(selector) => Some(selector.perform_semantic_analysis(ctx, is_lhs)?),
                    None => None,
                };

                MiddlePlayersScoreboardCommand::List(selector)
            }
            Self::Get(score) => {
                let score = score.perform_semantic_analysis(ctx, is_lhs)?;

                MiddlePlayersScoreboardCommand::Get(score)
            }
            Self::Set(score, value) => {
                let score = score.perform_semantic_analysis(ctx, is_lhs)?;

                MiddlePlayersScoreboardCommand::Set(score, value)
            }
            Self::Add(score, amount) => {
                let score = score.perform_semantic_analysis(ctx, is_lhs)?;

                MiddlePlayersScoreboardCommand::Add(score, amount)
            }
            Self::Remove(score, amount) => {
                let score = score.perform_semantic_analysis(ctx, is_lhs)?;

                MiddlePlayersScoreboardCommand::Remove(score, amount)
            }
            Self::Reset(selector, objective) => {
                let selector = selector.perform_semantic_analysis(ctx, is_lhs)?;

                MiddlePlayersScoreboardCommand::Reset(selector, objective)
            }
            Self::Enable(score) => {
                let score = score.perform_semantic_analysis(ctx, is_lhs)?;

                MiddlePlayersScoreboardCommand::Enable(score)
            }
            Self::Operation(left, operator, right) => {
                let left = left.perform_semantic_analysis(ctx, is_lhs);
                let right = right.perform_semantic_analysis(ctx, is_lhs);

                let left = left?;
                let right = right?;

                MiddlePlayersScoreboardCommand::Operation(left, operator, right)
            }
            Self::Display(command) => {
                let command = command.perform_semantic_analysis(ctx, is_lhs)?;

                MiddlePlayersScoreboardCommand::Display(Box::new(command))
            }
        })
    }
}
