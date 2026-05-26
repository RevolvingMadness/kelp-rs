use minecraft_command_types::command::{
    ScoreValue, enums::score_operation_operator::ScoreOperationOperator,
};

use crate::{
    parsed::{
        entity_selector::ParsedEntitySelector, expression::ParsedExpression,
        player_score::PlayerScore, semantic_analysis::SemanticAnalysisContext,
    },
    semantic::expression::command::scoreboard::players::{
        SemanticPlayersDisplayScoreboardCommand as MiddlePlayersDisplayScoreboardCommand,
        SemanticPlayersScoreboardCommand as MiddlePlayersScoreboardCommand,
        SemanticScoreboardNumberFormat as MiddleScoreboardNumberFormat,
    },
};

#[derive(Debug, Clone)]
pub enum ScoreboardNumberFormat {
    Blank,
    Fixed(ParsedExpression),
    Styled(ParsedExpression),
}

impl ScoreboardNumberFormat {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleScoreboardNumberFormat> {
        Some(match self {
            Self::Blank => MiddleScoreboardNumberFormat::Blank,
            Self::Fixed(expression) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                MiddleScoreboardNumberFormat::Fixed(expression)
            }
            Self::Styled(expression) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                MiddleScoreboardNumberFormat::Styled(expression)
            }
        })
    }
}

#[derive(Debug, Clone)]
pub enum PlayersDisplayScoreboardCommand {
    Name(PlayerScore, Option<ParsedExpression>),
    NumberFormat(PlayerScore, Option<ScoreboardNumberFormat>),
}

impl PlayersDisplayScoreboardCommand {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddlePlayersDisplayScoreboardCommand> {
        Some(match self {
            Self::Name(score, expression) => {
                let score = score.perform_semantic_analysis(ctx);
                let expression = match expression {
                    Some(expression) => {
                        let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                        Some(expression)
                    }
                    None => None,
                };

                let score = score?;

                MiddlePlayersDisplayScoreboardCommand::Name(score, expression)
            }
            Self::NumberFormat(score, number_format) => {
                let score = score.perform_semantic_analysis(ctx);
                let number_format = match number_format {
                    Some(number_format) => Some(number_format.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                let score = score?;

                MiddlePlayersDisplayScoreboardCommand::NumberFormat(score, number_format)
            }
        })
    }
}

#[derive(Debug, Clone)]
pub enum PlayersScoreboardCommand {
    List(Option<ParsedEntitySelector>),
    Get(PlayerScore),
    Set(PlayerScore, ScoreValue),
    Add(PlayerScore, ScoreValue),
    Remove(PlayerScore, ScoreValue),
    Reset(ParsedEntitySelector, Option<String>),
    Enable(PlayerScore),
    Operation(PlayerScore, ScoreOperationOperator, PlayerScore),
    Display(Box<PlayersDisplayScoreboardCommand>),
}

impl PlayersScoreboardCommand {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddlePlayersScoreboardCommand> {
        Some(match self {
            Self::List(selector) => {
                let selector = match selector {
                    Some(selector) => Some(selector.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                MiddlePlayersScoreboardCommand::List(selector)
            }
            Self::Get(score) => {
                let score = score.perform_semantic_analysis(ctx)?;

                MiddlePlayersScoreboardCommand::Get(score)
            }
            Self::Set(score, value) => {
                let score = score.perform_semantic_analysis(ctx)?;

                MiddlePlayersScoreboardCommand::Set(score, value)
            }
            Self::Add(score, amount) => {
                let score = score.perform_semantic_analysis(ctx)?;

                MiddlePlayersScoreboardCommand::Add(score, amount)
            }
            Self::Remove(score, amount) => {
                let score = score.perform_semantic_analysis(ctx)?;

                MiddlePlayersScoreboardCommand::Remove(score, amount)
            }
            Self::Reset(selector, objective) => {
                let selector = selector.perform_semantic_analysis(ctx)?;

                MiddlePlayersScoreboardCommand::Reset(selector, objective)
            }
            Self::Enable(score) => {
                let score = score.perform_semantic_analysis(ctx)?;

                MiddlePlayersScoreboardCommand::Enable(score)
            }
            Self::Operation(left, operator, right) => {
                let left = left.perform_semantic_analysis(ctx);
                let right = right.perform_semantic_analysis(ctx);

                let left = left?;
                let right = right?;

                MiddlePlayersScoreboardCommand::Operation(left, operator, right)
            }
            Self::Display(command) => {
                let command = command.perform_semantic_analysis(ctx)?;

                MiddlePlayersScoreboardCommand::Display(Box::new(command))
            }
        })
    }
}
