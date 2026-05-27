use minecraft_command_types::command::{
    ScoreValue, enums::score_operation_operator::ScoreOperationOperator,
};

use crate::{
    parsed::{
        entity_selector::ParsedEntitySelector, expression::ParsedExpression,
        player_score::ParsedPlayerScore, semantic_analysis::SemanticAnalysisContext,
    },
    semantic::expression::command::scoreboard::players::{
        SemanticPlayersDisplayScoreboardCommand, SemanticPlayersScoreboardCommand,
        SemanticScoreboardNumberFormat,
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
    ) -> Option<SemanticScoreboardNumberFormat> {
        Some(match self {
            Self::Blank => SemanticScoreboardNumberFormat::Blank,
            Self::Fixed(expression) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                SemanticScoreboardNumberFormat::Fixed(expression)
            }
            Self::Styled(expression) => {
                let (_, expression) = expression.perform_semantic_analysis(ctx)?;

                SemanticScoreboardNumberFormat::Styled(expression)
            }
        })
    }
}

#[derive(Debug, Clone)]
pub enum PlayersDisplayScoreboardCommand {
    Name(ParsedPlayerScore, Option<ParsedExpression>),
    NumberFormat(ParsedPlayerScore, Option<ScoreboardNumberFormat>),
}

impl PlayersDisplayScoreboardCommand {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticPlayersDisplayScoreboardCommand> {
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

                SemanticPlayersDisplayScoreboardCommand::Name(score, expression)
            }
            Self::NumberFormat(score, number_format) => {
                let score = score.perform_semantic_analysis(ctx);
                let number_format = match number_format {
                    Some(number_format) => Some(number_format.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                let score = score?;

                SemanticPlayersDisplayScoreboardCommand::NumberFormat(score, number_format)
            }
        })
    }
}

#[derive(Debug, Clone)]
pub enum PlayersScoreboardCommand {
    List(Option<ParsedEntitySelector>),
    Get(ParsedPlayerScore),
    Set(ParsedPlayerScore, ScoreValue),
    Add(ParsedPlayerScore, ScoreValue),
    Remove(ParsedPlayerScore, ScoreValue),
    Reset(ParsedEntitySelector, Option<String>),
    Enable(ParsedPlayerScore),
    Operation(ParsedPlayerScore, ScoreOperationOperator, ParsedPlayerScore),
    Display(Box<PlayersDisplayScoreboardCommand>),
}

impl PlayersScoreboardCommand {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticPlayersScoreboardCommand> {
        Some(match self {
            Self::List(selector) => {
                let selector = match selector {
                    Some(selector) => Some(selector.perform_semantic_analysis(ctx)?),
                    None => None,
                };

                SemanticPlayersScoreboardCommand::List(selector)
            }
            Self::Get(score) => {
                let score = score.perform_semantic_analysis(ctx)?;

                SemanticPlayersScoreboardCommand::Get(score)
            }
            Self::Set(score, value) => {
                let score = score.perform_semantic_analysis(ctx)?;

                SemanticPlayersScoreboardCommand::Set(score, value)
            }
            Self::Add(score, amount) => {
                let score = score.perform_semantic_analysis(ctx)?;

                SemanticPlayersScoreboardCommand::Add(score, amount)
            }
            Self::Remove(score, amount) => {
                let score = score.perform_semantic_analysis(ctx)?;

                SemanticPlayersScoreboardCommand::Remove(score, amount)
            }
            Self::Reset(selector, objective) => {
                let selector = selector.perform_semantic_analysis(ctx)?;

                SemanticPlayersScoreboardCommand::Reset(selector, objective)
            }
            Self::Enable(score) => {
                let score = score.perform_semantic_analysis(ctx)?;

                SemanticPlayersScoreboardCommand::Enable(score)
            }
            Self::Operation(left, operator, right) => {
                let left = left.perform_semantic_analysis(ctx);
                let right = right.perform_semantic_analysis(ctx);

                let left = left?;
                let right = right?;

                SemanticPlayersScoreboardCommand::Operation(left, operator, right)
            }
            Self::Display(command) => {
                let command = command.perform_semantic_analysis(ctx)?;

                SemanticPlayersScoreboardCommand::Display(Box::new(command))
            }
        })
    }
}
