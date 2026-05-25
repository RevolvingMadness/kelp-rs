use la_arena::Idx;
use minecraft_command_types::command::{
    ScoreValue, enums::score_operation_operator::ScoreOperationOperator,
};

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    high::{
        entity_selector::EntitySelector, expression::Expression, player_score::PlayerScore,
        semantic_analysis::SemanticAnalysisContext,
    },
    low::expression::command::scoreboard::players::{
        PlayersDisplayScoreboardCommand as MiddlePlayersDisplayScoreboardCommand,
        PlayersScoreboardCommand as MiddlePlayersScoreboardCommand,
        ScoreboardNumberFormat as MiddleScoreboardNumberFormat,
    },
};

#[derive(Debug, Clone, Copy)]
pub enum ScoreboardNumberFormat {
    Blank,
    Fixed(Idx<Expression>),
    Styled(Idx<Expression>),
}

impl ScoreboardNumberFormat {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleScoreboardNumberFormat> {
        Some(match self {
            Self::Blank => MiddleScoreboardNumberFormat::Blank,
            Self::Fixed(expression) => {
                let expression = Expression::perform_semantic_analysis(
                    expression,
                    high_allocator,
                    low_allocator,
                    ctx,
                )?;

                MiddleScoreboardNumberFormat::Fixed(expression)
            }
            Self::Styled(expression) => {
                let expression = Expression::perform_semantic_analysis(
                    expression,
                    high_allocator,
                    low_allocator,
                    ctx,
                )?;

                MiddleScoreboardNumberFormat::Styled(expression)
            }
        })
    }
}

#[derive(Debug, Clone)]
pub enum PlayersDisplayScoreboardCommand {
    Name(PlayerScore, Option<Idx<Expression>>),
    NumberFormat(PlayerScore, Option<ScoreboardNumberFormat>),
}

impl PlayersDisplayScoreboardCommand {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddlePlayersDisplayScoreboardCommand> {
        Some(match self {
            Self::Name(score, expression) => {
                let score = score.perform_semantic_analysis(high_allocator, low_allocator, ctx);
                let expression = match expression {
                    Some(expression) => {
                        let expression = Expression::perform_semantic_analysis(
                            expression,
                            high_allocator,
                            low_allocator,
                            ctx,
                        )?;

                        Some(expression)
                    }
                    None => None,
                };

                let score = score?;

                MiddlePlayersDisplayScoreboardCommand::Name(score, expression)
            }
            Self::NumberFormat(score, number_format) => {
                let score = score.perform_semantic_analysis(high_allocator, low_allocator, ctx);
                let number_format = match number_format {
                    Some(number_format) => Some(number_format.perform_semantic_analysis(
                        high_allocator,
                        low_allocator,
                        ctx,
                    )?),
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
    List(Option<EntitySelector>),
    Get(PlayerScore),
    Set(PlayerScore, ScoreValue),
    Add(PlayerScore, ScoreValue),
    Remove(PlayerScore, ScoreValue),
    Reset(EntitySelector, Option<String>),
    Enable(PlayerScore),
    Operation(PlayerScore, ScoreOperationOperator, PlayerScore),
    Display(Box<PlayersDisplayScoreboardCommand>),
}

impl PlayersScoreboardCommand {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddlePlayersScoreboardCommand> {
        Some(match self {
            Self::List(selector) => {
                let selector = match selector {
                    Some(selector) => Some(selector.perform_semantic_analysis(
                        high_allocator,
                        low_allocator,
                        ctx,
                    )?),
                    None => None,
                };

                MiddlePlayersScoreboardCommand::List(selector)
            }
            Self::Get(score) => {
                let score = score.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                MiddlePlayersScoreboardCommand::Get(score)
            }
            Self::Set(score, value) => {
                let score = score.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                MiddlePlayersScoreboardCommand::Set(score, value)
            }
            Self::Add(score, amount) => {
                let score = score.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                MiddlePlayersScoreboardCommand::Add(score, amount)
            }
            Self::Remove(score, amount) => {
                let score = score.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                MiddlePlayersScoreboardCommand::Remove(score, amount)
            }
            Self::Reset(selector, objective) => {
                let selector =
                    selector.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                MiddlePlayersScoreboardCommand::Reset(selector, objective)
            }
            Self::Enable(score) => {
                let score = score.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                MiddlePlayersScoreboardCommand::Enable(score)
            }
            Self::Operation(left, operator, right) => {
                let left = left.perform_semantic_analysis(high_allocator, low_allocator, ctx);
                let right = right.perform_semantic_analysis(high_allocator, low_allocator, ctx);

                let left = left?;
                let right = right?;

                MiddlePlayersScoreboardCommand::Operation(left, operator, right)
            }
            Self::Display(command) => {
                let command =
                    command.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                MiddlePlayersScoreboardCommand::Display(Box::new(command))
            }
        })
    }
}
