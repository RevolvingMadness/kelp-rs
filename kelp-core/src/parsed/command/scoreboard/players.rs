use minecraft_command_types::command::{
    ScoreValue, enums::score_operation_operator::ScoreOperationOperator,
};

use crate::{
    parsed::arena::ParsedAstArena,
    parsed::{
        entity_selector::EntitySelector,
        expression::{ParsedExpression, ParsedExpressionId},
        player_score::PlayerScore,
        semantic_analysis::SemanticAnalysisContext,
    },
    typed::arena::TypedAstArena,
    typed::expression::command::scoreboard::players::{
        TypedPlayersDisplayScoreboardCommand, TypedPlayersScoreboardCommand,
        TypedScoreboardNumberFormat,
    },
};

#[derive(Debug, Clone, Copy)]
pub enum ParsedScoreboardNumberFormat {
    Blank,
    Fixed(ParsedExpressionId),
    Styled(ParsedExpressionId),
}

impl ParsedScoreboardNumberFormat {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedScoreboardNumberFormat> {
        Some(match self {
            Self::Blank => TypedScoreboardNumberFormat::Blank,
            Self::Fixed(expression) => {
                let expression = ParsedExpression::perform_semantic_analysis(
                    expression,
                    parsed_arena,
                    typed_arena,
                    ctx,
                )?;

                TypedScoreboardNumberFormat::Fixed(expression)
            }
            Self::Styled(expression) => {
                let expression = ParsedExpression::perform_semantic_analysis(
                    expression,
                    parsed_arena,
                    typed_arena,
                    ctx,
                )?;

                TypedScoreboardNumberFormat::Styled(expression)
            }
        })
    }
}

#[derive(Debug, Clone)]
pub enum ParsedPlayersDisplayScoreboardCommand {
    Name(PlayerScore, Option<ParsedExpressionId>),
    NumberFormat(PlayerScore, Option<ParsedScoreboardNumberFormat>),
}

impl ParsedPlayersDisplayScoreboardCommand {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedPlayersDisplayScoreboardCommand> {
        Some(match self {
            Self::Name(score, expression) => {
                let score = score.perform_semantic_analysis(parsed_arena, typed_arena, ctx);
                let expression = match expression {
                    Some(expression) => {
                        let expression = ParsedExpression::perform_semantic_analysis(
                            expression,
                            parsed_arena,
                            typed_arena,
                            ctx,
                        )?;

                        Some(expression)
                    }
                    None => None,
                };

                let score = score?;

                TypedPlayersDisplayScoreboardCommand::Name(score, expression)
            }
            Self::NumberFormat(score, number_format) => {
                let score = score.perform_semantic_analysis(parsed_arena, typed_arena, ctx);
                let number_format = match number_format {
                    Some(number_format) => Some(number_format.perform_semantic_analysis(
                        parsed_arena,
                        typed_arena,
                        ctx,
                    )?),
                    None => None,
                };

                let score = score?;

                TypedPlayersDisplayScoreboardCommand::NumberFormat(score, number_format)
            }
        })
    }
}

#[derive(Debug, Clone)]
pub enum ParsedPlayersScoreboardCommand {
    List(Option<EntitySelector>),
    Get(PlayerScore),
    Set(PlayerScore, ScoreValue),
    Add(PlayerScore, ScoreValue),
    Remove(PlayerScore, ScoreValue),
    Reset(EntitySelector, Option<String>),
    Enable(PlayerScore),
    Operation(PlayerScore, ScoreOperationOperator, PlayerScore),
    Display(Box<ParsedPlayersDisplayScoreboardCommand>),
}

impl ParsedPlayersScoreboardCommand {
    #[must_use]
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedPlayersScoreboardCommand> {
        Some(match self {
            Self::List(selector) => {
                let selector = match selector {
                    Some(selector) => {
                        Some(selector.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?)
                    }
                    None => None,
                };

                TypedPlayersScoreboardCommand::List(selector)
            }
            Self::Get(score) => {
                let score = score.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedPlayersScoreboardCommand::Get(score)
            }
            Self::Set(score, value) => {
                let score = score.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedPlayersScoreboardCommand::Set(score, value)
            }
            Self::Add(score, amount) => {
                let score = score.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedPlayersScoreboardCommand::Add(score, amount)
            }
            Self::Remove(score, amount) => {
                let score = score.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedPlayersScoreboardCommand::Remove(score, amount)
            }
            Self::Reset(selector, objective) => {
                let selector =
                    selector.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedPlayersScoreboardCommand::Reset(selector, objective)
            }
            Self::Enable(score) => {
                let score = score.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedPlayersScoreboardCommand::Enable(score)
            }
            Self::Operation(left, operator, right) => {
                let left = left.perform_semantic_analysis(parsed_arena, typed_arena, ctx);
                let right = right.perform_semantic_analysis(parsed_arena, typed_arena, ctx);

                let left = left?;
                let right = right?;

                TypedPlayersScoreboardCommand::Operation(left, operator, right)
            }
            Self::Display(command) => {
                let command = command.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedPlayersScoreboardCommand::Display(Box::new(command))
            }
        })
    }
}
