use minecraft_command_types::command::{
    enums::score_operation_operator::ScoreOperationOperator,
    scoreboard::{
        PlayersDisplayScoreboardCommand, PlayersScoreboardCommand, ScoreboardNumberFormat,
    },
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext,
    data_type::DataTypeKind,
    datapack::HighDatapack,
    expression::Expression,
    high::{entity_selector::HighEntitySelector, player_score::HighPlayerScore},
    semantic_analysis_context::SemanticAnalysisContext,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighScoreboardNumberFormat {
    Blank,
    Fixed(Expression),
    Styled(Expression),
}

impl HighScoreboardNumberFormat {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            Self::Blank => Some(()),
            Self::Fixed(expression) | Self::Styled(expression) => {
                expression.perform_semantic_analysis(ctx, is_lhs, Some(&DataTypeKind::SNBT))
            }
        }
    }

    pub fn compile(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> ScoreboardNumberFormat {
        match self {
            Self::Blank => ScoreboardNumberFormat::Blank,
            Self::Fixed(expression) => {
                ScoreboardNumberFormat::Fixed(expression.resolve(datapack, ctx).as_snbt_macros(ctx))
            }
            Self::Styled(expression) => ScoreboardNumberFormat::Styled(
                expression.resolve(datapack, ctx).as_snbt_macros(ctx),
            ),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighPlayersDisplayScoreboardCommand {
    Name(HighPlayerScore, Option<Expression>),
    NumberFormat(HighPlayerScore, Option<HighScoreboardNumberFormat>),
}

impl HighPlayersDisplayScoreboardCommand {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            Self::Name(score, expression) => {
                let score_result = score.perform_semantic_analysis(ctx, is_lhs);
                let expression_result = expression.as_ref().map_or(Some(()), |expression| {
                    expression.perform_semantic_analysis(ctx, is_lhs, Some(&DataTypeKind::SNBT))
                });

                score_result?;
                expression_result?;

                Some(())
            }
            Self::NumberFormat(score, number_format) => {
                let score_result = score.perform_semantic_analysis(ctx, is_lhs);
                let number_format_result =
                    number_format.as_ref().map_or(Some(()), |number_format| {
                        number_format.perform_semantic_analysis(ctx, is_lhs)
                    });

                score_result?;
                number_format_result?;

                Some(())
            }
        }
    }

    pub fn compile(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> PlayersDisplayScoreboardCommand {
        match self {
            Self::Name(score, expression) => {
                let score = score.compile(datapack, ctx);

                PlayersDisplayScoreboardCommand::Name(
                    score.score,
                    expression.map(|e| e.resolve(datapack, ctx).as_snbt_macros(ctx)),
                )
            }
            Self::NumberFormat(score, number_format) => {
                let score = score.compile(datapack, ctx);

                PlayersDisplayScoreboardCommand::NumberFormat(
                    score.score,
                    number_format.map(|number_format| number_format.compile(datapack, ctx)),
                )
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighPlayersScoreboardCommand {
    List(Option<HighEntitySelector>),
    Get(HighPlayerScore),
    Set(HighPlayerScore, i32),
    Add(HighPlayerScore, i32),
    Remove(HighPlayerScore, i32),
    Reset(HighEntitySelector, Option<String>),
    Enable(HighPlayerScore),
    Operation(HighPlayerScore, ScoreOperationOperator, HighPlayerScore),
    Display(Box<HighPlayersDisplayScoreboardCommand>),
}

impl HighPlayersScoreboardCommand {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            Self::List(selector) => selector.as_ref().map_or(Some(()), |selector| {
                selector.perform_semantic_analysis(ctx, is_lhs)
            }),
            Self::Get(score)
            | Self::Set(score, _)
            | Self::Add(score, _)
            | Self::Remove(score, _)
            | Self::Enable(score) => score.perform_semantic_analysis(ctx, is_lhs),
            Self::Reset(selector, _) => selector.perform_semantic_analysis(ctx, is_lhs),
            Self::Operation(left, _, right) => {
                let left_result = left.perform_semantic_analysis(ctx, is_lhs);
                let right_result = right.perform_semantic_analysis(ctx, is_lhs);

                left_result?;
                right_result?;

                Some(())
            }
            Self::Display(command) => command.perform_semantic_analysis(ctx, is_lhs),
        }
    }

    pub fn compile(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> PlayersScoreboardCommand {
        match self {
            Self::List(high_entity_selector) => {
                let compiled_selector =
                    high_entity_selector.map(|selector| selector.compile(datapack, ctx));

                PlayersScoreboardCommand::List(compiled_selector)
            }
            Self::Get(score) => {
                let score = score.compile(datapack, ctx);

                PlayersScoreboardCommand::Get(score.score)
            }
            Self::Set(score, value) => {
                let score = score.compile(datapack, ctx);

                PlayersScoreboardCommand::Set(score.score, value)
            }
            Self::Add(score, value) => {
                let score = score.compile(datapack, ctx);

                PlayersScoreboardCommand::Add(score.score, value)
            }
            Self::Remove(score, value) => {
                let score = score.compile(datapack, ctx);

                PlayersScoreboardCommand::Remove(score.score, value)
            }
            Self::Reset(high_entity_selector, objective) => PlayersScoreboardCommand::Reset(
                high_entity_selector.compile(datapack, ctx),
                objective,
            ),
            Self::Enable(score) => {
                let score = score.compile(datapack, ctx);

                PlayersScoreboardCommand::Enable(score.score)
            }
            Self::Operation(left_score, operator, right_score) => {
                let left_score = left_score.compile(datapack, ctx);
                let right_score = right_score.compile(datapack, ctx);

                PlayersScoreboardCommand::Operation(left_score.score, operator, right_score.score)
            }
            Self::Display(high_players_display_scoreboard_command) => {
                let compiled_high_players_display_scoreboard_command =
                    high_players_display_scoreboard_command.compile(datapack, ctx);

                PlayersScoreboardCommand::Display(compiled_high_players_display_scoreboard_command)
            }
        }
    }
}
