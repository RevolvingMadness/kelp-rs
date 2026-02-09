use minecraft_command_types::command::{
    enums::score_operation_operator::ScoreOperationOperator,
    scoreboard::{
        PlayersDisplayScoreboardCommand, PlayersScoreboardCommand, ScoreboardNumberFormat,
    },
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext,
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
    pub fn perform_semantic_analysis(&self, ctx: &mut SemanticAnalysisContext, is_lhs: bool) -> Option<()> {
        match self {
            HighScoreboardNumberFormat::Blank => Some(()),
            HighScoreboardNumberFormat::Fixed(expression) => {
                expression.perform_semantic_analysis(ctx, is_lhs)
            }
            HighScoreboardNumberFormat::Styled(expression) => {
                expression.perform_semantic_analysis(ctx, is_lhs)
            }
        }
    }

    pub fn compile(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> ScoreboardNumberFormat {
        match self {
            HighScoreboardNumberFormat::Blank => ScoreboardNumberFormat::Blank,
            HighScoreboardNumberFormat::Fixed(expression) => ScoreboardNumberFormat::Fixed(
                expression.resolve(datapack, ctx).kind.as_snbt_macros(ctx),
            ),
            HighScoreboardNumberFormat::Styled(expression) => ScoreboardNumberFormat::Styled(
                expression.resolve(datapack, ctx).kind.as_snbt_macros(ctx),
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
    pub fn perform_semantic_analysis(&self, ctx: &mut SemanticAnalysisContext, is_lhs: bool) -> Option<()> {
        match self {
            HighPlayersDisplayScoreboardCommand::Name(score, expression) => {
                let score_result = score.perform_semantic_analysis(ctx, is_lhs);
                let expression_result = expression
                    .as_ref()
                    .map(|expression| expression.perform_semantic_analysis(ctx, is_lhs))
                    .unwrap_or(Some(()));

                score_result?;
                expression_result?;

                Some(())
            }
            HighPlayersDisplayScoreboardCommand::NumberFormat(score, number_format) => {
                let score_result = score.perform_semantic_analysis(ctx, is_lhs);
                let number_format_result = number_format
                    .as_ref()
                    .map(|number_format| number_format.perform_semantic_analysis(ctx, is_lhs))
                    .unwrap_or(Some(()));

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
            HighPlayersDisplayScoreboardCommand::Name(player_score, expression) => {
                PlayersDisplayScoreboardCommand::Name(
                    player_score.compile(datapack, ctx),
                    expression.map(|e| e.resolve(datapack, ctx).kind.as_snbt_macros(ctx)),
                )
            }
            HighPlayersDisplayScoreboardCommand::NumberFormat(high_player_score, number_format) => {
                let compiled_player_score = high_player_score.compile(datapack, ctx);

                PlayersDisplayScoreboardCommand::NumberFormat(
                    compiled_player_score,
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
    pub fn perform_semantic_analysis(&self, ctx: &mut SemanticAnalysisContext, is_lhs: bool) -> Option<()> {
        match self {
            HighPlayersScoreboardCommand::List(selector) => selector
                .as_ref()
                .map(|selector| selector.perform_semantic_analysis(ctx, is_lhs))
                .unwrap_or(Some(())),
            HighPlayersScoreboardCommand::Get(score) => score.perform_semantic_analysis(ctx, is_lhs),
            HighPlayersScoreboardCommand::Set(score, _) => score.perform_semantic_analysis(ctx, is_lhs),
            HighPlayersScoreboardCommand::Add(score, _) => score.perform_semantic_analysis(ctx, is_lhs),
            HighPlayersScoreboardCommand::Remove(score, _) => score.perform_semantic_analysis(ctx, is_lhs),
            HighPlayersScoreboardCommand::Reset(selector, _) => {
                selector.perform_semantic_analysis(ctx, is_lhs)
            }
            HighPlayersScoreboardCommand::Enable(score) => score.perform_semantic_analysis(ctx, is_lhs),
            HighPlayersScoreboardCommand::Operation(left, _, right) => {
                let left_result = left.perform_semantic_analysis(ctx, is_lhs);
                let right_result = right.perform_semantic_analysis(ctx, is_lhs);

                left_result?;
                right_result?;

                Some(())
            }
            HighPlayersScoreboardCommand::Display(command) => {
                command.perform_semantic_analysis(ctx, is_lhs)
            }
        }
    }

    pub fn compile(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> PlayersScoreboardCommand {
        match self {
            HighPlayersScoreboardCommand::List(high_entity_selector) => {
                let compiled_selector =
                    high_entity_selector.map(|selector| selector.compile(datapack, ctx));

                PlayersScoreboardCommand::List(compiled_selector)
            }
            HighPlayersScoreboardCommand::Get(high_player_score) => {
                let compiled_player_score = high_player_score.compile(datapack, ctx);

                PlayersScoreboardCommand::Get(compiled_player_score)
            }
            HighPlayersScoreboardCommand::Set(high_player_score, value) => {
                let compiled_player_score = high_player_score.compile(datapack, ctx);

                PlayersScoreboardCommand::Set(compiled_player_score, value)
            }
            HighPlayersScoreboardCommand::Add(high_player_score, value) => {
                let compiled_player_score = high_player_score.compile(datapack, ctx);

                PlayersScoreboardCommand::Add(compiled_player_score, value)
            }
            HighPlayersScoreboardCommand::Remove(high_player_score, value) => {
                let compiled_player_score = high_player_score.compile(datapack, ctx);

                PlayersScoreboardCommand::Remove(compiled_player_score, value)
            }
            HighPlayersScoreboardCommand::Reset(high_entity_selector, objective) => {
                PlayersScoreboardCommand::Reset(
                    high_entity_selector.compile(datapack, ctx),
                    objective,
                )
            }
            HighPlayersScoreboardCommand::Enable(high_player_score) => {
                let compiled_player_score = high_player_score.compile(datapack, ctx);

                PlayersScoreboardCommand::Enable(compiled_player_score)
            }
            HighPlayersScoreboardCommand::Operation(
                high_player_score,
                score_operation_operator,
                high_player_score1,
            ) => {
                let compiled_player_score = high_player_score.compile(datapack, ctx);
                let compiled_player_score1 = high_player_score1.compile(datapack, ctx);

                PlayersScoreboardCommand::Operation(
                    compiled_player_score,
                    score_operation_operator,
                    compiled_player_score1,
                )
            }
            HighPlayersScoreboardCommand::Display(high_players_display_scoreboard_command) => {
                let compiled_high_players_display_scoreboard_command =
                    high_players_display_scoreboard_command.compile(datapack, ctx);

                PlayersScoreboardCommand::Display(compiled_high_players_display_scoreboard_command)
            }
        }
    }
}
