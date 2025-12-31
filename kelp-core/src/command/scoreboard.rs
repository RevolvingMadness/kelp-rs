use minecraft_command_types::command::{
    enums::{
        score_operation_operator::ScoreOperationOperator,
        scoreboard_render_type::ScoreboardRenderType,
    },
    scoreboard::{
        ObjectivesScoreboardCommand, PlayersDisplayScoreboardCommand, PlayersScoreboardCommand,
        ScoreboardCommand, ScoreboardModification, ScoreboardNumberFormat,
    },
};
use minecraft_command_types_derive::HasMacro;

use crate::{
    command::{HighPlayerScore, context::CompileContext},
    datapack::HighDatapack,
    entity_selector::HighEntitySelector,
    expression::Expression,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighScoreboardNumberFormat {
    Blank,
    Fixed(Expression),
    Styled(Expression),
}

impl HighScoreboardNumberFormat {
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

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighScoreboardModification {
    DisplayAutoUpdate(bool),
    DisplayName(Expression),
    NumberFormat(Option<HighScoreboardNumberFormat>),
    RenderType(ScoreboardRenderType),
}

impl HighScoreboardModification {
    pub fn compile(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> ScoreboardModification {
        match self {
            HighScoreboardModification::DisplayAutoUpdate(value) => {
                ScoreboardModification::DisplayAutoUpdate(value)
            }
            HighScoreboardModification::DisplayName(expression) => {
                ScoreboardModification::DisplayName(
                    expression.resolve(datapack, ctx).kind.as_snbt_macros(ctx),
                )
            }
            HighScoreboardModification::NumberFormat(number_format) => {
                ScoreboardModification::NumberFormat(
                    number_format.map(|number_format| number_format.compile(datapack, ctx)),
                )
            }
            HighScoreboardModification::RenderType(render_type) => {
                ScoreboardModification::RenderType(render_type)
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighObjectivesScoreboardCommand {
    List,
    Add(String, String, Option<Expression>),
    Remove(String),
    SetDisplay(String, Option<String>),
    Modify(String, HighScoreboardModification),
}

impl HighObjectivesScoreboardCommand {
    pub fn compile(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> ObjectivesScoreboardCommand {
        match self {
            HighObjectivesScoreboardCommand::List => ObjectivesScoreboardCommand::List,
            HighObjectivesScoreboardCommand::Add(objective, display_name, expression) => {
                ObjectivesScoreboardCommand::Add(
                    objective,
                    display_name,
                    expression.map(|e| e.resolve(datapack, ctx).kind.as_snbt_macros(ctx)),
                )
            }
            HighObjectivesScoreboardCommand::Remove(objective) => {
                ObjectivesScoreboardCommand::Remove(objective)
            }
            HighObjectivesScoreboardCommand::SetDisplay(objective, display_name) => {
                ObjectivesScoreboardCommand::SetDisplay(objective, display_name)
            }
            HighObjectivesScoreboardCommand::Modify(objective, scoreboard_modification) => {
                ObjectivesScoreboardCommand::Modify(
                    objective,
                    scoreboard_modification.compile(datapack, ctx),
                )
            }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighScoreboardCommand {
    Objectives(Box<HighObjectivesScoreboardCommand>),
    Players(HighPlayersScoreboardCommand),
}

impl HighScoreboardCommand {
    pub fn compile(
        self,
        datapack: &mut HighDatapack,
        ctx: &mut CompileContext,
    ) -> ScoreboardCommand {
        match self {
            HighScoreboardCommand::Objectives(high_objectives_scoreboard_command) => {
                let compiled_high_objectives_scoreboard_command =
                    high_objectives_scoreboard_command.compile(datapack, ctx);

                ScoreboardCommand::Objectives(compiled_high_objectives_scoreboard_command)
            }
            HighScoreboardCommand::Players(high_players_scoreboard_command) => {
                let compiled_high_players_scoreboard_command =
                    high_players_scoreboard_command.compile(datapack, ctx);

                ScoreboardCommand::Players(compiled_high_players_scoreboard_command)
            }
        }
    }
}
