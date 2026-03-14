use minecraft_command_types::command::scoreboard::ScoreboardCommand as LowScoreboardCommand;
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    high::command::scoreboard::{
        objectives::ObjectivesScoreboardCommand, players::PlayersScoreboardCommand,
    },
    semantic_analysis_context::SemanticAnalysisContext,
};

pub mod objectives;
pub mod players;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum ScoreboardCommand {
    Objectives(Box<ObjectivesScoreboardCommand>),
    Players(PlayersScoreboardCommand),
}

impl ScoreboardCommand {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            Self::Objectives(command) => command.perform_semantic_analysis(ctx, is_lhs),
            Self::Players(command) => command.perform_semantic_analysis(ctx, is_lhs),
        }
    }

    pub fn compile(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowScoreboardCommand {
        match self {
            Self::Objectives(high_objectives_scoreboard_command) => {
                let compiled_high_objectives_scoreboard_command =
                    high_objectives_scoreboard_command.compile(datapack, ctx);

                LowScoreboardCommand::Objectives(compiled_high_objectives_scoreboard_command)
            }
            Self::Players(high_players_scoreboard_command) => {
                let compiled_high_players_scoreboard_command =
                    high_players_scoreboard_command.compile(datapack, ctx);

                LowScoreboardCommand::Players(compiled_high_players_scoreboard_command)
            }
        }
    }
}
