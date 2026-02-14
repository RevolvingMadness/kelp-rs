use minecraft_command_types::command::scoreboard::ScoreboardCommand;
use minecraft_command_types_derive::HasMacro;

use crate::{
    compile_context::CompileContext,
    datapack::HighDatapack,
    high::command::scoreboard::{
        objectives::HighObjectivesScoreboardCommand, players::HighPlayersScoreboardCommand,
    },
    semantic_analysis_context::SemanticAnalysisContext,
};

pub mod objectives;
pub mod players;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighScoreboardCommand {
    Objectives(Box<HighObjectivesScoreboardCommand>),
    Players(HighPlayersScoreboardCommand),
}

impl HighScoreboardCommand {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            HighScoreboardCommand::Objectives(command) => {
                command.perform_semantic_analysis(ctx, is_lhs)
            }
            HighScoreboardCommand::Players(command) => {
                command.perform_semantic_analysis(ctx, is_lhs)
            }
        }
    }

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
