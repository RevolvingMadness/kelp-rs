use minecraft_command_types::command::scoreboard::ScoreboardCommand;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    semantic::expression::command::scoreboard::{
        objectives::SemanticObjectivesScoreboardCommand, players::SemanticPlayersScoreboardCommand,
    },
};

pub mod objectives;
pub mod players;

#[derive(Debug, Clone)]
pub enum SemanticScoreboardCommand {
    Objectives(Box<SemanticObjectivesScoreboardCommand>),
    Players(SemanticPlayersScoreboardCommand),
}

impl SemanticScoreboardCommand {
    #[must_use]
    pub fn compile(self, datapack: &mut Datapack, ctx: &mut CompileContext) -> ScoreboardCommand {
        match self {
            Self::Objectives(command) => {
                let command = command.compile(datapack, ctx);

                ScoreboardCommand::Objectives(command)
            }
            Self::Players(command) => {
                let command = command.compile(datapack, ctx);

                ScoreboardCommand::Players(command)
            }
        }
    }
}
