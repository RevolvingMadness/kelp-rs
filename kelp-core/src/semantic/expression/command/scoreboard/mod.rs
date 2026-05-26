use minecraft_command_types::command::scoreboard::ScoreboardCommand as LowScoreboardCommand;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    semantic::expression::command::scoreboard::{
        objectives::ObjectivesScoreboardCommand, players::SemanticPlayersScoreboardCommand,
    },
};

pub mod objectives;
pub mod players;

#[derive(Debug, Clone)]
pub enum SemanticScoreboardCommand {
    Objectives(Box<ObjectivesScoreboardCommand>),
    Players(SemanticPlayersScoreboardCommand),
}

impl SemanticScoreboardCommand {
    #[must_use]
    pub fn compile(
        self,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> LowScoreboardCommand {
        match self {
            Self::Objectives(command) => {
                let command = command.compile(datapack, ctx);

                LowScoreboardCommand::Objectives(command)
            }
            Self::Players(command) => {
                let command = command.compile(datapack, ctx);

                LowScoreboardCommand::Players(command)
            }
        }
    }
}
