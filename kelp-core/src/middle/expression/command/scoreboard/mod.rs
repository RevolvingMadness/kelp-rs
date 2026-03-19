use minecraft_command_types::command::scoreboard::ScoreboardCommand as LowScoreboardCommand;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    middle::expression::command::scoreboard::{
        objectives::ObjectivesScoreboardCommand, players::PlayersScoreboardCommand,
    },
};

pub mod objectives;
pub mod players;

#[derive(Debug, Clone)]
pub enum ScoreboardCommand {
    Objectives(Box<ObjectivesScoreboardCommand>),
    Players(PlayersScoreboardCommand),
}

impl ScoreboardCommand {
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
