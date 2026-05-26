use minecraft_command_types::command::scoreboard::ScoreboardCommand;

use crate::{
    compile_context::CompileContext,
    datapack::Datapack,
    typed::arena::TypedAstArena,
    typed::expression::command::scoreboard::{
        objectives::TypedObjectivesScoreboardCommand, players::TypedPlayersScoreboardCommand,
    },
};

pub mod objectives;
pub mod players;

#[derive(Debug, Clone)]
pub enum TypedScoreboardCommand {
    Objectives(Box<TypedObjectivesScoreboardCommand>),
    Players(TypedPlayersScoreboardCommand),
}

impl TypedScoreboardCommand {
    #[must_use]
    pub fn compile(
        self,
        arena: &TypedAstArena,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ScoreboardCommand {
        match self {
            Self::Objectives(command) => {
                let command = command.compile(arena, datapack, ctx);

                ScoreboardCommand::Objectives(command)
            }
            Self::Players(command) => {
                let command = command.compile(arena, datapack, ctx);

                ScoreboardCommand::Players(command)
            }
        }
    }
}
