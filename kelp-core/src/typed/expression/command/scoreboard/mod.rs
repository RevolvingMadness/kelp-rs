use minecraft_command_types::command::scoreboard::ScoreboardCommand;

use crate::{
    ast_allocator::low::LowAstAllocator,
    compile_context::CompileContext,
    datapack::Datapack,
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
        allocator: &LowAstAllocator,
        datapack: &mut Datapack,
        ctx: &mut CompileContext,
    ) -> ScoreboardCommand {
        match self {
            Self::Objectives(command) => {
                let command = command.compile(allocator, datapack, ctx);

                ScoreboardCommand::Objectives(command)
            }
            Self::Players(command) => {
                let command = command.compile(allocator, datapack, ctx);

                ScoreboardCommand::Players(command)
            }
        }
    }
}
