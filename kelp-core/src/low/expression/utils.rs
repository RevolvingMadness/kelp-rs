use minecraft_command_types::command::{
    Command,
    scoreboard::{PlayersScoreboardCommand, ScoreboardCommand},
};

use crate::{compile_context::CompileContext, datapack::Datapack};

pub fn push_scoreboard_players(
    datapack: &mut Datapack,
    ctx: &mut CompileContext,
    command: PlayersScoreboardCommand,
) {
    ctx.add_command(
        datapack,
        Command::Scoreboard(ScoreboardCommand::Players(command)),
    );
}
