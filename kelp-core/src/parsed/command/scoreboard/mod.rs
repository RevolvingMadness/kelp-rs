use crate::{
    parsed::arena::ParsedAstArena,
    parsed::{
        command::scoreboard::{
            objectives::ParsedObjectivesScoreboardCommand, players::ParsedPlayersScoreboardCommand,
        },
        semantic_analysis::SemanticAnalysisContext,
    },
    typed::arena::TypedAstArena,
    typed::expression::command::scoreboard::TypedScoreboardCommand,
};

pub mod objectives;
pub mod players;

#[derive(Debug, Clone)]
pub enum ParsedScoreboardCommand {
    Objectives(ParsedObjectivesScoreboardCommand),
    Players(ParsedPlayersScoreboardCommand),
}

impl ParsedScoreboardCommand {
    pub fn perform_semantic_analysis(
        self,
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedScoreboardCommand> {
        Some(match self {
            Self::Objectives(command) => {
                let command = command.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedScoreboardCommand::Objectives(Box::new(command))
            }
            Self::Players(command) => {
                let command = command.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedScoreboardCommand::Players(command)
            }
        })
    }
}
