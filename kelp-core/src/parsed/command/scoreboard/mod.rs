use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    parsed::{
        command::scoreboard::{
            objectives::ParsedObjectivesScoreboardCommand, players::ParsedPlayersScoreboardCommand,
        },
        semantic_analysis::SemanticAnalysisContext,
    },
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
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedScoreboardCommand> {
        Some(match self {
            Self::Objectives(command) => {
                let command =
                    command.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                TypedScoreboardCommand::Objectives(Box::new(command))
            }
            Self::Players(command) => {
                let command =
                    command.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                TypedScoreboardCommand::Players(command)
            }
        })
    }
}
