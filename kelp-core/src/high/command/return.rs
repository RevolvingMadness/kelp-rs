use minecraft_command_types_derive::HasMacro;

use crate::{high::command::HighCommand, semantic_analysis_context::SemanticAnalysisContext};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighReturnCommand {
    Value(i32),
    Fail,
    Run(Box<HighCommand>),
}

impl HighReturnCommand {
    pub fn perform_semantic_analysis(&self, ctx: &mut SemanticAnalysisContext) -> Option<()> {
        match self {
            HighReturnCommand::Value(_) | HighReturnCommand::Fail => Some(()),
            HighReturnCommand::Run(command) => command.perform_semantic_analysis(ctx),
        }
    }
}
