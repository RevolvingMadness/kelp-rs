use minecraft_command_types_derive::HasMacro;

use crate::{high::command::Command, semantic_analysis_context::SemanticAnalysisContext};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum ReturnCommand {
    Value(i32),
    Fail,
    Run(Box<Command>),
}

impl ReturnCommand {
    pub fn perform_semantic_analysis(
        &self,
        ctx: &mut SemanticAnalysisContext,
        is_lhs: bool,
    ) -> Option<()> {
        match self {
            Self::Value(_) | Self::Fail => Some(()),
            Self::Run(command) => command.perform_semantic_analysis(ctx, is_lhs),
        }
    }
}
