use crate::{
    parsed::{command::ParsedCommand, semantic_analysis::SemanticAnalysisContext},
    semantic::expression::command::r#return::SemanticReturnCommand,
};

#[derive(Debug, Clone)]
pub enum ReturnCommand {
    Value(i32),
    Fail,
    Run(Box<ParsedCommand>),
}

impl ReturnCommand {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticReturnCommand> {
        Some(match self {
            Self::Value(value) => SemanticReturnCommand::Value(value),
            Self::Fail => SemanticReturnCommand::Fail,
            Self::Run(command) => {
                let command = command.perform_semantic_analysis(ctx)?;

                SemanticReturnCommand::Run(Box::new(command))
            }
        })
    }
}
