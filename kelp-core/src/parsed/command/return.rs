use crate::{
    parsed::{command::ParsedCommand, semantic_analysis::SemanticAnalysisContext},
    semantic::expression::command::r#return::SemanticReturnCommand as MiddleReturnCommand,
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
    ) -> Option<MiddleReturnCommand> {
        Some(match self {
            Self::Value(value) => MiddleReturnCommand::Value(value),
            Self::Fail => MiddleReturnCommand::Fail,
            Self::Run(command) => {
                let command = command.perform_semantic_analysis(ctx)?;

                MiddleReturnCommand::Run(Box::new(command))
            }
        })
    }
}
