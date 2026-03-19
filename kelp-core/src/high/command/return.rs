use crate::{
    high::command::Command,
    middle::expression::command::r#return::ReturnCommand as MiddleReturnCommand,
    semantic_analysis_context::SemanticAnalysisContext,
};

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub enum ReturnCommand {
    Value(i32),
    Fail,
    Run(Box<Command>),
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
