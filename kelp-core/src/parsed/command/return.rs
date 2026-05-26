use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    parsed::{command::Command, semantic_analysis::SemanticAnalysisContext},
    typed::expression::command::r#return::TypedReturnCommand as MiddleReturnCommand,
};

#[derive(Debug, Clone)]
pub enum ReturnCommand {
    Value(i32),
    Fail,
    Run(Box<Command>),
}

impl ReturnCommand {
    pub fn perform_semantic_analysis(
        self,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleReturnCommand> {
        Some(match self {
            Self::Value(value) => MiddleReturnCommand::Value(value),
            Self::Fail => MiddleReturnCommand::Fail,
            Self::Run(command) => {
                let command =
                    command.perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                MiddleReturnCommand::Run(Box::new(command))
            }
        })
    }
}
