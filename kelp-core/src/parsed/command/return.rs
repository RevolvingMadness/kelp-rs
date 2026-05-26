use crate::{
    parsed::arena::ParsedAstArena,
    parsed::{command::Command, semantic_analysis::SemanticAnalysisContext},
    typed::arena::TypedAstArena,
    typed::expression::command::r#return::TypedReturnCommand,
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
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<TypedReturnCommand> {
        Some(match self {
            Self::Value(value) => TypedReturnCommand::Value(value),
            Self::Fail => TypedReturnCommand::Fail,
            Self::Run(command) => {
                let command = command.perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                TypedReturnCommand::Run(Box::new(command))
            }
        })
    }
}
