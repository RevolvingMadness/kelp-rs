use crate::semantic::expression::command::SemanticCommand;

#[derive(Debug, Clone)]
pub enum SemanticReturnCommand {
    Value(i32),
    Fail,
    Run(Box<SemanticCommand>),
}
