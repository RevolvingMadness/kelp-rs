use crate::typed::expression::command::TypedCommand;

#[derive(Debug, Clone)]
pub enum TypedReturnCommand {
    Value(i32),
    Fail,
    Run(Box<TypedCommand>),
}
