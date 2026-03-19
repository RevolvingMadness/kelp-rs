use crate::middle::expression::command::Command;

#[derive(Debug, Clone)]
pub enum ReturnCommand {
    Value(i32),
    Fail,
    Run(Box<Command>),
}
