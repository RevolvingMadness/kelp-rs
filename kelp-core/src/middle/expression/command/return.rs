use minecraft_command_types_derive::HasMacro;

use crate::middle::expression::command::Command;

#[derive(Debug, Clone, Hash, HasMacro)]
pub enum ReturnCommand {
    Value(i32),
    Fail,
    Run(Box<Command>),
}
