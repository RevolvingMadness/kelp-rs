use minecraft_command_types_derive::HasMacro;

use crate::command::HighCommand;

#[derive(Debug, Clone, Eq, PartialEq, PartialOrd, Ord, Hash, HasMacro)]
pub enum HighReturnCommand {
    Value(i32),
    Fail,
    Run(Box<HighCommand>),
}
