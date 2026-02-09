use std::fmt::{Display, Formatter};

use minecraft_command_types::{command::Command, has_macro::HasMacro};

#[derive(Debug, Default)]
pub struct MCFunction {
    pub commands: Vec<Command>,
}

impl Display for MCFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for command in &self.commands {
            if command.has_macro() {
                f.write_str("$")?;
            }

            writeln!(f, "{}", command)?;
        }

        Ok(())
    }
}

impl MCFunction {
    pub fn add_command(&mut self, command: Command) {
        self.commands.push(command);
    }

    pub fn add_commands(&mut self, commands: Vec<Command>) {
        self.commands.extend(commands);
    }

    pub fn commands(&self) -> &Vec<Command> {
        &self.commands
    }
}
