use std::{collections::HashMap, mem::take};

use minecraft_command_types::{
    command::{Command, execute::ExecuteIfSubcommand, function::FunctionCommandArguments},
    has_macro::HasMacro,
    macroable::Macroable,
    nbt_path::NbtPathNode,
    resource_location::ResourceLocation,
};

use crate::{
    data::GeneratedData,
    datapack::{Datapack, mcfunction::MCFunction},
    low::expression::Expression,
};

#[derive(Debug, Clone)]
pub enum LoopType {
    While(bool, Box<ExecuteIfSubcommand>),
    Loop,
}

#[derive(Debug, Clone)]
pub struct LoopInfo {
    pub resource_location: ResourceLocation,
    pub type_: LoopType,
}

#[derive(Debug, Default, Clone)]
pub struct CompileContext {
    pub commands: Vec<Command>,
    pub macro_arguments: HashMap<usize, Expression>,
    pub macro_data: Option<GeneratedData>,
    pub loop_info: Option<LoopInfo>,
    macro_counter: usize,
}

impl CompileContext {
    #[inline]
    #[must_use]
    pub fn create_child_ctx(&mut self) -> Self {
        Self {
            macro_data: self.macro_data.clone(),
            macro_counter: self.macro_counter,
            loop_info: self.loop_info.clone(),
            ..Default::default()
        }
    }

    #[inline]
    pub fn compile_to_function(mut self, function: &mut MCFunction) {
        let commands = self.compile();

        function.add_commands(commands);
    }

    #[inline]
    pub fn compile(&mut self) -> Vec<Command> {
        self.commands.clone()
    }

    #[must_use]
    pub const fn num_commands(&self) -> usize {
        self.commands.len()
    }

    pub fn add_command<C: Into<Command>>(&mut self, datapack: &mut Datapack, command: C) {
        let command = command.into();

        if !command.has_macro() {
            self.commands.push(command);

            return;
        }

        let data = if let Some(macro_data) = self.macro_data.clone() {
            macro_data
        } else {
            let unique_data = datapack.get_unique_data();

            self.macro_data = Some(unique_data.clone());

            unique_data
        };

        for (id, expression) in take(&mut self.macro_arguments) {
            expression.assign_to_data(
                datapack,
                self,
                data.clone()
                    .with_path_node(NbtPathNode::named_string(id.to_string())),
            );
        }

        let mut unique_function_ctx = Self {
            macro_data: Some(data.clone()),
            ..Default::default()
        };

        let unique_function = datapack.get_unique_function_paths();

        datapack.compile_and_add_to_function(&unique_function, &mut unique_function_ctx);

        datapack
            .current_namespace_mut()
            .get_function_mut(&unique_function)
            .add_command(command);

        self.commands.push(Command::Function(
            ResourceLocation::new_namespace_paths(
                datapack.current_namespace_name(),
                unique_function,
            ),
            Some(FunctionCommandArguments::DataTarget(
                data.target.target,
                Some(data.path),
            )),
        ));
    }

    pub fn add_commands(&mut self, datapack: &mut Datapack, commands: Vec<Command>) {
        for command in commands {
            self.add_command(datapack, command);
        }
    }

    pub fn get_macro_string(&mut self, expression: Expression) -> String {
        let id = self.increment_macro();

        self.macro_arguments.insert(id, expression);

        format!("$({})", id)
    }

    pub fn get_macro_snbt<T>(&mut self, expression: Expression) -> Macroable<T> {
        let id = self.increment_macro();

        self.macro_arguments.insert(id, expression);

        Macroable::Macro(id.to_string())
    }

    #[inline]
    pub fn extend_context(&mut self, other: Self) {
        self.commands.extend(other.commands);
    }

    pub const fn increment_macro(&mut self) -> usize {
        self.macro_counter += 1;

        self.macro_counter - 1
    }
}
