use std::{collections::BTreeMap, mem::take};

use minecraft_command_types::{
    command::{Command, data::DataTarget, function::FunctionCommandArguments},
    has_macro::HasMacro,
    nbt_path::{NbtPath, NbtPathNode},
    resource_location::ResourceLocation,
    snbt::SNBT,
};

use crate::{datapack::HighDatapack, expression::ConstantExpressionKind};

#[derive(Debug, Default, Clone)]
pub struct CompileContext {
    pub commands: Vec<Command>,
    pub macro_arguments: BTreeMap<usize, ConstantExpressionKind>,
    pub macro_data: Option<(DataTarget, NbtPath)>,
    macro_counter: usize,
}

impl CompileContext {
    pub fn create_child_ctx(&mut self) -> CompileContext {
        CompileContext {
            macro_data: self.macro_data.clone(),
            macro_counter: self.macro_counter,
            ..Default::default()
        }
    }

    pub fn compile(&mut self) -> Vec<Command> {
        self.commands.clone()
    }

    pub fn num_commands(&self) -> usize {
        self.commands.len()
    }

    pub fn add_command(&mut self, datapack: &mut HighDatapack, command: Command) {
        if !command.has_macro() {
            self.commands.push(command);

            return;
        }

        let (target, path) = if let Some(macro_data) = self.macro_data.clone() {
            macro_data
        } else {
            let (target, path) = datapack.get_unique_data();

            self.macro_data = Some((target.clone(), path.clone()));

            (target, path)
        };

        for (id, expression) in take(&mut self.macro_arguments) {
            expression.assign_to_data(
                datapack,
                self,
                target.clone(),
                path.clone()
                    .with_node(NbtPathNode::named_string(id.to_string())),
            );
        }

        let mut unique_function_ctx = CompileContext {
            macro_data: Some((target.clone(), path.clone())),
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
            Some(FunctionCommandArguments::DataTarget(target, Some(path))),
        ));
    }

    pub fn add_commands(&mut self, datapack: &mut HighDatapack, commands: Vec<Command>) {
        for command in commands {
            self.add_command(datapack, command);
        }
    }

    pub fn get_macro_string(&mut self, expression: ConstantExpressionKind) -> String {
        let id = self.increment_macro();

        self.macro_arguments.insert(id, expression);

        format!("$({})", id)
    }

    pub fn get_macro_snbt(&mut self, expression: ConstantExpressionKind) -> SNBT {
        let id = self.increment_macro();

        self.macro_arguments.insert(id, expression);

        SNBT::Macro(id.to_string())
    }

    pub fn extend_context(&mut self, other: CompileContext) {
        self.commands.extend(other.commands);
    }

    pub fn increment_macro(&mut self) -> usize {
        self.macro_counter += 1;

        self.macro_counter - 1
    }
}
