use std::{cell::Cell, collections::HashMap};

use minecraft_command_types::{
    command::{
        Command,
        scoreboard::{ObjectivesScoreboardCommand, ScoreboardCommand},
    },
    datapack::{
        Datapack, Namespace,
        tag::{Tag, TagType, TagValue},
    },
    entity_selector::EntitySelector,
    resource_location::ResourceLocation,
};

use crate::datapack::mcfunction::MCFunction;

#[derive(Debug)]
pub struct DatapackNamespace {
    pub name: String,
    uses_scores_objective: Cell<bool>,
    functions: HashMap<Vec<String>, MCFunction>,
    function_stack: Vec<Vec<String>>,
    counter: Cell<usize>,
}

impl DatapackNamespace {
    #[must_use]
    pub fn new(name: String) -> Self {
        Self {
            name,
            functions: HashMap::new(),
            function_stack: Vec::new(),
            counter: Cell::new(0),
            uses_scores_objective: Cell::new(false),
        }
    }

    #[inline]
    #[must_use]
    pub fn number_of_functions(&self) -> usize {
        self.functions.len()
    }

    #[must_use]
    pub fn number_of_commands(&self) -> usize {
        self.functions
            .values()
            .map(MCFunction::number_of_commands)
            .sum()
    }

    pub fn within_function<F>(&mut self, paths: Vec<String>, function: F)
    where
        F: FnOnce(&mut Self),
    {
        self.function_stack.push(paths);

        function(self);

        self.function_stack.pop();
    }

    #[inline]
    pub fn add_default_function_if_missing(&mut self, paths: &Vec<String>) {
        if !self.functions.contains_key(paths) {
            self.functions.insert(paths.clone(), MCFunction::default());
        }
    }

    #[inline]
    pub fn push_function(&mut self, paths: Vec<String>) {
        self.add_default_function_if_missing(&paths);

        self.function_stack.push(paths);
    }

    #[inline]
    pub fn pop_function(&mut self) {
        self.function_stack.pop().unwrap();
    }

    fn increment_counter(&self) -> usize {
        let val = self.counter.get();

        self.counter.set(val + 1);

        val
    }

    pub fn get_score_selector(&self, id: usize) -> EntitySelector {
        let name = format!("__temp_score_{}__", id);

        EntitySelector::Name(name)
    }

    pub fn get_variable_selector(&self, name: &str, scope: usize) -> EntitySelector {
        EntitySelector::Name(format!(
            "{}_{}_{}",
            name,
            scope,
            self.current_function_paths().join("_")
        ))
    }

    pub fn get_scores_objective(&self) -> String {
        self.uses_scores_objective.set(true);

        format!("__{}_scores__", self.name)
    }

    pub fn get_unique_function_paths(&self) -> Vec<String> {
        let id = self.increment_counter();
        let paths = vec![format!("__{}_function_{}__", self.name, id)];

        paths
    }

    #[inline]
    pub fn current_function_paths(&self) -> &Vec<String> {
        self.function_stack.last().expect("No current function")
    }

    #[inline]
    pub fn current_function_mut(&mut self) -> &mut MCFunction {
        let paths = self.current_function_paths().clone();

        self.functions.get_mut(&paths).expect("No current function")
    }

    pub fn get_function_mut(&mut self, paths: &Vec<String>) -> &mut MCFunction {
        self.add_default_function_if_missing(paths);

        self.functions.get_mut(paths).unwrap()
    }

    pub fn next_function_name(&mut self) -> String {
        let id = self.increment_counter();
        format!("__generated_mcfunction_{}__", id)
    }

    pub fn get_new_function_mut(&mut self) -> &mut MCFunction {
        let name = self.next_function_name();
        self.get_function_mut(&vec![name])
    }

    pub fn ensure_load_function(&mut self, datapack: &mut Datapack) {
        if self.uses_scores_objective.get() {
            let load_paths = vec!["load".to_string()];

            let scores_objective = self.get_scores_objective();

            self.get_function_mut(&load_paths)
                .add_command(Command::Scoreboard(ScoreboardCommand::Objectives(
                    ObjectivesScoreboardCommand::Add(scores_objective, "dummy".to_string(), None),
                )));

            datapack.get_namespace_mut("minecraft").add_tag(
                TagType::Function,
                &vec!["load".to_string()],
                Tag {
                    replace: None,
                    values: vec![TagValue::ResourceLocation(
                        ResourceLocation::new_namespace_paths(&self.name, load_paths),
                    )],
                },
            );
        }
    }

    pub fn compile(&self) -> Namespace {
        let mut output_namespace = Namespace::default();

        for (paths, function) in &self.functions {
            output_namespace.add_function(paths, &function.to_string());
        }

        output_namespace
    }
}
