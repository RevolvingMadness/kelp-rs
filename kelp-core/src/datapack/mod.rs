use crate::compile_context::CompileContext;
use crate::data_type::DataTypeKind;
use crate::datapack::mcfunction::MCFunction;
use crate::datapack::namespace::HighNamespace;
use crate::expression::{
    constant::ConstantExpression, supports_variable_type_scope::SupportsVariableTypeScope,
};
use crate::high::data::{GeneratedDataTarget, HighDataTarget, HighDataTargetKind};
use crate::high::nbt_path::{HighNbtPath, HighNbtPathNode};
use crate::high::player_score::GeneratedPlayerScore;
use crate::semantic_analysis_context::SemanticAnalysisInfo;
use minecraft_command_types::command::data::DataTarget;
use minecraft_command_types::command::execute::{ExecuteIfSubcommand, ExecuteSubcommand};
use minecraft_command_types::command::scoreboard::{
    ObjectivesScoreboardCommand, PlayersScoreboardCommand, ScoreboardCommand,
};
use minecraft_command_types::command::{Command, PlayerScore};
use minecraft_command_types::datapack::Datapack;
use minecraft_command_types::datapack::tag::{Tag, TagType, TagValue};
use minecraft_command_types::entity_selector::EntitySelector;
use minecraft_command_types::nbt_path::{NbtPath, NbtPathNode};
use minecraft_command_types::resource_location::ResourceLocation;
use minecraft_command_types::snbt::SNBTString;
use nonempty::{NonEmpty, nonempty};
use serde_json::json;
use std::cell::{Cell, RefCell};
use std::collections::{BTreeMap, HashSet, VecDeque};

pub mod mcfunction;
pub mod namespace;

#[derive(Default, Clone, Copy)]
pub struct HighDatapackRequirements {
    pub always_succeed_predicate: bool,
    pub bitwise_left_shift_function: bool,
    pub bitwise_right_shift_function: bool,
}

#[derive(Default, Clone, Copy)]
pub struct HighDatapackSettings {
    pub num_match_cases_to_split: usize,
}

pub type Scope = BTreeMap<String, (DataTypeKind, ConstantExpression)>;
pub type Scopes = VecDeque<Scope>;

pub struct HighDatapack {
    pub name: String,
    pub requirements: Cell<HighDatapackRequirements>,
    pub settings: HighDatapackSettings,
    pub scopes: Scopes,
    namespaces: BTreeMap<String, HighNamespace>,
    namespace_stack: Vec<String>,
    counter: Cell<usize>,
    used_constants: RefCell<HashSet<i32>>,
}

impl SupportsVariableTypeScope for HighDatapack {
    fn get_variable(&self, name: &str) -> Option<Option<DataTypeKind>> {
        self.get_variable(name)
            .map(|(data_type, _)| Some(data_type))
    }

    fn add_info(&mut self, _: SemanticAnalysisInfo) {}
}

impl HighDatapack {
    pub fn new(name: impl Into<String>) -> HighDatapack {
        let mut scopes = Scopes::new();
        scopes.push_front(Scope::new());

        HighDatapack {
            name: name.into(),
            requirements: Cell::new(HighDatapackRequirements::default()),
            settings: HighDatapackSettings::default(),
            scopes,
            namespaces: BTreeMap::new(),
            namespace_stack: Vec::new(),
            counter: Cell::new(0),
            used_constants: RefCell::new(HashSet::new()),
        }
    }

    pub fn compile_and_add_to_function(
        &mut self,
        paths: &NonEmpty<String>,
        ctx: &mut CompileContext,
    ) {
        let commands = ctx.compile();

        self.get_function_mut(paths).add_commands(commands);
    }

    #[inline]
    pub fn start_scope(&mut self) {
        self.scopes.push_front(Scope::new());
    }

    #[inline]
    pub fn end_scope(&mut self) {
        self.scopes.pop_front().unwrap();
    }

    #[inline]
    pub fn current_namespace_name(&self) -> &str {
        self.namespace_stack.last().expect("No current namespace")
    }

    fn add_default_namespace_if_missing(&mut self, name: &str) {
        if !self.namespaces.contains_key(name) {
            self.namespaces
                .insert(name.to_string(), HighNamespace::new(name.to_string()));
        }
    }

    pub fn within_namespace<F>(&mut self, name: &str, function: F)
    where
        F: FnOnce(&mut HighDatapack),
    {
        self.add_default_namespace_if_missing(name);

        self.namespace_stack.push(name.to_string());

        function(self);

        self.namespace_stack.pop();
    }

    pub fn increment_counter(&self) -> usize {
        let val = self.counter.get();

        self.counter.set(val + 1);

        val
    }

    pub fn get_unique_function_paths(&self) -> NonEmpty<String> {
        self.current_namespace().get_unique_function_paths()
    }

    pub fn get_unique_score(&self) -> GeneratedPlayerScore {
        let incremented = self.increment_counter();

        let current_namespace = self.current_namespace();

        let selector = current_namespace.get_score_selector(incremented);

        GeneratedPlayerScore {
            is_generated: true,
            score: PlayerScore::new(selector, current_namespace.get_scores_objective()),
        }
    }

    pub fn get_unique_data(&self) -> (GeneratedDataTarget, NbtPath) {
        let current_namespace_name = self.current_namespace_name();

        (
            GeneratedDataTarget {
                is_generated: true,
                target: DataTarget::Storage(ResourceLocation::new_namespace_path(
                    "__kelp_storages__",
                    format!("__kelp_{}_storage__", current_namespace_name),
                )),
            },
            NbtPath(nonempty![NbtPathNode::named_string(format!(
                "__kelp_{}_storage_{}__",
                current_namespace_name,
                self.increment_counter()
            ),)]),
        )
    }

    pub fn get_unique_data_named(&self) -> (GeneratedDataTarget, NbtPath, String) {
        let current_namespace_name = self.current_namespace_name();
        let name = format!(
            "__kelp_{}_storage_{}__",
            current_namespace_name,
            self.increment_counter()
        );

        (
            GeneratedDataTarget {
                is_generated: true,
                target: DataTarget::Storage(ResourceLocation::new_namespace_path(
                    "__kelp_storages__",
                    format!("__kelp_{}_storage__", current_namespace_name),
                )),
            },
            NbtPath(nonempty![NbtPathNode::named_string(name.clone())]),
            name,
        )
    }

    pub fn get_unique_data_both(&self) -> (HighDataTarget, DataTarget, NbtPath, String) {
        let current_namespace_name = self.current_namespace_name();
        let name = format!(
            "__kelp_{}_storage_{}__",
            current_namespace_name,
            self.increment_counter()
        );
        let storage_location = ResourceLocation::new_namespace_path(
            "__kelp_storages__",
            format!("__kelp_{}_storage__", current_namespace_name),
        );

        (
            HighDataTarget {
                is_generated: true,
                kind: HighDataTargetKind::Storage(storage_location.clone()),
            },
            DataTarget::Storage(storage_location),
            NbtPath(nonempty![NbtPathNode::named_string(name.clone())]),
            name,
        )
    }

    pub fn get_unique_data_with_path_name(&self) -> (DataTarget, NbtPath, SNBTString) {
        let current_namespace_name = self.current_namespace_name();
        let name = format!(
            "__kelp_{}_storage_{}__",
            current_namespace_name,
            self.increment_counter()
        );

        (
            DataTarget::Storage(ResourceLocation::new_namespace_path(
                "__kelp_storages__",
                format!("__kelp_{}_storage__", current_namespace_name),
            )),
            NbtPath(nonempty![NbtPathNode::named_string(name.clone())]),
            SNBTString(false, name),
        )
    }

    pub fn get_high_unique_data(&self) -> (HighDataTarget, HighNbtPath) {
        let current_namespace_name = self.current_namespace_name();

        (
            HighDataTarget {
                is_generated: true,
                kind: HighDataTargetKind::Storage(ResourceLocation::new_namespace_path(
                    "__kelp_storages__",
                    format!("__kelp_{}_storage__", current_namespace_name),
                )),
            },
            HighNbtPath(nonempty![HighNbtPathNode::Named(
                format!(
                    "__kelp_{}_storage_{}__",
                    current_namespace_name,
                    self.increment_counter()
                )
                .into(),
                None,
            )]),
        )
    }

    pub fn get_variable(&self, name: &str) -> Option<(DataTypeKind, ConstantExpression)> {
        for scope in self.scopes.iter() {
            if let Some(value) = scope.get(name) {
                return Some(value.clone());
            }
        }

        None
    }

    pub fn get_variable_mut(&mut self, name: &str) -> Option<&mut ConstantExpression> {
        for scope in self.scopes.iter_mut() {
            if let Some((_, value)) = scope.get_mut(name) {
                return Some(value);
            }
        }

        None
    }

    pub fn get_variable_scope(&self, name: &str) -> Option<usize> {
        for (i, scope) in self.scopes.iter().enumerate() {
            if scope.contains_key(name) {
                return Some(i);
            }
        }

        None
    }

    #[inline]
    #[must_use]
    pub fn variable_exists_in_current_scope(&self, name: &str) -> bool {
        self.scopes.front().expect("No scopes").contains_key(name)
    }

    pub fn declare_variable(
        &mut self,
        name: &str,
        data_type: DataTypeKind,
        value: ConstantExpression,
    ) {
        self.scopes
            .front_mut()
            .expect("No scopes")
            .insert(name.to_string(), (data_type, value));
    }

    pub fn assign_variable(&mut self, name: &str, value: ConstantExpression) {
        for scope in self.scopes.iter_mut().rev() {
            if let Some((_, existing_value)) = scope.get_mut(name) {
                *existing_value = value;
                return;
            }
        }

        panic!("Variable '{}' has not been declared", name);
    }

    pub fn new_get_variable_score(&self, name: &str) -> Option<PlayerScore> {
        if self.variable_exists_in_current_scope(name) {
            return None;
        }

        let current_namespace = self.current_namespace();

        Some(PlayerScore::new(
            current_namespace.get_variable_selector(name, self.scopes.len()),
            current_namespace.get_scores_objective(),
        ))
    }

    pub fn get_variable_data(&self, name: &str) -> (DataTarget, NbtPath) {
        let current_namespace_name = self.current_namespace_name();

        (
            DataTarget::Storage(ResourceLocation::new_namespace_path(
                "kelp",
                format!("__{}_variables__", current_namespace_name),
            )),
            NbtPath(nonempty![NbtPathNode::named_string(format!(
                "__variable_{}_{}__",
                name,
                self.scopes.len(),
            ))]),
        )
    }

    pub fn get_constant_score(&self, constant: i32) -> GeneratedPlayerScore {
        self.used_constants.borrow_mut().insert(constant);

        GeneratedPlayerScore {
            is_generated: true,
            score: PlayerScore::new(
                HighDatapack::get_constant_selector(constant),
                self.get_constants_objective(),
            ),
        }
    }

    #[inline]
    #[must_use]
    pub fn get_constants_objective(&self) -> String {
        "__kelp_constants__".to_string()
    }

    #[inline]
    pub fn get_constant_selector(constant: i32) -> EntitySelector {
        EntitySelector::Name(format!("__kelp_constant_{}__", constant))
    }

    pub fn push_namespace(&mut self, name: &str) {
        self.add_default_namespace_if_missing(name);

        self.namespace_stack.push(name.to_string());
    }

    #[inline]
    pub fn pop_namespace(&mut self) {
        self.namespace_stack.pop();
    }

    #[inline]
    pub fn get_namespace_mut(&mut self, name: &str) -> &mut HighNamespace {
        self.add_default_namespace_if_missing(name);

        self.namespaces.get_mut(name).unwrap()
    }

    #[inline]
    pub fn get_namespace(&mut self, name: &str) -> &HighNamespace {
        self.add_default_namespace_if_missing(name);

        self.namespaces.get(name).unwrap()
    }

    #[inline]
    pub fn get_function_mut(&mut self, paths: &NonEmpty<String>) -> &mut MCFunction {
        self.current_namespace_mut().get_function_mut(paths)
    }

    pub fn push_function_to_current_namespace(&mut self, paths: NonEmpty<String>) {
        self.current_namespace_mut().push_function(paths);
    }

    pub fn pop_function_from_current_namespace(&mut self) {
        self.current_namespace_mut().pop_function();
    }

    pub fn while_loop<C, B>(&mut self, ctx: &mut CompileContext, condition: C, body: B)
    where
        C: Fn(&mut HighDatapack, &mut CompileContext) -> (bool, ExecuteIfSubcommand),
        B: FnOnce(&mut HighDatapack, &mut CompileContext),
    {
        let loop_function_paths = self.get_unique_function_paths();
        let current_namespace_name = self.current_namespace_name().to_string();
        let loop_function_location = ResourceLocation::new_namespace_paths(
            current_namespace_name,
            loop_function_paths.clone(),
        );

        let (should_be_inverted, execute_condition) = condition(self, ctx);

        ctx.add_command(
            self,
            Command::Execute(
                ExecuteSubcommand::If(should_be_inverted, execute_condition.clone()).then(
                    ExecuteSubcommand::Run(Box::new(Command::Function(
                        loop_function_location.clone(),
                        None,
                    ))),
                ),
            ),
        );

        let mut loop_ctx = CompileContext::default();
        body(self, &mut loop_ctx);

        loop_ctx.add_command(
            self,
            Command::Execute(
                ExecuteSubcommand::If(should_be_inverted, execute_condition).then(
                    ExecuteSubcommand::Run(Box::new(Command::Function(
                        loop_function_location,
                        None,
                    ))),
                ),
            ),
        );

        self.compile_and_add_to_function(&loop_function_paths, &mut loop_ctx);
    }

    pub fn add_context_to_current_function(&mut self, ctx: &mut CompileContext) {
        let commands = ctx.compile();

        self.current_namespace_mut()
            .current_function_mut()
            .add_commands(commands);
    }

    #[inline]
    pub fn current_namespace_mut(&mut self) -> &mut HighNamespace {
        let name = self.current_namespace_name().to_string();

        self.namespaces
            .get_mut(&name)
            .expect("No current namespace")
    }

    #[inline]
    pub fn current_namespace(&self) -> &HighNamespace {
        self.namespaces
            .get(self.current_namespace_name())
            .expect("No current namespace")
    }

    pub fn compile(&mut self) -> Datapack {
        let mut output_datapack = Datapack::new(88, json!(""));

        let mut load_function_ctx = CompileContext::default();
        let mut needs_load_function = false;

        {
            let used_constants = self.used_constants.borrow().clone();

            if !used_constants.is_empty() {
                needs_load_function = true;

                load_function_ctx.add_command(
                    self,
                    Command::Scoreboard(ScoreboardCommand::Objectives(
                        ObjectivesScoreboardCommand::Add(
                            self.get_constants_objective(),
                            "dummy".to_string(),
                            None,
                        ),
                    )),
                );

                for constant in used_constants.iter() {
                    load_function_ctx.add_command(
                        self,
                        Command::Scoreboard(ScoreboardCommand::Players(
                            PlayersScoreboardCommand::Set(
                                PlayerScore::new(
                                    EntitySelector::Name(format!("__kelp_constant_{}__", constant)),
                                    self.get_constants_objective(),
                                ),
                                *constant,
                            ),
                        )),
                    );
                }
            }
        }

        if needs_load_function {
            let load_function_commands = load_function_ctx.compile();

            let kelp_namespace = self.get_namespace_mut("kelp");
            let load_function = kelp_namespace.get_function_mut(&nonempty!["load".to_string()]);
            load_function.add_commands(load_function_commands);

            output_datapack.get_namespace_mut("minecraft").add_tag(
                TagType::Function,
                &nonempty!["load".to_string()],
                Tag {
                    replace: None,
                    values: vec![TagValue::ResourceLocation(
                        ResourceLocation::new_namespace_path("kelp", "load"),
                    )],
                },
            );
        }

        for (name, namespace) in self.namespaces.iter_mut() {
            self.namespace_stack.push(name.clone());

            namespace.ensure_load_function(&mut output_datapack);

            self.namespace_stack.pop();
        }

        for (name, namespace) in self.namespaces.iter() {
            let compiled_namespace = namespace.compile();
            output_datapack.add_namespace(name.clone(), compiled_namespace);
        }

        if self.requirements.get().always_succeed_predicate {
            output_datapack.get_namespace_mut("kelp").predicates.insert(
                nonempty!["always_succeed".to_string()],
                json!({"condition":"minecraft:random_chance","chance":1.0}),
            );
        }

        output_datapack
    }
}
