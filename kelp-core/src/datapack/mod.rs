use crate::compile_context::CompileContext;
use crate::data::GeneratedDataTarget;
use crate::datapack::mcfunction::MCFunction;
use crate::datapack::namespace::DatapackNamespace;
use crate::high::data::{DataTarget as HighDataTarget, DataTargetKind};
use crate::high::nbt_path::{NbtPath, NbtPathNode};
use crate::high::supports_expression_sigil::RegularSupportsExpressionSigilExt;
use crate::low::data_type::DataType;
use crate::low::environment::Environment;
use crate::low::environment::r#type::r#struct::{
    StructDeclaration, StructId, StructStructDeclaration, StructStructId, TupleStructDeclaration,
    TupleStructId,
};
use crate::low::environment::value::{ValueDeclaration, ValueId};
use crate::low::expression::resolved::ResolvedExpression;
use crate::player_score::GeneratedPlayerScore;
use crate::span::Span;
use crate::visibility::Visibility;
use minecraft_command_types::command::data::{DataCommand, DataTarget};
use minecraft_command_types::command::execute::{ExecuteIfSubcommand, ExecuteSubcommand};
use minecraft_command_types::command::scoreboard::{
    ObjectivesScoreboardCommand, PlayersScoreboardCommand, ScoreboardCommand,
};
use minecraft_command_types::command::{Command, PlayerScore};
use minecraft_command_types::datapack::pack::Pack;
use minecraft_command_types::datapack::pack::format::Format;
use minecraft_command_types::datapack::tag::{Tag, TagType, TagValue};
use minecraft_command_types::datapack::{Datapack as LowDatapack, PackMCMeta};
use minecraft_command_types::entity_selector::EntitySelector;
use minecraft_command_types::nbt_path::{NbtPath as LowNbtPath, NbtPathNode as LowNbtPathNode};
use minecraft_command_types::resource_location::ResourceLocation;
use minecraft_command_types::snbt::SNBTString;
use nonempty::{NonEmpty, nonempty};
use serde_json::json;
use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::mem::take;

pub mod mcfunction;
pub mod namespace;

#[derive(Default, Clone, Copy)]
pub struct DatapackRequirements {
    pub always_succeed_predicate: bool,
    pub bitwise_left_shift_function: bool,
    pub bitwise_right_shift_function: bool,
}

#[derive(Default, Clone, Copy)]
pub struct DatapackSettings {
    pub num_match_cases_to_split: usize,
}

pub struct Datapack {
    pub name: String,
    pub description: Option<String>,
    pub requirements: Cell<DatapackRequirements>,
    pub settings: DatapackSettings,
    pub environment: Environment,
    pub value_values: HashMap<ValueId, (DataType, ResolvedExpression)>,
    namespaces: HashMap<String, DatapackNamespace>,
    namespace_stack: Vec<String>,
    counter: Cell<usize>,
    used_constants: HashSet<i32>,
    used_data: Vec<(DataTarget, LowNbtPath)>,
}

impl Datapack {
    #[must_use]
    pub fn new(environment: Environment, name: String, description: Option<String>) -> Self {
        Self {
            name,
            description,
            requirements: Cell::new(DatapackRequirements::default()),
            settings: DatapackSettings::default(),
            environment,
            value_values: HashMap::new(),
            namespaces: HashMap::new(),
            namespace_stack: Vec::new(),
            counter: Cell::new(0),
            used_constants: HashSet::new(),
            used_data: Vec::new(),
        }
    }

    #[inline]
    #[must_use]
    pub fn get_struct_type(&self, id: StructId) -> (Visibility, &[String], &StructDeclaration) {
        self.environment.get_struct(id)
    }

    #[inline]
    #[must_use]
    pub fn get_struct_struct_type(
        &self,
        id: StructStructId,
    ) -> (Visibility, &[String], &StructStructDeclaration) {
        self.environment.get_struct_struct(id)
    }

    #[inline]
    #[must_use]
    pub fn get_tuple_struct_type(
        &self,
        id: TupleStructId,
    ) -> (Visibility, &[String], &TupleStructDeclaration) {
        self.environment.get_tuple_struct(id)
    }

    #[inline]
    pub fn declare_value(&mut self, id: ValueId, data_type: DataType, value: ResolvedExpression) {
        self.value_values.insert(id, (data_type, value));
    }

    #[inline]
    pub fn set_variable(&mut self, id: ValueId, value: ResolvedExpression) {
        self.value_values.get_mut(&id).unwrap().1 = value;
    }

    #[inline]
    #[must_use]
    pub fn get_value(&self, id: ValueId) -> &ValueDeclaration {
        self.environment.get_value(id)
    }

    #[inline]
    #[must_use]
    pub fn get_variable_value(&self, id: ValueId) -> &(DataType, ResolvedExpression) {
        self.value_values.get(&id).unwrap()
    }

    #[inline]
    #[must_use]
    pub fn get_variable_value_mut(&mut self, id: ValueId) -> &mut (DataType, ResolvedExpression) {
        self.value_values.get_mut(&id).unwrap()
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
    pub fn current_namespace_name(&self) -> &str {
        self.namespace_stack.last().expect("No current namespace")
    }

    fn add_default_namespace_if_missing(&mut self, name: &str) {
        if !self.namespaces.contains_key(name) {
            self.namespaces
                .insert(name.to_string(), DatapackNamespace::new(name.to_string()));
        }
    }

    pub fn within_namespace<F>(&mut self, name: &str, function: F)
    where
        F: FnOnce(&mut Self),
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

    pub fn get_unique_data(&mut self) -> (GeneratedDataTarget, LowNbtPath) {
        let current_namespace_name = self.current_namespace_name();

        let target = DataTarget::Storage(ResourceLocation::new_namespace_path(
            "__kelp_storages__",
            format!("__kelp_{}_storage__", current_namespace_name),
        ));

        let path = LowNbtPath(nonempty![LowNbtPathNode::named_string(format!(
            "__kelp_{}_storage_{}__",
            current_namespace_name,
            self.increment_counter()
        ))]);

        self.used_data.push((target.clone(), path.clone()));

        (
            GeneratedDataTarget {
                is_generated: true,
                target,
            },
            path,
        )
    }

    pub fn get_unique_data_named(&self) -> (GeneratedDataTarget, LowNbtPath, String) {
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
            LowNbtPath(nonempty![LowNbtPathNode::named_string(name.clone())]),
            name,
        )
    }

    pub fn get_unique_data_both(&self) -> (HighDataTarget, DataTarget, LowNbtPath, String) {
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
            DataTargetKind::Storage(storage_location.clone().regular_sigil()).with_generated_span(),
            DataTarget::Storage(storage_location),
            LowNbtPath(nonempty![LowNbtPathNode::named_string(name.clone())]),
            name,
        )
    }

    pub fn get_unique_data_with_path_name(&self) -> (DataTarget, LowNbtPath, SNBTString) {
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
            LowNbtPath(nonempty![LowNbtPathNode::named_string(name.clone())]),
            SNBTString(false, name),
        )
    }

    pub fn get_high_unique_data(&self) -> (HighDataTarget, NbtPath) {
        let current_namespace_name = self.current_namespace_name();

        (
            HighDataTarget {
                is_generated: true,
                span: Span::dummy(),
                kind: DataTargetKind::Storage(
                    ResourceLocation::new_namespace_path(
                        "__kelp_storages__",
                        format!("__kelp_{}_storage__", current_namespace_name),
                    )
                    .regular_sigil(),
                ),
            },
            NbtPath(nonempty![NbtPathNode::Named(
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

    pub fn get_constant_score(&mut self, constant: i32) -> GeneratedPlayerScore {
        self.used_constants.insert(constant);

        GeneratedPlayerScore {
            is_generated: true,
            score: PlayerScore::new(
                Self::get_constant_selector(constant),
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
    #[must_use]
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
    pub fn get_namespace_mut(&mut self, name: &str) -> &mut DatapackNamespace {
        self.add_default_namespace_if_missing(name);

        self.namespaces.get_mut(name).unwrap()
    }

    #[inline]
    pub fn get_namespace(&mut self, name: &str) -> &DatapackNamespace {
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
        C: Fn(&mut Self, &mut CompileContext) -> (bool, ExecuteIfSubcommand),
        B: FnOnce(&mut Self, &mut CompileContext),
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
    pub fn current_namespace_mut(&mut self) -> &mut DatapackNamespace {
        let name = self.current_namespace_name().to_string();

        self.namespaces
            .get_mut(&name)
            .expect("No current namespace")
    }

    #[inline]
    pub fn current_namespace(&self) -> &DatapackNamespace {
        self.namespaces
            .get(self.current_namespace_name())
            .expect("No current namespace")
    }

    #[inline]
    #[must_use]
    pub fn number_of_namespaces(&self) -> usize {
        self.namespaces.len()
    }

    #[inline]
    #[must_use]
    pub fn number_of_functions(&self) -> usize {
        self.namespaces
            .values()
            .map(DatapackNamespace::number_of_functions)
            .sum()
    }

    #[must_use]
    pub fn number_of_commands(&self) -> usize {
        self.namespaces
            .values()
            .map(DatapackNamespace::number_of_commands)
            .sum()
    }

    pub fn compile(mut self) -> LowDatapack {
        let mut output_datapack = LowDatapack::new_pack(PackMCMeta {
            pack: Pack {
                description: json!(self.description.clone().unwrap_or_default()),
                pack_format: Some(88),
                min_format: Some(Format::Array(88, 0)),
                max_format: None,
                supported_formats: None,
            },
            features: None,
            filter: None,
            overlays: None,
            language: None,
        });
        let mut load_function_ctx = CompileContext::default();

        let has_constants = !self.used_constants.is_empty();

        if has_constants {
            let constants_objective = self.get_constants_objective();

            load_function_ctx.add_command(
                &mut self,
                Command::Scoreboard(ScoreboardCommand::Objectives(
                    ObjectivesScoreboardCommand::Add(
                        constants_objective.clone(),
                        "dummy".to_string(),
                        None,
                    ),
                )),
            );

            for constant in take(&mut self.used_constants) {
                load_function_ctx.add_command(
                    &mut self,
                    Command::Scoreboard(ScoreboardCommand::Players(PlayersScoreboardCommand::Set(
                        PlayerScore::new(
                            EntitySelector::Name(format!("__kelp_constant_{}__", constant)),
                            constants_objective.clone(),
                        ),
                        constant,
                    ))),
                );
            }
        }

        let has_data = !self.used_data.is_empty();

        if has_data {
            for (target, path) in take(&mut self.used_data) {
                load_function_ctx
                    .add_command(&mut self, Command::Data(DataCommand::Remove(target, path)));
            }
        }

        if has_constants || has_data {
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

        for (name, namespace) in &mut self.namespaces {
            self.namespace_stack.push(name.clone());

            namespace.ensure_load_function(&mut output_datapack);

            self.namespace_stack.pop();
        }

        for (name, namespace) in &self.namespaces {
            let compiled_namespace = namespace.compile();
            output_datapack.add_namespace(name, compiled_namespace);
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
