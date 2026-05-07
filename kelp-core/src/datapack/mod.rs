use crate::compile_context::CompileContext;
use crate::data::GeneratedDataTarget;
use crate::datapack::mcfunction::MCFunction;
use crate::datapack::namespace::DatapackNamespace;
use crate::high::data::{DataTarget as HighDataTarget, DataTargetKind};
use crate::high::environment::HighEnvironment;
use crate::high::environment::r#type::HighTypeId;
use crate::high::environment::value::function::{HighFunctionDeclaration, HighFunctionId};
use crate::high::environment::value::variable::HighVariableId;
use crate::high::environment::value::{HighValueDeclarationKind, HighValueId};
use crate::high::nbt_path::{NbtPath, NbtPathNode};
use crate::high::supports_expression_sigil::RegularSupportsExpressionSigilExt;
use crate::low::data_type::resolved::ResolvedDataType;
use crate::low::data_type::unresolved::UnresolvedDataType;
use crate::low::environment::Environment;
use crate::low::environment::r#type::r#struct::{
    StructDeclaration, StructId, StructStructDeclaration, StructStructId, TupleStructDeclaration,
    TupleStructId,
};
use crate::low::environment::value::function::builtin::BuiltinFunctionDeclaration;
use crate::low::environment::value::function::regular::{
    RegularFunctionDeclaration, RegularFunctionId,
};
use crate::low::environment::value::function::{FunctionDeclaration, FunctionId};
use crate::low::environment::value::variable::VariableId;
use crate::low::environment::value::{ValueDeclaration, ValueId};
use crate::low::expression::resolved::ResolvedExpression;
use crate::player_score::GeneratedPlayerScore;
use crate::runtime_storage::RuntimeStorageTarget;
use crate::span::Span;
use crate::visibility::Visibility;
use hashbrown::{Equivalent, HashMap as HashbrownMap};
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
use serde_json::json;
use smallvec::SmallVec;
use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::mem::take;

pub mod mcfunction;
pub mod namespace;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct MonomorphizedStructKey {
    pub original_id: HighTypeId,
    pub generics: Vec<ResolvedDataType>,
}

#[derive(Hash, PartialEq, Eq)]
struct MonomorphizedStructKeyRef<'a> {
    pub id: HighTypeId,
    pub generics: &'a [ResolvedDataType],
}

impl Equivalent<MonomorphizedStructKey> for MonomorphizedStructKeyRef<'_> {
    fn equivalent(&self, key: &MonomorphizedStructKey) -> bool {
        self.id == key.original_id && self.generics == key.generics.as_slice()
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct MonomorphizedFunctionKey {
    pub original_id: HighFunctionId,
    pub generic_types: Vec<ResolvedDataType>,
}

#[derive(Hash, PartialEq, Eq)]
struct MonomorphizedFunctionKeyRef<'a> {
    pub original_id: HighFunctionId,
    pub generic_types: &'a [ResolvedDataType],
}

impl Equivalent<MonomorphizedFunctionKey> for MonomorphizedFunctionKeyRef<'_> {
    fn equivalent(&self, key: &MonomorphizedFunctionKey) -> bool {
        self.original_id == key.original_id && self.generic_types == key.generic_types.as_slice()
    }
}

#[derive(Debug, Clone)]
pub struct RuntimeFunction {
    pub resource_location: ResourceLocation,
    pub parameter_targets: Vec<RuntimeStorageTarget>,
    pub return_target: RuntimeStorageTarget,
}

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
    pub high_environment: HighEnvironment,
    pub variable_values: HashMap<VariableId, (ResolvedDataType, ResolvedExpression)>,
    pub function_values: HashMap<RegularFunctionId, RegularFunctionDeclaration>,
    pub function_return_targets: SmallVec<[RuntimeStorageTarget; 5]>,
    namespaces: HashMap<String, DatapackNamespace>,
    namespace_stack: Vec<String>,
    counter: Cell<usize>,
    used_constants: HashSet<i32>,
    used_data: Vec<(DataTarget, LowNbtPath)>,

    monomorphized_structs: HashbrownMap<MonomorphizedStructKey, StructId>,
    monomorphized_functions: HashbrownMap<MonomorphizedFunctionKey, FunctionId>,
    resolved_variables: HashMap<HighVariableId, VariableId>,
    pub runtime_functions: HashMap<FunctionId, RuntimeFunction>,
}

impl Datapack {
    #[must_use]
    pub fn new(
        high_environment: HighEnvironment,
        name: String,
        description: Option<String>,
    ) -> Self {
        Self {
            name,
            description,
            requirements: Cell::new(DatapackRequirements::default()),
            settings: DatapackSettings::default(),
            high_environment,
            environment: Environment::default(),
            variable_values: HashMap::new(),
            function_values: HashMap::new(),
            function_return_targets: SmallVec::new(),
            namespaces: HashMap::new(),
            namespace_stack: Vec::new(),
            counter: Cell::new(0),
            used_constants: HashSet::new(),
            used_data: Vec::new(),

            monomorphized_structs: HashbrownMap::new(),
            monomorphized_functions: HashbrownMap::new(),
            resolved_variables: HashMap::new(),
            runtime_functions: HashMap::new(),
        }
    }

    #[inline]
    #[must_use]
    pub fn get_resolved_variable(&self, id: HighVariableId) -> Option<VariableId> {
        self.resolved_variables.get(&id).copied()
    }

    #[inline]
    #[must_use]
    pub fn create_resource_location(&self, paths: Vec<String>) -> ResourceLocation {
        ResourceLocation::new_namespace_paths(self.current_namespace_name(), paths)
    }

    #[inline]
    #[must_use]
    pub fn get_struct_type(&self, id: StructId) -> (&[String], Visibility, &StructDeclaration) {
        self.environment.get_struct(id)
    }

    #[inline]
    #[must_use]
    pub fn get_struct_struct_type(
        &self,
        id: StructStructId,
    ) -> (&[String], Visibility, &StructStructDeclaration) {
        self.environment.get_struct_struct(id)
    }

    #[inline]
    #[must_use]
    pub fn get_tuple_struct_type(
        &self,
        id: TupleStructId,
    ) -> (&[String], Visibility, &TupleStructDeclaration) {
        self.environment.get_tuple_struct(id)
    }

    #[inline]
    pub fn declare_value(
        &mut self,
        id: HighVariableId,
        data_type: ResolvedDataType,
        value: ResolvedExpression,
    ) {
        let (module_path, visibility, declaration) = self.high_environment.get_value(id.into());

        let resolved_id = self.environment.declare_variable(
            module_path.to_vec(),
            visibility,
            declaration.name().to_owned(),
            data_type.clone(),
        );

        self.resolved_variables.insert(id, resolved_id);

        self.variable_values.insert(resolved_id, (data_type, value));
    }

    #[inline]
    pub fn set_variable(&mut self, id: VariableId, value: ResolvedExpression) {
        self.variable_values.get_mut(&id).unwrap().1 = value;
    }

    #[inline]
    #[must_use]
    pub fn get_value(&self, id: ValueId) -> &ValueDeclaration {
        self.environment.get_value(id)
    }

    #[inline]
    #[must_use]
    pub fn get_variable_value(&self, id: VariableId) -> &(ResolvedDataType, ResolvedExpression) {
        self.variable_values.get(&id).unwrap()
    }

    #[inline]
    #[must_use]
    pub fn get_function_value(&self, id: RegularFunctionId) -> &RegularFunctionDeclaration {
        self.function_values.get(&id).unwrap()
    }

    #[inline]
    #[must_use]
    pub fn get_function<I: Into<FunctionId>>(
        &self,
        id: I,
    ) -> (&[String], Visibility, &FunctionDeclaration) {
        self.environment.get_function(id)
    }

    #[inline]
    #[must_use]
    pub fn get_variable_value_mut(
        &mut self,
        id: VariableId,
    ) -> &mut (ResolvedDataType, ResolvedExpression) {
        self.variable_values.get_mut(&id).unwrap()
    }

    pub fn compile_and_add_to_function(&mut self, paths: &Vec<String>, ctx: &mut CompileContext) {
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

    pub fn get_unique_function_paths(&self) -> Vec<String> {
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

        let path = LowNbtPath(vec![LowNbtPathNode::named_string(format!(
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
            LowNbtPath(vec![LowNbtPathNode::named_string(name.clone())]),
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
            LowNbtPath(vec![LowNbtPathNode::named_string(name.clone())]),
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
            LowNbtPath(vec![LowNbtPathNode::named_string(name.clone())]),
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
            NbtPath(vec![NbtPathNode::Named(
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
    pub fn get_function_mut(&mut self, paths: &Vec<String>) -> &mut MCFunction {
        self.current_namespace_mut().get_function_mut(paths)
    }

    pub fn push_function_to_current_namespace(&mut self, paths: Vec<String>) {
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
            &current_namespace_name,
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
            let load_function = kelp_namespace.get_function_mut(&vec!["load".to_string()]);
            load_function.add_commands(load_function_commands);

            output_datapack.get_namespace_mut("minecraft").add_tag(
                TagType::Function,
                &vec!["load".to_string()],
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
                vec!["always_succeed".to_string()],
                json!({"condition":"minecraft:random_chance","chance":1.0}),
            );
        }

        output_datapack
    }
}

impl Datapack {
    #[inline]
    #[must_use]
    pub fn get_monomorphized_struct_id(
        &self,
        id: HighTypeId,
        generic_types: &[ResolvedDataType],
    ) -> Option<StructId> {
        let key = MonomorphizedStructKeyRef {
            id,
            generics: generic_types,
        };

        self.monomorphized_structs.get(&key).copied()
    }

    #[inline]
    pub fn get_monomorphized_value_id<I: Into<HighValueId>>(
        &mut self,
        id: I,
        generic_types: &[UnresolvedDataType],
    ) -> Option<ValueId> {
        let id = id.into();

        let (_, _, declaration) = self.high_environment.get_value(id);

        match declaration {
            HighValueDeclarationKind::Variable(..) => {
                assert!(generic_types.is_empty());

                let id = HighVariableId(id.0);

                let id = *self.resolved_variables.get(&id)?;

                Some(id.into())
            }
            HighValueDeclarationKind::Function(..) => {
                let id = HighFunctionId(id.0);

                let id = self.get_monomorphized_function_id(id, generic_types);

                Some(id.into())
            }
        }
    }

    #[inline]
    #[must_use]
    pub fn get_monomorphized_function_id<I: Into<HighFunctionId>>(
        &mut self,
        original_id: I,
        generic_types: &[UnresolvedDataType],
    ) -> FunctionId {
        let id = original_id.into();

        let resolved_generic_types = generic_types
            .iter()
            .cloned()
            .map(|data_type| data_type.resolve(self).unwrap())
            .collect::<Vec<_>>();

        let key = MonomorphizedFunctionKeyRef {
            original_id: id,
            generic_types: &resolved_generic_types,
        };

        if let Some(id) = self.monomorphized_functions.get(&key) {
            return *id;
        }

        let (module_path, visibility, declaration) = self.high_environment.get_function(id);

        let module_path = module_path.to_vec();
        let declaration = declaration.clone();

        match declaration {
            HighFunctionDeclaration::Regular(declaration) => {
                let parameters = declaration
                    .parameters
                    .into_iter()
                    .map(|(pattern, data_type)| {
                        let pattern = pattern.unwrap();

                        let data_type = data_type
                            .substitute_generics(&declaration.generic_names, generic_types)
                            .resolve(self)
                            .unwrap();

                        (pattern, data_type)
                    })
                    .collect();

                let return_type = declaration
                    .return_type
                    .substitute_generics(&declaration.generic_names, generic_types)
                    .resolve(self)
                    .unwrap();

                let body = declaration.body.unwrap();

                let declaration = RegularFunctionDeclaration {
                    name: declaration.name,
                    is_runtime: declaration.is_runtime,
                    generic_types: resolved_generic_types,
                    parameters,
                    return_type,
                    body,
                };

                self.declare_monomorphized_function(id, module_path, visibility, declaration)
            }
            HighFunctionDeclaration::Builtin(declaration) => {
                let parameters = declaration
                    .parameters
                    .into_iter()
                    .map(|data_type| {
                        data_type
                            .substitute_generics(&declaration.generic_names, generic_types)
                            .resolve(self)
                            .unwrap()
                    })
                    .collect();

                let return_type = declaration
                    .return_type
                    .substitute_generics(&declaration.generic_names, generic_types)
                    .resolve(self)
                    .unwrap();

                let declaration = BuiltinFunctionDeclaration {
                    name: declaration.name,
                    generic_types: resolved_generic_types,
                    parameters,
                    return_type,
                    kind: declaration.kind,
                };

                self.declare_monomorphized_function(id, module_path, visibility, declaration)
            }
        }
    }

    pub fn declare_monomorphized_struct(
        &mut self,
        original_id: HighTypeId,
        monomorphized_id: StructId,
        generic_types: Vec<ResolvedDataType>,
    ) {
        let key = MonomorphizedStructKey {
            original_id,
            generics: generic_types,
        };

        self.monomorphized_structs.insert(key, monomorphized_id);
    }

    #[inline]
    pub fn declare_monomorphized_struct_struct(
        &mut self,
        module_path: Vec<String>,
        visibility: Visibility,
        original_id: HighTypeId,
        name: String,
        generic_types: Vec<ResolvedDataType>,
        field_types: HashMap<String, ResolvedDataType>,
    ) -> StructStructId {
        let monomorphized_id = self.environment.declare_struct_struct(
            module_path,
            visibility,
            name,
            generic_types.clone(),
            field_types,
        );

        self.declare_monomorphized_struct(original_id, monomorphized_id.into(), generic_types);

        monomorphized_id
    }

    #[inline]
    pub fn declare_monomorphized_tuple_struct(
        &mut self,
        module_path: Vec<String>,
        visibility: Visibility,
        original_id: HighTypeId,
        name: String,
        generic_types: Vec<ResolvedDataType>,
        field_types: Vec<ResolvedDataType>,
    ) -> TupleStructId {
        let monomorphized_id = self.environment.declare_tuple_struct(
            module_path,
            visibility,
            name,
            generic_types.clone(),
            field_types,
        );

        self.declare_monomorphized_struct(original_id, monomorphized_id.into(), generic_types);

        monomorphized_id
    }

    pub fn declare_monomorphized_function<I: Into<HighFunctionId>, D: Into<FunctionDeclaration>>(
        &mut self,
        original_id: I,
        module_path: Vec<String>,
        visibility: Visibility,
        declaration: D,
    ) -> FunctionId {
        let declaration = declaration.into();

        let key = MonomorphizedFunctionKey {
            original_id: original_id.into(),
            generic_types: declaration.generic_types().to_vec(),
        };

        let monomorphized_id =
            self.environment
                .declare_function(module_path, visibility, declaration);

        self.monomorphized_functions.insert(key, monomorphized_id);

        monomorphized_id
    }
}
