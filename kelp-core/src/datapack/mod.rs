use crate::compile_context::CompileContext;
use crate::data::{GeneratedData, GeneratedDataTarget};
use crate::datapack::mcfunction::MCFunction;
use crate::datapack::namespace::DatapackNamespace;
use crate::low::data_type::DataType;
use crate::low::environment::Environment;
use crate::low::environment::r#type::r#struct::{
    RegularStructDeclaration, RegularStructId, StructDeclaration, StructId, TupleStructDeclaration,
    TupleStructId,
};
use crate::low::environment::value::function::builtin::BuiltinFunctionDeclaration;
use crate::low::environment::value::function::regular::{
    RegularFunctionDeclaration, RegularFunctionId,
};
use crate::low::environment::value::function::{FunctionDeclaration, FunctionId};
use crate::low::environment::value::variable::VariableId;
use crate::low::environment::value::{ValueDeclaration, ValueId};
use crate::low::expression::Expression;
use crate::player_score::GeneratedPlayerScore;
use crate::runtime_storage::RuntimeStorageTarget;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::SemanticEnvironment;
use crate::semantic::environment::r#type::HighGenericId;
use crate::semantic::environment::r#type::module::HighModuleId;
use crate::semantic::environment::r#type::r#struct::HighStructId;
use crate::semantic::environment::r#type::r#struct::tuple::HighTupleStructId;
use crate::semantic::environment::value::function::{HighFunctionId, SemanticFunctionDeclaration};
use crate::semantic::environment::value::variable::HighVariableId;
use crate::semantic::environment::value::{
    HighValueId, SemanticValueDeclaration, SemanticValueDeclarationKind,
};
use crate::visibility::Visibility;
use hashbrown::{Equivalent, HashMap as HashbrownMap};
use minecraft_command_types::command::data::{DataCommand, DataTarget};
use minecraft_command_types::command::execute::ExecuteIfSubcommand;
use minecraft_command_types::command::scoreboard::{
    ObjectivesScoreboardCommand, ScoreboardCommand,
};
use minecraft_command_types::command::{Command, PlayerScore, ScoreValue};
use minecraft_command_types::datapack::pack::Pack;
use minecraft_command_types::datapack::pack::format::Format;
use minecraft_command_types::datapack::tag::{Tag, TagType, TagValue};
use minecraft_command_types::datapack::{Datapack as LowDatapack, PackMCMeta};
use minecraft_command_types::entity_selector::EntitySelector;
use minecraft_command_types::nbt_path::{NbtPath, NbtPathNode};
use minecraft_command_types::resource_location::ResourceLocation;
use serde_json::json;
use smallvec::SmallVec;
use std::cell::Cell;
use std::collections::{HashMap, HashSet};
use std::mem::take;

pub mod mcfunction;
pub mod namespace;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct MonomorphizedStructKey {
    pub original_id: HighStructId,
    pub generics: Vec<DataType>,
}

#[derive(Hash, PartialEq, Eq)]
struct MonomorphizedStructKeyRef<'a> {
    pub original_id: HighStructId,
    pub generics: &'a [DataType],
}

impl Equivalent<MonomorphizedStructKey> for MonomorphizedStructKeyRef<'_> {
    fn equivalent(&self, key: &MonomorphizedStructKey) -> bool {
        self.original_id == key.original_id && self.generics == key.generics.as_slice()
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct MonomorphizedFunctionKey {
    pub original_id: HighFunctionId,
    pub generic_types: Vec<DataType>,
}

#[derive(Hash, PartialEq, Eq)]
struct MonomorphizedFunctionKeyRef<'a> {
    pub original_id: HighFunctionId,
    pub generic_types: &'a [DataType],
}

impl Equivalent<MonomorphizedFunctionKey> for MonomorphizedFunctionKeyRef<'_> {
    fn equivalent(&self, key: &MonomorphizedFunctionKey) -> bool {
        self.original_id == key.original_id && self.generic_types == key.generic_types.as_slice()
    }
}

#[derive(Debug, Clone)]
pub struct RegularRuntimeFunction {
    pub resource_location: ResourceLocation,
    pub parameter_targets: Vec<RuntimeStorageTarget>,
    pub return_target: RuntimeStorageTarget,
}

#[derive(Debug, Clone)]
pub struct RecursiveRuntimeFunction {
    pub resource_location: ResourceLocation,
    pub arguments_stack: GeneratedData,
    pub return_data: GeneratedData,
    pub prefix_data: GeneratedData,
}

#[derive(Debug, Clone)]
pub enum RuntimeFunction {
    Regular(RegularRuntimeFunction),
    Recursive(Box<RecursiveRuntimeFunction>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CompiletimeFunctionKey {
    pub id: FunctionId,
    pub arguments: Vec<Expression>,
}

#[derive(Hash, PartialEq, Eq)]
pub struct CompiletimeFunctionKeyRef<'a> {
    pub id: FunctionId,
    pub arguments: &'a [Expression],
}

impl Equivalent<CompiletimeFunctionKey> for CompiletimeFunctionKeyRef<'_> {
    fn equivalent(&self, key: &CompiletimeFunctionKey) -> bool {
        self.id == key.id && self.arguments == key.arguments.as_slice()
    }
}

#[derive(Debug, Clone)]
pub struct CompiletimeFunction {
    pub resource_location: ResourceLocation,
    pub return_value: Expression,
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
    pub semantic_environment: SemanticEnvironment,
    pub variable_values: HashMap<VariableId, (DataType, Expression)>,
    pub function_values: HashMap<RegularFunctionId, RegularFunctionDeclaration>,
    pub function_return_targets: SmallVec<[RuntimeStorageTarget; 5]>,
    namespaces: HashMap<String, DatapackNamespace>,
    namespace_stack: Vec<String>,
    counter: usize,
    used_scoreboard_constants: HashSet<ScoreValue>,
    used_data: Vec<(GeneratedDataTarget, String)>,

    monomorphized_structs: HashbrownMap<MonomorphizedStructKey, StructId>,
    monomorphized_functions: HashbrownMap<MonomorphizedFunctionKey, FunctionId>,
    generic_mapping: HashMap<HighGenericId, DataType>,
    resolved_variables: HashMap<HighVariableId, VariableId>,
    pub cached_runtime_functions: HashMap<FunctionId, RuntimeFunction>,
    pub cached_compiletime_functions: HashbrownMap<CompiletimeFunctionKey, CompiletimeFunction>,
    pub prefix_data: Option<GeneratedData>,
}

impl Datapack {
    #[must_use]
    pub fn new(
        semantic_environment: SemanticEnvironment,
        name: String,
        description: Option<String>,
    ) -> Self {
        Self {
            name,
            description,
            requirements: Cell::new(DatapackRequirements::default()),
            settings: DatapackSettings::default(),
            semantic_environment,
            environment: Environment::default(),
            variable_values: HashMap::new(),
            function_values: HashMap::new(),
            function_return_targets: SmallVec::new(),
            namespaces: HashMap::new(),
            namespace_stack: Vec::new(),
            counter: 0,
            used_scoreboard_constants: HashSet::new(),
            used_data: Vec::new(),

            monomorphized_structs: HashbrownMap::new(),
            monomorphized_functions: HashbrownMap::new(),
            generic_mapping: HashMap::new(),

            resolved_variables: HashMap::new(),
            cached_runtime_functions: HashMap::new(),
            cached_compiletime_functions: HashbrownMap::new(),
            prefix_data: None,
        }
    }

    #[inline]
    #[must_use]
    pub fn resolve_generic(&self, id: HighGenericId) -> Option<DataType> {
        self.generic_mapping.get(&id).cloned()
    }

    #[inline]
    pub fn declare_generic(&mut self, id: HighGenericId, data_type: DataType) {
        self.generic_mapping.insert(id, data_type);
    }

    #[inline]
    #[must_use]
    pub fn create_resource_location(&self, paths: Vec<String>) -> ResourceLocation {
        ResourceLocation::new_namespace_paths(self.current_namespace_name(), paths)
    }

    #[inline]
    #[must_use]
    pub fn get_struct_type(
        &self,
        id: StructId,
    ) -> (&[HighModuleId], Visibility, &StructDeclaration) {
        self.environment.get_struct(id)
    }

    #[inline]
    #[must_use]
    pub fn get_regular_struct_type(
        &self,
        id: RegularStructId,
    ) -> (&[HighModuleId], Visibility, &RegularStructDeclaration) {
        self.environment.get_regular_struct(id)
    }

    #[inline]
    #[must_use]
    pub fn get_tuple_struct_type(
        &self,
        id: TupleStructId,
    ) -> (&[HighModuleId], Visibility, &TupleStructDeclaration) {
        self.environment.get_tuple_struct(id)
    }

    #[inline]
    pub fn declare_value(&mut self, id: HighVariableId, data_type: DataType, value: Expression) {
        let SemanticValueDeclaration {
            module_path,
            visibility,
            kind: declaration,
        } = self.semantic_environment.get_value(id);

        let resolved_id = self.environment.declare_variable(
            module_path.clone(),
            *visibility,
            declaration.name().to_owned(),
            data_type.clone(),
        );

        self.resolved_variables.insert(id, resolved_id);

        self.variable_values.insert(resolved_id, (data_type, value));
    }

    #[inline]
    pub fn set_variable(&mut self, id: VariableId, value: Expression) {
        self.variable_values.get_mut(&id).unwrap().1 = value;
    }

    #[inline]
    #[must_use]
    pub fn get_value(&self, id: ValueId) -> &ValueDeclaration {
        self.environment.get_value(id)
    }

    #[inline]
    #[must_use]
    pub fn get_variable_value(&self, id: VariableId) -> Expression {
        let (_, expression) = self.variable_values.get(&id).unwrap();

        expression.clone()
    }

    #[inline]
    #[must_use]
    pub fn get_variable_value_mut(&mut self, id: VariableId) -> &mut Expression {
        let (_, expression) = self.variable_values.get_mut(&id).unwrap();

        expression
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
    ) -> (&[HighModuleId], Visibility, &FunctionDeclaration) {
        self.environment.get_function(id)
    }

    pub fn compile_and_add_to_function(&mut self, paths: &[String], ctx: &mut CompileContext) {
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

    pub const fn increment_counter(&mut self) -> usize {
        let val = self.counter;

        self.counter += 1;

        val
    }

    pub fn get_unique_function_paths(&self) -> Vec<String> {
        self.current_namespace().get_unique_function_paths()
    }

    pub fn get_unique_score(&mut self) -> GeneratedPlayerScore {
        let incremented = self.increment_counter();

        let current_namespace = self.current_namespace();

        let selector = current_namespace.get_score_selector(incremented);

        GeneratedPlayerScore {
            is_generated: true,
            score: PlayerScore::new(selector, current_namespace.get_scores_objective()),
        }
    }

    pub fn get_unique_data(&mut self) -> GeneratedData {
        let counter = self.increment_counter();
        let current_namespace_name = self.current_namespace_name();

        let name = format!("__kelp_{}_storage_{}__", current_namespace_name, counter,);

        let name_node = NbtPathNode::named_string(name.clone());

        if let Some(data_prefix) = self.prefix_data.clone() {
            let data = data_prefix.with_path_node(name_node);

            self.used_data.push((data.target.clone(), name));

            return data;
        }

        let target = DataTarget::Storage(ResourceLocation::new_namespace_path(
            "__kelp_storages__",
            name.clone(),
        ));

        let path = NbtPath(vec![name_node]);

        let data = GeneratedData {
            target: GeneratedDataTarget {
                is_generated: true,
                target,
            },
            path,
        };

        self.used_data.push((data.target.clone(), name));

        data
    }

    pub fn get_unique_data_named(&mut self) -> (GeneratedData, String) {
        let counter = self.increment_counter();
        let current_namespace_name = self.current_namespace_name();

        let name = format!("__kelp_{}_storage_{}__", current_namespace_name, counter);

        (
            GeneratedData {
                target: GeneratedDataTarget {
                    is_generated: true,
                    target: DataTarget::Storage(ResourceLocation::new_namespace_path(
                        "__kelp_storages__",
                        format!("__kelp_{}_storage__", current_namespace_name),
                    )),
                },
                path: NbtPath(vec![NbtPathNode::named_string(name.clone())]),
            },
            name,
        )
    }

    pub fn get_constant_score(&mut self, constant: ScoreValue) -> GeneratedPlayerScore {
        self.used_scoreboard_constants.insert(constant);

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
    pub fn get_constant_selector(constant: ScoreValue) -> EntitySelector {
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
    pub fn get_function_mut(&mut self, paths: &[String]) -> &mut MCFunction {
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
            Command::Function(loop_function_location.clone(), None)
                .run()
                .conditionally(should_be_inverted, execute_condition.clone()),
        );

        let mut loop_ctx = CompileContext::default();
        body(self, &mut loop_ctx);

        loop_ctx.add_command(
            self,
            Command::Function(loop_function_location, None)
                .run()
                .conditionally(should_be_inverted, execute_condition),
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

        let has_constants = !self.used_scoreboard_constants.is_empty();

        if has_constants {
            let constants_objective = self.get_constants_objective();

            load_function_ctx.add_command(
                &mut self,
                ScoreboardCommand::Objectives(ObjectivesScoreboardCommand::Add(
                    constants_objective.clone(),
                    "dummy".to_string(),
                    None,
                )),
            );

            for constant in take(&mut self.used_scoreboard_constants) {
                load_function_ctx.add_command(
                    &mut self,
                    PlayerScore::new(
                        EntitySelector::Name(format!("__kelp_constant_{}__", constant)),
                        constants_objective.clone(),
                    )
                    .set_value(constant),
                );
            }
        }

        let has_data = !self.used_data.is_empty();

        if has_data {
            for (target, name) in take(&mut self.used_data) {
                load_function_ctx.add_command(
                    &mut self,
                    DataCommand::Remove(
                        target.target,
                        NbtPath(vec![NbtPathNode::named_string(name)]),
                    ),
                );
            }
        }

        if has_constants || has_data {
            let load_function_commands = load_function_ctx.compile();

            let kelp_namespace = self.get_namespace_mut("kelp");
            let load_function = kelp_namespace.get_function_mut(&["load".to_string()]);
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
        id: HighStructId,
        generic_types: &[DataType],
    ) -> Option<StructId> {
        let key = MonomorphizedStructKeyRef {
            original_id: id,
            generics: generic_types,
        };

        self.monomorphized_structs.get(&key).copied()
    }

    #[inline]
    #[must_use]
    pub fn get_monomorphized_tuple_struct_id(
        &self,
        id: HighTupleStructId,
        generic_types: &[DataType],
    ) -> Option<TupleStructId> {
        let id = self.get_monomorphized_struct_id(id.into(), generic_types)?;

        Some(TupleStructId(id.0))
    }

    #[inline]
    pub fn get_monomorphized_value_id<I: Into<HighValueId>>(
        &mut self,
        id: I,
        generic_types: &[SemanticDataType],
    ) -> Option<ValueId> {
        let id = id.into();

        let SemanticValueDeclaration {
            kind: declaration, ..
        } = self.semantic_environment.get_value(id);

        match declaration {
            SemanticValueDeclarationKind::Variable(..) => {
                assert!(generic_types.is_empty());

                let id = HighVariableId(id.0);

                let id = *self.resolved_variables.get(&id)?;

                Some(id.into())
            }
            SemanticValueDeclarationKind::Function(..) => {
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
        generic_types: &[SemanticDataType],
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

        let (module_path, visibility, declaration) = self.semantic_environment.get_function(id);

        let module_path = module_path.to_vec();
        let declaration = declaration.clone();

        match declaration {
            SemanticFunctionDeclaration::Regular(declaration) => {
                let parameters = declaration
                    .parameters
                    .into_iter()
                    .map(|(pattern, data_type)| {
                        let pattern = pattern.unwrap();

                        let data_type = data_type
                            .substitute_generics(&declaration.generic_ids, generic_types)
                            .resolve(self)
                            .unwrap();

                        (pattern, data_type)
                    })
                    .collect();

                let return_type = declaration
                    .return_type
                    .substitute_generics(&declaration.generic_ids, generic_types)
                    .resolve(self)
                    .unwrap();

                let body = *declaration.body.unwrap();

                let declaration = RegularFunctionDeclaration {
                    module_path: module_path.clone(),
                    visibility,
                    name: declaration.name,
                    modifiers: declaration.modifiers,
                    generic_ids: declaration.generic_ids,
                    generic_types: resolved_generic_types,
                    parameters,
                    return_type,
                    body,
                };

                self.declare_monomorphized_function(id, module_path, visibility, declaration)
            }
            SemanticFunctionDeclaration::Builtin(declaration) => {
                let parameters = declaration
                    .parameters
                    .into_iter()
                    .map(|data_type| {
                        data_type
                            .substitute_generics(&declaration.generic_ids, generic_types)
                            .resolve(self)
                            .unwrap()
                    })
                    .collect();

                let return_type = declaration
                    .return_type
                    .substitute_generics(&declaration.generic_ids, generic_types)
                    .resolve(self)
                    .unwrap();

                let kind = declaration.kind.monomorphize(self, generic_types);

                let declaration = BuiltinFunctionDeclaration {
                    name: declaration.name,
                    generic_types: resolved_generic_types,
                    parameters,
                    return_type,
                    kind,
                };

                self.declare_monomorphized_function(id, module_path, visibility, declaration)
            }
        }
    }

    pub fn declare_monomorphized_struct(
        &mut self,
        original_id: HighStructId,
        monomorphized_id: StructId,
        generic_types: Vec<DataType>,
    ) {
        let key = MonomorphizedStructKey {
            original_id,
            generics: generic_types,
        };

        self.monomorphized_structs.insert(key, monomorphized_id);
    }

    #[inline]
    pub fn declare_monomorphized_regular_struct(
        &mut self,
        module_path: Vec<HighModuleId>,
        visibility: Visibility,
        original_id: HighStructId,
        name: String,
        generic_types: Vec<DataType>,
        field_types: HashMap<String, DataType>,
    ) -> RegularStructId {
        let monomorphized_id = self.environment.declare_regular_struct(
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
        module_path: Vec<HighModuleId>,
        visibility: Visibility,
        original_id: HighStructId,
        name: String,
        generic_types: Vec<DataType>,
        field_types: Vec<DataType>,
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
        module_path: Vec<HighModuleId>,
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
