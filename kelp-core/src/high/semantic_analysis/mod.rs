use hashbrown::{Equivalent, HashMap};
use std::{collections::HashMap as StdHashMap, hint::unreachable_unchecked};

use crate::{
    builtin_data_type::BuiltinDataType,
    high::{
        environment::{
            HighEnvironment,
            r#type::{
                HighTypeDeclaration, HighTypeDeclarationKind, HighTypeId,
                alias::HighAliasDeclaration,
                module::HighModuleDeclaration,
                r#struct::{
                    HighStructDeclaration, HighStructStructDeclaration, HighTupleStructDeclaration,
                },
            },
            value::{
                HighValueDeclaration, HighValueDeclarationKind, HighValueId,
                function::HighFunctionDeclaration, variable::HighVariableDeclaration,
            },
        },
        semantic_analysis::{
            info::{SemanticAnalysisInfo, SemanticAnalysisInfoKind, error::SemanticAnalysisError},
            scope::Scope,
        },
    },
    low::{
        data_type::DataType,
        environment::{
            Environment,
            r#type::r#struct::{
                StructDeclaration, StructId, StructStructDeclaration, StructStructId,
                TupleStructDeclaration, TupleStructId,
            },
            value::{function::FunctionId, variable::VariableId},
        },
    },
    path::{generic::GenericPath, regular::Path},
    span::Span,
    visibility::Visibility,
};

pub mod info;
pub mod scope;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct MonomorphizedStructKey {
    pub original_id: HighTypeId,
    pub generics: Vec<DataType>,
}

#[derive(Hash, PartialEq, Eq)]
struct MonomorphizedStructKeyRef<'a> {
    pub id: HighTypeId,
    pub generics: &'a [DataType],
}

impl Equivalent<MonomorphizedStructKey> for MonomorphizedStructKeyRef<'_> {
    fn equivalent(&self, key: &MonomorphizedStructKey) -> bool {
        self.id == key.original_id && self.generics == key.generics.as_slice()
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct MonomorphizedFunctionKey {
    pub original_id: HighValueId,
    pub generics: Vec<DataType>,
}

#[derive(Hash, PartialEq, Eq)]
struct MonomorphizedFunctionKeyRef<'a> {
    pub id: HighValueId,
    pub generics: &'a [DataType],
}

impl Equivalent<MonomorphizedFunctionKey> for MonomorphizedFunctionKeyRef<'_> {
    fn equivalent(&self, key: &MonomorphizedFunctionKey) -> bool {
        self.id == key.original_id && self.generics == key.generics.as_slice()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ResolvedItem {
    Type(HighTypeId),
    Value(HighValueId),
}

#[derive(Debug, Clone)]
pub struct SemanticAnalysisContext {
    pub infos: Vec<SemanticAnalysisInfo>,
    pub scopes: Vec<Scope>,
    pub loop_depth: u32,
    pub is_lhs: bool,
    pub current_module_path: Vec<String>,
    max_infos: usize,
    pub environment: Environment,
    high_environment: HighEnvironment,
    monomorphized_structs: HashMap<MonomorphizedStructKey, StructId>,
    monomorphized_functions: HashMap<MonomorphizedFunctionKey, FunctionId>,
    resolved_variables: HashMap<HighValueId, VariableId>,
}

impl SemanticAnalysisContext {
    #[must_use]
    pub fn new(max_infos: usize) -> Self {
        let mut self_ = Self {
            infos: Vec::new(),
            max_infos,
            environment: Environment::default(),
            high_environment: HighEnvironment::default(),
            scopes: vec![Scope::default()],
            monomorphized_structs: HashMap::new(),
            monomorphized_functions: HashMap::new(),
            resolved_variables: HashMap::new(),
            loop_depth: 0,
            is_lhs: false,
            current_module_path: Vec::new(),
        };

        let builtins = [
            BuiltinDataType::Boolean,
            BuiltinDataType::Byte,
            BuiltinDataType::Short,
            BuiltinDataType::Integer,
            BuiltinDataType::Long,
            BuiltinDataType::Float,
            BuiltinDataType::Double,
            BuiltinDataType::String,
            // BuiltinDataType::Unit,
            BuiltinDataType::Score,
            BuiltinDataType::List,
            BuiltinDataType::Compound,
            BuiltinDataType::Data,
            BuiltinDataType::SNBT,
            BuiltinDataType::ResourceLocation,
        ];

        self_.enter_module("std".to_owned());

        for builtin in builtins {
            self_.declare_builtin(builtin);
        }

        self_.exit_module_and_declare(Visibility::Public);

        self_.enter_module("mod".to_owned());

        self_
    }

    #[inline]
    #[must_use]
    pub fn get_monomorphized_struct_id(
        &self,
        id: HighTypeId,
        generic_types: &[DataType],
    ) -> Option<StructId> {
        let key = MonomorphizedStructKeyRef {
            id,
            generics: generic_types,
        };

        self.monomorphized_structs.get(&key).copied()
    }

    #[inline]
    #[must_use]
    pub fn get_monomorphized_function_id(
        &self,
        id: HighValueId,
        generic_types: &[DataType],
    ) -> Option<FunctionId> {
        let key = MonomorphizedFunctionKeyRef {
            id,
            generics: generic_types,
        };

        self.monomorphized_functions.get(&key).copied()
    }

    #[inline]
    pub fn enter_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    #[inline]
    pub fn exit_scope(&mut self) -> Scope {
        self.scopes.pop().unwrap()
    }

    #[inline]
    pub fn enter_module(&mut self, name: String) {
        self.current_module_path.push(name);

        self.enter_scope();
    }

    #[inline]
    #[must_use]
    pub fn exit_module(&mut self) -> HighModuleDeclaration {
        let name = self.current_module_path.pop().unwrap();

        let scope = self.exit_scope();

        HighModuleDeclaration {
            name,
            types: scope.types,
            values: scope.values,
        }
    }

    pub fn exit_module_and_declare(&mut self, visibility: Visibility) {
        let module = self.exit_module();

        self.declare_module(visibility, module);
    }

    #[must_use]
    pub fn is_item_visible(&self, visibility: Visibility, target_module_path: &[String]) -> bool {
        if matches!(visibility, Visibility::Public) {
            return true;
        }

        self.current_module_path.starts_with(target_module_path)
    }

    #[inline]
    pub fn add_invalid_generics<T>(
        &mut self,
        span: Span,
        data_type_name: String,
        expected: usize,
        actual: usize,
    ) -> Option<T> {
        self.add_error(
            span,
            SemanticAnalysisError::InvalidGenerics {
                data_type_name,
                expected,
                actual,
            },
        )
    }

    pub fn add_info<T>(&mut self, info: SemanticAnalysisInfo) -> Option<T> {
        if self.infos.len() >= self.max_infos {
            return None;
        }

        self.infos.push(info);

        None
    }

    pub fn add_error<T>(&mut self, span: Span, error: SemanticAnalysisError) -> Option<T> {
        self.add_info(SemanticAnalysisInfo {
            span,
            kind: SemanticAnalysisInfoKind::Error(error),
        })
    }

    pub fn declare_monomorphized_struct(
        &mut self,
        original_id: HighTypeId,
        monomorphized_id: StructId,
        generic_types: Vec<DataType>,
    ) -> StructId {
        let key = MonomorphizedStructKey {
            original_id,
            generics: generic_types,
        };

        self.monomorphized_structs.insert(key, monomorphized_id);

        monomorphized_id
    }

    #[inline]
    pub fn declare_monomorphized_struct_struct(
        &mut self,
        visibility: Visibility,
        original_id: HighTypeId,
        name: String,
        generic_types: Vec<DataType>,
        field_types: StdHashMap<String, DataType>,
    ) -> StructId {
        let id = self.environment.declare_struct_struct(
            self.current_module_path.clone(),
            visibility,
            name,
            generic_types.clone(),
            field_types,
        );

        self.declare_monomorphized_struct(original_id, id, generic_types)
    }

    #[inline]
    pub fn declare_monomorphized_tuple_struct(
        &mut self,
        visibility: Visibility,
        original_id: HighTypeId,
        name: String,
        generic_types: Vec<DataType>,
        field_types: Vec<DataType>,
    ) -> StructId {
        let monomorphized_id = self.environment.declare_tuple_struct(
            self.current_module_path.clone(),
            visibility,
            name,
            generic_types.clone(),
            field_types,
        );

        self.declare_monomorphized_struct(original_id, monomorphized_id, generic_types)
    }

    pub fn declare_monomorphized_function(
        &mut self,
        original_id: HighValueId,
        visibility: Visibility,
        name: String,
        generic_types: Vec<DataType>,
        parameter_types: Vec<DataType>,
        return_type: DataType,
    ) -> FunctionId {
        let monomorphized_id = self.environment.declare_function(
            self.current_module_path.clone(),
            visibility,
            name,
            generic_types.clone(),
            parameter_types,
            return_type,
        );

        let key = MonomorphizedFunctionKey {
            original_id,
            generics: generic_types,
        };

        self.monomorphized_functions.insert(key, monomorphized_id);

        monomorphized_id
    }

    #[must_use]
    pub fn declare_variable(
        &mut self,
        visibility: Visibility,
        name: String,
        data_type: Option<DataType>,
    ) -> HighValueId {
        let id = self.high_environment.declare_value(HighValueDeclaration {
            visibility,
            module_path: self.current_module_path.clone(),
            kind: HighValueDeclarationKind::Variable(HighVariableDeclaration {
                name: name.clone(),
                data_type,
            }),
        });

        self.scopes
            .last_mut()
            .expect("No scopes")
            .declare_value(name, id);

        id
    }

    #[inline]
    #[must_use]
    pub fn declare_variable_known(
        &mut self,
        visibility: Visibility,
        name: String,
        data_type: DataType,
    ) -> HighValueId {
        self.declare_variable(visibility, name, Some(data_type))
    }

    #[inline]
    #[must_use]
    pub fn declare_variable_unknown(
        &mut self,
        visibility: Visibility,
        name: String,
    ) -> HighValueId {
        self.declare_variable(visibility, name, None)
    }

    #[must_use]
    pub fn get_value_id(&self, name: &str) -> Option<HighValueId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.values.get(name))
            .copied()
    }

    #[must_use]
    pub fn type_is_declared_in_current_scope(&self, name: &str) -> bool {
        self.scopes.last().unwrap().types.contains_key(name)
    }

    #[inline]
    #[must_use]
    pub fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    pub fn declare_type_in_current_scope(&mut self, name_span: Span, name: String, id: HighTypeId) {
        let scope = self.current_scope_mut();

        if scope.types.contains_key(&name) {
            self.add_error::<()>(name_span, SemanticAnalysisError::TypeAlreadyDeclared(name));

            return;
        }

        scope.types.insert(name, id);
    }

    pub fn declare_value_in_current_scope(
        &mut self,
        name_span: Span,
        name: String,
        id: HighValueId,
    ) {
        let scope = self.current_scope_mut();

        if scope.values.contains_key(&name) {
            self.add_error::<()>(
                name_span,
                SemanticAnalysisError::ValueIsAlreadyDefined(name),
            );

            return;
        }

        scope.values.insert(name, id);
    }

    pub fn declare_type_if_not_defined(&mut self, name: String, id: HighTypeId) {
        let scope = self.current_scope_mut();

        if scope.types.contains_key(&name) {
            return;
        }

        scope.types.insert(name, id);
    }

    pub fn declare_value_if_not_defined(&mut self, name: String, id: HighValueId) {
        let scope = self.current_scope_mut();

        if scope.values.contains_key(&name) {
            return;
        }

        scope.values.insert(name, id);
    }

    #[inline]
    pub fn declare_module(
        &mut self,
        visibility: Visibility,
        declaration: HighModuleDeclaration,
    ) -> HighTypeId {
        self.declare_type(visibility, HighTypeDeclarationKind::Module(declaration))
    }

    #[inline]
    pub fn declare_alias(
        &mut self,
        visibility: Visibility,
        declaration: HighAliasDeclaration,
    ) -> HighTypeId {
        self.declare_type(visibility, HighTypeDeclarationKind::Alias(declaration))
    }

    #[inline]
    fn declare_struct(
        &mut self,
        visibility: Visibility,
        declaration: HighStructDeclaration,
    ) -> HighTypeId {
        self.declare_type(visibility, HighTypeDeclarationKind::Struct(declaration))
    }

    #[inline]
    pub fn declare_struct_struct(
        &mut self,
        visibility: Visibility,
        declaration: HighStructStructDeclaration,
    ) -> HighTypeId {
        self.declare_struct(visibility, HighStructDeclaration::Struct(declaration))
    }

    #[inline]
    pub fn declare_tuple_struct(
        &mut self,
        visibility: Visibility,
        declaration: HighTupleStructDeclaration,
    ) -> HighTypeId {
        self.declare_struct(visibility, HighStructDeclaration::Tuple(declaration))
    }

    #[inline]
    pub fn declare_builtin(&mut self, data_type: BuiltinDataType) -> HighTypeId {
        self.declare_type(
            Visibility::Public,
            HighTypeDeclarationKind::Builtin(data_type),
        )
    }

    fn declare_type(
        &mut self,
        visibility: Visibility,
        declaration: HighTypeDeclarationKind,
    ) -> HighTypeId {
        let name = declaration.name().to_owned();

        let id = self.high_environment.declare_type(HighTypeDeclaration {
            visibility,
            module_path: self.current_module_path.clone(),
            kind: declaration,
        });

        self.current_scope_mut().declare_type(name, id);

        id
    }

    #[inline]
    #[must_use]
    pub fn get_resolved_variable(&self, original_id: HighValueId) -> Option<VariableId> {
        self.resolved_variables.get(&original_id).copied()
    }

    #[inline]
    pub fn declare_resolved_variable(&mut self, original_id: HighValueId, resolved_id: VariableId) {
        self.resolved_variables.insert(original_id, resolved_id);
    }

    fn declare_value(
        &mut self,
        visibility: Visibility,
        declaration: HighValueDeclarationKind,
    ) -> HighValueId {
        let name = declaration.name().to_owned();

        let id = self.high_environment.declare_value(HighValueDeclaration {
            visibility,
            module_path: self.current_module_path.clone(),
            kind: declaration,
        });

        self.current_scope_mut().declare_value(name, id);

        id
    }

    pub fn declare_function(
        &mut self,
        visibility: Visibility,
        name: String,
        generic_names: Vec<String>,
        parameter_types: Vec<DataType>,
        return_type: DataType,
    ) -> HighValueId {
        self.declare_value(
            visibility,
            HighValueDeclarationKind::Function(HighFunctionDeclaration {
                name,
                generic_names,
                parameter_types,
                return_type,
            }),
        )
    }

    #[must_use]
    pub fn get_visible_type_id<T>(&mut self, path: &GenericPath<T>) -> Option<HighTypeId> {
        let (mut current_segment, segments) = path.segments.split_first()?;

        let Some(mut current_type_id) = self.get_type_id(&current_segment.name) else {
            return self.add_error(
                current_segment.name_span,
                SemanticAnalysisError::UnknownType(current_segment.name.clone()),
            );
        };

        for segment in segments {
            let module = self.get_visible_module(
                current_segment.name_span,
                &current_segment.name,
                current_type_id,
            )?;

            let Some(next_type_id) = module.get_type_id(&segment.name) else {
                let module_name = module.name.clone();
                return self.add_error(
                    segment.name_span,
                    SemanticAnalysisError::ModuleDoesntContainType {
                        module_name,
                        type_name: segment.name.clone(),
                    },
                );
            };

            current_type_id = next_type_id;
            current_segment = segment;
        }

        let (visibility, module_path, _) = self.get_type(current_type_id).as_tuple();
        let is_visible = self.is_item_visible(visibility, module_path);

        if !is_visible {
            return self.add_error(
                current_segment.name_span,
                SemanticAnalysisError::TypeNotPublic(current_segment.name.clone()),
            );
        }

        Some(current_type_id)
    }

    #[must_use]
    pub fn get_visible_value_id<T>(&mut self, path: &GenericPath<T>) -> Option<HighValueId> {
        let (module_segment, segments) = path.segments.split_first()?;

        if segments.is_empty() {
            let Some(resolved_value_id) = self.get_value_id(&module_segment.name) else {
                return self.add_error(
                    module_segment.name_span,
                    SemanticAnalysisError::UnknownValue(module_segment.name.clone()),
                );
            };

            return Some(resolved_value_id);
        }

        let (target_value_segment, segments) = segments.split_last().unwrap();

        let Some(mut current_type_id) = self.get_type_id(&module_segment.name) else {
            return self.add_error(
                module_segment.name_span,
                SemanticAnalysisError::UnknownModule(module_segment.name.clone()),
            );
        };

        let mut previous_segment = module_segment;

        for type_segment in segments {
            let module = self.get_visible_module(
                previous_segment.name_span,
                &previous_segment.name,
                current_type_id,
            )?;

            let Some(next_type_id) = module.get_type_id(&type_segment.name) else {
                let module_name = module.name.clone();

                return self.add_error(
                    type_segment.name_span,
                    SemanticAnalysisError::ModuleDoesntContainType {
                        module_name,
                        type_name: type_segment.name.clone(),
                    },
                );
            };

            current_type_id = next_type_id;
            previous_segment = type_segment;
        }

        let module = self.get_visible_module(
            previous_segment.name_span,
            &previous_segment.name,
            current_type_id,
        )?;

        let Some(resolved_value_id) = module.get_value_id(&target_value_segment.name) else {
            let module_name = module.name.clone();

            return self.add_error(
                target_value_segment.name_span,
                SemanticAnalysisError::ModuleDoesntContainValue {
                    module_name,
                    value_name: target_value_segment.name.clone(),
                },
            );
        };

        let declaration = self.get_value(resolved_value_id);
        let is_visible = self.is_item_visible(declaration.visibility, &declaration.module_path);

        if !is_visible {
            return self.add_error(
                target_value_segment.name_span,
                SemanticAnalysisError::ValueNotPublic(target_value_segment.name.clone()),
            );
        }

        Some(resolved_value_id)
    }

    #[must_use]
    pub fn get_visible_item(&mut self, path: &Path) -> Option<ResolvedItem> {
        let mut type_segments = path.segments.iter();

        if let Some(first_segment) = type_segments.next()
            && let Some(mut current_type_id) = self.get_type_id(&first_segment.name)
        {
            let mut found_type = true;

            for segment in type_segments {
                let (visibility, module_path, HighTypeDeclarationKind::Module(declaration)) =
                    self.get_type(current_type_id).as_tuple()
                else {
                    found_type = false;

                    break;
                };

                let is_visible = self.is_item_visible(visibility, module_path);

                if !is_visible {
                    return self.add_error(
                        segment.span,
                        SemanticAnalysisError::TypeNotPublic(segment.name.clone()),
                    );
                }

                let Some(next_type_id) = declaration.get_type_id(&segment.name) else {
                    found_type = false;

                    break;
                };

                current_type_id = next_type_id;
            }

            if found_type {
                let (visibility, module_path, _) = self.get_type(current_type_id).as_tuple();
                let is_visible = self.is_item_visible(visibility, module_path);

                if !is_visible {
                    let last_segment = path.segments.last().unwrap();

                    return self.add_error(
                        last_segment.span,
                        SemanticAnalysisError::TypeNotPublic(last_segment.name.clone()),
                    );
                }

                return Some(ResolvedItem::Type(current_type_id));
            }
        }

        let (first_segment, segments) = path.segments.split_first()?;

        if segments.is_empty() {
            let Some(resolved_value_id) = self.get_value_id(&first_segment.name) else {
                return self.add_error(
                    first_segment.span,
                    SemanticAnalysisError::UnknownItem(first_segment.name.clone()),
                );
            };

            return Some(ResolvedItem::Value(resolved_value_id));
        }

        let (last_segment, segments) = segments.split_last().unwrap();

        let Some(mut current_type_id) = self.get_type_id(&first_segment.name) else {
            return self.add_error(
                first_segment.span,
                SemanticAnalysisError::UnknownModule(first_segment.name.clone()),
            );
        };

        let mut previous_segment = first_segment;

        for segment in segments {
            let declaration = self.get_visible_module(
                previous_segment.span,
                &previous_segment.name,
                current_type_id,
            )?;

            let Some(next_type_id) = declaration.get_type_id(&segment.name) else {
                let module_name = declaration.name.clone();
                return self.add_error(
                    segment.span,
                    SemanticAnalysisError::ModuleDoesntContainType {
                        module_name,
                        type_name: segment.name.clone(),
                    },
                );
            };

            current_type_id = next_type_id;
            previous_segment = segment;
        }

        let declaration = self.get_visible_module(
            previous_segment.span,
            &previous_segment.name,
            current_type_id,
        )?;

        let Some(resolved_value_id) = declaration.get_value_id(&last_segment.name) else {
            let module_name = declaration.name.clone();
            return self.add_error(
                last_segment.span,
                SemanticAnalysisError::ModuleDoesntContainItem {
                    module_name,
                    item_name: last_segment.name.clone(),
                },
            );
        };

        let declaration = self.get_value(resolved_value_id);
        let is_visible = self.is_item_visible(declaration.visibility, &declaration.module_path);

        if !is_visible {
            return self.add_error(
                last_segment.span,
                SemanticAnalysisError::ValueNotPublic(last_segment.name.clone()),
            );
        }

        Some(ResolvedItem::Value(resolved_value_id))
    }

    #[must_use]
    fn get_type_id(&self, name: &str) -> Option<HighTypeId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.types.get(name))
            .copied()
    }

    #[must_use]
    pub fn get_type(&self, id: HighTypeId) -> &HighTypeDeclaration {
        &self.high_environment.types[id.0]
    }

    #[must_use]
    pub fn get_value(&self, id: HighValueId) -> &HighValueDeclaration {
        &self.high_environment.values[id.0]
    }

    #[must_use]
    pub fn get_visible_module(
        &mut self,
        name_span: Span,
        name: &str,
        id: HighTypeId,
    ) -> Option<&HighModuleDeclaration> {
        let (visibility, module_path, HighTypeDeclarationKind::Module(_)) =
            self.get_type(id).as_tuple()
        else {
            return self.add_error(
                name_span,
                SemanticAnalysisError::NotAModule(name.to_owned()),
            );
        };

        let is_visible = self.is_item_visible(visibility, module_path);

        if !is_visible {
            return self.add_error(
                name_span,
                SemanticAnalysisError::TypeNotPublic(name.to_owned()),
            );
        }

        let (_, _, HighTypeDeclarationKind::Module(declaration)) = self.get_type(id).as_tuple()
        else {
            unsafe { unreachable_unchecked() }
        };

        Some(declaration)
    }

    #[inline]
    #[must_use]
    pub fn get_visible_struct_struct(
        &mut self,
        name_span: Span,
        name: &str,
        id: StructId,
    ) -> Option<(StructStructId, &[String], &StructStructDeclaration)> {
        let (visibility, module_path, StructDeclaration::Struct(_)) =
            self.environment.get_struct(id)
        else {
            return self.add_error(
                name_span,
                SemanticAnalysisError::NotARegularStruct(name.to_owned()),
            );
        };

        let is_visible = self.is_item_visible(visibility, module_path);

        if !is_visible {
            return self.add_error(
                name_span,
                SemanticAnalysisError::TypeNotPublic(name.to_owned()),
            );
        }

        let (_, module_path, StructDeclaration::Struct(declaration)) =
            self.environment.get_struct(id)
        else {
            unsafe { unreachable_unchecked() }
        };

        Some((StructStructId(id.0), module_path, declaration))
    }

    #[inline]
    #[must_use]
    pub fn get_visible_tuple_struct(
        &mut self,
        name_span: Span,
        name: &str,
        id: StructId,
    ) -> Option<(TupleStructId, &[String], &TupleStructDeclaration)> {
        let (visibility, module_path, StructDeclaration::Tuple(_)) =
            self.environment.get_struct(id)
        else {
            return self.add_error(
                name_span,
                SemanticAnalysisError::NotATupleStruct(name.to_owned()),
            );
        };

        let is_visible = self.is_item_visible(visibility, module_path);

        if !is_visible {
            return self.add_error(
                name_span,
                SemanticAnalysisError::TypeNotPublic(name.to_owned()),
            );
        }

        let (_, module_path, StructDeclaration::Tuple(declaration)) =
            self.environment.get_struct(id)
        else {
            unsafe { unreachable_unchecked() }
        };

        Some((TupleStructId(id.0), module_path, declaration))
    }
}
