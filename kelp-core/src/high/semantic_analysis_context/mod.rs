use hashbrown::{Equivalent, HashMap};
use std::collections::HashMap as StdHashMap;

use crate::{
    builtin_data_type::BuiltinDataType,
    high::{
        environment::{
            HighEnvironment,
            r#type::{HighTypeDeclaration, HighTypeId, module::HighModuleDeclaration},
        },
        semantic_analysis_context::{
            info::{SemanticAnalysisInfo, SemanticAnalysisInfoKind, error::SemanticAnalysisError},
            scope::Scope,
        },
    },
    middle::{
        data_type::DataType,
        environment::{
            Environment,
            r#type::r#struct::{StructDeclaration, StructId},
            value::{ValueDeclaration, ValueId, variable::VariableId},
        },
    },
    path::{generic::GenericPath, regular::Path},
    span::Span,
};

pub mod info;
pub mod scope;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct MonomorphizedStructKey {
    pub id: HighTypeId,
    pub generics: Vec<DataType>,
}

#[derive(Hash, PartialEq, Eq)]
struct MonomorphizedStructKeyRef<'a> {
    pub id: HighTypeId,
    pub generics: &'a [DataType],
}

impl Equivalent<MonomorphizedStructKey> for MonomorphizedStructKeyRef<'_> {
    fn equivalent(&self, key: &MonomorphizedStructKey) -> bool {
        self.id == key.id && self.generics == key.generics.as_slice()
    }
}

pub enum ResolvedItem {
    Type(HighTypeId),
    Value(ValueId),
}

#[derive(Debug, Clone)]
pub struct SemanticAnalysisContext {
    pub infos: Vec<SemanticAnalysisInfo>,
    pub environment: Environment,
    pub scopes: Vec<Scope>,
    pub loop_depth: u32,
    pub is_lhs: bool,
    max_infos: usize,
    high_environment: HighEnvironment,
    monomorphized_structs: HashMap<MonomorphizedStructKey, StructId>,
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
            loop_depth: 0,
            is_lhs: false,
        };

        let builtins = [
            ("boolean", BuiltinDataType::Boolean),
            ("byte", BuiltinDataType::Byte),
            ("short", BuiltinDataType::Short),
            ("integer", BuiltinDataType::Integer),
            ("long", BuiltinDataType::Long),
            ("float", BuiltinDataType::Float),
            ("double", BuiltinDataType::Double),
            ("string", BuiltinDataType::String),
            // ("unit", BuiltinDataType::Unit),
            ("score", BuiltinDataType::Score),
            ("list", BuiltinDataType::List),
            ("compound", BuiltinDataType::Compound),
            ("data", BuiltinDataType::Data),
            ("snbt", BuiltinDataType::SNBT),
        ];

        let mut std_types = StdHashMap::new();
        for (name, builtin) in builtins {
            let id = self_.high_environment.declare_builtin_type(builtin);
            std_types.insert(name.to_owned(), id);
        }

        let standard_module = HighModuleDeclaration {
            name: "std".to_string(),
            types: std_types,
            values: StdHashMap::new(),
        };

        self_.declare_data_type(HighTypeDeclaration::Module(standard_module));

        self_.start_scope();

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
    pub fn start_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    #[inline]
    pub fn end_scope(&mut self) -> Scope {
        self.scopes.pop().unwrap()
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
        name: String,
        generic_types: Vec<DataType>,
        field_types: StdHashMap<String, DataType>,
    ) -> StructId {
        let id = self
            .environment
            .declare_struct(name, generic_types.clone(), field_types);

        let key = MonomorphizedStructKey {
            id: original_id,
            generics: generic_types,
        };

        self.monomorphized_structs.insert(key, id);

        id
    }

    pub fn declare_value(&mut self, declaration: ValueDeclaration) -> ValueId {
        let name = declaration.name().to_owned();

        let id = self.environment.declare_value(declaration);

        self.scopes
            .last_mut()
            .expect("No scopes")
            .declare_value(name, id);

        id
    }

    #[must_use]
    pub fn declare_variable(&mut self, name: String, data_type: Option<DataType>) -> VariableId {
        let id = self.environment.declare_variable(name.clone(), data_type);

        self.scopes
            .last_mut()
            .expect("No scopes")
            .declare_value(name, ValueId(id.0));

        id
    }

    #[inline]
    #[must_use]
    pub fn declare_variable_known(&mut self, name: String, data_type: DataType) -> VariableId {
        self.declare_variable(name, Some(data_type))
    }

    #[inline]
    #[must_use]
    pub fn declare_variable_unknown(&mut self, name: String) -> VariableId {
        self.declare_variable(name, None)
    }

    #[must_use]
    pub fn get_value_id(&self, name: &str) -> Option<ValueId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.values.get(name))
            .copied()
    }

    #[must_use]
    pub fn get_value(&self, id: ValueId) -> &ValueDeclaration {
        &self.environment.values[id.0]
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

    pub fn declare_value_in_current_scope(&mut self, name_span: Span, name: String, id: ValueId) {
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

    pub fn declare_value_if_not_defined(&mut self, name: String, id: ValueId) {
        let scope = self.current_scope_mut();

        if scope.values.contains_key(&name) {
            return;
        }

        scope.values.insert(name, id);
    }

    pub fn declare_data_type(&mut self, declaration: HighTypeDeclaration) -> HighTypeId {
        let name = declaration.name().to_owned();

        let id = self.high_environment.declare_type(declaration);

        self.scopes
            .last_mut()
            .expect("No scopes")
            .declare_type(name, id);

        id
    }

    #[must_use]
    pub fn resolve_type_generic_path<T>(&mut self, path: &GenericPath<T>) -> Option<HighTypeId> {
        let mut segments = path.segments.iter();
        let mut current_segment = segments.next()?;

        let Some(mut current_type_id) = self.get_type_id(&current_segment.name) else {
            return self.add_error(
                current_segment.name_span,
                SemanticAnalysisError::UnknownType(current_segment.name.clone()),
            );
        };

        for next_segment in segments {
            let HighTypeDeclaration::Module(declaration) = self.get_type(current_type_id) else {
                return self.add_error(
                    current_segment.name_span,
                    SemanticAnalysisError::NotAModule(current_segment.name.clone()),
                );
            };

            let Some(next_type_id) = declaration.get_type_id(&next_segment.name) else {
                return self.add_error(
                    next_segment.name_span,
                    SemanticAnalysisError::ModuleDoesntContainType {
                        module_name: declaration.name.clone(),
                        type_name: next_segment.name.clone(),
                    },
                );
            };

            current_type_id = next_type_id;
            current_segment = next_segment;
        }

        Some(current_type_id)
    }

    #[must_use]
    pub fn resolve_value_generic_path<T>(&mut self, path: &GenericPath<T>) -> Option<ValueId> {
        let (first_segment, segments) = path.segments.split_first()?;

        if segments.is_empty() {
            let Some(resolved_value_id) = self.get_value_id(&first_segment.name) else {
                return self.add_error(
                    first_segment.name_span,
                    SemanticAnalysisError::UnknownValue(first_segment.name.clone()),
                );
            };

            return Some(resolved_value_id);
        }

        let (last_segment, segments) = segments.split_last().unwrap();

        let Some(mut current_type_id) = self.get_type_id(&first_segment.name) else {
            return self.add_error(
                first_segment.name_span,
                SemanticAnalysisError::UnknownModule(first_segment.name.clone()),
            );
        };

        let mut previous_segment = first_segment;

        for segment in segments {
            let HighTypeDeclaration::Module(declaration) = self.get_type(current_type_id) else {
                return self.add_error(
                    previous_segment.name_span,
                    SemanticAnalysisError::NotAModule(previous_segment.name.clone()),
                );
            };

            let Some(next_type_id) = declaration.get_type_id(&segment.name) else {
                return self.add_error(
                    segment.name_span,
                    SemanticAnalysisError::ModuleDoesntContainType {
                        module_name: declaration.name.clone(),
                        type_name: segment.name.clone(),
                    },
                );
            };

            current_type_id = next_type_id;
            previous_segment = segment;
        }

        let HighTypeDeclaration::Module(declaration) = self.get_type(current_type_id) else {
            return self.add_error(
                previous_segment.name_span,
                SemanticAnalysisError::NotAModule(previous_segment.name.clone()),
            );
        };

        let Some(resolved_value_id) = declaration.get_value_id(&last_segment.name) else {
            return self.add_error(
                last_segment.name_span,
                SemanticAnalysisError::ModuleDoesntContainValue {
                    module_name: declaration.name.clone(),
                    value_name: last_segment.name.clone(),
                },
            );
        };

        Some(resolved_value_id)
    }

    #[must_use]
    pub fn resolve_item_path(&mut self, path: &Path) -> Option<ResolvedItem> {
        let mut type_segments = path.segments.iter();

        if let Some(first_segment) = type_segments.next()
            && let Some(mut current_type_id) = self.get_type_id(&first_segment.name)
        {
            let mut found_type = true;

            for next_segment in type_segments {
                let HighTypeDeclaration::Module(declaration) = self.get_type(current_type_id)
                else {
                    found_type = false;
                    break;
                };

                let Some(next_type_id) = declaration.get_type_id(&next_segment.name) else {
                    found_type = false;

                    break;
                };

                current_type_id = next_type_id;
            }

            if found_type {
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
            let HighTypeDeclaration::Module(declaration) = self.get_type(current_type_id) else {
                return self.add_error(
                    previous_segment.span,
                    SemanticAnalysisError::NotAModule(previous_segment.name.clone()),
                );
            };

            let Some(next_type_id) = declaration.get_type_id(&segment.name) else {
                return self.add_error(
                    segment.span,
                    SemanticAnalysisError::ModuleDoesntContainType {
                        module_name: declaration.name.clone(),
                        type_name: segment.name.clone(),
                    },
                );
            };

            current_type_id = next_type_id;
            previous_segment = segment;
        }

        let HighTypeDeclaration::Module(declaration) = self.get_type(current_type_id) else {
            return self.add_error(
                previous_segment.span,
                SemanticAnalysisError::NotAModule(previous_segment.name.clone()),
            );
        };

        let Some(resolved_value_id) = declaration.get_value_id(&last_segment.name) else {
            return self.add_error(
                last_segment.span,
                SemanticAnalysisError::ModuleDoesntContainItem {
                    module_name: declaration.name.clone(),
                    item_name: last_segment.name.clone(),
                },
            );
        };

        Some(ResolvedItem::Value(resolved_value_id))
    }

    #[must_use]
    pub fn get_type_id(&self, name: &str) -> Option<HighTypeId> {
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

    #[inline]
    #[must_use]
    pub fn get_struct_type(&self, id: StructId) -> &StructDeclaration {
        self.environment.get_struct(id)
    }
}
