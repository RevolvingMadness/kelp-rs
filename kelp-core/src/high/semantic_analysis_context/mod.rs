use hashbrown::{Equivalent, HashMap};
use std::collections::HashMap as StdHashMap;

use crate::{
    builtin_data_type::BuiltinDataType,
    high::{
        environment::{
            HighEnvironment,
            r#type::{HighTypeDeclaration, HighTypeId},
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

#[derive(Debug, Clone)]
pub struct SemanticAnalysisContext {
    pub infos: Vec<SemanticAnalysisInfo>,
    pub environment: Environment,
    pub loop_depth: u32,
    pub is_lhs: bool,
    max_infos: usize,
    scopes: Vec<Scope>,
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

        self_.declare_builtin_type(BuiltinDataType::Boolean);
        self_.declare_builtin_type(BuiltinDataType::Byte);
        self_.declare_builtin_type(BuiltinDataType::Short);
        self_.declare_builtin_type(BuiltinDataType::Integer);
        self_.declare_builtin_type(BuiltinDataType::Long);
        self_.declare_builtin_type(BuiltinDataType::Float);
        self_.declare_builtin_type(BuiltinDataType::Double);
        self_.declare_builtin_type(BuiltinDataType::String);
        // self_.declare_builtin_type(BuiltinDataType::Unit);
        self_.declare_builtin_type(BuiltinDataType::Score);
        self_.declare_builtin_type(BuiltinDataType::List);
        self_.declare_builtin_type(BuiltinDataType::Compound);
        self_.declare_builtin_type(BuiltinDataType::Data);
        self_.declare_builtin_type(BuiltinDataType::SNBT);

        self_.scopes.push(Scope::default());

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
    pub fn end_scope(&mut self) {
        self.scopes.pop();
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

    pub fn declare_data_type(&mut self, declaration: HighTypeDeclaration) -> HighTypeId {
        let name = declaration.name().to_owned();

        let id = self.high_environment.declare_type(declaration);

        self.scopes
            .last_mut()
            .expect("No scopes")
            .declare_type(name, id);

        id
    }

    #[inline]
    pub fn declare_builtin_type(&mut self, builtin_data_type: BuiltinDataType) -> HighTypeId {
        self.declare_data_type(HighTypeDeclaration::Builtin(builtin_data_type))
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
    pub fn get_data_type_id_semantic_analysis(
        &mut self,
        name_span: Span,
        name: &str,
    ) -> Option<HighTypeId> {
        let Some(id) = self.get_type_id(name) else {
            return self.add_error(
                name_span,
                SemanticAnalysisError::UnknownType(name.to_owned()),
            );
        };

        Some(id)
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
