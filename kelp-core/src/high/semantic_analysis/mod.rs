use std::collections::HashSet;

use hashbrown::HashMap;
use smallvec::SmallVec;
use strum::IntoEnumIterator;

use crate::{
    high::{
        environment::{
            HighEnvironment,
            r#type::{
                HighTypeDeclaration, HighTypeDeclarationKind, HighTypeId,
                alias::HighAliasDeclaration,
                builtin_data_type::{BuiltinTypeKind, HighBuiltinTypeDeclaration},
                module::HighModuleDeclaration,
                r#struct::{
                    HighStructDeclaration, HighStructId,
                    regular::{HighRegularStructDeclaration, HighRegularStructId},
                    tuple::{HighTupleStructDeclaration, HighTupleStructId},
                },
            },
            value::{
                HighValueDeclaration, HighValueDeclarationKind, HighValueId,
                function::{
                    HighFunctionDeclaration, HighFunctionId,
                    builtin::{
                        BuiltinFunctionKind, HighBuiltinFunctionDeclaration, HighBuiltinFunctionId,
                    },
                    regular::{HighRegularFunctionDeclaration, HighRegularFunctionId},
                },
                variable::HighVariableId,
            },
        },
        semantic_analysis::{
            info::{SemanticAnalysisInfo, SemanticAnalysisInfoKind, error::SemanticAnalysisError},
            scope::Scope,
        },
    },
    low::{
        data_type::unresolved::UnresolvedDataType,
        environment::{Environment, value::variable::VariableId},
        pattern::UnresolvedPattern,
    },
    path::{generic::GenericPath, regular::Path},
    span::Span,
    visibility::Visibility,
};

pub mod info;
pub mod scope;

#[derive(Debug, Clone, Copy)]
pub enum RegularFunctionModifiers {
    None,
    Runtime { recursive: bool },
}

impl RegularFunctionModifiers {
    #[must_use]
    pub const fn is_recursive(self) -> bool {
        match self {
            Self::None => false,
            Self::Runtime { recursive } => recursive,
        }
    }

    #[must_use]
    pub const fn is_runtime(self) -> bool {
        matches!(self, Self::Runtime { .. })
    }
}

#[derive(Debug, Clone)]
pub enum FunctionContext {
    Regular {
        modifiers: RegularFunctionModifiers,
        return_type: UnresolvedDataType,
        calls: HashSet<HighFunctionId>,
    },
    MCFunction,
}

impl FunctionContext {
    #[must_use]
    pub fn calls(&self, id: &HighFunctionId) -> bool {
        match self {
            Self::Regular { calls, .. } => calls.contains(id),
            Self::MCFunction => false,
        }
    }

    #[must_use]
    pub const fn get_calls(&self) -> Option<&HashSet<HighFunctionId>> {
        match self {
            Self::Regular { calls, .. } => Some(calls),
            Self::MCFunction => None,
        }
    }

    pub fn add_call(&mut self, id: HighFunctionId) {
        match self {
            Self::Regular { calls, .. } => {
                calls.insert(id);
            }
            Self::MCFunction => {}
        }
    }

    #[must_use]
    pub const fn is_recursive(&self) -> Option<bool> {
        match self {
            Self::Regular { modifiers, .. } => Some(modifiers.is_recursive()),
            Self::MCFunction { .. } => None,
        }
    }

    #[must_use]
    pub const fn is_runtime(&self) -> Option<bool> {
        match self {
            Self::Regular { modifiers, .. } => Some(modifiers.is_runtime()),
            Self::MCFunction { .. } => None,
        }
    }

    #[must_use]
    pub const fn return_type(&self) -> &UnresolvedDataType {
        match self {
            Self::Regular { return_type, .. } => return_type,
            Self::MCFunction => &UnresolvedDataType::Integer,
        }
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
    pub current_module_path: Vec<String>,
    pub function_contexts: SmallVec<[FunctionContext; 5]>,
    max_infos: usize,
    pub environment: Environment,
    pub high_environment: HighEnvironment,
    resolved_variables: HashMap<HighValueId, VariableId>,
}

impl SemanticAnalysisContext {
    #[must_use]
    pub fn new(id: &str, max_infos: usize) -> Self {
        let mut self_ = Self {
            infos: Vec::new(),
            max_infos,
            environment: Environment::default(),
            high_environment: HighEnvironment::default(),
            scopes: vec![Scope::default()],
            resolved_variables: HashMap::new(),
            loop_depth: 0,
            current_module_path: Vec::new(),
            function_contexts: SmallVec::new(),
        };

        self_.declare_std_module();

        self_.enter_module(id.to_owned());

        self_
    }

    pub fn declare_std_module(&mut self) {
        self.enter_module("std".to_owned());

        for builtin_type in BuiltinTypeKind::iter() {
            self.declare_builtin_type(builtin_type.declaration());
        }

        for builtin_function in BuiltinFunctionKind::iter() {
            self.declare_builtin_function(Visibility::Public, builtin_function.declaration());
        }

        self.exit_module_and_declare(Visibility::Public);
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
    pub fn is_item_visible(&self, target_module_path: &[String], visibility: Visibility) -> bool {
        if matches!(visibility, Visibility::Public) {
            return true;
        }

        self.current_module_path.starts_with(target_module_path)
    }

    #[inline]
    pub fn add_invalid_generics<T>(
        &mut self,
        span: Span,
        type_name: &str,
        expected: usize,
        actual: usize,
    ) -> Option<T> {
        self.add_error(
            span,
            SemanticAnalysisError::InvalidGenerics {
                type_name: type_name.to_owned(),
                expected,
                actual,
            },
        )
    }

    #[must_use]
    pub fn add_invalid_generics_type(
        &mut self,
        span: Span,
        type_name: &str,
        expected: usize,
        actual: usize,
    ) -> UnresolvedDataType {
        self.add_invalid_generics::<()>(span, type_name, expected, actual);

        UnresolvedDataType::Error
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

    pub fn add_error_unit(&mut self, span: Span, error: SemanticAnalysisError) -> Option<()> {
        self.add_info(SemanticAnalysisInfo {
            span,
            kind: SemanticAnalysisInfoKind::Error(error),
        })
    }

    pub fn add_error_type(
        &mut self,
        span: Span,
        error: SemanticAnalysisError,
    ) -> UnresolvedDataType {
        self.add_error_unit(span, error);

        UnresolvedDataType::Error
    }

    #[must_use]
    pub fn declare_variable(
        &mut self,
        visibility: Visibility,
        name: String,
        data_type: UnresolvedDataType,
    ) -> HighVariableId {
        let id = self.high_environment.declare_variable(
            visibility,
            self.current_module_path.clone(),
            name.clone(),
            data_type,
        );

        self.scopes
            .last_mut()
            .expect("No scopes")
            .declare_value(name, id.into());

        id
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
            self.add_error_unit(name_span, SemanticAnalysisError::TypeAlreadyDeclared(name));

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
            self.add_error_unit(
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
    ) -> HighStructId {
        let id = self.declare_type(visibility, HighTypeDeclarationKind::Struct(declaration));

        HighStructId(id.0)
    }

    #[inline]
    pub fn declare_regular_struct(
        &mut self,
        visibility: Visibility,
        declaration: HighRegularStructDeclaration,
    ) -> HighRegularStructId {
        let id = self.declare_struct(visibility, HighStructDeclaration::Struct(declaration));

        HighRegularStructId(id.0)
    }

    #[inline]
    pub fn declare_tuple_struct(
        &mut self,
        visibility: Visibility,
        declaration: HighTupleStructDeclaration,
    ) -> HighTupleStructId {
        let id = self.declare_struct(visibility, HighStructDeclaration::Tuple(declaration));

        HighTupleStructId(id.0)
    }

    #[inline]
    pub fn declare_builtin_type(&mut self, data_type: HighBuiltinTypeDeclaration) -> HighTypeId {
        self.declare_type(
            Visibility::Public,
            HighTypeDeclarationKind::Builtin(data_type),
        )
    }

    pub fn declare_type(
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

        let id = self.high_environment.declare_value(
            visibility,
            self.current_module_path.clone(),
            declaration,
        );

        self.current_scope_mut().declare_value(name, id);

        id
    }

    #[allow(clippy::too_many_arguments)]
    pub fn declare_regular_function(
        &mut self,
        visibility: Visibility,
        modifiers: RegularFunctionModifiers,
        name: String,
        generic_names: Vec<String>,
        parameters: Vec<(Option<UnresolvedPattern>, UnresolvedDataType)>,
        return_type: UnresolvedDataType,
    ) -> HighRegularFunctionId {
        let id = self.declare_value(
            visibility,
            HighValueDeclarationKind::Function(Box::new(HighFunctionDeclaration::Regular(
                HighRegularFunctionDeclaration {
                    name,
                    modifiers,
                    generic_names,
                    parameters,
                    return_type,
                    body: None,
                    calls: None,
                },
            ))),
        );

        HighRegularFunctionId(id.0)
    }

    pub fn declare_builtin_function(
        &mut self,
        visibility: Visibility,
        declaration: HighBuiltinFunctionDeclaration,
    ) -> HighBuiltinFunctionId {
        let id = self.declare_value(
            visibility,
            HighValueDeclarationKind::Function(Box::new(HighFunctionDeclaration::Builtin(
                declaration,
            ))),
        );

        HighBuiltinFunctionId(id.0)
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

        let HighTypeDeclaration {
            visibility,
            module_path,
            ..
        } = self.get_type(current_type_id);

        let is_visible = self.is_item_visible(module_path, *visibility);

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
        let is_visible = self.is_item_visible(&declaration.module_path, declaration.visibility);

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
                let HighTypeDeclaration {
                    visibility,
                    module_path,
                    kind: HighTypeDeclarationKind::Module(declaration),
                } = self.get_type(current_type_id)
                else {
                    found_type = false;

                    break;
                };

                let is_visible = self.is_item_visible(module_path, *visibility);

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
                let HighTypeDeclaration {
                    visibility,
                    module_path,
                    ..
                } = self.get_type(current_type_id);

                let is_visible = self.is_item_visible(module_path, *visibility);

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
        let is_visible = self.is_item_visible(&declaration.module_path, declaration.visibility);

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
        &self.high_environment.types[id.0 as usize]
    }

    #[must_use]
    pub fn get_value(&self, id: HighValueId) -> &HighValueDeclaration {
        &self.high_environment.values[id.0 as usize]
    }

    #[must_use]
    pub fn get_visible_module(
        &mut self,
        name_span: Span,
        name: &str,
        id: HighTypeId,
    ) -> Option<&HighModuleDeclaration> {
        let HighTypeDeclaration {
            visibility,
            module_path,
            kind: HighTypeDeclarationKind::Module(..),
        } = self.get_type(id)
        else {
            return self.add_error(
                name_span,
                SemanticAnalysisError::NotAModule(name.to_owned()),
            );
        };

        let is_visible = self.is_item_visible(module_path, *visibility);

        if !is_visible {
            return self.add_error(
                name_span,
                SemanticAnalysisError::TypeNotPublic(name.to_owned()),
            );
        }

        let HighTypeDeclarationKind::Module(declaration) = &self.get_type(id).kind else {
            unreachable!();
        };

        Some(declaration)
    }

    #[must_use]
    pub fn get_visible_regular_struct(
        &mut self,
        name_span: Span,
        name: &str,
        id: HighTypeId,
    ) -> Option<(&[String], Visibility, &HighRegularStructDeclaration)> {
        let (module_path, visibility, declaration) = self.high_environment.get_type(id);

        let is_visible = self.is_item_visible(module_path, visibility);

        if !is_visible {
            return self.add_error(
                name_span,
                SemanticAnalysisError::TypeNotPublic(name.to_owned()),
            );
        }

        let HighTypeDeclarationKind::Struct(declaration) = declaration else {
            return self.add_error(
                name_span,
                SemanticAnalysisError::NotAStruct(name.to_owned()),
            );
        };

        let HighStructDeclaration::Struct(..) = declaration else {
            return self.add_error(
                name_span,
                SemanticAnalysisError::NotARegularStruct(name.to_owned()),
            );
        };

        let (
            module_path,
            visibility,
            HighTypeDeclarationKind::Struct(HighStructDeclaration::Struct(declaration)),
        ) = self.high_environment.get_type(id)
        else {
            unreachable!();
        };

        Some((module_path, visibility, declaration))
    }

    #[must_use]
    pub fn get_visible_tuple_struct(
        &mut self,
        name_span: Span,
        name: &str,
        id: HighTypeId,
    ) -> Option<(&[String], Visibility, &HighTupleStructDeclaration)> {
        let (module_path, visibility, declaration) = self.high_environment.get_type(id);

        let is_visible = self.is_item_visible(module_path, visibility);

        if !is_visible {
            return self.add_error(
                name_span,
                SemanticAnalysisError::TypeNotPublic(name.to_owned()),
            );
        }

        let HighTypeDeclarationKind::Struct(declaration) = declaration else {
            return self.add_error(
                name_span,
                SemanticAnalysisError::NotAStruct(name.to_owned()),
            );
        };

        let HighStructDeclaration::Tuple(..) = declaration else {
            return self.add_error(
                name_span,
                SemanticAnalysisError::NotATupleStruct(name.to_owned()),
            );
        };

        let (
            module_path,
            visibility,
            HighTypeDeclarationKind::Struct(HighStructDeclaration::Tuple(declaration)),
        ) = self.high_environment.get_type(id)
        else {
            unreachable!();
        };

        Some((module_path, visibility, declaration))
    }
}
