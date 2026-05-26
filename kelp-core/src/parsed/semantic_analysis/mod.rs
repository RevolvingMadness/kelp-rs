use std::{collections::HashSet, fmt::Debug};

use la_arena::{ArenaMap, Idx};
use smallvec::SmallVec;
use strum::IntoEnumIterator;

use crate::{
    parsed::{
        environment::{
            resolved::{
                ResolvedEnvironment,
                r#type::{
                    HighGenericId, HighTypeId, ResolvedTypeDeclaration,
                    ResolvedTypeDeclarationKind,
                    alias::ResolvedTypeAliasDeclaration,
                    builtin_data_type::{BuiltinTypeKind, ResolvedBuiltinTypeDeclaration},
                    module::ResolvedModuleDeclaration,
                    r#struct::{
                        ResolvedStructDeclaration, regular::ResolvedRegularStructDeclaration,
                        tuple::ResolvedTupleStructDeclaration,
                    },
                },
                value::{
                    HighValueId, ResolvedValueDeclaration, ResolvedValueDeclarationKind,
                    function::{
                        HighFunctionId, ResolvedFunctionDeclaration,
                        builtin::{
                            BuiltinFunctionKind, HighBuiltinFunctionId,
                            ResolvedBuiltinFunctionDeclaration,
                        },
                        regular::ResolvedRegularFunctionDeclaration,
                    },
                    variable::{HighVariableId, ResolvedVariableDeclaration},
                },
            },
            unresolved::{
                UnresolvedEnvironment,
                r#type::{
                    UnresolvedTypeDeclaration, UnresolvedTypeDeclarationKind,
                    module::UnresolvedModuleDeclaration,
                },
                value::{
                    UnresolvedValueDeclaration, UnresolvedValueDeclarationKind,
                    variable::UnresolvedVariableDeclaration,
                },
            },
        },
        item::Item,
        semantic_analysis::{
            info::{SemanticAnalysisInfo, SemanticAnalysisInfoKind, error::SemanticAnalysisError},
            scope::Scope,
        },
    },
    path::{
        generic::{GenericPath, GenericPathSegment},
        regular::{Path, PathSegment},
    },
    span::Span,
    typed::{
        data_type::unresolved::UnresolvedDataType, environment::Environment,
        pattern::TypedPattern,
    },
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
        calls: HashSet<(Span, HighFunctionId)>,
    },
    MCFunction,
}

impl FunctionContext {
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

#[derive(Debug, Clone)]
pub struct SemanticAnalysisContext {
    pub infos: Vec<SemanticAnalysisInfo>,
    pub scopes: Vec<Scope>,
    pub loop_depth: u32,
    pub current_module_path: Vec<String>,
    pub function_contexts: SmallVec<[FunctionContext; 5]>,
    pub max_infos: usize,
    pub environment: Environment,
    pub unresolved_environment: UnresolvedEnvironment,
    pub resolved_environment: ResolvedEnvironment,

    pub item_type_ids: ArenaMap<Idx<Item>, HighTypeId>,
    pub item_value_ids: ArenaMap<Idx<Item>, HighValueId>,
    pub item_generic_ids: ArenaMap<Idx<Item>, Vec<HighGenericId>>,
    pub item_scopes: ArenaMap<Idx<Item>, Scope>,
}

impl SemanticAnalysisContext {
    #[must_use]
    pub fn new(id: &str, max_infos: usize) -> Self {
        let mut self_ = Self {
            infos: Vec::new(),
            max_infos,
            environment: Environment::default(),
            unresolved_environment: UnresolvedEnvironment::default(),
            resolved_environment: ResolvedEnvironment::default(),
            scopes: vec![Scope::default()],
            loop_depth: 0,
            current_module_path: Vec::new(),
            function_contexts: SmallVec::new(),

            item_type_ids: ArenaMap::default(),
            item_value_ids: ArenaMap::default(),
            item_generic_ids: ArenaMap::default(),
            item_scopes: ArenaMap::default(),
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
    pub fn push_scope(&mut self, scope: Scope) {
        self.scopes.push(scope);
    }

    #[inline]
    pub fn enter_module(&mut self, name: String) {
        self.current_module_path.push(name);

        self.enter_scope();
    }

    #[inline]
    pub fn exit_module(&mut self) -> UnresolvedModuleDeclaration {
        let name = self.current_module_path.pop().unwrap();

        let scope = self.exit_scope();

        scope.into_module_declaration(name)
    }

    pub fn exit_module_and_declare(&mut self, visibility: Visibility) -> HighTypeId {
        let module = self.exit_module();

        self.declare_module(visibility, module)
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

    pub fn add_info_static<T>(
        infos: &mut Vec<SemanticAnalysisInfo>,
        max_infos: usize,
        info: SemanticAnalysisInfo,
    ) -> Option<T> {
        if infos.len() >= max_infos {
            return None;
        }

        infos.push(info);

        None
    }

    #[inline]
    #[must_use]
    pub fn add_error<T>(&mut self, span: Span, error: SemanticAnalysisError) -> Option<T> {
        self.add_info(SemanticAnalysisInfo {
            span,
            kind: SemanticAnalysisInfoKind::Error(error),
        })
    }

    #[inline]
    #[must_use]
    pub fn add_error_static<T>(
        infos: &mut Vec<SemanticAnalysisInfo>,
        max_infos: usize,
        span: Span,
        error: SemanticAnalysisError,
    ) -> Option<T> {
        Self::add_info_static(
            infos,
            max_infos,
            SemanticAnalysisInfo {
                span,
                kind: SemanticAnalysisInfoKind::Error(error),
            },
        )
    }

    #[inline]
    pub fn add_error_unit(&mut self, span: Span, error: SemanticAnalysisError) -> Option<()> {
        self.add_error(span, error)
    }

    #[inline]
    pub fn add_error_unit_static(
        infos: &mut Vec<SemanticAnalysisInfo>,
        max_infos: usize,
        span: Span,
        error: SemanticAnalysisError,
    ) -> Option<()> {
        Self::add_error_static(infos, max_infos, span, error)
    }

    #[inline]
    #[must_use]
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
        let id = self.declare_unresolved_value(
            visibility,
            UnresolvedValueDeclarationKind::Variable(UnresolvedVariableDeclaration {
                name: name.clone(),
            }),
        );

        self.declare_resolved_value(
            id,
            visibility,
            ResolvedValueDeclarationKind::Variable(ResolvedVariableDeclaration { name, data_type }),
        );

        HighVariableId(id.0)
    }

    #[must_use]
    pub fn get_type_id(&self, name: &str) -> Option<HighTypeId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get_type_id(name))
    }

    #[must_use]
    pub fn get_value_id(&self, name: &str) -> Option<HighValueId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get_value_id(name))
    }

    #[inline]
    #[must_use]
    pub fn current_scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    #[inline]
    #[must_use]
    pub fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    pub fn declare_type_in_current_scope(&mut self, name_span: Span, name: String, id: HighTypeId) {
        let scope = self.current_scope_mut();

        if scope.type_is_declared(&name) {
            self.add_error_unit(name_span, SemanticAnalysisError::TypeAlreadyDeclared(name));

            return;
        }

        self.current_scope_mut().declare_type(name, id);
    }

    pub fn declare_value_in_current_scope(
        &mut self,
        name_span: Span,
        name: String,
        id: HighValueId,
    ) {
        let scope = self.current_scope_mut();

        if scope.value_is_declared(&name) {
            self.add_error_unit(name_span, SemanticAnalysisError::ValueAlreadyDeclared(name));

            return;
        }

        self.current_scope_mut().declare_value(name, id);
    }

    pub fn declare_type_if_not_defined(&mut self, name: String, id: HighTypeId) {
        let scope = self.current_scope_mut();

        if scope.type_is_declared(&name) {
            return;
        }

        self.current_scope_mut().declare_type(name, id);
    }

    pub fn declare_value_if_not_defined(&mut self, name: String, id: HighValueId) {
        let scope = self.current_scope_mut();

        if scope.value_is_declared(&name) {
            return;
        }

        self.current_scope_mut().declare_value(name, id);
    }

    #[inline]
    pub fn declare_module(
        &mut self,
        visibility: Visibility,
        declaration: UnresolvedModuleDeclaration,
    ) -> HighTypeId {
        self.declare_unresolved_type(
            visibility,
            UnresolvedTypeDeclarationKind::Module(declaration),
        )
    }

    #[inline]
    pub fn declare_module_auto(
        &mut self,
        visibility: Visibility,
        declaration: ResolvedModuleDeclaration,
    ) {
        self.declare_type_auto(visibility, ResolvedTypeDeclarationKind::Module(declaration));
    }

    #[inline]
    pub fn declare_alias(
        &mut self,
        id: HighTypeId,
        visibility: Visibility,
        declaration: ResolvedTypeAliasDeclaration,
    ) {
        self.declare_resolved_type(
            id,
            visibility,
            ResolvedTypeDeclarationKind::Alias(declaration),
        );
    }

    #[inline]
    pub fn declare_generic(&mut self, id: HighGenericId, visibility: Visibility, name: String) {
        self.declare_resolved_type(
            id.into(),
            visibility,
            ResolvedTypeDeclarationKind::Generic(name),
        );
    }

    pub fn declare_builtin_type(&mut self, data_type: ResolvedBuiltinTypeDeclaration) {
        self.declare_type_auto(
            Visibility::Public,
            ResolvedTypeDeclarationKind::Builtin(data_type),
        );
    }

    #[inline]
    pub fn declare_unresolved_type(
        &mut self,
        visibility: Visibility,
        kind: UnresolvedTypeDeclarationKind,
    ) -> HighTypeId {
        let name = kind.name().to_owned();

        let id = self
            .unresolved_environment
            .declare_type(UnresolvedTypeDeclaration {
                module_path: self.current_module_path.clone(),
                visibility,
                kind,
            });

        self.current_scope_mut().declare_type(name, id);

        id
    }

    pub fn declare_unresolved_value(
        &mut self,
        visibility: Visibility,
        kind: UnresolvedValueDeclarationKind,
    ) -> HighValueId {
        let name = kind.name().to_owned();

        let id = self
            .unresolved_environment
            .declare_value(UnresolvedValueDeclaration {
                visibility,
                module_path: self.current_module_path.clone(),
                kind,
            });

        self.current_scope_mut().declare_value(name, id);

        id
    }

    pub fn declare_type_auto(
        &mut self,
        visibility: Visibility,
        declaration: ResolvedTypeDeclarationKind,
    ) -> HighTypeId {
        let id = self.declare_unresolved_type(visibility, declaration.clone().into());

        self.current_scope_mut()
            .declare_type(declaration.name().to_owned(), id);

        self.declare_resolved_type(id, visibility, declaration);

        id
    }

    #[inline]
    pub fn declare_resolved_type(
        &mut self,
        id: HighTypeId,
        visibility: Visibility,
        declaration: ResolvedTypeDeclarationKind,
    ) {
        let name = declaration.name().to_owned();

        self.resolved_environment.declare_type(
            id,
            ResolvedTypeDeclaration {
                visibility,
                module_path: self.current_module_path.clone(),
                kind: declaration,
            },
        );

        self.current_scope_mut().declare_type(name, id);
    }

    #[inline]
    pub fn declare_resolved_value(
        &mut self,
        id: HighValueId,
        visibility: Visibility,
        kind: ResolvedValueDeclarationKind,
    ) {
        self.resolved_environment.declare_value(
            id,
            ResolvedValueDeclaration {
                visibility,
                module_path: self.current_module_path.clone(),
                kind,
            },
        );
    }

    pub fn declare_unresolved_and_resolved_value(
        &mut self,
        visibility: Visibility,
        declaration: ResolvedValueDeclarationKind,
    ) -> HighValueId {
        let id = self.declare_unresolved_value(visibility, declaration.clone().into());

        self.declare_resolved_value(id, visibility, declaration);

        id
    }

    #[allow(clippy::too_many_arguments)]
    pub fn declare_regular_function(
        &mut self,
        id: HighValueId,
        visibility: Visibility,
        modifiers: RegularFunctionModifiers,
        name: String,
        generic_ids: Vec<HighGenericId>,
        parameters: Vec<(Option<Idx<TypedPattern>>, UnresolvedDataType)>,
        return_type: UnresolvedDataType,
    ) {
        self.declare_resolved_value(
            id,
            visibility,
            ResolvedValueDeclarationKind::Function(Box::new(ResolvedFunctionDeclaration::Regular(
                ResolvedRegularFunctionDeclaration {
                    name,
                    modifiers,
                    generic_ids,
                    parameters,
                    return_type,
                    body: None,
                    calls: HashSet::new(),
                },
            ))),
        );
    }

    pub fn declare_builtin_function(
        &mut self,
        visibility: Visibility,
        declaration: ResolvedBuiltinFunctionDeclaration,
    ) -> HighBuiltinFunctionId {
        let id = self.declare_unresolved_and_resolved_value(
            visibility,
            ResolvedValueDeclarationKind::Function(Box::new(ResolvedFunctionDeclaration::Builtin(
                declaration,
            ))),
        );

        HighBuiltinFunctionId(id.0)
    }

    fn resolve_type_path<'a, T>(
        &mut self,
        path: &'a GenericPath<T>,
    ) -> Option<(HighTypeId, &'a GenericPathSegment<T>)> {
        let (last_segment, segments) = path.segments.split_last()?;

        if segments.is_empty() {
            return None;
        }

        let (first_segment, segments) = segments.split_first()?;

        let mut current_type_id = self.get_type_id(&first_segment.name)?;

        for segment in segments {
            let declaration = self.get_unresolved_type(current_type_id);

            let id = match declaration.get_visible_type_id(self, current_type_id, &segment.name) {
                Ok(id) => id,
                Err(error) => return self.add_error(segment.name_span, error),
            };

            let declaration = self.get_unresolved_type(id);

            if !declaration.is_visible(&self.current_module_path) {
                self.add_error_unit(
                    segment.name_span,
                    SemanticAnalysisError::TypeNotPublic(segment.name.clone()),
                );
            }

            current_type_id = id;
        }

        Some((current_type_id, last_segment))
    }

    #[must_use]
    pub fn get_visible_type_id<T>(&mut self, path: &GenericPath<T>) -> Option<HighTypeId> {
        if path.segments.len() == 1 {
            return self.get_type_id(&path.segments[0].name);
        }

        let (type_id, last_segment) = self.resolve_type_path(path)?;

        let declaration = self.get_unresolved_type(type_id);

        let id = match declaration.get_visible_type_id(self, type_id, &last_segment.name) {
            Ok(id) => id,
            Err(error) => return self.add_error(last_segment.name_span, error),
        };

        let declaration = self.get_unresolved_type(id);

        if !declaration.is_visible(&self.current_module_path) {
            self.add_error_unit(
                last_segment.name_span,
                SemanticAnalysisError::TypeNotPublic(last_segment.name.clone()),
            );
        }

        Some(id)
    }

    #[must_use]
    pub fn get_visible_value_id<T: Debug>(&mut self, path: &GenericPath<T>) -> Option<HighValueId> {
        if path.segments.len() == 1 {
            let segment = &path.segments[0];

            let id = self.get_value_id(&segment.name);

            if id.is_none() {
                return self.add_error(
                    segment.name_span,
                    SemanticAnalysisError::UnknownValue(segment.name.clone()),
                );
            }

            return id;
        }

        let (type_id, last_segment) = self.resolve_type_path(path)?;

        let declaration = self.get_unresolved_type(type_id);

        let id = match declaration.get_visible_value_id(self, type_id, &last_segment.name) {
            Ok(id) => id,
            Err(error) => return self.add_error(last_segment.name_span, error),
        };

        let declaration = self.unresolved_environment.get_value(id);

        if !declaration.is_visible(&self.current_module_path) {
            self.add_error_unit(
                last_segment.name_span,
                SemanticAnalysisError::ValueNotPublic(last_segment.name.clone()),
            );
        }

        Some(id)
    }

    fn try_resolve_path<'a>(
        &mut self,
        path: &'a Path,
    ) -> Result<(HighTypeId, &'a PathSegment), (Span, SemanticAnalysisError)> {
        let (last_segment, segments) = path.segments.split_last().unwrap();

        assert!(
            !segments.is_empty(),
            "segments.len should be checked before calling try_resolve_path"
        );

        let (first_segment, segments) = segments.split_first().unwrap();

        let Some(mut current_type_id) = self.get_type_id(&first_segment.name) else {
            return Err((
                first_segment.span,
                SemanticAnalysisError::UnknownType(first_segment.name.clone()),
            ));
        };

        for segment in segments {
            let declaration = self.get_unresolved_type(current_type_id);

            let id = declaration
                .get_visible_type_id(self, current_type_id, &segment.name)
                .map_err(|error| (segment.span, error))?;

            let declaration = self.get_unresolved_type(id);

            if !declaration.is_visible(&self.current_module_path) {
                self.add_error_unit(
                    segment.span,
                    SemanticAnalysisError::TypeNotPublic(segment.name.clone()),
                );
            }

            current_type_id = id;
        }

        Ok((current_type_id, last_segment))
    }

    pub fn try_get_visible_type(
        &mut self,
        path: &Path,
    ) -> Result<HighTypeId, (Span, SemanticAnalysisError)> {
        if path.segments.len() == 1 {
            let segment = &path.segments[0];

            return self.get_type_id(&segment.name).ok_or_else(|| {
                (
                    segment.span,
                    SemanticAnalysisError::UnknownType(segment.name.clone()),
                )
            });
        }

        let (type_id, last_segment) = self.try_resolve_path(path)?;

        let declaration = self.get_unresolved_type(type_id);

        let id = declaration
            .get_visible_type_id(self, type_id, &last_segment.name)
            .map_err(|error| (last_segment.span, error))?;

        let declaration = self.get_unresolved_type(id);

        if !declaration.is_visible(&self.current_module_path) {
            return Err((
                last_segment.span,
                SemanticAnalysisError::TypeNotPublic(last_segment.name.clone()),
            ));
        }

        Ok(id)
    }

    pub fn try_get_visible_value(
        &mut self,
        path: &Path,
    ) -> Result<HighValueId, (Span, SemanticAnalysisError)> {
        if path.segments.len() == 1 {
            let segment = &path.segments[0];

            return self.get_value_id(&segment.name).ok_or_else(|| {
                (
                    segment.span,
                    SemanticAnalysisError::UnknownValue(segment.name.clone()),
                )
            });
        }

        let (type_id, last_segment) = self.try_resolve_path(path)?;

        let declaration = self.get_unresolved_type(type_id);

        let id = declaration
            .get_visible_value_id(self, type_id, &last_segment.name)
            .map_err(|error| (last_segment.span, error))?;

        let declaration = self.unresolved_environment.get_value(id);

        if !declaration.is_visible(&self.current_module_path) {
            return Err((
                last_segment.span,
                SemanticAnalysisError::ValueNotPublic(last_segment.name.clone()),
            ));
        }

        Ok(id)
    }

    #[inline]
    #[must_use]
    pub fn get_resolved_type(&self, id: HighTypeId) -> &ResolvedTypeDeclaration {
        self.resolved_environment.get_type(id)
    }

    #[inline]
    #[must_use]
    pub fn get_unresolved_type(&self, id: HighTypeId) -> &UnresolvedTypeDeclaration {
        self.unresolved_environment.get_type(id)
    }

    #[inline]
    #[must_use]
    pub fn get_resolved_value(&self, id: HighValueId) -> &ResolvedValueDeclaration {
        self.resolved_environment.get_value(id)
    }

    #[inline]
    #[must_use]
    pub fn get_unresolved_value(&self, id: HighValueId) -> &UnresolvedValueDeclaration {
        self.unresolved_environment.get_value(id)
    }

    #[must_use]
    pub fn get_visible_regular_struct(
        &mut self,
        name_span: Span,
        name: &str,
        id: HighTypeId,
    ) -> Option<(&[String], Visibility, &ResolvedRegularStructDeclaration)> {
        let ResolvedTypeDeclaration {
            module_path,
            visibility,
            kind: declaration,
        } = self.resolved_environment.get_type(id);

        let is_visible = self.is_item_visible(module_path, *visibility);

        if !is_visible {
            return self.add_error(
                name_span,
                SemanticAnalysisError::TypeNotPublic(name.to_owned()),
            );
        }

        let ResolvedTypeDeclarationKind::Struct(declaration) = declaration else {
            return self.add_error(
                name_span,
                SemanticAnalysisError::NotAStruct(name.to_owned()),
            );
        };

        let ResolvedStructDeclaration::Struct(..) = declaration else {
            return self.add_error(
                name_span,
                SemanticAnalysisError::NotARegularStruct(name.to_owned()),
            );
        };

        let ResolvedTypeDeclaration {
            module_path,
            visibility,
            kind:
                ResolvedTypeDeclarationKind::Struct(ResolvedStructDeclaration::Struct(declaration)),
        } = self.resolved_environment.get_type(id)
        else {
            unreachable!();
        };

        Some((module_path, *visibility, declaration))
    }

    #[must_use]
    pub fn get_visible_tuple_struct(
        &mut self,
        name_span: Span,
        name: &str,
        id: HighTypeId,
    ) -> Option<(&[String], Visibility, &ResolvedTupleStructDeclaration)> {
        let ResolvedTypeDeclaration {
            module_path,
            visibility,
            kind: declaration,
        } = self.resolved_environment.get_type(id);

        let is_visible = self.is_item_visible(module_path, *visibility);

        if !is_visible {
            return self.add_error(
                name_span,
                SemanticAnalysisError::TypeNotPublic(name.to_owned()),
            );
        }

        let ResolvedTypeDeclarationKind::Struct(declaration) = declaration else {
            return self.add_error(
                name_span,
                SemanticAnalysisError::NotAStruct(name.to_owned()),
            );
        };

        let ResolvedStructDeclaration::Tuple(..) = declaration else {
            return self.add_error(
                name_span,
                SemanticAnalysisError::NotATupleStruct(name.to_owned()),
            );
        };

        let ResolvedTypeDeclaration {
            module_path,
            visibility,
            kind: ResolvedTypeDeclarationKind::Struct(ResolvedStructDeclaration::Tuple(declaration)),
        } = self.resolved_environment.get_type(id)
        else {
            unreachable!();
        };

        Some((module_path, *visibility, declaration))
    }
}

impl SemanticAnalysisContext {
    #[inline]
    pub fn declare_item_type_id(&mut self, id: Idx<Item>, type_id: HighTypeId) {
        self.item_type_ids.insert(id, type_id);
    }

    #[inline]
    #[must_use]
    pub fn get_item_type_id(&self, id: Idx<Item>) -> HighTypeId {
        *self.item_type_ids.get(id).unwrap()
    }

    #[inline]
    pub fn declare_item_value_id(&mut self, id: Idx<Item>, value_id: HighValueId) {
        self.item_value_ids.insert(id, value_id);
    }

    #[inline]
    #[must_use]
    pub fn get_item_value_id(&self, id: Idx<Item>) -> HighValueId {
        *self.item_value_ids.get(id).unwrap()
    }

    #[inline]
    pub fn declare_item_generic_ids(&mut self, id: Idx<Item>, ids: Vec<HighGenericId>) {
        self.item_generic_ids.insert(id, ids);
    }

    #[inline]
    #[must_use]
    pub fn get_item_generic_ids(&self, id: Idx<Item>) -> &[HighGenericId] {
        self.item_generic_ids.get(id).unwrap()
    }

    #[inline]
    pub fn declare_item_scope(&mut self, id: Idx<Item>, scope: Scope) {
        self.item_scopes.insert(id, scope);
    }

    #[inline]
    #[must_use]
    pub fn get_item_scope(&self, id: Idx<Item>) -> &Scope {
        self.item_scopes.get(id).unwrap()
    }
}
