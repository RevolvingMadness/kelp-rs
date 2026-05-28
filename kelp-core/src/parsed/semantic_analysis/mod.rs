use std::{collections::HashSet, fmt::Debug};

use smallvec::SmallVec;
use strum::IntoEnumIterator;

use crate::low::environment::Environment;
use crate::parsed::environment::r#type::{
    ParsedTypeDeclaration, ParsedTypeDeclarationKind, module::ParsedModuleDeclaration,
};
use crate::parsed::environment::value::{
    ParsedValueDeclaration, ParsedValueDeclarationKind, variable::ParsedVariableDeclaration,
};
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::module::HighModuleId;
use crate::semantic::environment::{
    SemanticEnvironment,
    r#type::{
        HighGenericId, HighTypeId, SemanticTypeDeclaration, SemanticTypeDeclarationKind,
        builtin_data_type::{BuiltinTypeKind, SemanticBuiltinTypeDeclaration},
        r#struct::{
            SemanticStructDeclaration, regular::SemanticRegularStructDeclaration,
            tuple::SemanticTupleStructDeclaration,
        },
    },
    value::{
        HighValueId, SemanticValueDeclaration, SemanticValueDeclarationKind,
        function::{
            HighFunctionId, SemanticFunctionDeclaration,
            builtin::{
                HighBuiltinFunctionId, SemanticBuiltinFunctionDeclaration,
                SemanticBuiltinFunctionKind,
            },
            regular::HighRegularFunctionId,
        },
        variable::{HighVariableId, SemanticVariableDeclaration},
    },
};
use crate::{
    parsed::{
        environment::ParsedEnvironment,
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
        return_type: SemanticDataType,
        callee_id: HighRegularFunctionId,
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
    pub const fn return_type(&self) -> &SemanticDataType {
        match self {
            Self::Regular { return_type, .. } => return_type,
            Self::MCFunction => &SemanticDataType::Integer,
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
    pub parsed_environment: ParsedEnvironment,
    pub semantic_environment: SemanticEnvironment,
    pub impl_generic_ids_and_names: Vec<(Vec<HighGenericId>, Vec<String>)>,
}

impl SemanticAnalysisContext {
    #[must_use]
    pub fn new(id: &str, max_infos: usize) -> Self {
        let mut self_ = Self {
            infos: Vec::new(),
            max_infos,
            environment: Environment::default(),
            parsed_environment: ParsedEnvironment::default(),
            semantic_environment: SemanticEnvironment::default(),
            scopes: vec![Scope::default()],
            loop_depth: 0,
            current_module_path: Vec::new(),
            function_contexts: SmallVec::new(),
            impl_generic_ids_and_names: Vec::new(),
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

        for builtin_function in SemanticBuiltinFunctionKind::iter() {
            self.declare_builtin_function(Visibility::Public, builtin_function.declaration());
        }

        self.exit_module_and_declare(Visibility::Public);
    }

    #[inline]
    pub fn enter_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    pub fn enter_implementation(
        &mut self,
        generic_ids: Vec<HighGenericId>,
        generic_names: Vec<String>,
    ) {
        self.enter_scope();

        self.impl_generic_ids_and_names
            .push((generic_ids, generic_names));
    }

    #[must_use]
    pub fn exit_implementation(&mut self) -> (Vec<HighGenericId>, Vec<String>, Scope) {
        let scope = self.exit_scope();

        let (generic_ids, generic_names) = self.impl_generic_ids_and_names.pop().unwrap();

        (generic_ids, generic_names, scope)
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
    pub fn exit_module(&mut self) -> ParsedModuleDeclaration {
        let name = self.current_module_path.pop().unwrap();

        let scope = self.exit_scope();

        scope.into_module_declaration(name)
    }

    pub fn exit_module_and_declare(&mut self, visibility: Visibility) -> HighModuleId {
        let module = self.exit_module();

        self.declare_parsed_module(visibility, module)
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
    ) -> SemanticDataType {
        self.add_invalid_generics::<()>(span, type_name, expected, actual);

        SemanticDataType::Error
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
    pub fn add_error_type(&mut self, span: Span, error: SemanticAnalysisError) -> SemanticDataType {
        self.add_error_unit(span, error);

        SemanticDataType::Error
    }

    #[must_use]
    pub fn declare_variable(
        &mut self,
        visibility: Visibility,
        name: String,
        data_type: SemanticDataType,
    ) -> HighVariableId {
        let id = self.declare_parsed_value(
            visibility,
            ParsedValueDeclarationKind::Variable(ParsedVariableDeclaration { name: name.clone() }),
        );

        self.set_semantic_value(
            id,
            visibility,
            SemanticValueDeclarationKind::Variable(SemanticVariableDeclaration { name, data_type }),
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
    pub fn declare_parsed_module(
        &mut self,
        visibility: Visibility,
        declaration: ParsedModuleDeclaration,
    ) -> HighModuleId {
        let id =
            self.declare_parsed_type(visibility, ParsedTypeDeclarationKind::Module(declaration));

        HighModuleId(id.0)
    }

    #[inline]
    pub fn set_semantic_generic(
        &mut self,
        id: HighGenericId,
        visibility: Visibility,
        name: String,
    ) {
        self.set_semantic_type(id, visibility, SemanticTypeDeclarationKind::Generic(name));
    }

    pub fn declare_builtin_type(&mut self, declaration: SemanticBuiltinTypeDeclaration) {
        self.declare_type(
            Visibility::Public,
            ParsedTypeDeclarationKind::Builtin(declaration.clone().into()),
            SemanticTypeDeclarationKind::Builtin(declaration),
        );
    }

    #[inline]
    pub fn declare_parsed_type(
        &mut self,
        visibility: Visibility,
        kind: ParsedTypeDeclarationKind,
    ) -> HighTypeId {
        let name = kind.name().to_owned();

        let id = self.parsed_environment.declare_type(ParsedTypeDeclaration {
            module_path: self.current_module_path.clone(),
            visibility,
            kind,
        });

        self.current_scope_mut().declare_type(name, id);

        id
    }

    pub fn declare_parsed_value(
        &mut self,
        visibility: Visibility,
        kind: ParsedValueDeclarationKind,
    ) -> HighValueId {
        let name = kind.name().to_owned();

        let id = self
            .parsed_environment
            .declare_value(ParsedValueDeclaration {
                visibility,
                module_path: self.current_module_path.clone(),
                kind,
            });

        self.current_scope_mut().declare_value(name, id);

        id
    }

    pub fn declare_type(
        &mut self,
        visibility: Visibility,
        parsed: ParsedTypeDeclarationKind,
        semantic: SemanticTypeDeclarationKind,
    ) -> HighTypeId {
        let id = self.declare_parsed_type(visibility, parsed);

        self.set_semantic_type(id, visibility, semantic);

        id
    }

    #[inline]
    pub fn set_semantic_type<I: Into<HighTypeId>>(
        &mut self,
        id: I,
        visibility: Visibility,
        declaration: SemanticTypeDeclarationKind,
    ) {
        let id = id.into();

        let name = declaration.name().to_owned();

        self.semantic_environment.declare_type(
            id,
            SemanticTypeDeclaration {
                visibility,
                module_path: self.current_module_path.clone(),
                kind: declaration,
            },
        );

        self.current_scope_mut().declare_type(name, id);
    }

    #[inline]
    pub fn set_semantic_value<I: Into<HighValueId>>(
        &mut self,
        id: I,
        visibility: Visibility,
        kind: SemanticValueDeclarationKind,
    ) {
        let id = id.into();

        self.semantic_environment.declare_value(
            id,
            SemanticValueDeclaration {
                visibility,
                module_path: self.current_module_path.clone(),
                kind,
            },
        );
    }

    pub fn declare_value(
        &mut self,
        visibility: Visibility,
        declaration: SemanticValueDeclarationKind,
    ) -> HighValueId {
        let id = self.declare_parsed_value(visibility, declaration.clone().into());

        self.set_semantic_value(id, visibility, declaration);

        id
    }

    pub fn declare_builtin_function(
        &mut self,
        visibility: Visibility,
        declaration: SemanticBuiltinFunctionDeclaration,
    ) -> HighBuiltinFunctionId {
        let id = self.declare_value(
            visibility,
            SemanticValueDeclarationKind::Function(Box::new(SemanticFunctionDeclaration::Builtin(
                declaration,
            ))),
        );

        HighBuiltinFunctionId(id.0)
    }

    #[allow(clippy::type_complexity)]
    fn resolve_type_path<'a, T: Clone>(
        &mut self,
        path: &'a GenericPath<T>,
    ) -> Option<(HighTypeId, Vec<Span>, Vec<T>, &'a GenericPathSegment<T>)> {
        let (last_segment, segments) = path.segments.split_last()?;

        if segments.is_empty() {
            return None;
        }

        let (first_segment, segments) = segments.split_first()?;

        let mut current_type_id = self.get_type_id(&first_segment.name)?;
        let mut accumulated_generic_spans = first_segment.generic_spans.clone();
        let mut accumulated_generic_types = first_segment.generic_types.clone();

        for segment in segments {
            let declaration = self.semantic_environment.get_type(current_type_id);

            let id = match declaration.get_visible_type_id(self, current_type_id, &segment.name) {
                Ok(id) => id,
                Err(error) => return self.add_error(segment.name_span, error),
            };

            let declaration = self.semantic_environment.get_type(id);

            if !declaration.is_visible(&self.current_module_path) {
                self.add_error_unit(
                    segment.name_span,
                    SemanticAnalysisError::TypeNotPublic(segment.name.clone()),
                );
            }

            current_type_id = id;

            accumulated_generic_spans.extend(segment.generic_spans.iter().copied());
            accumulated_generic_types.extend(segment.generic_types.iter().cloned());
        }

        Some((
            current_type_id,
            accumulated_generic_spans,
            accumulated_generic_types,
            last_segment,
        ))
    }

    #[must_use]
    pub fn get_visible_type_id<T: Debug + Clone>(
        &mut self,
        path: &GenericPath<T>,
    ) -> Option<(HighTypeId, Vec<Span>, Vec<T>)> {
        if path.segments.len() == 1 {
            let segment = &path.segments[0];

            let id = self.get_type_id(&segment.name);

            if id.is_none() {
                return self.add_error(
                    segment.name_span,
                    SemanticAnalysisError::UnknownType(segment.name.clone()),
                );
            }

            return id.map(|id| {
                (
                    id,
                    segment.generic_spans.clone(),
                    segment.generic_types.clone(),
                )
            });
        }

        let (type_id, mut generic_spans, mut generic_types, last_segment) =
            self.resolve_type_path(path)?;

        let declaration = self.semantic_environment.get_type(type_id);

        let id = match declaration.get_visible_type_id(self, type_id, &last_segment.name) {
            Ok(id) => id,
            Err(error) => return self.add_error(last_segment.name_span, error),
        };

        let declaration = self.semantic_environment.get_type(id);

        if !declaration.is_visible(&self.current_module_path) {
            self.add_error_unit(
                last_segment.name_span,
                SemanticAnalysisError::TypeNotPublic(last_segment.name.clone()),
            );
        }

        generic_spans.extend(last_segment.generic_spans.iter().copied());
        generic_types.extend(last_segment.generic_types.iter().cloned());

        Some((id, generic_spans, generic_types))
    }

    #[must_use]
    pub fn get_visible_value_id<T: Debug + Clone>(
        &mut self,
        path: &GenericPath<T>,
    ) -> Option<(HighValueId, Vec<Span>, Vec<T>)> {
        if path.segments.len() == 1 {
            let segment = &path.segments[0];

            let id = self.get_value_id(&segment.name);

            if id.is_none() {
                return self.add_error(
                    segment.name_span,
                    SemanticAnalysisError::UnknownValue(segment.name.clone()),
                );
            }

            return id.map(|id| {
                (
                    id,
                    segment.generic_spans.clone(),
                    segment.generic_types.clone(),
                )
            });
        }

        let (type_id, mut generic_spans, mut generic_types, last_segment) =
            self.resolve_type_path(path)?;

        let declaration = self.semantic_environment.get_type(type_id);

        let id = match declaration.get_visible_value_id(self, type_id, &last_segment.name) {
            Ok(id) => id,
            Err(error) => return self.add_error(last_segment.name_span, error),
        };

        let declaration = self.semantic_environment.get_value(id);

        if !declaration.is_visible(&self.current_module_path) {
            self.add_error_unit(
                last_segment.name_span,
                SemanticAnalysisError::ValueNotPublic(last_segment.name.clone()),
            );
        }

        generic_spans.extend(last_segment.generic_spans.iter().copied());
        generic_types.extend(last_segment.generic_types.iter().cloned());

        Some((id, generic_spans, generic_types))
    }

    fn try_resolve_path<'a>(
        &mut self,
        path: &'a Path,
    ) -> Result<(HighTypeId, &'a PathSegment), (Span, SemanticAnalysisError)> {
        let (last_segment, segments) = path.segments.split_last().unwrap();

        let (first_segment, segments) = segments
            .split_first()
            .expect("segments.len should be checked before calling try_resolve_path");

        let Some(mut current_type_id) = self.get_type_id(&first_segment.name) else {
            return Err((
                first_segment.span,
                SemanticAnalysisError::UnknownType(first_segment.name.clone()),
            ));
        };

        for segment in segments {
            let declaration = self.semantic_environment.get_type(current_type_id);

            let id = declaration
                .get_visible_type_id(self, current_type_id, &segment.name)
                .map_err(|error| (segment.span, error))?;

            let declaration = self.semantic_environment.get_type(id);

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

        let declaration = self.semantic_environment.get_type(type_id);

        let id = declaration
            .get_visible_type_id(self, type_id, &last_segment.name)
            .map_err(|error| (last_segment.span, error))?;

        let declaration = self.semantic_environment.get_type(id);

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

        let declaration = self.semantic_environment.get_type(type_id);

        let id = declaration
            .get_visible_value_id(self, type_id, &last_segment.name)
            .map_err(|error| (last_segment.span, error))?;

        let declaration = self.semantic_environment.get_value(id);

        if !declaration.is_visible(&self.current_module_path) {
            return Err((
                last_segment.span,
                SemanticAnalysisError::ValueNotPublic(last_segment.name.clone()),
            ));
        }

        Ok(id)
    }

    #[must_use]
    pub fn get_visible_regular_struct(
        &mut self,
        id: HighTypeId,
        segment: &GenericPathSegment<SemanticDataType>,
    ) -> Option<&SemanticRegularStructDeclaration> {
        let SemanticTypeDeclaration {
            module_path,
            visibility,
            kind: declaration,
        } = self.semantic_environment.get_type(id);

        let is_visible = self.is_item_visible(module_path, *visibility);

        if !is_visible {
            return self.add_error(
                segment.name_span,
                SemanticAnalysisError::TypeNotPublic(segment.name.clone()),
            );
        }

        let data_type = declaration.clone().into_data_type(self, id, segment);

        let SemanticDataType::Struct(id, _) = data_type else {
            return self.add_error(
                segment.name_span,
                SemanticAnalysisError::NotAStruct(data_type),
            );
        };

        let (_, _, declaration) = self.semantic_environment.get_struct(id);

        let SemanticStructDeclaration::Struct(declaration) = declaration else {
            return Self::add_error_static(
                &mut self.infos,
                self.max_infos,
                segment.name_span,
                SemanticAnalysisError::NotARegularStruct(data_type),
            );
        };

        Some(declaration)
    }

    #[must_use]
    pub fn get_visible_tuple_struct(
        &mut self,
        id: HighTypeId,
        segment: &GenericPathSegment<SemanticDataType>,
    ) -> Option<&SemanticTupleStructDeclaration> {
        let SemanticTypeDeclaration {
            module_path,
            visibility,
            kind: declaration,
        } = self.semantic_environment.get_type(id);

        let is_visible = self.is_item_visible(module_path, *visibility);

        if !is_visible {
            return self.add_error(
                segment.name_span,
                SemanticAnalysisError::TypeNotPublic(segment.name.clone()),
            );
        }

        let data_type = declaration.clone().into_data_type(self, id, segment);

        let SemanticDataType::Struct(id, _) = data_type else {
            return self.add_error(
                segment.name_span,
                SemanticAnalysisError::NotAStruct(data_type),
            );
        };

        let (_, _, declaration) = self.semantic_environment.get_struct(id);

        let SemanticStructDeclaration::Tuple(declaration) = declaration else {
            return Self::add_error_static(
                &mut self.infos,
                self.max_infos,
                segment.name_span,
                SemanticAnalysisError::NotATupleStruct(data_type),
            );
        };

        Some(declaration)
    }
}
