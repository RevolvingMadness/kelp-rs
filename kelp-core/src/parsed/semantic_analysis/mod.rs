use std::collections::HashMap;
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
use crate::semantic::environment::r#type::HighVisibleTypeId;
use crate::semantic::environment::r#type::generic::{HighGenericId, SemanticGenericDeclaration};
use crate::semantic::environment::r#type::module::HighModuleId;
use crate::semantic::environment::r#type::r#struct::HighStructId;
use crate::semantic::environment::r#type::r#struct::regular::HighRegularStructId;
use crate::semantic::environment::r#type::r#struct::tuple::HighTupleStructId;
use crate::semantic::environment::value::HighVisibleValueId;
use crate::semantic::environment::{
    SemanticEnvironment,
    r#type::{
        HighTypeId, SemanticTypeDeclaration, SemanticTypeDeclarationKind,
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
use crate::semantic::path::{ParsedPath, ParsedPathSegment};
use crate::semantic::typed_path::{SemanticTypedPath, SemanticTypedPathSegment};
use crate::{
    parsed::{
        environment::ParsedEnvironment,
        semantic_analysis::{
            info::{SemanticAnalysisInfo, error::SemanticAnalysisError},
            scope::Scope,
        },
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
        id: HighRegularFunctionId,
        declaration_span: Span,
        modifiers: RegularFunctionModifiers,
        return_type: SemanticDataType,
        calls: HashSet<(Span, HighFunctionId)>,
    },
    MCFunction,
}

impl FunctionContext {
    #[must_use]
    pub const fn declaration_span(&self) -> Option<Span> {
        match self {
            Self::Regular {
                declaration_span, ..
            } => Some(*declaration_span),
            Self::MCFunction => None,
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
    pub current_module_path: Vec<HighModuleId>,
    pub function_contexts: SmallVec<[FunctionContext; 5]>,
    pub max_infos: usize,
    pub environment: Environment,
    pub parsed_environment: ParsedEnvironment,
    pub semantic_environment: SemanticEnvironment,
    pub impl_generic_ids: Vec<Vec<HighGenericId>>,
}

impl SemanticAnalysisContext {
    #[must_use]
    pub fn new(project_id: &str, max_infos: usize) -> Self {
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
            impl_generic_ids: Vec::new(),
        };

        self_.declare_std_module();

        self_.enter_parsed_module(Visibility::Public, None, project_id.to_owned());

        self_
    }

    pub fn declare_std_module(&mut self) {
        self.enter_parsed_module(Visibility::Public, None, "std".to_owned());

        for builtin_type in BuiltinTypeKind::iter() {
            self.declare_builtin_type(builtin_type.declaration());
        }

        for builtin_function in SemanticBuiltinFunctionKind::iter() {
            self.declare_builtin_function(Visibility::Public, builtin_function.declaration());
        }

        self.exit_module();
    }

    #[inline]
    pub fn enter_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    pub fn enter_parsed_implementation(&mut self, generic_ids: Vec<HighGenericId>) {
        self.enter_scope();

        self.impl_generic_ids.push(generic_ids);
    }

    #[must_use]
    pub fn exit_parsed_implementation(&mut self) -> (Vec<HighGenericId>, Scope) {
        let scope = self.exit_scope();

        let generic_ids = self.impl_generic_ids.pop().unwrap();

        (generic_ids, scope)
    }

    pub fn enter_semantic_implementation(&mut self, scope: Scope, generic_ids: Vec<HighGenericId>) {
        self.push_scope(scope);

        self.impl_generic_ids.push(generic_ids);
    }

    #[must_use]
    pub fn exit_semantic_implementation(&mut self) -> (Vec<HighGenericId>, Scope) {
        let scope = self.exit_scope();

        let generic_ids = self.impl_generic_ids.pop().unwrap();

        (generic_ids, scope)
    }

    #[inline]
    pub fn exit_scope(&mut self) -> Scope {
        self.scopes.pop().unwrap()
    }

    #[inline]
    pub fn push_scope(&mut self, scope: Scope) {
        self.scopes.push(scope);
    }

    pub fn enter_parsed_module(
        &mut self,
        visibility: Visibility,
        name_span: Option<Span>,
        name: String,
    ) -> HighModuleId {
        let id = self.declare_parsed_type(
            visibility,
            ParsedTypeDeclarationKind::Module(ParsedModuleDeclaration {
                name_span,
                name,
                types: HashMap::new(),
                values: HashMap::new(),
            }),
        );

        let id = HighModuleId(id.0);

        self.current_module_path.push(id);

        self.enter_scope();

        id
    }

    pub fn enter_semantic_module(&mut self, id: HighModuleId) {
        self.current_module_path.push(id);

        self.enter_scope();
    }

    #[inline]
    pub fn exit_module(&mut self) {
        let id = self.current_module_path.pop().unwrap();

        let scope = self.exit_scope();

        let ParsedTypeDeclaration {
            kind: ParsedTypeDeclarationKind::Module(declaration),
            ..
        } = self.parsed_environment.get_type_mut(id)
        else {
            unreachable!();
        };

        let (types, values) = scope.into_tuple();

        declaration.types.extend(types);
        declaration.values.extend(values);
    }

    #[must_use]
    pub fn is_item_visible(
        &self,
        target_module_path: &[HighModuleId],
        visibility: Visibility,
    ) -> bool {
        if matches!(visibility, Visibility::Public) {
            return true;
        }

        self.current_module_path.starts_with(target_module_path)
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
    pub fn add_error<T>(&mut self, error: SemanticAnalysisError) -> Option<T> {
        self.add_info(SemanticAnalysisInfo::Error(error))
    }

    #[inline]
    #[must_use]
    pub fn add_error_static<T>(
        infos: &mut Vec<SemanticAnalysisInfo>,
        max_infos: usize,
        error: SemanticAnalysisError,
    ) -> Option<T> {
        Self::add_info_static(infos, max_infos, SemanticAnalysisInfo::Error(error))
    }

    #[inline]
    pub fn add_error_unit(&mut self, error: SemanticAnalysisError) -> Option<()> {
        self.add_error(error)
    }

    #[inline]
    pub fn add_error_unit_static(
        infos: &mut Vec<SemanticAnalysisInfo>,
        max_infos: usize,
        error: SemanticAnalysisError,
    ) -> Option<()> {
        Self::add_error_static(infos, max_infos, error)
    }

    #[inline]
    #[must_use]
    pub fn add_error_type(&mut self, error: SemanticAnalysisError) -> SemanticDataType {
        self.add_error_unit(error);

        SemanticDataType::Error
    }

    #[must_use]
    pub fn declare_variable(
        &mut self,
        visibility: Visibility,
        name_span: Span,
        name: String,
        data_type: SemanticDataType,
    ) -> HighVariableId {
        let id = self.declare_parsed_value(
            visibility,
            ParsedValueDeclarationKind::Variable(ParsedVariableDeclaration {
                name_span,
                name: name.clone(),
            }),
        );

        self.set_semantic_value(
            id,
            visibility,
            SemanticValueDeclarationKind::Variable(SemanticVariableDeclaration {
                name_span,
                name,
                data_type,
            }),
        );

        HighVariableId(id.0)
    }

    pub fn get_type_id_in_scope(
        &self,
        segment: &ParsedPathSegment,
    ) -> Result<HighVisibleTypeId, SemanticAnalysisError> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| {
                let id = scope.get_type_id(&segment.name)?;

                let id = HighVisibleTypeId(id.0);

                Some(id)
            })
            .ok_or_else(|| SemanticAnalysisError::UnknownType {
                span: segment.name_span,
                name: segment.name.clone(),
            })
    }

    pub fn get_semantic_value_id(
        &self,
        name: &str,
        name_span: Span,
        actual_generic_count: usize,
    ) -> Result<HighVisibleValueId, SemanticAnalysisError> {
        let Some(id) = self
            .scopes
            .iter()
            .rev()
            .find_map(|scope| scope.get_value_id(name))
            .map(|id| HighVisibleValueId(id.0))
        else {
            return Err(SemanticAnalysisError::UnknownValue {
                span: name_span,
                name: name.to_owned(),
            });
        };

        let declaration = self.semantic_environment.get_value(id);

        let expected_generic_count = declaration.kind.generic_count();

        if actual_generic_count != expected_generic_count {
            return Err(SemanticAnalysisError::InvalidGenerics {
                type_name_span: name_span,
                type_kind: declaration.kind.get_value_kind().into(),
                declaration_span: declaration.kind.name_span(),
                expected: expected_generic_count,
                actual: actual_generic_count,
            });
        }

        Ok(id)
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
        let scope = self.current_scope();

        if let Some(declaration_span) =
            scope.get_type_declaration_span(&self.parsed_environment, &name)
        {
            self.add_error_unit(SemanticAnalysisError::TypeAlreadyDeclared {
                declaration_span,
                redeclaration_span: name_span,
                name,
            });

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
        let scope = self.current_scope();

        if let Some(declaration_span) =
            scope.get_value_declaration_span(&self.parsed_environment, &name)
        {
            self.add_error_unit(SemanticAnalysisError::ValueAlreadyDeclared {
                declaration_span,
                redeclaration_span: name_span,
                name,
            });

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
        name_span: Span,
        name: String,
    ) {
        self.set_semantic_type(
            id,
            visibility,
            SemanticTypeDeclarationKind::Generic(SemanticGenericDeclaration { name_span, name }),
        );
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

    // fn resolve_type_path(&mut self, path: SemanticTypedPath) -> Option<ResolvedTypePath> {
    //     let mut segments = path.segments.into_iter();

    //     let first_segment = segments.next()?;
    //     let last_segment = segments.next_back()?;

    //     let mut current_type_id = first_segment.get_type_id(self)?;
    //     let mut current_span = first_segment.name_span;

    //     let mut inherited_generic_spans = first_segment.generic_spans.clone();
    //     let mut inherited_generic_types = first_segment.generic_types.clone();

    //     for segment in segments {
    //         let declaration = self.semantic_environment.get_type(current_type_id);

    //         let type_id = segment.type_id.unwrap();
    //         // let id = match declaration.get_visible_type_id(
    //         //     &self.semantic_environment,
    //         //     &self.current_module_path,
    //         //     current_type_id,
    //         //     current_span,
    //         //     &segment.type_id,
    //         //     segment.name_span,
    //         // ) {
    //         //     Ok(id) => id,
    //         //     Err(error) => return self.add_error(error),
    //         // };

    //         current_type_id = type_id;
    //         current_span = segment.name_span;

    //         inherited_generic_spans.extend(segment.generic_spans.iter().copied());
    //         inherited_generic_types.extend(segment.generic_types.iter().cloned());
    //     }

    //     Some(ResolvedTypePath {
    //         id: current_type_id,
    //         inherited_generic_spans,
    //         inherited_generic_types,
    //         last_segment,
    //     })
    // }

    // #[must_use]
    // #[allow(clippy::type_complexity)]
    // pub fn get_visible_type_id(
    //     &mut self,
    //     mut path: SemanticTypedPath,
    // ) -> Option<(
    //     HighVisibleTypeId,
    //     Vec<Span>,
    //     Vec<SemanticDataType>,
    //     SemanticTypedPathSegment,
    // )> {
    //     if path.segments.len() == 1 {
    //         let segment = path.segments.pop().unwrap();

    //         let id = segment.get_type_id(self)?;

    //         return Some((
    //             id,
    //             segment.generic_spans.clone(),
    //             segment.generic_types.clone(),
    //             segment,
    //         ));
    //     }

    //     let ResolvedTypePath {
    //         id,
    //         inherited_generic_spans,
    //         inherited_generic_types,
    //         last_segment,
    //     } = self.resolve_type_path(path)?;

    //     let supplied_generic_spans = &last_segment.generic_spans;
    //     let supplied_generic_types = &last_segment.generic_types;

    //     let declaration = self.semantic_environment.get_type(id);

    //     let id = last_segment.type_id.unwrap();
    //     // let id = match declaration.get_visible_type_id(
    //     //     &self.semantic_environment,
    //     //     &self.current_module_path,
    //     //     id,
    //     //     last_segment.name_span,
    //     //     &last_segment.type_id,
    //     //     last_segment.name_span,
    //     // ) {
    //     //     Ok(id) => id,
    //     //     Err(error) => return self.add_error(error),
    //     // };

    //     let mut generic_spans = inherited_generic_spans;
    //     generic_spans.extend(supplied_generic_spans.iter().copied());

    //     let mut generic_types = inherited_generic_types;
    //     generic_types.extend(supplied_generic_types.iter().cloned());

    //     Some((id, generic_spans, generic_types, last_segment))
    // }

    // #[must_use]
    // #[allow(clippy::type_complexity)]
    // pub fn get_visible_value_within_type(
    //     &mut self,
    //     mut path: SemanticTypedPath,
    // ) -> Option<(
    //     HighVisibleValueId,
    //     Vec<SemanticDataType>,
    //     Option<SemanticDataType>,
    // )> {
    //     if path.segments.len() == 1 {
    //         let segment = path.segments.remove(0);
    //         let supplied_generic_types = &segment.generic_types;

    //         let id = segment.get_value_id(self)?;

    //         let declaration = self.semantic_environment.get_value(id).clone();

    //         let data_type = declaration.into_data_type(
    //             self,
    //             id,
    //             Vec::new(),
    //             supplied_generic_types,
    //             segment.name_span,
    //         );

    //         return Some((id, segment.generic_types, data_type));
    //     }

    //     let ResolvedTypePath {
    //         id,
    //         inherited_generic_types,
    //         last_segment,
    //         ..
    //     } = self.resolve_type_path(path)?;
    //     let supplied_generic_types = &last_segment.generic_types;

    //     let id = last_segment.get_value_id(self)?;

    //     let declaration = self.semantic_environment.get_value(id).clone();

    //     let data_type = declaration.into_data_type(
    //         self,
    //         id,
    //         inherited_generic_types.clone(),
    //         supplied_generic_types,
    //         last_segment.name_span,
    //     );

    //     let mut generic_types = inherited_generic_types;

    //     generic_types.extend(supplied_generic_types.iter().cloned());

    //     Some((id, generic_types, data_type))
    // }

    // fn try_resolve_path<'a>(
    //     &self,
    //     path: &'a ParsedPath,
    // ) -> Result<(HighVisibleTypeId, &'a ParsedPathSegment), SemanticAnalysisError> {
    //     let (last_segment, segments) = path.segments.split_last().unwrap();

    //     let (first_segment, segments) = segments
    //         .split_first()
    //         .expect("segments.len should be checked before calling try_resolve_path");

    //     let mut current_type_id = self.get_type_id_in_scope(first_segment)?;

    //     let mut current_span = first_segment.name_span;

    //     for segment in segments {
    //         let declaration = self.semantic_environment.get_type(current_type_id);

    //         let id = declaration.get_visible_type_id(
    //             &self.semantic_environment,
    //             &self.current_module_path,
    //             current_type_id,
    //             current_span,
    //             &segment.name,
    //             segment.name_span,
    //         )?;

    //         current_type_id = id;
    //         current_span = segment.name_span;
    //     }

    //     Ok((current_type_id, last_segment))
    // }

    // pub fn try_get_visible_type(
    //     &mut self,
    //     path: &ParsedPath,
    // ) -> Result<HighVisibleTypeId, SemanticAnalysisError> {
    //     if path.segments.len() == 1 {
    //         let segment = &path.segments[0];

    //         return self.get_type_id_in_scope(segment);
    //     }

    //     let (type_id, last_segment) = self.try_resolve_path(path)?;

    //     let declaration = self.semantic_environment.get_type(type_id);

    //     let id = declaration.get_visible_type_id(
    //         &self.semantic_environment,
    //         &self.current_module_path,
    //         type_id,
    //         last_segment.name_span,
    //         &last_segment.name,
    //         last_segment.name_span,
    //     )?;

    //     Ok(id)
    // }

    // pub fn try_get_visible_value(
    //     &mut self,
    //     path: &ParsedPath,
    // ) -> Result<HighVisibleValueId, SemanticAnalysisError> {
    //     if path.segments.len() == 1 {
    //         let segment = &path.segments[0];

    //         return self.get_semantic_value_id(&segment.name, segment.name_span, 0);
    //     }

    //     let (type_id, last_segment) = self.try_resolve_path(path)?;

    //     let declaration = self.semantic_environment.get_type(type_id);

    //     let id = declaration.get_visible_value_id(
    //         &self.semantic_environment,
    //         &self.current_module_path,
    //         type_id,
    //         last_segment.name_span,
    //         &last_segment.name,
    //         last_segment.name_span,
    //     )?;

    //     Ok(id)
    // }

    #[must_use]
    pub fn get_struct_id(
        &mut self,
        id: HighVisibleTypeId,
        generic_spans: &[Span],
        generic_types: &[SemanticDataType],
        name_span: Span,
    ) -> Option<(HighStructId, SemanticDataType)> {
        let SemanticTypeDeclaration {
            kind: declaration, ..
        } = self.semantic_environment.get_type(id);

        let data_type =
            declaration
                .clone()
                .into_data_type(self, id, generic_spans, generic_types, name_span);

        let SemanticDataType::Struct(id, _) = data_type else {
            return self.add_error(SemanticAnalysisError::NotAStruct {
                type_span: name_span,
                data_type,
            });
        };

        let id = HighStructId(id.0);

        Some((id, data_type))
    }

    #[must_use]
    pub fn get_regular_struct(
        &mut self,
        id: HighStructId,
        data_type: SemanticDataType,
        name_span: Span,
    ) -> Option<(HighRegularStructId, &SemanticRegularStructDeclaration)> {
        let declaration = self.semantic_environment.get_struct_declaration(id);

        let SemanticStructDeclaration::Struct(declaration) = declaration else {
            return Self::add_error_static(
                &mut self.infos,
                self.max_infos,
                SemanticAnalysisError::NotARegularStruct {
                    type_span: name_span,
                    data_type,
                },
            );
        };

        let id = HighRegularStructId(id.0);

        Some((id, declaration))
    }

    #[must_use]
    pub fn get_tuple_struct(
        &mut self,
        id: HighStructId,
        data_type: SemanticDataType,
        name_span: Span,
    ) -> Option<(HighTupleStructId, &SemanticTupleStructDeclaration)> {
        let declaration = self.semantic_environment.get_struct_declaration(id);

        let SemanticStructDeclaration::Tuple(declaration) = declaration else {
            return Self::add_error_static(
                &mut self.infos,
                self.max_infos,
                SemanticAnalysisError::NotATupleStruct {
                    type_span: name_span,
                    data_type,
                },
            );
        };

        let id = HighTupleStructId(id.0);

        Some((id, declaration))
    }
}
