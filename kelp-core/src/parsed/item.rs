use std::collections::{HashMap, HashSet};

use minecraft_command_types::resource_location::ResourceLocation;

use crate::parsed::environment::value::ParsedValueDeclarationKind;
use crate::parsed::environment::value::function::ParsedFunctionDeclaration;
use crate::parsed::environment::value::function::regular::ParsedRegularFunctionDeclaration;
use crate::parsed::pattern::ParsedPattern;
use crate::parsed::semantic_analysis::RegularFunctionModifiers;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::{
    HighGenericId, HighTypeId, SemanticTypeDeclarationKind,
    r#struct::{
        SemanticStructDeclaration, regular::SemanticRegularStructDeclaration,
        tuple::SemanticTupleStructDeclaration,
    },
};
use crate::semantic::environment::value::function::regular::HighRegularFunctionId;
use crate::{
    parsed::environment::r#type::{
        ParsedTypeDeclaration, ParsedTypeDeclarationKind,
        alias::ParsedTypeAliasDeclaration,
        r#struct::{
            ParsedStructDeclaration, regular::ParsedRegularStructDeclaration,
            tuple::ParsedTupleStructDeclaration,
        },
    },
    semantic::environment::implementation::SemanticImplementation,
};
use crate::{
    parsed::{
        data_type::ParsedDataType,
        expression::block::ParsedBlockExpression,
        semantic_analysis::{
            FunctionContext, SemanticAnalysisContext, info::error::SemanticAnalysisError,
            scope::Scope,
        },
        use_tree::UseTree,
    },
    semantic::item::SemanticItem,
    span::Span,
    trait_ext::CollectOptionAllIterExt,
    visibility::Visibility,
};

#[derive(Debug, Clone)]
pub enum ParsedItemKind {
    InherentImplementationItem {
        generic_names: Vec<String>,
        target_type_span: Span,
        target_type: ParsedDataType,
        associated_items: Vec<ParsedItem>,

        scope: Option<Scope>,
    },
    ModuleDeclaration {
        name_span: Span,
        name: String,
        items: Vec<ParsedItem>,
        id: Option<HighTypeId>,
    },
    FunctionDeclaration {
        recursive_keyword_span: Option<Span>,
        runtime_keyword_span: Option<Span>,
        name_span: Span,
        name: String,
        generic_names: Vec<String>,
        is_method: bool,
        parameters: Vec<(ParsedPattern, ParsedDataType)>,
        return_type: ParsedDataType,
        body: ParsedBlockExpression,

        id: Option<HighRegularFunctionId>,
        generic_ids: Option<Vec<HighGenericId>>,
    },
    MinecraftFunctionDeclaration {
        resource_location: ResourceLocation,
        body: ParsedBlockExpression,
    },
    TypeAliasDeclaration {
        name_span: Span,
        name: String,
        generic_names: Vec<String>,
        alias: ParsedDataType,

        id: Option<HighTypeId>,
        generic_ids: Option<Vec<HighGenericId>>,
    },
    RegularStructDeclaration {
        name_span: Span,
        name: String,
        generic_names: Vec<String>,
        field_types: HashMap<String, ParsedDataType>,

        id: Option<HighTypeId>,
        generic_ids: Option<Vec<HighGenericId>>,
    },
    TupleStructDeclaration {
        name_span: Span,
        name: String,
        generic_names: Vec<String>,
        field_types: Vec<ParsedDataType>,

        id: Option<HighTypeId>,
        generic_ids: Option<Vec<HighGenericId>>,
    },
    Use(UseTree),
}

#[derive(Debug, Clone)]
pub struct ParsedItem {
    pub span: Span,
    pub visibility: Visibility,
    pub kind: ParsedItemKind,
}

impl ParsedItem {
    pub fn resolve_names(&mut self, ctx: &mut SemanticAnalysisContext) -> Option<()> {
        match &mut self.kind {
            ParsedItemKind::InherentImplementationItem {
                generic_names,
                associated_items,
                scope: this_scope,
                ..
            } => {
                ctx.enter_scope();

                let generic_ids = generic_names
                    .iter()
                    .cloned()
                    .map(|generic_name| {
                        let id = ctx.declare_parsed_type(
                            Visibility::Public,
                            ParsedTypeDeclarationKind::Generic(generic_name),
                        );

                        HighGenericId(id.0)
                    })
                    .collect::<Vec<_>>();

                ctx.declare_parsed_type(
                    Visibility::Public,
                    ParsedTypeDeclarationKind::Alias(ParsedTypeAliasDeclaration {
                        name: "Self".to_owned(),
                        generic_ids,
                    }),
                );

                let _associated_items = associated_items
                    .iter_mut()
                    .map(|item| item.resolve_names(ctx))
                    .collect_option_all::<Vec<_>>();

                let scope = ctx.exit_scope();

                *this_scope = Some(scope);

                Some(())
            }
            ParsedItemKind::ModuleDeclaration {
                name_span,
                name,
                items,
                id: type_id,
                ..
            } => {
                if ctx.current_scope().type_is_declared(name) {
                    return ctx.add_error(
                        *name_span,
                        SemanticAnalysisError::TypeAlreadyDeclared(name.clone()),
                    );
                }

                ctx.enter_module(name.clone());

                for item in items {
                    item.resolve_names(ctx);
                }

                let id = ctx.exit_module_and_declare(self.visibility);

                *type_id = Some(id);

                Some(())
            }
            ParsedItemKind::RegularStructDeclaration {
                name_span,
                name,
                generic_names,
                id: this_id,
                generic_ids: this_generic_ids,
                ..
            } => {
                if ctx.current_scope().type_is_declared(name) {
                    return ctx.add_error(
                        *name_span,
                        SemanticAnalysisError::TypeAlreadyDeclared(name.clone()),
                    );
                }

                ctx.enter_scope();

                let generic_ids = generic_names
                    .iter()
                    .cloned()
                    .map(|generic_name| {
                        let id = ctx.declare_parsed_type(
                            Visibility::Public,
                            ParsedTypeDeclarationKind::Generic(generic_name),
                        );

                        HighGenericId(id.0)
                    })
                    .collect::<Vec<_>>();

                *this_generic_ids = Some(generic_ids.clone());

                ctx.exit_scope();

                let id = ctx.declare_parsed_type(
                    self.visibility,
                    ParsedTypeDeclarationKind::Struct(ParsedStructDeclaration::Struct(
                        ParsedRegularStructDeclaration {
                            name: name.clone(),
                            generic_ids,
                        },
                    )),
                );

                *this_id = Some(id);

                Some(())
            }
            ParsedItemKind::TupleStructDeclaration {
                name_span,
                name,
                generic_names,
                id: type_id,
                generic_ids: type_generic_ids,
                ..
            } => {
                if ctx.current_scope().type_is_declared(name) {
                    return ctx.add_error(
                        *name_span,
                        SemanticAnalysisError::TypeAlreadyDeclared(name.clone()),
                    );
                }

                ctx.enter_scope();

                let generic_ids = generic_names
                    .iter()
                    .cloned()
                    .map(|generic_name| {
                        let id = ctx.declare_parsed_type(
                            Visibility::Public,
                            ParsedTypeDeclarationKind::Generic(generic_name),
                        );

                        HighGenericId(id.0)
                    })
                    .collect::<Vec<_>>();

                *type_generic_ids = Some(generic_ids.clone());

                ctx.exit_scope();

                let id = ctx.declare_parsed_type(
                    self.visibility,
                    ParsedTypeDeclarationKind::Struct(ParsedStructDeclaration::Tuple(
                        ParsedTupleStructDeclaration {
                            name: name.clone(),
                            generic_ids,
                        },
                    )),
                );

                *type_id = Some(id);

                Some(())
            }
            ParsedItemKind::FunctionDeclaration {
                name_span,
                name,
                generic_names,
                id: this_id,
                generic_ids: this_generic_ids,
                ..
            } => {
                if ctx.current_scope().value_is_declared(name) {
                    return ctx.add_error(
                        *name_span,
                        SemanticAnalysisError::ValueAlreadyDeclared(name.clone()),
                    );
                }

                ctx.enter_scope();

                let generic_ids = generic_names
                    .iter()
                    .cloned()
                    .map(|generic_name| {
                        let id = ctx.declare_parsed_type(
                            Visibility::Public,
                            ParsedTypeDeclarationKind::Generic(generic_name),
                        );

                        HighGenericId(id.0)
                    })
                    .collect::<Vec<_>>();

                *this_generic_ids = Some(generic_ids.clone());

                ctx.exit_scope();

                let id = ctx.declare_parsed_value(
                    self.visibility,
                    ParsedValueDeclarationKind::Function(Box::new(
                        ParsedFunctionDeclaration::Regular(ParsedRegularFunctionDeclaration {
                            name: name.clone(),
                            generic_ids,
                        }),
                    )),
                );

                *this_id = Some(HighRegularFunctionId(id.0));

                Some(())
            }
            _ => Some(()),
        }
    }

    pub fn resolve_imports(&mut self, ctx: &mut SemanticAnalysisContext) -> Option<()> {
        match &mut self.kind {
            ParsedItemKind::ModuleDeclaration { name, items, .. } => {
                ctx.enter_module(name.clone());

                for item in items {
                    item.resolve_imports(ctx);
                }

                ctx.exit_module();
            }
            ParsedItemKind::Use(tree) => match tree {
                UseTree::Wildcard(path) => {
                    let mut path = path.clone();

                    let id = match ctx.try_get_visible_type(&path) {
                        Ok(id) => id,
                        Err((span, error)) => return ctx.add_error(span, error),
                    };

                    let ParsedTypeDeclaration {
                        kind: ParsedTypeDeclarationKind::Module(module),
                        ..
                    } = ctx.parsed_environment.get_type(id).clone()
                    else {
                        let last_segment = path.segments.pop().unwrap();

                        return ctx.add_error(
                            last_segment.span,
                            SemanticAnalysisError::NotAModule(last_segment.name),
                        );
                    };

                    for (name, id) in module.types {
                        ctx.declare_type_if_not_defined(name, id);
                    }

                    for (name, id) in module.values {
                        ctx.declare_value_if_not_defined(name, id);
                    }
                }
                UseTree::Group(path, use_trees) => {
                    let mut has_error = false;

                    for mut tree in use_trees {
                        if let Some(prefix_path) = &path {
                            match &mut tree {
                                UseTree::Path(path)
                                | UseTree::Wildcard(path)
                                | UseTree::As(path, _, _) => {
                                    let mut new_segments = prefix_path.segments.clone();

                                    new_segments.append(&mut path.segments);

                                    path.segments = new_segments;
                                }
                                UseTree::Group(path, _) => {
                                    if let Some(path) = path {
                                        let mut new_segments = prefix_path.segments.clone();
                                        new_segments.append(&mut path.segments);
                                        path.segments = new_segments;
                                    } else {
                                        *path = Some(prefix_path.clone());
                                    }
                                }
                            }
                        }

                        let item = Self {
                            span: self.span,
                            visibility: self.visibility,
                            kind: ParsedItemKind::Use(tree.clone()),
                        };

                        if item.perform_semantic_analysis(ctx).is_none() {
                            has_error = true;
                        }
                    }

                    if has_error {
                        return None;
                    }
                }
                UseTree::As(path, alias_span, alias) => {
                    let type_result = ctx.try_get_visible_type(path);

                    if let Ok(id) = type_result {
                        ctx.declare_type_in_current_scope(*alias_span, alias.clone(), id);
                    }

                    let value_resilt = ctx.try_get_visible_value(path);

                    if let Ok(id) = value_resilt {
                        ctx.declare_value_in_current_scope(*alias_span, alias.clone(), id);
                    }

                    if let Err((type_span, type_error)) = type_result
                        && let Err((value_span, value_error)) = value_resilt
                    {
                        ctx.add_error_unit(type_span, type_error);
                        ctx.add_error_unit(value_span, value_error);

                        return None;
                    }
                }
                UseTree::Path(path) => {
                    let last_segment = path.segments.last().unwrap();

                    let type_result = ctx.try_get_visible_type(path);

                    if let Ok(id) = type_result {
                        ctx.declare_type_in_current_scope(
                            last_segment.span,
                            last_segment.name.clone(),
                            id,
                        );
                    }

                    let value_resilt = ctx.try_get_visible_value(path);

                    if let Ok(id) = value_resilt {
                        ctx.declare_value_in_current_scope(
                            last_segment.span,
                            last_segment.name.clone(),
                            id,
                        );
                    }

                    if let Err((type_span, type_error)) = type_result
                        && let Err((value_span, value_error)) = value_resilt
                    {
                        ctx.add_error_unit(type_span, type_error);
                        ctx.add_error_unit(value_span, value_error);

                        return None;
                    }
                }
            },
            _ => {}
        }

        Some(())
    }

    pub fn resolve_types(&mut self, ctx: &mut SemanticAnalysisContext) {
        match &mut self.kind {
            ParsedItemKind::InherentImplementationItem {
                generic_names,
                target_type_span,
                target_type,
                associated_items,
                scope: this_scope,
                ..
            } => {
                let scope = this_scope.take().unwrap();

                ctx.push_scope(scope);

                for item in associated_items {
                    item.resolve_types(ctx);
                }

                let scope = ctx.exit_scope();

                let target_type = target_type.clone().perform_semantic_analysis(ctx);

                let (types, values) = scope.clone().into_tuple();

                *this_scope = Some(scope);

                match target_type {
                    target_type @ SemanticDataType::Struct(id, _) => {
                        let implementation = SemanticImplementation {
                            generic_names: generic_names.clone(),
                            target_type,
                            types,
                            values,
                        };

                        ctx.semantic_environment
                            .impls
                            .entry(HighTypeId(id.0))
                            .or_default()
                            .push(implementation);
                    }

                    SemanticDataType::Error => {}

                    _ => {
                        ctx.add_error_unit(
                            *target_type_span,
                            SemanticAnalysisError::InherentImplRequiresNomialType,
                        );
                    }
                }
            }
            ParsedItemKind::ModuleDeclaration { name, items, .. } => {
                ctx.enter_module(name.clone());

                for item in items {
                    item.resolve_types(ctx);
                }

                let _ = ctx.exit_module();
            }
            ParsedItemKind::RegularStructDeclaration {
                name,
                generic_names,
                field_types,

                id,
                generic_ids,
                ..
            } => {
                let id = id.unwrap();
                let generic_ids = generic_ids.as_ref().unwrap();

                ctx.enter_scope();

                for (generic_id, generic_name) in generic_ids
                    .iter()
                    .copied()
                    .zip(generic_names.iter().cloned())
                {
                    ctx.declare_generic(generic_id, Visibility::Public, generic_name);
                }

                let field_types = field_types
                    .iter()
                    .map(|(field_name, field_type)| {
                        let field_name = field_name.clone();
                        let field_type = field_type.clone();

                        let field_type = field_type.perform_semantic_analysis(ctx);

                        (field_name, field_type)
                    })
                    .collect();

                ctx.exit_scope();

                ctx.declare_semantic_type(
                    id,
                    Visibility::Public,
                    SemanticTypeDeclarationKind::Struct(SemanticStructDeclaration::Struct(
                        SemanticRegularStructDeclaration {
                            name: name.clone(),
                            generic_ids: generic_ids.clone(),
                            field_types,
                        },
                    )),
                );
            }
            ParsedItemKind::TupleStructDeclaration {
                name,
                generic_names,
                field_types,

                id,
                generic_ids,
                ..
            } => {
                let id = id.unwrap();
                let generic_ids = generic_ids.as_ref().unwrap();

                ctx.enter_scope();

                for (generic_id, generic_name) in generic_ids
                    .iter()
                    .copied()
                    .zip(generic_names.iter().cloned())
                {
                    ctx.declare_generic(generic_id, Visibility::Public, generic_name);
                }

                let field_types = field_types
                    .iter()
                    .cloned()
                    .map(|field_type| field_type.perform_semantic_analysis(ctx))
                    .collect();

                ctx.exit_scope();

                ctx.declare_semantic_type(
                    id,
                    Visibility::Public,
                    SemanticTypeDeclarationKind::Struct(SemanticStructDeclaration::Tuple(
                        SemanticTupleStructDeclaration {
                            name: name.clone(),
                            generic_ids: generic_ids.clone(),
                            field_types,
                        },
                    )),
                );
            }
            ParsedItemKind::FunctionDeclaration {
                recursive_keyword_span,
                runtime_keyword_span,
                name,
                generic_names,
                parameters,
                return_type,
                id,
                generic_ids,
                ..
            } => {
                let id = id.unwrap();
                let generic_ids = generic_ids.as_ref().unwrap();

                ctx.enter_scope();

                for (generic_id, generic_name) in generic_ids
                    .iter()
                    .copied()
                    .zip(generic_names.iter().cloned())
                {
                    ctx.declare_semantic_type(
                        generic_id.into(),
                        Visibility::Public,
                        SemanticTypeDeclarationKind::Generic(generic_name),
                    );
                }

                let parameters = parameters
                    .iter()
                    .map(|(_, parameter_type)| {
                        let parameter_type = parameter_type.clone().perform_semantic_analysis(ctx);

                        (None, parameter_type)
                    })
                    .collect::<Vec<_>>();

                let return_type = return_type.clone().perform_semantic_analysis(ctx);

                ctx.exit_scope();

                let modifiers = if runtime_keyword_span.is_some() {
                    RegularFunctionModifiers::Runtime {
                        recursive: recursive_keyword_span.is_some(),
                    }
                } else {
                    RegularFunctionModifiers::None
                };

                ctx.declare_regular_function(
                    id.into(),
                    self.visibility,
                    modifiers,
                    name.clone(),
                    generic_ids.clone(),
                    parameters,
                    return_type,
                );
            }
            _ => {}
        }
    }

    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticItem> {
        Some(match self.kind {
            ParsedItemKind::InherentImplementationItem {
                associated_items, ..
            } => {
                ctx.enter_scope();

                let _associated_items = associated_items
                    .into_iter()
                    .map(|item| item.perform_semantic_analysis(ctx))
                    .collect_option_all::<Vec<_>>();

                ctx.exit_scope();

                SemanticItem::InherentImplementation
            }
            ParsedItemKind::ModuleDeclaration { name, items, .. } => {
                let mut failed = false;

                ctx.enter_module(name);

                for item in items {
                    if item.perform_semantic_analysis(ctx).is_none() {
                        failed = true;
                    }
                }

                ctx.exit_module();

                if failed {
                    return None;
                }

                SemanticItem::ModuleDeclaration
            }
            ParsedItemKind::FunctionDeclaration {
                recursive_keyword_span,
                runtime_keyword_span,
                generic_names,
                parameters,
                return_type,
                body,
                id,
                generic_ids,
                ..
            } => {
                let id = id.unwrap();
                let generic_ids = generic_ids.as_ref().unwrap();

                ctx.enter_scope();

                for (generic_id, generic_name) in generic_ids
                    .iter()
                    .copied()
                    .zip(generic_names.iter().cloned())
                {
                    ctx.declare_semantic_type(
                        generic_id.into(),
                        Visibility::Public,
                        SemanticTypeDeclarationKind::Generic(generic_name),
                    );
                }

                let parameter_types = parameters
                    .iter()
                    .map(|(_, parameter_type)| {
                        parameter_type.clone().perform_semantic_analysis(ctx)
                    })
                    .collect::<Vec<_>>();

                let return_type = return_type.perform_semantic_analysis(ctx);

                let mut failed = false;

                if let Some(runtime_keyword_span) = runtime_keyword_span {
                    let all_types_are_runtime = {
                        let parameter_types_are_runtime =
                            parameter_types.iter().all(|parameter_type| {
                                !matches!(parameter_type.is_compiletime(), Some(true))
                            });

                        let return_type_is_runtime =
                            !matches!(return_type.is_compiletime(), Some(true));

                        parameter_types_are_runtime && return_type_is_runtime
                    };

                    if !all_types_are_runtime {
                        ctx.add_error_unit(
                            runtime_keyword_span,
                            SemanticAnalysisError::FunctionTypesNotAllRuntime,
                        );

                        failed = true;
                    }

                    if let Some(recursive_keyword_span) = recursive_keyword_span {
                        let all_types_are_data = {
                            let parameter_types_valid =
                                parameter_types.iter().all(|parameter_type| {
                                    matches!(parameter_type, SemanticDataType::Data(..))
                                });

                            let return_type_valid =
                                matches!(return_type, SemanticDataType::Data(..));

                            parameter_types_valid && return_type_valid
                        };

                        if !all_types_are_data {
                            ctx.add_error_unit(
                                recursive_keyword_span,
                                SemanticAnalysisError::FunctionTypesNotAllData,
                            );

                            failed = true;
                        }
                    }
                } else if let Some(recursive_keyword_span) = recursive_keyword_span {
                    ctx.add_error_unit(
                        recursive_keyword_span,
                        SemanticAnalysisError::RecursiveFunctionNotRuntime,
                    );

                    failed = true;
                }

                let modifiers = if runtime_keyword_span.is_some() {
                    RegularFunctionModifiers::Runtime {
                        recursive: recursive_keyword_span.is_some(),
                    }
                } else {
                    RegularFunctionModifiers::None
                };

                let scope = ctx.exit_scope();

                if failed {
                    return None;
                }

                ctx.push_scope(scope);

                ctx.function_contexts.push(FunctionContext::Regular {
                    modifiers,
                    return_type,
                    callee_id: id,
                    calls: HashSet::new(),
                });

                let mut resolved_parameters = Vec::with_capacity(parameters.len());
                let mut resolved_all_parameters = true;

                for ((pattern, _), data_type) in parameters.into_iter().zip(parameter_types) {
                    let Some(resolved_pattern) = pattern.perform_semantic_analysis(ctx, &data_type)
                    else {
                        resolved_all_parameters = false;

                        continue;
                    };

                    if resolved_all_parameters {
                        resolved_parameters.push((resolved_pattern, data_type));
                    }
                }

                let after_body_analysis = |ctx: &mut SemanticAnalysisContext| {
                    let context = ctx.function_contexts.pop().unwrap();

                    ctx.exit_scope();

                    ctx.semantic_environment
                        .update_regular_function(id, |declaration| {
                            if let FunctionContext::Regular { calls, .. } = &context {
                                declaration.calls.clone_from(calls);
                            }
                        });

                    context
                };

                let Some((body_span, tail_expression_span, body)) =
                    body.perform_semantic_analysis(ctx)
                else {
                    after_body_analysis(ctx);

                    return None;
                };

                let context = after_body_analysis(ctx);

                if !body.kind.definitely_diverges() {
                    body.data_type.assert_equals(
                        ctx,
                        tail_expression_span.unwrap_or(body_span),
                        context.return_type(),
                    )?;
                }

                if resolved_all_parameters {
                    ctx.semantic_environment
                        .update_regular_function(id, |declaration| {
                            declaration.parameters = resolved_parameters
                                .into_iter()
                                .map(|(pattern, data_type)| (Some(pattern), data_type))
                                .collect();
                            declaration.body = Some(Box::new(body));
                        });
                } else {
                    return None;
                }

                SemanticItem::FunctionDeclaration
            }
            ParsedItemKind::MinecraftFunctionDeclaration {
                resource_location,
                body,
            } => {
                ctx.function_contexts.push(FunctionContext::MCFunction);

                let (body_span, tail_expression_span, body) =
                    body.perform_semantic_analysis(ctx)?;

                ctx.function_contexts.pop();

                body.data_type.assert_equals(
                    ctx,
                    tail_expression_span.unwrap_or(body_span),
                    &SemanticDataType::Unit,
                )?;

                SemanticItem::MinecraftFunctionDeclaration(resource_location, body)
            }
            ParsedItemKind::TypeAliasDeclaration { .. } => SemanticItem::TypeAliasDeclaration,
            ParsedItemKind::RegularStructDeclaration { .. } => {
                SemanticItem::RegularStructDeclaration
            }
            ParsedItemKind::TupleStructDeclaration { .. } => SemanticItem::TupleStructDeclaration,
            ParsedItemKind::Use(..) => SemanticItem::Use,
        })
    }
}
