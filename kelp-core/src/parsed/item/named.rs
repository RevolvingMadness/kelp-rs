use std::collections::{HashMap, HashSet};

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    parsed::{
        data_type::ParsedDataType,
        environment::r#type::{ParsedTypeDeclaration, ParsedTypeDeclarationKind},
        expression::block::ParsedBlockExpression,
        item::ParsedSelfFunctionParameter,
        pattern::ParsedPattern,
        semantic_analysis::{
            FunctionContext, RegularFunctionModifiers, SemanticAnalysisContext,
            info::error::SemanticAnalysisError, scope::Scope,
        },
        use_tree::UseTree,
    },
    path::regular::Path,
    semantic::{
        data_type::SemanticDataType,
        environment::{
            implementation::SemanticImplementation,
            r#type::{
                HighGenericId, HighTypeId, SemanticTypeDeclarationKind,
                alias::SemanticTypeAliasDeclaration,
                module::HighModuleId,
                r#struct::{
                    SemanticStructDeclaration, regular::SemanticRegularStructDeclaration,
                    tuple::SemanticTupleStructDeclaration,
                },
            },
            value::function::regular::HighRegularFunctionId,
        },
        item::SemanticItem,
    },
    span::Span,
    trait_ext::CollectOptionAllIterExt as _,
    visibility::Visibility,
};

fn prepend_path_to_tree(tree: &mut UseTree, prefix: &Path) {
    match tree {
        UseTree::Path(path) | UseTree::Wildcard(path) | UseTree::As(path, _, _) => {
            let mut new_segments = prefix.segments.clone();
            new_segments.append(&mut path.segments);
            path.segments = new_segments;
        }
        UseTree::Group(nested_prefix, _) => match nested_prefix {
            Some(new_path) => {
                let mut new_segments = prefix.segments.clone();
                new_segments.append(&mut new_path.segments);
                new_path.segments = new_segments;
            }
            None => {
                *nested_prefix = Some(prefix.clone());
            }
        },
    }
}

fn resolve_use_tree(tree: &UseTree, ctx: &mut SemanticAnalysisContext) -> Option<()> {
    match tree {
        UseTree::Path(path) => {
            let last_segment = path.segments.last().unwrap();
            let type_result = ctx.try_get_visible_type(path);
            if let Ok(id) = type_result {
                ctx.declare_type_in_current_scope(last_segment.span, last_segment.name.clone(), id);
            }

            let value_result = ctx.try_get_visible_value(path);
            if let Ok(id) = value_result {
                ctx.declare_value_in_current_scope(
                    last_segment.span,
                    last_segment.name.clone(),
                    id,
                );
            }

            if let Err((type_span, type_error)) = type_result
                && value_result.is_err()
            {
                ctx.add_error_unit(type_span, type_error);

                return None;
            }
        }
        UseTree::Wildcard(path) => {
            let id = match ctx.try_get_visible_type(path) {
                Ok(id) => id,
                Err((span, error)) => return ctx.add_error(span, error),
            };

            let ParsedTypeDeclaration {
                kind: ParsedTypeDeclarationKind::Module(module),
                ..
            } = ctx.parsed_environment.get_type(id).clone()
            else {
                let last_segment = path.segments.last().unwrap();
                return ctx.add_error(
                    last_segment.span,
                    SemanticAnalysisError::NotAModule(last_segment.name.clone()),
                );
            };

            for (name, id) in module.types {
                ctx.declare_type_if_not_defined(name, id);
            }
            for (name, id) in module.values {
                ctx.declare_value_if_not_defined(name, id);
            }
        }
        UseTree::As(path, alias_span, alias) => {
            let type_result = ctx.try_get_visible_type(path);
            if let Ok(id) = type_result {
                ctx.declare_type_in_current_scope(*alias_span, alias.clone(), id);
            }

            let value_result = ctx.try_get_visible_value(path);
            if let Ok(id) = value_result {
                ctx.declare_value_in_current_scope(*alias_span, alias.clone(), id);
            }

            if let Err((type_span, type_error)) = type_result
                && value_result.is_err()
            {
                ctx.add_error_unit(type_span, type_error);

                return None;
            }
        }
        UseTree::Group(prefix, trees) => {
            for mut tree in trees.iter().cloned() {
                if let Some(prefix_path) = prefix {
                    prepend_path_to_tree(&mut tree, prefix_path);
                }

                resolve_use_tree(&tree, ctx)?;
            }
        }
    }
    Some(())
}

#[derive(Debug, Clone)]
pub enum NamedItemKind {
    InherentImplementationItem {
        generic_names: Vec<String>,
        target_type_span: Span,
        target_type: ParsedDataType,
        associated_items: Vec<NamedItem>,

        self_type_id: HighTypeId,
        generic_ids: Vec<HighGenericId>,
        associated_items_scope: Scope,
    },
    ModuleDeclaration {
        name_span: Span,
        name: String,
        items: Vec<NamedItem>,
        id: HighModuleId,
    },
    FunctionDeclaration {
        recursive_keyword_span: Option<Span>,
        runtime_keyword_span: Option<Span>,
        name_span: Span,
        name: String,
        generic_names: Vec<String>,
        self_parameter: Option<ParsedSelfFunctionParameter>,
        parameters: Vec<(ParsedPattern, ParsedDataType)>,
        return_type: ParsedDataType,
        body: Box<ParsedBlockExpression>,

        id: HighRegularFunctionId,
        generic_ids: Vec<HighGenericId>,
    },
    MinecraftFunctionDeclaration {
        resource_location: ResourceLocation,
        body: Box<ParsedBlockExpression>,
    },
    TypeAliasDeclaration {
        name_span: Span,
        name: String,
        generic_names: Vec<String>,
        alias: ParsedDataType,

        id: HighTypeId,
        generic_ids: Vec<HighGenericId>,
    },
    RegularStructDeclaration {
        name_span: Span,
        name: String,
        generic_names: Vec<String>,
        field_types: HashMap<String, ParsedDataType>,

        id: HighTypeId,
        generic_ids: Vec<HighGenericId>,
    },
    TupleStructDeclaration {
        name_span: Span,
        name: String,
        generic_names: Vec<String>,
        field_types: Vec<ParsedDataType>,

        id: HighTypeId,
        generic_ids: Vec<HighGenericId>,
    },
    Use(UseTree),
}

#[derive(Debug, Clone)]
pub struct NamedItem {
    pub span: Span,
    pub visibility: Visibility,
    pub kind: NamedItemKind,
}

impl NamedItem {
    pub fn resolve_imports(&mut self, ctx: &mut SemanticAnalysisContext) -> Option<()> {
        match &mut self.kind {
            NamedItemKind::ModuleDeclaration { name, items, .. } => {
                ctx.enter_module(name.clone());

                for item in items {
                    item.resolve_imports(ctx);
                }

                ctx.exit_module();
            }
            NamedItemKind::Use(tree) => return resolve_use_tree(tree, ctx),
            _ => {}
        }

        Some(())
    }

    pub fn resolve_types(&self, ctx: &mut SemanticAnalysisContext) {
        match &self.kind {
            NamedItemKind::InherentImplementationItem {
                generic_names,
                target_type_span,
                target_type,
                associated_items,
                self_type_id,
                associated_items_scope,
                generic_ids,
            } => {
                ctx.enter_scope();

                for (generic_id, generic_name) in generic_ids
                    .iter()
                    .copied()
                    .zip(generic_names.iter().cloned())
                {
                    ctx.set_semantic_generic(generic_id, Visibility::Public, generic_name);
                }

                let target_type = target_type.clone().perform_semantic_analysis(ctx);

                ctx.set_semantic_type(
                    *self_type_id,
                    Visibility::Public,
                    SemanticTypeDeclarationKind::Alias(SemanticTypeAliasDeclaration {
                        name: "Self".to_owned(),
                        generic_ids: Vec::new(),
                        alias: target_type.clone(),
                    }),
                );

                ctx.push_scope(associated_items_scope.clone());

                println!("{:#?}", ctx.scopes);

                for item in associated_items {
                    item.resolve_types(ctx);
                }

                let scope = ctx.exit_scope();

                ctx.exit_scope();

                let (types, values) = scope.into_tuple();

                match target_type {
                    target_type @ SemanticDataType::Struct(id, _) => {
                        let implementation = SemanticImplementation {
                            generic_names: generic_names.clone(),
                            target_type,
                            types,
                            values,
                        };

                        ctx.semantic_environment
                            .implementations
                            .entry(id.into())
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
            NamedItemKind::ModuleDeclaration { name, items, .. } => {
                ctx.enter_module(name.clone());

                for item in items {
                    item.resolve_types(ctx);
                }

                let _ = ctx.exit_module();
            }
            NamedItemKind::FunctionDeclaration {
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
                ctx.enter_scope();

                for (generic_id, generic_name) in generic_ids
                    .iter()
                    .copied()
                    .zip(generic_names.iter().cloned())
                {
                    ctx.set_semantic_generic(generic_id, Visibility::Public, generic_name);
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
                    (*id).into(),
                    self.visibility,
                    modifiers,
                    name.clone(),
                    generic_ids.clone(),
                    parameters,
                    return_type,
                );
            }
            NamedItemKind::MinecraftFunctionDeclaration { .. } => {}
            NamedItemKind::TypeAliasDeclaration {
                name,
                generic_names,
                alias,
                id,
                generic_ids,
                ..
            } => {
                ctx.enter_scope();

                for (generic_id, generic_name) in generic_ids
                    .iter()
                    .copied()
                    .zip(generic_names.iter().cloned())
                {
                    ctx.set_semantic_generic(generic_id, Visibility::Public, generic_name);
                }

                let alias = alias.clone().perform_semantic_analysis(ctx);

                ctx.exit_scope();

                ctx.set_semantic_type(
                    *id,
                    Visibility::Public,
                    SemanticTypeDeclarationKind::Alias(SemanticTypeAliasDeclaration {
                        name: name.clone(),
                        generic_ids: generic_ids.clone(),
                        alias,
                    }),
                );
            }
            NamedItemKind::RegularStructDeclaration {
                name,
                generic_names,
                field_types,

                id,
                generic_ids,
                ..
            } => {
                ctx.enter_scope();

                for (generic_id, generic_name) in generic_ids
                    .iter()
                    .copied()
                    .zip(generic_names.iter().cloned())
                {
                    ctx.set_semantic_generic(generic_id, Visibility::Public, generic_name);
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

                ctx.set_semantic_type(
                    *id,
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
            NamedItemKind::TupleStructDeclaration {
                name,
                generic_names,
                field_types,

                id,
                generic_ids,
                ..
            } => {
                ctx.enter_scope();

                for (generic_id, generic_name) in generic_ids
                    .iter()
                    .copied()
                    .zip(generic_names.iter().cloned())
                {
                    ctx.set_semantic_generic(generic_id, Visibility::Public, generic_name);
                }

                let field_types = field_types
                    .iter()
                    .cloned()
                    .map(|field_type| field_type.perform_semantic_analysis(ctx))
                    .collect();

                ctx.exit_scope();

                ctx.set_semantic_type(
                    *id,
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
            NamedItemKind::Use(..) => {}
        }
    }

    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<SemanticItem> {
        Some(match self.kind {
            NamedItemKind::InherentImplementationItem {
                generic_names,
                target_type,
                associated_items,
                self_type_id,
                generic_ids,
                associated_items_scope,
                ..
            } => {
                ctx.enter_scope();

                for (generic_id, generic_name) in generic_ids
                    .iter()
                    .copied()
                    .zip(generic_names.iter().cloned())
                {
                    ctx.set_semantic_generic(generic_id, Visibility::Public, generic_name);
                }

                let target_type = target_type.perform_semantic_analysis(ctx);

                ctx.set_semantic_type(
                    self_type_id,
                    Visibility::Public,
                    SemanticTypeDeclarationKind::Alias(SemanticTypeAliasDeclaration {
                        name: "Self".to_owned(),
                        generic_ids: Vec::new(),
                        alias: target_type,
                    }),
                );

                ctx.push_scope(associated_items_scope);

                let _associated_items = associated_items
                    .into_iter()
                    .map(|item| item.perform_semantic_analysis(ctx))
                    .collect_option_all::<Vec<_>>();

                ctx.exit_scope();

                ctx.exit_scope();

                SemanticItem::InherentImplementation
            }
            NamedItemKind::ModuleDeclaration { name, items, .. } => {
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
            NamedItemKind::FunctionDeclaration {
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
                ctx.enter_scope();

                for (generic_id, generic_name) in generic_ids
                    .iter()
                    .copied()
                    .zip(generic_names.iter().cloned())
                {
                    ctx.set_semantic_generic(generic_id, Visibility::Public, generic_name);
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
            NamedItemKind::MinecraftFunctionDeclaration {
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
            NamedItemKind::TypeAliasDeclaration { .. } => SemanticItem::TypeAliasDeclaration,
            NamedItemKind::RegularStructDeclaration { .. } => {
                SemanticItem::RegularStructDeclaration
            }
            NamedItemKind::TupleStructDeclaration { .. } => SemanticItem::TupleStructDeclaration,
            NamedItemKind::Use(..) => SemanticItem::Use,
        })
    }
}
