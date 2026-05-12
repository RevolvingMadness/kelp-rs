use std::collections::{HashMap, HashSet};

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    high::{
        data_type::DataType,
        environment::{
            r#type::{
                HighTypeDeclarationKind,
                alias::HighAliasDeclaration,
                r#struct::{
                    regular::HighRegularStructDeclaration, tuple::HighTupleStructDeclaration,
                },
            },
            value::function::builtin::{BuiltinFunctionKind, HighBuiltinFunctionDeclaration},
        },
        expression::block::BlockExpression,
        item::{
            associated::AssociatedItem, function_declaration::FunctionDeclarationItem,
            type_alias_declaration::TypeAliasDeclarationItem,
        },
        semantic_analysis::{
            FunctionContext, RegularFunctionModifiers, ResolvedItem, SemanticAnalysisContext,
            info::error::SemanticAnalysisError,
        },
        use_tree::UseTree,
    },
    low::{data_type::unresolved::UnresolvedDataType, item::Item as MiddleItem},
    span::Span,
    visibility::Visibility,
};

pub mod associated;
pub mod function_declaration;
pub mod type_alias_declaration;

#[derive(Debug, Clone)]
pub enum ItemKind {
    InherentImplementationItem {
        generic_names: Vec<String>,
        target_type_span: Span,
        target_type: DataType,
        associated_items: Vec<AssociatedItem>,
    },
    ModuleDeclaration(Span, String, Vec<Item>),
    FunctionDeclaration(FunctionDeclarationItem),
    MinecraftFunctionDeclaration(ResourceLocation, BlockExpression),
    TypeAliasDeclaration(TypeAliasDeclarationItem),
    RegularStructDeclaration(Span, String, Vec<String>, HashMap<String, DataType>),
    TupleStructDeclaration(Span, String, Vec<String>, Vec<DataType>),
    Use(UseTree),
}

#[derive(Debug, Clone)]
pub struct Item {
    pub span: Span,
    pub visibility: Visibility,
    pub kind: ItemKind,
}

impl Item {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleItem> {
        Some(match self.kind {
            ItemKind::InherentImplementationItem {
                generic_names,
                target_type_span,
                target_type,
                associated_items,
            } => {
                todo!()
            }
            ItemKind::ModuleDeclaration(name_span, name, items) => {
                if ctx.type_is_declared_in_current_scope(&name) {
                    return ctx
                        .add_error(name_span, SemanticAnalysisError::TypeAlreadyDeclared(name));
                }

                let mut error = false;

                ctx.enter_module(name);

                for item in items {
                    if item.perform_semantic_analysis(ctx).is_none() {
                        error = true;
                    }
                }

                ctx.exit_module_and_declare(self.visibility);

                if error {
                    return None;
                }

                MiddleItem::ModuleDeclaration
            }
            ItemKind::FunctionDeclaration(FunctionDeclarationItem {
                recursive_keyword_span,
                runtime_keyword_span,
                name_span: _,
                name,
                generic_names,
                parameters,
                return_type,
                body,
            }) => {
                ctx.enter_scope();

                for generic_name in generic_names.clone() {
                    ctx.declare_type(
                        Visibility::Public,
                        HighTypeDeclarationKind::Generic(generic_name),
                    );
                }

                let parameter_types = parameters
                    .iter()
                    .map(|(_, data_type)| data_type.clone().perform_semantic_analysis(ctx))
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
                                    matches!(parameter_type, UnresolvedDataType::Data(..))
                                });

                            let return_type_valid =
                                matches!(return_type, UnresolvedDataType::Data(..));

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

                let id = ctx.declare_regular_function(
                    self.visibility,
                    modifiers,
                    name,
                    generic_names,
                    parameter_types
                        .iter()
                        .cloned()
                        .map(|parameter_type| (None, parameter_type))
                        .collect(),
                    return_type.clone(),
                );

                if failed {
                    return None;
                }

                ctx.push_scope(scope);

                let mut calls = HashSet::new();

                calls.insert(id.into());

                ctx.function_contexts.push(FunctionContext::Regular {
                    modifiers,
                    return_type,
                    calls,
                });

                let mut resolved_parameters = Vec::with_capacity(parameters.len());
                let mut resolved_all_parameters = true;

                for ((pattern, _), data_type) in
                    parameters.into_iter().zip(parameter_types.into_iter())
                {
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

                    ctx.high_environment
                        .update_regular_function(id, |declaration| {
                            if let Some(calls) = context.get_calls() {
                                declaration.calls = Some(calls.clone());
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
                    ctx.high_environment
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

                MiddleItem::FunctionDeclaration
            }
            ItemKind::MinecraftFunctionDeclaration(resource_location, body) => {
                ctx.function_contexts.push(FunctionContext::MCFunction);

                let (body_span, tail_expression_span, body) =
                    body.perform_semantic_analysis(ctx)?;

                ctx.function_contexts.pop();

                body.data_type.assert_equals(
                    ctx,
                    tail_expression_span.unwrap_or(body_span),
                    &UnresolvedDataType::Unit,
                )?;

                MiddleItem::MinecraftFunctionDeclaration(resource_location, body)
            }
            ItemKind::TypeAliasDeclaration(TypeAliasDeclarationItem {
                name_span,
                name,
                generic_names,
                alias,
            }) => {
                if ctx.type_is_declared_in_current_scope(&name) {
                    return ctx
                        .add_error(name_span, SemanticAnalysisError::TypeAlreadyDeclared(name));
                }

                ctx.enter_scope();

                for generic_name in generic_names.clone() {
                    ctx.declare_type(
                        Visibility::Public,
                        HighTypeDeclarationKind::Generic(generic_name),
                    );
                }

                let alias = alias.perform_semantic_analysis(ctx);

                ctx.exit_scope();

                ctx.declare_alias(
                    self.visibility,
                    HighAliasDeclaration {
                        name,
                        generic_names,
                        alias,
                    },
                );

                MiddleItem::TypeAliasDeclaration
            }
            ItemKind::RegularStructDeclaration(name_span, name, generic_names, field_types) => {
                if ctx.type_is_declared_in_current_scope(&name) {
                    return ctx
                        .add_error(name_span, SemanticAnalysisError::TypeAlreadyDeclared(name));
                }

                ctx.enter_scope();

                for generic_name in generic_names.clone() {
                    ctx.declare_type(
                        Visibility::Public,
                        HighTypeDeclarationKind::Generic(generic_name),
                    );
                }

                let field_types = field_types
                    .into_iter()
                    .map(|(field_name, field_type)| {
                        let field_type = field_type.perform_semantic_analysis(ctx);

                        (field_name, field_type)
                    })
                    .collect();

                ctx.exit_scope();

                ctx.declare_regular_struct(
                    self.visibility,
                    HighRegularStructDeclaration {
                        name,
                        generic_names,
                        field_types,
                    },
                );

                MiddleItem::RegularStructDeclaration
            }
            ItemKind::TupleStructDeclaration(name_span, name, generic_names, field_types) => {
                if ctx.type_is_declared_in_current_scope(&name) {
                    return ctx
                        .add_error(name_span, SemanticAnalysisError::TypeAlreadyDeclared(name));
                }

                ctx.enter_scope();

                for generic_name in generic_names.clone() {
                    ctx.declare_type(
                        Visibility::Public,
                        HighTypeDeclarationKind::Generic(generic_name),
                    );
                }

                let field_types = field_types
                    .into_iter()
                    .map(|field_type| field_type.perform_semantic_analysis(ctx))
                    .collect::<Vec<_>>();

                let generic_types = generic_names
                    .iter()
                    .cloned()
                    .map(UnresolvedDataType::Generic)
                    .collect::<Vec<_>>();

                ctx.enter_scope();

                let id = ctx.declare_tuple_struct(
                    self.visibility,
                    HighTupleStructDeclaration {
                        name: name.clone(),
                        generic_names: generic_names.clone(),
                        field_types: field_types.clone(),
                    },
                );

                ctx.declare_builtin_function(
                    Visibility::Public,
                    HighBuiltinFunctionDeclaration {
                        name,
                        generic_names,
                        parameters: field_types,
                        return_type: UnresolvedDataType::Struct(id.into(), generic_types.clone()),
                        kind: BuiltinFunctionKind::TupleConstructor(id, generic_types),
                    },
                );

                MiddleItem::TupleStructDeclaration
            }
            ItemKind::Use(tree) => {
                match tree {
                    UseTree::Wildcard(mut path) => {
                        let ResolvedItem::Type(id) = ctx.get_visible_item(&path)? else {
                            let last_segment = path.segments.pop().unwrap();

                            return ctx.add_error(
                                last_segment.span,
                                SemanticAnalysisError::NotAType(last_segment.name),
                            );
                        };

                        let declaration = ctx.get_type(id).clone();

                        let (_, _, HighTypeDeclarationKind::Module(module)) =
                            declaration.as_tuple_owned()
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
                                kind: ItemKind::Use(tree),
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
                        let item = ctx.get_visible_item(&path)?;

                        match item {
                            ResolvedItem::Type(id) => {
                                ctx.declare_type_in_current_scope(alias_span, alias, id);
                            }
                            ResolvedItem::Value(id) => {
                                ctx.declare_value_in_current_scope(alias_span, alias, id);
                            }
                        }
                    }
                    UseTree::Path(path) => {
                        let last_segment = path.segments.last().unwrap();
                        let last_segment_span = last_segment.span;
                        let last_segment_name = last_segment.name.clone();

                        let item = ctx.get_visible_item(&path)?;

                        match item {
                            ResolvedItem::Type(id) => {
                                ctx.declare_type_in_current_scope(
                                    last_segment_span,
                                    last_segment_name,
                                    id,
                                );
                            }
                            ResolvedItem::Value(id) => {
                                ctx.declare_value_in_current_scope(
                                    last_segment_span,
                                    last_segment_name,
                                    id,
                                );
                            }
                        }
                    }
                }

                MiddleItem::Use
            }
        })
    }
}
