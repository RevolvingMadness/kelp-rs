use std::collections::HashMap;

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    high::{
        data_type::DataType,
        environment::{
            r#type::{
                HighTypeDeclarationKind,
                alias::HighAliasDeclaration,
                r#struct::{
                    regular::HighStructStructDeclaration, tuple::HighTupleStructDeclaration,
                },
            },
            value::function::builtin::{BuiltinFunctionKind, HighBuiltinFunctionDeclaration},
        },
        expression::block::BlockExpression,
        pattern::Pattern,
        semantic_analysis::{
            FunctionContext, ResolvedItem, SemanticAnalysisContext,
            info::error::SemanticAnalysisError,
        },
        use_tree::UseTree,
    },
    low::{data_type::unresolved::UnresolvedDataType, item::Item as MiddleItem},
    span::Span,
    visibility::Visibility,
};

#[derive(Debug, Clone)]
pub enum ItemKind {
    ModuleDeclaration(Span, String, Vec<Item>),
    FunctionDeclaration {
        runtime_keyword_span: Option<Span>,
        name_span: Span,
        name: String,
        generic_names: Vec<String>,
        parameters: Vec<(Pattern, DataType)>,
        return_type: DataType,
        body: BlockExpression,
    },
    MCFNDeclaration(ResourceLocation, BlockExpression),
    TypeAliasDeclaration(Span, String, Vec<String>, DataType),
    StructStructDeclaration(Span, String, Vec<String>, HashMap<String, DataType>),
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
            ItemKind::FunctionDeclaration {
                runtime_keyword_span,
                name,
                generic_names,
                parameters,
                return_type,
                body,
                ..
            } => {
                let parameter_types = parameters
                    .iter()
                    .map(|(_, data_type)| {
                        data_type
                            .clone()
                            .resolve_partially(Some(&generic_names), ctx)
                    })
                    .collect::<Vec<_>>();

                let return_type = return_type.resolve_partially(Some(&generic_names), ctx);

                let all_types_are_runtime = {
                    let parameter_types_are_runtime = parameter_types
                        .iter()
                        .all(|t| !matches!(t.is_compiletime(), Some(true)));

                    let return_type_is_runtime =
                        !matches!(return_type.is_compiletime(), Some(true));

                    parameter_types_are_runtime && return_type_is_runtime
                };

                let is_runtime = runtime_keyword_span.is_some() && all_types_are_runtime;

                let id = ctx.declare_regular_function(
                    self.visibility,
                    is_runtime,
                    name,
                    generic_names.clone(),
                    parameter_types
                        .iter()
                        .cloned()
                        .map(|parameter_type| (None, parameter_type))
                        .collect(),
                    return_type.clone(),
                    None,
                );

                if let Some(runtime_keyword_span) = runtime_keyword_span
                    && !all_types_are_runtime
                {
                    return ctx.add_error(
                        runtime_keyword_span,
                        SemanticAnalysisError::FunctionTypesNotAllRuntime,
                    );
                }

                for generic_name in generic_names {
                    ctx.declare_type(
                        Visibility::Public,
                        HighTypeDeclarationKind::Generic(generic_name),
                    );
                }

                ctx.enter_scope();
                ctx.function_contexts.push(FunctionContext {
                    is_runtime,
                    return_type,
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

                let Some((body_span, tail_expression_span, body)) =
                    body.perform_semantic_analysis(ctx)
                else {
                    ctx.function_contexts.pop().unwrap();
                    ctx.exit_scope();

                    return None;
                };

                let context = ctx.function_contexts.pop().unwrap();
                ctx.exit_scope();

                if !body.kind.definitely_diverges() {
                    body.data_type.assert_equals(
                        ctx,
                        tail_expression_span.unwrap_or(body_span),
                        &context.return_type,
                    )?;
                }

                if resolved_all_parameters {
                    ctx.update_regular_function(id, resolved_parameters, body);
                } else {
                    return None;
                }

                MiddleItem::FunctionDeclaration
            }
            ItemKind::MCFNDeclaration(resource_location, body) => {
                let (body_span, tail_expression_span, body) =
                    body.perform_semantic_analysis(ctx)?;

                body.data_type.assert_equals(
                    ctx,
                    tail_expression_span.unwrap_or(body_span),
                    &UnresolvedDataType::Unit,
                )?;

                MiddleItem::MCFNDeclaration(resource_location, body)
            }
            ItemKind::TypeAliasDeclaration(name_span, name, generic_names, alias) => {
                if ctx.type_is_declared_in_current_scope(&name) {
                    return ctx
                        .add_error(name_span, SemanticAnalysisError::TypeAlreadyDeclared(name));
                }

                let alias = alias.resolve_partially(Some(&generic_names), ctx);

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
            ItemKind::StructStructDeclaration(name_span, name, generic_names, field_types) => {
                if ctx.type_is_declared_in_current_scope(&name) {
                    return ctx
                        .add_error(name_span, SemanticAnalysisError::TypeAlreadyDeclared(name));
                }

                let field_types = field_types
                    .into_iter()
                    .map(|(field_name, field_type)| {
                        let field_type = field_type.resolve_partially(Some(&generic_names), ctx);

                        (field_name, field_type)
                    })
                    .collect();

                ctx.declare_struct_struct(
                    self.visibility,
                    HighStructStructDeclaration {
                        name,
                        generic_names,
                        field_types,
                    },
                );

                MiddleItem::StructStructDeclaration
            }
            ItemKind::TupleStructDeclaration(name_span, name, generic_names, field_types) => {
                if ctx.type_is_declared_in_current_scope(&name) {
                    return ctx
                        .add_error(name_span, SemanticAnalysisError::TypeAlreadyDeclared(name));
                }

                let field_types = field_types
                    .into_iter()
                    .map(|field_type| field_type.resolve_partially(Some(&generic_names), ctx))
                    .collect::<Vec<_>>();

                let generic_types = generic_names
                    .iter()
                    .cloned()
                    .map(UnresolvedDataType::Generic)
                    .collect::<Vec<_>>();

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
