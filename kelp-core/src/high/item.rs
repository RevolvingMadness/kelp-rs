use std::collections::HashMap;

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    high::{
        data_type::unresolved::UnresolvedDataType,
        environment::r#type::{
            HighTypeDeclarationKind,
            alias::HighAliasDeclaration,
            r#struct::{HighStructStructDeclaration, HighTupleStructDeclaration},
        },
        expression::block::BlockExpression,
        pattern::Pattern,
        semantic_analysis::{
            ResolvedItem, SemanticAnalysisContext, info::error::SemanticAnalysisError,
        },
        use_tree::UseTree,
    },
    low::{data_type::DataType, item::Item as MiddleItem},
    span::Span,
    visibility::Visibility,
};

#[derive(Debug, Clone)]
pub enum ItemKind {
    ModuleDeclaration(Span, String, Vec<Item>),
    FunctionDeclaration {
        name_span: Span,
        name: String,
        generic_names: Vec<String>,
        parameters: Vec<(Pattern, UnresolvedDataType)>,
        return_type: UnresolvedDataType,
        body: BlockExpression,
    },
    MCFNDeclaration(ResourceLocation, BlockExpression),
    TypeAliasDeclaration(Span, String, Vec<String>, UnresolvedDataType),
    StructStructDeclaration(
        Span,
        String,
        Vec<String>,
        HashMap<String, UnresolvedDataType>,
    ),
    TupleStructDeclaration(Span, String, Vec<String>, Vec<UnresolvedDataType>),
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

                let id = ctx.declare_regular_function(
                    self.visibility,
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

                for generic_name in generic_names {
                    ctx.declare_type(
                        Visibility::Public,
                        HighTypeDeclarationKind::Generic(generic_name),
                    );
                }

                ctx.enter_scope();
                ctx.function_return_types.push(return_type);

                let mut resolved_parameters = Vec::with_capacity(parameters.len());
                let mut resolved_all_parameters = true;

                for ((pattern, _), data_type) in
                    parameters.into_iter().zip(parameter_types.into_iter())
                {
                    let Some(data_type) = data_type else {
                        resolved_all_parameters = false;

                        pattern.kind.destructure_unknown(ctx);

                        continue;
                    };

                    let Some(resolved_pattern) = pattern.perform_semantic_analysis(ctx, &data_type)
                    else {
                        resolved_all_parameters = false;

                        continue;
                    };

                    if resolved_all_parameters {
                        resolved_parameters.push((resolved_pattern, data_type));
                    }
                }

                let Some(body) = body.perform_semantic_analysis(ctx) else {
                    ctx.function_return_types.pop().unwrap();
                    ctx.exit_scope();

                    return None;
                };

                let return_type = ctx.function_return_types.pop().unwrap();
                ctx.exit_scope();

                if let Some(return_type) = &return_type
                    && let (body_span, tail_expression_span, body) = &body
                    && !body.kind.definitely_diverges()
                {
                    body.data_type.assert_equals(
                        ctx,
                        tail_expression_span.unwrap_or(*body_span),
                        return_type,
                    )?;
                }

                if resolved_all_parameters {
                    ctx.update_regular_function(id, resolved_parameters, body.2);
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
                    &DataType::Unit,
                )?;

                MiddleItem::MCFNDeclaration(resource_location, body)
            }
            ItemKind::TypeAliasDeclaration(name_span, name, generic_names, alias) => {
                if ctx.type_is_declared_in_current_scope(&name) {
                    return ctx
                        .add_error(name_span, SemanticAnalysisError::TypeAlreadyDeclared(name));
                }

                let alias = alias.resolve_partially(Some(&generic_names), ctx)?;

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
                    .collect();

                ctx.declare_tuple_struct(
                    self.visibility,
                    HighTupleStructDeclaration {
                        name,
                        generic_names,
                        field_types,
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
