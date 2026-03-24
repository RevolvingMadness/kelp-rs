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
        expression::Expression,
        semantic_analysis_context::{
            ResolvedItem, SemanticAnalysisContext, info::error::SemanticAnalysisError,
        },
        use_tree::UseTree,
    },
    middle::item::Item as MiddleItem,
    span::Span,
    visibility::Visibility,
};

#[derive(Debug, Clone)]
pub enum ItemKind {
    ModuleDeclaration(Span, String, Vec<Item>),
    MCFNDeclaration(ResourceLocation, Expression),
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
            ItemKind::MCFNDeclaration(resource_location, body) => {
                let (_, body) = body.perform_semantic_analysis(ctx)?;

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
                        let last_segment = path.segments.pop().unwrap();

                        let ResolvedItem::Type(id) = ctx.get_visible_item(&path)? else {
                            return ctx.add_error(
                                last_segment.span,
                                SemanticAnalysisError::NotAType(last_segment.name),
                            );
                        };

                        let declaration = ctx.get_type(id).clone();

                        let (_, _, HighTypeDeclarationKind::Module(module)) =
                            declaration.as_tuple_owned()
                        else {
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
