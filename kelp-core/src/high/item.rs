use std::collections::HashMap;

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    high::{
        data_type::unresolved::UnresolvedDataType,
        environment::r#type::{
            HighTypeDeclaration, alias::HighAliasDeclaration, module::HighModuleDeclaration,
            r#struct::HighStructDeclaration,
        },
        semantic_analysis_context::{
            ResolvedItem, SemanticAnalysisContext, info::error::SemanticAnalysisError,
        },
        statement::Statement,
        use_tree::UseTree,
    },
    middle::item::Item as MiddleItem,
    span::Span,
};

#[derive(Debug, Clone)]
pub enum Item {
    ModuleDeclaration(Span, String, Vec<Self>),
    MCFNDeclaration(ResourceLocation, Box<Statement>),
    TypeAliasDeclaration(Span, String, Vec<String>, UnresolvedDataType),
    StructDeclaration(
        Span,
        String,
        Vec<String>,
        HashMap<String, UnresolvedDataType>,
    ),
    Use(UseTree),
}

impl Item {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleItem> {
        Some(match self {
            Self::ModuleDeclaration(name_span, name, items) => {
                if ctx.type_is_declared_in_current_scope(&name) {
                    return ctx
                        .add_error(name_span, SemanticAnalysisError::TypeAlreadyDeclared(name));
                }

                ctx.start_scope();

                let mut error = false;

                for item in items {
                    if item.perform_semantic_analysis(ctx).is_none() {
                        error = true;
                    }
                }

                let module_scope = ctx.end_scope();

                ctx.declare_data_type(HighTypeDeclaration::Module(HighModuleDeclaration {
                    name,
                    types: module_scope.types,
                    values: module_scope.values,
                }));

                if error {
                    return None;
                }

                MiddleItem::ModuleDeclaration
            }
            Self::MCFNDeclaration(resource_location, statement) => {
                let statement = statement.perform_semantic_analysis(ctx)?;

                MiddleItem::MCFNDeclaration(resource_location, Box::new(statement))
            }
            Self::TypeAliasDeclaration(name_span, name, generic_names, alias) => {
                if ctx.type_is_declared_in_current_scope(&name) {
                    return ctx
                        .add_error(name_span, SemanticAnalysisError::TypeAlreadyDeclared(name));
                }

                let alias = alias.resolve_partially(Some(&generic_names), ctx);

                ctx.declare_data_type(HighTypeDeclaration::Alias(HighAliasDeclaration {
                    name,
                    generic_names,
                    alias,
                }));

                MiddleItem::TypeAliasDeclaration
            }
            Self::StructDeclaration(name_span, name, generic_names, field_types) => {
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
                    .collect::<HashMap<_, _>>();

                ctx.declare_data_type(HighTypeDeclaration::Struct(HighStructDeclaration {
                    name,
                    generic_names,
                    field_types,
                }));

                MiddleItem::StructDeclaration
            }
            Self::Use(tree) => {
                match tree {
                    UseTree::Wildcard(mut path) => {
                        let ResolvedItem::Type(id) = ctx.resolve_item_path(&path)? else {
                            let last_segment = path.segments.pop().unwrap();

                            return ctx.add_error(
                                last_segment.span,
                                SemanticAnalysisError::NotAType(last_segment.name),
                            );
                        };

                        let declaration = ctx.get_type(id).clone();

                        let HighTypeDeclaration::Module(module) = declaration else {
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

                            let item = Self::Use(tree);

                            if item.perform_semantic_analysis(ctx).is_none() {
                                has_error = true;
                            }
                        }

                        if has_error {
                            return None;
                        }
                    }
                    UseTree::As(path, alias_span, alias) => {
                        let item = ctx.resolve_item_path(&path)?;

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

                        let item = ctx.resolve_item_path(&path)?;

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
