use std::collections::HashMap;

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    high::{
        data_type::DataType,
        environment::{
            HighImpl,
            r#type::{
                HighTypeDeclaration, HighTypeDeclarationKind, HighTypeId,
                alias::HighTypeAliasDeclaration,
                r#struct::{
                    HighStructDeclaration, regular::HighRegularStructDeclaration,
                    tuple::HighTupleStructDeclaration,
                },
            },
            value::function::regular::HighRegularFunctionId,
        },
        expression::block::BlockExpression,
        item::{
            associated::AssociatedItem, function_declaration::FunctionDeclarationItem,
            type_alias_declaration::TypeAliasDeclarationItem,
        },
        semantic_analysis::{
            FunctionContext, ResolvedItem, SemanticAnalysisContext,
            info::error::SemanticAnalysisError,
        },
        use_tree::UseTree,
    },
    low::{data_type::unresolved::UnresolvedDataType, item::Item as MiddleItem},
    span::Span,
    trait_ext::CollectOptionAllIterExt,
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
    ModuleDeclaration {
        name_span: Span,
        name: String,
        items: Vec<Item>,
        id: Option<HighTypeId>,
    },
    FunctionDeclaration(FunctionDeclarationItem),
    MinecraftFunctionDeclaration {
        resource_location: ResourceLocation,
        body: BlockExpression,
    },
    TypeAliasDeclaration(TypeAliasDeclarationItem),
    RegularStructDeclaration {
        name_span: Span,
        name: String,
        generic_names: Vec<String>,
        field_types: HashMap<String, DataType>,
        id: Option<HighTypeId>,
    },
    TupleStructDeclaration {
        name_span: Span,
        name: String,
        generic_names: Vec<String>,
        field_types: Vec<DataType>,
        id: Option<HighTypeId>,
    },
    Use(UseTree),
}

#[derive(Debug, Clone)]
pub struct Item {
    pub span: Span,
    pub visibility: Visibility,
    pub kind: ItemKind,
}

impl Item {
    pub fn resolve_names(&mut self, ctx: &mut SemanticAnalysisContext) -> Option<()> {
        match &mut self.kind {
            ItemKind::ModuleDeclaration {
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

                let id = ctx.declare_scope_type(self.visibility, name.clone());

                *type_id = Some(id);

                ctx.enter_module(name.clone());

                for item in items {
                    item.resolve_names(ctx);
                }

                ctx.exit_module_and_declare(id, self.visibility);

                Some(())
            }
            ItemKind::RegularStructDeclaration {
                name_span,
                name,
                id: type_id,
                ..
            }
            | ItemKind::TupleStructDeclaration {
                name_span,
                name,
                id: type_id,
                ..
            } => {
                if ctx.current_scope().type_is_declared(name) {
                    return ctx.add_error(
                        *name_span,
                        SemanticAnalysisError::TypeAlreadyDeclared(name.clone()),
                    );
                }

                let id = ctx.declare_scope_type(self.visibility, name.clone());

                *type_id = Some(id);

                Some(())
            }
            ItemKind::FunctionDeclaration(FunctionDeclarationItem {
                name_span,
                name,
                id: value_id,
                ..
            }) => {
                if ctx.current_scope().value_is_declared(name) {
                    return ctx.add_error(
                        *name_span,
                        SemanticAnalysisError::ValueAlreadyDeclared(name.clone()),
                    );
                }

                let id = ctx.declare_scope_value(self.visibility, name.clone());

                *value_id = Some(HighRegularFunctionId(id.0));

                Some(())
            }
            _ => Some(()),
        }
    }

    pub fn resolve_imports(&mut self, ctx: &mut SemanticAnalysisContext) -> Option<()> {
        match &mut self.kind {
            ItemKind::ModuleDeclaration { name, items, .. } => {
                ctx.enter_module(name.clone());

                for item in items {
                    item.resolve_imports(ctx);
                }

                ctx.exit_module();
            }
            ItemKind::Use(tree) => match tree {
                UseTree::Wildcard(path) => {
                    let mut path = path.clone();

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
                        ctx.declare_type_if_not_defined(Visibility::None, name, id);
                    }

                    for (name, id) in module.values {
                        ctx.declare_value_if_not_defined(Visibility::None, name, id);
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
                            kind: ItemKind::Use(tree.clone()),
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
                    let item = ctx.get_visible_item(path)?;

                    match item {
                        ResolvedItem::Type(id) => {
                            ctx.declare_type_in_current_scope(
                                Visibility::None,
                                *alias_span,
                                alias.clone(),
                                id,
                            );
                        }
                        ResolvedItem::Value(id) => {
                            ctx.declare_value_in_current_scope(
                                Visibility::None,
                                *alias_span,
                                alias.clone(),
                                id,
                            );
                        }
                    }
                }
                UseTree::Path(path) => {
                    let last_segment = path.segments.last().unwrap();
                    let last_segment_span = last_segment.span;
                    let last_segment_name = last_segment.name.clone();

                    let item = ctx.get_visible_item(path)?;

                    match item {
                        ResolvedItem::Type(id) => {
                            ctx.declare_type_in_current_scope(
                                Visibility::None,
                                last_segment_span,
                                last_segment_name,
                                id,
                            );
                        }
                        ResolvedItem::Value(id) => {
                            ctx.declare_value_in_current_scope(
                                Visibility::None,
                                last_segment_span,
                                last_segment_name,
                                id,
                            );
                        }
                    }
                }
            },
            _ => {}
        }

        Some(())
    }

    pub fn resolve_types(&mut self, ctx: &mut SemanticAnalysisContext) -> Option<()> {
        match &mut self.kind {
            ItemKind::ModuleDeclaration { name, items, .. } => {
                ctx.enter_module(name.clone());

                for item in items {
                    item.resolve_types(ctx);
                }

                let _ = ctx.exit_module();

                Some(())
            }
            ItemKind::RegularStructDeclaration {
                name,
                generic_names,
                field_types,
                id,
                ..
            } => {
                let id = id.unwrap();

                ctx.enter_scope();

                let generic_ids = generic_names
                    .iter()
                    .cloned()
                    .map(|generic_name| ctx.declare_generic(Visibility::Public, generic_name))
                    .collect::<Vec<_>>();

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

                ctx.high_environment.declare_type(
                    id,
                    HighTypeDeclaration {
                        module_path: ctx.current_module_path.clone(),
                        visibility: Visibility::Public,
                        kind: HighTypeDeclarationKind::Struct(HighStructDeclaration::Struct(
                            HighRegularStructDeclaration {
                                name: name.clone(),
                                generic_ids,
                                field_types,
                            },
                        )),
                    },
                );

                Some(())
            }
            ItemKind::TupleStructDeclaration {
                name,
                generic_names,
                field_types,
                id,
                ..
            } => {
                let id = id.unwrap();

                ctx.enter_scope();

                let generic_ids = generic_names
                    .iter()
                    .cloned()
                    .map(|generic_name| ctx.declare_generic(Visibility::Public, generic_name))
                    .collect::<Vec<_>>();

                let field_types = field_types
                    .iter()
                    .cloned()
                    .map(|field_type| field_type.perform_semantic_analysis(ctx))
                    .collect();

                ctx.exit_scope();

                ctx.high_environment.declare_type(
                    id,
                    HighTypeDeclaration {
                        module_path: ctx.current_module_path.clone(),
                        visibility: Visibility::Public,
                        kind: HighTypeDeclarationKind::Struct(HighStructDeclaration::Tuple(
                            HighTupleStructDeclaration {
                                name: name.clone(),
                                generic_ids,
                                field_types,
                            },
                        )),
                    },
                );

                Some(())
            }
            ItemKind::FunctionDeclaration(item) => item.resolve_types(ctx, self.visibility),
            _ => Some(()),
        }
    }

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
                ctx.enter_scope();

                for generic_name in generic_names.clone() {
                    ctx.declare_type_auto(
                        Visibility::Public,
                        HighTypeDeclarationKind::Generic(generic_name),
                    );
                }

                let target_type = target_type.perform_semantic_analysis(ctx);

                ctx.declare_alias(
                    Visibility::Public,
                    HighTypeAliasDeclaration {
                        name: "Self".to_owned(),
                        generic_ids: Vec::new(),
                        alias: target_type.clone(),
                    },
                );

                let _associated_items = associated_items
                    .into_iter()
                    .map(|item| item.perform_semantic_analysis(ctx, self.visibility))
                    .collect_option_all::<Vec<_>>();

                ctx.exit_scope();

                match target_type {
                    target_type @ UnresolvedDataType::Struct(id, _) => {
                        let implementation = HighImpl {
                            generic_names,
                            target_type,
                            functions: HashMap::new(),
                        };

                        ctx.high_environment
                            .impls
                            .entry(HighTypeId(id.0))
                            .or_default()
                            .push(implementation);
                    }

                    _ => {
                        return ctx.add_error(
                            target_type_span,
                            SemanticAnalysisError::InherentImplRequiresNomialType,
                        );
                    }
                }

                MiddleItem::InherentImplementation
            }
            ItemKind::ModuleDeclaration { name, items, .. } => {
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

                MiddleItem::ModuleDeclaration
            }
            ItemKind::FunctionDeclaration(item) => {
                return item.perform_semantic_analysis(ctx);
            }
            ItemKind::MinecraftFunctionDeclaration {
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
                    &UnresolvedDataType::Unit,
                )?;

                MiddleItem::MinecraftFunctionDeclaration(resource_location, body)
            }
            ItemKind::TypeAliasDeclaration(item) => {
                return item.perform_semantic_analysis(ctx, self.visibility);
            }
            ItemKind::RegularStructDeclaration { .. } => MiddleItem::RegularStructDeclaration,
            ItemKind::TupleStructDeclaration { .. } => MiddleItem::TupleStructDeclaration,
            ItemKind::Use(..) => MiddleItem::Use,
        })
    }
}
