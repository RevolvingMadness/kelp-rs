use std::collections::HashMap;

use minecraft_command_types::resource_location::ResourceLocation;

use crate::parsed::environment::{
    r#type::alias::ParsedTypeAliasDeclaration,
    r#type::r#struct::{
        regular::ParsedRegularStructDeclaration, tuple::ParsedTupleStructDeclaration,
        ParsedStructDeclaration,
    },
    r#type::ParsedTypeDeclaration,
    r#type::ParsedTypeDeclarationKind,
};
use crate::semantic::environment::{
    r#type::{
        r#struct::{
            regular::SemanticRegularStructDeclaration, tuple::SemanticTupleStructDeclaration,
            SemanticStructDeclaration,
        }, HighGenericId, HighTypeId,
        SemanticTypeDeclarationKind,
    },
    HighImpl,
};
use crate::{
    parsed::{
        data_type::ParsedDataType,
        expression::block::ParsedBlockExpression,
        item::{
            associated::AssociatedItem, function_declaration::FunctionDeclarationItem,
            type_alias_declaration::TypeAliasDeclarationItem,
        },
        semantic_analysis::{
            info::error::SemanticAnalysisError, scope::Scope, FunctionContext,
            SemanticAnalysisContext,
        },
        use_tree::UseTree,
    },
    span::Span,
    trait_ext::CollectOptionAllIterExt,
    semantic::item::SemanticItem as MiddleItem,
    visibility::Visibility,
};
use crate::semantic::data_type::SemanticDataType;

pub mod associated;
pub mod function_declaration;
pub mod type_alias_declaration;

#[derive(Debug, Clone)]
pub enum ItemKind {
    InherentImplementationItem {
        generic_names: Vec<String>,
        target_type_span: Span,
        target_type: ParsedDataType,
        associated_items: Vec<AssociatedItem>,

        scope: Option<Scope>,
    },
    ModuleDeclaration {
        name_span: Span,
        name: String,
        items: Vec<ParsedItem>,
        id: Option<HighTypeId>,
    },
    FunctionDeclaration(FunctionDeclarationItem),
    MinecraftFunctionDeclaration {
        resource_location: ResourceLocation,
        body: ParsedBlockExpression,
    },
    TypeAliasDeclaration(TypeAliasDeclarationItem),
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
    pub kind: ItemKind,
}

impl ParsedItem {
    pub fn resolve_names(&mut self, ctx: &mut SemanticAnalysisContext) -> Option<()> {
        match &mut self.kind {
            ItemKind::InherentImplementationItem {
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
                        let id = ctx.declare_unresolved_type(
                            Visibility::Public,
                            ParsedTypeDeclarationKind::Generic(generic_name),
                        );

                        HighGenericId(id.0)
                    })
                    .collect::<Vec<_>>();

                ctx.declare_unresolved_type(
                    Visibility::Public,
                    ParsedTypeDeclarationKind::Alias(ParsedTypeAliasDeclaration {
                        name: "Self".to_owned(),
                        generic_ids,
                    }),
                );

                let _associated_items = associated_items
                    .iter_mut()
                    .map(|item| item.resolve_names(ctx, self.visibility))
                    .collect_option_all::<Vec<_>>();

                let scope = ctx.exit_scope();

                *this_scope = Some(scope);

                Some(())
            }
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

                ctx.enter_module(name.clone());

                for item in items {
                    item.resolve_names(ctx);
                }

                let id = ctx.exit_module_and_declare(self.visibility);

                *type_id = Some(id);

                Some(())
            }
            ItemKind::RegularStructDeclaration {
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
                        let id = ctx.declare_unresolved_type(
                            Visibility::Public,
                            ParsedTypeDeclarationKind::Generic(generic_name),
                        );

                        HighGenericId(id.0)
                    })
                    .collect::<Vec<_>>();

                *this_generic_ids = Some(generic_ids.clone());

                ctx.exit_scope();

                let id = ctx.declare_unresolved_type(
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
            ItemKind::TupleStructDeclaration {
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
                        let id = ctx.declare_unresolved_type(
                            Visibility::Public,
                            ParsedTypeDeclarationKind::Generic(generic_name),
                        );

                        HighGenericId(id.0)
                    })
                    .collect::<Vec<_>>();

                *type_generic_ids = Some(generic_ids.clone());

                ctx.exit_scope();

                let id = ctx.declare_unresolved_type(
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
            ItemKind::FunctionDeclaration(item) => item.resolve_names(ctx, self.visibility),
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

                    let id = match ctx.try_get_visible_type(&path) {
                        Ok(id) => id,
                        Err((span, error)) => return ctx.add_error(span, error),
                    };

                    let ParsedTypeDeclaration {
                        kind: ParsedTypeDeclarationKind::Module(module),
                        ..
                    } = ctx.get_unresolved_type(id).clone()
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

    pub fn resolve_types(&mut self, ctx: &mut SemanticAnalysisContext) -> Option<()> {
        match &mut self.kind {
            ItemKind::InherentImplementationItem {
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
                    item.resolve_types(ctx, self.visibility);
                }
                let scope = ctx.exit_scope();
                let (types, values) = scope.clone().into_tuple();
                *this_scope = Some(scope);

                let target_type = target_type.clone().perform_semantic_analysis(ctx);

                match target_type {
                    target_type @ SemanticDataType::Struct(id, _) => {
                        let implementation = HighImpl {
                            generic_names: generic_names.clone(),
                            target_type,
                            types,
                            values,
                        };

                        ctx.resolved_environment
                            .impls
                            .entry(HighTypeId(id.0))
                            .or_default()
                            .push(implementation);
                    }

                    SemanticDataType::Error => return None,

                    _ => {
                        return ctx.add_error(
                            *target_type_span,
                            SemanticAnalysisError::InherentImplRequiresNomialType,
                        );
                    }
                }

                Some(())
            }
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

                ctx.declare_resolved_type(
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

                Some(())
            }
            ItemKind::TupleStructDeclaration {
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

                ctx.declare_resolved_type(
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

                Some(())
            }
            ItemKind::FunctionDeclaration(item) => {
                item.resolve_types(ctx, self.visibility);

                Some(())
            }
            _ => Some(()),
        }
    }

    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleItem> {
        Some(match self.kind {
            ItemKind::InherentImplementationItem {
                associated_items, ..
            } => {
                ctx.enter_scope();

                let _associated_items = associated_items
                    .into_iter()
                    .map(|item| item.perform_semantic_analysis(ctx))
                    .collect_option_all::<Vec<_>>();

                ctx.exit_scope();

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
                    &SemanticDataType::Unit,
                )?;

                MiddleItem::MinecraftFunctionDeclaration(resource_location, body)
            }
            ItemKind::TypeAliasDeclaration(..) => MiddleItem::TypeAliasDeclaration,
            ItemKind::RegularStructDeclaration { .. } => MiddleItem::RegularStructDeclaration,
            ItemKind::TupleStructDeclaration { .. } => MiddleItem::TupleStructDeclaration,
            ItemKind::Use(..) => MiddleItem::Use,
        })
    }
}
