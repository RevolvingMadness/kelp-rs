use std::collections::HashMap;

use la_arena::Idx;
use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    ast_allocator::{high::HighAstAllocator, low::LowAstAllocator},
    high::{
        data_type::DataType,
        environment::{
            resolved::{
                HighImpl,
                r#type::{
                    HighGenericId, HighTypeId, ResolvedTypeDeclarationKind,
                    r#struct::{
                        ResolvedStructDeclaration, regular::ResolvedRegularStructDeclaration,
                        tuple::ResolvedTupleStructDeclaration,
                    },
                },
            },
            unresolved::r#type::{
                UnresolvedTypeDeclaration, UnresolvedTypeDeclarationKind,
                alias::UnresolvedTypeAliasDeclaration,
                r#struct::{
                    UnresolvedStructDeclaration, regular::UnresolvedRegularStructDeclaration,
                    tuple::UnresolvedTupleStructDeclaration,
                },
            },
        },
        expression::block::BlockExpression,
        item::{
            function_declaration::FunctionDeclarationItem,
            type_alias_declaration::TypeAliasDeclarationItem,
        },
        semantic_analysis::{
            FunctionContext, SemanticAnalysisContext, info::error::SemanticAnalysisError,
        },
        use_tree::UseTree,
    },
    low::{data_type::unresolved::UnresolvedDataType, item::Item as MiddleItem},
    path::regular::Path,
    span::Span,
    trait_ext::CollectOptionAllIterExt as _,
    visibility::Visibility,
};

pub mod function_declaration;
pub mod type_alias_declaration;

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

            let UnresolvedTypeDeclaration {
                kind: UnresolvedTypeDeclarationKind::Module(module),
                ..
            } = ctx.get_unresolved_type(id).clone()
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
pub enum Item {
    InherentImplementationItem {
        generic_names: Vec<String>,
        target_type_span: Span,
        target_type: DataType,
        associated_items: Vec<Idx<Self>>,
    },
    ModuleDeclaration {
        name_span: Span,
        name: String,
        items: Vec<Idx<Self>>,
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
    },
    TupleStructDeclaration {
        name_span: Span,
        name: String,
        generic_names: Vec<String>,
        field_types: Vec<DataType>,
    },
    Use(UseTree),
}

impl Item {
    pub fn resolve_names(
        id: Idx<Self>,
        allocator: &HighAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<()> {
        match &allocator.items[id] {
            Self::InherentImplementationItem {
                generic_names,
                associated_items,
                ..
            } => {
                ctx.enter_scope();

                let generic_ids = generic_names
                    .iter()
                    .cloned()
                    .map(|generic_name| {
                        let id = ctx.declare_unresolved_type(
                            Visibility::Public,
                            UnresolvedTypeDeclarationKind::Generic(generic_name),
                        );

                        HighGenericId(id.0)
                    })
                    .collect::<Vec<_>>();

                ctx.declare_unresolved_type(
                    Visibility::Public,
                    UnresolvedTypeDeclarationKind::Alias(UnresolvedTypeAliasDeclaration {
                        name: "Self".to_owned(),
                        generic_ids,
                    }),
                );

                let associated_items = associated_items
                    .iter()
                    .copied()
                    .map(|item| Self::resolve_names(item, allocator, ctx))
                    .collect_option_all::<Vec<_>>();

                let scope = ctx.exit_scope();

                let _associated_items = associated_items?;

                ctx.declare_item_scope(id, scope);

                Some(())
            }
            Self::ModuleDeclaration {
                name_span,
                name,
                items,
                ..
            } => {
                if ctx.current_scope().type_is_declared(name) {
                    return ctx.add_error(
                        *name_span,
                        SemanticAnalysisError::TypeAlreadyDeclared(name.clone()),
                    );
                }

                ctx.enter_module(name.clone());

                for item in items.iter().copied() {
                    Self::resolve_names(item, allocator, ctx);
                }

                let visibility = allocator.get_item_visiblity(id);

                let type_id = ctx.exit_module_and_declare(visibility);

                ctx.declare_item_type_id(id, type_id);

                Some(())
            }
            Self::FunctionDeclaration(item) => {
                let visibility = allocator.get_item_visiblity(id);

                item.resolve_names(id, ctx, visibility)
            }
            Self::MinecraftFunctionDeclaration { .. } => Some(()),
            Self::TypeAliasDeclaration(item) => {
                let visibility = allocator.get_item_visiblity(id);

                item.resolve_names(id, ctx, visibility)
            }
            Self::RegularStructDeclaration {
                name_span,
                name,
                generic_names,
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
                            UnresolvedTypeDeclarationKind::Generic(generic_name),
                        );

                        HighGenericId(id.0)
                    })
                    .collect::<Vec<_>>();

                ctx.exit_scope();

                ctx.declare_item_generic_ids(id, generic_ids.clone());

                let visibility = allocator.get_item_visiblity(id);

                let type_id = ctx.declare_unresolved_type(
                    visibility,
                    UnresolvedTypeDeclarationKind::Struct(UnresolvedStructDeclaration::Struct(
                        UnresolvedRegularStructDeclaration::new(name, generic_ids),
                    )),
                );

                ctx.declare_item_type_id(id, type_id);

                Some(())
            }
            Self::TupleStructDeclaration {
                name_span,
                name,
                generic_names,
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
                            UnresolvedTypeDeclarationKind::Generic(generic_name),
                        );

                        HighGenericId(id.0)
                    })
                    .collect::<Vec<_>>();

                ctx.exit_scope();

                ctx.declare_item_generic_ids(id, generic_ids.clone());

                let visibility = allocator.get_item_visiblity(id);

                let type_id = ctx.declare_unresolved_type(
                    visibility,
                    UnresolvedTypeDeclarationKind::Struct(UnresolvedStructDeclaration::Tuple(
                        UnresolvedTupleStructDeclaration::new(name, generic_ids),
                    )),
                );

                ctx.declare_item_type_id(id, type_id);

                Some(())
            }
            Self::Use(..) => Some(()),
        }
    }

    pub fn resolve_imports(
        id: Idx<Self>,
        allocator: &HighAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<()> {
        match allocator.get_item(id) {
            Self::ModuleDeclaration { name, items, .. } => {
                ctx.enter_module(name.clone());

                for item in items.iter().copied() {
                    Self::resolve_imports(item, allocator, ctx);
                }

                ctx.exit_module();
            }
            Self::Use(tree) => {
                return resolve_use_tree(tree, ctx);
            }
            _ => {}
        }

        Some(())
    }

    pub fn resolve_types(
        id: Idx<Self>,
        high_allocator: &HighAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<()> {
        match high_allocator.get_item(id) {
            Self::InherentImplementationItem {
                associated_items, ..
            } => {
                let scope = ctx.get_item_scope(id).clone();

                ctx.push_scope(scope);
                for item in associated_items.iter().copied() {
                    Self::resolve_types(item, high_allocator, ctx);
                }
                ctx.exit_scope();

                Some(())
            }
            Self::ModuleDeclaration { name, items, .. } => {
                ctx.enter_module(name.clone());

                for item in items.iter().copied() {
                    Self::resolve_types(item, high_allocator, ctx);
                }

                let _ = ctx.exit_module();

                Some(())
            }
            Self::FunctionDeclaration(..) => Some(()),
            Self::MinecraftFunctionDeclaration { .. } => Some(()),
            Self::TypeAliasDeclaration(item) => {
                let visibility = high_allocator.get_item_visiblity(id);

                item.resolve_types(id, ctx, visibility);

                Some(())
            }
            Self::RegularStructDeclaration {
                name,
                generic_names,
                field_types,
                ..
            } => {
                let type_id = ctx.get_item_type_id(id);
                let generic_ids = ctx.get_item_generic_ids(id).to_vec();

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
                    type_id,
                    Visibility::Public,
                    ResolvedTypeDeclarationKind::Struct(ResolvedStructDeclaration::Struct(
                        ResolvedRegularStructDeclaration {
                            name: name.clone(),
                            generic_ids,
                            field_types,
                        },
                    )),
                );

                Some(())
            }
            Self::TupleStructDeclaration {
                name,
                generic_names,
                field_types,
                ..
            } => {
                let type_id = ctx.get_item_type_id(id);
                let generic_ids = ctx.get_item_generic_ids(id).to_vec();

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
                    type_id,
                    Visibility::Public,
                    ResolvedTypeDeclarationKind::Struct(ResolvedStructDeclaration::Tuple(
                        ResolvedTupleStructDeclaration {
                            name: name.clone(),
                            generic_ids,
                            field_types,
                        },
                    )),
                );

                Some(())
            }
            Self::Use(..) => Some(()),
        }
    }

    pub fn resolve_value_types(
        id: Idx<Self>,
        high_allocator: &HighAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<()> {
        match high_allocator.get_item(id) {
            Self::InherentImplementationItem {
                generic_names,
                target_type_span,
                target_type,
                associated_items,
                ..
            } => {
                let scope = ctx.get_item_scope(id).clone();

                ctx.push_scope(scope);
                for item in associated_items.iter().copied() {
                    Self::resolve_value_types(item, high_allocator, ctx);
                }
                let scope = ctx.exit_scope();
                let (types, values) = scope.into_tuple();

                let target_type = target_type.clone().perform_semantic_analysis(ctx);

                match target_type {
                    target_type @ UnresolvedDataType::Struct(id, _) => {
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

                    UnresolvedDataType::Error => return None,

                    _ => {
                        return ctx.add_error(
                            *target_type_span,
                            SemanticAnalysisError::InherentImplRequiresNomialType,
                        );
                    }
                }

                Some(())
            }
            Self::ModuleDeclaration { name, items, .. } => {
                ctx.enter_module(name.clone());

                for item in items.iter().copied() {
                    Self::resolve_value_types(item, high_allocator, ctx);
                }

                let _ = ctx.exit_module();

                Some(())
            }
            Self::FunctionDeclaration(item) => {
                let visibility = high_allocator.get_item_visiblity(id);

                item.resolve_types(id, ctx, visibility);

                Some(())
            }
            Self::MinecraftFunctionDeclaration { .. } => Some(()),
            Self::TypeAliasDeclaration(..) => Some(()),
            Self::RegularStructDeclaration { .. } => Some(()),
            Self::TupleStructDeclaration { .. } => Some(()),
            Self::Use(..) => Some(()),
        }
    }

    pub fn perform_semantic_analysis(
        id: Idx<Self>,
        high_allocator: &HighAstAllocator,
        low_allocator: &mut LowAstAllocator,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<Idx<MiddleItem>> {
        Some(match high_allocator.get_item(id) {
            Self::InherentImplementationItem {
                associated_items, ..
            } => {
                ctx.enter_scope();

                let _associated_items = associated_items
                    .iter()
                    .copied()
                    .map(|item| {
                        Self::perform_semantic_analysis(item, high_allocator, low_allocator, ctx)
                    })
                    .collect_option_all::<Vec<_>>();

                ctx.exit_scope();

                low_allocator.allocate_item(MiddleItem::InherentImplementation)
            }
            Self::ModuleDeclaration { name, items, .. } => {
                let mut failed = false;

                ctx.enter_module(name.clone());

                for item in items.iter().copied() {
                    if Self::perform_semantic_analysis(item, high_allocator, low_allocator, ctx)
                        .is_none()
                    {
                        failed = true;
                    }
                }

                ctx.exit_module();

                if failed {
                    return None;
                }

                low_allocator.allocate_item(MiddleItem::ModuleDeclaration)
            }
            Self::FunctionDeclaration(item) => {
                return item.perform_semantic_analysis(id, high_allocator, low_allocator, ctx);
            }
            Self::MinecraftFunctionDeclaration {
                resource_location,
                body,
            } => {
                ctx.function_contexts.push(FunctionContext::MCFunction);

                let (body_span, tail_expression_span, body) = body
                    .clone()
                    .perform_semantic_analysis(high_allocator, low_allocator, ctx)?;

                ctx.function_contexts.pop();

                let body_type = low_allocator.get_expression_type(body);

                body_type.assert_equals(
                    ctx,
                    tail_expression_span.unwrap_or(body_span),
                    &UnresolvedDataType::Unit,
                )?;

                low_allocator.allocate_item(MiddleItem::MinecraftFunctionDeclaration(
                    resource_location.clone(),
                    body,
                ))
            }
            Self::TypeAliasDeclaration(..) => {
                low_allocator.allocate_item(MiddleItem::TypeAliasDeclaration)
            }
            Self::RegularStructDeclaration { .. } => {
                low_allocator.allocate_item(MiddleItem::RegularStructDeclaration)
            }
            Self::TupleStructDeclaration { .. } => {
                low_allocator.allocate_item(MiddleItem::TupleStructDeclaration)
            }
            Self::Use(..) => low_allocator.allocate_item(MiddleItem::Use),
        })
    }
}
