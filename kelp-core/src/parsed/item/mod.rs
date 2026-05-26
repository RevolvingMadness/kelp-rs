use std::collections::HashMap;

use la_arena::Idx;
use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    parsed::arena::ParsedAstArena,
    parsed::{
        data_type::ParsedDataType,
        environment::r#type::{
            ParsedTypeDeclaration, ParsedTypeDeclarationKind,
            alias::ParsedTypeAliasDeclaration,
            r#struct::{
                ParsedStructDeclaration, regular::ParsedRegularStructDeclaration,
                tuple::ParsedTupleStructDeclaration,
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
    path::regular::Path,
    span::Span,
    trait_ext::CollectOptionAllIterExt as _,
    typed::arena::TypedAstArena,
    typed::{
        data_type::SemanticDataType,
        environment::{
            HighImpl,
            r#type::{
                HighGenericId, SemanticTypeDeclarationKind,
                r#struct::{
                    SemanticStructDeclaration, regular::SemanticRegularStructDeclaration,
                    tuple::SemanticTupleStructDeclaration,
                },
            },
        },
        item::TypedItem as MiddleItem,
    },
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
pub enum ParsedItem {
    InherentImplementationItem {
        generic_names: Vec<String>,
        target_type_span: Span,
        target_type: ParsedDataType,
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
        field_types: HashMap<String, ParsedDataType>,
    },
    TupleStructDeclaration {
        name_span: Span,
        name: String,
        generic_names: Vec<String>,
        field_types: Vec<ParsedDataType>,
    },
    Use(UseTree),
}

impl ParsedItem {
    pub fn resolve_names(
        id: Idx<Self>,
        arena: &ParsedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<()> {
        match &arena.items[id] {
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

                let associated_items = associated_items
                    .iter()
                    .copied()
                    .map(|item| Self::resolve_names(item, arena, ctx))
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
                    Self::resolve_names(item, arena, ctx);
                }

                let visibility = arena.get_item_visiblity(id);

                let type_id = ctx.exit_module_and_declare(visibility);

                ctx.declare_item_type_id(id, type_id);

                Some(())
            }
            Self::FunctionDeclaration(item) => {
                let visibility = arena.get_item_visiblity(id);

                item.resolve_names(id, ctx, visibility)
            }
            Self::MinecraftFunctionDeclaration { .. } => Some(()),
            Self::TypeAliasDeclaration(item) => {
                let visibility = arena.get_item_visiblity(id);

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
                        let id = ctx.declare_parsed_type(
                            Visibility::Public,
                            ParsedTypeDeclarationKind::Generic(generic_name),
                        );

                        HighGenericId(id.0)
                    })
                    .collect::<Vec<_>>();

                ctx.exit_scope();

                ctx.declare_item_generic_ids(id, generic_ids.clone());

                let visibility = arena.get_item_visiblity(id);

                let type_id = ctx.declare_parsed_type(
                    visibility,
                    ParsedTypeDeclarationKind::Struct(ParsedStructDeclaration::Struct(
                        ParsedRegularStructDeclaration::new(name, generic_ids),
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
                        let id = ctx.declare_parsed_type(
                            Visibility::Public,
                            ParsedTypeDeclarationKind::Generic(generic_name),
                        );

                        HighGenericId(id.0)
                    })
                    .collect::<Vec<_>>();

                ctx.exit_scope();

                ctx.declare_item_generic_ids(id, generic_ids.clone());

                let visibility = arena.get_item_visiblity(id);

                let type_id = ctx.declare_parsed_type(
                    visibility,
                    ParsedTypeDeclarationKind::Struct(ParsedStructDeclaration::Tuple(
                        ParsedTupleStructDeclaration::new(name, generic_ids),
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
        arena: &ParsedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<()> {
        match arena.get_item(id) {
            Self::ModuleDeclaration { name, items, .. } => {
                ctx.enter_module(name.clone());

                for item in items.iter().copied() {
                    Self::resolve_imports(item, arena, ctx);
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
        parsed_arena: &ParsedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<()> {
        match parsed_arena.get_item(id) {
            Self::InherentImplementationItem {
                associated_items, ..
            } => {
                let scope = ctx.get_item_scope(id).clone();

                ctx.push_scope(scope);
                for item in associated_items.iter().copied() {
                    Self::resolve_types(item, parsed_arena, ctx);
                }
                ctx.exit_scope();

                Some(())
            }
            Self::ModuleDeclaration { name, items, .. } => {
                ctx.enter_module(name.clone());

                for item in items.iter().copied() {
                    Self::resolve_types(item, parsed_arena, ctx);
                }

                let _ = ctx.exit_module();

                Some(())
            }
            Self::FunctionDeclaration(..) => Some(()),
            Self::MinecraftFunctionDeclaration { .. } => Some(()),
            Self::TypeAliasDeclaration(item) => {
                let visibility = parsed_arena.get_item_visiblity(id);

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

                ctx.declare_semantic_type(
                    type_id,
                    Visibility::Public,
                    SemanticTypeDeclarationKind::Struct(SemanticStructDeclaration::Struct(
                        SemanticRegularStructDeclaration {
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

                ctx.declare_semantic_type(
                    type_id,
                    Visibility::Public,
                    SemanticTypeDeclarationKind::Struct(SemanticStructDeclaration::Tuple(
                        SemanticTupleStructDeclaration {
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
        parsed_arena: &ParsedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<()> {
        match parsed_arena.get_item(id) {
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
                    Self::resolve_value_types(item, parsed_arena, ctx);
                }
                let scope = ctx.exit_scope();
                let (types, values) = scope.into_tuple();

                let target_type = target_type.clone().perform_semantic_analysis(ctx);

                match target_type {
                    target_type @ SemanticDataType::Struct(id, _) => {
                        let implementation = HighImpl {
                            generic_names: generic_names.clone(),
                            target_type,
                            types,
                            values,
                        };

                        ctx.semantic_environment
                            .impls
                            .entry(id.into())
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
            Self::ModuleDeclaration { name, items, .. } => {
                ctx.enter_module(name.clone());

                for item in items.iter().copied() {
                    Self::resolve_value_types(item, parsed_arena, ctx);
                }

                let _ = ctx.exit_module();

                Some(())
            }
            Self::FunctionDeclaration(item) => {
                let visibility = parsed_arena.get_item_visiblity(id);

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
        parsed_arena: &ParsedAstArena,
        typed_arena: &mut TypedAstArena,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<Idx<MiddleItem>> {
        Some(match parsed_arena.get_item(id) {
            Self::InherentImplementationItem {
                associated_items, ..
            } => {
                ctx.enter_scope();

                let _associated_items = associated_items
                    .iter()
                    .copied()
                    .map(|item| {
                        Self::perform_semantic_analysis(item, parsed_arena, typed_arena, ctx)
                    })
                    .collect_option_all::<Vec<_>>();

                ctx.exit_scope();

                typed_arena.allocate_item(MiddleItem::InherentImplementation)
            }
            Self::ModuleDeclaration { name, items, .. } => {
                let mut failed = false;

                ctx.enter_module(name.clone());

                for item in items.iter().copied() {
                    if Self::perform_semantic_analysis(item, parsed_arena, typed_arena, ctx)
                        .is_none()
                    {
                        failed = true;
                    }
                }

                ctx.exit_module();

                if failed {
                    return None;
                }

                typed_arena.allocate_item(MiddleItem::ModuleDeclaration)
            }
            Self::FunctionDeclaration(item) => {
                return item.perform_semantic_analysis(id, parsed_arena, typed_arena, ctx);
            }
            Self::MinecraftFunctionDeclaration {
                resource_location,
                body,
            } => {
                ctx.function_contexts.push(FunctionContext::MCFunction);

                let (body_span, tail_expression_span, body) = body
                    .clone()
                    .perform_semantic_analysis(parsed_arena, typed_arena, ctx)?;

                ctx.function_contexts.pop();

                let body_type = typed_arena.get_expression_type(body);

                body_type.assert_equals(
                    ctx,
                    tail_expression_span.unwrap_or(body_span),
                    &SemanticDataType::Unit,
                )?;

                typed_arena.allocate_item(MiddleItem::MinecraftFunctionDeclaration(
                    resource_location.clone(),
                    body,
                ))
            }
            Self::TypeAliasDeclaration(..) => {
                typed_arena.allocate_item(MiddleItem::TypeAliasDeclaration)
            }
            Self::RegularStructDeclaration { .. } => {
                typed_arena.allocate_item(MiddleItem::RegularStructDeclaration)
            }
            Self::TupleStructDeclaration { .. } => {
                typed_arena.allocate_item(MiddleItem::TupleStructDeclaration)
            }
            Self::Use(..) => typed_arena.allocate_item(MiddleItem::Use),
        })
    }
}
