use std::collections::{HashMap, HashSet};

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    parsed::{
        data_type::ParsedDataType,
        environment::r#type::{ParsedTypeDeclaration, ParsedTypeDeclarationKind},
        expression::block::ParsedBlockExpression,
        item::{ParsedSelfFunctionParameter, typed::TypedItem},
        pattern::{ParsedPattern, ParsedPatternKind},
        semantic_analysis::{
            RegularFunctionModifiers, SemanticAnalysisContext, info::error::SemanticAnalysisError,
            scope::Scope,
        },
        use_tree::UseTree,
    },
    path::{generic::TypedPath, regular::Path},
    semantic::{
        data_type::SemanticDataType,
        environment::{
            implementation::SemanticImplementation,
            r#type::{
                HighGenericId, HighTypeId, SemanticTypeDeclarationKind,
                alias::SemanticTypeAliasDeclaration,
                module::HighModuleId,
                r#struct::{
                    SemanticStructDeclaration,
                    regular::SemanticRegularStructDeclaration,
                    tuple::{HighTupleStructId, SemanticTupleStructDeclaration},
                },
            },
            value::{
                SemanticValueDeclarationKind,
                function::{
                    SemanticFunctionDeclaration,
                    builtin::{
                        HighBuiltinFunctionId, SemanticBuiltinFunctionDeclaration,
                        SemanticBuiltinFunctionKind,
                    },
                    regular::{HighRegularFunctionId, SemanticRegularFunctionDeclaration},
                },
            },
        },
    },
    span::Span,
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
                ctx.declare_type_in_current_scope(
                    last_segment.span,
                    last_segment.name.clone(),
                    id.into(),
                );
            }

            let value_result = ctx.try_get_visible_value(path);
            if let Ok(id) = value_result {
                ctx.declare_value_in_current_scope(
                    last_segment.span,
                    last_segment.name.clone(),
                    id.into(),
                );
            }

            if let Err(type_error) = type_result
                && value_result.is_err()
            {
                ctx.add_error_unit(type_error);

                return None;
            }
        }
        UseTree::Wildcard(path) => {
            let id = match ctx.try_get_visible_type(path) {
                Ok(id) => id,
                Err(error) => return ctx.add_error(error),
            };

            let declaration = ctx.parsed_environment.get_type(id).clone();

            let type_kind = declaration.kind.get_type_kind();

            let ParsedTypeDeclaration {
                kind: ParsedTypeDeclarationKind::Module(module),
                ..
            } = declaration
            else {
                let last_segment = path.segments.last().unwrap();
                return ctx.add_error(SemanticAnalysisError::NotAModule {
                    span: last_segment.span,
                    type_kind,
                });
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
                ctx.declare_type_in_current_scope(*alias_span, alias.clone(), id.into());
            }

            let value_result = ctx.try_get_visible_value(path);
            if let Ok(id) = value_result {
                ctx.declare_value_in_current_scope(*alias_span, alias.clone(), id.into());
            }

            if let Err(type_error) = type_result
                && value_result.is_err()
            {
                ctx.add_error_unit(type_error);

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
pub struct TypedSelfFunctionParameter {
    pub pattern_span: Span,
    pub data_type_span: Span,
    pub data_type: SemanticDataType,
}

#[derive(Debug, Clone)]
pub enum NamedItemKind {
    InherentImplementationItem {
        generics: Vec<(Span, String)>,
        target_type_span: Span,
        target_type: ParsedDataType,
        associated_items: Vec<NamedItem>,

        self_type_id: HighTypeId,
        generic_ids: Vec<HighGenericId>,
        associated_items_scope: Scope,
    },
    ModuleDeclaration {
        id: HighModuleId,
        items: Vec<NamedItem>,
    },
    FunctionDeclaration {
        recursive_keyword_span: Option<Span>,
        runtime_keyword_span: Option<Span>,
        name_span: Span,
        name: String,
        generic_ids: Vec<HighGenericId>,
        generics: Vec<(Span, String)>,
        self_parameter: Option<ParsedSelfFunctionParameter>,
        parameters: Vec<(ParsedPattern, ParsedDataType)>,
        return_type: ParsedDataType,
        body: Box<ParsedBlockExpression>,

        id: HighRegularFunctionId,
    },
    MinecraftFunctionDeclaration {
        resource_location: ResourceLocation,
        body: Box<ParsedBlockExpression>,
    },
    TypeAliasDeclaration {
        name_span: Span,
        name: String,
        generic_ids: Vec<HighGenericId>,
        generics: Vec<(Span, String)>,
        alias: ParsedDataType,

        id: HighTypeId,
    },
    RegularStructDeclaration {
        name_span: Span,
        name: String,
        generic_ids: Vec<HighGenericId>,
        generics: Vec<(Span, String)>,
        field_types: HashMap<String, ParsedDataType>,

        id: HighTypeId,
    },
    TupleStructDeclaration {
        name_span: Span,
        name: String,
        generic_ids: Vec<HighGenericId>,
        generics: Vec<(Span, String)>,
        field_types: Vec<ParsedDataType>,

        id: HighTupleStructId,
        constructor_id: HighBuiltinFunctionId,
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
            NamedItemKind::ModuleDeclaration { id, items, .. } => {
                ctx.enter_semantic_module(*id);

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

    #[must_use]
    pub fn resolve_types(self, ctx: &mut SemanticAnalysisContext) -> TypedItem {
        match self.kind {
            NamedItemKind::InherentImplementationItem {
                generics,
                target_type_span,
                target_type,
                associated_items,
                self_type_id,
                associated_items_scope,
                generic_ids,
            } => {
                ctx.enter_scope();

                for (generic_id, (generic_span, generic_name)) in
                    generic_ids.iter().copied().zip(generics.iter().cloned())
                {
                    ctx.set_semantic_generic(
                        generic_id,
                        Visibility::Public,
                        generic_span,
                        generic_name,
                    );
                }

                let target_type = target_type.perform_semantic_analysis(ctx);

                ctx.set_semantic_type(
                    self_type_id,
                    Visibility::Public,
                    SemanticTypeDeclarationKind::Alias(SemanticTypeAliasDeclaration {
                        name_span: target_type_span,
                        name: "Self".to_owned(),
                        generic_ids: Vec::new(),
                        alias: target_type.clone(),
                    }),
                );

                ctx.enter_semantic_implementation(associated_items_scope, generic_ids);

                let associated_items = associated_items
                    .into_iter()
                    .map(|item| item.resolve_types(ctx))
                    .collect();

                let (generic_ids, associated_items_scope) = ctx.exit_semantic_implementation();

                ctx.exit_scope();

                let (types, values) = associated_items_scope.clone().into_tuple();

                match &target_type {
                    target_type @ SemanticDataType::Struct(id, _) => {
                        let implementation = SemanticImplementation::new(
                            generics
                                .iter()
                                .map(|(_, generic_name)| generic_name.clone())
                                .collect(),
                            target_type.clone(),
                            types,
                            values,
                        );

                        ctx.semantic_environment
                            .add_implementation(*id, implementation);
                    }

                    SemanticDataType::Error => {}

                    _ => {
                        ctx.add_error_unit(SemanticAnalysisError::InherentImplRequiresNomialType {
                            type_span: target_type_span,
                            data_type: target_type.clone(),
                        });
                    }
                }

                TypedItem::InherentImplementationItem {
                    generics,
                    target_type_span,
                    target_type,
                    associated_items,
                    self_type_id,
                    generic_ids,
                    associated_items_scope,
                }
            }
            NamedItemKind::ModuleDeclaration { id, items } => {
                ctx.enter_semantic_module(id);

                let items = items
                    .into_iter()
                    .map(|item| item.resolve_types(ctx))
                    .collect();

                ctx.exit_module();

                TypedItem::ModuleDeclaration { id, items }
            }
            NamedItemKind::FunctionDeclaration {
                recursive_keyword_span,
                runtime_keyword_span,
                name_span,
                name,
                mut generic_ids,
                generics,
                self_parameter,
                parameters,
                return_type,
                body,
                id,
            } => {
                ctx.enter_scope();

                for (generic_id, (generic_span, generic_name)) in
                    generic_ids.iter().copied().zip(generics.iter().cloned())
                {
                    ctx.set_semantic_generic(
                        generic_id,
                        Visibility::Public,
                        generic_span,
                        generic_name,
                    );
                }

                let declared_generic_count = generic_ids.len();

                if let Some(impl_generic_ids) = ctx.impl_generic_ids.last() {
                    generic_ids.splice(0..0, impl_generic_ids.iter().copied());
                }

                let self_parameter = self_parameter.map(|parameter| {
                    let data_type = if ctx.impl_generic_ids.is_empty() {
                        ctx.add_error_type(SemanticAnalysisError::MethodNotInImpl {
                            method_name_span: name_span,
                            self_keyword_span: parameter.pattern_span,
                        })
                    } else {
                        parameter.data_type.perform_semantic_analysis(ctx)
                    };

                    TypedSelfFunctionParameter {
                        pattern_span: parameter.pattern_span,
                        data_type_span: parameter.pattern_span,
                        data_type,
                    }
                });

                let is_method = self_parameter.is_some();

                let mut parameters = parameters
                    .into_iter()
                    .map(|(pattern, parameter_type)| {
                        let parameter_type = parameter_type.perform_semantic_analysis(ctx);

                        (pattern, parameter_type)
                    })
                    .collect::<Vec<_>>();

                if let Some(self_parameter) = &self_parameter {
                    let self_pattern = ParsedPatternKind::Binding(TypedPath::single(
                        self_parameter.pattern_span,
                        "self",
                    ))
                    .with_span(self_parameter.pattern_span);

                    parameters.insert(0, (self_pattern, self_parameter.data_type.clone()));
                }

                let return_type = return_type.perform_semantic_analysis(ctx);

                ctx.exit_scope();

                let modifiers = if runtime_keyword_span.is_some() {
                    RegularFunctionModifiers::Runtime {
                        recursive: recursive_keyword_span.is_some(),
                    }
                } else {
                    RegularFunctionModifiers::None
                };

                ctx.set_semantic_value(
                    id,
                    self.visibility,
                    SemanticValueDeclarationKind::Function(Box::new(
                        SemanticFunctionDeclaration::Regular(SemanticRegularFunctionDeclaration {
                            name_span,
                            name,
                            modifiers,
                            declared_generic_count,
                            generic_ids: generic_ids.clone(),
                            is_method,
                            parameters: parameters
                                .iter()
                                .map(|(_, data_type)| (None, data_type.clone()))
                                .collect(),
                            return_type: return_type.clone(),
                            body: None,
                            calls: HashSet::new(),
                        }),
                    )),
                );

                TypedItem::FunctionDeclaration {
                    recursive_keyword_span,
                    runtime_keyword_span,
                    generic_ids,
                    generics,
                    name_span,
                    parameters,
                    return_type,
                    body,
                    id,
                }
            }
            NamedItemKind::MinecraftFunctionDeclaration {
                resource_location,
                body,
            } => TypedItem::MinecraftFunctionDeclaration {
                resource_location,
                body,
            },
            NamedItemKind::TypeAliasDeclaration {
                name_span,
                name,
                generics,
                alias,
                id,
                generic_ids,
            } => {
                ctx.enter_scope();

                for (generic_id, (generic_span, generic_name)) in
                    generic_ids.iter().copied().zip(generics)
                {
                    ctx.set_semantic_generic(
                        generic_id,
                        Visibility::Public,
                        generic_span,
                        generic_name,
                    );
                }

                let alias = alias.perform_semantic_analysis(ctx);

                ctx.exit_scope();

                ctx.set_semantic_type(
                    id,
                    Visibility::Public,
                    SemanticTypeDeclarationKind::Alias(SemanticTypeAliasDeclaration {
                        name_span,
                        name,
                        generic_ids,
                        alias,
                    }),
                );

                TypedItem::TypeAliasDeclaration
            }
            NamedItemKind::RegularStructDeclaration {
                name_span,
                name,
                generics: generic_names,
                field_types,
                id,
                generic_ids,
            } => {
                ctx.enter_scope();

                for (generic_id, (generic_span, generic_name)) in generic_ids
                    .iter()
                    .copied()
                    .zip(generic_names.iter().cloned())
                {
                    ctx.set_semantic_generic(
                        generic_id,
                        Visibility::Public,
                        generic_span,
                        generic_name,
                    );
                }

                let field_types = field_types
                    .into_iter()
                    .map(|(field_name, field_type)| {
                        let field_type = field_type.perform_semantic_analysis(ctx);

                        (field_name, field_type)
                    })
                    .collect::<HashMap<_, _>>();

                ctx.exit_scope();

                ctx.set_semantic_type(
                    id,
                    Visibility::Public,
                    SemanticTypeDeclarationKind::Struct(SemanticStructDeclaration::Struct(
                        SemanticRegularStructDeclaration {
                            name_span,
                            name,
                            generic_ids,
                            field_types,
                        },
                    )),
                );

                TypedItem::RegularStructDeclaration
            }
            NamedItemKind::TupleStructDeclaration {
                name_span,
                name,
                generics,
                field_types,
                id,
                generic_ids,
                constructor_id,
            } => {
                ctx.enter_scope();

                for (generic_id, (generic_span, generic_name)) in
                    generic_ids.iter().copied().zip(generics)
                {
                    ctx.set_semantic_generic(
                        generic_id,
                        Visibility::Public,
                        generic_span,
                        generic_name,
                    );
                }

                let field_types = field_types
                    .into_iter()
                    .map(|field_type| field_type.perform_semantic_analysis(ctx))
                    .collect::<Vec<_>>();

                ctx.exit_scope();

                ctx.set_semantic_type(
                    id,
                    Visibility::Public,
                    SemanticTypeDeclarationKind::Struct(SemanticStructDeclaration::Tuple(
                        SemanticTupleStructDeclaration {
                            name_span,
                            name: name.clone(),
                            generic_ids: generic_ids.clone(),
                            field_types: field_types.clone(),
                        },
                    )),
                );

                ctx.set_semantic_value(
                    constructor_id,
                    Visibility::Public,
                    SemanticValueDeclarationKind::Function(Box::new(
                        SemanticFunctionDeclaration::Builtin(SemanticBuiltinFunctionDeclaration {
                            name,
                            generic_ids: generic_ids.clone(),
                            parameters: field_types,
                            return_type: SemanticDataType::Struct(
                                id.into(),
                                generic_ids
                                    .iter()
                                    .copied()
                                    .map(SemanticDataType::Generic)
                                    .collect(),
                            ),
                            kind: SemanticBuiltinFunctionKind::TupleStructConstructor(
                                id,
                                generic_ids.clone(),
                            ),
                        }),
                    )),
                );

                TypedItem::TupleStructDeclaration
            }
            NamedItemKind::Use(..) => TypedItem::Use,
        }
    }
}
