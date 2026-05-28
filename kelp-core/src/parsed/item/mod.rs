use std::collections::HashMap;

use minecraft_command_types::resource_location::ResourceLocation;

use crate::parsed::environment::value::ParsedValueDeclarationKind;
use crate::parsed::environment::value::function::ParsedFunctionDeclaration;
use crate::parsed::environment::value::function::regular::ParsedRegularFunctionDeclaration;
use crate::parsed::environment::{
    r#type::{
        ParsedTypeDeclarationKind,
        alias::ParsedTypeAliasDeclaration,
        r#struct::{
            ParsedStructDeclaration, regular::ParsedRegularStructDeclaration,
            tuple::ParsedTupleStructDeclaration,
        },
    },
    value::function::builtin::ParsedBuiltinFunctionDeclaration,
};
use crate::parsed::item::named::{NamedItem, NamedItemKind};
use crate::parsed::pattern::ParsedPattern;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::HighGenericId;
use crate::semantic::environment::r#type::r#struct::tuple::HighTupleStructId;
use crate::semantic::environment::value::function::builtin::HighBuiltinFunctionId;
use crate::semantic::environment::value::function::regular::HighRegularFunctionId;
use crate::{
    parsed::{
        data_type::ParsedDataType,
        expression::block::ParsedBlockExpression,
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
        use_tree::UseTree,
    },
    span::Span,
    trait_ext::CollectOptionAllIterExt,
    visibility::Visibility,
};

pub mod named;
pub mod typed;

#[derive(Debug, Clone)]
pub struct TypedSelfFunctionParameter {
    pub pattern_span: Span,
    pub data_type_span: Span,
    pub data_type: SemanticDataType,
}

#[derive(Debug, Clone)]
pub struct ParsedSelfFunctionParameter {
    pub pattern_span: Span,
    pub data_type_span: Span,
    pub data_type: ParsedDataType,
}

#[derive(Debug, Clone)]
pub enum ParsedItemKind {
    InherentImplementationItem {
        generic_names: Vec<String>,
        target_type_span: Span,
        target_type: ParsedDataType,
        associated_items: Vec<ParsedItem>,
    },
    ModuleDeclaration {
        name_span: Span,
        name: String,
        items: Vec<ParsedItem>,
    },
    FunctionDeclaration {
        recursive_keyword_span: Option<Span>,
        runtime_keyword_span: Option<Span>,
        name_span: Span,
        name: String,
        generic_names: Vec<String>,
        self_parameter: Option<ParsedSelfFunctionParameter>,
        parameters: Vec<(ParsedPattern, ParsedDataType)>,
        return_type: ParsedDataType,
        body: Box<ParsedBlockExpression>,
    },
    MinecraftFunctionDeclaration {
        resource_location: ResourceLocation,
        body: Box<ParsedBlockExpression>,
    },
    TypeAliasDeclaration {
        name_span: Span,
        name: String,
        generic_names: Vec<String>,
        alias: ParsedDataType,
    },
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

#[derive(Debug, Clone)]
pub struct ParsedItem {
    pub span: Span,
    pub visibility: Visibility,
    pub kind: ParsedItemKind,
}

impl ParsedItem {
    #[must_use]
    pub fn resolve_names(self, ctx: &mut SemanticAnalysisContext) -> Option<NamedItem> {
        let kind = match self.kind {
            ParsedItemKind::InherentImplementationItem {
                generic_names: impl_generic_names,
                target_type_span,
                target_type,
                associated_items,
            } => {
                ctx.enter_scope();

                let impl_generic_ids = impl_generic_names
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

                let self_type_id = ctx.declare_parsed_type(
                    Visibility::Public,
                    ParsedTypeDeclarationKind::Alias(ParsedTypeAliasDeclaration {
                        name: "Self".to_owned(),
                        generic_ids: Vec::new(),
                        alias: target_type.clone(),
                    }),
                );

                ctx.enter_scope();

                let associated_items = associated_items
                    .into_iter()
                    .map(|item| {
                        let mut item = item.resolve_names(ctx)?;

                        if let NamedItemKind::FunctionDeclaration {
                            generic_names: ref mut fn_generic_names,
                            generic_ids: ref mut fn_generic_ids,
                            ..
                        } = item.kind
                        {
                            fn_generic_names.splice(0..0, impl_generic_names.iter().cloned());

                            fn_generic_ids.splice(0..0, impl_generic_ids.iter().copied());
                        }

                        Some(item)
                    })
                    .collect_option_all::<Vec<_>>();

                let associated_items_scope = ctx.exit_scope();

                ctx.exit_scope();

                let associated_items = associated_items?;

                NamedItemKind::InherentImplementationItem {
                    generic_names: impl_generic_names,
                    target_type_span,
                    target_type,
                    associated_items,
                    self_type_id,
                    generic_ids: impl_generic_ids,
                    associated_items_scope,
                }
            }
            ParsedItemKind::ModuleDeclaration {
                name_span,
                name,
                items,
            } => {
                if ctx.current_scope().type_is_declared(&name) {
                    return ctx
                        .add_error(name_span, SemanticAnalysisError::TypeAlreadyDeclared(name));
                }

                ctx.enter_module(name.clone());

                let items = items
                    .into_iter()
                    .map(|item| item.resolve_names(ctx))
                    .collect_option_all::<Vec<_>>();

                ctx.exit_module_and_declare(self.visibility);

                let items = items?;

                NamedItemKind::ModuleDeclaration { name, items }
            }
            ParsedItemKind::FunctionDeclaration {
                recursive_keyword_span,
                runtime_keyword_span,
                name_span,
                name,
                generic_names,
                self_parameter,
                parameters,
                return_type,
                body,
            } => {
                if ctx.current_scope().value_is_declared(&name) {
                    return ctx
                        .add_error(name_span, SemanticAnalysisError::ValueAlreadyDeclared(name));
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

                let id = ctx.declare_parsed_value(
                    self.visibility,
                    ParsedValueDeclarationKind::Function(Box::new(
                        ParsedFunctionDeclaration::Regular(ParsedRegularFunctionDeclaration {
                            name: name.clone(),
                            generic_ids: generic_ids.clone(),
                        }),
                    )),
                );

                let id = HighRegularFunctionId(id.0);

                NamedItemKind::FunctionDeclaration {
                    recursive_keyword_span,
                    runtime_keyword_span,
                    name,
                    generic_names,
                    self_parameter,
                    parameters,
                    return_type,
                    body,
                    id,
                    generic_ids,
                }
            }
            ParsedItemKind::MinecraftFunctionDeclaration {
                resource_location,
                body,
            } => NamedItemKind::MinecraftFunctionDeclaration {
                resource_location,
                body,
            },
            ParsedItemKind::TypeAliasDeclaration {
                name_span,
                name,
                generic_names,
                alias,
            } => {
                if ctx.current_scope().type_is_declared(&name) {
                    return ctx
                        .add_error(name_span, SemanticAnalysisError::TypeAlreadyDeclared(name));
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

                let id = ctx.declare_parsed_type(
                    self.visibility,
                    ParsedTypeDeclarationKind::Alias(ParsedTypeAliasDeclaration {
                        name: name.clone(),
                        generic_ids: generic_ids.clone(),
                        alias: alias.clone(),
                    }),
                );

                NamedItemKind::TypeAliasDeclaration {
                    name,
                    generic_names,
                    alias,
                    id,
                    generic_ids,
                }
            }
            ParsedItemKind::RegularStructDeclaration {
                name_span,
                name,
                generic_names,
                field_types,
            } => {
                if ctx.current_scope().type_is_declared(&name) {
                    return ctx
                        .add_error(name_span, SemanticAnalysisError::TypeAlreadyDeclared(name));
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

                let id = ctx.declare_parsed_type(
                    self.visibility,
                    ParsedTypeDeclarationKind::Struct(ParsedStructDeclaration::Struct(
                        ParsedRegularStructDeclaration {
                            name: name.clone(),
                            generic_ids: generic_ids.clone(),
                        },
                    )),
                );

                NamedItemKind::RegularStructDeclaration {
                    name,
                    generic_names,
                    field_types,
                    id,
                    generic_ids,
                }
            }
            ParsedItemKind::TupleStructDeclaration {
                name_span,
                name,
                generic_names,
                field_types,
            } => {
                if ctx.current_scope().type_is_declared(&name) {
                    return ctx
                        .add_error(name_span, SemanticAnalysisError::TypeAlreadyDeclared(name));
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

                let id = ctx.declare_parsed_type(
                    self.visibility,
                    ParsedTypeDeclarationKind::Struct(ParsedStructDeclaration::Tuple(
                        ParsedTupleStructDeclaration::new(name.clone(), generic_ids.clone()),
                    )),
                );

                let id = HighTupleStructId(id.0);

                let constructor_id = ctx.declare_parsed_value(
                    self.visibility,
                    ParsedValueDeclarationKind::Function(Box::new(
                        ParsedFunctionDeclaration::Builtin(ParsedBuiltinFunctionDeclaration {
                            name: name.clone(),
                            generic_ids: generic_ids.clone(),
                        }),
                    )),
                );

                let constructor_id = HighBuiltinFunctionId(constructor_id.0);

                NamedItemKind::TupleStructDeclaration {
                    name,
                    generic_names,
                    field_types,
                    id,
                    generic_ids,
                    constructor_id,
                }
            }
            ParsedItemKind::Use(tree) => NamedItemKind::Use(tree),
        };

        Some(NamedItem {
            span: self.span,
            visibility: self.visibility,
            kind,
        })
    }
}
