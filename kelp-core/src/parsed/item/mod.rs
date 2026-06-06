use std::collections::HashMap;

use minecraft_command_types::resource_location::ResourceLocation;

use crate::parsed::environment::r#type::generic::ParsedGenericDeclaration;
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
use crate::parsed::semantic_analysis::RegularFunctionModifiers;
use crate::semantic::data_type::SemanticDataType;
use crate::semantic::environment::r#type::generic::HighGenericId;
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

#[derive(Debug, Clone, Copy)]
pub struct FunctionQualifiers {
    pub recursive: Option<Span>,
    pub runtime: Option<Span>,
}

impl FunctionQualifiers {
    pub fn into_modifiers(self) -> RegularFunctionModifiers {
        if self.runtime.is_some() {
            let recursive = self.recursive.is_some();

            RegularFunctionModifiers::Runtime { recursive }
        } else {
            RegularFunctionModifiers::None
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParsedItemKind {
    InherentImplementationItem {
        generics: Vec<(Span, String)>,
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
        qualifiers: FunctionQualifiers,
        name_span: Span,
        name: String,
        generics: Vec<(Span, String)>,
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
        generics: Vec<(Span, String)>,
        alias: ParsedDataType,
    },
    RegularStructDeclaration {
        name_span: Span,
        name: String,
        generics: Vec<(Span, String)>,
        field_types: HashMap<String, ParsedDataType>,
    },
    TupleStructDeclaration {
        name_span: Span,
        name: String,
        generics: Vec<(Span, String)>,
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
                generics: impl_generics,
                target_type_span,
                target_type,
                associated_items,
            } => {
                ctx.enter_scope();

                let impl_generic_ids = impl_generics
                    .iter()
                    .cloned()
                    .map(|(span, name)| {
                        let id = ctx.declare_parsed_type(
                            Visibility::Public,
                            ParsedTypeDeclarationKind::Generic(ParsedGenericDeclaration {
                                name_span: span,
                                name,
                            }),
                        );

                        HighGenericId(id.0)
                    })
                    .collect::<Vec<_>>();

                let self_type_id = ctx.declare_parsed_type(
                    Visibility::Public,
                    ParsedTypeDeclarationKind::Alias(ParsedTypeAliasDeclaration {
                        name_span: target_type_span,
                        name: "Self".to_owned(),
                        generic_ids: Vec::new(),
                        alias: target_type.clone(),
                    }),
                );

                ctx.enter_parsed_implementation(impl_generic_ids);

                let associated_items = associated_items
                    .into_iter()
                    .map(|item| item.resolve_names(ctx))
                    .collect_option_all::<Vec<_>>();

                let (impl_generic_ids, associated_items_scope) = ctx.exit_parsed_implementation();

                ctx.exit_scope();

                let associated_items = associated_items?;

                NamedItemKind::InherentImplementationItem {
                    generics: impl_generics,
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
                if let Some(declaration_span) = ctx
                    .current_scope()
                    .get_type_declaration_span(&ctx.parsed_environment, &name)
                {
                    return ctx.add_error(SemanticAnalysisError::TypeAlreadyDeclared {
                        declaration_span,
                        redeclaration_span: name_span,
                        name,
                    });
                }

                let id = ctx.enter_parsed_module(self.visibility, Some(name_span), name);

                let items = items
                    .into_iter()
                    .map(|item| item.resolve_names(ctx))
                    .collect_option_all::<Vec<_>>();

                ctx.exit_module();

                let items = items?;

                NamedItemKind::ModuleDeclaration { id, items }
            }
            ParsedItemKind::FunctionDeclaration {
                qualifiers,
                name_span,
                name,
                generics: generic_names,
                self_parameter,
                parameters,
                return_type,
                body,
            } => {
                if let Some(declaration_span) = ctx
                    .current_scope()
                    .get_value_declaration_span(&ctx.parsed_environment, &name)
                {
                    return ctx.add_error(SemanticAnalysisError::ValueAlreadyDeclared {
                        declaration_span,
                        redeclaration_span: name_span,
                        name,
                    });
                }

                ctx.enter_scope();

                let generic_ids = generic_names
                    .iter()
                    .cloned()
                    .map(|(span, name)| {
                        let id = ctx.declare_parsed_type(
                            Visibility::Public,
                            ParsedTypeDeclarationKind::Generic(ParsedGenericDeclaration {
                                name_span: span,
                                name,
                            }),
                        );

                        HighGenericId(id.0)
                    })
                    .collect::<Vec<_>>();

                ctx.exit_scope();

                let id = ctx.declare_parsed_value(
                    self.visibility,
                    ParsedValueDeclarationKind::Function(Box::new(
                        ParsedFunctionDeclaration::Regular(ParsedRegularFunctionDeclaration {
                            name_span,
                            name: name.clone(),
                            generic_ids: generic_ids.clone(),
                        }),
                    )),
                );

                let id = HighRegularFunctionId(id.0);

                NamedItemKind::FunctionDeclaration {
                    qualifiers,
                    name_span,
                    name,
                    generics: generic_names,
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
                generics: generic_names,
                alias,
            } => {
                if let Some(declaration_span) = ctx
                    .current_scope()
                    .get_type_declaration_span(&ctx.parsed_environment, &name)
                {
                    return ctx.add_error(SemanticAnalysisError::TypeAlreadyDeclared {
                        declaration_span,
                        redeclaration_span: name_span,
                        name,
                    });
                }

                ctx.enter_scope();

                let generic_ids = generic_names
                    .iter()
                    .cloned()
                    .map(|(span, name)| {
                        let id = ctx.declare_parsed_type(
                            Visibility::Public,
                            ParsedTypeDeclarationKind::Generic(ParsedGenericDeclaration {
                                name_span: span,
                                name,
                            }),
                        );

                        HighGenericId(id.0)
                    })
                    .collect::<Vec<_>>();

                ctx.exit_scope();

                let id = ctx.declare_parsed_type(
                    self.visibility,
                    ParsedTypeDeclarationKind::Alias(ParsedTypeAliasDeclaration {
                        name_span,
                        name: name.clone(),
                        generic_ids: generic_ids.clone(),
                        alias: alias.clone(),
                    }),
                );

                NamedItemKind::TypeAliasDeclaration {
                    name_span,
                    name,
                    generics: generic_names,
                    alias,
                    id,
                    generic_ids,
                }
            }
            ParsedItemKind::RegularStructDeclaration {
                name_span,
                name,
                generics: generic_names,
                field_types,
            } => {
                if let Some(declaration_span) = ctx
                    .current_scope()
                    .get_type_declaration_span(&ctx.parsed_environment, &name)
                {
                    return ctx.add_error(SemanticAnalysisError::TypeAlreadyDeclared {
                        declaration_span,
                        redeclaration_span: name_span,
                        name,
                    });
                }

                ctx.enter_scope();

                let generic_ids = generic_names
                    .iter()
                    .cloned()
                    .map(|(span, name)| {
                        let id = ctx.declare_parsed_type(
                            Visibility::Public,
                            ParsedTypeDeclarationKind::Generic(ParsedGenericDeclaration {
                                name_span: span,
                                name,
                            }),
                        );

                        HighGenericId(id.0)
                    })
                    .collect::<Vec<_>>();

                ctx.exit_scope();

                let id = ctx.declare_parsed_type(
                    self.visibility,
                    ParsedTypeDeclarationKind::Struct(ParsedStructDeclaration::Struct(
                        ParsedRegularStructDeclaration {
                            name_span,
                            name: name.clone(),
                            generic_ids: generic_ids.clone(),
                        },
                    )),
                );

                NamedItemKind::RegularStructDeclaration {
                    name_span,
                    name,
                    generics: generic_names,
                    field_types,
                    id,
                    generic_ids,
                }
            }
            ParsedItemKind::TupleStructDeclaration {
                name_span,
                name,
                generics: generic_names,
                field_types,
            } => {
                if let Some(declaration_span) = ctx
                    .current_scope()
                    .get_type_declaration_span(&ctx.parsed_environment, &name)
                {
                    return ctx.add_error(SemanticAnalysisError::TypeAlreadyDeclared {
                        declaration_span,
                        redeclaration_span: name_span,
                        name,
                    });
                }

                ctx.enter_scope();

                let generic_ids = generic_names
                    .iter()
                    .cloned()
                    .map(|(span, name)| {
                        let id = ctx.declare_parsed_type(
                            Visibility::Public,
                            ParsedTypeDeclarationKind::Generic(ParsedGenericDeclaration {
                                name_span: span,
                                name,
                            }),
                        );

                        HighGenericId(id.0)
                    })
                    .collect::<Vec<_>>();

                ctx.exit_scope();

                let id = ctx.declare_parsed_type(
                    self.visibility,
                    ParsedTypeDeclarationKind::Struct(ParsedStructDeclaration::Tuple(
                        ParsedTupleStructDeclaration {
                            name_span,
                            name: name.clone(),
                            generic_ids: generic_ids.clone(),
                        },
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
                    name_span,
                    name,
                    generics: generic_names,
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
