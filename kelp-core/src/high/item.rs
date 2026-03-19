use std::collections::{BTreeMap, HashMap};

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    high::{data_type::DataType, statement::Statement},
    middle::{data_type_declaration::DataTypeDeclarationKind, item::Item as MiddleItem},
    semantic_analysis_context::{SemanticAnalysisContext, SemanticAnalysisError},
    span::Span,
    trait_ext::CollectOptionAllIterExt,
};

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    MCFNDeclaration(ResourceLocation, Box<Statement>),
    TypeAliasDeclaration(String, Vec<String>, DataType),
    StructDeclaration(String, Vec<String>, BTreeMap<String, DataType>),
}

impl ItemKind {
    #[must_use]
    pub const fn with_span(self, span: Span) -> Item {
        Item { span, kind: self }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub span: Span,
    pub kind: ItemKind,
}

impl Item {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
    ) -> Option<MiddleItem> {
        Some(match self.kind {
            ItemKind::MCFNDeclaration(resource_location, statement) => {
                let statement = statement.perform_semantic_analysis(ctx)?;

                MiddleItem::MCFNDeclaration(resource_location, Box::new(statement))
            }
            ItemKind::TypeAliasDeclaration(name, generic_names, alias) => {
                if ctx.data_type_is_declared(&name) {
                    return ctx
                        .add_error(self.span, SemanticAnalysisError::TypeIsAlreadyDefined(name));
                }

                let Some(alias) = alias.perform_semantic_analysis(Some(&generic_names), ctx) else {
                    ctx.declare_data_type(&name, None);

                    return None;
                };

                ctx.declare_data_type(
                    &name,
                    Some(DataTypeDeclarationKind::Alias {
                        name: name.clone(),
                        generics: generic_names.clone(),
                        alias: alias.clone(),
                    }),
                );

                MiddleItem::TypeAliasDeclaration(name, generic_names, alias)
            }
            ItemKind::StructDeclaration(name, generic_names, field_types) => {
                if ctx.data_type_is_declared(&name) {
                    return ctx
                        .add_error(self.span, SemanticAnalysisError::TypeIsAlreadyDefined(name));
                }

                let Some(field_types) = field_types
                    .into_iter()
                    .map(|(field_name, field_type)| {
                        let field_type =
                            field_type.perform_semantic_analysis(Some(&generic_names), ctx)?;

                        Some((field_name, field_type))
                    })
                    .collect_option_all::<HashMap<_, _>>()
                else {
                    ctx.declare_data_type(&name, None);

                    return None;
                };

                ctx.declare_data_type(
                    &name,
                    Some(DataTypeDeclarationKind::Struct {
                        name: name.clone(),
                        generics: generic_names.clone(),
                        fields: field_types.clone(),
                    }),
                );

                MiddleItem::StructDeclaration(name, generic_names, field_types)
            }
        })
    }
}
