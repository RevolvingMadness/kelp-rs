use std::collections::{BTreeMap, HashMap};

use minecraft_command_types::resource_location::ResourceLocation;

use crate::{
    high::{
        data_type::unresolved::UnresolvedDataType,
        environment::r#type::{
            HighTypeDeclaration, alias::HighAliasDeclaration, r#struct::HighStructDeclaration,
        },
        semantic_analysis_context::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
        statement::Statement,
    },
    middle::item::Item as MiddleItem,
    span::Span,
};

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    MCFNDeclaration(ResourceLocation, Box<Statement>),
    TypeAliasDeclaration(Span, String, Vec<String>, UnresolvedDataType),
    StructDeclaration(
        Span,
        String,
        Vec<String>,
        BTreeMap<String, UnresolvedDataType>,
    ),
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
            ItemKind::TypeAliasDeclaration(name_span, name, generic_names, alias) => {
                if ctx.type_is_declared_in_current_scope(&name) {
                    return ctx
                        .add_error(name_span, SemanticAnalysisError::TypeIsAlreadyDefined(name));
                }

                let alias = alias.resolve_generics(Some(&generic_names), ctx);

                ctx.declare_data_type(HighTypeDeclaration::Alias(HighAliasDeclaration {
                    name,
                    generic_names,
                    alias,
                }));

                MiddleItem::TypeAliasDeclaration
            }
            ItemKind::StructDeclaration(name_span, name, generic_names, field_types) => {
                if ctx.type_is_declared_in_current_scope(&name) {
                    return ctx
                        .add_error(name_span, SemanticAnalysisError::TypeIsAlreadyDefined(name));
                }

                let field_types = field_types
                    .into_iter()
                    .map(|(field_name, field_type)| {
                        let field_type = field_type.resolve_generics(Some(&generic_names), ctx);

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
        })
    }
}
