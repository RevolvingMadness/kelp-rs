use la_arena::Idx;

use crate::{
    parsed::{
        data_type::DataType,
        environment::{
            resolved::r#type::{HighGenericId, alias::ResolvedTypeAliasDeclaration},
            unresolved::r#type::{
                UnresolvedTypeDeclarationKind, alias::UnresolvedTypeAliasDeclaration,
            },
        },
        item::Item,
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    },
    span::Span,
    visibility::Visibility,
};

#[derive(Debug, Clone)]
pub struct TypeAliasDeclarationItem {
    pub name_span: Span,
    pub name: String,
    pub generic_names: Vec<String>,
    pub alias: DataType,
}

impl TypeAliasDeclarationItem {
    pub fn resolve_names(
        &self,
        item_id: Idx<Item>,
        ctx: &mut SemanticAnalysisContext,
        visibility: Visibility,
    ) -> Option<()> {
        if ctx.current_scope().type_is_declared(&self.name) {
            return ctx.add_error(
                self.name_span,
                SemanticAnalysisError::TypeAlreadyDeclared(self.name.clone()),
            );
        }

        ctx.enter_scope();

        let generic_ids = self
            .generic_names
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

        ctx.declare_item_generic_ids(item_id, generic_ids.clone());

        ctx.exit_scope();

        let type_id = ctx.declare_unresolved_type(
            visibility,
            UnresolvedTypeDeclarationKind::Alias(UnresolvedTypeAliasDeclaration {
                name: self.name.clone(),
                generic_ids,
            }),
        );

        ctx.declare_item_type_id(item_id, type_id);

        Some(())
    }

    pub fn resolve_types(
        &self,
        item_id: Idx<Item>,
        ctx: &mut SemanticAnalysisContext,
        visibility: Visibility,
    ) {
        let id = ctx.get_item_type_id(item_id);
        let generic_ids = ctx.get_item_generic_ids(item_id).to_vec();

        ctx.enter_scope();

        for (generic_id, generic_name) in generic_ids
            .iter()
            .copied()
            .zip(self.generic_names.iter().cloned())
        {
            ctx.declare_generic(generic_id, Visibility::Public, generic_name);
        }

        let alias = self.alias.clone().perform_semantic_analysis(ctx);

        ctx.exit_scope();

        ctx.declare_alias(
            id,
            visibility,
            ResolvedTypeAliasDeclaration {
                name: self.name.clone(),
                generic_ids: generic_ids
                    .into_iter()
                    .map(|generic_id| HighGenericId(generic_id.0))
                    .collect(),
                alias,
            },
        );
    }
}
