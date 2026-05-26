use crate::parsed::environment::r#type::ParsedTypeDeclarationKind;
use crate::semantic::environment::r#type::{
    HighGenericId, HighTypeId, alias::ResolvedTypeAliasDeclaration,
};
use crate::{
    parsed::{
        data_type::ParsedDataType,
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
    pub alias: ParsedDataType,

    pub id: Option<HighTypeId>,
    pub generic_ids: Option<Vec<HighGenericId>>,
}

impl TypeAliasDeclarationItem {
    pub fn resolve_names(
        &mut self,
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
                    ParsedTypeDeclarationKind::Generic(generic_name),
                );

                HighGenericId(id.0)
            })
            .collect();

        self.generic_ids = Some(generic_ids);

        ctx.exit_scope();

        let id = ctx.declare_unresolved_type(
            visibility,
            ParsedTypeDeclarationKind::Generic(self.name.clone()),
        );

        self.id = Some(id);

        Some(())
    }

    pub fn resolve_types(&mut self, ctx: &mut SemanticAnalysisContext, visibility: Visibility) {
        let id = self.id.unwrap();
        let generic_ids = self.generic_ids.as_ref().unwrap();

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
                    .iter()
                    .map(|generic_id| HighGenericId(generic_id.0))
                    .collect(),
                alias,
            },
        );
    }
}
