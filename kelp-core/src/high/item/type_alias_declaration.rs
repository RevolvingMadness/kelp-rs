use crate::{
    high::{
        data_type::DataType,
        environment::r#type::alias::HighTypeAliasDeclaration,
        semantic_analysis::{SemanticAnalysisContext, info::error::SemanticAnalysisError},
    },
    low::item::Item,
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
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        visibility: Visibility,
    ) -> Option<Item> {
        if ctx.current_scope().type_is_declared(&self.name) {
            return ctx.add_error(
                self.name_span,
                SemanticAnalysisError::TypeAlreadyDeclared(self.name),
            );
        }

        ctx.enter_scope();

        let generic_ids = self
            .generic_names
            .into_iter()
            .map(|generic_name| ctx.declare_generic(Visibility::Public, generic_name))
            .collect::<Vec<_>>();

        let alias = self.alias.perform_semantic_analysis(ctx);

        ctx.exit_scope();

        ctx.declare_alias(
            visibility,
            HighTypeAliasDeclaration {
                name: self.name,
                generic_ids,
                alias,
            },
        );

        Some(Item::TypeAliasDeclaration)
    }
}
