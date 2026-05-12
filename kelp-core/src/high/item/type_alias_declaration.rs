use crate::{
    high::{
        data_type::DataType,
        environment::r#type::{HighTypeDeclarationKind, alias::HighAliasDeclaration},
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
        if ctx.type_is_declared_in_current_scope(&self.name) {
            return ctx.add_error(
                self.name_span,
                SemanticAnalysisError::TypeAlreadyDeclared(self.name),
            );
        }

        ctx.enter_scope();

        for generic_name in self.generic_names.clone() {
            ctx.declare_type(
                Visibility::Public,
                HighTypeDeclarationKind::Generic(generic_name),
            );
        }

        let alias = self.alias.perform_semantic_analysis(ctx);

        ctx.exit_scope();

        ctx.declare_alias(
            visibility,
            HighAliasDeclaration {
                name: self.name,
                generic_names: self.generic_names,
                alias,
            },
        );

        Some(Item::TypeAliasDeclaration)
    }
}
