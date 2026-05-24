use crate::{
    high::{
        item::{
            function_declaration::FunctionDeclarationItem,
            type_alias_declaration::TypeAliasDeclarationItem,
        },
        semantic_analysis::SemanticAnalysisContext,
    },
    low::item::Item,
    span::Span,
    visibility::Visibility,
};

#[derive(Debug, Clone)]
pub enum AssociatedItemKind {
    FunctionDeclaration(FunctionDeclarationItem),
    TypeAliasDeclaration(TypeAliasDeclarationItem),
}

#[derive(Debug, Clone)]
pub struct AssociatedItem {
    pub span: Span,
    pub visibility: Visibility,
    pub kind: AssociatedItemKind,
}

impl AssociatedItem {
    pub fn perform_semantic_analysis(
        self,
        ctx: &mut SemanticAnalysisContext,
        visibility: Visibility,
    ) -> Option<Item> {
        match self.kind {
            AssociatedItemKind::FunctionDeclaration(item) => item.perform_semantic_analysis(ctx),
            AssociatedItemKind::TypeAliasDeclaration(item) => {
                item.perform_semantic_analysis(ctx, visibility)
            }
        }
    }
}
