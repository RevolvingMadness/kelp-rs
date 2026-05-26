use crate::{
    parsed::{
        item::{
            function_declaration::FunctionDeclarationItem,
            type_alias_declaration::TypeAliasDeclarationItem,
        },
        semantic_analysis::SemanticAnalysisContext,
    },
    semantic::item::Item,
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
    pub fn resolve_names(
        &mut self,
        ctx: &mut SemanticAnalysisContext,
        visibility: Visibility,
    ) -> Option<()> {
        match &mut self.kind {
            AssociatedItemKind::FunctionDeclaration(item) => item.resolve_names(ctx, visibility),
            AssociatedItemKind::TypeAliasDeclaration(item) => item.resolve_names(ctx, visibility),
        }
    }

    pub fn resolve_types(&mut self, ctx: &mut SemanticAnalysisContext, visibility: Visibility) {
        match &mut self.kind {
            AssociatedItemKind::FunctionDeclaration(item) => item.resolve_types(ctx, visibility),
            AssociatedItemKind::TypeAliasDeclaration(item) => item.resolve_types(ctx, visibility),
        }
    }

    pub fn perform_semantic_analysis(self, ctx: &mut SemanticAnalysisContext) -> Option<Item> {
        match self.kind {
            AssociatedItemKind::FunctionDeclaration(item) => item.perform_semantic_analysis(ctx),
            AssociatedItemKind::TypeAliasDeclaration(..) => Some(Item::TypeAliasDeclaration),
        }
    }
}
