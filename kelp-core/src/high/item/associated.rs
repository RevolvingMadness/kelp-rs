use crate::{
    high::item::{
        function_declaration::FunctionDeclarationItem,
        type_alias_declaration::TypeAliasDeclarationItem,
    },
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
