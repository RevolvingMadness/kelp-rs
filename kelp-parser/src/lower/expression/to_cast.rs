use crate::{cst_node, lower::expression::CSTExpression, syntax::SyntaxKind};

cst_node!(CSTToCastExpression, SyntaxKind::ToCastExpression);

impl<'a> CSTToCastExpression<'a> {
    pub fn expression(&self) -> Option<CSTExpression<'a>> {
        self.0.children().find_map(CSTExpression::cast)
    }

    pub fn runtime_storage_type<'b>(&self, text: &'b str) -> Option<&'b str> {
        self.0.children_tokens().find_map(|token| {
            if token.kind == SyntaxKind::RuntimeStorageType {
                Some(token.text(text))
            } else {
                None
            }
        })
    }
}
