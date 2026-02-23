use crate::{cst_node, cstlib::token::CSTToken, syntax::SyntaxKind};

cst_node!(CSTTokenExpression, SyntaxKind::LiteralExpression);

impl<'a> CSTTokenExpression<'a> {
    pub fn value_token(&self) -> Option<&'a CSTToken<'a>> {
        self.0.children_tokens().next()
    }
}
