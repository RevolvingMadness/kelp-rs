use crate::{cst_node, lower::expression::CSTExpression, syntax::SyntaxKind};

cst_node!(
    CSTParenthesizedExpression,
    SyntaxKind::ParenthesizedExpression
);

impl<'a> CSTParenthesizedExpression<'a> {
    pub fn expression(&self) -> Option<CSTExpression<'a>> {
        self.0.children().find_map(CSTExpression::cast)
    }
}
