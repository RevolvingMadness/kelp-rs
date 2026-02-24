use crate::{cst_node, lower::expression::CSTExpression, syntax::SyntaxKind};

cst_node!(CSTIndexExpression, SyntaxKind::IndexExpression);

impl<'a> CSTIndexExpression<'a> {
    pub fn target(&self) -> Option<CSTExpression<'a>> {
        self.children().find_map(CSTExpression::cast)
    }

    pub fn index(self) -> Option<CSTExpression<'a>> {
        self.children().filter_map(CSTExpression::cast).nth(1)
    }
}
