use crate::{cst_node, lower::expression::CSTExpression, syntax::SyntaxKind};

cst_node!(CSTIndexExpression, SyntaxKind::IndexExpression);

impl<'a> CSTIndexExpression<'a> {
    pub(crate) fn target(&self) -> Option<CSTExpression<'a>> {
        self.0.children().find_map(CSTExpression::cast)
    }

    pub(crate) fn index(self) -> Option<CSTExpression<'a>> {
        self.0.children().filter_map(CSTExpression::cast).nth(1)
    }
}
