use crate::{cst_node, lower::expression::CSTExpression, syntax::SyntaxKind};

cst_node!(CSTTupleExpression, SyntaxKind::TupleExpression);

impl<'a> CSTTupleExpression<'a> {
    pub fn expressions(&self) -> Vec<CSTExpression<'a>> {
        self.children().filter_map(CSTExpression::cast).collect()
    }
}
